(* Pure helpers for "Start thread from chat". Extracted from the handler so the
   title/body prefill, checkbox-id parsing, and channel-row marker classification are
   unit-testable without a DB or a live request (see test/test_earde.ml). Placed here,
   above community_channel_shell_page, because the channel renderer uses
   classify_message_links. UI says "Start thread", never "Promote". *)
module Start_thread = struct
  (* First-sentence-ish title from the seed message: cut at the first . ! ? or newline,
     then hard-cap to ~80 bytes on a word boundary with an ellipsis. Always trimmed; the
     form marks the field required, so an empty seed yields "" and the user types one. *)
  let derive_title (content : string) : string =
    let s = String.trim content in
    let n = String.length s in
    let stop =
      let rec find i =
        if i >= n then n
        else match s.[i] with
          | '.' | '!' | '?' | '\n' | '\r' -> i
          | _ -> find (i + 1)
      in find 0
    in
    let first = String.trim (String.sub s 0 stop) in
    let max_len = 80 in
    if String.length first <= max_len then first
    else begin
      let cut = String.sub first 0 max_len in
      let cut = match String.rindex_opt cut ' ' with
        | Some sp when sp > 40 -> String.sub cut 0 sp
        | _ -> cut
      in
      cut ^ "\xe2\x80\xa6" (* … *)
    end

  (* Checkbox field names are "msg_<id>" (value "on"). Repeated same-name fields don't
     survive Dream.form's assoc list, so one distinct key per candidate is used. Parse
     -> deduped int64 list; non-matching keys and unparseable ids are ignored. *)
  let parse_selected_ids (form_data : (string * string) list) : int64 list =
    let plen = String.length "msg_" in
    List.filter_map (fun (k, _v) ->
      if String.length k > plen && String.sub k 0 plen = "msg_" then
        (try Some (Int64.of_string (String.sub k plen (String.length k - plen)))
         with _ -> None)
      else None
    ) form_data
    |> List.sort_uniq Int64.compare

  (* Channel-row marker for a chat message, decided from all its thread-source links.
     Mk_seed: the message seeds a thread (wins — a message seeds at most one). Mk_referenced
     (target_post_id, title, ref_count): used only as context elsewhere; still startable.
     Mk_no_link: no relation. *)
  type msg_marker =
    | Mk_seed of int * string
    | Mk_referenced of int * string * int
    | Mk_no_link

  (* Pure classification from one message's links [(post_id, title, is_seed)]. Order-
     independent: seed wins; otherwise the highest post_id (most recent) is the link
     target and ref_count is how many threads reference the message. *)
  let classify_message_links (links : (int * string * bool) list) : msg_marker =
    match List.find_opt (fun (_, _, is_seed) -> is_seed) links with
    | Some (pid, title, _) -> Mk_seed (pid, title)
    | None ->
        match links with
        | [] -> Mk_no_link
        | first :: rest ->
            let (pid, title, _) =
              List.fold_left (fun (bp, bt, bs) (pid, title, is_seed) ->
                if pid > bp then (pid, title, is_seed) else (bp, bt, bs))
                first rest
            in
            Mk_referenced (pid, title, List.length links)

  (* Server-side selection guard: keep only ids that are real candidates (in [valid]),
     drop the seed (forced separately), dedup, order chronologically (ids are monotonic),
     then cap context to max_total-1 so seed+context never exceeds max_total. *)
  let normalize_selection ~seed ~max_total ~(valid : int64 list) (selected : int64 list) : int64 list =
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
    in
    selected
    |> List.filter (fun id -> id <> seed && List.mem id valid)
    |> List.sort_uniq Int64.compare
    |> take (max_total - 1)

  (* ?source_thread=<post_id> reverse-navigation parameter. Strict positive-int parse:
     anything unparseable, zero, or negative reads as "no focus" so a mangled URL renders
     the normal channel page instead of an error. *)
  let parse_source_thread (raw : string option) : int option =
    match raw with
    | None -> None
    | Some s ->
        (match int_of_string_opt (String.trim s) with
         | Some n when n > 0 -> Some n
         | _ -> None)

  (* Comma-joined id list for the data-source-highlight-ids attribute. Digits and commas
     only by construction, so it is attribute-safe without escaping. *)
  let highlight_ids_attr (ids : int64 list) : string =
    String.concat "," (List.map Int64.to_string ids)

  (* Compact display forms of a Postgres timestamp-text ("YYYY-MM-DD HH:MM:SS..."):
     date_of_ts -> "YYYY-MM-DD", minute_of_ts -> "YYYY-MM-DD HH:MM". Pure truncation —
     timestamps are stored UTC and rendered verbatim elsewhere in chat, so no timezone
     math here. Short/odd inputs pass through untouched. *)
  let date_of_ts (ts : string) : string =
    if String.length ts >= 10 then String.sub ts 0 10 else ts

  let minute_of_ts (ts : string) : string =
    if String.length ts >= 16 then String.sub ts 0 16 else ts

  (* Provenance metadata for the promoted-conversation block, derived purely from the
     persisted source rows — never from the curator's editable introduction. Participants
     are distinct non-empty authors of AVAILABLE rows (deleted rows are masked and would
     otherwise all collapse into one fake "" participant). Date range spans all rows. *)
  type source_summary = {
    ss_available : int;
    ss_unavailable : int;
    ss_participants : int;
    ss_date_range : string;
  }

  let summarize_source (msgs : Thread_source_store.thread_source_msg list) : source_summary =
    let available = List.filter (fun (m : Thread_source_store.thread_source_msg) -> not m.sm_deleted) msgs in
    let participants =
      List.fold_left (fun acc (m : Thread_source_store.thread_source_msg) ->
        let a = String.trim m.sm_author in
        if a = "" || List.mem a acc then acc else a :: acc) [] available
    in
    let dates = List.map (fun (m : Thread_source_store.thread_source_msg) -> date_of_ts m.sm_created_at) msgs in
    let date_range =
      match dates with
      | [] -> ""
      | first :: rest ->
          let last = List.fold_left (fun _ d -> d) first rest in
          if first = last then first else first ^ " \xe2\x80\x93 " ^ last (* – *)
    in
    { ss_available = List.length available;
      ss_unavailable = List.length msgs - List.length available;
      ss_participants = List.length participants;
      ss_date_range = date_range }
end

let community_channel_shell_page ?user ?realtime_token ?(noindex=false) ~is_member ?(can_start=false)
    ?(thread_links : (int64 * int * string * bool) list = [])
    ?(source_focus : (int * string * int64 list) option)
    ~(rail_communities : Community_types.community list)
    ~(channels : Channel_store.channel list) ~(sections : Section_store.community_section list)
    ~(channel : Channel_store.channel) ~(messages : (Chat_store.chat_message * string option) list)
    ~(community : Community_types.community) request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" (esc community.slug) (esc channel.slug) in

  (* Launch community sidebar (pass-8 grammar, channel variant): identity
     head, Overview link, factual visibility marker, Live channels with the
     CURRENT channel active, Knowledge sections (this renderer has no
     per-section counts, so none are shown), and the intentionally public
     moderation log. Settings and Home requests need the mod/top-mod
     authority the channel handler never loads, so they are not rendered
     here — nothing is invented. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
               (Page_shell.launch_tile_color community.slug) (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>" src)
    | _ ->
        Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
          (Page_shell.launch_tile_color community.slug) (esc tile_glyph)
  in
  let side_head =
    Printf.sprintf
      "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
      (esc community.slug) side_face (esc community.name) (esc community.slug)
  in
  let nav_overview =
    Printf.sprintf
      "<a class='navitem navitem--pad' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
      (esc community.slug)
  in
  let vis_note =
    if community.visibility = Community_types.Community_private then
      "<div class='launch-side-vis'>private community</div>"
    else ""
  in
  let nav_live =
    if channels = [] then ""
    else
      "<p class='kicker sidebar__group'>Live</p>"
      ^ String.concat "" (List.map (fun (c : Channel_store.channel) ->
          let cls = if c.slug = channel.slug then "navitem navitem--active" else "navitem" in
          Printf.sprintf
            "<a class='%s' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
            cls (esc community.slug) (esc c.slug) (esc c.slug))
          channels)
  in
  let nav_knowledge =
    if sections = [] then ""
    else
      "<p class='kicker sidebar__group'>Knowledge</p>"
      ^ String.concat "" (List.map (fun (s : Section_store.community_section) ->
          Printf.sprintf
            "<a class='navitem' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s</a>"
            (esc community.slug) (esc s.slug) (esc s.name))
          sections)
  in
  let nav_network =
    "<p class='kicker sidebar__group'>Network</p>"
    ^ Printf.sprintf
        "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
        (esc community.slug)
  in
  let sidebar =
    Printf.sprintf
      "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
      (esc community.name) side_head nav_overview vis_note nav_live nav_knowledge nav_network
  in

  let topic_html = match channel.topic with
    | Some t when t <> "" -> Printf.sprintf "<span class='cs-ch-topic'>%s</span>" (esc t)
    | _ -> ""
  in
  (* Shared cursors are opt-in and community-gated: the server decides whether
     the control exists at all (Features allow-list), so the browser can't
     enable the feature by editing its DOM/URL. Logged-out viewers get no
     control — they have no realtime token to share through. The checkbox
     itself only governs broadcasting; seeing others' cursors needs no opt-in. *)
  let share_control =
    if user <> None && Features.shared_cursors_enabled ~community_slug:community.slug then
      Printf.sprintf
        "<label class='cs-cursor-share' id='chat-cursor-share' data-community-slug='%s'><input type='checkbox' id='chat-cursor-share-toggle'>Share cursor</label>"
        (esc community.slug)
    else ""
  in
  let head = Printf.sprintf
    "<div class='cs-main-head'><span class='cs-hash'>#</span><span>%s</span>%s%s</div>"
    (esc channel.name) topic_html share_control
  in

  (* Stream oldest → newest (the DB read already returns ascending), so newest sits at the
     bottom. Deleted messages are masked; a NULL/unknown author (GDPR tombstone) shows
     "[deleted]". Avatar glyph = first letter of the resolved author. *)
  let render_message ((m : Chat_store.chat_message), (author : string option)) =
    let name = match author with Some u -> u | None -> "[deleted]" in
    let initial =
      if String.length name > 0 && name.[0] <> '['
      then String.uppercase_ascii (String.sub name 0 1) else "?"
    in
    let body =
      if m.deleted_at <> None then "<span class='cs-msg-deleted'>[message deleted]</span>"
      else esc m.content
    in
    (* Per-message thread action, from this message's thread-source links. A seed links
       to its thread ("Thread ->") and suppresses "Start thread". A message only
       referenced as context shows "Referenced in ->" (+N if several) AND still offers
       "Start thread". "Start thread" needs a member who can_start on a non-deleted,
       authored message; the form re-validates every permission server-side. Thread/
       reference links are public (visible to everyone). *)
    let short t = if String.length t > 40 then String.sub t 0 39 ^ "\xe2\x80\xa6" else t in
    let links_for_msg =
      List.filter_map (fun (mid, pid, title, is_seed) ->
        if mid = m.id then Some (pid, title, is_seed) else None) thread_links in
    let marker = Start_thread.classify_message_links links_for_msg in
    (* A message that already seeds a thread is "promoted"; suppress Start thread on it so a
       message can't be promoted twice (the "Thread ->" marker below already links its thread).
       Referenced-only messages (Mk_referenced) are still startable. *)
    let already_promoted = match marker with Start_thread.Mk_seed _ -> true | _ -> false in
    let promote_url = Printf.sprintf "/c/%s/ch/%s/messages/%Ld/start-thread" (esc community.slug) (esc channel.slug) m.id in
    let start_link =
      if can_start && m.deleted_at = None && m.user_id <> None && not already_promoted then
        Printf.sprintf "<a class='cs-msg-start' href='%s' data-promote-url='%s'>Start thread</a>" promote_url promote_url
      else "" in
    (* Minute precision, matching chat_message_json, so SSR rows and rows the
       JS appends later (live, catch-up, composer response) display alike. *)
    let time_text = Start_thread.minute_of_ts m.created_at in
    let time_html =
      if start_link = "" then Printf.sprintf "<span class='cs-msg-time'>%s</span>" (esc time_text)
      else Printf.sprintf "<span class='cs-msg-time-slot'><span class='cs-msg-time'>%s</span>%s</span>" (esc time_text) start_link
    in
    (* Provenance markers stay attached under the message text. Start thread is rendered
       in the meta row beside the timestamp so hover never changes message height. *)
    let action =
      match marker with
      | Start_thread.Mk_seed (post_id, title) ->
          Printf.sprintf "<a class='cs-msg-thread' href='%s'>Started thread &rarr; %s</a>"
            (Post_cards.canonical_thread_path community.slug post_id title) (esc (short title))
      | Start_thread.Mk_referenced (post_id, title, count) ->
          let extra = if count > 1 then Printf.sprintf " <span class='cs-msg-refmore'>+%d</span>" (count - 1) else "" in
          Printf.sprintf "<a class='cs-msg-ref' href='%s'>Included in thread &rarr; %s</a>%s"
            (Post_cards.canonical_thread_path community.slug post_id title) (esc (short title)) extra
      | Start_thread.Mk_no_link -> ""
    in
    let actions_row = if action = "" then "" else Printf.sprintf "<div class='cs-msg-actions'>%s</div>" action in
    (* id='msg-<id>' makes per-message deep links (#msg-…) work with JS off; the reverse-
       navigation highlighter also targets rows through it. *)
    Printf.sprintf
      "<div class='cs-msg' id='msg-%Ld' data-message-id='%Ld' data-has-thread='%s'><div class='cs-msg-avatar'>%s</div><div class='cs-msg-body'><div class='cs-msg-meta'><span class='cs-msg-author'>%s</span>%s</div><div class='cs-msg-text'>%s</div>%s</div></div>"
      m.id m.id (if already_promoted then "true" else "false") (esc initial) (esc name) time_html body actions_row
  in
  let messages_html =
    if messages = [] then
      "<div class='cs-msg-empty'>No messages yet. Be the first to say something.</div>"
    else String.concat "\n" (List.map render_message messages)
  in

  (* Composer is a real <form method=POST> (works with JS off). Three states:
     anon → log in; logged-in non-member → join; member → send. The send form carries
     community_slug + channel_slug (not a raw id) so the handler re-validates ownership. *)
  let composer =
    match user with
    | None ->
        Printf.sprintf "<div class='cs-composer cs-composer-prompt'>Please <a href='/login'>log in</a> to chat in #%s.</div>"
          (esc channel.slug)
    | Some _ when not is_member && community.visibility = Community_types.Community_private ->
        (* Private community: a non-member viewing this is an authorized mod/admin; they still
           can't chat without membership, but show no self-join button (Slice C). *)
        "<div class='cs-composer cs-composer-prompt'><span>Only members can chat in this private community.</span></div>"
    | Some _ when not is_member ->
        Printf.sprintf "<div class='cs-composer cs-composer-prompt'><span>Join this community to chat.</span><form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='cs-send'>Join &amp; chat</button></form></div>"
          csrf_token community.id channel_url
    | Some _ ->
        Printf.sprintf "<div class='cs-composer'><form action='/messages' method='POST'>%s<input type='hidden' name='community_slug' value='%s'><input type='hidden' name='channel_slug' value='%s'><textarea name='content' rows='1' maxlength='4000' placeholder='Message #%s' required></textarea><button type='submit' class='cs-send'>Send</button></form></div>"
          csrf_token (esc community.slug) (esc channel.slug) (esc channel.slug)
  in

  let realtime_socket_url =
    match Sys.getenv_opt "REALTIME_SOCKET_URL" with
    | Some url when String.trim url <> "" -> String.trim url
    | _ ->
        Logs.warn (fun m ->
            m "REALTIME_SOCKET_URL is not set; live chat websocket disabled for this page");
        ""
  in
  let realtime_signed_token =
    if realtime_socket_url = "" then ""
    else Option.value realtime_token ~default:""
  in
  (* Reverse navigation (?source_thread=<post_id>): an SSR-visible context notice plus
     data attributes the page JS uses to scroll to the first source message and flash the
     whole group. Everything degrades: with JS off the notice still explains the state and
     the links still work; without source_focus the page is byte-identical to before. *)
  let source_notice, source_data_attrs =
    match source_focus with
    | None -> ("", "")
    | Some (post_id, post_title, highlight_ids) ->
        let thread_href = Post_cards.canonical_thread_path community.slug post_id post_title in
        let short_title =
          if String.length post_title > 60 then String.sub post_title 0 59 ^ "\xe2\x80\xa6" else post_title in
        let notice = Printf.sprintf
          "<div class='cs-source-notice'><span class='cs-source-notice-text'>Viewing the conversation promoted to <b>%s</b></span><span class='cs-source-notice-actions'><a href='%s'>&larr; Back to thread</a><a href='%s'>Jump to latest &darr;</a></span></div>"
          (esc short_title) thread_href channel_url in
        let attrs = match highlight_ids with
          | [] -> ""
          | first :: _ ->
              Printf.sprintf " data-source-anchor-id='%Ld' data-source-highlight-ids='%s'"
                first (Start_thread.highlight_ids_attr highlight_ids) in
        (notice, attrs)
  in
  (* The typing row sits between the scrolling message body and the composer
     (Discord placement): it never scrolls with history and keeps its reserved
     height when empty so the composer doesn't jump. JS fills it by id.
     cs-chat-stage wraps the scroller with a sibling shared-cursor overlay
     covering its visible box; both are empty/inert without JS. *)
  let main =
    Printf.sprintf
      "%s%s<div class='cs-chat-stage'><div id='chat-live-root' class='cs-main-body cs-chat-body' data-channel-id='%d' data-can-start='%s' data-socket-url='%s' data-signed-token='%s'%s>%s</div><div class='cs-cursor-overlay' id='chat-cursor-overlay' aria-hidden='true'></div></div><div class='cs-typing' id='chat-typing' hidden></div>%s"
      head
      source_notice
      channel.id
      (if can_start then "true" else "false")
      (Components.html_escape realtime_socket_url)
      (Components.html_escape realtime_signed_token)
      source_data_attrs
      messages_html
      composer
  in
  (* Presence pane: markup (classes + every #chat-presence-* id chat_live.js
     fills) unchanged; only the outer wrapper is the launch `.aside--chat`
     column instead of the legacy `.cs-aside` grid cell. *)
  let presence_pane =
    "<div class='cs-presence' id='chat-presence'>\
       <div class='ca-label' id='chat-presence-heading'>In this channel</div>\
       <div class='cs-presence-status' id='chat-presence-status'>Connecting&#8230;</div>\
       <ul class='cs-presence-list' id='chat-presence-list'></ul>\
     </div>"
  in
  let aside = Printf.sprintf "<aside class='aside aside--chat'>%s</aside>" presence_pane in
  let title = Printf.sprintf "#%s · %s" channel.name community.name in
  (* The parameterized reverse-navigation view canonicalizes to the clean channel URL so
     crawlers never index per-thread duplicates of the same channel page. *)
  let canonical_link =
    if source_focus = None then ""
    else Printf.sprintf "<link rel='canonical' href='%s'>" channel_url
  in
  let head_extra =
    canonical_link ^
    "<script src='/static/js/phoenix.js' defer></script>\
     <script src='/static/js/chat_live.js' defer></script>"
  in
  (* The complete <main> element, built here so the launch wrapper can never
     interpose a box: .cs-main is a flex column whose head / chat stage /
     typing row / composer MUST stay direct children, and for a private
     community the ph-no-capture replay guard rides on this element itself
     (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    Printf.sprintf "<main class='%s'>%s</main>"
      (if community.visibility = Community_types.Community_private then "cs-main ph-no-capture" else "cs-main")
      main
  in
  Community_shell.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~head_extra ~aside ~community ~sidebar
    ~page_class:"launch-community-channel" ~title ~main_el ()
