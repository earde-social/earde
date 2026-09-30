open Html.Infix

(* The shared-threads surfaces: the per-thread Share page and the community
   management page. Pure rendering over handler-supplied view models — no
   Caqti, no session access, no authority decision. See the .mli for the
   full contract.

   Two rules shape everything here. The canonical discussion stays one
   thread: the copy speaks of sharing a thread with a community, never of a
   copy or a mirror, and no requester, reviewer, or remover username exists
   in this module's vocabulary. And the private request note is private
   workflow text: it renders only where the handler already authorized it,
   labelled, escaped, and never parsed as Markdown or HTML. *)

type candidate = { candidate_name : string; candidate_slug : string }

type share_placement = {
  share_placement_id : string;
  share_destination_name : string;
  share_destination_slug : string;
  share_pending : bool;
  share_can_withdraw : bool;
  share_can_remove : bool;
  share_note : string option;
}

type share_state = {
  share_thread_title : string;
  share_origin_name : string;
  share_origin_slug : string;
  share_thread_path : string;
  share_candidates : candidate list;
  share_placements : share_placement list;
  share_manage_connections : bool;
}

type section_option = { section_id : string; section_name : string }

type pending_entry = {
  pending_id : string;
  pending_title : string;
  pending_thread_path : string;
  pending_counterpart_name : string;
  pending_counterpart_slug : string;
  pending_note : string option;
  pending_requested_at : string;
}

type accepted_entry = {
  accepted_id : string;
  accepted_title : string;
  accepted_thread_path : string;
  accepted_counterpart_name : string;
  accepted_counterpart_slug : string;
  accepted_section : string option;
  accepted_at : string;
}

type management_state = {
  community_name : string;
  community_slug : string;
  community_eligible : bool;
  sections_enabled : bool;
  section_options : section_option list;
  incoming : pending_entry list;
  outgoing : pending_entry list;
  shared_into : accepted_entry list;
  shared_from : accepted_entry list;
}

type notice =
  | Request_sent
  | Request_accepted
  | Request_rejected
  | Request_withdrawn
  | Placement_removed

type feedback =
  | Stale_form
  | Destination_required
  | Destination_unavailable
  | Already_shared
  | Note_invalid
  | Thread_unavailable
  | Section_invalid
  | Source_ineligible
  | Origin_unavailable
  | Review_unavailable
  | Withdrawal_unavailable
  | Removal_unavailable

(* A community slug in an action path must be a single non-empty URL path
   segment; anything else drops every actionable form for that row. *)
let valid_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* A placement id reaches the page as a decimal string and only ever becomes
   a path segment; anything but plain digits is refused, so no route value
   can be smuggled through it. It identifies a record and grants nothing —
   the handler re-resolves and re-authorizes it. *)
let valid_placement_id value =
  String.length value > 0
  && String.length value <= 19
  && String.for_all (fun c -> c >= '0' && c <= '9') value

(* A section id in a select option: same digits-only rule. *)
let valid_section_id = valid_placement_id

(* An internal path this module received from the handler: server-built,
   but still refused as an attribute value if it could not be one of our
   own absolute paths. *)
let valid_internal_path value =
  String.length value > 1
  && value.[0] = '/'
  && String.for_all
       (fun byte -> Char.code byte > 0x20 && Char.code byte <> 0x7f)
       value

let settings_path ~community_slug =
  Printf.sprintf "/c/%s/settings/shared-threads" community_slug

let action_path ~community_slug ~placement_id ~verb =
  Html.text
    (Printf.sprintf "%s/%s/%s"
       (settings_path ~community_slug)
       placement_id verb)

let notice_copy = function
  | Request_sent ->
      Html.static
        "Sharing request sent. The other community's moderators will review it."
  | Request_accepted ->
      Html.static "The thread is now shared into this community."
  | Request_rejected ->
      Html.static "The request was declined. Nothing else changed."
  | Request_withdrawn -> Html.static "The sharing request was withdrawn."
  | Placement_removed -> Html.static "The thread is no longer shared."

let feedback_copy = function
  | Stale_form ->
      Html.static
        "This page had been open too long, so the action could no longer be \
         submitted. Nothing was changed. Try again."
  | Destination_required ->
      Html.static "Choose a community to share this thread with."
  | Destination_unavailable ->
      Html.static "That community is not available to share this thread with."
  | Already_shared ->
      Html.static
        "This thread is already shared with that community, or a request is \
         already waiting."
  | Note_invalid ->
      Html.static
        "That note could not be saved. Notes are limited to 2,000 characters \
         of ordinary text."
  | Thread_unavailable -> Html.static "This thread can no longer be shared."
  | Section_invalid ->
      Html.static
        "Choose one of this community's own forum sections for the thread."
  | Source_ineligible ->
      Html.static "This community cannot take part in thread sharing right now."
  | Origin_unavailable ->
      Html.static
        "The other community, or the connection with it, is no longer \
         available for sharing."
  | Review_unavailable ->
      Html.static "That sharing request is no longer pending."
  | Withdrawal_unavailable ->
      Html.static "That sharing request is no longer pending."
  | Removal_unavailable -> Html.static "That thread is no longer shared here."

let notice_html = function
  | None -> Html.empty
  | Some notice ->
      Html.template "<div class='ccn-alert sth-notice'><p>%s</p></div>"
        [ notice_copy notice ]

let feedback_html = function
  | None -> Html.empty
  | Some feedback ->
      Html.template "<div class='ccn-alert'><p>%s</p></div>"
        [ feedback_copy feedback ]

(* Counterpart identity: the name, with the address underneath. It links to
   the community only when the slug is addressable. *)
let community_identity ~name ~slug =
  let name = Html.text name in
  if valid_community_slug slug then
    Html.template
      "<div class='ccn-identity'><a class='ccn-name' href='/c/%s'>%s</a><span \
       class='ccn-slug'>/c/%s</span></div>"
      [ Html.text slug; name; Html.text slug ]
  else
    Html.template
      "<div class='ccn-identity'><span class='ccn-name'>%s</span></div>"
      [ name ]

(* The canonical thread identity on a management row: the title, linking to
   the one canonical thread page. *)
let thread_identity ~title ~path =
  let title = Html.text title in
  if valid_internal_path path then
    Html.template "<a class='ccn-name sth-title' href='%s'>%s</a>"
      [ Html.text path; title ]
  else Html.template "<span class='ccn-name sth-title'>%s</span>" [ title ]

(* Private workflow text: labelled as private, HTML-escaped, and never
   parsed as Markdown or HTML. *)
let note_html = function
  | None -> Html.empty
  | Some note ->
      Html.template
        "<div class='ccn-note'><p class='ccn-note-label'>Private note — \
         visible only to the requester and the moderators involved.</p><p \
         class='ccn-note-body'>%s</p></div>"
        [ Html.text note ]

let time_html raw =
  Html.template "<span class='ccn-state sth-time'>%s</span>"
    [ Html.text (Components.time_ago raw) ]

(* One single-purpose form per action: the route path carries the subjects,
   and the only application fields are the closed ones each form needs (the
   share-page origin marker, the accept form's section). Dream's framework
   CSRF field is emitted only when a live request is supplied. *)
let action_form ?request ~action ~cls ~label ~extra_fields () =
  let csrf_field =
    match request with
    | None -> Html.empty
    | Some request -> Csrf_field.tag request
  in
  Html.template
    "<form method='POST' action='%s' class='ccn-form %s'>%s%s<button \
     type='submit' class='ccn-btn'>%s</button></form>"
    [ action; cls; csrf_field; extra_fields; label ]

let share_context_field =
  Html.static "<input type='hidden' name='context' value='share'>"

(* --- The per-thread Share page --- *)

let share_heading (state : share_state) =
  let thread =
    if valid_internal_path state.share_thread_path then
      Html.template "<a class='ccn-name sth-title' href='%s'>%s</a>"
        [
          Html.text state.share_thread_path; Html.text state.share_thread_title;
        ]
    else
      Html.template "<span class='ccn-name sth-title'>%s</span>"
        [ Html.text state.share_thread_title ]
  in
  Html.template
    "<div class='create-head'><h1 class='create-title'>Share thread</h1><p \
     class='create-sub ccn-intro'>Share this thread with a connected \
     community. The discussion stays in one place: the thread and its comments \
     remain here in %s.</p></div><div class='ccn-identity sth-thread'>%s<span \
     class='ccn-slug'>/c/%s</span></div>"
    [
      Html.text state.share_origin_name;
      thread;
      Html.text state.share_origin_slug;
    ]

let request_section ?request (state : share_state) =
  let action = Html.text (state.share_thread_path ^ "/share") in
  let body =
    match state.share_candidates with
    | [] ->
        let connections_link =
          if
            state.share_manage_connections
            && valid_community_slug state.share_origin_slug
          then
            Html.template
              "<p class='ccn-hint'><a class='ccn-link' \
               href='/c/%s/settings/connections'>Manage community \
               connections</a></p>"
              [ Html.text state.share_origin_slug ]
          else Html.empty
        in
        (* A quiet empty state, never an empty select — and no hint that
           further communities were withheld. *)
        Html.template
          "<p class='ccn-empty'>No connected community can receive this thread \
           right now.</p>%s"
          [ connections_link ]
    | candidates ->
        let csrf_field =
          match request with
          | None -> Html.empty
          | Some request -> Csrf_field.tag request
        in
        let options =
          Html.concat
            (List.filter_map
               (fun c ->
                 if valid_community_slug c.candidate_slug then
                   Some
                     (Html.template "<option value='%s'>%s</option>"
                        [
                          Html.text c.candidate_slug; Html.text c.candidate_name;
                        ])
                 else None)
               candidates)
        in
        if options = Html.empty then
          Html.static
            "<p class='ccn-empty'>No connected community can receive this \
             thread right now.</p>"
        else if not (valid_internal_path state.share_thread_path) then
          Html.empty
        else
          Html.template
            "<form method='POST' action='%s' class='ccn-confirm-form'>%s<label \
             class='ccn-label' for='sth-destination'>Community</label><select \
             id='sth-destination' name='destination' class='ccn-input'><option \
             value=''>Choose a community…</option>%s</select><label \
             class='ccn-label' for='sth-note'>Private note \
             (optional)</label><p class='ccn-hint'>Visible only to you and the \
             moderators involved.</p><textarea id='sth-note' name='note' \
             rows='5' maxlength='2000' class='ccn-textarea'></textarea><button \
             type='submit' class='ccn-btn'>Request sharing</button></form>"
            [ action; csrf_field; options ]
  in
  Html.template
    "<section class='ccn-section'><h2 class='ccn-section-title'>Share with a \
     community</h2><p class='ccn-section-desc'>The other community's \
     moderators review the request before the thread appears \
     there.</p>%s</section>"
    [ body ]

let placement_row ?request ~origin_slug (p : share_placement) =
  let actionable =
    valid_community_slug origin_slug && valid_placement_id p.share_placement_id
  in
  let state_label = if p.share_pending then "Awaiting approval" else "Shared" in
  let withdraw =
    if p.share_pending && p.share_can_withdraw && actionable then
      action_form ?request
        ~action:
          (action_path ~community_slug:origin_slug
             ~placement_id:p.share_placement_id ~verb:"withdraw")
        ~cls:(Html.static "sth-withdraw")
        ~label:(Html.static "Withdraw request")
        ~extra_fields:share_context_field ()
    else Html.empty
  in
  let remove =
    if (not p.share_pending) && p.share_can_remove && actionable then
      action_form ?request
        ~action:
          (action_path ~community_slug:origin_slug
             ~placement_id:p.share_placement_id ~verb:"remove")
        ~cls:(Html.static "sth-remove")
        ~label:(Html.static "Stop sharing")
        ~extra_fields:share_context_field ()
    else Html.empty
  in
  Html.template
    "<li class='ccn-row sth-share-row'>%s<p class='ccn-state'>%s</p>%s<div \
     class='ccn-actions'>%s%s</div></li>"
    [
      community_identity ~name:p.share_destination_name
        ~slug:p.share_destination_slug;
      Html.text state_label;
      note_html p.share_note;
      withdraw;
      remove;
    ]

let placements_section ?request (state : share_state) =
  let body =
    match state.share_placements with
    | [] ->
        Html.static
          "<p class='ccn-empty'>This thread is not shared anywhere yet.</p>"
    | rows ->
        Html.template "<ul class='ccn-list'>%s</ul>"
          [
            (Html.join (Html.static "\n"))
              (List.map
                 (placement_row ?request ~origin_slug:state.share_origin_slug)
                 rows);
          ]
  in
  Html.template
    "<section class='ccn-section'><h2 class='ccn-section-title'>Where this \
     thread is shared</h2>%s</section>"
    [ body ]

let share_body ?request ~(state : share_state) () =
  let back =
    if valid_internal_path state.share_thread_path then
      Html.template
        "<p><a class='ccn-link' href='%s'>Back to the thread</a></p>"
        [ Html.text state.share_thread_path ]
    else Html.empty
  in
  share_heading state
  ++ request_section ?request state
  ++ placements_section ?request state
  ++ back

(* --- The community management page --- *)

let management_heading (state : management_state) =
  ignore state;
  Html.static
    "<div class='create-head'><h1 class='create-title'>Shared threads</h1><p \
     class='create-sub ccn-intro'>Threads shared between this community and \
     its connected communities.</p></div>"

(* One generic page-level notice. It states the rule, never which of the
   three facts this community currently fails. *)
let ineligible_notice_html (state : management_state) =
  if state.community_eligible then Html.empty
  else
    Html.static
      "<div class='ccn-ineligible'><p>This community cannot accept newly \
       shared threads right now. Pending requests can still be rejected or \
       withdrawn, and shared threads can still be removed below.</p></div>"

(* The one section select, rendered per accept form from the single list
   the read model loaded once. *)
let section_select ~row_id ~(sections : section_option list) =
  let options =
    Html.concat
      (List.filter_map
         (fun s ->
           if valid_section_id s.section_id then
             Some
               (Html.template "<option value='%s'>%s</option>"
                  [ Html.text s.section_id; Html.text s.section_name ])
           else None)
         sections)
  in
  Html.template
    "<select name='section' class='ccn-input sth-section' aria-label='Forum \
     section for row %s'><option value=''>Choose a \
     section…</option>%s</select>"
    [ Html.text row_id; options ]

let incoming_row ?request ~(state : management_state) (p : pending_entry) =
  let actionable =
    valid_community_slug state.community_slug && valid_placement_id p.pending_id
  in
  let accept =
    if not actionable then Html.empty
    else if not state.community_eligible then
      Html.static
        "<p class='ccn-accept-unavailable'>Accepting is unavailable while this \
         community cannot take part in sharing.</p>"
    else
      let selector =
        if state.sections_enabled then
          section_select ~row_id:p.pending_id ~sections:state.section_options
        else Html.empty
      in
      action_form ?request
        ~action:
          (action_path ~community_slug:state.community_slug
             ~placement_id:p.pending_id ~verb:"accept")
        ~cls:(Html.static "sth-accept") ~label:(Html.static "Accept")
        ~extra_fields:selector ()
  in
  let reject =
    if not actionable then Html.empty
    else
      (* Rejection stays available on an ineligible community: a pending
         request must always be closable. *)
      action_form ?request
        ~action:
          (action_path ~community_slug:state.community_slug
             ~placement_id:p.pending_id ~verb:"reject")
        ~cls:(Html.static "sth-reject") ~label:(Html.static "Reject")
        ~extra_fields:Html.empty ()
  in
  Html.template
    "<li class='ccn-row ccn-row--incoming'><div class='ccn-identity'>%s<span \
     class='ccn-slug'>from</span></div>%s%s%s<div \
     class='ccn-actions'>%s%s</div></li>"
    [
      thread_identity ~title:p.pending_title ~path:p.pending_thread_path;
      community_identity ~name:p.pending_counterpart_name
        ~slug:p.pending_counterpart_slug;
      note_html p.pending_note;
      time_html p.pending_requested_at;
      accept;
      reject;
    ]

let outgoing_row ?request ~(state : management_state) (p : pending_entry) =
  let actionable =
    valid_community_slug state.community_slug && valid_placement_id p.pending_id
  in
  let withdraw =
    if not actionable then Html.empty
    else
      action_form ?request
        ~action:
          (action_path ~community_slug:state.community_slug
             ~placement_id:p.pending_id ~verb:"withdraw")
        ~cls:(Html.static "sth-withdraw")
        ~label:(Html.static "Withdraw") ~extra_fields:Html.empty ()
  in
  Html.template
    "<li class='ccn-row ccn-row--outgoing'><div class='ccn-identity'>%s<span \
     class='ccn-slug'>to</span></div>%s%s%s<div \
     class='ccn-actions'>%s</div></li>"
    [
      thread_identity ~title:p.pending_title ~path:p.pending_thread_path;
      community_identity ~name:p.pending_counterpart_name
        ~slug:p.pending_counterpart_slug;
      note_html p.pending_note;
      time_html p.pending_requested_at;
      withdraw;
    ]

let section_label = function
  | Some name ->
      Html.template
        "<span class='ccn-state sth-section-label'>Section: %s</span>"
        [ Html.text name ]
  | None ->
      Html.static
        "<span class='ccn-state sth-section-label'>Uncategorized</span>"

let accepted_row ?request ~(state : management_state) ~direction
    (a : accepted_entry) =
  let actionable =
    valid_community_slug state.community_slug
    && valid_placement_id a.accepted_id
  in
  let remove =
    if not actionable then Html.empty
    else
      action_form ?request
        ~action:
          (action_path ~community_slug:state.community_slug
             ~placement_id:a.accepted_id ~verb:"remove")
        ~cls:(Html.static "sth-remove") ~label:(Html.static "Remove")
        ~extra_fields:Html.empty ()
  in
  Html.template
    "<li class='ccn-row ccn-row--accepted'><div class='ccn-identity'>%s<span \
     class='ccn-slug'>%s</span></div>%s%s%s<div \
     class='ccn-actions'>%s</div></li>"
    [
      thread_identity ~title:a.accepted_title ~path:a.accepted_thread_path;
      direction;
      community_identity ~name:a.accepted_counterpart_name
        ~slug:a.accepted_counterpart_slug;
      section_label a.accepted_section;
      time_html a.accepted_at;
      remove;
    ]

let listed section_title empty rows =
  let body =
    match rows with
    | [] -> Html.template "<p class='ccn-empty'>%s</p>" [ empty ]
    | rows ->
        Html.template "<ul class='ccn-list'>%s</ul>"
          [ Html.join (Html.static "\n") rows ]
  in
  Html.template
    "<section class='ccn-section'><h2 \
     class='ccn-section-title'>%s</h2>%s</section>"
    [ section_title; body ]

(* The four sections, fixed order: the two queues that ask for a decision,
   then the durable record in both directions. An empty page and a busy one
   read the same way. Row order inside each section is the read model's. *)
let management_body ?request ~(state : management_state) () =
  management_heading state
  ++ ineligible_notice_html state
  ++ listed
       (Html.static "Incoming requests")
       (Html.static "No incoming requests.")
       (List.map (incoming_row ?request ~state) state.incoming)
  ++ listed
       (Html.static "Outgoing requests")
       (Html.static "No outgoing requests.")
       (List.map (outgoing_row ?request ~state) state.outgoing)
  ++ listed
       (Html.static "Shared into this community")
       (Html.static "Nothing is shared into this community.")
       (List.map
          (accepted_row ?request ~state ~direction:(Html.static "from"))
          state.shared_into)
  ++ listed
       (Html.static "Shared from this community")
       (Html.static "Nothing is shared from this community.")
       (List.map
          (accepted_row ?request ~state ~direction:(Html.static "to"))
          state.shared_from)

(* --- Documents --- *)

(* Identical shell handling to the connections management surface: with
   launch chrome the mono community context precedes the create-shell marker;
   without it (only when the durable community record could not be re-read
   AFTER authorization succeeded) the same body renders inside the
   chrome-free launch message document rather than fabricating community
   data. This surface deliberately lives inside the connections skin scope
   (the ccn-* fragments beneath body.launch-community-connections), so it
   reuses that one stylesheet family whole — the CSS census pins one scope
   class per shell surface, and the distinguishing hook for this page is
   the community-shared-threads wrap class inside the panel. *)
(* [in_settings_shell]: the management page is a community-settings surface,
   so its launch branch renders inside the shared settings shell (header band
   + grouped settings navigation, Shared threads active — the viewer is
   top_mod-or-durable-admin by the read model's SQL). The share surface is a
   thread workflow reachable by ordinary members, so it keeps the mono
   community context + create-shell document. *)
let document ?user ?request ?shell ?(in_settings_shell = false) ~title ~body ()
    =
  let wrapped =
    Html.template
      "<div class='create-wrap community-shared-threads'><div \
       class='create-panel'>%s</div></div>"
      [ body ]
  in
  match shell with
  | None ->
      Page_shell.launch_message_page ?request ~noindex:true ~title
        ~content:
          (Html.template "<div class='create-shell'>%s</div>" [ wrapped ])
        ()
  | Some
      ((community_record : Community_types.community), rail_communities, sidebar)
    ->
      let content =
        if in_settings_shell then
          Community_settings_shell.wrap ~slug:community_record.slug
            ~active:Community_settings_shell.Shared_threads
            ~can_complete_setup:
              (Community_settings_shell.can_complete_setup
                 ~community:community_record ~authorized:true)
            ~network_manager:true ~panel:wrapped ()
        else
          let context =
            Html.template
              "<div class='launch-review-context'><span \
               class='launch-review-context-name'>%s</span><span \
               class='launch-review-context-slug'>/c/%s</span></div>"
              [
                Html.text community_record.name; Html.text community_record.slug;
              ]
          in
          context
          ++ Html.template "<div class='create-shell'>%s</div>" [ wrapped ]
      in
      Community_shell.launch_community_page ?user ?request ~noindex:true
        ~rail_communities ~community:community_record ~sidebar
        ~page_class:"launch-community-connections" ~title ~content ()

let share_page ?user ?request ?shell ~state ~notice ~feedback () =
  document ?user ?request ?shell ~title:"Share thread"
    ~body:
      (notice_html notice ++ feedback_html feedback
      ++ share_body ?request ~state ())
    ()

let management_page ?user ?request ?shell ~state ~notice ~feedback () =
  document ?user ?request ?shell ~in_settings_shell:true ~title:"Shared threads"
    ~body:
      (notice_html notice ++ feedback_html feedback
      ++ management_body ?request ~state ())
    ()
