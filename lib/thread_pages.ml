(* What the thread page may say about a promoted thread's chat origin, decided by the
   HANDLER from the viewer's read authorization on the source channel's community:
   Ts_visible carries the (slug, name) of the source channel (None if the channel was
   deleted) plus the chronological source rows; Ts_private renders only a neutral
   "promoted from a private conversation" notice — no channel name, authors, content,
   or timestamps ever reach an unauthorized viewer's markup. *)
type thread_source_view =
  | Ts_private
  | Ts_visible of (string * string) option * Thread_source_store.thread_source_msg list

(* An accepted DESTINATION-context rendering of the canonical thread,
   resolved and authorized by the handler (accepted placement, destination
   binding, origin currently public, viewer admitted by the destination's
   existing can_view_community rule). The page then renders the same
   canonical post and comments under the destination shell: [stc_section]
   is the placement's own destination section — the effective local section
   context (None = the destination's flat/uncategorized context) — and
   [stc_origin_name] feeds the visible "Shared from" provenance label. The
   origin may be named and linked because this context only exists while
   the origin community is public. *)
type shared_thread_page_context = {
  stc_origin_name : string;
  stc_section : (string * string) option;
}

(* The closed post-creation notices (slice 4): the composer's PRG lands on
   the canonical origin thread with ?shared=..., the handler maps the query
   value onto this variant, and anything else renders nothing. Fixed copy
   only — no query-supplied text, no failure cause, no destination name:
   which private or unavailable condition sank a request must not surface
   here. Deliberately origin-only: the handler never resolves a value for a
   destination-context rendering. *)
type thread_creation_notice =
  | Creation_share_requested
  | Creation_share_failed

(* /c/:slug/t/:post_id-:post_slug — the canonical thread view, now on the Cartographic Civic
   launch chrome (Components.launch_community_surface_page: earde.css only).
   Replaces the legacy warm-card post_page for normal threads (post_page stays only as the
   unmappable-post fallback and is left byte-for-byte unchanged). Visible comment/composer UI
   keeps its markup and is re-skinned by the route-scoped CSS; the security-critical
   mod/admin/ban *dialogs* are copied verbatim from post_page (identical routes, CSRF, dialog
   ids and Rule A/B/C logic) so moderation behavior cannot drift — re-skinning those overlays
   would be risky for zero user-facing benefit.
   The optimistic-vote DOM contract (cs-vote = [upvote form, score span, downvote form], buttons
   carrying the exact legacy Tailwind colour classes the shared vote JS toggles — mapped to
   launch accents in earde.css) is preserved exactly. *)
let thread_shell_page ?user ?(noindex=false) ?(can_share=false) ?(can_comment=false)
    ?(creation_notice : thread_creation_notice option)
    ?(shared_context : shared_thread_page_context option)
    ?(shared_with : (string * string) list = []) ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames
    ~banned_usernames ~(rail_communities : Community_types.community list) ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list) ~(community : Community_types.community)
    ?(thread_source : thread_source_view option)
    ~user_post_votes ~user_comment_votes ~(post : Post_types.post) ~(comments : Comment_store.comment list) request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in

  (* The effective LOCAL section context: on a destination-context rendering
     it is the placement's own destination section; otherwise the post's
     origin section. None renders the surrounding community's flat /
     sectionless context — exactly the existing sectionless behavior. *)
  let local_section = match shared_context with
    | Some sc -> sc.stc_section
    | None ->
        (match post.section_name, post.section_slug with
         | Some sn, Some ss -> Some (sn, ss)
         | _ -> None) in
  (* The thread path in THIS page's community context: the canonical origin
     URL on the origin page, the destination-context URL under a
     destination shell. Server-built from the displayed community and the
     canonical post — never a stored or submitted URL. *)
  let local_thread_path =
    Post_cards.canonical_thread_path community.slug post.id post.title in
  (* The closed, server-validated context field a destination-context
     composer carries so a successful comment can return here: it names the
     displayed community only, and the POST handler re-resolves and
     re-authorizes it — it grants nothing by itself. Absent on the origin
     page, whose forms stay byte-identical. *)
  let context_field = match shared_context with
    | Some _ ->
        Printf.sprintf "<input type='hidden' name='context_community' value='%s'>"
          (esc community.slug)
    | None -> "" in

  (* Launch community sidebar (pass-8/9 grammar): the LOCAL section is active.
     Settings gate mirrors the overview: render-time visibility only, handler re-checks. *)
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
      ?active_section_slug:(Option.map snd local_section)
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* Breadcrumb + back-to-section (only when the thread has a LOCAL section here). *)
  let section_crumb = match local_section with
    | Some (sn, ss) -> Printf.sprintf " / <a href='/c/%s/s/%s'>§ %s</a>" (esc community.slug) (esc ss) (esc sn)
    | None -> "" in
  let crumb = Printf.sprintf "<div class='fh-crumb'><a href='/c/%s'>/c/%s</a>%s / <b>thread</b></div>"
    (esc community.slug) (esc community.slug) section_crumb in
  let back_link = match local_section with
    | Some (sn, ss) -> Printf.sprintf "<a class='btn sm' href='/c/%s/s/%s'>&larr; %s</a>" (esc community.slug) (esc ss) (esc sn)
    | None -> "" in

  (* --- post vote column: reuses the proven cs-vote contract from render_forum_row --- *)
  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_post_votes) in
  let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in
  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9650;</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500'>&#9650;</a>" in
  let downvote_html =
    if not post.allow_downvotes then ""
    else match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9660;</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2]'>&#9660;</a>" in
  let vote_col = Printf.sprintf "<div class='cs-vote'>%s<span class='cs-vote-score'>%d</span>%s</div>" upvote_html post.score downvote_html in

  (* --- post moderation controls (delete-own / mod-remove / admin-remove / ban), verbatim --- *)
  let is_post_deleted =
    (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_")
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let post_target_is_admin = List.mem post.username admin_usernames in
  let post_action_btn =
    if is_post_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">%s<input type='hidden' name='post_id' value='%d'><button type='submit' class='ct-act ct-act-danger'>&#128465;&#65039; delete</button></form>" csrf_token post.id
        else if is_current_user_mod && not post_target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='ct-act ct-act-mod'>&#128737;&#65039; Mod Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-amber-900'>&#128737;&#65039; Moderator Removal</h3>
                  <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain why this post is being removed (visible to the community)...'
                      class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else if is_admin && not post_target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-red-900'>&#9889; Admin Intervention</h3>
                  <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain the admin intervention reason (visible to the community)...'
                      class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else ""
  in
  let ban_post_btn =
    if (is_current_user_mod || is_admin) && not (post_target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username
          && not (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_") ->
          if List.mem post.username banned_usernames then
            "<span class='ct-act ct-act-danger'>&#128683; Banned</span>"
          else if is_current_user_mod then
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='ct-act ct-act-mod'>&#128296; Mod Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-amber-900'>&#128296; Mod Ban</h3>
                    <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain why this user is being banned (visible to the community)...'
                        class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (esc post.username) post.community_id post.id
          else
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-red-900'>&#9889; Admin Ban</h3>
                    <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain the admin intervention reason (visible to the community)...'
                        class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (esc post.username) post.community_id post.id
      | _ -> ""
    else ""
  in
  (* Quiet secondary "Report" affordance for logged-in non-authors. Render-time gate is
     visibility only; create_report_handler re-checks login / ban / self-report. *)
  let post_report_btn =
    if is_post_deleted then ""
    else match current_user with
    | Some u when u <> post.username ->
        Printf.sprintf "<a class='ct-act' href='/c/%s/report?type=post&amp;id=%d'>&#9873; report</a>"
          (esc post.community_slug) post.id
    | _ -> "" in
  (* Quiet "Share with a community" affordance on the same ct-act treatment.
     Render-time gate only: the handler decided can_share in SQL (author
     while member and unbanned, origin top_mod, or durable admin — false for
     a tombstoned post), and the share route fully reauthorizes on GET, so
     this link grants nothing. *)
  let post_share_btn =
    if can_share then
      Printf.sprintf "<a class='ct-act' href='%s/share'>&#8644; Share with a community</a>"
        (esc (Post_cards.canonical_thread_path post.community_slug post.id post.title))
    else "" in
  let post_mod_controls = post_action_btn ^ ban_post_btn ^ post_report_btn ^ post_share_btn in

  (* --- post meta + body --- *)
  let domain_html = match post.url with
    | Some u -> (match Post_cards.extract_domain u with
        | Some d -> Printf.sprintf "<a class='ft-domain' href='%s' target='_blank' rel='noopener'>%s &#8599;</a>" (Components.safe_url u) (esc d)
        | None -> "")
    | None -> "" in
  (* Destination-context provenance: the one visible compact label. The
     origin may be linked because this context requires a currently public
     origin. Nothing else about the placement (actors, notes, ids, lifecycle
     wording) ever reaches this page. *)
  let shared_from_label = match shared_context with
    | Some sc ->
        Printf.sprintf "<span class='sth-shared-from'>&#8644; Shared from <a href='/c/%s'>%s</a></span>"
          (esc post.community_slug) (esc sc.stc_origin_name)
    | None ->
        (* Origin-side provenance: only the ORIGIN rendering may say where
           the discussion also lives, so the two directions can never share
           a page — a destination context keeps its "Shared from" line and
           ignores [shared_with] entirely. *)
        Post_cards.shared_with_html shared_with in
  let meta = Printf.sprintf "<div class='th-meta'>%s<span>by %s</span><span>%s</span><span>%d comments</span>%s</div>"
    domain_html (Components.render_author ~mod_usernames ~admin_usernames post.username)
    (esc (Components.time_ago post.created_at)) post.comment_count shared_from_label in
  (* Post mod/owner actions get their own compact row under the meta — no far-right float. *)
  let post_actions_row =
    if post_mod_controls = "" then "" else Printf.sprintf "<div class='th-actions'>%s</div>" post_mod_controls in
  (* Promoted-conversation block. Real provenance lives in thread_source_messages — this
     is its display, always rendered from the persisted relations regardless of the
     curator's editable introduction (the post body). Renders nothing for a normal post.
     Source messages are display-only structured rows: never forum replies, no vote UI,
     no forum authorship. If the source channel was deleted (FK SET NULL) surviving rows
     still render, just without channel links. Ts_private renders only a neutral notice. *)
  let source_html =
    match thread_source with
    | None -> ""
    | Some Ts_private ->
        "<div class='th-src th-src--private'>\
           <div class='th-src-label'>Promoted conversation</div>\
           <div class='th-src-private-note'>This thread was promoted from a private conversation.</div>\
         </div>"
    | Some (Ts_visible (None, [])) -> ""
    | Some (Ts_visible (channel_opt, msgs)) ->
        let summary = Chat_pages.Start_thread.summarize_source msgs in
        (* Channel links use the POST's own community slug — the source
           channel lives beside the canonical post, so the link is correct
           under a destination shell too (identical bytes on the origin
           page, where the two slugs are equal). *)
        let from_html = match channel_opt with
          | Some (cslug, cname) ->
              Printf.sprintf "Promoted from <a href='/c/%s/ch/%s'>#%s</a>"
                (esc post.community_slug) (esc cslug) (esc cname)
          | None -> "Promoted from chat" in
        let plural n = if n = 1 then "" else "s" in
        let meta_bits =
          (if summary.Chat_pages.Start_thread.ss_date_range = "" then []
           else [ esc summary.Chat_pages.Start_thread.ss_date_range ])
          @ [ Printf.sprintf "%d message%s" summary.Chat_pages.Start_thread.ss_available (plural summary.Chat_pages.Start_thread.ss_available)
            ; Printf.sprintf "%d participant%s" summary.Chat_pages.Start_thread.ss_participants (plural summary.Chat_pages.Start_thread.ss_participants) ]
          @ (if summary.Chat_pages.Start_thread.ss_unavailable = 0 then []
             else [ Printf.sprintf "%d unavailable" summary.Chat_pages.Start_thread.ss_unavailable ]) in
        let meta_html = String.concat " &middot; " meta_bits in
        let row (m : Thread_source_store.thread_source_msg) =
          if m.sm_deleted then
            "<div class='th-src-msg th-src-msg--gone'><span class='th-src-unavailable'>[message unavailable]</span></div>"
          else
            let name = if String.trim m.sm_author = "" then "[deleted]" else m.sm_author in
            let seed_badge = if m.sm_is_seed then "<span class='th-src-seed'>seed</span>" else "" in
            let chat_link = match channel_opt with
              | Some (cslug, _) ->
                  Printf.sprintf "<a class='th-src-jump' href='/c/%s/ch/%s?source_thread=%d#msg-%Ld'>view in chat</a>"
                    (esc post.community_slug) (esc cslug) post.id m.sm_id
              | None -> "" in
            Printf.sprintf
              "<div class='th-src-msg'><div class='th-src-msg-meta'><span class='th-src-author'>%s</span>%s<span class='th-src-time'>%s</span>%s</div><div class='th-src-text'>%s</div></div>"
              (esc name) seed_badge (esc (Chat_pages.Start_thread.minute_of_ts m.sm_created_at)) chat_link (esc m.sm_content) in
        let rows_html = String.concat "" (List.map row msgs) in
        let view_original = match channel_opt with
          | Some (cslug, _) ->
              Printf.sprintf
                "<div class='th-src-foot'><a href='/c/%s/ch/%s?source_thread=%d'>View original conversation &rarr;</a></div>"
                (esc post.community_slug) (esc cslug) post.id
          | None -> "" in
        Printf.sprintf
          "<div class='th-src'>\
             <div class='th-src-label'>Promoted conversation</div>\
             <div class='th-src-head'>%s</div>\
             <div class='th-src-meta'>%s</div>\
             <div class='th-src-msgs'>%s</div>\
             %s\
           </div>"
          from_html meta_html rows_html view_original in
  let body_html =
    let img = match post.image_url with
      | Some i when i <> "" -> Printf.sprintf "<div class='th-img'><img src='%s' alt='Post image'></div>" (esc i)
      | _ -> "" in
    let txt = match post.content with
      | Some c when String.trim c <> "" -> Printf.sprintf "<div class='th-body'>%s</div>" (esc c)
      | _ -> "" in
    let link = match post.url with
      | Some u -> Printf.sprintf "<div class='th-linkrow'><a class='btn sm' href='%s' target='_blank' rel='noopener'>Open link &#8599;</a></div>" (Components.safe_url u)
      | None -> "" in
    img ^ txt ^ link in

  (* --- comment composer (shell-styled) ---
     Gated by the one SQL participation capability the handler computed
     (origin membership, or membership in a currently readable accepted
     destination — minus tombstone and every ban), the same capability
     POST /comments enforces, so this render gate can never grant what the
     server refuses. A member who cannot currently comment (banned, or a
     tombstoned thread) gets neither a composer nor a misleading Join
     button; a logged-in non-member of a public community keeps the Join
     path — joining THIS displayed community is a real participation path
     in both contexts. *)
  let composer =
    if can_comment then
      Printf.sprintf "<form class='ct-composer' action='/comments' method='POST'>%s<input type='hidden' name='post_id' value='%d'>%s<textarea name='content' required rows='3' placeholder='Add to the thread&#8230;'></textarea><div class='ct-composer-actions'><button type='submit' class='btn sm primary'>Reply</button></div></form>" csrf_token post.id context_field
    else match current_user with
      | Some _ when is_member ->
          (* A current member without the capability: no composer, and no
             join CTA that could not help. *)
          ""
      (* Private community: viewer is an authorized non-member (mod/admin); no self-join button. *)
      | Some _ when community.visibility = Community_types.Community_private ->
          Printf.sprintf "<div class='ct-join'><span>Only members of <a href='/c/%s'>/c/%s</a> can reply.</span></div>"
            (esc community.slug) (esc community.slug)
      | Some _ ->
          Printf.sprintf "<div class='ct-join'><span>You must be a member of <a href='/c/%s'>/c/%s</a> to reply.</span><form action='/join' method='POST' class='inline'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn sm primary'>Join /c/%s</button></form></div>"
            (esc community.slug) (esc community.slug) csrf_token community.id
            local_thread_path (esc community.slug)
      | None ->
          "<div class='ct-join'><span><a href='/login'>Log in</a> to join the discussion.</span></div>" in

  (* --- comments, shell-styled. Mod/admin/ban dialogs copied verbatim from post_page. --- *)
  let rec render_comment_tree all_comments parent_id depth =
    let children = List.filter (fun (c : Comment_store.comment) -> c.parent_id = parent_id) all_comments in
    if children = [] then ""
    else String.concat "\n" (List.map (fun (c : Comment_store.comment) ->
      let nested = render_comment_tree all_comments (Some c.id) (depth + 1) in
      let cvote = Option.value ~default:0 (List.assoc_opt c.id user_comment_votes) in
      let up_color = if cvote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
      let down_color = if cvote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
      let up_action = if cvote = 1 then 0 else 1 in
      let down_action = if cvote = -1 then 0 else -1 in
      let upvote_html = match current_user with
        | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9650;</button></form>" csrf_token c.id up_action up_color
        | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500'>&#9650;</a>" in
      let downvote_html =
        if not post.allow_downvotes then ""
        else match current_user with
        | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9660;</button></form>" csrf_token c.id down_action down_color
        | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2]'>&#9660;</a>" in
      let cvote_pill = Printf.sprintf "<div class='cs-vote'>%s<span class='cs-vote-score'>%d</span>%s</div>" upvote_html c.score downvote_html in

      (* Same participation capability as the top composer — reply controls
         must never render for a viewer the POST would refuse. *)
      let reply_button = if can_comment then
          Printf.sprintf "<button type='button' class='ct-act' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\">&#8624; reply</button>" c.id
        else "" in
      let reply_form = if can_comment then
          Printf.sprintf "<form id='reply-form-%d' class='ct-composer ct-reply hidden' action='/comments' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='parent_id' value='%d'>%s<textarea name='content' required rows='2' placeholder='Write a reply&#8230;'></textarea><div class='ct-composer-actions'><button type='button' class='btn sm' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\">Cancel</button><button type='submit' class='btn sm primary'>Reply</button></div></form>" c.id csrf_token post.id c.id context_field c.id
        else "" in

      let is_comment_deleted =
        Components.is_deleted_user c.username
        || c.content = "[deleted]"
        || c.content = "[removed by admin]"
        || c.content = "[removed by moderator]" in
      let comment_target_is_admin = List.mem c.username admin_usernames in
      let delete_comment_btn =
        if is_comment_deleted then ""
        else match current_user with
        | None -> ""
        | Some u ->
            if u = c.username then
              Printf.sprintf "<form action='/delete-comment' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this comment? This action cannot be undone.')\">%s<input type='hidden' name='comment_id' value='%d'><button type='submit' class='ct-act ct-act-danger'>&#128465;&#65039;</button></form>" csrf_token c.id
            else if is_current_user_mod && not comment_target_is_admin then
              Printf.sprintf "
                <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='ct-act ct-act-mod'>&#128737;&#65039; Mod Remove</button>
                <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                  <div class='bg-white rounded-2xl overflow-hidden'>
                    <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                      <h3 class='text-base font-bold text-amber-900'>&#128737;&#65039; Moderator Removal</h3>
                      <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                    </div>
                    <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                      %s
                      <label class='flex flex-col gap-1.5'>
                        <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                        <textarea name='reason' required maxlength='255' rows='4'
                          placeholder='Explain why this comment is being removed (visible to the community)...'
                          class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                      </label>
                      <div class='flex justify-end gap-2 pt-1'>
                        <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                          class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                        <button type='submit'
                          class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                      </div>
                    </form>
                  </div>
                </dialog>"
                c.id c.id post.community_slug c.id csrf_token c.id
            else if is_admin && not comment_target_is_admin then
              Printf.sprintf "
                <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Remove</button>
                <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                  <div class='bg-white rounded-2xl overflow-hidden'>
                    <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                      <h3 class='text-base font-bold text-red-900'>&#9889; Admin Intervention</h3>
                      <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                    </div>
                    <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                      %s
                      <label class='flex flex-col gap-1.5'>
                        <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                        <textarea name='reason' required maxlength='255' rows='4'
                          placeholder='Explain the admin intervention reason (visible to the community)...'
                          class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                      </label>
                      <div class='flex justify-end gap-2 pt-1'>
                        <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                          class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                        <button type='submit'
                          class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                      </div>
                    </form>
                  </div>
                </dialog>"
                c.id c.id post.community_slug c.id csrf_token c.id
            else "" in
      let ban_comment_btn =
        if (is_current_user_mod || is_admin) && not (comment_target_is_admin && not is_admin) then
          match current_user with
          | Some u when u <> c.username && not (Components.is_deleted_user c.username) ->
              if List.mem c.username banned_usernames then
                "<span class='ct-act ct-act-danger'>&#128683; Banned</span>"
              else if is_current_user_mod then
                Printf.sprintf "
                  <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='ct-act ct-act-mod'>&#128296; Mod Ban</button>
                  <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-amber-900'>&#128296; Mod Ban</h3>
                        <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                      </div>
                      <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <input type='hidden' name='target_username' value='%s'>
                        <input type='hidden' name='community_id' value='%d'>
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required rows='4'
                            placeholder='Explain why this user is being banned (visible to the community)...'
                            class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id csrf_token (esc c.username) post.community_id c.id
              else
                Printf.sprintf "
                  <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Ban</button>
                  <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-red-900'>&#9889; Admin Ban</h3>
                        <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                      </div>
                      <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <input type='hidden' name='target_username' value='%s'>
                        <input type='hidden' name='community_id' value='%d'>
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required rows='4'
                            placeholder='Explain the admin intervention reason (visible to the community)...'
                            class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id csrf_token (esc c.username) post.community_id c.id
          | _ -> ""
        else "" in

      (* Quiet secondary "Report" affordance on each comment for logged-in non-authors.
         Visibility-only gate; the POST handler re-checks login / ban / self-report. *)
      let comment_report_btn =
        if is_comment_deleted then ""
        else match current_user with
        | Some u when u <> c.username ->
            Printf.sprintf "<a class='ct-act' href='/c/%s/report?type=comment&amp;id=%d'>&#9873; report</a>"
              (esc post.community_slug) c.id
        | _ -> "" in
      let op_badge = if c.username = post.username then "<span class='role op'>OP</span>" else "" in
      let toggle_btn = Printf.sprintf "<button type='button' class='ct-collapse' onclick='toggleComment(%d, this)'>[-]</button>" c.id in
      (* Cap the indent after a few levels so deep chains don't staircase off-screen; the thread
         structure is unchanged (parent_id recursion), only the left-rail indent flattens. *)
      let children_cls = if depth >= 3 then "cmt-children cmt-flat" else "cmt-children" in
      let children_div = if nested = "" then "" else Printf.sprintf "<div id='comment-children-%d' class='%s'>%s</div>" c.id children_cls nested in
      let reply_cls = if depth > 0 then " reply" else "" in
      (* children_div is a SIBLING of .cmt (not nested in .cbody) so each level indents only by the
         guide rail, not by the parent's vote column — keeps deep threads from staircasing.
         Collapse still works: toggleComment finds comment-children-<id> by id wherever it sits. *)
      Printf.sprintf "
      <div class='cmt%s'>
          %s
          <div class='cbody'>
              <div class='cmeta'>%s %s %s <span class='ctime'>%s</span></div>
              <div id='comment-content-%d'>
                  <div class='ctext'>%s</div>
                  <div class='cactions'>%s%s%s%s</div>
                  %s
              </div>
          </div>
      </div>%s"
      reply_cls
      cvote_pill
      toggle_btn (Components.render_author ~mod_usernames ~admin_usernames c.username) op_badge
      (esc (Components.time_ago c.created_at))
      c.id
      (esc c.content)
      reply_button delete_comment_btn ban_comment_btn comment_report_btn
      reply_form
      children_div
    ) children) in
  let comments_html =
    if comments = [] then "<div class='cs-empty'>No comments yet &mdash; start the discussion.</div>"
    else render_comment_tree comments None 0 in

  (* --- main pane --- *)
  (* Local tab row — only the real "Thread" view. Future Source-chat/Backlinks slots are
     intentionally omitted until those features exist (no unavailable tabs, no fake counts). *)
  let th_tabs =
    "<div class='th-tabs'><span class='th-tab active'>Thread</span></div>" in
  let topbar = Printf.sprintf "<div class='th-topbar'><div class='th-topbar-row'>%s%s</div>%s</div>"
    crumb
    (if back_link = "" then "" else Printf.sprintf "<div class='th-back'>%s</div>" back_link)
    th_tabs in
  (* Fixed-copy creation notice under the topbar. The failed variant links
     the existing Share page (server-built local path) so the author can try
     again without a composer re-render — never a resubmission of the form. *)
  let creation_notice_html = match creation_notice with
    | None -> ""
    | Some Creation_share_requested ->
        "<div class='notice notice--success'>Thread created and sharing requested. It appears in the other community only after its moderators approve the request.</div>"
    | Some Creation_share_failed ->
        Printf.sprintf
          "<div class='notice notice--warn'>Thread created, but the sharing request could not be sent. You can try again from the thread's <a href='%s/share'>Share page</a>.</div>"
          local_thread_path in
  let post_block = Printf.sprintf "
    <article class='th-main'>
        <div class='th-head'>
            %s
            <div class='th-headmain'>
                <h1 class='th-title'>%s</h1>
                %s
                %s
                %s
                %s
            </div>
        </div>
    </article>"
    vote_col (esc post.title) meta post_actions_row source_html body_html in
  let discussion = Printf.sprintf "
    <section class='th-discussion'>
        <div class='th-comments-label'>// %d comments</div>
        %s
        %s
    </section>"
    post.comment_count composer comments_html in
  (* .thread-shell-main is the readable column AND the scroll pane: it sits straight inside cs-main
     (no cs-main-body) and earde.css makes it flex:1 + overflow-y:auto, so the thread scrolls inside
     the viewport-locked app shell (same containment as feed/section/chat) — the topbar and the
     rail/sidebar/right rail stay fixed; only this pane scrolls. *)
  let main = Printf.sprintf "<div class='thread-shell-main'>%s%s%s%s</div>" topbar creation_notice_html post_block discussion in

  (* --- right rail: real data only. The Section rows are the LOCAL context:
     destination section under a destination shell, origin section at home. --- *)
  let section_row = match local_section with
    | Some (sn, ss) -> Printf.sprintf "<tr><td>Section</td><td class='num'><a href='/c/%s/s/%s'>%s</a></td></tr>" (esc community.slug) (esc ss) (esc sn)
    | None -> "<tr><td>Section</td><td class='num'>&mdash;</td></tr>" in
  let back_cta = match local_section with
    | Some (sn, ss) -> Printf.sprintf "<div class='ca-cta'><a class='btn sm block' href='/c/%s/s/%s'>&larr; Back to %s</a></div>" (esc community.slug) (esc ss) (esc sn)
    | None -> "" in
  let stats_block = Printf.sprintf "
    <div class='ca-block'>
        <div class='ca-label'>Thread &middot; indexed</div>
        <table class='ca-grid'><tbody>
            <tr><td>Community</td><td class='num'><a href='/c/%s'>/c/%s</a></td></tr>
            %s
            <tr><td>Created</td><td class='num'>%s</td></tr>
            <tr><td>Replies</td><td class='num'>%d</td></tr>
            <tr><td>Score</td><td class='num'>%d</td></tr>
        </tbody></table>
        %s
    </div>"
    (esc community.slug) (esc community.slug) section_row
    (esc (Components.time_ago post.created_at)) post.comment_count post.score back_cta in
  (* Participants: deduped, real authors from the loaded post + comments; tombstoned users skipped. *)
  let participants_block =
    let names = post.username :: List.map (fun (c : Comment_store.comment) -> c.username) comments in
    let seen = Hashtbl.create 16 in
    let uniq = List.filter (fun u ->
      if Components.is_deleted_user u || Hashtbl.mem seen u then false
      else (Hashtbl.add seen u (); true)) names in
    if uniq = [] then ""
    else
      let rows = String.concat "" (List.map (fun u ->
        let badge = if List.mem u mod_usernames then "<span class='role mod'>MOD</span>" else "" in
        Printf.sprintf "<div class='member'><a href='/u/%s'>%s</a>%s</div>" (esc u) (esc u) badge) uniq) in
      Printf.sprintf "<div class='ca-block'><div class='ca-label'>Participants</div>%s</div>" rows in
  (* Source participants: authors of the promoted chat conversation. A separately labeled
     group — deliberately NOT deduplicated against forum Participants above, because the
     two lists communicate different roles (spoke in the source chat vs. posted in the
     thread). Tombstoned/unavailable identities are skipped: there is no safe generic
     identity chip in the current UI, and the block's "N unavailable" note already covers
     the gap. Only rendered for viewers authorized to see the source (Ts_visible). *)
  let source_participants_block =
    match thread_source with
    | Some (Ts_visible (_, msgs)) ->
        let names =
          List.fold_left (fun acc (m : Thread_source_store.thread_source_msg) ->
            let a = String.trim m.sm_author in
            if m.sm_deleted || a = "" || List.mem a acc then acc else acc @ [a]) [] msgs in
        if names = [] then ""
        else
          let rows = String.concat "" (List.map (fun u ->
            Printf.sprintf "<div class='member'><a href='/u/%s'>%s</a></div>" (esc u) (esc u)) names) in
          Printf.sprintf "<div class='ca-block'><div class='ca-label'>Source participants</div>%s</div>" rows
    | _ -> "" in
  let right_pane = stats_block ^ participants_block ^ source_participants_block in

  (* --- head: canonical + meta description + page-scoped collapse script --- *)
  let canonical = Post_cards.canonical_thread_path post.community_slug post.id post.title in
  let excerpt =
    match post.content with
    | Some c when String.trim c <> "" ->
        let c = String.trim c in
        if String.length c > 155 then String.sub c 0 155 ^ "\xe2\x80\xa6" else c
    | _ -> (match post.url with Some u -> u | None -> post.title) in
  let head_extra = Printf.sprintf
    "<link rel='canonical' href='%s'><meta name='description' content='%s'><script src='/static/js/thread.js' defer></script>"
    (esc canonical) (esc excerpt) in

  (* The complete <main> element, built here so the launch wrapper can never interpose a
     box: .cs-main is a flex column whose only child (.thread-shell-main) owns the scroll,
     and for a private community the ph-no-capture replay guard rides on this element
     itself (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    Printf.sprintf "<main class='%s'>%s</main>"
      (if community.visibility = Community_types.Community_private then "cs-main ph-no-capture" else "cs-main")
      main
  in
  let aside = Printf.sprintf "<aside class='aside'>%s</aside>" right_pane in
  Community_shell.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~head_extra ~aside ~community ~sidebar ~page_class:"launch-community-thread"
    ~title:post.title ~main_el ()
