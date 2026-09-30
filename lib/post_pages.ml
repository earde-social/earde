open Html.Infix

(* The three /new-post states below (community chooser, join gate, creation
   form) moved onto the Cartographic launch shell (pass 15B). The create-*
   fragments are kept verbatim — every field name/id/value, the hidden
   community_id, the section <select>, the Dream CSRF tag and the legacy
   .create-shell marker are unchanged — and only the outer document moved onto
   Components.launch_app_page (page_class
   launch-post-creation), which owns the topbar, dark rail, analytics assets
   and the member behavior script. [rail_communities] carries the viewer's
   joined communities as the handler loaded them (post-authorization only);
   ordering and the rail tiles stay owned by the shared launch doc. The
   community-bound states keep their (id, visibility) analytics pair through
   the wrapper's [analytics_community], exactly what create_page received. *)
let choose_community_page ?user ?request ?(rail_communities = []) (communities : Community_types.community list) =
  let render_option (community : Community_types.community) =
    (Html.template "
    <a href='/new-post?community=%s' class='create-comm'>
        <span>
            <span class='create-comm-name'>%s</span>
            <span class='create-comm-slug'>/c/%s</span>
        </span>
        <span class='create-comm-go'>Post here &rarr;</span>
    </a>"
  [ (Html.text community.slug)
  ; (Html.text (community.name))
  ; (Html.text community.slug) ])
  in

  let list_html = (Html.join (Html.static "\n")) (List.map render_option communities) in

  let content = (Html.template "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Choose a community</h1>
            <p class='create-sub'>Select where you want to publish your post.</p>
        </div>

        <div class='create-list'>
            %s
        </div>

        <div class='create-foot'>
            Can't find the right place? <a href='/bring' class='create-link'>Connect a project</a>
        </div>
      </div>
    </div>"
  [ list_html ])
  in
  Page_shell.launch_app_page ?user ?request ~rail_communities
    ~page_class:"launch-post-creation" ~title:"Choose Community"
    ~content:(Html.template "<div class='create-shell'>%s</div>"
  [ content ]) ()

let join_to_post_page ?user ?(rail_communities = []) (community : Community_types.community) request =
  let csrf_token = Csrf_field.tag request in
  let content = (Html.template "
    <div class='create-wrap create-wrap--narrow'>
      <div class='create-panel'>
        <div class='create-gate'>
            <div class='create-gate-icon'>
                <svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><rect x='3' y='11' width='18' height='11' rx='2'/><path d='M7 11V7a5 5 0 0110 0v4'/></svg>
            </div>
            <h1 class='create-title'>Members only</h1>
            <p class='create-gate-text'>
                You must be a member of <span class='create-gate-name'>/c/%s</span> to create a post here.
            </p>

            <form action='/join' method='POST' class='create-form'>
                %s
                <input type='hidden' name='community_id' value='%s'>
                <input type='hidden' name='redirect_to' value='/new-post?community=%s'>

                <button type='submit' class='create-btn create-btn--block'>Join community &amp; post</button>
            </form>

            <div class='create-foot'>
                <a href='/' class='create-link'>Cancel and return home</a>
            </div>
        </div>
      </div>
    </div>"
  [ (Html.text (community.slug))
  ; csrf_token
  ; Html.int (community.id)
  ; (Html.text community.slug) ])
  in
  Page_shell.launch_app_page ?user ~request ~rail_communities
    ~analytics_community:(community.id, community.visibility)
    ~page_class:"launch-post-creation" ~title:("Join " ^ community.name)
    ~content:(Html.template "<div class='create-shell'>%s</div>"
  [ content ]) ()

(* GET form for "Start thread from chat", on the Cartographic launch shell
   (pass 16A). The seed message is shown selected + locked; nearby messages are
   checkboxes; the thread body is editable notes. SSR, normal POST, CSRF; the
   only JS is the pre-existing page-scoped selection counter below. The
   create-shell fragment (candidate ledger, hidden seed input, msg_<id>
   checkboxes, title/content/section fields) is pinned — only the outer
   document moved onto Components.launch_community_page. *)
let start_thread_form ?user ?error ?(rail_communities = [])
    ~(channels : Channel_store.channel list) ~can_manage
    ~(community : Community_types.community) ~(channel : Channel_store.channel)
    ~(seed_id : int64) ~(candidates : (Chat_store.chat_message * string option) list)
    ~(sections : Section_store.community_section list)
    ~(default_section_id : int) ~default_title ~default_body request =
  let csrf_token = Csrf_field.tag request in
  let action = Printf.sprintf "/c/%s/ch/%s/messages/%Ld/start-thread"
    (community.slug) (channel.slug) seed_id in
  let section_dropdown =
    if sections = [] then Html.empty
    else begin
      let options = (Html.join (Html.static "\n")) (List.map (fun (s : Section_store.community_section) ->
        let selected = if s.section_id = default_section_id then Html.static " selected" else Html.empty in
        (Html.template "<option value='%s'%s>%s</option>"
  [ Html.int (s.section_id)
  ; selected
  ; (Html.text (s.name)) ])
      ) sections) in
      (Html.template "
            <div class='create-field'>
                <label class='create-label'>Section <span class='req'>*</span></label>
                <select name='section_id' required class='create-select'>%s</select>
            </div>"
  [ options ])
    end
  in
  let render_candidate ((m : Chat_store.chat_message), (author : string option)) =
    let name = match author with Some u -> u | None -> "[deleted]" in
    let is_seed = m.id = seed_id in
    let checkbox =
      if is_seed then
        (Html.template "<input type='checkbox' checked disabled class='st-check'><input type='hidden' name='seed' value='%s'>"
  [ Html.int64 (m.id) ])
      else
        (Html.template "<input type='checkbox' name='msg_%s' value='on' class='st-check'>"
  [ Html.int64 (m.id) ])
    in
    let seed_tag = if is_seed then (Html.static "<span class='st-seed-tag'>seed</span>") else Html.empty in
    (Html.template "<label class='st-msg%s'>%s<span class='st-msg-body'><span class='st-msg-meta'><span class='st-msg-author'>%s</span><span class='st-msg-time'>%s</span>%s</span><span class='st-msg-text'>%s</span></span></label>"
  [ (if is_seed then (Html.static " st-msg--seed") else Html.empty)
  ; checkbox
  ; (Html.text (name))
  ; (Html.text (m.created_at))
  ; seed_tag
  ; (Html.text (m.content)) ])
  in
  let candidates_html =
    if candidates = [] then (Html.static "<p class='create-hint'>No nearby messages.</p>")
    else (Html.join (Html.static "\n")) (List.map render_candidate candidates)
  in
  let error_html = match error with
    | Some e when e <> "" -> (Html.template "<div class='st-error'>%s</div>"
  [ (Html.text (e)) ])
    | _ -> Html.empty in
  let content = (Html.template "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Start a thread from <span class='accent'>#%s</span></h1>
            <p class='create-sub'>Crystallize this chat conversation into a durable forum thread. The messages you select are attached as source context on the thread.</p>
        </div>
        %s
        <form action='%s' method='POST' class='create-form'>
            %s
            <div class='create-field'>
                <label class='create-label'>Source messages</label>
                <p class='create-hint'>The seed message is always included. Tick nearby messages to attach (up to 10 total). <span id='st-selection-count'></span></p>
                <div class='st-msglist'>%s</div>
            </div>
            %s
            <div class='create-field'>
                <label class='create-label'>Title <span class='req'>*</span></label>
                <input type='text' name='title' required maxlength='300' class='create-input' value='%s' placeholder='What is this thread about?'>
            </div>
            <div class='create-field'>
                <label class='create-label'>Introduction / context <span class='create-label-opt'>(optional)</span></label>
                <textarea name='content' class='create-textarea' style='min-height:160px;' placeholder='Why is this conversation worth keeping? Add any context for the forum.'>%s</textarea>
                <p class='create-hint'>The selected messages are shown on the thread automatically &mdash; this field is only for your own introduction.</p>
            </div>
            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Start thread</button>
                <a href='/c/%s/ch/%s' class='st-cancel'>Cancel</a>
            </div>
        </form>
      </div>
    </div>
    <script>
    (function () {
      var out = document.getElementById('st-selection-count');
      var list = document.querySelector('.st-msglist');
      if (!out || !list) return;
      function update() {
        var rows = list.querySelectorAll('.st-msg');
        var count = 0;
        var authors = {};
        rows.forEach(function (row) {
          var box = row.querySelector('.st-check');
          if (!box || !box.checked) return;
          count += 1;
          var a = row.querySelector('.st-msg-author');
          if (a) authors[a.textContent] = true;
        });
        var participants = Object.keys(authors).length;
        out.textContent = count + ' of 10 selected · ' +
          participants + (participants === 1 ? ' participant' : ' participants');
      }
      list.addEventListener('change', update);
      update();
    })();
    </script>"
  [ (Html.text (channel.name))
  ; error_html
  ; (Html.text action)
  ; csrf_token
  ; candidates_html
  ; section_dropdown
  ; (Html.text (default_title))
  ; (Html.text (default_body))
  ; (Html.text (community.slug))
  ; (Html.text (channel.slug)) ])
  in
  (* Cartographic launch shell (pass 16A): the same four-pane chrome as the
     sibling community routes, wrapping the promotion form verbatim — the
     create-* fields, the hidden seed input, the msg_<id> checkbox names and
     ordering, the section <select>, the Dream CSRF tag, the selection-count
     script and the legacy .create-shell marker are unchanged; only the outer
     document swapped create_page → launch_community_page. The mono context
     crumb is Cartographic chrome placed OUTSIDE the pinned fragment. The
     sidebar reuses the shared knowledge grammar with NO active entry —
     starting a thread is a contextual action, not a permanent sidebar
     destination — and the moderator-only entries stay hidden because their
     active flags default to false. can_manage is the handler's real
     admin-or-moderator check, so Settings shows only to authorized viewers
     (the settings handler re-checks). [rail_communities] carries the viewer's
     joined communities as the handler loaded them (post-authorization only);
     ordering, dedup by slug and the single active marker stay owned by the
     shared launch doc builder, which also owns the analytics assets with the
     same community-id/visibility pair the legacy wrapper received and marks
     .shell ph-no-capture for private communities. *)
  let crumb =
    (Html.template "<div class='launch-st-context'><a href='/c/%s'>/c/%s</a> <span class='launch-st-context__sep'>/</span> <a href='/c/%s/ch/%s'>#%s</a> <span class='launch-st-context__sep'>/</span> <b>start thread</b></div>"
  [ (Html.text (community.slug))
  ; (Html.text (community.slug))
  ; (Html.text (community.slug))
  ; (Html.text (channel.slug))
  ; (Html.text (channel.slug)) ])
  in
  let sidebar =
    Community_pages.launch_knowledge_sidebar ~community ~channels ~sections ~can_manage ()
  in
  Community_shell.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-start-thread"
    ~title:("Start thread — #" ^ channel.name)
    ~content:(crumb ++ (Html.template "<div class='create-shell'>%s</div>"
  [ content ])) ()

let new_post_form ?user ?preselected_section_id ?(rail_communities = [])
    ?(share_candidates : (string * string) list = [])
    (sections : Section_store.community_section list) (community : Community_types.community) request =
  let csrf_token = Csrf_field.tag request in
  let section_dropdown =
    if sections = [] then Html.empty
    else begin
      let options = (Html.join (Html.static "\n")) (List.map (fun (s : Section_store.community_section) ->
        let selected = match preselected_section_id with
          | Some id when id = s.section_id -> Html.static " selected"
          | _ -> Html.empty
        in
        (Html.template "<option value='%s'%s>%s</option>"
  [ Html.int (s.section_id)
  ; selected
  ; (Html.text (s.name)) ])
      ) sections) in
      (Html.template "
            <div class='create-field'>
                <label class='create-label'>Section <span class='req'>*</span></label>
                <select name='section_id' required class='create-select'>
                    <option value=''>-- Select a section --</option>
                    %s
                </select>
            </div>"
  [ options ])
    end
  in
  (* Optional Shared thread area (slice 4). Rendered only when at least one
     eligible connected destination exists — the candidates are the handler's
     server-resolved list and this markup decides nothing: POST /posts
     re-resolves the slug and the placement store revalidates connection,
     eligibility, and uniqueness under its own locks. The note is readable by
     its requester and by the authorized managers on either side of the
     request (not automatically by the thread author) and must never surface
     publicly, so the copy marks it private here at the point of entry. *)
  let share_area =
    if share_candidates = [] then Html.empty
    else begin
      let options = (Html.join (Html.static "\n")) (List.map (fun (slug, name) ->
        (Html.template "<option value='%s'>%s</option>"
  [ (Html.text (slug))
  ; (Html.text (name)) ])
      ) share_candidates) in
      (Html.template "
            <div class='create-field'>
                <label class='create-label' for='share-destination'>Share with a connected community <span class='create-label-opt'>(optional)</span></label>
                <select name='share_destination' id='share-destination' class='create-select'>
                    <option value='' selected>Do not share</option>
                    %s
                </select>
                <p class='create-hint'>Your thread is published in /c/%s immediately either way; it appears in the community you pick only after that community's moderators approve the request.</p>
            </div>

            <div class='create-field'>
                <label class='create-label' for='share-note'>Private request note <span class='create-label-opt'>(optional)</span></label>
                <textarea name='share_note' id='share-note' maxlength='2000' class='create-textarea' style='min-height:80px;' placeholder='A short note for the reviewing moderators (optional).'></textarea>
                <p class='create-hint'>Visible only to the requester and authorized moderators. Never shown on the thread, in feeds, or in notifications. Up to 2,000 characters.</p>
            </div>"
  [ options
  ; (Html.text (community.slug)) ])
    end
  in
  let content = (Html.template "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Post to <span class='accent'>/c/%s</span></h1>
            <p class='create-sub'>Share a link, an image, or a text discussion with the community.</p>
        </div>

        <form action='/posts' method='POST' enctype='multipart/form-data' class='create-form'>
            %s
            <input type='hidden' name='community_id' value='%s'>

            %s

            <div class='create-field'>
                <label class='create-label'>Title <span class='req'>*</span></label>
                <input type='text' name='title' required class='create-input' placeholder='An interesting title'>
            </div>

            <div class='create-field'>
                <label class='create-label'>URL <span class='create-label-opt'>(optional)</span></label>
                <input type='url' name='url' class='create-input' placeholder='https://example.com'>
            </div>

            <div class='create-field'>
                <label class='create-label'>Image <span class='create-label-opt'>(optional, max 5 MB)</span></label>
                <input type='file' name='image' accept='image/*' class='create-file'>
                <p class='create-hint'>Converted to WebP automatically. Leave blank for a text or link post.</p>
            </div>

            <div class='create-field'>
                <label class='create-label'>Text <span class='create-label-opt'>(optional)</span></label>
                <textarea name='content' class='create-textarea' style='min-height:140px;' placeholder='Share your thoughts...'></textarea>
            </div>
            %s

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Submit post</button>
            </div>
        </form>
      </div>
    </div>"
  [ (Html.text (community.slug))
  ; csrf_token
  ; Html.int (community.id)
  ; section_dropdown
  ; share_area ])
  in
  Page_shell.launch_app_page ?user ~request ~rail_communities
    ~analytics_community:(community.id, community.visibility)
    ~page_class:"launch-post-creation" ~title:("Post to " ^ community.name)
    ~content:(Html.template "<div class='create-shell'>%s</div>"
  [ content ]) ()

(* The legacy /p/:id fallback (pass 20A), reachable only for the pathological
   unmappable post (community_slug = "" — view_post_handler 301s every mappable
   post to its canonical thread URL before this renders). The inner warm-card
   fragment — post card, recursive comment tree, every /vote, /vote-comment,
   /comments, /delete-post, /delete-comment, mod_delete, /ban-community-user
   and /join form, the mod/admin/ban dialogs and the toggleComment script — is
   treated as pinned and kept byte-identical; only the outer document wrapper
   moved onto Components.launch_app_page under
   body.launch-legacy-post, skinned by the "legacy post fallback only" section
   of earde.css). [user_communities] (already loaded by the handler, previously
   ignored) now feeds the launch rail — VIEWER membership only, same source and
   order as every other launch surface; it never enters page content.
   [moderated_communities] stays unused. Members get the shared launch behavior
   script from the wrapper (confirmModal, copyPostLink, optimistic voting, one
   notification fetch); guests get the share-only script so the byte-pinned
   Share control keeps working, and zero notification fetches. *)
let post_page ?user ?(noindex=false) ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community ~user_communities ~moderated_communities:_ user_post_votes user_comment_votes (post : Post_types.post) (comments : Comment_store.comment list) request =
  let csrf_token = Csrf_field.tag request in
  let current_user = Dream.session_field request "username" in

  (* Recursive comment tree: children filtered at render time rather than
     pre-grouped in SQL to keep the query simple and avoid a recursive CTE. *)
  let rec render_comment_tree all_comments current_parent_id =
    let children = List.filter (fun (c : Comment_store.comment) -> c.parent_id = current_parent_id) all_comments in

    if children = [] then Html.empty
    else
      let children_html = List.map (fun (c : Comment_store.comment) ->
        let nested_html = render_comment_tree all_comments (Some c.id) in

        (* Reply button toggles a hidden form; splitting button from form keeps the
           action bar flex container clean — form spans full width below the bar. *)
        let reply_button =
          if is_member then
            (Html.template "<button type='button' onclick=\"document.getElementById('reply-form-%s').classList.toggle('hidden')\" class='flex items-center gap-1 text-xs font-bold text-gray-500 hover:text-gray-900 bg-transparent'>💬 Reply</button>"
  [ Html.int (c.id) ])
          else Html.empty
        in
        let reply_form_html =
          if is_member then
            (Html.template "
            <form id='reply-form-%s' action='/comments' method='POST' class='hidden w-full mt-3 mb-2'>
                %s
                <input type='hidden' name='post_id' value='%s'>
                <input type='hidden' name='parent_id' value='%s'>
                <textarea name='content' required rows='3' class='w-full p-3 border border-[#E0D9CC] rounded-xl shadow-sm focus:outline-none focus:ring-1 focus:ring-[#C94C4C] focus:border-[#C94C4C] text-sm' placeholder='Write a reply...'></textarea>
                <div class='flex justify-end gap-2 mt-2'>
                    <button type='button' onclick=\"document.getElementById('reply-form-%s').classList.toggle('hidden')\" class='text-sm text-gray-500 font-medium hover:text-gray-700 px-3 py-1.5'>Cancel</button>
                    <button type='submit' class='bg-[#C94C4C] text-white text-sm font-medium px-4 py-1.5 rounded-full hover:bg-[#A83A3A] transition'>Post Reply</button>
                </div>
            </form>"
  [ Html.int (c.id)
  ; csrf_token
  ; Html.int (post.id)
  ; Html.int (c.id)
  ; Html.int (c.id) ])
          else Html.empty
        in

        let current_vote = Option.value ~default:0 (List.assoc_opt c.id user_comment_votes) in

        let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
        let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in

        let up_action = if current_vote = 1 then 0 else 1 in
        let down_action = if current_vote = -1 then 0 else -1 in

        let is_admin = Dream.session_field request "is_admin" = Some "true" in
        (* Tombstone sentinels written by soft_delete_comment / admin_delete_comment. *)
        let is_comment_deleted =
          Components.is_deleted_user c.username
          || c.content = "[deleted]"
          || c.content = "[removed by admin]"
          || c.content = "[removed by moderator]"
        in
        let comment_target_is_admin = List.mem c.username admin_usernames in
        (* Rule A/B/C: strictly mutually exclusive — mirrors post action_btn logic.
           Rule A: own comment → personal Delete. Rule B: mod (not own, not admin target) → Mod Remove dialog.
           Rule C: admin acting without mod role (not own, not admin target) → Admin Remove dialog. *)
        let delete_comment_btn =
          if is_comment_deleted then Html.empty
          else match current_user with
          | None -> Html.empty
          | Some u ->
              if u = c.username then
                (* Rule A: personal delete — no audit trail needed *)
                (Html.template "<form action='/delete-comment' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this comment? This action cannot be undone.')\">
                    %s <input type='hidden' name='comment_id' value='%s'>
                    <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold'>🗑️</button>
                </form>"
  [ csrf_token
  ; Html.int (c.id) ])
              else if is_current_user_mod && not comment_target_is_admin then
                (* Rule B: mod removal — dialog enforces a public reason in the mod log *)
                (Html.template "
                  <button onclick=\"document.getElementById('mod-modal-comment-%s').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🛡️ Mod Remove</button>
                  <dialog id='mod-modal-comment-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-amber-900'>🛡️ Moderator Removal</h3>
                        <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                      </div>
                      <form action='/c/%s/comments/%s/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required maxlength='255' rows='4'
                            placeholder='Explain why this comment is being removed (visible to the community)...'
                            class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('mod-modal-comment-%s').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
  [ Html.int (c.id)
  ; Html.int (c.id)
  ; (Html.text post.community_slug)
  ; Html.int (c.id)
  ; csrf_token
  ; Html.int (c.id) ])
              else if is_admin && not comment_target_is_admin then
                (* Rule C: admin override — logged as admin_delete_comment in mod_actions *)
                (Html.template "
                  <button onclick=\"document.getElementById('mod-modal-comment-%s').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Remove</button>
                  <dialog id='mod-modal-comment-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-red-900'>⚡ Admin Intervention</h3>
                        <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                      </div>
                      <form action='/c/%s/comments/%s/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required maxlength='255' rows='4'
                            placeholder='Explain the admin intervention reason (visible to the community)...'
                            class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('mod-modal-comment-%s').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
  [ Html.int (c.id)
  ; Html.int (c.id)
  ; (Html.text post.community_slug)
  ; Html.int (c.id)
  ; csrf_token
  ; Html.int (c.id) ])
              else Html.empty
        in

        (* Ban button mirrors render_post's ban_btn logic; closed over post.community_id.
           banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
           Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority. *)
        let ban_comment_btn =
          if (is_current_user_mod || is_admin) && not (comment_target_is_admin && not is_admin) then
            match current_user with
            | Some u when u <> c.username && not (Components.is_deleted_user c.username) ->
                if List.mem c.username banned_usernames then
                  (Html.static "<span class='text-xs text-red-600 font-bold'>🚫 Banned</span>")
                else if is_current_user_mod then
                  (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
                  (Html.template "
                    <button onclick=\"document.getElementById('ban-modal-comment-%s').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🔨 Mod Ban</button>
                    <dialog id='ban-modal-comment-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                      <div class='bg-white rounded-2xl overflow-hidden'>
                        <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                          <h3 class='text-base font-bold text-amber-900'>🔨 Mod Ban</h3>
                          <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                        </div>
                        <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                          %s
                          <input type='hidden' name='target_username' value='%s'>
                          <input type='hidden' name='community_id' value='%s'>
                          <label class='flex flex-col gap-1.5'>
                            <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                            <textarea name='reason' required rows='4'
                              placeholder='Explain why this user is being banned (visible to the community)...'
                              class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                          </label>
                          <div class='flex justify-end gap-2 pt-1'>
                            <button type='button' onclick=\"document.getElementById('ban-modal-comment-%s').close()\"
                              class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                            <button type='submit'
                              class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                          </div>
                        </form>
                      </div>
                    </dialog>"
  [ Html.int (c.id)
  ; Html.int (c.id)
  ; csrf_token
  ; (Html.text (c.username))
  ; Html.int (post.community_id)
  ; Html.int (c.id) ])
                else
                  (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
                  (Html.template "
                    <button onclick=\"document.getElementById('ban-modal-comment-%s').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Ban</button>
                    <dialog id='ban-modal-comment-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                      <div class='bg-white rounded-2xl overflow-hidden'>
                        <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                          <h3 class='text-base font-bold text-red-900'>⚡ Admin Ban</h3>
                          <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                        </div>
                        <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                          %s
                          <input type='hidden' name='target_username' value='%s'>
                          <input type='hidden' name='community_id' value='%s'>
                          <label class='flex flex-col gap-1.5'>
                            <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                            <textarea name='reason' required rows='4'
                              placeholder='Explain the admin intervention reason (visible to the community)...'
                              class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                          </label>
                          <div class='flex justify-end gap-2 pt-1'>
                            <button type='button' onclick=\"document.getElementById('ban-modal-comment-%s').close()\"
                              class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                            <button type='submit'
                              class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                          </div>
                        </form>
                      </div>
                    </dialog>"
  [ Html.int (c.id)
  ; Html.int (c.id)
  ; csrf_token
  ; (Html.text (c.username))
  ; Html.int (post.community_id)
  ; Html.int (c.id) ])
            | _ -> Html.empty
          else Html.empty
        in

        (* Html.image_src (via user_avatar) replaces the prior raw interpolation of the stored
           avatar_url; same markup, same letter-tile fallback. *)
        let avatar_html =
          Components.user_avatar ~alt:"Avatar"
            ~img_class:"w-7 h-7 rounded-full object-cover shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] flex-shrink-0"
            ~tile_class:"w-7 h-7 bg-[#DFF5F8] rounded-full flex items-center justify-center text-xs text-[#C94C4C] font-bold shadow-sm border border-[#A8DDE8] flex-shrink-0"
            ~username:c.username c.avatar_url
        in

        let upvote_html = match current_user with
          | Some _ -> (Html.template "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%s'><input type='hidden' name='direction' value='%s'><button type='submit' class='%s text-xs font-bold leading-none'>▲</button></form>"
  [ csrf_token
  ; Html.int (c.id)
  ; Html.int (up_action)
  ; (Html.text up_color) ])
          | None -> (Html.static "<a href='/login' class='text-gray-400 hover:text-orange-500 text-xs font-bold leading-none'>▲</a>")
        in
        let downvote_html =
          if not post.allow_downvotes then Html.empty
          else match current_user with
          | Some _ -> (Html.template "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%s'><input type='hidden' name='direction' value='%s'><button type='submit' class='%s text-xs font-bold leading-none'>▼</button></form>"
  [ csrf_token
  ; Html.int (c.id)
  ; Html.int (down_action)
  ; (Html.text down_color) ])
          | None -> (Html.static "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] text-xs font-bold leading-none'>▼</a>")
        in

        let op_badge =
          if c.username = post.username then
            (Html.static "<span class='ml-1.5 font-bold text-[10px] bg-[#DFF5F8] text-[#69C3D2] px-1.5 py-0.5 rounded'>OP</span>")
          else Html.empty
        in

        (* Separate IDs for content and children so toggleComment can collapse each independently. *)
        let toggle_btn = (Html.template "<button type='button' onclick='toggleComment(%s, this)' class='text-xs text-gray-400 hover:text-gray-700 font-mono transition-colors'>[-]</button>"
  [ Html.int (c.id) ])
        in

        (* comment-children omitted when empty so toggleComment null-guards cleanly.
           ml-2.5 aligns the thread line under the avatar (w-5 = 1.25rem = ml-2.5 + half border). *)
        let comment_children_div =
          if nested_html = Html.empty then Html.empty
          else (Html.template "<div id='comment-children-%s' class='pl-3 border-l-2 border-[#E0D9CC] ml-2.5 mt-2'>%s</div>"
  [ Html.int (c.id)
  ; nested_html ])
        in

        let total_comment_contribs = c.author_local_post_count + c.author_local_comment_count in
        let comment_active_since = match c.author_first_active_at with
          | None | Some "" -> ""
          | Some ts -> Printf.sprintf " · active since %s" (Components.format_month_year ts)
        in
        let comment_local_stats = (Html.template "<span class='font-semibold text-red-700'>%s</span> local karma · <span class='font-semibold text-red-700'>%s</span> contributions here%s"
  [ Html.int (c.author_local_karma)
  ; Html.int (total_comment_contribs)
  ; (Html.text comment_active_since) ])
        in
        (Html.template "
        <div class='mt-3 hover:bg-[#EDE9DF] transition rounded-r pr-2 py-1'>
            <div class='flex items-start gap-2 mb-1'>
                %s
                %s
                <div class='min-w-0 flex-1'>
                    <div class='flex flex-wrap items-center gap-1 text-xs text-gray-500'>
                        %s%s
                        <span class='text-gray-400'>•</span>
                        <span class='text-gray-400'>%s</span>
                        %s
                        %s
                    </div>
                    <div class='text-xs text-gray-400 mt-0.5'>%s</div>
                </div>
            </div>
            <div id='comment-content-%s'>
                <div class='text-sm text-gray-900 whitespace-pre-wrap break-words'>%s</div>
                <div class='flex items-center gap-3 mt-2'>
                    <div class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-2 py-0.5'>
                        %s
                        <span class='text-xs font-semibold text-gray-700'>%s</span>
                        %s
                    </div>
                    %s
                </div>
                %s
            </div>
            %s
        </div>"
  [ toggle_btn
  ; avatar_html
  ; (Components.render_author ~mod_usernames ~admin_usernames c.username)
  ; op_badge
  ; (Html.text (Components.time_ago c.created_at))
  ; delete_comment_btn
  ; ban_comment_btn
  ; comment_local_stats
  ; Html.int (c.id)
  ; (Html.text (c.content))
  ; upvote_html
  ; Html.int (c.score)
  ; downvote_html
  ; reply_button
  ; reply_form_html
  ; comment_children_div ])
      ) children in
      (Html.join (Html.static "\n")) children_html
  in

  let comments_html =
    if comments = [] then (Html.static "<p class='text-gray-500 italic mt-4'>No comments yet.</p>")
    else render_comment_tree comments None
  in

  let action_section =
    if is_member then
      (Html.template "
      <form action='/comments' method='POST' class='mt-0'>
          %s
          <input type='hidden' name='post_id' value='%s'>
          <textarea name='content' required rows='2' class='w-full border border-[#E0D9CC] rounded-xl px-4 py-3 text-sm bg-gray-50 focus:bg-white focus:ring-1 focus:ring-[#C94C4C] focus:border-[#C94C4C] resize-y transition-colors placeholder-gray-400' placeholder='Add a comment...'></textarea>
          <div class='flex justify-end mt-2 mb-8'>
              <button type='submit' class='px-4 py-1.5 text-sm font-medium bg-[#C94C4C] text-white rounded-full hover:bg-[#A83A3A] transition-colors shadow-sm'>Comment</button>
          </div>
      </form>"
  [ csrf_token
  ; Html.int (post.id) ])
    else
      (Html.template "
      <div class='mt-6 mb-8 p-6 bg-[#F0EDE4] rounded-xl border border-[#E8E2D9] text-center'>
          <h3 class='text-gray-900 font-bold mb-2'>Join the discussion</h3>
          <p class='text-[#69C3D2] text-sm mb-4'>You must be a member of /c/%s to comment.</p>
          <form action='/join' method='POST'>
              %s
              <input type='hidden' name='community_id' value='%s'>
              <input type='hidden' name='redirect_to' value='/p/%s'>
              <button type='submit' class='bg-[#C94C4C] text-white px-6 py-2 rounded-full font-bold hover:bg-[#A83A3A] transition shadow-sm'>Join /c/%s</button>
          </form>
      </div>"
  [ (Html.text post.community_slug)
  ; csrf_token
  ; Html.int (post.community_id)
  ; Html.int (post.id)
  ; (Html.text post.community_slug) ])
  in

  let post_content = match post.content with | Some c -> (Html.template "<div class='ph-mask text-sm text-gray-800 leading-relaxed whitespace-pre-wrap break-words mt-2 mb-3'>%s</div>"
  [ (Html.text (c)) ]) | None -> Html.empty in
  let link_content = match post.url with | Some u -> (Html.template "<div class='mb-6'><a href='%s' target='_blank' class='text-blue-600 hover:underline break-all'>🔗 %s</a></div>"
  [ (Html.external_url (u))
  ; (Html.text (u)) ]) | None -> Html.empty in
  (* Image stored as /static/uploads/<uuid>.webp — served directly by Dream.static. *)
  let image_content = match post.image_url with
    | None -> Html.empty
    | Some img -> (Html.template "<div class='mb-4'><img src='%s' alt='Post image' class='w-full max-h-[700px] object-contain bg-stone-900 rounded-xl border border-[#E0D9CC]'></div>"
  [ (Html.text (img)) ])
  in

  let current_vote_direction = match List.assoc_opt post.id user_post_votes with Some d -> d | None -> 0 in
  let up_color = if current_vote_direction = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote_direction = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote_direction = 1 then 0 else 1 in
  let down_action = if current_vote_direction = -1 then 0 else -1 in
  (* Voting pill mirrors the pattern in components.ml render_post: toggle by sending
     direction=0 when already voted, avoiding a separate undo endpoint.
     Comments and Share pills share the same action bar to keep post metadata
     actions cohesive and avoid redundant top-of-post share button placement. *)
  let comments_pill =
    (Html.template "<div class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5 text-sm font-medium text-gray-700'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z'/></svg>%s</div>"
  [ Html.int (post.comment_count) ])
  in
  let share_pill =
    (Html.template "<button type='button' data-share-path='/p/%s' onclick='copyPostLink(this.dataset.sharePath, this)' class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5 text-sm font-medium text-gray-700 hover:bg-gray-100 transition-colors cursor-pointer'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z'/></svg>Share</button>"
  [ Html.int (post.id) ])
  in
  let voting_pill =
    let downvote_btn_logged_in =
      if post.allow_downvotes then
        (Html.template "<form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%s'><input type='hidden' name='direction' value='%s'><button type='submit' class='%s text-sm font-bold leading-none'>▼</button></form>"
  [ csrf_token
  ; Html.int (post.id)
  ; Html.int (down_action)
  ; (Html.text down_color) ])
      else Html.empty
    in
    let downvote_btn_logged_out =
      if post.allow_downvotes then (Html.static "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] text-sm font-bold leading-none'>▼</a>")
      else Html.empty
    in
    match current_user with
    | Some _ ->
        (Html.template "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5'><form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%s'><input type='hidden' name='direction' value='%s'><button type='submit' class='%s text-sm font-bold leading-none'>▲</button></form><span class='text-sm font-semibold text-gray-700'>%s</span>%s</div>%s%s</div>"
  [ csrf_token
  ; Html.int (post.id)
  ; Html.int (up_action)
  ; (Html.text up_color)
  ; Html.int (post.score)
  ; downvote_btn_logged_in
  ; comments_pill
  ; share_pill ])
    | None ->
        (Html.template "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5'><a href='/login' class='text-gray-400 hover:text-orange-500 text-sm font-bold leading-none'>▲</a><span class='text-sm font-semibold text-gray-700'>%s</span>%s</div>%s%s</div>"
  [ Html.int (post.score)
  ; downvote_btn_logged_out
  ; comments_pill
  ; share_pill ])
  in

  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  (* Sentinels match soft_delete_post / admin_delete_post exactly.
     post.content is string option — None means a link post with no body (not deleted). *)
  let is_post_deleted =
    (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_")
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let post_target_is_admin = List.mem post.username admin_usernames in
  (* Rule A/B/C: strictly mutually exclusive — prevents admins silently using the personal
     Delete path to avoid the public mod log (admin spoofing). No two rules fire at once.
     Rule A: own post → personal Delete. Rule B: mod/top_mod (not own post) → Mod Remove dialog.
     Rule C: admin acting without mod role (not own post) → Admin Remove dialog. *)
  let post_action_btn =
    if is_post_deleted then Html.empty
    else match current_user with
    | None -> Html.empty
    | Some u ->
        if u = post.username then
          (* Rule A: personal delete — no audit required *)
          (Html.template "<form action='/delete-post' method='POST' class='inline m-0 p-0 ml-3' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%s'>
              <button type='submit' class='text-sm text-red-500 hover:text-red-700 font-bold'>🗑️ Delete</button>
          </form>"
  [ csrf_token
  ; Html.int (post.id) ])
        else if is_current_user_mod && not post_target_is_admin then
          (* Rule B: mod/top_mod removal — dialog enforces a public reason *)
          (Html.template "
            <button onclick=\"document.getElementById('mod-modal-%s').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-1 transition-colors ml-3'>🛡️ Mod Remove</button>
            <dialog id='mod-modal-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-amber-900'>🛡️ Moderator Removal</h3>
                  <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                </div>
                <form action='/c/%s/posts/%s/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain why this post is being removed (visible to the community)...'
                      class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%s').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
  [ Html.int (post.id)
  ; Html.int (post.id)
  ; (Html.text post.community_slug)
  ; Html.int (post.id)
  ; csrf_token
  ; Html.int (post.id) ])
        else if is_admin && not post_target_is_admin then
          (* Rule C: admin override (not a mod of this community) — logged as admin_delete_post *)
          (Html.template "
            <button onclick=\"document.getElementById('mod-modal-%s').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-1 transition-colors ml-3'>⚡ Admin Remove</button>
            <dialog id='mod-modal-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-red-900'>⚡ Admin Intervention</h3>
                  <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                </div>
                <form action='/c/%s/posts/%s/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain the admin intervention reason (visible to the community)...'
                      class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%s').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
  [ Html.int (post.id)
  ; Html.int (post.id)
  ; (Html.text post.community_slug)
  ; Html.int (post.id)
  ; csrf_token
  ; Html.int (post.id) ])
        else Html.empty
  in

  (* post_page renders the post inline, not via Components.render_post, so ban_post_btn
     must be computed here separately — same guard logic as components.ml's ban_btn.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority. *)
  let ban_post_btn =
    if (is_current_user_mod || is_admin) && not (post_target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username
          && not (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_") ->
          if List.mem post.username banned_usernames then
            (Html.static "<span class='text-sm text-red-600 font-bold ml-1'>🚫 Banned</span>")
          else if is_current_user_mod then
            (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
            (Html.template "
              <button onclick=\"document.getElementById('ban-modal-postpage-%s').showModal()\" class='text-sm font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors ml-1'>🔨 Mod Ban</button>
              <dialog id='ban-modal-postpage-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-amber-900'>🔨 Mod Ban</h3>
                    <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%s'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain why this user is being banned (visible to the community)...'
                        class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%s').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
  [ Html.int (post.id)
  ; Html.int (post.id)
  ; csrf_token
  ; (Html.text (post.username))
  ; Html.int (post.community_id)
  ; Html.int (post.id) ])
          else
            (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
            (Html.template "
              <button onclick=\"document.getElementById('ban-modal-postpage-%s').showModal()\" class='text-sm font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors ml-1'>⚡ Admin Ban</button>
              <dialog id='ban-modal-postpage-%s' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-red-900'>⚡ Admin Ban</h3>
                    <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%s'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain the admin intervention reason (visible to the community)...'
                        class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%s').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
  [ Html.int (post.id)
  ; Html.int (post.id)
  ; csrf_token
  ; (Html.text (post.username))
  ; Html.int (post.community_id)
  ; Html.int (post.id) ])
      | _ -> Html.empty
    else Html.empty
  in

  let post_rules_html =
    match community.Community_types.rules with
    | Some rules when rules <> "" ->
        (Html.template "<div class='mt-4 pt-4 border-t border-gray-100'><h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-2'>Rules</h3><p class='text-xs text-gray-600 whitespace-pre-wrap'>%s</p></div>"
  [ (Html.text (rules)) ])
    | _ -> Html.empty
  in

  let post_mods_html =
    if mod_usernames = [] then (Html.static "<p class='text-xs text-gray-400 italic'>No moderators yet.</p>")
    else
      let links = (Html.join (Html.static "\n")) (List.map (fun u ->
        (Html.template "<li><a href='/u/%s' class='text-sm text-gray-700 hover:text-[#C94C4C] transition'>u/%s</a></li>"
  [ (Html.text (u))
  ; (Html.text (u)) ])
      ) mod_usernames) in
      (Html.template "<ul class='space-y-1'>%s</ul>"
  [ links ])
  in

  let post_right_sidebar = (Html.template "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h2 class='font-bold text-gray-900 mb-1'>%s</h2>
        <div class='text-xs text-[#C94C4C] font-mono mb-3'>/c/%s</div>
        <p class='text-sm text-gray-600'>%s</p>
        %s
        <a href='/new-post?community=%s' class='bg-[#C94C4C] text-white rounded-xl px-4 py-2 w-full block text-center mt-4 hover:bg-[#A83A3A] transition text-sm font-semibold'>+ Create Post</a>
        <a href='/c/%s/modlog' class='mt-2 flex items-center gap-2 text-sm text-gray-400 hover:text-gray-600 transition-colors'>
            <span>&#128220;</span><span>Public Modlog</span>
        </a>
        <div class='mt-4 pt-4 border-t border-gray-100'>
            <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-2'>Moderators</h3>
            %s
        </div>
    </div>"
  [ (Html.text (community.name))
  ; (Html.text (community.slug))
  ; (Html.text ((Option.value ~default:"No description." community.description)))
  ; post_rules_html
  ; (Html.text (community.slug))
  ; (Html.text (community.slug))
  ; post_mods_html ])
  in

  (* Escaped like every other letter-tile initial (Components.initial_tile and
     the launch moderator rows already do this): the byte is taken from a
     stored username, so a legacy account whose name starts with "<" must not
     put a raw markup character into the document. *)
  let post_author_initial =
    if is_post_deleted then "?"
    else
      ((String.uppercase_ascii (String.sub post.username 0 1)))
  in
  let post_local_stats_html =
    let total_contribs = post.author_local_post_count + post.author_local_comment_count in
    let active_since_part = match post.author_first_active_at with
      | None | Some "" -> ""
      | Some ts -> Printf.sprintf " · active since %s" (Components.format_month_year ts)
    in
    (Html.template "<span class='font-semibold text-red-700'>%s</span> local karma · <span class='font-semibold text-red-700'>%s</span> contributions here%s"
  [ Html.int (post.author_local_karma)
  ; Html.int (total_contribs)
  ; (Html.text active_since_part) ])
  in
  let content = (Html.template "
    <div class='flex flex-col lg:flex-row gap-6 items-start xl:translate-x-20 2xl:translate-x-24'>
        <div class='min-w-0 flex-1'>
            <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-6 mb-6'>
                <div class='mb-4'>
                    <div class='flex items-center gap-1 flex-wrap text-sm mb-3'>
                        <a href='/c/%s' class='font-bold text-[#C94C4C] hover:underline'>/c/%s</a>
                        %s
                    </div>
                    <div class='flex items-start gap-3 mb-3'>
                        <div class='w-8 h-8 bg-[#DFF5F8] rounded-full flex items-center justify-center text-sm text-[#C94C4C] font-bold border border-[#A8DDE8] flex-shrink-0'>%s</div>
                        <div class='min-w-0 flex-1'>
                            <div class='flex flex-wrap items-center gap-1.5 text-sm text-gray-700'>
                                %s
                                <span class='text-gray-400'>·</span>
                                <span class='text-gray-500'>%s</span>
                                %s
                            </div>
                            <div class='text-xs text-gray-500 mt-0.5'>%s</div>
                        </div>
                    </div>
                    <h1 class='text-lg md:text-xl font-bold text-gray-900 mb-3 leading-snug'>%s</h1>
                </div>
                %s
                %s
                %s
                %s
                %s
                <div>%s</div>
            </div>
        </div>
        <div class='w-80 hidden lg:flex flex-col gap-6 self-start sticky top-20 h-[calc(100vh-5rem)] overflow-y-auto pb-8 [&::-webkit-scrollbar]:w-1.5 [&::-webkit-scrollbar-track]:bg-transparent [&::-webkit-scrollbar-thumb]:bg-transparent hover:[&::-webkit-scrollbar-thumb]:bg-gray-300 [&::-webkit-scrollbar-thumb]:rounded-full'>
            %s
        </div>
    </div>"
  [ (Html.text post.community_slug)
  ; (Html.text post.community_slug)
  ; (match post.section_name, post.section_slug with
     | Some sn, Some ss ->
         (Html.template "<span class='text-gray-400 mx-1'>›</span><a href='/c/%s/s/%s' class='font-bold text-[#C94C4C] hover:underline'>%s</a>"
  [ (Html.text (post.community_slug))
  ; (Html.text (ss))
  ; (Html.text (sn)) ])
     | _ when post.community_sections_enabled ->
         (Html.template "<span class='text-gray-400 mx-1'>›</span><a href='/c/%s/s/uncategorized' class='font-bold text-[#C94C4C] hover:underline'>Uncategorized</a>"
  [ (Html.text (post.community_slug)) ])
     | _ -> Html.empty)
  ; (Html.text post_author_initial)
  ; (Components.render_author ~mod_usernames ~admin_usernames post.username)
  ; (Html.text (Components.time_ago post.created_at))
  ; (post_action_btn ++ ban_post_btn)
  ; post_local_stats_html
  ; (Html.text (post.title))
  ; image_content
  ; link_content
  ; post_content
  ; voting_pill
  ; action_section
  ; comments_html
  ; post_right_sidebar ])
  in
  (* Inline script keeps post_page self-contained; prepended so the function
     is defined before any onclick fires (no DOMContentLoaded needed). *)
  let toggle_script = (Html.static {|<script>
function toggleComment(id, btn) {
  const content = document.getElementById('comment-content-' + id);
  const children = document.getElementById('comment-children-' + id);
  const isCollapsed = content.classList.contains('hidden');
  if (isCollapsed) {
    content.classList.remove('hidden');
    if (children) children.classList.remove('hidden');
    btn.innerText = '[-]';
  } else {
    content.classList.add('hidden');
    if (children) children.classList.add('hidden');
    btn.innerText = '[+]';
  }
}
</script>|}) in
  (* The byte-pinned voting_pill shows Share to every viewer; only member
     documents carry the wrapper's behavior script (which defines
     copyPostLink). Guests get the share-only script — same single
     copyPostLink source, no notification fetch — appended after the pinned
     fragment so each document holds exactly one definition. *)
  let guest_share_script = match user with
    | None -> Page_shell.launch_share_script
    | Some _ -> Html.empty
  in
  Page_shell.launch_app_page ?user ~noindex ~request
    ~rail_communities:user_communities
    ~analytics_community:(post.community_id, community.visibility)
    ~page_class:"launch-legacy-post" ~title:post.title
    ~content:(toggle_script ++ content ++ guest_share_script) ()
