open Html.Infix

(* Session reads (current user, is_admin) happen inside a render function to avoid
   threading extra parameters through every call site — coupling is contained here. *)
(* is_current_user_mod and mod_usernames are optional — callers without community context
   (home feed, profile, search) omit them; they default to false/[] so delete
   visibility and author badges behave identically to before on those pages. *)
(* Shared post moderation/admin action markup: own-post Delete (Rule A), Mod/Admin Remove
   dialog (Rule B/C), and Mod/Admin Ban dialog. Lifted verbatim out of render_post so the
   exact same forms, routes, CSRF tags, confirm/dialog behavior, required-reason fields and
   Rule A/B/C visibility can be reused by other surfaces (e.g. search results) with zero
   drift. Returns "" when the viewer has no actionable controls. csrf_token is passed in so
   the caller computes it once and shares it with its other forms. *)
let post_admin_actions ?(is_current_user_mod = false) ?(admin_usernames = [])
    ?(banned_usernames = []) ~csrf_token request (post : Post_types.post) =
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  (* Tombstone check: both the username prefix (anonymize_user) and content sentinels signal
     a deleted post. Showing a delete button on a tombstone is misleading — the action would
     no-op or error, and could confuse mods into thinking a second deletion is needed. *)
  let is_already_deleted =
    Components.is_deleted_user post.username
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let target_is_admin = List.mem post.username admin_usernames in
  (* Rule A/B/C: strictly mutually exclusive — prevents admins silently using the
     personal Delete path to avoid the public mod log (admin spoofing).
     Rule A: own post → personal Delete. Rule B: mod/top_mod (not own post, not admin-only)
     → Mod Remove dialog. Rule C: admin acting without mod role → Admin Remove dialog. *)
  let action_btn =
    if is_already_deleted then Html.empty
    else
      match current_user with
      | None -> Html.empty
      | Some u ->
          if u = post.username then
            (* Rule A: personal delete — no audit required *)
            Html.template
              "<form action='/delete-post' method='POST' class='inline m-0 p-0 \
               ml-2' onsubmit=\"confirmModal(event, 'Do you really want to \
               delete this post? This action cannot be undone.')\">\n\
              \              %s <input type='hidden' name='post_id' value='%s'>\n\
              \              <button type='submit' class='text-xs \
               text-gray-400 hover:text-red-700 opacity-50 hover:opacity-100 \
               transition'>🗑️</button>\n\
              \          </form>"
              [ csrf_token; Html.int post.id ]
          else if is_current_user_mod && not target_is_admin then
            (* Rule B: mod/top_mod removal — dialog enforces public reason *)
            Html.template
              "\n\
              \            <button \
               onclick=\"document.getElementById('mod-modal-%s').showModal()\" \
               class='text-xs font-bold text-amber-700 hover:text-amber-900 \
               border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded \
               px-2 py-0.5 transition-colors ml-2'>🛡️ Mod Remove</button>\n\
              \            <dialog id='mod-modal-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \              <div class='bg-white rounded-2xl overflow-hidden'>\n\
              \                <div class='bg-amber-50 border-b \
               border-amber-200 px-6 py-4'>\n\
              \                  <h3 class='text-base font-bold \
               text-amber-900'>🛡️ Moderator Removal</h3>\n\
              \                  <p class='text-xs text-amber-700 mt-0.5'>This \
               action is logged publicly in the mod log.</p>\n\
              \                </div>\n\
              \                <form action='/c/%s/posts/%s/mod_delete' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                  %s\n\
              \                  <label class='flex flex-col gap-1.5'>\n\
              \                    <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                    <textarea name='reason' required \
               maxlength='255' rows='4'\n\
              \                      placeholder='Explain why this post is \
               being removed (visible to the community)...'\n\
              \                      class='ph-mask w-full rounded-xl border \
               border-amber-300 bg-amber-50 px-3 py-2 text-sm \
               focus:outline-none focus:ring-2 focus:ring-amber-400 \
               resize-none'></textarea>\n\
              \                  </label>\n\
              \                  <div class='flex justify-end gap-2 pt-1'>\n\
              \                    <button type='button' \
               onclick=\"document.getElementById('mod-modal-%s').close()\"\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                    <button type='submit'\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Removal</button>\n\
              \                  </div>\n\
              \                </form>\n\
              \              </div>\n\
              \            </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                Html.text post.community_slug;
                Html.int post.id;
                csrf_token;
                Html.int post.id;
              ]
          else if is_admin && not target_is_admin then
            (* Rule C: admin override (not a mod of this community) — logged as admin_delete_post *)
            Html.template
              "\n\
              \            <button \
               onclick=\"document.getElementById('mod-modal-%s').showModal()\" \
               class='text-xs font-bold text-red-700 hover:text-red-900 border \
               border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 \
               transition-colors ml-2'>⚡ Admin Remove</button>\n\
              \            <dialog id='mod-modal-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \              <div class='bg-white rounded-2xl overflow-hidden'>\n\
              \                <div class='bg-red-50 border-b border-red-200 \
               px-6 py-4'>\n\
              \                  <h3 class='text-base font-bold \
               text-red-900'>⚡ Admin Intervention</h3>\n\
              \                  <p class='text-xs text-red-700 mt-0.5'>This \
               action is logged publicly as an admin override.</p>\n\
              \                </div>\n\
              \                <form action='/c/%s/posts/%s/mod_delete' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                  %s\n\
              \                  <label class='flex flex-col gap-1.5'>\n\
              \                    <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                    <textarea name='reason' required \
               maxlength='255' rows='4'\n\
              \                      placeholder='Explain the admin \
               intervention reason (visible to the community)...'\n\
              \                      class='ph-mask w-full rounded-xl border \
               border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none \
               focus:ring-2 focus:ring-red-400 resize-none'></textarea>\n\
              \                  </label>\n\
              \                  <div class='flex justify-end gap-2 pt-1'>\n\
              \                    <button type='button' \
               onclick=\"document.getElementById('mod-modal-%s').close()\"\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                    <button type='submit'\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Removal</button>\n\
              \                  </div>\n\
              \                </form>\n\
              \              </div>\n\
              \            </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                Html.text post.community_slug;
                Html.int post.id;
                csrf_token;
                Html.int post.id;
              ]
          else Html.empty
  in

  (* Ban button: mods exile per-community only; suppressed on home/profile feeds where
     is_current_user_mod defaults to false. Never shown for already-deleted accounts.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority
     so an admin who is also a mod doesn't bypass the public mod log via admin path. *)
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin)
    then
      match current_user with
      | Some u
        when u <> post.username
             && not (Components.is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            Html.static
              "<span class='text-xs text-red-500 font-semibold ml-2'>🚫 \
               Banned</span>"
          else if is_current_user_mod then
            (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
            Html.template
              "\n\
              \              <button \
               onclick=\"document.getElementById('ban-modal-post-%s').showModal()\" \
               class='text-xs font-bold text-amber-700 hover:text-amber-900 \
               border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded \
               px-2 py-0.5 transition-colors ml-1'>🔨 Mod Ban</button>\n\
              \              <dialog id='ban-modal-post-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \                <div class='bg-white rounded-2xl \
               overflow-hidden'>\n\
              \                  <div class='bg-amber-50 border-b \
               border-amber-200 px-6 py-4'>\n\
              \                    <h3 class='text-base font-bold \
               text-amber-900'>🔨 Mod Ban</h3>\n\
              \                    <p class='text-xs text-amber-700 \
               mt-0.5'>This action is logged publicly in the mod log.</p>\n\
              \                  </div>\n\
              \                  <form action='/ban-community-user' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                    %s\n\
              \                    <input type='hidden' name='target_username' \
               value='%s'>\n\
              \                    <input type='hidden' name='community_id' \
               value='%s'>\n\
              \                    <label class='flex flex-col gap-1.5'>\n\
              \                      <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                      <textarea name='reason' required rows='4'\n\
              \                        placeholder='Explain why this user is \
               being banned (visible to the community)...'\n\
              \                        class='ph-mask w-full rounded-xl border \
               border-amber-300 bg-amber-50 px-3 py-2 text-sm \
               focus:outline-none focus:ring-2 focus:ring-amber-400 \
               resize-none'></textarea>\n\
              \                    </label>\n\
              \                    <div class='flex justify-end gap-2 pt-1'>\n\
              \                      <button type='button' \
               onclick=\"document.getElementById('ban-modal-post-%s').close()\"\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                      <button type='submit'\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-amber-600 hover:bg-amber-700 \
               transition-colors shadow-sm'>Confirm Ban</button>\n\
              \                    </div>\n\
              \                  </form>\n\
              \                </div>\n\
              \              </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                csrf_token;
                Html.text post.username;
                Html.int post.community_id;
                Html.int post.id;
              ]
          else
            (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
            Html.template
              "\n\
              \              <button \
               onclick=\"document.getElementById('ban-modal-post-%s').showModal()\" \
               class='text-xs font-bold text-red-700 hover:text-red-900 border \
               border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 \
               transition-colors ml-1'>⚡ Admin Ban</button>\n\
              \              <dialog id='ban-modal-post-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \                <div class='bg-white rounded-2xl \
               overflow-hidden'>\n\
              \                  <div class='bg-red-50 border-b border-red-200 \
               px-6 py-4'>\n\
              \                    <h3 class='text-base font-bold \
               text-red-900'>⚡ Admin Ban</h3>\n\
              \                    <p class='text-xs text-red-700 mt-0.5'>This \
               action is logged publicly as an admin override.</p>\n\
              \                  </div>\n\
              \                  <form action='/ban-community-user' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                    %s\n\
              \                    <input type='hidden' name='target_username' \
               value='%s'>\n\
              \                    <input type='hidden' name='community_id' \
               value='%s'>\n\
              \                    <label class='flex flex-col gap-1.5'>\n\
              \                      <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                      <textarea name='reason' required rows='4'\n\
              \                        placeholder='Explain the admin \
               intervention reason (visible to the community)...'\n\
              \                        class='ph-mask w-full rounded-xl border \
               border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none \
               focus:ring-2 focus:ring-red-400 resize-none'></textarea>\n\
              \                    </label>\n\
              \                    <div class='flex justify-end gap-2 pt-1'>\n\
              \                      <button type='button' \
               onclick=\"document.getElementById('ban-modal-post-%s').close()\"\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                      <button type='submit'\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Ban</button>\n\
              \                    </div>\n\
              \                  </form>\n\
              \                </div>\n\
              \              </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                csrf_token;
                Html.text post.username;
                Html.int post.community_id;
                Html.int post.id;
              ]
      | _ -> Html.empty
    else Html.empty
  in
  action_btn ++ ban_btn

(* URL slug from a thread title — descriptive only. post_id is the authoritative key in
   /c/:slug/t/:post_id-:post_slug, so a stale or missing slug still resolves (the handler 301s
   to canonical). Same shape as the section and community stores' slugify: lowercase, any run of
   non-alphanumerics collapses to a single '-', trimmed, length-capped so URLs stay readable. *)
let slugify title =
  let b = Buffer.create (String.length title) in
  let pending_dash = ref false in
  (* defer dashes so leading/collapsed runs never emit one *)
  let any = ref false in
  String.iter
    (fun c ->
      let lc = Char.lowercase_ascii c in
      if (lc >= 'a' && lc <= 'z') || (lc >= '0' && lc <= '9') then begin
        if !pending_dash && !any then Buffer.add_char b '-';
        pending_dash := false;
        any := true;
        Buffer.add_char b lc
      end
      else pending_dash := true)
    title;
  let s = Buffer.contents b in
  (* Cap length, then drop any dash the cut left dangling. *)
  let max_len = 70 in
  let s = if String.length s > max_len then String.sub s 0 max_len else s in
  if String.length s > 0 && s.[String.length s - 1] = '-' then
    String.sub s 0 (String.length s - 1)
  else s

(* Canonical thread path — single source of truth for the route, the /p redirect, and section
   links. Slug omitted entirely when empty so we never emit a dangling trailing dash. *)
let canonical_thread_path community_slug post_id title =
  let s = slugify title in
  if s = "" then Printf.sprintf "/c/%s/t/%d" community_slug post_id
  else Printf.sprintf "/c/%s/t/%d-%s" community_slug post_id s

(* Destination rendering context for one accepted shared-thread placement
   feed row: the destination community's slug (every internal link must stay
   in the destination context — never eject the reader to the origin URL)
   plus the joined provenance and destination-section fields. [None] is the
   community's own post and renders byte-identically to before. The origin
   name may be linked because the feed queries only return a shared row
   while the origin community is currently public. *)
type feed_shared = string * Post_types.feed_shared_context

let shared_from_html (post : Post_types.post)
    (ctx : Post_types.feed_shared_context) =
  Html.template
    "<span class='sth-shared-from'>&#8644; Shared from <a \
     href='/c/%s'>%s</a></span>"
    [ Html.text post.community_slug; Html.text ctx.fs_origin_name ]

(* Origin-side provenance, the reverse direction of [shared_from_html]: the
   one centralized copy for "this canonical discussion also lives in an
   accepted destination". The destinations are (slug, name) pairs the read
   model already restricted to currently publicly renderable placements
   (accepted, public origin, public destination), so naming and linking the
   first one is safe by construction. Launch copy lives only here: one
   destination reads "Shared with <name>", several read
   "Shared with <first> and <n> more" — the caller keeps the full list in
   its view model, so changing this wording later touches no data path. *)
let shared_with_html (destinations : (string * string) list) =
  match destinations with
  | [] -> Html.empty
  | (slug, name) :: rest ->
      let more =
        match List.length rest with
        | 0 -> Html.empty
        | n -> Html.text (Printf.sprintf " and %d more" n)
      in
      Html.template
        "<span class='sth-shared-from'>&#8644; Shared with <a \
         href='/c/%s'>%s</a>%s</span>"
        [ Html.text slug; Html.text name; more ]

let render_post ?(is_current_user_mod = false) ?(mod_usernames = [])
    ?(admin_usernames = []) ?(banned_usernames = [])
    ?(shared : feed_shared option) request user_votes (post : Post_types.post) =
  let csrf_token = Csrf_field.tag request in
  let content_preview = Option.value ~default:"" post.content in
  let link_part =
    match post.url with
    | Some u ->
        Html.template
          "<a href='%s' class='text-xs text-[#C94C4C] hover:underline' \
           target='_blank'>%s ↗</a>"
          [ Html.external_url u; Html.text u ]
    | None -> Html.empty
  in
  (* Shared rows link into the DESTINATION thread context (server-built from
     the destination slug and canonical post data — no stored URL); the
     community's own rows keep the legacy /p/:id links byte-for-byte. *)
  let thread_target =
    match shared with
    | Some (destination_slug, _) ->
        canonical_thread_path destination_slug post.id post.title
    | None -> Printf.sprintf "/p/%d" post.id
  in

  let current_vote =
    Option.value ~default:0 (List.assoc_opt post.id user_votes)
  in

  let up_color =
    if current_vote = 1 then "text-orange-500"
    else "text-gray-400 hover:text-orange-500"
  in
  let down_color =
    if current_vote = -1 then "text-[#69C3D2]"
    else "text-gray-400 hover:text-[#69C3D2]"
  in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in

  let current_user = Dream.session_field request "username" in
  (* Mod/admin action markup (own-post Delete, Mod/Admin Remove, Mod/Admin Ban) is built by
     the shared post_admin_actions helper so search results reuse the exact same forms.
     On a SHARED row the destination's moderator standing grants nothing over canonical
     content, so the mod arm is dropped (and with it the destination-scoped ban list,
     which pairs with the wrong community): only the author's own delete and the global
     admin's origin-scoped controls remain. *)
  let admin_actions =
    match shared with
    | None ->
        post_admin_actions ~is_current_user_mod ~admin_usernames
          ~banned_usernames ~csrf_token request post
    | Some _ ->
        post_admin_actions ~is_current_user_mod:false ~admin_usernames
          ~banned_usernames:[] ~csrf_token request post
  in

  let upvote_html =
    match current_user with
    | Some _ ->
        Html.template
          "<form action='/vote' method='POST' class='m-0 p-0'>%s<input \
           type='hidden' name='post_id' value='%s'><input type='hidden' \
           name='direction' value='%s'><button type='submit' class='%s \
           font-bold text-sm leading-none'>▲</button></form>"
          [
            csrf_token; Html.int post.id; Html.int up_action; Html.text up_color;
          ]
    | None ->
        Html.static
          "<a href='/login' class='text-gray-400 hover:text-orange-500 \
           font-bold text-sm leading-none'>▲</a>"
  in
  let downvote_html =
    if not post.allow_downvotes then Html.empty
    else
      match current_user with
      | Some _ ->
          Html.template
            "<form action='/vote' method='POST' class='m-0 p-0'>%s<input \
             type='hidden' name='post_id' value='%s'><input type='hidden' \
             name='direction' value='%s'><button type='submit' class='%s \
             font-bold text-sm leading-none'>▼</button></form>"
            [
              csrf_token;
              Html.int post.id;
              Html.int down_action;
              Html.text down_color;
            ]
      | None ->
          Html.static
            "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] \
             font-bold text-sm leading-none'>▼</a>"
  in

  (* Image thumbnail: capped at 320 px wide in the card — full resolution served from
     static/uploads/; no second resize needed because the file is already ≤1920x1080. *)
  let image_html =
    match post.image_url with
    | None -> Html.empty
    | Some img ->
        Html.template
          "<a href='%s' class='block mt-2 mb-1'><img src='%s' alt='Post image' \
           class='w-full max-h-[512px] object-contain bg-stone-900 rounded-lg \
           border border-[#E0D9CC]'></a>"
          [ Html.text thread_target; Html.text img ]
  in
  (* Meta head: the community's own rows keep the /c/:slug chip and their
     origin-section chip byte-for-byte. A shared row instead leads with the
     "Shared from <origin>" provenance and shows the placement's DESTINATION
     section — the effective local context — never the origin's. *)
  let context_chip =
    match shared with
    | None ->
        Html.template
          "<a href='/c/%s' class='font-semibold text-gray-700 \
           hover:text-[#C94C4C] transition relative z-10'>/c/%s</a>"
          [ Html.text post.community_slug; Html.text post.community_slug ]
    | Some (_, ctx) -> shared_from_html post ctx
  in
  let section_chip =
    match shared with
    | None -> (
        match (post.section_name, post.section_slug) with
        | Some sn, Some ss ->
            Html.template
              "<span class='text-gray-300'>›</span><a href='/c/%s/s/%s' \
               class='font-medium text-[#C94C4C] hover:underline transition \
               relative z-10'>%s</a>"
              [ Html.text post.community_slug; Html.text ss; Html.text sn ]
        | _ when post.community_sections_enabled ->
            Html.template
              "<span class='text-gray-300'>›</span><a \
               href='/c/%s/s/uncategorized' class='font-medium text-[#C94C4C] \
               hover:underline transition relative z-10'>Uncategorized</a>"
              [ Html.text post.community_slug ]
        | _ -> Html.empty)
    | Some (destination_slug, ctx) -> (
        match (ctx.fs_section_name, ctx.fs_section_slug) with
        | Some sn, Some ss ->
            Html.template
              "<span class='text-gray-300'>›</span><a href='/c/%s/s/%s' \
               class='font-medium text-[#C94C4C] hover:underline transition \
               relative z-10'>%s</a>"
              [ Html.text destination_slug; Html.text ss; Html.text sn ]
        | _ -> Html.empty)
  in
  Html.template
    "\n\
    \  <div data-href='%s' onclick=\"if(!event.target.closest('a, button, \
     form')) window.location=this.dataset.href\" class='cursor-pointer \
     border-b border-[#E8E2D9] py-4 flex gap-4 hover:bg-[#F0EBE0] \
     transition-colors'>\n\n\
    \      <div class='flex flex-col items-center pt-0.5 w-7 shrink-0 \
     cursor-default' onclick=\"event.stopPropagation()\">\n\
    \          %s\n\
    \          <span class='font-semibold text-gray-600 text-xs \
     my-0.5'>%s</span>\n\
    \          %s\n\
    \      </div>\n\n\
    \      <div class='flex-1 min-w-0'>\n\
    \          <div class='flex flex-wrap items-center gap-x-1.5 text-xs \
     text-gray-500 mb-1'>\n\
    \              %s\n\
    \              %s\n\
    \              <span class='text-gray-300'>•</span>\n\
    \              <span>by</span>\n\
    \              <span class='relative z-10'>%s</span>\n\
    \              <span class='text-gray-300'>•</span>\n\
    \              <span>%s</span>\n\
    \              <span class='relative z-10'>%s</span>\n\
    \          </div>\n\n\
    \          <h3 class='text-base font-semibold text-gray-900 leading-snug \
     mb-1'>\n\
    \              <a href='%s' class='ph-mask hover:text-[#C94C4C] transition \
     break-words'>%s</a>\n\
    \          </h3>\n\n\
    \          <div class='relative z-10 text-xs'>%s</div>\n\
    \          %s\n\
    \          <p class='ph-mask text-sm text-gray-600 mt-2 break-words \
     line-clamp-6'>%s</p>\n\n\
    \          <div class='flex items-center mt-2 text-xs text-gray-400'>\n\
    \              <a href='%s' class='hover:text-[#C94C4C] flex items-center \
     gap-1 transition relative z-10'>\n\
    \                  <span>💬</span><span>%s comments</span>\n\
    \              </a>\n\
    \              <button type='button' data-share-path='%s' \
     onclick='copyPostLink(this.dataset.sharePath, this)' class='text-xs \
     font-medium text-gray-500 hover:text-gray-900 flex items-center \
     transition-colors cursor-pointer ml-4'>🔗 Share</button>\n\
    \          </div>\n\
    \      </div>\n\
    \  </div>"
    [
      Html.internal_path thread_target;
      upvote_html;
      Html.int post.score;
      downvote_html;
      context_chip;
      section_chip;
      Components.render_author ~mod_usernames ~admin_usernames post.username;
      Html.text (Components.time_ago post.created_at);
      admin_actions;
      Html.text thread_target;
      Html.text post.title;
      link_part;
      image_html;
      Html.text content_preview;
      Html.text thread_target;
      Html.int post.comment_count;
      Html.text thread_target;
    ]

(* Compact host for a link post's domain chip: strip scheme + a leading www. and cut at the
   first path/query/fragment. None when it doesn't look like an http(s) URL, so the row shows
   no chip rather than a misleading fragment. Display goes through Html.text; the href uses
   Html.external_url. Hand-rolled (no Uri dep on the hot render path) — only needs host extraction. *)
let extract_domain url =
  let u = String.trim url in
  let strip_prefix p s =
    let lp = String.length p in
    if String.length s >= lp && String.lowercase_ascii (String.sub s 0 lp) = p
    then Some (String.sub s lp (String.length s - lp))
    else None
  in
  match
    match strip_prefix "https://" u with
    | Some r -> Some r
    | None -> strip_prefix "http://" u
  with
  | None -> None
  | Some rest ->
      let host_end =
        match
          List.filter_map (fun c -> String.index_opt rest c) [ '/'; '?'; '#' ]
        with
        | [] -> String.length rest
        | l -> List.fold_left min (String.length rest) l
      in
      let host = String.sub rest 0 host_end in
      let host =
        match strip_prefix "www." host with Some h -> h | None -> host
      in
      if host = "" then None else Some host

(* The section row's ⋯ menu reuses the EXACT moderation UI render_post builds — identical routes,
   CSRF, dialog ids and Rule A/B/C permission logic — so the forum feed can't drift from the
   warm-card feed. render_post is deliberately NOT refactored to call this; it keeps its own inline
   copy so warm-card pages stay byte-for-byte unchanged (a conservative duplication, by request).
   Returns "" when the viewer has no available action (anon, or a tombstoned / admin-protected
   target) so the caller can omit the menu entirely. Dialog ids are keyed by post.id and never
   collide with render_post because the two renderers never appear on the same page. *)
let mod_action_controls ~is_current_user_mod ~admin_usernames ~banned_usernames
    request (post : Post_types.post) =
  let csrf_token = Csrf_field.tag request in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let is_already_deleted =
    Components.is_deleted_user post.username
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let target_is_admin = List.mem post.username admin_usernames in
  (* Rule A: own post → personal Delete (labelled in the menu, route/CSRF identical to the card).
     Rule B: mod/top_mod → Mod Remove dialog. Rule C: admin without mod role → Admin Remove. *)
  let action_btn =
    if is_already_deleted then Html.empty
    else
      match current_user with
      | None -> Html.empty
      | Some u ->
          if u = post.username then
            Html.template
              "<form action='/delete-post' method='POST' class='m-0 p-0' \
               onsubmit=\"confirmModal(event, 'Do you really want to delete \
               this post? This action cannot be undone.')\">\n\
              \              %s <input type='hidden' name='post_id' value='%s'>\n\
              \              <button type='submit' class='text-xs font-bold \
               text-gray-600 hover:text-red-700 border border-gray-300 \
               bg-gray-50 hover:bg-red-50 rounded px-2 py-0.5 \
               transition-colors'>🗑️ Delete</button>\n\
              \          </form>"
              [ csrf_token; Html.int post.id ]
          else if is_current_user_mod && not target_is_admin then
            Html.template
              "\n\
              \            <button \
               onclick=\"document.getElementById('mod-modal-%s').showModal()\" \
               class='text-xs font-bold text-amber-700 hover:text-amber-900 \
               border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded \
               px-2 py-0.5 transition-colors'>🛡️ Mod Remove</button>\n\
              \            <dialog id='mod-modal-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \              <div class='bg-white rounded-2xl overflow-hidden'>\n\
              \                <div class='bg-amber-50 border-b \
               border-amber-200 px-6 py-4'>\n\
              \                  <h3 class='text-base font-bold \
               text-amber-900'>🛡️ Moderator Removal</h3>\n\
              \                  <p class='text-xs text-amber-700 mt-0.5'>This \
               action is logged publicly in the mod log.</p>\n\
              \                </div>\n\
              \                <form action='/c/%s/posts/%s/mod_delete' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                  %s\n\
              \                  <label class='flex flex-col gap-1.5'>\n\
              \                    <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                    <textarea name='reason' required \
               maxlength='255' rows='4'\n\
              \                      placeholder='Explain why this post is \
               being removed (visible to the community)...'\n\
              \                      class='ph-mask w-full rounded-xl border \
               border-amber-300 bg-amber-50 px-3 py-2 text-sm \
               focus:outline-none focus:ring-2 focus:ring-amber-400 \
               resize-none'></textarea>\n\
              \                  </label>\n\
              \                  <div class='flex justify-end gap-2 pt-1'>\n\
              \                    <button type='button' \
               onclick=\"document.getElementById('mod-modal-%s').close()\"\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                    <button type='submit'\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Removal</button>\n\
              \                  </div>\n\
              \                </form>\n\
              \              </div>\n\
              \            </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                Html.text post.community_slug;
                Html.int post.id;
                csrf_token;
                Html.int post.id;
              ]
          else if is_admin && not target_is_admin then
            Html.template
              "\n\
              \            <button \
               onclick=\"document.getElementById('mod-modal-%s').showModal()\" \
               class='text-xs font-bold text-red-700 hover:text-red-900 border \
               border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 \
               transition-colors'>⚡ Admin Remove</button>\n\
              \            <dialog id='mod-modal-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \              <div class='bg-white rounded-2xl overflow-hidden'>\n\
              \                <div class='bg-red-50 border-b border-red-200 \
               px-6 py-4'>\n\
              \                  <h3 class='text-base font-bold \
               text-red-900'>⚡ Admin Intervention</h3>\n\
              \                  <p class='text-xs text-red-700 mt-0.5'>This \
               action is logged publicly as an admin override.</p>\n\
              \                </div>\n\
              \                <form action='/c/%s/posts/%s/mod_delete' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                  %s\n\
              \                  <label class='flex flex-col gap-1.5'>\n\
              \                    <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                    <textarea name='reason' required \
               maxlength='255' rows='4'\n\
              \                      placeholder='Explain the admin \
               intervention reason (visible to the community)...'\n\
              \                      class='ph-mask w-full rounded-xl border \
               border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none \
               focus:ring-2 focus:ring-red-400 resize-none'></textarea>\n\
              \                  </label>\n\
              \                  <div class='flex justify-end gap-2 pt-1'>\n\
              \                    <button type='button' \
               onclick=\"document.getElementById('mod-modal-%s').close()\"\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                    <button type='submit'\n\
              \                      class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Removal</button>\n\
              \                  </div>\n\
              \                </form>\n\
              \              </div>\n\
              \            </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                Html.text post.community_slug;
                Html.int post.id;
                csrf_token;
                Html.int post.id;
              ]
          else Html.empty
  in
  (* Ban: mods exile per-community; admins (without mod role) act via the override path. Mutually
     exclusive with the mod path so an admin who is also a mod still goes through the public mod
     log. A banned account shows a badge instead of a button to prevent double-ban confusion. *)
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin)
    then
      match current_user with
      | Some u
        when u <> post.username
             && not (Components.is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            Html.static
              "<span class='text-xs text-red-500 font-semibold'>🚫 Banned</span>"
          else if is_current_user_mod then
            Html.template
              "\n\
              \              <button \
               onclick=\"document.getElementById('ban-modal-post-%s').showModal()\" \
               class='text-xs font-bold text-amber-700 hover:text-amber-900 \
               border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded \
               px-2 py-0.5 transition-colors'>🔨 Mod Ban</button>\n\
              \              <dialog id='ban-modal-post-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \                <div class='bg-white rounded-2xl \
               overflow-hidden'>\n\
              \                  <div class='bg-amber-50 border-b \
               border-amber-200 px-6 py-4'>\n\
              \                    <h3 class='text-base font-bold \
               text-amber-900'>🔨 Mod Ban</h3>\n\
              \                    <p class='text-xs text-amber-700 \
               mt-0.5'>This action is logged publicly in the mod log.</p>\n\
              \                  </div>\n\
              \                  <form action='/ban-community-user' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                    %s\n\
              \                    <input type='hidden' name='target_username' \
               value='%s'>\n\
              \                    <input type='hidden' name='community_id' \
               value='%s'>\n\
              \                    <label class='flex flex-col gap-1.5'>\n\
              \                      <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                      <textarea name='reason' required rows='4'\n\
              \                        placeholder='Explain why this user is \
               being banned (visible to the community)...'\n\
              \                        class='ph-mask w-full rounded-xl border \
               border-amber-300 bg-amber-50 px-3 py-2 text-sm \
               focus:outline-none focus:ring-2 focus:ring-amber-400 \
               resize-none'></textarea>\n\
              \                    </label>\n\
              \                    <div class='flex justify-end gap-2 pt-1'>\n\
              \                      <button type='button' \
               onclick=\"document.getElementById('ban-modal-post-%s').close()\"\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                      <button type='submit'\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-amber-600 hover:bg-amber-700 \
               transition-colors shadow-sm'>Confirm Ban</button>\n\
              \                    </div>\n\
              \                  </form>\n\
              \                </div>\n\
              \              </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                csrf_token;
                Html.text post.username;
                Html.int post.community_id;
                Html.int post.id;
              ]
          else
            Html.template
              "\n\
              \              <button \
               onclick=\"document.getElementById('ban-modal-post-%s').showModal()\" \
               class='text-xs font-bold text-red-700 hover:text-red-900 border \
               border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 \
               transition-colors'>⚡ Admin Ban</button>\n\
              \              <dialog id='ban-modal-post-%s' class='rounded-2xl \
               shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 \
               backdrop:backdrop-blur-sm border-0'>\n\
              \                <div class='bg-white rounded-2xl \
               overflow-hidden'>\n\
              \                  <div class='bg-red-50 border-b border-red-200 \
               px-6 py-4'>\n\
              \                    <h3 class='text-base font-bold \
               text-red-900'>⚡ Admin Ban</h3>\n\
              \                    <p class='text-xs text-red-700 mt-0.5'>This \
               action is logged publicly as an admin override.</p>\n\
              \                  </div>\n\
              \                  <form action='/ban-community-user' \
               method='POST' class='px-6 py-5 flex flex-col gap-4'>\n\
              \                    %s\n\
              \                    <input type='hidden' name='target_username' \
               value='%s'>\n\
              \                    <input type='hidden' name='community_id' \
               value='%s'>\n\
              \                    <label class='flex flex-col gap-1.5'>\n\
              \                      <span class='text-sm font-semibold \
               text-gray-700'>Reason <span class='text-red-500'>*</span></span>\n\
              \                      <textarea name='reason' required rows='4'\n\
              \                        placeholder='Explain the admin \
               intervention reason (visible to the community)...'\n\
              \                        class='ph-mask w-full rounded-xl border \
               border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none \
               focus:ring-2 focus:ring-red-400 resize-none'></textarea>\n\
              \                    </label>\n\
              \                    <div class='flex justify-end gap-2 pt-1'>\n\
              \                      <button type='button' \
               onclick=\"document.getElementById('ban-modal-post-%s').close()\"\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 \
               transition-colors'>Cancel</button>\n\
              \                      <button type='submit'\n\
              \                        class='px-4 py-2 rounded-xl text-sm \
               font-bold text-white bg-red-600 hover:bg-red-700 \
               transition-colors shadow-sm'>Confirm Ban</button>\n\
              \                    </div>\n\
              \                  </form>\n\
              \                </div>\n\
              \              </dialog>"
              [
                Html.int post.id;
                Html.int post.id;
                csrf_token;
                Html.text post.username;
                Html.int post.community_id;
                Html.int post.id;
              ]
      | _ -> Html.empty
    else Html.empty
  in
  action_btn ++ ban_btn

(* Section-feed thread row — the cool-grey, thread-first counterpart to render_post (warm card).
   Used only by community_section_shell_page. Vote markup is reused verbatim from the card so the
   optimistic-vote JS (see layout) keeps working: the .cs-vote control's only direct children are
   the upvote form, the score <span>, and the downvote form, with the same Tailwind colour classes
   the JS toggles. The body is intentionally lighter than the card: strong title, an optional
   single-line preview, a compact monospace meta row, and moderation tucked into a ⋯ menu. *)
let render_forum_row ?(is_current_user_mod = false) ?(mod_usernames = [])
    ?(admin_usernames = []) ?(banned_usernames = []) ?(show_context = false)
    ?(shared : feed_shared option) ?(shared_with = []) request user_votes
    (post : Post_types.post) =
  let csrf_token = Csrf_field.tag request in
  let current_user = Dream.session_field request "username" in
  let current_vote =
    Option.value ~default:0 (List.assoc_opt post.id user_votes)
  in
  let up_color =
    if current_vote = 1 then "text-orange-500"
    else "text-gray-400 hover:text-orange-500"
  in
  let down_color =
    if current_vote = -1 then "text-[#69C3D2]"
    else "text-gray-400 hover:text-[#69C3D2]"
  in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in
  let upvote_html =
    match current_user with
    | Some _ ->
        Html.template
          "<form action='/vote' method='POST' class='m-0 p-0'>%s<input \
           type='hidden' name='post_id' value='%s'><input type='hidden' \
           name='direction' value='%s'><button type='submit' class='%s \
           font-bold text-sm leading-none'>▲</button></form>"
          [
            csrf_token; Html.int post.id; Html.int up_action; Html.text up_color;
          ]
    | None ->
        Html.static
          "<a href='/login' class='text-gray-400 hover:text-orange-500 \
           font-bold text-sm leading-none'>▲</a>"
  in
  let downvote_html =
    if not post.allow_downvotes then Html.empty
    else
      match current_user with
      | Some _ ->
          Html.template
            "<form action='/vote' method='POST' class='m-0 p-0'>%s<input \
             type='hidden' name='post_id' value='%s'><input type='hidden' \
             name='direction' value='%s'><button type='submit' class='%s \
             font-bold text-sm leading-none'>▼</button></form>"
            [
              csrf_token;
              Html.int post.id;
              Html.int down_action;
              Html.text down_color;
            ]
      | None ->
          Html.static
            "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] \
             font-bold text-sm leading-none'>▼</a>"
  in
  (* Domain chip: only for link posts with a parseable host; external link opens in a new tab. *)
  let domain_html =
    match post.url with
    | Some u -> (
        match extract_domain u with
        | Some d ->
            Html.template
              "<a class='ft-domain' href='%s' target='_blank' \
               rel='noopener'>%s ↗</a>"
              [ Html.external_url u; Html.text d ]
        | None -> Html.empty)
    | None -> Html.empty
  in
  (* Secondary one-line preview: only when there's real body text (CSS clamps to a single line).
     Omitted entirely for link-only / empty posts — no placeholder. *)
  let preview_html =
    match post.content with
    | Some c when String.trim c <> "" ->
        Html.template "<div class='ft-preview'>%s</div>" [ Html.text c ]
    | _ -> Html.empty
  in
  (* ⋯ menu rendered only when the viewer actually has an action (keeps the feed clean, not admin-y).
     On a SHARED row the destination's moderator standing grants nothing over canonical content:
     the mod arm is dropped (with its destination-scoped ban list, which pairs with the wrong
     community) — only own-post delete and global-admin origin-scoped controls survive. *)
  let mod_controls =
    match shared with
    | None ->
        mod_action_controls ~is_current_user_mod ~admin_usernames
          ~banned_usernames request post
    | Some _ ->
        mod_action_controls ~is_current_user_mod:false ~admin_usernames
          ~banned_usernames:[] request post
  in
  let mod_menu =
    if mod_controls = Html.empty then Html.empty
    else
      Html.template
        "<details class='cs-row-mod'><summary>⋯</summary><div \
         class='cs-row-mod-menu'>%s</div></details>"
        [ mod_controls ]
  in
  (* Internal links point at the canonical thread URL, not legacy /p/:id (which now 301s here).
     A shared row links into the DESTINATION thread context instead — the reader stays in the
     community they are browsing; the path is server-built from the destination slug and the
     canonical post, never a stored URL. *)
  let thread_href =
    match shared with
    | Some (destination_slug, _) ->
        canonical_thread_path destination_slug post.id post.title
    | None -> canonical_thread_path post.community_slug post.id post.title
  in
  (* show_context is set by the global Feed, where rows span communities/sections, so each row
     names its own origin: "§ Section · /c/slug". Section pages leave it off — the page header
     already supplies that context — so those views stay byte-for-byte unchanged. A shared row
     instead always carries its one compact provenance line. *)
  let context_html =
    match shared with
    | Some (_, ctx) ->
        Html.template "<div class='ft-ctx'>%s</div>"
          [ shared_from_html post ctx ]
    | None ->
        if not show_context then Html.empty
        else
          let section_html =
            match (post.section_name, post.section_slug) with
            | Some name, Some slug when String.trim name <> "" ->
                Html.template
                  "<span class='sec'>§</span> <a href='/c/%s/s/%s'>%s</a> \
                   <span class='ft-ctx-dot'>·</span> "
                  [
                    Html.text post.community_slug;
                    Html.text slug;
                    Html.text name;
                  ]
            | _ -> Html.empty
          in
          (* Origin-side provenance rides the same context line the global feed
         already renders; the destination direction (shared = Some) above
         never combines with it — one card, one provenance direction. An
         empty [shared_with] leaves the line byte-identical. *)
          let shared_with_span =
            let span = shared_with_html shared_with in
            if Html.is_empty span then Html.empty
            else Html.static " <span class='ft-ctx-dot'>·</span> " ++ span
          in
          Html.template
            "<div class='ft-ctx'>%s<a class='ft-ctx-c' \
             href='/c/%s'>/c/%s</a>%s</div>"
            [
              section_html;
              Html.text post.community_slug;
              Html.text post.community_slug;
              shared_with_span;
            ]
  in
  Html.template
    "\n\
    \  <div class='cs-thread'>\n\
    \      <div class='cs-vote'>%s<span class='cs-vote-score'>%s</span>%s</div>\n\
    \      <div class='ft-main'>\n\
    \          %s\n\
    \          <div class='ft-title'><a href='%s'>%s</a></div>\n\
    \          %s\n\
    \          <div class='ft-meta'>%s<span>by %s</span><span>%s</span><a \
     href='%s'>💬 %s</a></div>\n\
    \      </div>\n\
    \      %s\n\
    \  </div>"
    [
      upvote_html;
      Html.int post.score;
      downvote_html;
      context_html;
      Html.text thread_href;
      Html.text post.title;
      preview_html;
      domain_html;
      Components.render_author ~mod_usernames ~admin_usernames post.username;
      Html.text (Components.time_ago post.created_at);
      Html.text thread_href;
      Html.int post.comment_count;
      mod_menu;
    ]
