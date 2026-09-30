(* /u/:username — the public user profile, on the launch app chrome
   (Components.launch_app_page: earde.css only, no legacy per-page CSS).
   The inner account-* markup (account-prof header, account-badges,
   account-bio, account-tabs, account-thread rows with the /vote form DOM the
   shared behavior script drives, account-comment, account-comm-card,
   account-admin) is kept and skinned by the "public user profile only"
   integration section at the end of earde.css. .account-bio stays because it
   is a replay-masking selector in analytics.js. rail_communities feeds the
   launch rail only (VIEWER membership, same order as every other launch
   surface); it never enters profile content. Tabs, ?tab= values, default and
   unknown-value fallback, activity ordering and every destination link are
   unchanged. *)
let user_profile_page ?user ?(rail_communities = []) ~is_admin ~is_globally_banned ~profile_id ~admin_usernames ~moderated_communities ~active_tab user_votes username joined_at bio_opt avatar_url_opt karma posts user_comments community_stats request =
  let csrf_token = Dream.csrf_tag request in
  let bio = Option.value ~default:"This user hasn't written a bio yet." bio_opt in
  (* Profile avatar: route the stored URL through Components.user_avatar (safe_img_src) instead
     of raw interpolation. Avatarless profiles now show a local letter tile rather than the old
     external Gravatar "mystery person" default — one fewer third-party image dependency. *)
  let avatar_html =
    Components.user_avatar ~alt:(username ^ " avatar")
      ~img_class:"account-avatar"
      ~tile_class:"account-avatar flex items-center justify-center text-2xl font-bold"
      ~username avatar_url_opt
  in

  (* Profile role badges: ADMIN badge driven by admin_usernames (already fetched),
     MOD badges iterate moderated_communities — one badge per community the user moderates. *)
  let profile_admin_badge =
    if List.mem username admin_usernames then
      "<span class='account-badge account-badge--admin'>Admin</span>"
    else ""
  in
  let mod_badges =
    String.concat " " (List.map (fun (a : Community_types.community) ->
      Printf.sprintf "<a href='/c/%s' class='account-badge account-badge--mod'>Mod of /c/%s</a>"
        a.slug a.slug
    ) moderated_communities)
  in
  (* Banned badge: shown on the profile header so any visitor sees the account status.
     Admin controls: Ban ↔ Unban toggle driven by is_globally_banned to prevent
     double-ban confusion and surface the current state clearly. *)
  let globally_banned_badge =
    if is_globally_banned then
      "<span class='account-badge account-badge--banned'>Globally banned</span>"
    else ""
  in
  let role_badges =
    if profile_admin_badge = "" && mod_badges = "" && globally_banned_badge = "" then ""
    else Printf.sprintf "<div class='account-badges'>%s%s%s</div>" profile_admin_badge mod_badges globally_banned_badge
  in
  (* Edit Profile: only render when the logged-in user is viewing their own profile.
     Avoids exposing /settings entry point on others' profiles — a cosmetic boundary
     that reinforces the expectation that settings are personal. *)
  let edit_profile_btn =
    match user with
    | Some logged_in when logged_in = username ->
      "<a href='/settings' class='account-btn account-btn--secondary'>Edit profile</a>"
    | _ -> ""
  in
  (* Admin Panel: only render for the logged-in admin on their own profile.
     Gating on both own-profile AND is_admin prevents leaking the /admin route
     to non-admins even if they inspect another admin's profile page. *)
  let admin_panel_btn =
    match user with
    | Some logged_in when logged_in = username && is_admin ->
      "<a href='/admin' class='account-btn account-btn--secondary'>Admin panel</a>"
    | _ -> ""
  in
  let header_actions =
    if edit_profile_btn = "" && admin_panel_btn = "" then ""
    else Printf.sprintf "<div class='account-actions'>%s%s</div>" edit_profile_btn admin_panel_btn
  in

  (* The username is attacker-chosen for accounts created before the signup
     charset rule, so every sink below is escaped for its own context. The two
     confirmModal hooks are JavaScript string literals inside an HTML
     attribute — html_escape alone decodes back to a live apostrophe there,
     so they take js_single_quoted_attr. *)
  let admin_controls =
    if is_admin && (Option.value ~default:"" user) <> username then
      let js_username = Components.js_single_quoted_attr username in
      let ban_or_unban_btn =
        if is_globally_banned then
          Printf.sprintf "
            <form action='/admin/unban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">
                %s
                <button type='submit' class='account-btn account-btn--secondary'>Unban user</button>
            </form>" profile_id js_username csrf_token
        else
          Printf.sprintf "
            <form action='/admin/ban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Permanently ban u/%s? They will be blocked from logging in and posting.')\">
                %s
                <button type='submit' class='account-btn'>Ban user</button>
            </form>" profile_id js_username csrf_token
      in
      Printf.sprintf "
        <div class='account-admin'>
            <h3 class='account-admin-title'>Admin controls</h3>
            %s
        </div>" ban_or_unban_btn
    else ""
  in

  let tab_class active =
    if active_tab = active then "account-tab account-tab--active" else "account-tab"
  in
  (* User-facing label is "Threads"; the query param stays ?tab=posts so existing links,
     the handler's tab dispatch, and bookmarks keep working unchanged. *)
  let esc_username = Components.html_escape username in
  let tab_nav = Printf.sprintf "
    <div class='account-tabs'>
        <a href='/u/%s?tab=posts' class='%s'>Threads</a>
        <a href='/u/%s?tab=comments' class='%s'>Comments</a>
        <a href='/u/%s?tab=communities' class='%s'>Communities</a>
    </div>" esc_username (tab_class "posts") esc_username (tab_class "comments") esc_username (tab_class "communities")
  in

  (* Profile-specific thread row. The shared render_forum_row markup is scoped to the
     community surfaces, so the profile carries its own row instead of inheriting it.
     This row reuses the SAME real post data and the SAME vote-form DOM (up-form, score span,
     down-form, with the optimistic-vote colour classes) so voting behaves identically — only
     the surrounding layout differs. No query / canonical-URL change. *)
  let render_thread_row (post : Post_types.post) =
    let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in
    let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
    let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
    let up_action = if current_vote = 1 then 0 else 1 in
    let down_action = if current_vote = -1 then 0 else -1 in
    let upvote_html = match user with
      | Some _ -> Printf.sprintf "<form action='/vote' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▲</button></form>" csrf_token post.id up_action up_color
      | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 font-bold text-sm leading-none'>▲</a>"
    in
    let downvote_html =
      if not post.allow_downvotes then ""
      else match user with
      | Some _ -> Printf.sprintf "<form action='/vote' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▼</button></form>" csrf_token post.id down_action down_color
      | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] font-bold text-sm leading-none'>▼</a>"
    in
    let domain_html = match post.url with
      | Some u -> (match Post_cards.extract_domain u with
          | Some d -> Printf.sprintf "<a class='account-thread-domain' href='%s' target='_blank' rel='noopener'>%s ↗</a>" (Components.safe_url u) (Components.html_escape d)
          | None -> "")
      | None -> ""
    in
    let preview_html = match post.content with
      | Some c when String.trim c <> "" -> Printf.sprintf "<div class='account-thread-preview'>%s</div>" (Components.html_escape c)
      | _ -> ""
    in
    (* Each row names its origin community/section, since a profile spans communities. *)
    let context_html =
      let section_html = match post.section_name, post.section_slug with
        | Some name, Some slug when String.trim name <> "" ->
            Printf.sprintf "<span class='sec'>§</span> <a href='/c/%s/s/%s'>%s</a> <span class='dot'>·</span> "
              (Components.html_escape post.community_slug) (Components.html_escape slug) (Components.html_escape name)
        | _ -> ""
      in
      Printf.sprintf "<div class='account-thread-ctx'>%s<a href='/c/%s'>/c/%s</a></div>"
        section_html (Components.html_escape post.community_slug) (Components.html_escape post.community_slug)
    in
    let thread_href = Post_cards.canonical_thread_path post.community_slug post.id post.title in
    Printf.sprintf "
        <div class='account-thread'>
            <div class='account-thread-vote'>%s<span class='account-thread-score'>%d</span>%s</div>
            <div class='account-thread-main'>
                %s
                <div class='account-thread-title'><a href='%s'>%s</a></div>
                %s
                <div class='account-thread-meta'>%s<span>by %s</span><span>%s</span><a href='%s'>💬 %d</a></div>
            </div>
        </div>"
      upvote_html post.score downvote_html
      context_html
      thread_href (Components.html_escape post.title)
      preview_html
      domain_html (Components.render_author ~admin_usernames post.username) (Components.time_ago post.created_at) thread_href post.comment_count
  in

  let posts_html =
    if posts = [] then
      "<div class='account-empty'>This user hasn't started any threads yet.</div>"
    else
      Printf.sprintf "<div class='account-threads'>%s</div>"
        (String.concat "\n" (List.map render_thread_row posts))
  in

  let comments_html =
    if user_comments = [] then
      "<div class='account-empty'>This user hasn't commented anything yet.</div>"
    else
      String.concat "\n" (List.map (fun (_id, content, created_at, post_id, post_title, score) ->
        Printf.sprintf "
          <div class='account-comment'>
              <div class='account-comment-meta'>
                  <span>%s</span>
                  <span class='dot'>·</span>
                  <span>%d points</span>
              </div>
              <div class='account-comment-body'>%s</div>
              <a href='/p/%d' class='account-comment-link'>&#8618; Commented on: %s</a>
          </div>" created_at score (Components.html_escape content) post_id (Components.html_escape post_title)
      ) user_comments)
  in

  let communities_html =
    if community_stats = [] then
      "<div class='account-empty'>No community activity yet.</div>"
    else begin
      let cards = String.concat "\n" (List.map (fun (s : Community_user_stats_store.community_user_stat) ->
        let total = s.local_post_count + s.local_comment_count in
        let active_since =
          match s.first_active_at with
          | None -> ""
          | Some ts ->
              Printf.sprintf "<div class='account-comm-sub'>active since %s</div>"
                (Components.format_month_year ts)
        in
        Printf.sprintf "
          <a href='/c/%s' class='account-comm-card'>
              <div class='account-comm-name'>%s</div>
              <div class='account-comm-slug'>/c/%s</div>
              <div class='account-comm-stat'><strong>%d</strong> local karma</div>
              <div class='account-comm-stat'><strong>%d</strong> contributions here</div>
              <div class='account-comm-sub'>%d posts &middot; %d comments</div>
              %s
          </a>"
          s.community_slug
          (Components.html_escape s.community_name)
          s.community_slug
          s.local_karma
          total
          s.local_post_count
          s.local_comment_count
          active_since
      ) community_stats) in
      Printf.sprintf "
        <h3 class='account-section-title'>Community reputation</h3>
        <div class='account-comm-grid'>%s</div>" cards
    end
  in

  let feed_html =
    if active_tab = "comments" then comments_html
    else if active_tab = "communities" then communities_html
    else posts_html
  in

  let body = Printf.sprintf "
    <div class='account-wrap'>
        <div class='account-panel'>
            <div class='account-prof'>
                %s
                <div class='account-prof-body'>
                    <h1 class='account-name'>u/%s</h1>
                    %s
                    <div class='account-stats'>
                        <span><strong>%d</strong> karma</span>
                        <span>Joined %s</span>
                    </div>
                    %s
                </div>
            </div>
            <div class='account-bio'>%s</div>
            %s
        </div>

        %s
        %s
    </div>" avatar_html esc_username role_badges karma joined_at header_actions (Components.html_escape bio) admin_controls tab_nav feed_html
  in
  (* Standard launch scroller column around the untouched account-* fragments;
     no noindex — the profile stays a public, crawlable discovery surface. *)
  let content =
    Printf.sprintf "<div class='scroll'><div class='container container--list'>%s</div></div>" body
  in
  Page_shell.launch_app_page ?user ~request ~rail_communities
    ~page_class:"launch-user-profile" ~title:(username ^ "'s Profile") ~content ()

(* /settings — the account-global settings surface, on the launch app chrome
   (Components.launch_app_page: earde.css only, no legacy per-page CSS).
   The five account-panel sections (multipart profile form, password form,
   data export, the consent-managed analytics panel, the delete-account
   danger zone) keep their markup verbatim: field names/order, CSRF
   positions, the data-analytics-* hooks analytics.js drives, and the
   confirmModal onsubmit are the load-bearing contracts. Launch chrome
   renders outside them: the legacy in-page Settings/Notifications account
   nav is superseded by the topbar user menu (same real destinations), and
   the serif page head carries the mono account identifier plus the existing
   view-profile link. Panels are skinned by the "account settings only"
   integration section at the end of earde.css.

   The stored bio is user-controlled via this form and was previously
   interpolated raw; it is now escaped at this template boundary (textarea
   text context) per the store-raw/escape-at-render convention.

   There is deliberately NO hidden existing_avatar_url field. It used to
   round-trip the stored avatar URL through the browser, and
   update_profile_handler wrote whatever came back straight into
   users.avatar_url — so any user could point their own row at another
   user's upload and have account deletion unlink that victim's file. The
   handler now re-reads the caller's own stored avatar from the database
   when no new file is submitted, which is the only value the browser could
   legitimately have supplied. The read-only preview below still renders
   [avatar_url]; it just no longer travels back as an input. *)
let settings_page ?user ?(rail_communities = []) bio avatar_url request =
  let csrf_token = Dream.csrf_tag request in
  let current_bio = Components.html_escape (Option.value ~default:"" bio) in

  (* Read-only preview of the stored avatar above the upload input. Same Components.user_avatar
     gate (safe_img_src) + letter-tile fallback as the profile header, so a missing/unsafe
     avatar shows the local tile rather than a broken image. Does not change the upload form. *)
  let avatar_preview =
    Components.user_avatar ~alt:"Current avatar"
      ~img_class:"account-avatar"
      ~tile_class:"account-avatar flex items-center justify-center text-2xl font-bold"
      ~username:(Option.value ~default:"" user) avatar_url
  in

  let identity_line =
    match user with
    | Some u ->
        Printf.sprintf "<p class='account-ident'>u/%s</p>"
          (Components.html_escape u)
    | None -> ""
  in
  let view_profile_link =
    match user with
    | Some u ->
        Printf.sprintf
          "<a class='account-view-profile' href='/u/%s'>View public profile &#8599;</a>"
          (Components.html_escape u)
    | None -> ""
  in
  let page_head =
    Printf.sprintf
      "<div class='page__head'><div class='page__head-inner page__head-inner--list account-head-launch'>\
       <div><h1 class='page__title page__title--sm'>Account settings</h1>%s</div>\
       %s</div></div>"
      identity_line view_profile_link
  in

  let body = Printf.sprintf "
    <div class='account-wrap account-wrap--narrow'>
        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Profile information</h2>
            <form action='/settings' method='POST' enctype='multipart/form-data' class='account-form'>
                %s
                <div class='account-field'>
                    <label class='account-label'>Avatar <span class='account-label-opt'>(optional)</span></label>
                    <div class='account-avatar-row'>
                        %s
                        <div class='account-avatar-ctl'>
                            <input type='file' name='avatar_url' accept='image/*' class='account-file'>
                            <span class='account-hint'>PNG or JPG &middot; square images look best</span>
                        </div>
                    </div>
                </div>
                <div class='account-field'>
                    <label class='account-label'>Bio</label>
                    <textarea name='bio' rows='4' class='account-textarea' placeholder='Tell the community a bit about yourself...'>%s</textarea>
                </div>
                <button type='submit' class='account-btn'>Save profile</button>
            </form>
        </div>

        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Change password</h2>
            <form action='/settings/password' method='POST' class='account-form'>
                %s
                <div class='account-field'>
                    <label class='account-label'>Current password</label>
                    <input type='password' name='old_password' required class='account-input'>
                </div>
                <div class='account-field-pair'>
                    <div class='account-field'>
                        <label class='account-label'>New password</label>
                        <input type='password' name='new_password' required minlength='8' class='account-input'>
                    </div>
                    <div class='account-field'>
                        <label class='account-label'>Confirm new password</label>
                        <input type='password' name='confirm_password' required minlength='8' class='account-input'>
                    </div>
                </div>
                <button type='submit' class='account-btn account-btn--secondary'>Update password</button>
            </form>
        </div>

        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Your data</h2>
            <p class='account-section-desc'>Export your account data as JSON.</p>
            <a href='/export-data' class='account-btn account-btn--secondary'>&#8595; Download my data</a>
        </div>

        <div class='account-panel account-panel--card' data-analytics-settings hidden>
            <h2 class='account-section-title'>Analytics</h2>
            <p class='account-section-desc'>Control whether Earde may collect optional product-usage analytics (PostHog) in your browser. Nothing is collected without your consent &mdash; see the <a href='/privacy#cookies-analytics'>Privacy Policy</a> for what analytics covers.</p>
            <div class='account-form'>
                <span data-analytics-state class='account-hint'></span>
                <div>
                    <button type='button' data-analytics-accept class='account-btn account-btn--secondary'>Enable analytics</button>
                    <button type='button' data-analytics-refuse class='account-btn account-btn--secondary'>Disable analytics</button>
                </div>
                <span data-analytics-error hidden class='account-hint' style='color:#C94C4C;'>Couldn&#39;t save your choice &mdash; please try again.</span>
            </div>
        </div>

        <div class='account-panel account-danger'>
            <h2 class='account-section-title'>Danger zone</h2>
            <p class='account-section-desc'>Permanently delete your account and personal data. Your posts and comments will remain, but their author will be anonymized as <strong>[deleted]</strong>. This action is irreversible.</p>
            <form action='/delete-account' method='POST' onsubmit=\"confirmModal(event, 'WARNING: Permanently delete your personal data? This action is irreversible.')\">
                %s
                <button type='submit' class='account-btn account-btn--danger'>Delete account</button>
            </form>
        </div>
    </div>"
    csrf_token avatar_preview current_bio csrf_token csrf_token
  in
  let content =
    Printf.sprintf
      "%s<div class='scroll'><div class='container container--list'>%s</div></div>"
      page_head body
  in
  Page_shell.launch_app_page ~noindex:true ?user ~request ~rail_communities
    ~page_class:"launch-account-settings" ~title:"Settings" ~content ()

(* /notifications — the account-global notification center, on the launch
   app chrome (Components.launch_app_page: earde.css only, no legacy
   per-page CSS). The row renderer below is kept byte-for-byte: its anatomy
   (account-notif / account-notif-icon / account-notif-body / account-notif-msg
   / account-notif-time, the account-notif--unread accent and its cosmetic
   onclick clear) is pinned by the gated Phnt UI suite's substring checks and
   by the replay-masking selector .account-notif-msg in analytics.js. Rows are
   skinned onto the approved flat .notif anatomy by the "notifications only"
   integration section at the end of earde.css. The handoff's filter tabs and
   "mark all read" POST are deliberately absent — no such routes exist; the
   GET itself marks everything read (Notification_store.mark_notifs_read) exactly as before. *)
let notifications_page ?user ?(rail_communities = []) (notifs : Notification_store.notification list) request =
  let render_notif (n : Notification_store.notification) =
    let unread_class = if n.is_read then "" else " account-notif--unread" in
    (* Project-home notifications are structured: no stored prose, so the
       label and destination derive from the durable kind and the joined
       current project/community identity. Names and slugs are escaped at
       this template boundary; destinations are built structurally from
       the joined slugs (never from anything browser-supplied), and each
       destination page enforces its own authorization. The actor is
       deliberately absent from the copy, so a deleted actor renders
       identically. Should a joined identity be missing mid-race (the
       subject FKs cascade), the row degrades to a generic unlinked line
       rather than inventing names. *)
    let project_home_label_link =
      match n.notif_type with
      | "project_home_requested" | "project_home_accepted"
      | "project_home_rejected" | "project_home_removed" -> (
          match (n.project_name, n.project_slug, n.community_name, n.community_slug) with
          | Some project_name, Some project_slug, Some community_name, Some community_slug ->
              let p = Components.html_escape project_name in
              let c = Components.html_escape community_name in
              let label, href =
                match n.notif_type with
                | "project_home_requested" ->
                    ( Printf.sprintf "%s requested %s as its community home" p c,
                      Printf.sprintf "/c/%s/project-home-requests" (Components.html_escape community_slug) )
                | "project_home_accepted" ->
                    ( Printf.sprintf "%s accepted the community-home request for %s" c p,
                      Printf.sprintf "/projects/%s/request-home" (Components.html_escape project_slug) )
                | "project_home_rejected" ->
                    ( Printf.sprintf "%s rejected the community-home request for %s" c p,
                      Printf.sprintf "/projects/%s/request-home" (Components.html_escape project_slug) )
                | _ ->
                    (* Removal is the one kind whose recipients span BOTH
                       sides: the removal store notifies every project
                       steward and every community top moderator (minus the
                       actor). A steward-only destination would 404 for the
                       moderators, so this points at the community's own
                       page — reachable by a top moderator always, and by a
                       steward for exactly the published (public/unlisted)
                       communities a home can be removed from. Its
                       Connected-projects section is also where the removal
                       is actually visible to either side. *)
                    ( Printf.sprintf "%s is no longer connected to %s as its home" p c,
                      Printf.sprintf "/c/%s" (Components.html_escape community_slug) )
              in
              Some (label, Some href)
          | _ -> Some ("A project community-home update", None))
      | _ -> None
    in
    (* Community-connection notifications are structured the same way: no
       stored prose, so the copy and destination derive from the durable kind,
       the joined counterpart identity, and the recipient's own management
       context. The counterpart is derived by the read model relative to the
       stored community, so one row reads correctly from either direction.
       Every destination is that recipient's own connections surface, which
       re-proves its top-mod/admin gate for itself — the link grants nothing.
       Reviewer and remover are deliberately absent from the copy, so a
       deleted actor renders identically and no username ever appears. If the
       connection or either community has gone, the row degrades to a generic
       unlinked line rather than inventing a name. *)
    let connection_label_link =
      match n.notif_type with
      | "community_connection_requested" | "community_connection_accepted"
      | "community_connection_rejected" | "community_connection_removed" -> (
          match (n.counterpart_name, n.community_slug) with
          | Some counterpart_name, Some context_slug ->
              let other = Components.html_escape counterpart_name in
              let label =
                match n.notif_type with
                | "community_connection_requested" ->
                    Printf.sprintf "%s wants to connect with your community." other
                | "community_connection_accepted" ->
                    Printf.sprintf "%s accepted your connection request." other
                | "community_connection_rejected" ->
                    Printf.sprintf "%s rejected your connection request." other
                | _ ->
                    Printf.sprintf "%s removed the community connection." other
              in
              Some
                ( label,
                  Some
                    (Printf.sprintf "/c/%s/settings/connections"
                       (Components.html_escape context_slug)) )
          | _ -> Some ("A community connection update", None))
      | _ -> None
    in
    (* Shared-thread notifications are structured the same way: no stored
       prose, so the copy and destination derive from the durable kind, the
       joined canonical thread and community identities, and the recipient's
       own side of the placement. Unlike the two structured families above,
       the thread title is canonical CONTENT and either community may since
       have gone private, so every detail is gated on the recipient's
       CURRENT read access (the two st_*_visible booleans, computed durably
       in the same bounded query). When any needed detail is inaccessible or
       gone the row degrades to a generic unlinked line — never deleted,
       never naming what its recipient may no longer reach. The acting user
       and the private request note are deliberately absent from the copy,
       so a deleted actor renders identically and no username or note ever
       appears. Every destination is built structurally from the joined
       slugs and re-proves its own authorization for itself — the link
       grants nothing — but a link only renders when the matching
       capability boolean says its target page would currently let this
       recipient in: the Share page and the management page gate harder
       than reading, and a link that is guaranteed to 404 is worse than
       none. *)
    let shared_thread_label_link =
      match n.notif_type with
      | "shared_thread_requested" | "shared_thread_accepted"
      | "shared_thread_rejected" | "shared_thread_removed"
      | "shared_thread_withdrawn" -> (
          match
            ( ( n.st_post_id, n.st_post_title, n.st_origin_name,
                n.st_origin_slug ),
              ( n.st_destination_name, n.st_destination_slug,
                n.st_origin_context ),
              (n.st_origin_visible, n.st_destination_visible) )
          with
          | ( ( Some post_id, Some title, Some origin_name, Some origin_slug ),
              ( Some destination_name, Some destination_slug,
                Some origin_context ),
              (Some true, Some true) ) ->
              let t = Components.html_escape title in
              let o = Components.html_escape origin_name in
              let d = Components.html_escape destination_name in
              (* Read access (checked above) decides what may be named; the
                 two capability booleans decide what may be linked, because
                 both link targets carry stricter gates than reading. A
                 Share-page link degrades to the canonical thread — always
                 readable here, since this branch requires current origin
                 read access — and a management link degrades to plain
                 text: never a link its recipient is guaranteed to 404
                 on. *)
              let thread_path =
                Post_cards.canonical_thread_path origin_slug post_id title
              in
              let share_or_thread =
                Components.html_escape
                  (if n.st_share_capable = Some true then
                     thread_path ^ "/share"
                   else thread_path)
              in
              let destination_settings suffix =
                if n.st_manage_capable = Some true then
                  Some
                    (Printf.sprintf "/c/%s/settings/shared-threads%s"
                       (Components.html_escape destination_slug)
                       suffix)
                else None
              in
              let label, href =
                match n.notif_type with
                | "shared_thread_requested" ->
                    ( Printf.sprintf
                        "%s requested to share &#8220;%s&#8221; with %s" o t d,
                      destination_settings "#incoming" )
                | "shared_thread_accepted" ->
                    ( Printf.sprintf "%s accepted &#8220;%s&#8221;" d t,
                      Some share_or_thread )
                | "shared_thread_rejected" ->
                    ( Printf.sprintf "%s declined &#8220;%s&#8221;" d t,
                      Some share_or_thread )
                | "shared_thread_withdrawn" ->
                    ( Printf.sprintf
                        "A request to share &#8220;%s&#8221; with %s was \
                         withdrawn"
                        t d,
                      destination_settings "" )
                | _ ->
                    (* Removal notifies both sides: origin-context
                       recipients return to the thread's own Share page,
                       destination-context ones to their management
                       surface. *)
                    ( Printf.sprintf
                        "&#8220;%s&#8221; is no longer shared with %s" t d,
                      if origin_context then Some share_or_thread
                      else destination_settings "" )
              in
              Some (label, href)
          | _ -> Some ("A shared thread update", None))
      | _ -> None
    in
    (* One structured slot: a notification is project-home, community-
       connection, or shared-thread — never two at once (the durable shape
       CHECK holds exactly one subject family per row), so the three
       derivations cannot collide. *)
    let structured_label_link =
      match project_home_label_link with
      | Some _ as label -> label
      | None -> (
          match connection_label_link with
          | Some _ as label -> label
          | None -> shared_thread_label_link)
    in
    let message = Option.value n.message ~default:"" in
    let icon = match n.notif_type with
      | "mention"    -> "&#64;"   (* @ symbol — avoids mojibake in Printf *)
      | "mod_action" -> "&#9888;" (* ⚠ warning sign *)
      | "project_home_requested" | "project_home_accepted"
      | "project_home_rejected" | "project_home_removed" -> "&#127968;" (* 🏠 *)
      | "community_connection_requested" | "community_connection_accepted"
      | "community_connection_rejected" | "community_connection_removed" ->
          "&#8644;" (* ⇄ — the same sigil the Connections nav entry uses *)
      | "shared_thread_requested" | "shared_thread_accepted"
      | "shared_thread_rejected" | "shared_thread_removed"
      | "shared_thread_withdrawn" -> "&#128279;" (* 🔗 — one thread, linked elsewhere *)
      | _ ->
          (* Legacy comment_reply: distinguish post vs comment reply by message suffix. *)
          let len = String.length message in
          if len >= 5 && String.sub message (len - 5) 5 = "post." then "&#128221;" (* 📝 *)
          else "&#128172;" (* 💬 *)
    in
    (* Structured labels are built above from already-escaped parts; legacy
       prose is escaped here. *)
    let msg_html =
      match structured_label_link with
      | Some (label, _) -> label
      | None -> Components.html_escape message
    in
    let inner = Printf.sprintf "
        <div class='account-notif-icon'>%s</div>
        <div class='account-notif-body'>
            <div class='account-notif-msg'>%s</div>
            <div class='account-notif-time'>%s</div>
        </div>" icon msg_html (Components.time_ago n.created_at)
    in
    let link =
      match structured_label_link with
      | Some (_, link) -> link
      | None -> (
          match n.post_id with
          | Some pid -> Some (Printf.sprintf "/p/%d" pid)
          | None -> None)
    in
    (* Notifications without a destination render as non-clickable divs.
       Read state is already persisted server-side on page load (Notification_store.mark_notifs_read);
       the onclick is a purely cosmetic clear of the unread accent on this visit. *)
    match link with
    | Some href ->
        Printf.sprintf "
    <a href='%s' onclick=\"this.classList.remove('account-notif--unread');\" class='account-notif%s'>%s
    </a>" href unread_class inner
    | None ->
        Printf.sprintf "
    <div class='account-notif%s'>%s
    </div>" unread_class inner
  in
  let list_html =
    if notifs = [] then "<div class='account-empty'>No notifications yet.</div>"
    else Printf.sprintf "<div class='account-notifs'>%s</div>" (String.concat "\n" (List.map render_notif notifs))
  in
  (* Launch chrome renders outside the pinned row fragments: serif page head
     (the legacy in-page Settings/Notifications nav is superseded by the
     topbar user menu), then the standard scroller column. The sub line names
     only kinds the backend actually produces — no "promotions". *)
  let page_head =
    "<div class='page__head'><div class='page__head-inner page__head-inner--list'>\
     <h1 class='page__title'>Notifications</h1>\
     <p class='page__sub'>Replies, mentions, moderation decisions and project requests.</p>\
     </div></div>"
  in
  let content =
    Printf.sprintf "%s<div class='scroll'><div class='container container--list'>%s</div></div>"
      page_head list_html
  in
  Page_shell.launch_app_page ~noindex:true ?user ~request ~rail_communities
    ~page_class:"launch-notifications" ~title:"Notifications" ~content ()
