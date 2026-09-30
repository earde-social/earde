open Html.Infix

(* [connected_projects] is the pre-rendered Connected-projects fragment supplied by the
   community route (Community_connected_projects_pages), empty when the community has no
   accepted project home and always empty on the section feeds, which are not /c/:slug.
   [connected_communities] is its community-to-community counterpart
   (Community_connected_communities_pages), empty when nothing is publicly connected or the
   community is not itself connection-eligible; that decision belongs to the read model,
   not to this template. *)
(* Launch sidebar for the flat (sections_enabled = false) community home — the
   launch sidebar grammar reduced to what a single-feed community really has: the
   identity head, the one Feed surface (always current — /c/:slug IS the feed),
   the factual private marker, and the Network group with the intentionally
   public moderation log plus Settings under the exact mod-or-admin gate the
   settings handler enforces. No Live or Knowledge groups: a flat community has
   no channel or section surfaces, and nothing is invented. Home requests,
   Reports and Manage moderators stay off this sidebar (the settings/report
   suites count those links per document); Manage moderators keeps its legacy
   placement inside the Moderators panel instead. *)
let launch_flat_community_sidebar ~(community : Community_types.community) ~can_manage () =
  let slug = (Html.text (community.slug)) in
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None ->
             (Html.template "<span class='avatar avatar--32' style='background:%s'>%s</span>"
  [ (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
         | Some src ->
             (Html.template "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>"
  [ src ]))
    | _ ->
        (Html.template "<span class='avatar avatar--32' style='background:%s'>%s</span>"
  [ (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
  in
  let side_head =
    (Html.template "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
  [ slug
  ; side_face
  ; (Html.text (community.name))
  ; slug ])
  in
  let nav_feed =
    (Html.template "<a class='navitem navitem--pad navitem--active' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Feed</a>"
  [ slug ])
  in
  let vis_note =
    if community.visibility = Community_types.Community_private then
      (Html.static "<div class='launch-side-vis'>private community</div>")
    else Html.empty
  in
  let nav_network =
    (Html.static "<p class='kicker sidebar__group'>Network</p>")
    ++ (Html.template "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
  [ slug ])
    ++ (if can_manage then
         (Html.template "<a class='navitem navitem--pad' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
  [ slug ])
       else Html.empty)
  in
  (Html.template "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s</div></aside>"
  [ (Html.text (community.name))
  ; side_head
  ; nav_feed
  ; vis_note
  ; nav_network ])

(* /c/:slug (flat, sections_enabled = false) — the single-feed community home,
   now on the Cartographic Civic launch chrome (Components.launch_community_page:
   earde.css only, no Tailwind/Google Fonts). This stays a wrapper-and-CSS
   conversion of the legacy simple feed, not a structured community: one feed,
   no channels, no sections, no invented hierarchy. Every contract is the
   pre-launch behavior reskinned:
   - the feed rows and the empty state come from the untouched shared
     Components.render_post / legacy strings byte-for-byte (their DOM is the
     optimistic-vote, confirmModal, dialog and ph-mask contract shared with the
     legacy / and /all feeds) and are skinned purely by the route-scoped CSS;
   - sort (?sort=hot|new|top, unknown→hot upstream) and pagination
     (?sort=&page=, "Page N", prev iff page>1, next iff a full 20-row page)
     keep their exact URLs and grammar;
   - the /join and /leave POSTs keep their exact fields and presence rules
     (anon: none; member: Leave; non-member: Join unless private);
   - + New post keeps the real /new-post?community= destination (still legacy);
   - Settings (mod/admin), Manage moderators (top-mod/admin), the public
     modlog and the top-mod/admin downvote toggle keep their gates and routes;
   - the pre-rendered ccp-* connected-projects fragment is spliced verbatim
     (its markup is pinned by the fragment suites) and restyled by CSS only. *)
let community_page ?user ?(noindex=false) ?(connected_projects = Html.empty) ?(connected_communities = Html.empty) ~is_member ~is_current_user_mod ~is_current_user_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities user_votes current_page sort_mode (community : Community_types.community) (posts : Post_types.feed_item list) request =
  let csrf_token = Csrf_field.tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let slug = (Html.text (community.slug)) in
  (* Fed only the legacy left sidebar; the launch shell's global rail owns
     joined-community navigation now. Kept in the signature so the handler
     call site (shared with the structured branch's data load) stays intact. *)
  ignore (moderated_communities : Community_types.community list);
  let has_next = List.length posts = 20 in

  (* Own rows render byte-identically; a shared row carries THIS community's
     slug as its destination context so its links stay local and its
     provenance/section chips come from the placement, not the origin. *)
  let posts_html =
    if posts = [] then (Html.static "<div class='bg-gray-50 p-12 text-center rounded-xl border border-dashed border-[#D0C9BC] text-gray-500'>No posts yet. Be the first to share something!</div>")
    else (Html.join (Html.static "\n")) (List.map (fun (item : Post_types.feed_item) ->
      let shared = Option.map (fun ctx -> (community.slug, ctx)) item.fi_shared in
      Post_cards.render_post ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ?shared request user_votes item.fi_post) posts)
  in

  let base_url = Printf.sprintf "/c/%s" (community.slug) in

  (* Sort chips — the exact hrefs and accepted values of the legacy tabs
     (hot/new/top; the handler maps anything else to hot). *)
  let chip mode label =
    let cls = if mode = sort_mode then "chip chip--active" else "chip" in
    (Html.template "<a class='%s' href='%s?sort=%s'>%s</a>"
  [ (Html.text cls)
  ; (Html.text base_url)
  ; Html.text mode
  ; label ])
  in
  let sort_bar =
    (Html.template "<div class='tabbar launch-flat-sortbar'><div class='chips' aria-label='Sort'>%s%s%s</div></div>"
  [ (chip "hot" (Html.static "Hot"))
  ; (chip "new" (Html.static "New"))
  ; (chip "top" (Html.static "Top")) ])
  in

  (* Pager — same ?sort=&page= URLs and the same presence rules as before. *)
  let prev_btn = if current_page <= 1 then Html.empty else (Html.template "<a class='btn btn--secondary btn--sm' href='%s?sort=%s&page=%s'>&larr; Previous</a>"
  [ (Html.text base_url)
  ; Html.text sort_mode
  ; Html.int ((current_page - 1)) ]) in
  let next_btn = if not has_next then Html.empty else (Html.template "<a class='btn btn--secondary btn--sm' href='%s?sort=%s&page=%s'>Next &rarr;</a>"
  [ (Html.text base_url)
  ; Html.text sort_mode
  ; Html.int ((current_page + 1)) ]) in
  let pager = (Html.template "<div class='pager launch-pager'>%s<span>Page %s</span>%s</div>"
  [ prev_btn
  ; Html.int (current_page)
  ; next_btn ]) in

  let membership_btn =
    match user with
    | None -> Html.empty
    | Some _ ->
        if is_member then (Html.template "<form action='/leave' method='POST'>%s<input type='hidden' name='community_id' value='%s'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--secondary btn--block launch-leave'>Leave</button></form>"
  [ csrf_token
  ; Html.int (community.id)
  ; (Html.text community.slug) ])
        (* No self-serve join for private communities: a non-member who can see this
           page is a mod/admin; show no misleading Join button (the /join route 404s anyway). *)
        else if community.visibility = Community_types.Community_private then Html.empty
        else (Html.template "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%s'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--primary btn--block'>Join</button></form>"
  [ csrf_token
  ; Html.int (community.id)
  ; (Html.text community.slug) ])
  in

  (* Admins see the settings entry without needing mod status — global authority. *)
  let settings_btn =
    if is_current_user_mod || is_admin then
      (Html.template "<a class='btn btn--secondary' href='/c/%s/settings'>&#9881; Settings</a>"
  [ slug ])
    else Html.empty
  in

  (* Create Post shortcut keeps its real legacy destination and query parameter
     (/new-post stays a legacy page this pass). Logged-in viewers only. *)
  let create_post_btn =
    match user with
    | None -> Html.empty
    | Some _ ->
        (Html.template "<a class='btn btn--primary' href='/new-post?community=%s'>+ New post</a>"
  [ (Html.text community.slug) ])
  in

  (* Community face: avatar image when set and safe, else the launch tile glyph
     on the deterministic palette tone — same fallback as the rail and sidebar. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let face size_cls =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None ->
             (Html.template "<span class='avatar %s' style='background:%s'>%s</span>"
  [ size_cls
  ; (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
         | Some src ->
             (Html.template "<span class='avatar %s'><img class='launch-avatar-img' src='%s' alt=''></span>"
  [ size_cls
  ; src ]))
    | _ ->
        (Html.template "<span class='avatar %s' style='background:%s'>%s</span>"
  [ size_cls
  ; (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
  in

  (* Optional banner — rendered only when a safe banner_url is present; no
     gradient placeholder (launch grammar, matching the structured overview). *)
  let banner_html =
    match community.banner_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None -> Html.empty
         | Some src -> (Html.template "<div class='launch-banner'><img src='%s' class='launch-banner-img' alt=''></div>"
  [ src ]))
    | _ -> Html.empty
  in

  (* Header band. Badges are facts the record already states; public is
     unmarked. No stats line — this page loads no community-wide counts and
     none are fabricated. *)
  let badges =
    (if community.is_network_community then
       (Html.static " <span class='badge badge--network badge--lg'>&#9672; Network community</span>")
     else Html.empty)
    ++ (if community.visibility = Community_types.Community_private then
         (Html.static " <span class='badge badge--plain badge--lg'>Private</span>")
       else Html.empty)
  in
  let actions = create_post_btn ++ membership_btn ++ settings_btn in
  let chead =
    (Html.template "<div class='chead'><div class='chead__row'>%s<div class='launch-chead-id'>\
       <div class='titleline'><h1 class='chead__title'>%s</h1><span class='chead__slug'>/c/%s</span>%s</div>\
       <p class='chead__desc'>%s</p>\
       </div>%s</div></div>"
  [ (face (Html.static "avatar--62"))
  ; (Html.text (community.name))
  ; slug
  ; badges
  ; (Html.text ((Option.value ~default:"No description." community.description)))
  ; (if actions = Html.empty then Html.empty else (Html.template "<div class='chead__actions'>%s</div>"
  [ actions ])) ])
  in

  (* Moderators panel. Manage moderators keeps its legacy placement here and
     its exact top-mod-or-admin gate — regular mods cannot appoint or demote
     peers, preventing collusion against the council. *)
  let manage_mods_link =
    if is_current_user_top_mod || is_admin then
      (Html.template "<div class='launch-manage-mods'><a class='btn--link mono' href='/c/%s/manage-mods'>Manage moderators &rarr;</a></div>"
  [ slug ])
    else Html.empty
  in
  let mods_inner =
    if mod_usernames = [] then (Html.static "<div class='empty--inline'>No moderators yet.</div>")
    else
      (Html.template "<div class='launch-mods'>%s</div>"
  [ (Html.concat (List.map (fun u ->
          let initial =
            if String.length u > 0 then (Html.text ((String.sub (String.uppercase_ascii u) 0 1))) else (Html.static "?") in
          (Html.template "<a class='launch-modrow' href='/u/%s'><span class='avatar avatar--22 avatar--mod'>%s</span><span class='launch-mod-name'>u/%s</span></a>"
  [ (Html.text (u))
  ; initial
  ; (Html.text (u)) ])) mod_usernames)) ])
  in
  let mods_panel =
    (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Moderators</span></div>%s%s</section>"
  [ mods_inner
  ; manage_mods_link ])
  in

  let rules_panel = match community.rules with
    | Some r when r <> "" ->
        (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Rules</span></div><div class='panel__body launch-rules'>%s</div></section>"
  [ (Html.text (r)) ])
    | _ -> Html.empty
  in

  (* Toggle downvotes: exposed only to top_mod/admin to prevent vote manipulation arms races
     by regular mods who have less community-wide accountability. Same POST
     route and allow_downvotes field as before. *)
  let mod_tools_panel =
    if is_current_user_top_mod || is_admin then
      let (next_val, label, state) =
        if community.allow_downvotes then ("false", "Disable downvotes", "enabled")
        else ("true", "Enable downvotes", "disabled")
      in
      (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Mod tools</span></div><div class='panel__body'><p class='launch-modtool-state'>Downvotes are currently %s.</p><form action='/c/%s/toggle_downvotes' method='POST'>%s<input type='hidden' name='allow_downvotes' value='%s'><button type='submit' class='btn btn--secondary btn--sm'>%s</button></form></div></section>"
  [ (Html.text state)
  ; slug
  ; csrf_token
  ; (Html.text next_val)
  ; (Html.text label) ])
    else Html.empty
  in

  let sidebar =
    launch_flat_community_sidebar ~community
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* Single-feed body: the ledger column plus the factual side panels. The
     pre-rendered ccp-* fragment closes the side stack, spliced verbatim. *)
  let content =
    (Html.template "<div class='scroll'>%s%s<div class='container launch-flat-body'><div class='two-col'><div class='stack'>%s<div class='launch-flat-ledger'>%s</div>%s</div><div class='stack stack--sm'>%s%s%s%s%s</div></div></div></div>"
  [ banner_html
  ; chead
  ; sort_bar
  ; posts_html
  ; pager
  ; mods_panel
  ; rules_panel
  ; mod_tools_panel
  ; connected_projects
  ; connected_communities ])
  in
  (* The byte-pinned rows show Share to every viewer, but only member
     documents carry the full behavior script (which defines copyPostLink).
     Guests get the share-only script — same single copyPostLink source, no
     notification fetch — so the control works in both viewer states and each
     document holds exactly one definition. *)
  let head_extra = match user with
    | None -> Page_shell.launch_share_script
    | Some _ -> Html.empty
  in
  Community_shell.launch_community_page ?user ~noindex ~request ~head_extra
    ~rail_communities:user_communities ~community ~sidebar
    ~page_class:"launch-flat-community" ~title:community.name ~content ()

(* The forum-section feed inside a structured community, and (below) the canonical thread
   view — the knowledge routes on the Cartographic Civic launch chrome. Kept as
   separate functions (not folded into community_page) so the simple-community feed and the
   section feed can diverge in chrome without one breaking the other. SSR-only — every link
   works with JS disabled; the page JS only enhances. *)
(* Launch community sidebar for the knowledge routes (section + thread), in the exact
   launch sidebar grammar: identity head, Overview link, factual visibility marker, Live channels
   (never active here — these are forum surfaces), Knowledge sections with the current/parent
   section active, and the intentionally public moderation log. Settings renders only under
   the is_mod-or-admin gate its handler enforces; Home requests needs the top-mod authority
   the section/thread handlers never load, so it is not rendered — nothing is invented. The
   virtual Uncategorized feed is appended (active) only when the viewer is on it: the handler
   404s that route unless real orphaned content exists, so the entry is always backed by
   data. Archived channels are hidden, mirroring the pre-launch Channels nav group.
   [settings_active] marks the Settings entry current — used by the settings hub and by
   every management surface on the shared settings shell (Connections, Shared threads,
   Project home requests, Manage moderators, Reports): inside settings, Settings is the
   one active community-level item and the internal settings navigation distinguishes
   the surfaces. Each of those routes re-proved its own gate before rendering.
   [show_visibility_note] defaults to the existing factual marker; the management
   surfaces pass false because they never name an ineligibility reason
   (private/draft/legacy) anywhere in their documents.
   [moderation_log_active] marks the always-present Moderation log entry current —
   used only by the modlog route, which is public by design, so the
   entry itself renders for every viewer exactly as before. *)
let launch_knowledge_sidebar ~(community : Community_types.community) ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list) ?active_section_slug
    ?(append_uncategorized = false) ?(settings_active = false)
    ?(moderation_log_active = false)
    ?(show_visibility_note = true) ~can_manage () =
  let slug = (Html.text (community.slug)) in
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None ->
             (Html.template "<span class='avatar avatar--32' style='background:%s'>%s</span>"
  [ (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
         | Some src ->
             (Html.template "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>"
  [ src ]))
    | _ ->
        (Html.template "<span class='avatar avatar--32' style='background:%s'>%s</span>"
  [ (Html.text (Page_shell.launch_tile_color community.slug))
  ; (Html.text (tile_glyph)) ])
  in
  let side_head =
    (Html.template "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
  [ slug
  ; side_face
  ; (Html.text (community.name))
  ; slug ])
  in
  let nav_overview =
    (Html.template "<a class='navitem navitem--pad' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
  [ slug ])
  in
  let vis_note =
    if show_visibility_note && community.visibility = Community_types.Community_private then
      (Html.static "<div class='launch-side-vis'>private community</div>")
    else Html.empty
  in
  let live_channels = List.filter (fun (c : Channel_store.channel) -> not c.is_archived) channels in
  let nav_live =
    if live_channels = [] then Html.empty
    else
      (Html.static "<p class='kicker sidebar__group'>Live</p>")
      ++ Html.concat (List.map (fun (c : Channel_store.channel) ->
          (Html.template "<a class='navitem' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
  [ slug
  ; (Html.text (c.slug))
  ; (Html.text (c.slug)) ]))
          live_channels)
  in
  let section_item (s : Section_store.community_section) =
    let cls =
      if active_section_slug = Some s.slug then "navitem navitem--active" else "navitem" in
    (Html.template "<a class='%s' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s</a>"
  [ (Html.text cls)
  ; slug
  ; (Html.text (s.slug))
  ; (Html.text (s.name)) ])
  in
  let knowledge_items =
    List.map section_item sections
    @ (if append_uncategorized then
         [ (Html.template "<a class='navitem navitem--active' href='/c/%s/s/uncategorized'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>Uncategorized</a>"
  [ slug ]) ]
       else [])
  in
  let nav_knowledge =
    if knowledge_items = [] then Html.empty
    else (Html.static "<p class='kicker sidebar__group'>Knowledge</p>") ++ Html.concat knowledge_items
  in
  let nav_network =
    (Html.static "<p class='kicker sidebar__group'>Network</p>")
    ++ (Html.template "<a class='navitem navitem--pad%s' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
  [ (if moderation_log_active then (Html.static " navitem--active") else Html.empty)
  ; slug ])
    ++ (if can_manage then
         (Html.template "<a class='navitem navitem--pad%s' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
  [ (if settings_active then (Html.static " navitem--active") else Html.empty)
  ; slug ])
       else Html.empty)
  in
  (Html.template "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
  [ (Html.text (community.name))
  ; side_head
  ; nav_overview
  ; vis_note
  ; nav_live
  ; nav_knowledge
  ; nav_network ])

let community_section_shell_page ?user ?(noindex=false) ?thread_count ?last_activity ~is_current_user_mod ~mod_usernames ~admin_usernames
    ~banned_usernames ~(rail_communities : Community_types.community list) ~(channels : Channel_store.channel list)
    ~(sections : Section_store.community_section list)
    ~(section : Section_store.community_section) ~user_votes ~current_page ~sort_mode
    ~(community : Community_types.community) ~(posts : Post_types.feed_item list) request =
  (* base_url drives the sort tabs, the New-thread link, and pagination — all stay on the section URL. *)
  let base_url = Printf.sprintf "/c/%s/s/%s" (community.slug) (section.slug) in

  (* Launch community sidebar: the current section is active; the virtual
     Uncategorized feed isn't a real section row, so it is appended (active) only when we're
     on it — keeps it highlighted without an extra orphaned-count query just to decorate the
     sidebar. Settings gate mirrors the overview: the render-time check is visibility only,
     the settings handler re-checks authority. *)
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ?active_section_slug:(if section.slug = "uncategorized" then None else Some section.slug)
      ~append_uncategorized:(section.slug = "uncategorized")
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* New-thread action: pre-selects this section; suppressed for anon users and the virtual
     Uncategorized feed (users cannot post directly into it). Shared by header + right rail. *)
  let new_thread_btn ?(cls="btn sm primary") () =
    match user with
    | None -> Html.empty
    | Some _ ->
        if section.slug = "uncategorized" then Html.empty
        else (Html.template "<a href='/new-post?community=%s&section=%s' class='%s'>+ New thread</a>"
  [ (Html.text (community.slug))
  ; (Html.text (section.slug))
  ; (Html.text cls) ])
  in

  (* Sort tabs — Hot/New/Top/Active, active highlighted. No counts (they would be fabricated) and
     no Unanswered (no such state). Preserves the ?sort override + the section's default sort. *)
  let tab mode label =
    let cls = if mode = sort_mode then " class='active'" else "" in
    (Html.template "<a%s href='%s?sort=%s'>%s</a>"
  [ (Html.text cls)
  ; (Html.text base_url)
  ; Html.text mode
  ; label ])
  in
  let ftabs = (Html.template "<div class='ftabs'>%s%s%s%s</div>"
  [ (tab "hot" (Html.static "Hot"))
  ; (tab "new" (Html.static "New"))
  ; (tab "top" (Html.static "Top"))
  ; (tab "active" (Html.static "Active")) ])
  in

  (* Section header: breadcrumb, prominent § title, optional description, New thread, sort tabs.
     It lives outside cs-main-body so it stays put while the thread list scrolls under it. *)
  let desc_html = match section.description with
    | Some d when String.trim d <> "" -> (Html.template "<div class='fh-desc'>%s</div>"
  [ (Html.text (d)) ])
    | _ -> Html.empty
  in
  let forum_head = (Html.template "
    <div class='forum-head'>
        <div class='fh-crumb'><a href='/c/%s'>/c/%s</a> / <b>§ %s</b></div>
        <div class='fh-top'>
            <div><div class='fh-title'><span class='sec'>§</span> %s</div>%s</div>
            <div class='fh-actions'>%s</div>
        </div>
        %s
    </div>"
  [ (Html.text (community.slug))
  ; (Html.text (community.slug))
  ; (Html.text (section.name))
  ; (Html.text (section.name))
  ; desc_html
  ; (new_thread_btn ())
  ; ftabs ])
  in

  (* Own rows are byte-identical to before; shared rows carry this
     community's slug as their destination link context and their compact
     "Shared from" provenance line. *)
  let posts_html =
    if posts = [] then (Html.static "<div class='cs-empty'>No threads here yet — start the first one.</div>")
    else (Html.join (Html.static "\n")) (List.map (fun (item : Post_types.feed_item) ->
      let shared = Option.map (fun ctx -> (community.slug, ctx)) item.fi_shared in
      Post_cards.render_forum_row ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ?shared request user_votes item.fi_post) posts)
  in

  let has_next = List.length posts = 20 in
  let prev_btn = if current_page <= 1 then Html.empty else (Html.template "<a class='btn sm' href='%s?sort=%s&page=%s'>&larr; Previous</a>"
  [ (Html.text base_url)
  ; Html.text sort_mode
  ; Html.int ((current_page - 1)) ]) in
  let next_btn = if not has_next then Html.empty else (Html.template "<a class='btn sm' href='%s?sort=%s&page=%s'>Next &rarr;</a>"
  [ (Html.text base_url)
  ; Html.text sort_mode
  ; Html.int ((current_page + 1)) ]) in
  let pager = (Html.template "<div class='cs-pager'><div>%s</div><span class='cs-pager-n'>Page %s</span><div>%s</div></div>"
  [ prev_btn
  ; Html.int (current_page)
  ; next_btn ]) in

  (* cs-flush zeroes cs-main-body's gutter so thread rows can span full width (their own padding);
     forum_head sits outside the scroll area so it stays put while the list scrolls. *)
  let main = (Html.template "%s<div class='cs-main-body cs-flush'>%s%s</div>"
  [ forum_head
  ; posts_html
  ; pager ]) in

  (* Right rail — real section metadata only. Rows whose data we don't have (thread count / last
     activity) are simply omitted; nothing here is fabricated. *)
  let cap s = if s = "" then s else String.make 1 (Char.uppercase_ascii s.[0]) ^ String.sub s 1 (String.length s - 1) in
  let threads_row = match thread_count with
    | Some n -> (Html.template "<tr><td>Threads</td><td class='num'>%s</td></tr>"
  [ Html.int (n) ])
    | None -> Html.empty
  in
  let activity_row = match last_activity with
    | Some ts when String.trim ts <> "" -> (Html.template "<tr><td>Last activity</td><td class='num'>%s</td></tr>"
  [ (Html.text ((Components.time_ago ts))) ])
    | _ -> Html.empty
  in
  let stats_cta =
    let b = new_thread_btn ~cls:"btn sm primary block" () in
    if Html.is_empty b then Html.empty
    else Html.template "<div class='ca-cta'>%s</div>" [ b ]
  in
  let stats_block = (Html.template "
    <div class='ca-block'>
        <div class='ca-label'>Section · indexed</div>
        <table class='ca-grid'><tbody>%s<tr><td>Default sort</td><td class='num'>%s</td></tr>%s</tbody></table>
        %s
    </div>"
  [ threads_row
  ; (Html.text ((cap section.default_sort)))
  ; activity_row
  ; stats_cta ])
  in
  (* community.rules are community-wide, NOT per-section — label them honestly. Sections have no
     rules field of their own yet, so we never imply they do; omitted entirely when unset. *)
  let rules_block = match community.rules with
    | Some r when String.trim r <> "" ->
        (Html.template "<div class='ca-block'><div class='ca-label'>Community rules</div><div class='ca-rules'>%s</div></div>"
  [ (Html.text (r)) ])
    | _ -> Html.empty
  in
  let mods_block =
    if mod_usernames = [] then Html.empty
    else
      let rows = Html.concat (List.map (fun u ->
        (Html.template "<div class='member'><a href='/u/%s'>%s</a><span class='role mod'>MOD</span></div>"
  [ (Html.text (u))
  ; (Html.text (u)) ])) mod_usernames)
      in
      (Html.template "<div class='ca-block'><div class='ca-label'>Moderators</div>%s</div>"
  [ rows ])
  in
  let right_pane = stats_block ++ rules_block ++ mods_block in

  let title = Printf.sprintf "%s · %s" section.name community.name in
  (* The complete <main> element, built here so the launch wrapper can never interpose a
     box: .cs-main is a flex column whose forum head and scrolling body MUST stay direct
     children, and for a private community the ph-no-capture replay guard rides on this
     element itself (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    (Html.template "<main class='%s'>%s</main>"
  [ (if community.visibility = Community_types.Community_private then (Html.static "cs-main ph-no-capture") else (Html.static "cs-main"))
  ; main ])
  in
  let aside = (Html.template "<aside class='aside'>%s</aside>"
  [ right_pane ]) in
  Community_shell.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~aside ~community ~sidebar ~page_class:"launch-community-section" ~title ~main_el ()

(* /c/:slug — the public community home / overview / entry page of a FLAT
   community (structured ones render community_overview_page). Markup scoped
   under .community-home; every datum here is real — no fake member/online/message counts and
   no created_at, because Community_types.community carries neither. SSR-only: every link/form works with JS
   off. Public presentation only — management (downvotes, mods, sections, bans) lives in
   /c/:slug/settings; the only management affordance here is the gated "Edit community" link. *)
(* [connected_projects_count]/[connected_communities_count]: how many records the two public
   read models returned for this community, counted by the same /c/:slug route after its
   existing authorization from exactly the sets /c/:slug/network renders. Counts, not
   fragments — this page names the two destinations and their sizes; the lists live there. *)
(* /c/:slug (structured) — the community overview, now on the Cartographic
   Civic launch chrome (Components.launch_community_page: earde.css only, no
   no legacy per-page CSS, no Tailwind). Every form, destination, and
   permission gate is the pre-launch contract reskinned: the /join and /leave
   POSTs keep their exact fields, the settings and review links keep their
   existing mod/top-mod gates, and the pre-rendered ccp-* connected-projects
   fragment is spliced in verbatim (its markup is pinned by the fragment
   suites) and restyled purely by the route-scoped CSS.
   The main column is what happens inside the community: community spaces
   (live channels beside forum sections), then recent knowledge. The
   community's external network is not activity, so the full connection lists
   moved to /c/:slug/network and only a compact entry point with their two
   counts remains, at the foot of the subordinate column. That grouping is
   built here, in the composition, so the CSS never has to reorder or fill
   cards by position. *)
let community_overview_page ?user ?(noindex=false) ?(connected_projects_count=0) ?(connected_communities_count=0) ~is_member ~is_current_user_mod ~is_current_user_top_mod
    ~mod_usernames ~orphaned ~(rail_communities : Community_types.community list)
    ~(channels : Channel_store.channel list) ~(recent_posts : Post_types.feed_item list)
    (community : Community_types.community) (section_stats : (Section_store.community_section * int * string option) list) request =
  let csrf_token = Csrf_field.tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let (orphaned_count, _) = orphaned in
  let slug = (Html.text (community.slug)) in

  (* Real stats only. sections/threads/channels are derivable from data already loaded; the
     prototype's "members"/"since" have no backing column/query, so they are omitted. *)
  let section_count = List.length section_stats + (if orphaned_count > 0 then 1 else 0) in
  let thread_count =
    List.fold_left (fun acc (_, c, _) -> acc + c) 0 section_stats + orphaned_count in
  let channel_count = List.length channels in
  let plural n = if n = 1 then "" else "s" in

  (* Community face: the avatar image when one is set and passes the gate
     (Html.image_src accepts the local /static/uploads/ path), else the launch
     rail's capitalized 2-letter slug glyph on the deterministic palette
     tone — the same fallback every launch tile uses, so the crest, sidebar
     head, and rail tile all agree. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let tile_color = Page_shell.launch_tile_color community.slug in
  let face size_cls =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None ->
             (Html.template "<span class='avatar %s' style='background:%s'>%s</span>"
  [ size_cls
  ; (Html.text tile_color)
  ; (Html.text (tile_glyph)) ])
         | Some src ->
             (Html.template "<span class='avatar %s'><img class='launch-avatar-img' src='%s' alt=''></span>"
  [ size_cls
  ; src ]))
    | _ ->
        (Html.template "<span class='avatar %s' style='background:%s'>%s</span>"
  [ size_cls
  ; (Html.text tile_color)
  ; (Html.text (tile_glyph)) ])
  in

  (* Primary CTA target = the community's General section feed (every community has one after
     the default-structure merge). Prefer the canonical "general" slug, fall back to the first
     section — so a renamed/reordered General still resolves and the link is never dead. *)
  let target_section =
    match List.find_opt (fun ((s : Section_store.community_section), _, _) -> s.slug = "general") section_stats with
    | Some s -> Some s
    | None -> (match section_stats with s :: _ -> Some s | [] -> None)
  in

  (* The primary CTA opens chat. Prefer the canonical "general" channel, fall back to the
     first channel (mirrors target_section) so the link is never dead; if a community somehow
     has no channels we fall back to the section feed below. *)
  let target_channel =
    match List.find_opt (fun (c : Channel_store.channel) -> c.slug = "general") channels with
    | Some c -> Some c
    | None -> (match channels with c :: _ -> Some c | [] -> None)
  in

  (* Three-state CTA, all wired to existing routes — no chat route invented:
     - anon: send to the existing /login entry point.
     - logged-in non-member: POST the existing /join route with redirect_to the chat/feed.
     - member: a plain link straight into chat. Fields and semantics are the
       pre-launch contract byte-for-byte; only the button classes changed. *)
  let primary_cta =
    match user with
    | None ->
        (Html.static "<a class='btn btn--primary' href='/login'>Log in to join &rarr;</a>")
    | Some _ ->
        (match target_channel with
        | Some (c : Channel_store.channel) ->
            let chat_url = Printf.sprintf "/c/%s/ch/%s" (community.slug) (c.slug) in
            (* Private community: anyone seeing this overview is authorized to read it, so link
               straight in — never show a self-join button. *)
            if is_member || community.visibility = Community_types.Community_private then
              (Html.template "<a class='btn btn--primary' href='%s'>Open #%s &rarr;</a>"
  [ (Html.text chat_url)
  ; (Html.text (c.slug)) ])
            else
              (Html.template "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%s'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn btn--primary btn--block'>Join &amp; open #%s &rarr;</button></form>"
  [ csrf_token
  ; Html.int (community.id)
  ; (Html.text chat_url)
  ; (Html.text (c.slug)) ])
        | None ->
            (* No channels (shouldn't happen post default-structure) — fall back to a section feed. *)
            (match target_section with
             | Some ((s : Section_store.community_section), _, _) ->
                 let feed_url = Printf.sprintf "/c/%s/s/%s" (community.slug) (s.slug) in
                 if is_member || community.visibility = Community_types.Community_private then
                   (Html.template "<a class='btn btn--primary' href='%s'>Open %s &rarr;</a>"
  [ (Html.text feed_url)
  ; (Html.text (s.name)) ])
                 else
                   (Html.template "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%s'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn btn--primary btn--block'>Join &amp; open %s &rarr;</button></form>"
  [ csrf_token
  ; Html.int (community.id)
  ; (Html.text feed_url)
  ; (Html.text (s.name)) ])
             | None when orphaned_count > 0 ->
                 (Html.template "<a class='btn btn--primary' href='/c/%s/s/uncategorized'>Browse threads &rarr;</a>"
  [ slug ])
             | None -> Html.empty))
  in

  (* The primary CTA already handles joining for non-members, so the secondary slot only needs
     the Leave action for existing members. Same POST /leave contract as before. *)
  let membership_btn =
    match user with
    | Some _ when is_member ->
        (Html.template "<form action='/leave' method='POST'>%s<input type='hidden' name='community_id' value='%s'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--secondary btn--block launch-leave'>Leave</button></form>"
  [ csrf_token
  ; Html.int (community.id)
  ; slug ])
    | _ -> Html.empty
  in

  let settings_btn =
    if is_current_user_mod || is_admin then
      (Html.template "<a class='btn btn--secondary' href='/c/%s/settings'>&#9881; Settings</a>"
  [ slug ])
    else Html.empty
  in

  (* --- community sidebar: real navigation only. Channels and sections keep
     their real destinations (those pages stay legacy in this pass);
     Moderation log is intentionally public; Settings and Home requests
     render only under the exact gates their handlers enforce. *)
  let side_head =
    (Html.template "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
  [ slug
  ; (face (Html.static "avatar--32"))
  ; (Html.text (community.name))
  ; slug ])
  in
  let nav_overview =
    (Html.template "<a class='navitem navitem--pad navitem--active' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
  [ slug ])
  in
  (* Visibility state, stated factually where the viewer already is. Public
     is the default and carries no marker. *)
  let vis_note =
    if community.visibility = Community_types.Community_private then
      (Html.static "<div class='launch-side-vis'>private community</div>")
    else Html.empty
  in
  let nav_live =
    if channels = [] then Html.empty
    else
      (Html.static "<p class='kicker sidebar__group'>Live</p>")
      ++ Html.concat (List.map (fun (c : Channel_store.channel) ->
          (Html.template "<a class='navitem' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
  [ slug
  ; (Html.text (c.slug))
  ; (Html.text (c.slug)) ]))
          channels)
  in
  let nav_sections_items =
    List.map (fun ((s : Section_store.community_section), post_count, _) ->
        (Html.template "<a class='navitem' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s<span class='navitem__trail'>%s</span></a>"
  [ slug
  ; (Html.text (s.slug))
  ; (Html.text (s.name))
  ; Html.int (post_count) ]))
      section_stats
    @ (if orphaned_count > 0 then
         [ (Html.template "<a class='navitem' href='/c/%s/s/uncategorized'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>Uncategorized<span class='navitem__trail'>%s</span></a>"
  [ slug
  ; Html.int (orphaned_count) ]) ]
       else [])
  in
  let nav_knowledge =
    if nav_sections_items = [] then Html.empty
    else (Html.static "<p class='kicker sidebar__group'>Knowledge</p>") ++ Html.concat nav_sections_items
  in
  let nav_network =
    (Html.static "<p class='kicker sidebar__group'>Network</p>")
    ++ (Html.template "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
  [ slug ])
    ++ (if is_current_user_top_mod || is_admin then
         (* Same top_mod-or-admin gate the review-queue handler and the
            settings nav apply to /c/:slug/project-home-requests. *)
         (Html.template "<a class='navitem navitem--pad' href='/c/%s/project-home-requests'><span class='navitem__sigil navitem__sigil--box navitem__sigil--project'>&#9672;</span>Home requests</a>"
  [ slug ])
       else Html.empty)
    ++ (if is_current_user_top_mod || is_admin then
         (* Same top_mod-or-admin gate the connections read model applies in
            SQL to /c/:slug/settings/connections. The link grants nothing —
            that surface reauthorizes from scratch. *)
         (Html.template "<a class='navitem navitem--pad' href='/c/%s/settings/connections'><span class='navitem__sigil navitem__sigil--box'>&#8644;</span>Connections</a>"
  [ slug ])
       else Html.empty)
    ++ (if is_current_user_mod || is_admin then
         (Html.template "<a class='navitem navitem--pad' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
  [ slug ])
       else Html.empty)
  in
  let sidebar =
    (Html.template "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
  [ (Html.text (community.name))
  ; side_head
  ; nav_overview
  ; vis_note
  ; nav_live
  ; nav_knowledge
  ; nav_network ])
  in

  (* --- header band. Badges are facts the record already states: the network
     marker and, for a private community its viewers are inside of, the
     restrained visibility badge. Public is unmarked. *)
  let badges =
    (if community.is_network_community then
       (Html.static " <span class='badge badge--network badge--lg'>&#9672; Network community</span>")
     else Html.empty)
    ++ (if community.visibility = Community_types.Community_private then
         (Html.static " <span class='badge badge--plain badge--lg'>Private</span>")
       else Html.empty)
  in
  let chead =
    (Html.template "<div class='chead'><div class='chead__row'>%s<div class='launch-chead-id'>\
       <div class='titleline'><h1 class='chead__title'>%s</h1><span class='chead__slug'>/c/%s</span>%s</div>\
       <p class='chead__desc'>%s</p>\
       <p class='chead__stats'>%s section%s &middot; %s thread%s &middot; %s channel%s</p>\
       </div><div class='chead__actions'>%s%s%s</div></div></div>"
  [ (face (Html.static "avatar--62"))
  ; (Html.text (community.name))
  ; slug
  ; badges
  ; (Html.text ((Option.value ~default:"No description." community.description)))
  ; Html.int (section_count)
  ; (Html.text (plural section_count))
  ; Html.int (thread_count)
  ; (Html.text (plural thread_count))
  ; Html.int (channel_count)
  ; (Html.text (plural channel_count))
  ; primary_cta
  ; membership_btn
  ; settings_btn ])
  in

  (* --- forum sections: flat panel rows over real per-section counts. --- *)
  let render_section ((s : Section_store.community_section), post_count, _last_activity) =
    (Html.template "<a class='project-row launch-secrow' href='/c/%s/s/%s'><span class='launch-sec-sigil'>&sect;</span><span class='launch-sec-main'><span class='launch-sec-name'>%s</span>%s</span><span class='launch-sec-count'>%s</span></a>"
  [ slug
  ; (Html.text (s.slug))
  ; (Html.text (s.name))
  ; (match s.description with
       | Some d when d <> "" -> (Html.template "<span class='launch-sec-desc'>%s</span>"
  [ (Html.text (d)) ])
       | _ -> Html.empty)
  ; Html.int (post_count) ])
  in
  let uncategorized_row =
    if orphaned_count > 0 then
      (Html.template "<a class='project-row launch-secrow' href='/c/%s/s/uncategorized'><span class='launch-sec-sigil'>&sect;</span><span class='launch-sec-main'><span class='launch-sec-name'>Uncategorized</span><span class='launch-sec-desc'>Posts from deleted sections</span></span><span class='launch-sec-count'>%s</span></a>"
  [ slug
  ; Html.int (orphaned_count) ])
    else Html.empty
  in
  let sections_inner = Html.concat (List.map render_section section_stats) ++ uncategorized_row in
  let sections_panel =
    (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Forum sections</span><span class='count-pill'>%s</span></div>%s</section>"
  [ Html.int (section_count)
  ; (if sections_inner = Html.empty then (Html.static "<div class='empty--inline'>No sections yet.</div>") else sections_inner) ])
  in

  (* --- recent durable knowledge: real newest rows from the same
     destination-aware community feed query the durable-thread surfaces use
     (no second Recent-only placement query), combined ordering and the
     five-item limit applied by that query. Own rows keep the /p/:id link
     byte-for-byte; a shared row links into THIS community's thread context,
     shows the placement's destination section, and carries the compact
     "Shared from" provenance. --- *)
  let recent_panel =
    if recent_posts = [] then Html.empty
    else
      let rows = Html.concat (List.map (fun (item : Post_types.feed_item) ->
        let p = item.fi_post in
        let href = match item.fi_shared with
          | Some _ -> Post_cards.canonical_thread_path community.slug p.id p.title
          | None -> Printf.sprintf "/p/%d" p.id in
        let section_name = match item.fi_shared with
          | Some ctx -> ctx.fs_section_name
          | None -> p.section_name in
        let shared_note = match item.fi_shared with
          | Some ctx ->
              (Html.template "<span class='sth-shared-from'>&#8644; Shared from %s</span>"
  [ (Html.text (ctx.fs_origin_name)) ])
          | None -> Html.empty in
        (Html.template "<a class='project-row launch-postrow' href='%s'>%s<span class='launch-post-title'>%s</span><span class='launch-post-foot'>&#9650; %s &middot; by %s &middot; %s &middot; &#128172; %s%s</span></a>"
  [ (Html.text (href))
  ; (match section_name with
           | Some n when n <> "" -> (Html.template "<span class='launch-post-meta'><span class='launch-post-sec'>&sect; %s</span></span>"
  [ (Html.text (n)) ])
           | _ -> Html.empty)
  ; (Html.text (p.title))
  ; Html.int (p.score)
  ; (Html.text (p.username))
  ; (Html.text (Components.time_ago p.created_at))
  ; Html.int (p.comment_count)
  ; (if shared_note = Html.empty then Html.empty else (Html.static " &middot; ") ++ shared_note) ]))
        recent_posts) in
      (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Recent durable knowledge</span></div>%s</section>"
  [ rows ])
  in

  (* --- live channels: real data only, each row links into the SSR chat channel. No fake
     online/talking counts. --- *)
  let channels_panel =
    if channels = [] then Html.empty
    else
      let rows = Html.concat (List.map (fun (c : Channel_store.channel) ->
        (Html.template "<a class='project-row launch-chanrow' href='/c/%s/ch/%s'><span class='launch-chan-line'><span class='launch-chan-hash'>#</span> %s</span>%s</a>"
  [ slug
  ; (Html.text (c.slug))
  ; (Html.text (c.slug))
  ; (match c.topic with
           | Some t when t <> "" -> (Html.template "<span class='launch-chan-topic'>%s</span>"
  [ (Html.text (t)) ])
           | _ -> Html.empty) ]))
        channels) in
      (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Live channels</span></div>%s</section>"
  [ rows ])
  in

  (* --- moderators. The page knows usernames only (no per-mod roles here), so
     rows carry no role badges — nothing is invented. Moderator management
     stays behind the gated Settings surface. *)
  let mods_inner =
    if mod_usernames = [] then (Html.static "<div class='empty--inline'>No moderators yet.</div>")
    else
      (Html.template "<div class='launch-mods'>%s</div>"
  [ (Html.concat (List.map (fun u ->
          let initial =
            if String.length u > 0 then (Html.text ((String.sub (String.uppercase_ascii u) 0 1))) else (Html.static "?") in
          (Html.template "<a class='launch-modrow' href='/u/%s'><span class='avatar avatar--22 avatar--mod'>%s</span><span class='launch-mod-name'>u/%s</span></a>"
  [ (Html.text (u))
  ; initial
  ; (Html.text (u)) ])) mod_usernames)) ])
  in
  let mods_panel =
    (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Moderators</span></div>%s</section>"
  [ mods_inner ])
  in

  let rules_panel = match community.rules with
    | Some r when r <> "" ->
        (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Rules</span></div><div class='panel__body launch-rules'>%s</div></section>"
  [ (Html.text (r)) ])
    | _ -> Html.empty
  in

  (* Public moderation log card. /c/:slug/modlog is intentionally public (no permission check in
     modlog_handler) so the community's moderation history is transparent; this is a plain link —
     no embedded log, no new query. *)
  let modlog_panel =
    (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Moderation log</span></div><div class='panel__body'><p class='launch-modlog-desc'>A public record of moderation actions in this community, kept open for transparency.</p><a class='btn--link mono launch-modlog-link' href='/c/%s/modlog'>View moderation log &rarr;</a></div></section>"
  [ slug ])
  in

  (* Optional banner — rendered only when a safe banner_url is present; absent => no block at
     all, so banner-less communities read exactly like the reference (no gradient placeholder). *)
  let banner_html =
    match community.banner_url with
    | Some url when String.trim url <> "" ->
        (match Html.image_src_opt (url) with
         | None -> Html.empty
         | Some src -> (Html.template "<div class='launch-banner'><img src='%s' class='launch-banner-img' alt=''></div>"
  [ src ]))
    | _ -> Html.empty
  in

  (* Compact network entry point. Two stable navigation rows — a zero count is
     rendered, never hidden, because the destination exists either way and a
     row that disappears at zero would make the panel's shape depend on data.
     Each row is one anchor, so the whole row is the link with no JavaScript
     and no nested interactive elements; the arrow is decoration and is hidden
     from assistive technology. Both rows lead to the same public page, at the
     block they name. *)
  let network_row ~anchor ~label ~count =
    (Html.template "<a class='project-row launch-netrow' href='/c/%s/network#%s'><span class='launch-net-label'>%s</span><span class='launch-net-count'>%s</span><span class='launch-net-go' aria-hidden='true'>&rarr;</span></a>"
  [ slug
  ; anchor
  ; label
  ; Html.int (count) ])
  in
  (* Same top-mod-or-admin gate the sidebar applies to this exact destination,
     and the same non-authorization: the connections surface re-decides in SQL.
     Management itself — requests, notes, review, removal — stays there; this
     is only a way in. *)
  let network_cta =
    if is_current_user_top_mod || is_admin then
      (Html.template "<div class='panel__body launch-net-cta'><a class='btn--link mono' href='/c/%s/settings/connections/new'>Connect a community &rarr;</a></div>"
  [ slug ])
    else Html.empty
  in
  let network_panel =
    (Html.template "<section class='panel'><div class='section-head'><span class='kicker'>Network</span></div>%s%s%s</section>"
  [ (network_row ~anchor:(Html.static "projects") ~label:(Html.static "Connected projects")
         ~count:connected_projects_count)
  ; (network_row ~anchor:(Html.static "communities") ~label:(Html.static "Connected communities")
         ~count:connected_communities_count)
  ; network_cta ])
  in

  (* Group sibling panels under one wrapper. Absent panels never leave a frame
     behind: no surface at all means no container, and a lone surface stands on
     its own rather than inside a one-child group — which is also why the CSS
     needs no count-dependent rule to fill a half-empty track. *)
  let group cls panels =
    match List.filter (fun p -> not (Html.is_empty p)) panels with
    | [] -> Html.empty
    | [ only ] -> only
    | present -> Html.template "<div class='%s'>%s</div>" [ cls; Html.concat present ]
  in
  (* Community spaces: live chat and durable forum structure are the two ways to
     take part here, so they are one composition of equal siblings — channels
     left, sections right — rather than two panels that happened to land in
     different columns. *)
  let spaces_block = group (Html.static "launch-spaces") [ channels_panel; sections_panel ] in

  (* The overview's right column is page content inside the main scroller
     (two-col), not the shell's `.aside` pane — exactly the reference's
     anatomy. It holds what is subordinate to activity inside the community:
     its own rules, then governance (moderators + the public moderation log),
     and last the way out to its external network. *)
  let content =
    (Html.template "<div class='scroll'>%s%s<div class='container launch-overview-body'><div class='two-col'><div class='stack'>%s%s</div><div class='stack stack--sm'>%s%s%s%s</div></div></div></div>"
  [ banner_html
  ; chead
  ; spaces_block
  ; recent_panel
  ; rules_panel
  ; mods_panel
  ; modlog_panel
  ; network_panel ])
  in
  Community_shell.launch_community_page ?user ~noindex ~request ~rail_communities
    ~community ~sidebar ~page_class:"launch-community-overview" ~title:community.name ~content ()
