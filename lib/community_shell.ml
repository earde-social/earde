open Html.Infix

(* Cartographic Civic launch community document (the structured
   /c/:slug overview and channel surfaces). Like the other launch documents, a complete
   self-contained page loading only earde.css plus the shared desktop-only
   mobile gate — no Tailwind, no external fonts, no legacy per-page CSS —
   but with the approved four-pane community shell: the
   54px top bar (brand → /feed, the REAL /search form, viewer-state actions —
   identical clusters to [launch_app_page], including the member ⋯ menu with
   the existing logout POST form and the id='notif-badge' bell), the dark
   icon rail (Feed, the CURRENT community tile active with the white bleeding
   marker, ＋ → /bring — this renderer receives no joined-community data and
   none is invented), the caller-rendered 244px community [sidebar], and the
   central overview [content]. The overview's right column is page content,
   not the `.aside` pane, so no aside parameter exists.

   Analytics assets carry the §5.3 community group attribute (and the §13
   private marker): the (id, visibility) pair comes from the [community]
   record itself, never from URL shape.
   Replay privacy: for a private community the ph-no-capture class rides on
   the existing `.shell` element (sidebar + main together) — never a new
   wrapper div, so the flex chain is untouched. Member documents carry the
   exact shared [launch_behavior_script] (its vote selectors match nothing
   here); anonymous documents carry no script at all. [page_class]
   ("launch-community-overview") is the scoping root stamped on <body> for
   the route's integration CSS at the end of earde.css. Shared doc builder
   behind [launch_community_page] (overview) and
   [launch_community_surface_page] (channel), so the two routes'
   global rails can never diverge. *)
let launch_community_doc ?(noindex = false) ?request ?user
    ?(rail_communities = []) ?(head_extra = Html.empty) ?(aside = Html.empty)
    ~(community : Community_types.community) ~sidebar ~page_class ~title
    ~main_el () =
  let analytics_head, analytics_banner =
    Page_shell.analytics_assets ?request
      ~analytics_community:(community.id, community.visibility)
      ()
  in
  let robots_meta =
    if noindex then Html.static "<meta name='robots' content='noindex'>"
    else Html.empty
  in
  let is_admin =
    match request with
    | Some req -> (
        try Dream.session_field req "is_admin" = Some "true" with _ -> false)
    | None -> false
  in
  let house_icon =
    Html.static
      "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
       stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
       stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
       7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>"
  in
  let bell_icon =
    Html.static
      "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
       stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
       stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 0-12 \
       0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>"
  in
  (* Same /search route + ?q= contract (and `required`) as the legacy search
     forms; no /c/:slug test counts page-wide forms, so the real command
     field is safe here (unlike the onboarding wrappers' link variant). *)
  let search_form =
    Html.static
      "<form class='topbar__search-cell' action='/search' method='GET' \
       role='search'><div class='search'><span class='search__sigil' \
       aria-hidden='true'>/</span><label class='sr-only' for='q'>Search \
       Earde</label><input class='search__input' id='q' type='text' name='q' \
       required placeholder='grep threads &middot; projects &middot; \
       communities&hellip;'><button class='search__enter' type='submit' \
       aria-label='Search'>&#8629;</button></div></form>"
  in
  let actions =
    match user with
    | Some username ->
        let u = Html.text username in
        let admin_item =
          if is_admin then Html.static "<a href='/admin'>Admin</a>"
          else Html.empty
        in
        let initial =
          if String.length username > 0 then
            Html.text (String.sub (String.uppercase_ascii username) 0 1)
          else Html.static "?"
        in
        (* Badge server-rendered from the request's unread count; absent at
           zero (see [Notification_badge]). *)
        Html.template
          "%s<a class='bell' href='/notifications' title='Notifications' \
           aria-label='Notifications'>%s%s</a><details \
           class='launch-user'><summary class='userchip'><span class='avatar \
           avatar--24'>%s</span><span \
           class='userchip__name'>u/%s</span></summary><div \
           class='launch-user__menu'><a href='/u/%s'>Profile</a><a \
           href='/settings'>Settings</a><a \
           href='/notifications'>Notifications</a>%s<form action='/logout' \
           method='POST'><button type='submit'>Log \
           out</button></form></div></details>"
          [
            Page_shell.launch_connect_cta;
            bell_icon;
            Notification_badge.badge_html ?request ();
            initial;
            u;
            u;
            admin_item;
          ]
    | None -> Page_shell.topbar_anon_actions
  in
  (* The rail shows only what this renderer really has: Feed, the joined
     communities the handler supplies (kept in their established order), the
     current community, and ＋. Nothing is fabricated: anonymous documents get
     no joined tiles, and the current community is appended only when it is
     not already in the joined run (dedup by slug) — so navigating between
     feed, overview and channel never removes, adds or reorders joined tiles;
     only the active marker moves. *)
  let tile_glyph slug =
    let raw =
      if String.length slug >= 2 then String.sub slug 0 2
      else if String.length slug = 1 then slug
      else "?"
    in
    String.capitalize_ascii raw
  in
  let tile_face_of (c : Community_types.community) =
    match c.avatar_url with
    | Some url -> (
        match Html.image_src_opt url with
        | Some src ->
            Html.template "<img class='launch-rail__img' src='%s' alt=''>"
              [ src ]
        | None -> Html.text (tile_glyph c.slug))
    | None -> Html.text (tile_glyph c.slug)
  in
  (* One tile per community in the run; the current community carries the
     white active marker wherever it sits (in its joined slot when the viewer
     belongs to it, appended at the end when directly visiting a community
     they haven't joined — an anonymous viewer's rail reduces to exactly the
     current tile). *)
  let rail_tile (c : Community_types.community) =
    let marker =
      if c.slug = community.slug then
        Html.static "<span class='rail__marker rail__marker--community'></span>"
      else Html.empty
    in
    Html.template
      "<a class='rail__item rail__item--community' href='/c/%s' title='/c/%s' \
       style='background:%s'>%s%s</a>"
      [
        Html.text c.slug;
        Html.text c.slug;
        Html.text (Page_shell.launch_tile_color c.slug);
        marker;
        tile_face_of c;
      ]
  in
  let rail_run =
    if
      List.exists
        (fun (c : Community_types.community) -> c.slug = community.slug)
        rail_communities
    then rail_communities
    else rail_communities @ [ community ]
  in
  let rail =
    Html.template
      "<nav class='rail' aria-label='Primary'><a class='rail__item' \
       href='/feed' title='Feed' aria-label='Feed'>%s</a><span \
       class='rail__divider'></span>%s<span class='rail__spacer'></span><a \
       class='rail__item rail__item--add' href='/bring' title='Connect a \
       project' aria-label='Connect a project'>&#65291;</a></nav>"
      [ house_icon; Html.concat (List.map rail_tile rail_run) ]
  in
  (* Replay privacy on the existing shell element — class only, no wrapper. *)
  let shell_cls =
    if Community_types.community_is_private community.visibility then
      "shell ph-no-capture"
    else "shell"
  in
  let behavior_script =
    match user with
    | Some _ -> Page_shell.launch_behavior_script
    | None -> Html.empty
  in
  Html.to_string
    (Html.template
       "<!DOCTYPE html>\n\
        <html lang='en'>\n\
        <head>\n\
        <meta charset='UTF-8'>\n\
        <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
        <title>%s - Earde</title>\n\
        %s\n\
        <link rel='stylesheet' href='/static/css/earde.css'>\n\
        %s\n\
        %s\n\
        </head>\n\
        <body class='%s'>\n\
        <div class='app'>\n\
        <header class='topbar'><a class='topbar__brand' href='/feed' \
        aria-label='Earde feed'><img class='topbar__mark' \
        src='/static/images/logo-mark.svg' alt=''><img \
        class='topbar__wordmark' src='/static/images/logo-wordmark.svg' \
        alt='Earde'></a>%s<div class='topbar__actions'>%s</div></header>\n\
        <div class='%s'>%s%s%s%s</div>\n\
        %s\n\
        </div>\n\
        %s\n\
        %s\n\
        %s\n\
        </body>\n\
        </html>"
       [
         Html.text title;
         robots_meta;
         Page_shell.mobile_gate_css_link;
         analytics_head ++ head_extra;
         Html.text page_class;
         search_form;
         actions;
         Html.text shell_cls;
         rail;
         sidebar;
         main_el;
         aside;
         Page_shell.launch_footer;
         Page_shell.mobile_desktop_gate;
         analytics_banner;
         behavior_script;
       ])

(* Public overview entry point: the overview content is wrapped in the exact
   `<main class='main'>` element (same newlines) the pre-extraction template
   emitted, with no aside. [rail_communities] carries the viewer's joined
   communities so the overview's global rail matches the feed and channel
   routes instead of collapsing to the current tile alone. [head_extra]
   (default absent — every pre-15A caller's output is byte-identical) lets a
   route add a small head fragment; the flat community home uses it to ship
   the guest-only [launch_share_script]. *)
let launch_community_page ?noindex ?request ?user ?rail_communities ?head_extra
    ~(community : Community_types.community) ~sidebar ~page_class ~title
    ~content () =
  launch_community_doc ?noindex ?request ?user ?rail_communities ?head_extra
    ~community ~sidebar ~page_class ~title
    ~main_el:
      (Html.static "<main class='main'>\n" ++ content ++ Html.static "\n</main>")
    ()

(* Pass-9 channel-safe variant: identical launch chrome (topbar, rail,
   analytics assets, behavior script, mobile gate, tile colours) but the
   caller supplies the COMPLETE prebuilt <main> element — the live channel's
   `<main class='cs-main …'>` flex column must keep its head / stage /
   typing / composer as direct children, which the overview's
   `<main class='main'>` + `.scroll` anatomy would break. [head_extra]
   carries the channel's existing realtime scripts (phoenix.js +
   chat_live.js) and canonical link; [aside] the presence pane;
   [rail_communities] the real joined communities its handler already
   loads. Used only by [Chat_pages.community_channel_shell_page]. *)
let launch_community_surface_page ?noindex ?request ?user ?rail_communities
    ?head_extra ?aside ~(community : Community_types.community) ~sidebar
    ~page_class ~title ~main_el () =
  launch_community_doc ?noindex ?request ?user ?rail_communities ?head_extra
    ?aside ~community ~sidebar ~page_class ~title ~main_el ()
