(** Reusable HTML primitives. layout wraps every page; render_post and community_card
    are the only components with non-trivial state (CSRF tokens, session reads).
    All functions return raw HTML strings — no virtual DOM, no diffing overhead. *)

(** === ESCAPING === *)
val html_escape : string -> string
val safe_url : string -> string
(** [safe_internal_path p] gates a server-built rooted internal path (e.g. "/c/x/t/1"):
    passes a single-leading-slash path (html-escaped), rejects ""/"#"/protocol-relative
    "//host" (and the "/\\host" variant) → "#". Use for app nav targets, NOT [safe_url]
    (which only passes http(s) and would collapse every relative path to "#"). *)
val safe_internal_path : string -> string

(** === IMAGES ===
    [safe_img_src] is the escaping gate for image [src] attributes: it passes rooted local
    upload paths ([/static/uploads/...]) AND [http(s)://] URLs, collapses everything else
    ([javascript:]/[data:]/protocol-relative/empty) to ["#"], and always html-escapes the
    result. Do NOT use [safe_url] for image src — it rejects local upload paths. *)
val safe_img_src : string -> string

(** [initial_tile ?class_ name] → the shared letter-tile fallback ([<div>] with the name's
    uppercased first letter, ["?"] when empty). [class_] carries the surface's existing
    utility classes so each call site keeps its look. *)
val initial_tile : ?class_:string -> string -> string

(** [user_avatar ?alt ~img_class ~tile_class ~username avatar_url] → an [<img>] when the
    avatar URL is safe/non-empty, else an [initial_tile] for the username. *)
val user_avatar : ?alt:string -> img_class:string -> tile_class:string -> username:string -> string option -> string

(** [community_avatar ?alt ~img_class ~tile_class ~name avatar_url] → an [<img>] when the
    avatar URL is safe/non-empty, else an [initial_tile] for the community name. *)
val community_avatar : ?alt:string -> img_class:string -> tile_class:string -> name:string -> string option -> string

(** [community_banner ~wrap_class ~img_class ~fallback_class banner_url] → the banner [<img>]
    (wrapped) when safe/non-empty, else the [fallback_class] placeholder div (e.g. a gradient). *)
val community_banner : wrap_class:string -> img_class:string -> fallback_class:string -> string option -> string

(** [post_thumbnail ?alt ~img_class image_url] → a small post [<img>], or [""] when there is
    no image. Added for the later feed/search slice; not wired broadly yet. *)
val post_thumbnail : ?alt:string -> img_class:string -> string option -> string

(** === LAYOUT === *)
(** The mono App command bar shared by every shell/App surface. Pure renderer —
    [request] is unused and [is_admin] is passed in explicitly, so rendering tests
    can exercise the logged-in/anonymous navigation without a Dream server. *)
val render_app_topbar : ?user:string -> ?request:Dream.request -> is_admin:bool -> unit -> string

(** [head_extra] injects markup into <head> (e.g. a page-scoped stylesheet);
    [full_bleed] drops the capped/padded <main> so a page can own its full-width
    layout. [chrome] selects the page furniture: [`Site] (default) renders the warm
    Tailwind navbar + Privacy footer; [`App] swaps in the mono command bar and
    drops the footer, for the in-app shell; [`Auth] drops both (no navbar/command bar,
    no footer) for the focused auth layout. All default to the prior behavior, so
    existing callers are unaffected. *)
(* analytics_community renders the browser community-group attribute
   (analytics spec §5.3): exactly community:<database_id>, community-scoped
   pages only. The pair carries the authoritative Db visibility so the §13
   private-community marker (data-analytics-private-community='true', value
   only — never name/slug) cannot be forgotten or derived from URL shape;
   on marked documents the browser records consent but never initializes
   PostHog. The identity attribute (user:<id>) is derived from the request
   session automatically. *)
val layout : ?noindex:bool -> ?user:string -> ?request:Dream.request -> ?head_extra:string -> ?full_bleed:bool -> ?chrome:[ `Site | `App | `Auth ] -> ?analytics_community:(int * Db.community_visibility) -> title:string -> string -> string

(** Focused auth/account-lifecycle layout: a single centered card (auth.css) in the
    cool-grey shell idiom, with no rail/sidebar/command bar/footer. [card] is the inner
    card HTML (a form or a message panel); the brand mark and outer shell are supplied. *)
val auth_page : ?user:string -> ?noindex:bool -> ?request:Dream.request -> title:string -> card:string -> unit -> string

(** Cartographic Civic launch entry document (pass 1: /bring only). A complete,
    self-contained document loading only /static/css/earde.css — no Tailwind, no
    external fonts, no legacy per-page CSS — with a viewer-independent, form-free
    top bar (command field is a link to /search) and icon rail over a centred
    onboarding column. Emits no forms and only real routes, so pages whose tests
    assert zero forms document-wide can adopt it. [request] feeds only the shared
    analytics assets (identical to [layout]'s). [page_class] is the route-specific
    scoping root stamped on <body> (e.g. "launch-bring") that the integration CSS
    at the end of earde.css keys on. Not wired into [layout]; no existing wrapper
    changes. *)
val launch_entry_page : ?noindex:bool -> ?request:Dream.request -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch auth document (pass 2: /login and /signup only).
    A complete, self-contained document loading only /static/css/earde.css — no
    Tailwind, no external fonts, no auth.css, no mobile gate, no notification
    polling — under the approved deterministic *anonymous* top bar (Bring a
    project / Log in / Sign up, command field as a link to /search) and no
    rail/sidebar/aside. [request] feeds only the shared analytics assets
    (identical to [layout]'s). [page_class] ("launch-login" / "launch-signup")
    is stamped on <body> next to the shared "launch-auth" scope root the
    integration CSS at the end of earde.css keys on. Used only by
    [Pages.login_form] and [Pages.signup_form]; no existing wrapper changes. *)
val launch_auth_page : ?noindex:bool -> ?request:Dream.request -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch app document (pass 3: /feed only). A complete,
    self-contained document loading only /static/css/earde.css plus the shared
    desktop-only mobile gate — no Tailwind, no external fonts, no shell.css —
    under the approved app chrome: 54px top bar (brand → /feed, a real /search
    form, viewer-state actions: anonymous Bring/Log in/Sign up, or the member
    ＋ Connect, the id='notif-badge' bell, and a pure-CSS user menu whose
    logout stays a POST form), the dark 64px icon rail (Feed active, one tile
    per real joined community targeting the legacy /c/:slug/ch/general
    destination, ＋ → /bring), the central main column and an optional right
    [aside]. Member documents carry the exact [layout] inline behavior script
    (optimistic voting, confirm modal, ONE-SHOT notification fetch); anonymous
    documents carry no script. [request] feeds the shared analytics assets and
    the admin session flag. [page_class] (e.g. "launch-feed") is the scoping
    root stamped on <body> for the integration CSS at the end of earde.css.
    Used only by [Pages.feed_page]; no existing wrapper changes. *)
val launch_app_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Db.community list -> ?aside:string -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch onboarding document (pass 4: /projects/new only).
    A complete, self-contained document loading only /static/css/earde.css plus
    the shared desktop-only mobile gate — no Tailwind, no external fonts, no
    shell.css, no create.css — under the launch app chrome: the 54px top bar
    (brand → /feed, command field as a styled link to /search, viewer-state
    actions: anonymous Bring/Log in/Sign up, or the member ＋ Connect, the
    id='notif-badge' bell and the user chip as a plain /u/:name link), the dark
    64px icon rail (Feed, ＋ → /bring; no community tiles — the onboarding
    renderers receive no membership data), and a centred onboarding column.
    [stepper] is caller-supplied markup rendered inside the column BEFORE the
    [.create-shell] wrapper; [content] is wrapped in the identical
    <div class='create-shell'>…</div> that [create_page] emits, so the feature
    fragment the test suites slice (create-shell → </main>) stays byte-exact.
    Member documents carry the shared launch behavior script (bell badge);
    anonymous documents carry no script. [request] feeds only the shared
    analytics assets. [page_class] (e.g. "launch-project-new") is the scoping
    root stamped on <body> for the integration CSS at the end of earde.css.
    Used only by [Project_setup_pages.project_setup_page]; no existing wrapper
    changes. *)
val launch_onboarding_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?stepper:string -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch community document (pass 8: the structured
    /c/:slug overview only). A complete, self-contained document loading only
    /static/css/earde.css plus the shared desktop-only mobile gate — no
    Tailwind, no external fonts, no shell.css, no community-home.css — with
    the approved four-pane community shell: the 54px top bar (brand → /feed,
    the real /search form, the same viewer-state clusters as
    [launch_app_page] including the member logout POST form and the
    id='notif-badge' bell), the dark icon rail (Feed, the CURRENT community
    tile active with the white marker, ＋ → /bring; joined communities are
    not supplied to this renderer and are not invented), the caller-rendered
    community [sidebar], and the central overview [content]. Analytics
    assets carry the community group attribute (and private marker) derived
    from [community] itself; for a private community the ph-no-capture
    replay guard rides on the existing `.shell` element (no wrapper div).
    Member documents carry the shared launch behavior script; anonymous
    documents carry no script. [page_class] ("launch-community-overview") is
    the scoping root stamped on <body> for the integration CSS at the end of
    earde.css. Used only by [Pages.community_overview_page]; no existing
    wrapper changes. *)
val launch_community_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> community:Db.community -> sidebar:string -> page_class:string -> title:string -> content:string -> unit -> string

(** Deterministic launch-palette colour (hex string) for a community slug.
    The database stores no per-community colour, so launch chrome derives a
    stable presentational one from the slug alone (same slug → same colour). *)
val launch_tile_color : string -> string

(** Focused in-product creation layout: the mono app command bar over a single centered
    cool-grey panel (create.css), with no rail/sidebar. Used by the creation flows
    (new-community, new-post, choose-community, join-to-post). [body] is the inner page
    HTML; the outer .create-shell and topbar are supplied. *)
val create_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> ?analytics_community:(int * Db.community_visibility) -> title:string -> body:string -> unit -> string

(** Focused in-product account layout: the mono app command bar over a single centered
    cool-grey column (account.css), with no rail/sidebar/footer. Used by the personal
    account pages (profile, settings, notifications). [body] is the inner page HTML; the
    outer .account-shell and topbar are supplied. *)
val account_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> title:string -> body:string -> unit -> string

(** Focused in-product admin layout: the mono app command bar over a single centered
    cool-grey column (admin.css), with no rail/sidebar/footer. Used by the admin-only
    /admin dashboard. Adds no authorization — the handler gates it on is_admin. [body]
    is the inner page HTML; the outer .admin-shell and topbar are supplied. *)
val admin_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> title:string -> body:string -> unit -> string

(** Focused in-product community-management layout: the mono app command bar over a single
    centered cool-grey column (community-manage.css), with no rail/sidebar/footer. Shared by
    the per-community management surfaces (/c/:slug/settings, /c/:slug/manage-mods,
    /c/:slug/modlog) so they read as one operator console. Adds no authorization — the
    handlers gate authority. [body] is the inner page HTML; the outer .cm-shell and topbar
    are supplied. *)
val community_manage_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> ?analytics_community:(int * Db.community_visibility) -> title:string -> body:string -> unit -> string

(** The public community home (/c/:slug): App command bar (no warm navbar, no footer) over the
    page's own full-bleed .community-home structure. Loads shell.css (for .app-topbar) +
    community-home.css. Adds no authorization. [body] is the page's full markup, supplied verbatim
    (NOT wrapped in an extra shell div). *)
val community_home_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> ?analytics_community:(int * Db.community_visibility) -> title:string -> body:string -> unit -> string

(** Focused in-product search layout: App command bar (no warm navbar, no footer) over a single
    centered cool-grey column. Loads shell.css (for .app-topbar) + search.css; wraps [body] in a
    [.search-shell] div. Adds no authorization. *)
val search_page : ?user:string -> ?request:Dream.request -> ?noindex:bool -> title:string -> body:string -> unit -> string

(** === HELPERS === *)
val is_deleted_user : string -> bool
(** [extract_domain url] → bare host (no scheme/www) for a link post's domain chip, or [None]. *)
val extract_domain : string -> string option
val render_author : ?mod_usernames:string list -> ?admin_usernames:string list -> string -> string
val time_ago : string -> string
val format_month_year : string -> string

(** === CARDS === *)
(** [post_admin_actions ~csrf_token request post] → the post's mod/admin action markup
    (own-post Delete, Mod/Admin Remove dialog, Mod/Admin Ban dialog), or "" when the viewer has
    no controls. Shared by [render_post] and the search results page so both emit identical
    forms/routes/CSRF/dialogs/reason fields with the same Rule A/B/C visibility. *)
val post_admin_actions : ?is_current_user_mod:bool -> ?admin_usernames:string list -> ?banned_usernames:string list -> csrf_token:string -> Dream.request -> Db.post -> string
val render_post : ?is_current_user_mod:bool -> ?mod_usernames:string list -> ?admin_usernames:string list -> ?banned_usernames:string list -> Dream.request -> (int * int) list -> Db.post -> string
(** Thread-first section-feed row (cool-grey shell idiom). Same call shape as [render_post];
    used by [Pages.community_section_shell_page]. Preserves the optimistic-vote DOM contract. *)
val render_forum_row : ?is_current_user_mod:bool -> ?mod_usernames:string list -> ?admin_usernames:string list -> ?banned_usernames:string list -> ?show_context:bool -> Dream.request -> (int * int) list -> Db.post -> string
val community_card : Db.community -> string

(** [slugify title] → a URL-safe, descriptive thread slug (lowercase, non-alphanumerics
    collapsed to single dashes, trimmed, length-capped). Descriptive only — [post_id] is
    authoritative in the canonical URL. *)
val slugify : string -> string
(** [canonical_thread_path community_slug post_id title] → the canonical thread path
    [/c/:community_slug/t/:post_id-:post_slug]. Slug omitted when empty. *)
val canonical_thread_path : string -> int -> string -> string
val left_sidebar : ?user:string -> moderated_communities:Db.community list -> Db.community list -> string

(** === COMMUNITY SHELL === *)

(** A clickable entry in the community sidebar (a chat channel or forum section).
    Shell-local with prefixed fields to stay decoupled from Db.channel/community_section
    and to avoid record-field disambiguation. *)
type nav_item = {
  ni_label  : string;
  ni_href   : string;
  ni_sigil  : string;
  ni_active : bool;
  ni_badge  : string option;
}

(** A titled group of nav items, e.g. "Text Channels" / "Forum Sections". *)
type nav_group = {
  ng_label : string;
  ng_items : nav_item list;
}

(** Replay privacy (analytics spec §6): wraps [body] in PostHog's built-in
    ph-no-capture block class when the community is private, excluding its
    content from session replay. Identity for public communities. *)
val private_replay_guard : community:Db.community -> string -> string

(** The persistent in-app community shell (community rail · sidebar · main · optional
    right pane). Wraps [layout] internally with the shell stylesheet and full-bleed main.
    [main]/[right_pane] are caller-rendered HTML fragments. Private communities get
    PostHog's ph-no-capture class on the <main> element itself (never a wrapper div,
    which would break the .cs-main flex column that sizes the chat/thread panes). *)
val community_shell :
  ?user:string -> ?request:Dream.request -> ?noindex:bool ->
  ?rail_communities:Db.community list -> ?active_slug:string -> ?right_pane:string ->
  ?head_extra:string ->
  title:string -> community:Db.community -> nav_groups:nav_group list -> main:string -> unit -> string

(** The global Feed shell (rail · main · optional right pane). Unlike [community_shell] it has
    no community sidebar column — Feed lives outside any one community. Wraps [layout] with the
    shell stylesheet and full-bleed main. [main]/[right_pane] are caller-rendered HTML. *)
val feed_shell :
  ?user:string -> ?request:Dream.request -> ?noindex:bool ->
  ?rail_communities:Db.community list -> ?right_pane:string -> ?head_extra:string ->
  title:string -> main:string -> unit -> string
