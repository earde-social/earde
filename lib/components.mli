(** Reusable HTML primitives. The [launch_*] functions are the complete
    Cartographic Civic page documents every live route renders through;
    [render_post] and [render_forum_row] are the components with non-trivial
    state (CSRF tokens, session reads). All functions return raw HTML strings —
    no virtual DOM, no diffing overhead. *)

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

(** The one topbar Connect-a-project CTA every launch topbar renders (visible
    label "Connect a project", href /bring), byte-identical for anonymous and
    authenticated viewers. Exposed so page modules never grow a private copy. *)
val launch_connect_cta : string

(** Entry-chrome topbar policy for [launch_entry_page]:
    [Entry_connect_cta] (default) keeps the viewer-independent chrome — the
    shared Connect CTA alone (/privacy). [Entry_viewer user] renders viewer
    auth controls and suppresses the CTA — /bring only, where the compact CTA
    would self-link. Both arms stay form-free. *)
type entry_topbar =
  | Entry_connect_cta
  | Entry_viewer of string option

(** Cartographic Civic launch entry document (pass 1: /bring only). A complete,
    self-contained document loading only /static/css/earde.css — no Tailwind, no
    external fonts, no legacy per-page CSS — with a form-free
    top bar (command field is a link to /search; contents per [topbar]) and icon
    rail over a centred
    onboarding column. Emits no forms and only real routes, so pages whose tests
    assert zero forms document-wide can adopt it. [request] feeds the shared
    analytics assets (the same [analytics_assets] every launch document
    emits) and, under [Entry_viewer], the server-rendered notification badge.
    [page_class] is the route-specific scoping root stamped on <body>
    (e.g. "launch-bring") that the integration CSS at the end of earde.css
    keys on. *)
val launch_entry_page : ?noindex:bool -> ?request:Dream.request -> ?topbar:entry_topbar -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch auth document (pass 2: /login and /signup only).
    A complete, self-contained document loading only /static/css/earde.css — no
    Tailwind, no external fonts, no legacy per-page CSS, no mobile gate, no
    notification polling — under the approved deterministic *anonymous* top bar (the
    shared Connect CTA / Log in / Sign up, command field as a link to /search) and no
    rail/sidebar/aside. [request] feeds only the shared analytics assets
    (identical to every other launch document's). [page_class]
    ("launch-login" / "launch-signup")
    is stamped on <body> next to the shared "launch-auth" scope root the
    integration CSS at the end of earde.css keys on. Used only by
    [Pages.login_form] and [Pages.signup_form]; no existing wrapper changes. *)
val launch_auth_page : ?noindex:bool -> ?request:Dream.request -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch message document (pass 17: the shared
    [Pages.msg_page] only). A complete, self-contained document loading only
    /static/css/earde.css — no Tailwind, no external fonts, no legacy
    per-page CSS, no mobile gate, no behavior script, no notification wiring
    — and no chrome
    at all beyond the paper shell: no top bar, no rail, no sidebar, no
    footer, no forms. Strictly viewer- and resource-independent (the only
    per-render variation is [title] and [content]) because its ~400 handler
    call sites include byte-identity anti-enumeration pins. The single body
    class is the neutral "launch-message-page". [request] feeds only the
    shared analytics assets (identical to every other launch document's). *)
val launch_message_page : ?noindex:bool -> ?request:Dream.request -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch app document (pass 3: /feed only). A complete,
    self-contained document loading only /static/css/earde.css plus the shared
    desktop-only mobile gate — no Tailwind, no external fonts, no legacy
    per-page CSS — under the approved app chrome: 54px top bar (brand → /feed, a real /search
    form, viewer-state actions: the shared Connect-a-project CTA plus anonymous
    Log in/Sign up, or the member
    cluster with the same CTA, the id='notif-badge' bell, and a pure-CSS user menu whose
    logout stays a POST form), the dark 64px icon rail (Feed active, one tile
    per real joined community targeting the legacy /c/:slug/ch/general
    destination, ＋ → /bring), the central main column and an optional right
    [aside]. Member documents carry the shared launch behavior script
    (optimistic voting, confirm modal, ONE-SHOT notification fetch); anonymous
    documents carry no script. [request] feeds the shared analytics assets and
    the admin session flag. [page_class] (e.g. "launch-feed") is the scoping
    root stamped on <body> for the integration CSS at the end of earde.css.
    [analytics_community] threads the (id, authoritative visibility) pair to
    the shared analytics assets — the community group attribute and the
    private no-capture marker — for global launch
    surfaces whose content is community-bound (the post-creation form and its
    join gate); absent, the assets are byte-identical to before.
    Used only by [Pages.feed_page]; no existing wrapper changes. *)
val launch_app_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Db.community list -> ?analytics_community:(int * Db.community_visibility) -> ?aside:string -> page_class:string -> title:string -> content:string -> unit -> string

(** Cartographic Civic launch onboarding document (pass 4: /projects/new only).
    A complete, self-contained document loading only /static/css/earde.css plus
    the shared desktop-only mobile gate — no Tailwind, no external fonts, no
    legacy per-page CSS — under the launch app chrome: the 54px top bar
    (brand → /feed, command field as a styled link to /search, viewer-state
    actions: anonymous Bring/Log in/Sign up, or the member GitHub-mark Connect, the
    id='notif-badge' bell and the user chip as a plain /u/:name link), the dark
    64px icon rail (Feed, ＋ → /bring; no community tiles — the onboarding
    renderers receive no membership data), and a centred onboarding column.
    [stepper] is caller-supplied markup rendered inside the column BEFORE the
    [.create-shell] wrapper; [content] is wrapped in a
    <div class='create-shell'>…</div>, so the feature fragment the test suites
    slice (create-shell → </main>) stays byte-exact.
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
    Tailwind, no external fonts, no legacy per-page CSS — with
    the approved four-pane community shell: the 54px top bar (brand → /feed,
    the real /search form, the same viewer-state clusters as
    [launch_app_page] including the member logout POST form and the
    id='notif-badge' bell), the dark icon rail (Feed, then [rail_communities]
    — the viewer's joined communities in their established order, with the
    current community's tile carrying the white active marker and appended
    only when not already joined; nothing is invented — then ＋ → /bring),
    the caller-rendered community [sidebar], and the central overview
    [content]. Analytics
    assets carry the community group attribute (and private marker) derived
    from [community] itself; for a private community the ph-no-capture
    replay guard rides on the existing `.shell` element (no wrapper div).
    Member documents carry the shared launch behavior script; anonymous
    documents carry no script by default. [head_extra] (default absent — all
    pre-15A callers' output is byte-identical) appends a per-page <head>
    fragment after the analytics assets; the flat community home uses it to
    ship [launch_share_script] to guests. [page_class]
    ("launch-community-overview") is the scoping root stamped on <body> for
    the integration CSS at the end of earde.css. *)
val launch_community_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Db.community list -> ?head_extra:string -> community:Db.community -> sidebar:string -> page_class:string -> title:string -> content:string -> unit -> string

(** Guest-only public-interaction script: exactly the shared [copyPostLink]
    definition the authenticated behavior script also embeds (one source
    snippet, so the two can never diverge) and nothing else — no confirm
    modal, no vote handler, and no /api/unread-notifs fetch. For routes whose
    byte-pinned rows show the Share control to anonymous viewers (currently
    the flat community home). *)
val launch_share_script : string

(** Cartographic Civic launch community document, channel-safe variant
    (pass 9: GET /c/:slug/ch/:channel_slug only). Identical launch chrome to
    [launch_community_page] — same top bar, dark rail (current community
    active), analytics assets with the community group/private markers,
    shared behavior script for members, desktop-only mobile gate, and the
    ph-no-capture replay guard on `.shell` for private communities — but the
    caller supplies [main_el], the COMPLETE prebuilt `<main>` element. The
    live channel's `<main class='cs-main …'>` is a load-bearing flex column
    whose head / chat stage / typing row / composer must stay DIRECT
    children, so no wrapper of any kind is added around or inside it.
    [head_extra] appends per-page <head> tags after the analytics assets
    (the channel's existing phoenix.js + chat_live.js defer scripts and the
    reverse-navigation canonical link). [aside] renders after [main_el]
    inside `.shell` (the presence pane). [rail_communities] adds real
    joined-community tiles to the rail (the channel handler already loads
    them; the current community's tile carries the active marker in its
    joined slot, or is appended when not joined). Used only by
    [Pages.community_channel_shell_page]; no existing wrapper changes. *)
val launch_community_surface_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Db.community list -> ?head_extra:string -> ?aside:string -> community:Db.community -> sidebar:string -> page_class:string -> title:string -> main_el:string -> unit -> string

(** Deterministic launch-palette colour (hex string) for a community slug.
    The database stores no per-community colour, so launch chrome derives a
    stable presentational one from the slug alone (same slug → same colour). *)
val launch_tile_color : string -> string

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

(** Destination rendering context for an accepted shared-thread placement feed row:
    the destination community's slug (all internal links stay in the destination
    context) paired with the row's provenance/destination-section fields. Absent =
    the community's own post, byte-identical rendering. On a shared row the
    destination's moderator standing grants no canonical-content controls. *)
type feed_shared = string * Db.feed_shared_context

val render_post : ?is_current_user_mod:bool -> ?mod_usernames:string list -> ?admin_usernames:string list -> ?banned_usernames:string list -> ?shared:feed_shared -> Dream.request -> (int * int) list -> Db.post -> string
(** Thread-first section-feed row (cool-grey shell idiom). Same call shape as [render_post];
    used by [Pages.community_section_shell_page]. Preserves the optimistic-vote DOM contract. *)
val render_forum_row : ?is_current_user_mod:bool -> ?mod_usernames:string list -> ?admin_usernames:string list -> ?banned_usernames:string list -> ?show_context:bool -> ?shared:feed_shared -> Dream.request -> (int * int) list -> Db.post -> string

(** [slugify title] → a URL-safe, descriptive thread slug (lowercase, non-alphanumerics
    collapsed to single dashes, trimmed, length-capped). Descriptive only — [post_id] is
    authoritative in the canonical URL. *)
val slugify : string -> string
(** [canonical_thread_path community_slug post_id title] → the canonical thread path
    [/c/:community_slug/t/:post_id-:post_slug]. Slug omitted when empty. *)
val canonical_thread_path : string -> int -> string -> string

(** === REPLAY PRIVACY === *)

(** Replay privacy (analytics spec §6): wraps [body] in PostHog's built-in
    ph-no-capture block class when the community is private, excluding its
    content from session replay. Identity for public communities. *)
val private_replay_guard : community:Db.community -> string -> string
