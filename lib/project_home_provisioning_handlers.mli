(** HTTP layer for the dedicated-community-home creation flow of one verified
    permanent project: GET [/projects/:slug/community-home/new] renders the
    entry page, and POST [/projects/:slug/community-home] — the exact action
    that page's single form emits — provisions the private community setup
    draft.

    Both handlers are factories over the closed onboarding mode (and, for the
    POST, a configuration loader) so tests can inject fixed values without
    touching the process environment, and a disabled or rejected request never
    reads the route parameter, loads configuration, parses a form, or runs SQL.
    Nothing here logs a query or form value, and no submitted value ever reaches
    a redirect, a query string, or a cookie: failed submissions re-render over
    POST rather than redirecting.

    Gate order, applied before any route-parameter read, configuration load,
    origin inspection, form parse, or SQL: onboarding mode [Off] answers a clean
    303 to [/bring]; an anonymous, malformed, or non-positive session user
    answers a clean 303 to [/login]; in [Admins] mode an authenticated non-admin
    answers a clean 303 to [/bring]. A session [is_admin] claim has no meaning
    without a valid positive user id. Every redirect has an empty body and
    carries [Cache-Control: no-store], [Pragma: no-cache], and
    [Referrer-Policy: no-referrer]; no arbitrary return URL is preserved.

    Past the gates the project is authorized entirely by
    {!Project_home_provisioning_read_model} for anything rendered, and entirely
    by {!Project_home_provisioning_store} for anything durable: neither the
    route shape nor a session flag ever authorizes. Nonexistent, foreign,
    unstewarded, stale, revoked, and already-homed projects — and a malformed
    route slug — collapse to one generic 404, so probing cannot become an
    ownership or state oracle. Every durable failure past that answers one
    generic non-cacheable 500 carrying no Caqti, PostgreSQL, or
    error-constructor detail. Rendered pages are [Cache-Control: no-store],
    [Referrer-Policy: no-referrer], and noindex, and carry no query or flash
    state. *)

val make_project_home_provisioning_page_handler :
  mode:Project_onboarding.mode -> Dream.handler
(** GET [/projects/:slug/community-home/new]. Only a steward of a verified
    project with no active home relation reaches the page; the form is prefilled
    from the read model's suggested identity and carries a fresh framework CSRF
    field. *)

val make_project_home_provisioning_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** POST [/projects/:slug/community-home], the page's own action.

    After the shared gates the sequence is fixed: read the route slug, load the
    injected configuration (solely to reuse the same public-origin policy as
    every other authenticated project-home mutation — no GitHub credential is
    used and no outbound HTTP occurs), check
    {!Request_origin.same_origin_request}, then parse the framework-verified
    form. A configuration failure is a generic non-cacheable 503 carrying no
    diagnostic; an origin rejection is a generic 403 that never reflects the
    supplied origin; a wrong content type or malformed framework form is a
    generic 400. Every CSRF failure — missing, invalid, expired, wrong-session,
    or duplicated token — is refused with HTTP 403 and never opens the store,
    but it is answered with the owner-authorized page carrying
    {!Project_home_provisioning_pages.Stale_form} and a fresh CSRF field rather
    than a terminal message page: the framework token lives one hour while the
    session that renders it lives two weeks, so a page left open (or served
    before a restart, which rotates the encryption secret) would otherwise
    become permanently unsubmittable. That re-render is already past the
    session, rollout and same-origin gates and re-authorizes stewardship in SQL,
    and it reflects nothing from the unverified submission. No store SQL opens
    before the submitted identity is structurally and semantically valid.

    {!Project_home_provisioning_form} owns the entire field grammar and
    canonicalization; nothing is trimmed, normalized, or repaired here. Each of
    its four errors re-renders the owner-authorized page with the matching
    {!Project_home_provisioning_pages.feedback} and HTTP 422 — never a redirect
    — after re-authorizing stewardship, so a form is never re-shown to a user
    who lost access between the GET and the POST. A semantic rejection preserves
    the exact three submitted values, escaped by the page; a structural
    [Invalid_form] rejection reflects nothing, since an unknown, duplicated, or
    missing field cannot be proven safe to echo.

    A valid identity is handed to {!Project_home_provisioning_store.provision}
    in one short SQL scope with no outer transaction and no pre-checking of slug
    availability, verification, stewardship, active home, lifecycle, membership,
    or moderator role. The result maps as: a committed
    {!Project_home_relation.Accepted} home answers a clean 303 to
    [/c/<created-slug>/settings], the existing canonical private-community
    management route the new top moderator may already reach, built structurally
    from the store's canonical slug with no query, fragment, or success token
    (any other resulting status is the generic 500); [Project_unavailable] and
    [Invalid_project_slug] are the one generic 404; [Community_slug_unavailable]
    re-renders with HTTP 409, the matching feedback, and the submitted values
    preserved so the slug can be edited and retried, revealing nothing about the
    conflicting community; [Active_home_exists] answers a clean 303 to
    [/projects/<slug>/request-home], the authoritative GET that renders the
    pending or connected state; [Invalid_user_id], [Inconsistent_data], and
    [Storage_error] are the generic non-cacheable 500.

    Replay is handled durably rather than by a token: a fresh-CSRF resubmission
    of a succeeded form creates nothing, because the project lock and the
    partial unique active-home index make the store answer [Active_home_exists],
    which redirects to the current home page. A stale replay of an unverifiable
    CSRF token stays a 403 that creates nothing. *)
