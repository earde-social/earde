(** HTTP layer for the final setup surface of one provisioned network community:
    GET [/c/:slug/setup] renders the review-and-publish page, and POST
    [/c/:slug/publish] — exactly the action that page's single form emits —
    commits the publication.

    Both handlers are factories over the closed onboarding mode (and, for the
    POST, a configuration loader) so tests can inject fixed values without
    touching the process environment, and a disabled or rejected request never
    reads the route parameter, loads configuration, or runs SQL. Nothing here
    logs a query or a form value, and no state travels in a query string,
    cookie, or flash message.

    Gate order, applied before any route-parameter read or SQL: onboarding mode
    [Off] answers a clean 303 to [/bring]; an anonymous, malformed, or
    non-positive session user answers a clean 303 to [/login]; in [Admins] mode
    an authenticated non-admin answers a clean 303 to [/bring]. A session
    [is_admin] claim has no meaning without a valid positive user id, and it
    never authorizes the page itself. Every redirect has an empty body and
    carries [Cache-Control: no-store], [Pragma: no-cache], and
    [Referrer-Policy: no-referrer].

    Past the gates the community is authorized entirely by
    {!Network_community_publication_read_model}: a current [top_mod] of that
    exact community or a durable [users.is_admin] holder, and only while the
    community is still a private network setup draft. Neither the route shape
    nor a session flag ever authorizes. A nonexistent slug, a legacy community,
    an already-published network community, an ordinary member, a [mod], a
    [legacy_mod], a moderator of another community, a removed top moderator, and
    a malformed route slug all collapse to one byte-identical generic 404, so
    probing cannot become an authorization or lifecycle oracle. Every durable
    failure past that answers one generic non-cacheable 500 carrying no Caqti,
    PostgreSQL, or error-constructor detail. Rendered pages are
    [Cache-Control: no-store], [Referrer-Policy: no-referrer], and noindex. *)

val make_network_community_publication_page_handler :
  mode:Project_onboarding.mode -> Dream.handler
(** GET [/c/:slug/setup]. The form is prefilled with the draft's current
    canonical identity, [public] as the initial publication choice, no feedback,
    and a fresh framework CSRF field. *)

val make_network_community_publication_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** POST [/c/:slug/publish]. Past the shared gates the sequence is fixed: read
    the route slug, load the GitHub App configuration (a failure is one generic
    non-cacheable 503 that names no configuration detail), enforce the
    established same-origin policy through {!Request_origin} (a rejection is one
    generic 403, decided {i before} the body is parsed), then [Dream.form] — a
    wrong content type or malformed framework form is one generic 400, and every
    CSRF failure (missing, invalid, expired, wrong-session, duplicated) is
    refused with 403 and never reaches the store — answered by re-rendering the
    owner-authorized page with [Network_community_publication_pages.Stale_form]
    and a fresh token, reflecting nothing submitted, so a page whose one-hour
    token outlived its fourteen-day session does not become permanently
    unsubmittable. The route slug is never trimmed, lowercased, decoded,
    repaired, or reflected: the read model and the store own its validation.

    The submission is then parsed by
    {!Network_community_publication_form.of_fields}, whose five errors each
    re-render the owner-authorized page with the matching generic feedback at
    HTTP 422 — never a redirect. A structurally valid but semantically rejected
    submission keeps its exact four submitted values, escaped by the page;
    {!Network_community_publication_form.Invalid_form} reflects nothing, so no
    unknown, duplicated, or planted field can ride back. Every re-render, at any
    status, re-authorizes the community durably through
    {!Network_community_publication_read_model.load_for_publisher} in its own
    short scope, so a private draft never renders again on stale authority, and
    each carries a fresh framework CSRF field.

    An accepted submission goes to
    {!Network_community_publication_store.publish} in one short [Dream.sql]
    scope with no outer transaction and nothing pre-checked: the store owns the
    locks, the lifecycle transition, the slug arbitration, the commit, and the
    rollback. A commit answers a clean 303 to [/c/<slug>] built structurally
    from the canonical slug the store returned — the new one when publication
    moved it, with no query, fragment, or result token, and deliberately not
    [/settings]: the product result of publishing is the now-public community.
    {!Network_community_publication_store.Community_slug_unavailable} re-renders
    at HTTP 409 under the {i current} route slug the failed transaction
    preserved, keeping the submission editable and retryable and naming nothing
    about the conflicting community.
    {!Network_community_publication_store.Draft_unavailable} — which absorbs a
    missing, legacy, already-published, renamed, or concurrently published
    community, a lost top-mod role, a revoked durable admin, and a removed home
    relation, and therefore also a replayed successful submission — is the same
    byte-identical generic 404 as the GET's. A defensive
    {!Network_community_publication_store.Invalid_community_slug} is that same
    404; a defensive {!Network_community_publication_store.Invalid_user_id},
    {!Network_community_publication_store.Inconsistent_data}, and
    {!Network_community_publication_store.Storage_error} are one generic
    non-cacheable 500 carrying no Caqti, PostgreSQL, SQLSTATE, or
    constraint-name detail.

    [load_config] is used for the origin policy only: no GitHub credential is
    read and no outbound HTTP occurs. *)
