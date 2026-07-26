(** HTTP layer for the final setup surface of one provisioned network
    community: GET [/c/:slug/setup] renders the review-and-publish page.

    The POST that page's form names, [/c/:slug/publish], is deliberately
    {i not} registered in this slice — there is no publication store yet, and
    no route anywhere may publish, transition, or edit a network community's
    identity in the meantime.

    The handler is a factory over the closed onboarding mode so tests can
    inject a fixed value without touching the process environment, and a
    disabled or rejected request never reads the route parameter or runs SQL.
    Nothing here logs a query, and no state travels in a query string,
    cookie, or flash message.

    Gate order, applied before any route-parameter read or SQL: onboarding
    mode [Off] answers a clean 303 to [/bring]; an anonymous, malformed, or
    non-positive session user answers a clean 303 to [/login]; in [Admins]
    mode an authenticated non-admin answers a clean 303 to [/bring]. A
    session [is_admin] claim has no meaning without a valid positive user id,
    and it never authorizes the page itself. Every redirect has an empty body
    and carries [Cache-Control: no-store], [Pragma: no-cache], and
    [Referrer-Policy: no-referrer].

    Past the gates the community is authorized entirely by
    {!Network_community_publication_read_model}: a current [top_mod] of that
    exact community or a durable [users.is_admin] holder, and only while the
    community is still a private network setup draft. Neither the route shape
    nor a session flag ever authorizes. A nonexistent slug, a legacy
    community, an already-published network community, an ordinary member, a
    [mod], a [legacy_mod], a moderator of another community, a removed top
    moderator, and a malformed route slug all collapse to one byte-identical
    generic 404, so probing cannot become an authorization or lifecycle
    oracle. Every durable failure past that answers one generic
    non-cacheable 500 carrying no Caqti, PostgreSQL, or error-constructor
    detail. Rendered pages are [Cache-Control: no-store],
    [Referrer-Policy: no-referrer], and noindex. *)

val make_network_community_publication_page_handler :
  mode:Project_onboarding.mode -> Dream.handler
(** GET [/c/:slug/setup]. The form is prefilled with the draft's current
    canonical identity, [public] as the initial publication choice, no
    feedback, and a fresh framework CSRF field. *)
