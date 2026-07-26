(** HTTP layer for the dedicated-community-home creation entry of one
    verified permanent project: GET [/projects/:slug/community-home/new].

    The handler is a factory over the closed onboarding mode so tests can
    inject a fixed value without touching the process environment, and a
    disabled or rejected request never reads the route parameter or runs SQL.
    Nothing here logs a query value, and no request value is ever reflected
    into a redirect, a query string, or a cookie.

    Gate order, applied before any route-parameter read or SQL: onboarding
    mode [Off] answers a clean 303 to [/bring]; an anonymous, malformed, or
    non-positive session user answers a clean 303 to [/login]; in [Admins]
    mode an authenticated non-admin answers a clean 303 to [/bring]. Every
    redirect has an empty body and carries [Cache-Control: no-store],
    [Pragma: no-cache], and [Referrer-Policy: no-referrer].

    Past the gates the project is authorized entirely by
    {!Project_home_provisioning_read_model}: only a steward of a verified
    project with no active home relation reaches the page. Nonexistent,
    foreign, unstewarded, stale, revoked, and already-homed projects — and a
    malformed route slug — collapse to one generic 404, so probing cannot
    become an ownership or state oracle. Every durable failure past that
    answers one generic non-cacheable 500 carrying no Caqti, PostgreSQL, or
    error-constructor detail. The rendered page is [Cache-Control: no-store],
    [Referrer-Policy: no-referrer], and noindex, and carries no query or
    flash state.

    This slice registers the GET only: the page's form posts to
    [/projects/:slug/community-home], which is deliberately not routed yet. *)

val make_project_home_provisioning_page_handler :
  mode:Project_onboarding.mode -> Dream.handler
