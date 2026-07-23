(** HTTP layer for the GitHub App installation flow
    (POST /integrations/github/install/start and
    GET /integrations/github/install/return), kept out of the legacy
    [Handlers] macro-module. *)

val make_start_installation_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for the authenticated installation start. Gate order:
    closed feature mode ([Off] is a controlled 404 with no configuration or
    SQL access), session authentication (anything but a positive [user_id]
    redirects to /login), the existing [Project_onboarding.onboarding_available]
    authorization decision (controlled 403, still no configuration read),
    [load_config] (any error is a generic 503), then a same-origin browser
    check against the validated public origin (exact Origin match, or —
    only when Origin is absent — [Sec-Fetch-Site: same-origin];
    controlled 403 otherwise).

    Only after every gate passes does it create the per-flow browser
    material, persist one onboarding state row (hashes only) via
    [Github_onboarding_state_store.issue] under [Dream.sql], and answer an
    explicit 303 to [Github_onboarding_urls.installation_url] carrying the
    flow's encrypted per-flow cookie. Issuance and cookie-storage failures
    are the same generic 503 — no cookie, no GitHub redirect, no diagnostic
    detail. The production route passes the mode resolved through
    [Project_onboarding] and [Github_app_config.from_env]; tests inject
    fixed values. *)

val make_setup_return_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for the GitHub App setup return — the cross-site
    top-level redirect GitHub sends the browser to after installation. There
    is deliberately no Dream-session gate: authorization is possession-based
    (the raw callback [state] plus this flow's encrypted per-flow cookie),
    and the owning user was recorded by the authenticated start endpoint.

    Every outcome is an explicit 303 with [Cache-Control: no-store],
    [Pragma: no-cache], and [Referrer-Policy: no-referrer] and an empty body
    — never a rendered page, so the state-bearing callback URL neither
    lingers, caches, nor leaks onward as a Referer.

    Order: [Off] (the kill switch) redirects to /bring before configuration,
    parsing, cookies, or SQL; [Admins] and [Public] both let an
    already-issued flow continue. Then [load_config] (any error is the same
    clean /bring redirect), then strict parsing of the raw request target —
    exactly one case-sensitive [state] (canonical via
    [Github_onboarding_crypto.state_of_callback]) and exactly one
    [installation_id] (ASCII decimal, positive, int64-representable), with
    unrelated extra parameters ignored and duplicates, blanks, or malformed
    values rejected without logging. Then the per-flow cookie: [Missing]
    redirects to /bring without SQL or state mutation; [Invalid] redirects
    to /bring and deletes the useless cookie.

    With valid material it atomically attaches the untrusted pending
    installation id via
    [Github_onboarding_state_store.attach_pending_installation] under
    [Dream.sql] (state, cookie-derived session-binding hash, flow — never a
    caller-supplied user). Success answers a 303 to
    [Github_onboarding_urls.authorization_url] with the cookie-derived PKCE
    challenge, leaving the per-flow cookie untouched (the OAuth callback
    still needs it); [State_unavailable] (and the parser-precluded
    [Invalid_pending_installation_id]) redirect to /bring and drop the
    cookie; [Storage_error] redirects to /bring keeping the cookie so a
    refresh can retry. *)
