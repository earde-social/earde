(** HTTP layer for starting the GitHub App installation flow
    (POST /integrations/github/install/start), kept out of the legacy
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
