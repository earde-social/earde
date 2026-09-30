(** HTTP layer for the GitHub App installation flow
    (POST /integrations/github/install/start,
    GET /integrations/github/install/return, and
    GET /integrations/github/authorize/callback). *)

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

val make_bring_handler :
  mode:Project_onboarding.mode ->
  Dream.handler
(** Handler factory for the onboarding entry and return page (GET /bring).
    Requires no GitHub configuration, credentials, database connection, or
    outbound HTTP: every state is a normal 200 HTML page in the shared
    layout, with [Cache-Control: no-store] and [Referrer-Policy:
    no-referrer] — the page reflects session identity, rollout mode, and
    one-time callback feedback, so it must never be cached or leak its
    query onward.

    Access is derived only from the closed [mode] and the Dream session
    (a [user_id] counts only when it parses as a positive integer;
    [is_admin = "true"] matters only alongside a valid user), through the
    existing [Project_onboarding.onboarding_available] policy: [Off]
    explains that project onboarding is currently unavailable; a permitted
    mode without a valid user shows a plain /login link (no return URL);
    [Admins] for a non-admin shows the rollout-limited state; an authorized
    viewer gets the single parameter-free POST form to
    /integrations/github/install/start, whose handler remains authoritative
    and repeats every access and same-origin check.

    The callback's [github] query parameter is interpreted strictly and
    duplicate-aware: exactly one [github=connected] renders the success
    banner, exactly one [github=failed] the generic anti-oracle failure
    banner, and anything else — missing, blank, bare, unknown,
    differently-cased, duplicated, or conflicting — renders none. The raw
    value never reaches the rendered page or a log, and feedback never
    changes feature or user access. *)

val make_oauth_callback_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit -> (Github_app_config.t, Github_app_config.error) result) ->
  load_credentials:
    (unit ->
     (Github_oauth_credentials.t, Github_oauth_credentials.error) result) ->
  exchange_transport:(module Github_oauth_token_exchange.TRANSPORT) ->
  installations_transport:(module Github_user_installations.TRANSPORT) ->
  repositories_transport:
    (module Github_user_installation_repositories.TRANSPORT) ->
  Dream.handler
(** Handler factory for the final GitHub OAuth authorization callback.
    Sessionless like the setup return: authorization is possession-based
    (the raw callback [state] plus this flow's encrypted per-flow cookie),
    and the authoritative user is read back from the consumed state row —
    never from a Dream session.

    Every outcome is a 303 with an empty body, [Cache-Control: no-store],
    [Pragma: no-cache], and [Referrer-Policy: no-referrer], to exactly one
    of two clean local targets: [/projects/new] on full success — the
    owner-authorized setup page, which re-derives the viewer's drafts from
    the normal session, so the Location stays parameter-free —
    [/bring?github=failed] for every failure. Which internal stage failed —
    parsing, GitHub rejection, cookie, configuration, credentials, state
    consumption, token exchange, installation verification, or persistence
    — is deliberately indistinguishable, and no callback value, token,
    verifier, binding, or identity ever appears in a response.

    Order: [Off] (the kill switch) answers the failure redirect before
    parsing, configuration, cookies, SQL, or GitHub. Then strict parsing of
    the raw target — exactly one case-sensitive canonical [state], plus
    either exactly one valid [code] and no [error] (authorization success)
    or exactly one non-empty [error] and no [code] (authorization
    rejection, its value never inspected); everything else is rejected.
    Then [load_config]; then the per-flow cookie ([Missing] fails without
    deletion, [Invalid] fails and deletes it).

    A parsed rejection ends there: the cookie is deleted, and credentials,
    SQL, and GitHub stay untouched — the state row expires naturally. For
    an authorization code, [load_credentials] runs next (failure keeps the
    cookie and touches nothing else); then the state is consumed in one
    short [Dream.sql] scope ([Storage_error] keeps the cookie; every other
    consume error deletes it), and — with no pooled database connection
    held — the code is exchanged, the pending installation verified, and
    the verified installation's public repositories listed over the
    injected transports, the complete repository set materialized before
    any further SQL. A second, separate [Dream.sql] scope then runs
    [Github_installation_store.record_verified] under the consumed row's
    stored user and, only after it succeeds,
    [Project_onboarding_draft_store.refresh_verified] for that same user —
    sequentially on the one supplied connection, with no outer transaction
    (the draft store owns its own). A draft failure after the installation
    committed is terminal for the flow but intentionally leaves the
    installation row active: a fresh onboarding attempt reuses it
    idempotently and recreates the draft. Success is reported only after
    the draft refresh succeeds; the draft id never appears in any
    response. All terminal branches — success included — delete the
    per-flow cookie; no token is ever persisted or passed to the
    persistence layer. *)
