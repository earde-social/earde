(** HTTP layer for the project-setup flow over verified GitHub drafts
    (GET /projects/new and POST /projects/new/repositories), kept out of the
    legacy [Handlers] macro-module. Both handlers derive access only from the
    injected closed mode and the Dream session (a [user_id] counts only when
    it parses as a positive integer; [is_admin = "true"] matters only
    alongside a valid user), through the existing
    [Project_onboarding.onboarding_available] policy — never from
    [connected_by_user_id], GitHub logins, query parameters, or form fields.
    Every rendered page carries [Cache-Control: no-store] and
    [Referrer-Policy: no-referrer]; every redirect is an explicit empty-body
    303 that additionally carries [Pragma: no-cache] and never reflects a
    request value. No query or form value is ever logged. *)

val make_new_project_handler :
  mode:Project_onboarding.mode ->
  Dream.handler
(** Handler factory for GET /projects/new. Gate order: [Off] is a clean 303
    to /bring with no query read and no SQL; a missing/invalid session user
    is a clean 303 to /login (no return URL); an [Admins]-mode non-admin is
    a clean 303 to /bring (no feature-flag names revealed).

    For an authorized viewer the raw request target is parsed
    duplicate-aware: exactly one [draft=<positive decimal int64>] selects a
    draft explicitly (anything else — blank, bare, duplicated, signed,
    spaced, non-decimal, overflowing, zero, or percent-encoded — is treated
    as an unavailable selection, never as a database identifier), and
    exactly one recognized [selection=saved|stale|invalid|unavailable]
    yields the cosmetic one-time feedback (anything else yields none;
    feedback never affects authorization or which rows are read). Reads go
    through [Project_onboarding_draft_read_model] in short [Dream.sql]
    scopes: an explicitly selected available draft renders the configuration
    form; an unavailable explicit draft falls back to the normal
    chooser/empty state with the generic unavailable feedback (which
    overrides any supplied feedback); with no explicit draft, zero drafts
    render the empty state, exactly one renders its configuration (re-loaded
    and re-authorized, with a single bounded retry of the list decision if
    it disappears in between), and several render the chooser in read-model
    order with nothing auto-selected. [Inconsistent_data] and
    [Storage_error] are the repository's generic non-cacheable 500 with no
    database detail. The configuration form receives the live request so it
    carries Dream's framework CSRF field. *)

val make_repository_selection_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit ->
      (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for POST /projects/new/repositories. The same access
    gates as the GET run first ([Off] → /bring; anonymous → /login;
    [Admins]-mode non-admin → /bring), then [load_config] (any error is a
    generic non-cacheable 503 with no configuration read surfaced, no form
    parsing, and no SQL) — the configuration exists only to reuse the exact
    normalized public-origin policy of the GitHub installation-start
    handler, shared through [Request_origin.same_origin_request]: a present
    [Origin] must equal the configured public origin exactly, an absent
    [Origin] requires [Sec-Fetch-Site: same-origin], and everything else is
    a generic 403 before the form is read. No GitHub credential is used and
    no outbound HTTP occurs.

    The form is then read through Dream's own CSRF-verifying form API:
    missing, invalid, expired, wrong-session, or duplicated CSRF tokens are
    a generic 403; a wrong content type is a generic 400; verified
    application fields (Dream already strips its own [dream.csrf] field) go
    to the strict [Project_setup_repository_form] parser, whose rejection is
    a clean PRG redirect to [/projects/new?selection=invalid] that retains
    no untrusted draft id. A parsed submission runs
    [Project_onboarding_draft_selection_store.replace] (with no primary
    repository — that is a later step) in one short [Dream.sql] scope, and
    the result maps to structurally built ([Uri]) 303 redirects:
    success → [?draft=<id>&selection=saved]; [Selection_stale] →
    [?draft=<id>&selection=stale]; [Invalid_selection] →
    [?draft=<id>&selection=invalid]; [Draft_unavailable] →
    [?selection=unavailable] with the submitted id dropped;
    [Invalid_user_id]/[Invalid_draft_id] → [?selection=invalid] with the id
    dropped; [Inconsistent_data]/[Storage_error] → the generic non-cacheable
    500 (a server failure is never disguised as a user form error). *)
