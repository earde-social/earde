(** HTTP layer for the existing-community home request of one verified
    permanent project (GET and POST /projects/:slug/request-home). Both handlers derive access only
    from the injected closed mode and the Dream session (a [user_id] counts
    only when it parses as a positive integer; [is_admin = "true"] matters
    only alongside a valid user), through the existing
    [Project_onboarding.onboarding_available] policy — never from
    provenance columns, query parameters, or form fields. Every rendered
    page carries [Cache-Control: no-store] and
    [Referrer-Policy: no-referrer]; every redirect is an explicit
    empty-body 303 that additionally carries [Pragma: no-cache] and never
    reflects a request value. No query or form value is ever logged, and no
    submitted value — target community id, request note — ever travels
    through a URL or cookie. *)

val make_project_home_choice_handler :
  mode:Project_onboarding.mode ->
  Dream.handler
(** Handler factory for GET /projects/:slug/request-home. The same access
    gates as the other project-setup handlers run first ([Off] → /bring;
    anonymous → /login; [Admins]-mode non-admin → /bring), then the route
    slug is read verbatim — never trimmed, lowercased, decoded, or
    repaired — and the choice view loaded through
    [Project_home_choice_read_model.load_for_steward] in one short
    [Dream.sql] scope, which owns canonical-slug validation, steward
    authorization, and [verified]-status filtering inside its SQL.

    A steward receives the noindex, non-cacheable choice page as a 200:
    the eligible-community chooser (with a fresh framework CSRF field), the
    no-eligible-communities state, or the current pending/accepted active
    relation, mapped mechanically from the read model's public accessors. A
    [Rejected] or [Removed] value crossing the active-relation accessor is
    the generic 500 — never reconstructed into a page state. Every
    unavailable project — invalid slug shape, nonexistent, foreign,
    unstewarded, stale, revoked, or a missing route parameter — collapses
    to the one generic 404, and read-model failures are the generic
    non-cacheable 500. No query parameters and no flash state exist. *)

val make_project_home_request_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit ->
      (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for POST /projects/:slug/request-home. The same access
    gates run first, then the route slug is read; [load_config] follows
    (any error is a generic non-cacheable 503 with no form parsing and no
    SQL) — the configuration exists only to reuse the exact public-origin
    policy shared through [Request_origin.same_origin_request], whose
    rejection is a generic 403 before the form is read. The form then goes
    through Dream's own CSRF-verifying form API: a wrong content type is a
    generic 400, and every CSRF failure (missing, invalid, expired,
    wrong-session, duplicated) is refused with 403 and never reaches the
    store — answered by reloading the authorized current state with
    [Project_home_choice_pages.Stale_form] and a fresh token, reflecting
    nothing submitted, so a page whose one-hour token outlived its
    fourteen-day session does not become permanently unsubmittable.
    Verified application fields reach the strict
    [Project_home_request_form] parser.

    A structurally invalid submission never reaches the store: the current
    owner-authorized view is reloaded and re-rendered as a 400 with generic
    invalid feedback and an empty note — nothing malformed is reflected. A
    parsed submission builds the pending relation through
    [Project_home_request_form.create_relation]; an invalid note re-renders
    the current state as a 422, preserving the submitted note in the
    textarea only when a private output-safety gate passes (valid UTF-8, no
    NUL, no ASCII control except tab and LF, no DEL — over-length notes are
    preserved untruncated, unsafe ones dropped whole, and nothing is made
    domain-acceptable).

    A valid pending relation runs [Project_home_request_store.create] in
    its own short [Dream.sql] scope — the store owns its transaction,
    steward authorization, target eligibility, locking, and active-home
    concurrency; the handler never pre-authorizes the community. Success is
    the PRG redirect back to the same permanent route with nothing else in
    the URL; the redirected GET observes the durable pending relation.
    [Project_unavailable] (and the impossible [Invalid_project_slug]) is
    the generic 404; [Community_unavailable] and [Active_home_exists]
    reload the current view — never reusing the pre-store read — and
    re-render it as a 409 with the matching generic feedback (an active
    relation renders with no form, so no note survives there; the
    unavailable-community case keeps the safely preserved note). The
    impossible invalid-input errors and
    [Inconsistent_data]/[Storage_error] are the generic non-cacheable
    500 — a server failure is never disguised as a user request error, and
    no lifecycle detail, SQL, or constraint name is ever identified. *)
