(** HTTP layer for the moderator review of pending project-home requests (GET
    /c/:slug/project-home-requests and the accept/reject POSTs). All three
    handlers derive access only from the injected closed mode and the Dream
    session (a [user_id] counts only when it parses as a positive integer;
    [is_admin = "true"] matters only alongside a valid user), through the
    existing [Project_onboarding.onboarding_available] policy — never from
    provenance columns, query parameters, or form fields.

    Authorization is never decided in the handler: both the read model and the
    transactional review store reauthorize entirely in SQL (a current [top_mod]
    of the target community, or a durable [users.is_admin]), and every
    unauthorized or missing outcome collapses to one generic 404 so a community
    slug cannot become an authorization oracle. Every rendered queue carries
    [Cache-Control: no-store] and [Referrer-Policy: no-referrer] and stays
    [noindex]; every redirect is an explicit empty-body 303 that additionally
    carries [Pragma: no-cache] and never reflects a request value. No query or
    form value is ever logged, and no request note, relation id, or internal id
    ever travels through a URL, cookie, or log. *)

val make_project_home_review_queue_handler :
  mode:Project_onboarding.mode -> Dream.handler
(** Handler factory for GET /c/:slug/project-home-requests. The same access
    gates as the other project handlers run first ([Off] → /bring; anonymous or
    malformed/non-positive [user_id] → /login; [Admins]-mode non-admin →
    /bring), then the [slug] route value is read verbatim — never trimmed,
    lowercased, decoded, or repaired — and the pending queue loaded through
    [Project_home_review_read_model.load_for_reviewer] in one short [Dream.sql]
    scope, which owns canonical-slug validation and top-mod/admin authorization
    inside its SQL.

    An authorized reviewer receives the noindex, non-cacheable queue page as a
    200 with [feedback = None]: the target community (with its current host
    eligibility) and its pending requests in read-model order, each request
    carrying a reject form and — only for a verified project against an eligible
    community with at least one repository — an accept form, both with a fresh
    framework CSRF field. The private request note is passed only to this
    authorized page. [Ok None] (missing community, ordinary member, non-member,
    [mod]/[legacy_mod], moderator of another community, removed or downgraded
    top moderator, session-only admin) and an invalid community slug collapse to
    the one generic 404; read-model corruption or storage failure is the generic
    non-cacheable 500. The GET takes no locks, performs no state change, and
    uses no query parameters or flash state. *)

val make_project_home_accept_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for POST /c/:slug/projects/:project_slug/accept. After the
    same access gates, the [slug] and [project_slug] route values are read
    (either missing is the generic 404), then [load_config] follows (any error
    is a generic non-cacheable 503 with no form parsing and no SQL) — the
    configuration exists only to reuse the exact public-origin policy shared
    through [Request_origin.same_origin_request], whose rejection is a generic
    403 before the form is read. The form then goes through Dream's own
    CSRF-verifying form API: a wrong content type is a generic 400; every CSRF
    failure (missing, invalid, expired, wrong-session, duplicated) is refused
    with 403 and never reaches the store — answered by reloading the
    reviewer-authorized queue with [Project_home_review_pages.Stale_form] and a
    fresh token, so a queue whose one-hour token outlived its fourteen-day
    session does not become permanently unactionable. Past that the application
    field list must be exactly empty — any remaining field (a browser-supplied
    decision, an id, a return URL, a duplicate, or an unknown key) is a generic
    400 that never reaches the store.

    A verified form runs [Project_home_review_store.review] with
    [decision = Accept] in one short [Dream.sql] scope — the store owns its
    transaction, durable validation, current top-mod/admin authorization, lock
    ordering, verification/eligibility rules, concurrent-review arbitration, and
    commit/rollback; the handler never pre-authorizes. Success is the PRG
    redirect to /c/<canonical-slug>/project-home-requests with no project slug,
    decision, result status, reviewer, or feedback token in the URL, whose
    redirected GET observes the durable queue with the reviewed request gone.
    [Invalid_project_slug]/[Invalid_community_slug] are the generic 404;
    [Community_unavailable]/[Reviewer_unauthorized] are the generic 404 without
    reloading the private queue; [Review_unavailable], [Project_unavailable],
    and [Target_ineligible] reload the authoritative current queue — never
    reusing the pre-POST read — and re-render it as a 409 with, respectively,
    the generic review-unavailable, project-unavailable, and target-ineligible
    copy (a lost reviewer authorization during reload becomes the generic 404).
    The defensive [Invalid_user_id], a decision/result mismatch, and
    [Inconsistent_data]/[Storage_error] are the generic non-cacheable 500 — a
    server failure is never disguised as a recoverable review error, and no
    lifecycle detail, SQL, or constraint name is ever identified. *)

val make_project_home_reject_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for POST /c/:slug/projects/:project_slug/reject. The gate,
    route, configuration, origin, CSRF, and empty-form sequence is identical to
    the accept handler; only the store [decision] differs ([Reject]) and the
    store-result mapping is decision-specific. Rejection is allowed for
    verified, stale, and revoked projects, so [Project_unavailable] on this
    route means the project is genuinely absent and reloads with the generic
    review-unavailable copy (never acceptance-specific wording for a failed
    rejection), and [Target_ineligible] is impossible and maps defensively to
    the generic non-cacheable 500. Success is the same PRG redirect to
    /c/<canonical-slug>/project-home-requests, whose reloaded queue shows the
    request rejected and the active-home slot free. All other outcomes map
    exactly as in the accept handler. *)
