(** HTTP layer for permanent project creation over verified GitHub drafts
    (POST /projects and the permanent GET /projects/:slug/setup
    destination). Both
    handlers derive access only from the injected closed mode and the Dream
    session (a [user_id] counts only when it parses as a positive integer;
    [is_admin = "true"] matters only alongside a valid user), through the
    existing [Project_onboarding.onboarding_available] policy — never from
    provenance columns, GitHub logins, query parameters, or form fields.
    Every rendered page carries [Cache-Control: no-store] and
    [Referrer-Policy: no-referrer]; every redirect is an explicit
    empty-body 303 that additionally carries [Pragma: no-cache] and never
    reflects a request value. No query or form value is ever logged, and no
    submitted permanent-project value ever travels through a URL or
    cookie. *)

val make_project_creation_handler :
  mode:Project_onboarding.mode ->
  load_config:
    (unit ->
      (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** Handler factory for POST /projects. The same access gates as the other
    project-setup handlers run first ([Off] → /bring; anonymous → /login;
    [Admins]-mode non-admin → /bring), then [load_config] (any error is a
    generic non-cacheable 503 with no form parsing and no SQL) — the
    configuration exists only to reuse the exact public-origin policy
    shared through [Request_origin.same_origin_request], whose rejection is
    a generic 403 before the form is read. The form then goes through
    Dream's own CSRF-verifying form API: every CSRF failure is a generic
    403, a wrong content type a generic 400, and verified application
    fields reach the strict [Project_identity_form] parser.

    A structurally invalid submission never reaches the finalization
    store: a routing draft id is recovered only by a strict private helper
    (exactly one [draft_id] field, positive decimal int64) and counts only
    after [Project_onboarding_draft_read_model.load_available] re-authorizes
    it for the current user — an owner-authorized draft with a selection
    re-renders the identity step with neutral values and generic invalid
    feedback as a 400; an authorized draft with no selection takes the
    repository-step redirect; everything else is one generic 400 that
    discloses nothing about the submitted id.

    A parsed submission loads the owner-authorized draft view (unavailable
    → [?selection=unavailable] with the id dropped; read-model failures →
    generic 500), derives the authoritative selected snapshot ids
    server-side (an empty selection → [?draft=<id>&selection=required]),
    and builds the identity through
    [Project_identity_form.create_identity]. Field-level domain errors
    re-render the identity step with the submitted values, the matching
    feedback, and a fresh CSRF field as a non-cacheable 422; the impossible
    selected-context errors map to the generic 500 and the repository-step
    redirect respectively.

    A valid identity runs [Project_finalization_store.finalize] in its own
    short [Dream.sql] scope (the store owns its transaction; no connection
    is retained from the read). Success is the permanent PRG redirect to
    [/projects/<canonical-slug>/setup], built structurally with nothing
    else in the URL. [Draft_unavailable] → [?selection=unavailable];
    [No_repositories_selected] → [?draft=<id>&selection=required];
    [Selection_stale] → [?draft=<id>&selection=stale]. The three
    re-renderable conflicts ([Kind_namespace_mismatch], [Slug_unavailable],
    [Repository_already_connected]) first reload the current draft view —
    never reusing the pre-finalization view — and, when the draft is still
    available with a selection, re-render the submitted values with the
    matching feedback as a 409 (falling back to the unavailable or
    required redirect otherwise; a reload failure is the generic 500). The
    impossible invalid-input errors and [Inconsistent_data]/[Storage_error]
    are the generic non-cacheable 500 — a server failure is never disguised
    as a user input error, and no conflicting project or repository is ever
    identified. *)

val make_project_home_setup_handler :
  mode:Project_onboarding.mode ->
  Dream.handler
(** Handler factory for GET /projects/:slug/setup — the permanent PRG
    destination. The same access gates run first ([Off] → /bring;
    anonymous → /login; [Admins]-mode non-admin → /bring); then the route
    slug is read and the project loaded through
    [Project_home_setup_read_model.load_for_steward] in one short
    [Dream.sql] scope — authorization by [project_stewards] membership and
    [verified] status inside the SQL, never by provenance. A steward
    receives the rendered permanent setup page as a non-cacheable 200;
    every unavailable state — invalid slug shape, nonexistent slug, a
    project stewarded by someone else, and stale or revoked projects —
    collapses to the repository's one generic 404, and read-model failures
    are the generic non-cacheable 500. Foreign probes are never redirected
    to a project page and no project name leaks through a failure. *)
