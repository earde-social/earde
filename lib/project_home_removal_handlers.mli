(** HTTP layer for removing an accepted project-home relation, from either
    authorized surface.

    Two routes, one behaviour:

    - [POST /projects/:project_slug/community-home/:community_slug/remove] —
      emitted by the steward-facing home-choice page;
    - [POST /c/:community_slug/projects/:project_slug/remove-home] — emitted by
      the community settings "Connected projects" section.

    Both call {!Project_home_removal_store.remove} with the same project and
    community slugs and differ only in where a completed or already-completed
    removal returns the browser. The route shape adds no second authorization
    policy: the store alone owns project and community validation, all three
    durable authorities (project steward, target [top_mod], durable
    [users.is_admin]), lock ordering, transition legality, concurrency, and
    commit/rollback — so a top moderator may successfully call the
    project-shaped route and a steward the community-shaped one. The page that
    emitted a form is never proof of anything.

    Both handlers apply the established project-onboarding gates, in the
    established order, before reading a route parameter, loading configuration,
    checking origin, reading the form, or opening SQL: mode [Off] redirects to
    [/bring]; an anonymous, malformed, zero, or negative session user redirects
    to [/login]; and an authenticated non-admin under [Admins] redirects to
    [/bring]. A session [is_admin] claim grants nothing without a valid positive
    session user id, and grants nothing durable at all — the store consults the
    [users.is_admin] column.

    Nothing here logs a route value, a form value, or an identifier, and nothing
    caller-controlled is ever reflected into a URL, cookie, header, or body.
    Every redirect is a 303 with an empty body; every error is one of the
    generic non-cacheable responses, so missing projects, missing communities,
    lost stewardship, insufficient moderator role, revoked durable admin, a
    wrong project/community pair, and an already-removed relation all stay
    indistinguishable. *)

val make_project_side_home_removal_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** [POST /projects/:project_slug/community-home/:community_slug/remove].

    On success — and on [Removal_unavailable], so a stale form, a replayed
    submission, and a concurrent removal are all safe and produce no error
    oracle and no second mutation — redirects to
    [/projects/<project-slug>/request-home], the permanent steward route that
    authoritatively shows the project's current home state. No success or
    failure query parameter is added.

    Configuration failure is a generic non-cacheable 503, a rejected origin a
    generic 403 (checked before the form is read), a CSRF failure a generic 403,
    a wrong content type or malformed framework form a generic 400, and any
    application field at all a generic 400 that never reaches the store — the
    route itself expresses removal, so the verified form must carry zero
    application fields. Malformed slugs and every unavailable or unauthorized
    outcome are one generic 404; durable corruption and storage failures are one
    generic non-cacheable 500. *)

val make_community_side_home_removal_handler :
  mode:Project_onboarding.mode ->
  load_config:(unit -> (Github_app_config.t, Github_app_config.error) result) ->
  Dream.handler
(** [POST /c/:community_slug/projects/:project_slug/remove-home].

    Identical security sequence, store call, and result mapping to
    {!make_project_side_home_removal_handler}; only the destination differs.
    Success and [Removal_unavailable] alike redirect to the existing canonical
    community settings route with its existing panel parameter,
    [/c/<community-slug>/settings?panel=projects], where the connected-project
    management section renders the current durable state and the removed project
    no longer appears. No new settings route is introduced. *)
