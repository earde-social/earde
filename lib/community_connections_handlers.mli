(** HTTP layer for the community-connections management workflow: the
    management page, the two-step connect flow, and the four community-scoped
    mutations.

    {b Every handler is community-scoped.} The subject community comes from
    the route and is resolved server-side; the acting user id and the session
    global-admin state come from the authenticated session. Nothing is ever
    taken from a hidden form field: a connection or target id in a path or a
    field identifies a record and never establishes authority.

    {b Authorization} is a current ['top_mod'] row of the {i route} community,
    or a global administrator whose session claim is still backed by a durable
    [users.is_admin] flag. It is decided in the read model's SQL before any
    mutation, and the subject binding is re-verified inside the store's own
    guarded SQL:

    - request — the route community is the requester;
    - accept / reject — the route community must equal the connection's
      [recipient_community_id], and that id is passed into the guarded
      mutation;
    - remove — the route community must be one of the two sides, and that id
      is passed as the acting community.

    A mismatch between the route community and the connection's subjects is
    refused before the store is opened, and no action is ever authorized
    after a mutation has committed.

    Every unauthorized or unavailable outcome collapses into one generic 404,
    so a community slug or a connection id cannot become an existence oracle.
    Every mutation requires the framework CSRF field and answers with a
    same-origin [303] to the community's own management page; a stale token
    re-renders the authorized page rather than dead-ending, because the
    one-hour token lives inside a two-week session. Nothing here logs a
    query, a note, or an id. *)

val make_connections_page_handler : Dream.handler
(** [GET /c/:slug/settings/connections] — the authorized management page. *)

val make_connections_search_handler : Dream.handler
(** [GET /c/:slug/settings/connections/new] — step one (target search) and,
    when [?target=] names a still-connectable community, step two (the
    confirmation form). Read-only; a [?q=] that matches nothing and one that
    was withheld are indistinguishable. *)

val make_connection_request_handler : Dream.handler
(** [POST /c/:slug/settings/connections/request] — sends one request from the
    route community to the community named by the [target] field, with the
    optional private [note]. *)

val make_connection_accept_handler : Dream.handler
(** [POST /c/:slug/settings/connections/:id/accept] *)

val make_connection_reject_handler : Dream.handler
(** [POST /c/:slug/settings/connections/:id/reject] *)

val make_connection_removal_handler : Dream.handler
(** [POST /c/:slug/settings/connections/:id/remove] *)
