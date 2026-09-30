(** HTTP layer for the shared-threads workflow: the per-thread Share page,
    the community Shared-threads management page, and the five
    community-scoped mutations.

    {b Every handler is subject-scoped by its route.} The community comes
    from the route slug and the thread or placement from the route id;
    every subject is resolved server-side, and the acting user id and the
    session global-admin state come from the authenticated session. Nothing
    is ever taken from a hidden form field to establish authority: the two
    closed application fields that exist (the accept form's [section], the
    share-page forms' [context] marker) identify a record or a return
    surface and grant nothing.

    {b Authorization} is decided in the read models' SQL before any
    mutation, and the subject binding is re-verified inside the store's own
    guarded SQL:

    - share (GET and POST) — the canonical author while a current unbanned
      member of the origin community, an origin ['top_mod'], or a durable
      global admin; the route slug must equal the post's immutable origin;
    - accept / reject — a ['top_mod'] of the route community or a durable
      admin, and the route community must equal the placement's
      destination;
    - withdraw — the placement's original requester, an origin ['top_mod'],
      or a durable admin, and the route slug must equal the origin;
    - remove — a ['top_mod'] of the route community or a durable admin,
      and the route community must be one of the placement's two sides.

    A durable global admin is always both halves: the session claim only
    enables the [users.is_admin] check, never replaces it. Every
    unauthorized or unavailable outcome collapses into one generic 404, so
    a slug, thread id, or placement id cannot become an existence oracle —
    a tombstoned thread answers exactly like a missing one.

    Every mutation requires the framework CSRF field and answers success
    with a same-origin [303] carrying a closed [?done=] notice; a stale
    token re-renders the authorized page rather than dead-ending. Accepting
    into a sectioned destination requires the accept form's single section
    choice, resolved server-side and revalidated under lock by the store; a
    flat destination accepts with none. Nothing here logs a note, a query,
    or an id. *)

val make_share_page_handler : Dream.handler
(** [GET /c/:slug/t/:thread/share] — the authorized per-thread Share
    page. *)

val make_share_request_handler : Dream.handler
(** [POST /c/:slug/t/:thread/share] — creates one pending sharing request
    into the community named by the [destination] field, with the optional
    private [note]. *)

val make_management_page_handler : Dream.handler
(** [GET /c/:slug/settings/shared-threads] — the community management
    page: incoming requests, outgoing requests, shared into, shared
    from. *)

val make_accept_handler : Dream.handler
(** [POST /c/:slug/settings/shared-threads/:placement_id/accept] *)

val make_reject_handler : Dream.handler
(** [POST /c/:slug/settings/shared-threads/:placement_id/reject] *)

val make_withdrawal_handler : Dream.handler
(** [POST /c/:slug/settings/shared-threads/:placement_id/withdraw] *)

val make_removal_handler : Dream.handler
(** [POST /c/:slug/settings/shared-threads/:placement_id/remove] *)
