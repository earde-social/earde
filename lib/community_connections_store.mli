(** Transactional writes for the mutual connection between two Earde
    communities: request, accept, reject, remove.

    Each call is one explicit transaction. Every successful mutation writes
    exactly one {!Community_connection_audit} event on the same transaction,
    so a committed connection change without its event — or an event without
    its change — cannot exist. Every failure rolls back whole: no partial
    row, no orphan event, no timestamp left behind.

    {b Lock order.} There is exactly one order, and every function takes a
    prefix of it: the two [communities] rows in ascending id order
    ([FOR KEY SHARE] — the same lock the foreign keys take, so concurrent
    callers never block each other and a deletion of a referenced community
    cannot slip between the check and the insertion), then the exact
    [community_connections] row ([FOR UPDATE]). {!request} takes the
    community prefix because it creates new references; {!review} and
    {!remove} take only the connection row, which already carries an
    immutable pair.

    {b Race safety} is the partial unique index over
    [(LEAST(requester, recipient), GREATEST(requester, recipient))
     WHERE status IN ('pending','accepted')] and the status-guarded
    [UPDATE ... WHERE], never a read-before-write check. Two concurrent
    requests for the same pair — in the same direction or in opposite
    directions — resolve to exactly one active connection; the loser gets
    [Active_connection_exists] and writes nothing. A uniqueness conflict
    never surfaces raw SQL: it maps to that one closed variant.

    {b Authorization is not implemented here.} These functions take the
    acting user only as provenance and the acting community only as the
    subject boundary the SQL mutation verifies. Nothing checks moderator
    roles, membership, [users.is_admin], or community visibility — the
    handler slice owns all of that, and no value returned here grants
    anything.

    {b No eligibility policy} either: this slice enforces no
    network/published/public predicate on the two communities, because none
    has been decided yet. Only structural coherence of the durable rows is
    checked, so valid lifecycle drift on either side never welds a
    connection in place or hides it.

    Error privacy: every error is payload-free, every collapsible cause
    collapses into one variant, and no printer or serializer exists — the
    store cannot be used to probe communities, connections, or history. No
    tokens, OAuth or PKCE material, session bindings, or GitHub identifiers
    are read or written. Nothing is logged. *)

type decision =
  | Accept
  | Reject

type created_connection
(** Proof that the complete pending request committed. Carries only the
    local [community_connections.id]. *)

val created_connection_id : created_connection -> int64
(** The created connection's local row id — an internal identifier for later
    workflow wiring, not a bearer credential. *)

type reviewed_connection
(** Proof that the complete review committed. *)

val reviewed_connection_id : reviewed_connection -> int64

val reviewed_requester_community_id : reviewed_connection -> int

val reviewed_recipient_community_id : reviewed_connection -> int

val reviewed_status : reviewed_connection -> Community_connections.status
(** {!Community_connections.Accepted} or {!Community_connections.Rejected},
    matching the decision. *)

type removed_connection
(** Proof that the complete removal committed. Both community ids cross so
    the caller can address the other side without a second read. *)

val removed_connection_id : removed_connection -> int64

val removed_requester_community_id : removed_connection -> int

val removed_recipient_community_id : removed_connection -> int

val removed_status : removed_connection -> Community_connections.status
(** Always {!Community_connections.Removed}. *)

type error =
  | Invalid_user_id
  | Invalid_connection_id
  | Invalid_community_id
  | Invalid_connection
  | Community_unavailable
  | Active_connection_exists
  | Review_unavailable
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

val request :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  connection:Community_connections.t ->
  (created_connection, error) result Lwt.t
(** Creates exactly one [pending] connection from the requesting community
    to the recipient community named by the already-validated pure
    [connection] value.

    Self-connection cannot reach this function: {!Community_connections}
    refuses to build such a value, and the durable CHECK refuses such a row.
    A non-positive [actor_user_id] is [Invalid_user_id]; a [connection]
    whose status is not {!Community_connections.Pending} is
    [Invalid_connection] — an accepted, rejected, or removed value is never
    silently transitioned. The request note is taken only through
    {!Community_connections.request_note}, already canonical.

    [Community_unavailable] covers a missing community on either side alike,
    without saying which. [Active_connection_exists] deliberately collapses
    an existing pending request, an existing accepted connection, the same
    request arriving twice, the mirrored request arriving from the other
    side, and any concurrently committed winner — without identifying the
    status, the direction, or the winner. Historical rejected and removed
    rows fall outside the uniqueness predicate: after a rejection or a
    removal a fresh request succeeds, from either direction. *)

val review :
  (module Caqti_lwt.CONNECTION) ->
  reviewer_user_id:int ->
  connection_id:int64 ->
  recipient_community_id:int ->
  decision:decision ->
  (reviewed_connection, error) result Lwt.t
(** Accepts or rejects the exact still-pending connection [connection_id]
    whose recipient is [recipient_community_id].

    The recipient is verified inside the SQL mutation boundary — both in the
    locking [SELECT] and again in the guarded [UPDATE] — so a caller cannot
    review a request addressed to another community even if the row id is
    guessed, and a concurrent review or removal cannot be overwritten. The
    transition itself is the pure {!Community_connections.apply} over the
    value reconstructed from the locked row; the store never hand-rolls a
    second status machine.

    [Review_unavailable] collapses every zero-row cause alike: no such
    connection, another community's request, an already accepted, rejected,
    or removed row, and a concurrent review that committed first. Nothing is
    written in any of those cases, and no audit event is appended. *)

val remove :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  connection_id:int64 ->
  acting_community_id:int ->
  (removed_connection, error) result Lwt.t
(** Removes the exact accepted connection [connection_id] on behalf of
    [acting_community_id].

    A mutual connection is symmetric, so either side may remove it: the SQL
    mutation boundary requires [acting_community_id] to be one of the two
    communities on the locked row — in both the locking [SELECT] and the
    guarded [UPDATE] — and records the acting user in [removed_by_user_id].
    Which side acted stays visible only through the returned community ids
    and the audit event; no role, membership, or authority is checked here,
    and the returned value grants none.

    [Removal_unavailable] collapses every zero-row cause alike: no such
    connection, a still-pending request, a rejected or already removed row,
    a community that is not part of this connection, and a concurrent
    removal that committed first. Nothing is written in any of those cases,
    and no audit event is appended. The historical removed row stays behind,
    outside the active-pair uniqueness predicate, so a later fresh request
    between the same two communities is possible from either direction. *)
