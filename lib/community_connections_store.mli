(** Transactional writes for the mutual connection between two Earde
    communities: request, accept, reject, remove.

    Each call is one explicit transaction. Every successful mutation writes
    exactly one {!Community_connection_audit} event and its
    {!Community_connection_notifications} rows on that same transaction, so a
    committed connection change without its event — or an event without its
    change, or a notification without either — cannot exist. Every failure rolls
    back whole: no partial row, no orphan event, no notification, no timestamp
    left behind. A stale or losing concurrent transition fails its guard before
    reaching either, so it creates neither.

    {b Notification recipients} are computed here, at mutation time, from
    current durable [community_moderators] rows under the held locks — never
    from handler input, a session claim, or a snapshot taken earlier. A
    {!request} notifies the recipient community's exact ['top_mod']s; a
    {!review} notifies the requesting community's; a {!remove} notifies those of
    whichever community did not act. The acting user is always excluded and the
    set is deduplicated, so no one is notified of their own act and a user
    holding ['top_mod'] on both sides receives at most one row. Zero recipients
    is a legitimate outcome and never fails the transition. No email is sent and
    no external call is made inside the transaction.

    {b Lock order.} There is exactly one order and every function follows it
    whole: the two [communities] rows in ascending id order ([FOR SHARE]), then
    the exact [community_connections] row ([FOR UPDATE]). [FOR SHARE] — rather
    than the weaker [FOR KEY SHARE] — because the eligibility decision below
    reads visibility, onboarding state and discoverability, and only [FOR SHARE]
    conflicts with the row lock a concurrent visibility or publication change
    takes; it also blocks deletion of a referenced community. [FOR SHARE] locks
    are mutually compatible, so two connection transactions sharing a community
    never wait on each other and no ordering cycle can form among them.

    {!review} and {!remove} do not know the pair before they have seen the row,
    so each begins with one {b unlocked} read of that row's two community ids,
    purely to decide the lock order. That read is never the race-safety
    mechanism: the guarded [UPDATE] re-verifies status and subject under the row
    lock, and a row reviewed or removed in between simply fails the guard. The
    locked row's pair is additionally checked against the discovered one — those
    two columns are immutable, so a difference is durable corruption, not a
    race.

    {b Race safety} is the partial unique index over
    [(LEAST(requester, recipient), GREATEST(requester, recipient)) WHERE status
     IN ('pending','accepted')] and the status-guarded [UPDATE ... WHERE], never
    a read-before-write check. Two concurrent requests for the same pair — in
    the same direction or in opposite directions — resolve to exactly one active
    connection; the loser gets [Active_connection_exists] and writes nothing. A
    uniqueness conflict never surfaces raw SQL: it maps to that one closed
    variant.

    {b Authorization is not implemented here.} These functions take the acting
    user only as provenance and the acting community only as the subject
    boundary the SQL mutation verifies. Nothing checks moderator roles,
    membership, [users.is_admin], or community visibility — the handler slice
    owns all of that, and no value returned here grants anything.

    {b Eligibility} is the one durable policy this module does enforce, and only
    where creating a connection is at stake. {!request} and an accepting
    {!review} require both communities to satisfy
    {!Community_connections.connection_eligible} — revalidated here, under the
    held locks, because the search and confirmation surfaces decided on an older
    snapshot. A rejecting {!review} and {!remove} deliberately require nothing:
    an ineligible community must still be able to close a pending request and
    detach an accepted connection, or going private would weld its connections
    in place.

    Error privacy: every error is payload-free, every collapsible cause
    collapses into one variant, and no printer or serializer exists — the store
    cannot be used to probe communities, connections, or history. No tokens,
    OAuth or PKCE material, session bindings, or GitHub identifiers are read or
    written. Nothing is logged. *)

type decision = Accept | Reject

type created_connection
(** Proof that the complete pending request committed. Carries only the local
    [community_connections.id]. *)

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
(** Proof that the complete removal committed. Both community ids cross so the
    caller can address the other side without a second read. *)

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
  | Requester_ineligible
      (** The requesting community may not currently create a connection. *)
  | Recipient_ineligible
      (** The recipient community may not currently take part in creating one.
          Callers rendering for the other side must collapse this into their
          generic "target unavailable" outcome: which of missing, private,
          draft, or undiscoverable it was must not cross. *)
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
(** Creates exactly one [pending] connection from the requesting community to
    the recipient community named by the already-validated pure [connection]
    value.

    Self-connection cannot reach this function: {!Community_connections} refuses
    to build such a value, and the durable CHECK refuses such a row. A
    non-positive [actor_user_id] is [Invalid_user_id]; a [connection] whose
    status is not {!Community_connections.Pending} is [Invalid_connection] — an
    accepted, rejected, or removed value is never silently transitioned. The
    request note is taken only through {!Community_connections.request_note},
    already canonical.

    [Community_unavailable] covers a missing community on either side alike,
    without saying which. [Requester_ineligible] and [Recipient_ineligible]
    report the two eligibility failures separately so the caller can speak about
    the community whose moderators are asking without describing the other one.
    [Active_connection_exists] deliberately collapses an existing pending
    request, an existing accepted connection, the same request arriving twice,
    the mirrored request arriving from the other side, and any concurrently
    committed winner — without identifying the status, the direction, or the
    winner. Historical rejected and removed rows fall outside the uniqueness
    predicate: after a rejection or a removal a fresh request succeeds, from
    either direction. *)

val review :
  (module Caqti_lwt.CONNECTION) ->
  reviewer_user_id:int ->
  connection_id:int64 ->
  recipient_community_id:int ->
  decision:decision ->
  (reviewed_connection, error) result Lwt.t
(** Accepts or rejects the exact still-pending connection [connection_id] whose
    recipient is [recipient_community_id].

    The recipient is verified inside the SQL mutation boundary — both in the
    locking [SELECT] and again in the guarded [UPDATE] — so a caller cannot
    review a request addressed to another community even if the row id is
    guessed, and a concurrent review or removal cannot be overwritten. The
    transition itself is the pure {!Community_connections.apply} over the value
    reconstructed from the locked row; the store never hand-rolls a second
    status machine.

    [Review_unavailable] collapses every zero-row cause alike: no such
    connection, another community's request, an already accepted, rejected, or
    removed row, and a concurrent review that committed first. Nothing is
    written in any of those cases, and no audit event is appended.

    Accepting additionally requires both communities to be currently eligible,
    checked under the held locks: [Recipient_ineligible] first (the reviewing
    community — the one its own moderators may hear about), then
    [Requester_ineligible]. Rejecting reaches neither check and stays available
    on an ineligible pair, so a request can always be closed. *)

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
    communities on the locked row — in both the locking [SELECT] and the guarded
    [UPDATE] — and records the acting user in [removed_by_user_id]. Which side
    acted stays visible only through the returned community ids and the audit
    event; no role, membership, or authority is checked here, and the returned
    value grants none.

    [Removal_unavailable] collapses every zero-row cause alike: no such
    connection, a still-pending request, a rejected or already removed row, a
    community that is not part of this connection, and a concurrent removal that
    committed first. Nothing is written in any of those cases, and no audit
    event is appended. The historical removed row stays behind, outside the
    active-pair uniqueness predicate, so a later fresh request between the same
    two communities is possible from either direction. *)
