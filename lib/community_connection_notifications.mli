(** Transaction-local, durable in-app notification insertion for the
    community↔community connection lifecycle.

    One notifications row per (recipient, kind, connection): a closed kind,
    the recipient, the acting user, and exactly two durable subject rows —
    the recipient's own management-context community and the connection —
    as internal foreign keys. Nothing else. No request note, actor username,
    copied community name or slug, rendered prose, URL, decision text,
    authorization provenance, or SQL diagnostic is ever written; rendering
    derives entirely from current entity state at read time, and the
    counterpart community is derived there from the stored context community
    rather than stored twice.

    Everything here is deliberately transaction-local: it runs on the
    caller's open connection, after the enclosing store's guarded lifecycle
    mutation has succeeded and its audit event has been inserted, and before
    that store commits. This module never opens, commits, or rolls back a
    transaction itself — the enclosing store owns the boundary, so a failed
    notification insertion rolls the whole mutation (and its audit event)
    back, and a rolled-back or stale-losing mutation leaves neither an audit
    event nor a notification behind.

    It sends no email and makes no external call, so nothing here can block
    or fail a transaction on the network.

    Error privacy: both errors are payload-free and no printer or serializer
    exists. The internal row ids stay between the store and this module; they
    never reach handler or page APIs. *)

type kind =
  | Connection_requested
  | Connection_accepted
  | Connection_rejected
  | Connection_removed

type error =
  | Inconsistent_data
  | Storage_error

val community_top_moderator_ids :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (int list, error) result Lwt.t
(** All current ['top_mod'] users of one community, resolved from durable
    rows on the caller's open transaction — never from handler or form
    input, and never from a session claim.

    Exactly [role = 'top_mod']: an ordinary ['mod'] or a ['legacy_mod'] is
    not a recipient, and global admins are never broadcast to. Plain reads,
    no row locks: the enclosing store already holds both community row locks
    and the connection row lock, and these reads must never extend the shared
    lock protocol. Callers must resolve recipients only after their locks are
    held and the transition has committed its guarded UPDATE. A non-positive
    id in the result set is [Inconsistent_data]; an empty result is [Ok []]
    and a legitimate outcome. *)

val insert_many :
  (module Caqti_lwt.CONNECTION) ->
  kind:kind ->
  actor_user_id:int ->
  community_id:int ->
  connection_id:int64 ->
  recipient_user_ids:int list ->
  (unit, error) result Lwt.t
(** Inserts exactly one durable notification per distinct recipient on the
    caller's open transaction.

    [community_id] is the {i recipient's} management-context community — the
    one whose connections surface the rendered notification links to, and the
    one the read model derives the counterpart relative to. It is never the
    counterpart community and never the actor's community when those differ.

    Recipient ids are deduplicated internally and the actor is always
    excluded, so a mutation can never notify its own actor and a user who
    holds ['top_mod'] on both sides receives at most one row per applicable
    kind. An empty (or actor-only) recipient set is a successful no-op: zero
    recipients never fails a business transition.

    Every id must already be validated and locked by the enclosing store; a
    non-positive id here — including any recipient — is [Inconsistent_data],
    because it can only mean the caller skipped its own validation or
    resolution. Each inserted row is revalidated (positive id, byte-identical
    kind, exact recipient) before success. Any database failure — including a
    duplicate refused by the per-(recipient, kind, connection) unique index,
    which is impossible under the locked protocol — is [Storage_error]. On
    either error the caller must roll back; this module never commits on its
    behalf. *)
