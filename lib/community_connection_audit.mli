(** Transaction-local, append-only audit recording for the community-connection
    lifecycle.

    One durable row per successfully committed lifecycle transition, in the
    [community_connection_audit_events] table: a closed action, the acting user,
    and the exact durable subject rows (the connection and both communities) as
    internal foreign keys plus a database timestamp — nothing else. No request
    note, submitted text, community names or slugs, moderator roles,
    authorization provenance, tokens, session values, or diagnostics are ever
    written.

    {!insert} is deliberately transaction-local: it runs on the caller's open
    connection, after the enclosing store's business mutation has succeeded and
    returned validated ids, and before that store commits. It never opens,
    commits, or rolls back a transaction itself — the enclosing store owns the
    boundary, so a failed audit insertion rolls the whole business mutation back
    and a rolled-back mutation leaves no event behind.

    Error privacy: both errors are payload-free and no printer or serializer
    exists. The internal row ids stay between the store and this module; they
    never reach handler or page APIs. *)

type action =
  | Connection_requested
  | Connection_accepted
  | Connection_rejected
  | Connection_removed

val string_of_action : action -> string
(** The exact database value, byte-for-byte the table's CHECK vocabulary;
    round-trips with {!action_of_string}. Exposed so the read side shares one
    vocabulary with the write side instead of restating it. *)

val action_of_string : string -> action option
(** Accepts exactly the four canonical database spellings and nothing else: no
    aliases, capitalization variants, padding, or abbreviations. A stored value
    outside them is corrupt data, never a fifth action. *)

type error = Inconsistent_data | Storage_error

val insert :
  (module Caqti_lwt.CONNECTION) ->
  action:action ->
  actor_user_id:int ->
  connection_id:int64 ->
  requester_community_id:int ->
  recipient_community_id:int ->
  (unit, error) result Lwt.t
(** Appends exactly one audit event on the caller's open transaction.

    Every id must already be validated and locked by the enclosing store; a
    non-positive id, or two equal community ids, is [Inconsistent_data] — it can
    only mean the caller skipped its own validation. The two communities are
    recorded in the connection's own requester/recipient order, never re-sorted,
    so the event stays readable from either side without joining the connection
    row. The returned row is revalidated (positive id, byte-identical action)
    before success. Any database failure — including a foreign key refused
    because a subject row vanished outside the locked protocol — is
    [Storage_error]. On either error the caller must roll back; this module
    never commits on its behalf. *)
