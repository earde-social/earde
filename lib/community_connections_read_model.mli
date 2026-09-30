(** Read-only access to community↔community mutual connections and their audit
    history. This feature module owns its SQL, takes no locks, writes nothing,
    performs no network IO, and logs nothing.

    {b It decides nothing about who may see what.} Every function answers for
    the community ids or connection id it is given; the caller completes its own
    authorization first. In particular {!connection} carries [request_note],
    which is private workflow text between the two moderator teams — never
    public content — and the acting user ids, which are provenance. A rendering
    surface must decide deliberately what of that it shows, and must HTML-escape
    whatever it does.

    Symmetry is real: an accepted connection belongs to both communities
    equally, so {!list_accepted} returns it from either side, and the
    requester/recipient split it carries records only who asked and who
    reviewed.

    Every returned row is validated against the same invariants the durable
    CHECK constraints hold — closed status vocabulary, positive distinct
    community ids, a canonical note, and the per-status timestamp/actor shape. A
    row that fails is [Inconsistent_data] for the whole call, never a silently
    repaired value and never a partial list.

    Error privacy: the error variant is payload-free and no printer or
    serializer exists; Caqti/PostgreSQL details (which can echo SQL parameters)
    are dropped, never returned or logged. *)

type connection = {
  id : int64;
  requester_community_id : int;
  recipient_community_id : int;
  status : Community_connections.status;
  request_note : string option;
      (** Private workflow text, canonical (LF line endings, outer-trimmed,
          never [Some ""]). Present on every status, since the historical rows
          keep the note they were requested with. *)
  requested_by_user_id : int option;
  reviewed_by_user_id : int option;
  removed_by_user_id : int option;
      (** Provenance only, and [None] once that account is deleted — the
          connection and its history survive the actor. *)
  created_at : string;
  updated_at : string;
  reviewed_at : string option;
  removed_at : string option;
}

type audit_event = {
  event_id : int64;
  event_action : Community_connection_audit.action;
  event_actor_user_id : int option;
  event_connection_id : int64;
  event_requester_community_id : int;
  event_recipient_community_id : int;
  event_created_at : string;
}

type error =
  | Invalid_connection_id
  | Invalid_community_id
  | Inconsistent_data
  | Storage_error

val load :
  (module Caqti_lwt.CONNECTION) ->
  connection_id:int64 ->
  (connection option, error) result Lwt.t
(** The connection with this exact row id, in any status. [None] is simply
    absence — no distinction is drawn between never-existed and deleted. *)

val active_for_pair :
  (module Caqti_lwt.CONNECTION) ->
  community_a:int ->
  community_b:int ->
  (connection option, error) result Lwt.t
(** The single active ([pending] or [accepted]) connection between the two
    communities, in either direction — the durable partial unique index
    guarantees there is at most one, and finding more than one is
    [Inconsistent_data]. The arguments are unordered and interchangeable; two
    equal ids are [Invalid_community_id], since no community connects to itself.
    Historical rejected and removed rows are never returned here: their pair is
    free for a fresh request. *)

val list_accepted :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (connection list, error) result Lwt.t
(** Every accepted connection this community is part of, from either side,
    newest first. Pending, rejected, and removed rows are excluded. *)

val list_incoming_pending :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (connection list, error) result Lwt.t
(** Pending requests addressed to this community — its review queue — in arrival
    order, oldest first. *)

val list_outgoing_pending :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (connection list, error) result Lwt.t
(** Pending requests this community sent and is awaiting review on, in arrival
    order, oldest first. *)

val list_audit_events :
  (module Caqti_lwt.CONNECTION) ->
  connection_id:int64 ->
  (audit_event list, error) result Lwt.t
(** The append-only audit trail of one connection, in append order. Empty for a
    connection id that never existed — the audit table is not an existence
    oracle, and the events carry no note or prose. *)
