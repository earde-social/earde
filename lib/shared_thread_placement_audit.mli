(** Append-only audit insertion for the shared-thread placement lifecycle.

    One event per successfully committed durable transition, written by the
    transactional store {i inside its own already-open transaction}: this
    module never starts, commits, or rolls back one, so a committed
    placement change without its event — or an event describing a
    rolled-back change — cannot exist.

    The event body is foreign keys plus a closed action vocabulary and a
    database-clock timestamp; nothing else. No request note, post title,
    community name or slug, moderator role, authorization provenance,
    token, session value, or SQL diagnostic is ever stored or returned.
    [mod_actions] is deliberately not reused (moderator-only semantics, an
    INTEGER target that cannot address BIGINT placement rows, and cascading
    actor deletion that audit must survive). *)

type action =
  | Placement_requested
  | Placement_accepted
  | Placement_rejected
  | Placement_removed
  | Placement_withdrawn

val string_of_action : action -> string
(** The exact durable spelling, byte-for-byte the table's CHECK:
    [shared_thread_requested], [shared_thread_accepted],
    [shared_thread_rejected], [shared_thread_removed],
    [shared_thread_withdrawn]. *)

val action_of_string : string -> action option
(** Closed inverse of {!string_of_action}; anything else is [None]. Shared
    with future read surfaces so the five spellings exist in exactly one
    place. *)

type error =
  | Inconsistent_data
      (** A caller-side invariant did not hold (non-positive id, equal
          communities) or the inserted row did not land byte-exactly. The
          enclosing store treats this as corruption and rolls back. *)
  | Storage_error

val insert :
  (module Caqti_lwt.CONNECTION) ->
  action:action ->
  actor_user_id:int ->
  placement_id:int64 ->
  post_id:int ->
  origin_community_id:int ->
  destination_community_id:int ->
  (unit, error) result Lwt.t
(** Appends exactly one event on the caller's open transaction and
    revalidates the RETURNING pair. The caller must already have validated
    and locked every subject; this function only refuses obviously corrupt
    input ([Inconsistent_data]) and never authorizes anything. Every Caqti
    error is dropped payload-free ([Storage_error]). *)
