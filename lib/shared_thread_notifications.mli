(** Durable in-app notification insertion for the shared-thread placement
    lifecycle.

    Structured rows only: a closed kind, the recipient, the actor (provenance,
    SET NULL on deletion), the recipient's own community as context, and the
    placement subject reference. No prose, no post link, no copied name or slug
    — rendering derives everything from current entity state at read time. This
    module runs on the enclosing store's already-open transaction and never
    starts, commits, or rolls back one, so a notification failure rolls the
    business mutation and its audit event back with it.

    Recipient policy lives in the store, not here: the store resolves top
    moderators, the requester, and the thread author under its held locks and
    assigns each recipient the community context of their own side — origin
    context for origin-side recipients, destination context for destination-side
    ones. This module only enforces the mechanical rules: the actor is never
    notified, each user receives at most one row per call (the first listed
    context wins, and stores list origin-side recipients first), and zero
    recipients is a legitimate no-op. *)

type kind =
  | Placement_requested
  | Placement_accepted
  | Placement_rejected
  | Placement_removed
  | Placement_withdrawn

type error = Inconsistent_data | Storage_error

val community_top_moderator_ids :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (int list, error) result Lwt.t
(** The exact ['top_mod'] user ids of one community, in ascending id order.
    ['mod'] and ['legacy_mod'] are not recipients, and [users.is_admin] is not
    consulted — a global admin is an authority, not a subscriber. Read without
    [FOR UPDATE]: the enclosing store's community and placement locks already
    serialize the lifecycle. An empty list is legitimate. *)

val insert_many :
  (module Caqti_lwt.CONNECTION) ->
  kind:kind ->
  actor_user_id:int ->
  placement_id:int64 ->
  recipients:(int * int) list ->
  (unit, error) result Lwt.t
(** Inserts one structured notification per surviving recipient on the caller's
    open transaction. Each element of [recipients] is [(user_id, community_id)]
    — the recipient and the community context of their own side. The actor is
    unconditionally excluded; duplicate users collapse to their first listed
    occurrence; an empty remainder inserts nothing and succeeds. Every inserted
    row is revalidated through RETURNING. The caller must already have validated
    and locked every subject; a non-positive id here is [Inconsistent_data], and
    every Caqti error is dropped payload-free ([Storage_error]). *)
