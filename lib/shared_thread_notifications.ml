(* Durable in-app notification insertion for the shared-thread placement
   lifecycle, always inside the enclosing store's already-open transaction —
   this module never starts, commits, or rolls back one, so notification
   failure and business failure share exactly one atomic outcome. The kind
   vocabulary is closed on both sides: the variant here and the shape CHECK
   on notifications; no caller-supplied string ever reaches the column. See
   the .mli for the full contract.

   The top-moderator query is deliberately a third copy of the one in
   Project_home_notifications and Community_connection_notifications rather
   than a call into either: the features are independent, their error types
   are distinct, and a dependency between lifecycles would exist only to
   share four lines of SQL.

   Unlike the sibling modules, a recipient here carries its own community
   context: a removal notifies both sides at once, and each recipient's
   notifications.community_id must be their own community — origin context
   for origin-side recipients (top moderators, the requester, the thread
   author), destination context for destination-side ones. The enclosing
   store builds that assignment; this module only deduplicates it. *)

open Lwt.Infix

type kind =
  | Placement_requested
  | Placement_accepted
  | Placement_rejected
  | Placement_removed
  | Placement_withdrawn

type error = Inconsistent_data | Storage_error

(* Private: the durable vocabulary, byte-for-byte the table's CHECK. *)
let string_of_kind = function
  | Placement_requested -> "shared_thread_requested"
  | Placement_accepted -> "shared_thread_accepted"
  | Placement_rejected -> "shared_thread_rejected"
  | Placement_removed -> "shared_thread_removed"
  | Placement_withdrawn -> "shared_thread_withdrawn"

(* === recipient resolution === *)

(* A plain bounded read over the durable role rows, deliberately without
   FOR UPDATE: the enclosing store already holds both community row locks
   and the placement row lock that serialize the lifecycle, and locking
   role rows here would extend that shared lock protocol with an order no
   sibling store follows. A role change racing past this read converges on
   the next lifecycle event, exactly like the audit trail.

   The role comparison is exact: 'mod' and 'legacy_mod' are not recipients,
   and users.is_admin is not consulted at all — a global admin is an
   authority, not a subscriber. *)
let top_moderators_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.int)
    "SELECT user_id FROM community_moderators WHERE community_id = $1 AND role \
     = 'top_mod' ORDER BY user_id"

let community_top_moderator_ids (module C : Caqti_lwt.CONNECTION) ~community_id
    =
  if community_id <= 0 then Lwt.return (Error Inconsistent_data)
  else
    C.collect_list top_moderators_query community_id >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok rows ->
        if List.for_all (fun id -> id > 0) rows then Lwt.return (Ok rows)
        else Lwt.return (Error Inconsistent_data)

(* === insertion === *)

(* The structured shared-thread row shape: no message, no post link, no
   project, no home relation and no connection — the shape CHECK enforces
   exactly this. The canonical post is derived from the placement at read
   time, never stored twice. RETURNING id, notif_type, and user_id lets the
   caller-side revalidation prove each row landed byte-exactly for the
   intended recipient. *)
let insert_notification_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 int string int) (t2 int int64))
  ->? Caqti_type.(t3 int string int))
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     community_id, shared_thread_placement_id) VALUES ($1, $2, $3, $4, $5) \
     RETURNING id, notif_type, user_id"

let insert_many (module C : Caqti_lwt.CONNECTION) ~kind ~actor_user_id
    ~placement_id ~recipients =
  let kind_string = string_of_kind kind in
  if
    actor_user_id <= 0
    || Int64.compare placement_id 0L <= 0
    || List.exists
         (fun (user_id, community_id) -> user_id <= 0 || community_id <= 0)
         recipients
  then
    (* The enclosing store validated and locked every subject id, and
       resolved every recipient from durable rows, before calling; an
       invalid one here is caller-side corruption, never an input error to
       report outward. *)
    Lwt.return (Error Inconsistent_data)
  else
    (* The actor is unconditionally dropped — no mutation may notify its own
       actor, whichever resolution path produced the id — and each user
       collapses to exactly one row, keeping the FIRST context the caller
       assigned: stores list origin-side recipients first, so a user
       reachable through both communities is notified once, in origin
       context. The durable (user, kind, placement) unique index enforces
       the same collapse. An empty remainder is a legitimate no-op — zero
       recipients never fails the business transition. *)
    let recipients =
      List.rev
        (List.fold_left
           (fun acc (user_id, community_id) ->
             if
               user_id = actor_user_id
               || List.exists (fun (seen, _) -> seen = user_id) acc
             then acc
             else (user_id, community_id) :: acc)
           [] recipients)
    in
    let rec insert_all = function
      | [] -> Lwt.return (Ok ())
      | (recipient, community_id) :: rest -> (
          (* As in the sibling stores, every Caqti error is dropped
             payload-free — error payloads can echo SQL parameters. *)
          C.find_opt insert_notification_query
            ( (recipient, kind_string, actor_user_id),
              (community_id, placement_id) )
          >>= function
          | Error _ -> Lwt.return (Error Storage_error)
          | Ok None ->
              (* A plain INSERT ... RETURNING cannot return zero rows without
                 an interfering rule or trigger. *)
              Lwt.return (Error Inconsistent_data)
          | Ok (Some (row_id, stored_kind, stored_recipient)) ->
              if
                row_id > 0
                && String.equal stored_kind kind_string
                && stored_recipient = recipient
              then insert_all rest
              else Lwt.return (Error Inconsistent_data))
    in
    insert_all recipients
