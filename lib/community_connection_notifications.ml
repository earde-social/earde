(* Durable in-app notification insertion for the community↔community
   connection lifecycle, always inside the enclosing store's already-open
   transaction — this module never starts, commits, or rolls back one, so
   notification failure and business failure share exactly one atomic
   outcome. The kind vocabulary is closed on both sides: the variant here
   and the shape CHECK on notifications; no caller-supplied string ever
   reaches the column. See the .mli for the full contract.

   The top-moderator query is deliberately a second copy of the one in
   Project_home_notifications rather than a call into it: the two features
   are independent, their error types are distinct, and a dependency from
   the connection lifecycle onto the project-home lifecycle would exist
   only to share four lines of SQL. *)

open Lwt.Infix

type kind =
  | Connection_requested
  | Connection_accepted
  | Connection_rejected
  | Connection_removed

type error =
  | Inconsistent_data
  | Storage_error

(* Private: the durable vocabulary, byte-for-byte the table's CHECK. *)
let string_of_kind = function
  | Connection_requested -> "community_connection_requested"
  | Connection_accepted -> "community_connection_accepted"
  | Connection_rejected -> "community_connection_rejected"
  | Connection_removed -> "community_connection_removed"

(* === recipient resolution === *)

(* A plain bounded read over the durable role rows, deliberately without
   FOR UPDATE: the enclosing store already holds both community row locks
   and the connection row lock that serialize the lifecycle, and locking
   role rows here would extend that shared lock protocol with an order no
   sibling store follows. A role change racing past this read converges on
   the next lifecycle event, exactly like the audit trail.

   The role comparison is exact: 'mod' and 'legacy_mod' are not recipients,
   and users.is_admin is not consulted at all — a global admin is an
   authority, not a subscriber. *)
let top_moderators_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.int)
  "SELECT user_id FROM community_moderators \
   WHERE community_id = $1 AND role = 'top_mod' \
   ORDER BY user_id"

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

(* The structured community-connection row shape: no message, no post link,
   no project and no home relation — the shape CHECK enforces exactly this.
   RETURNING id, notif_type, and user_id lets the caller-side revalidation
   prove each row landed byte-exactly for the intended recipient. *)
let insert_notification_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 int string int) (t2 int int64))
   ->? Caqti_type.(t3 int string int))
  "INSERT INTO notifications \
     (user_id, notif_type, actor_user_id, community_id, connection_id) \
   VALUES ($1, $2, $3, $4, $5) \
   RETURNING id, notif_type, user_id"

let insert_many (module C : Caqti_lwt.CONNECTION) ~kind ~actor_user_id
    ~community_id ~connection_id ~recipient_user_ids =
  let kind_string = string_of_kind kind in
  if
    actor_user_id <= 0 || community_id <= 0
    || Int64.compare connection_id 0L <= 0
    || List.exists (fun id -> id <= 0) recipient_user_ids
  then
    (* The enclosing store validated and locked every subject id, and
       resolved every recipient from durable rows, before calling; an
       invalid one here is caller-side corruption, never an input error to
       report outward. *)
    Lwt.return (Error Inconsistent_data)
  else
    (* Deduplicated defensively, and the actor unconditionally dropped: no
       mutation may notify its own actor, whichever resolution path produced
       the id, and a user who is top_mod on both sides collapses to one row.
       An empty remainder is a legitimate no-op — zero recipients never
       fails the business transition. *)
    let recipients =
      List.filter
        (fun id -> id <> actor_user_id)
        (List.sort_uniq compare recipient_user_ids)
    in
    let rec insert_all = function
      | [] -> Lwt.return (Ok ())
      | recipient :: rest -> (
          (* As in the sibling stores, every Caqti error is dropped
             payload-free — error payloads can echo SQL parameters. *)
          C.find_opt insert_notification_query
            ((recipient, kind_string, actor_user_id), (community_id, connection_id))
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
