(* Durable in-app notification insertion for the project/community-home
   lifecycle, always inside the enclosing store's already-open
   transaction — this module never starts, commits, or rolls back one,
   so notification failure and business failure share exactly one atomic
   outcome. The kind vocabulary is closed on both sides: the variant
   here and the shape CHECK on notifications; no caller-supplied string
   ever reaches the column. See the .mli for the full contract. *)

open Lwt.Infix

type kind = Home_requested | Home_accepted | Home_rejected | Home_removed
type error = Inconsistent_data | Storage_error

(* Private: the durable vocabulary, byte-for-byte the table's CHECK. *)
let string_of_kind = function
  | Home_requested -> "project_home_requested"
  | Home_accepted -> "project_home_accepted"
  | Home_rejected -> "project_home_rejected"
  | Home_removed -> "project_home_removed"

(* === recipient resolution === *)

(* Plain bounded reads over the durable role rows, deliberately without
   FOR UPDATE: the enclosing stores already hold the project/community
   row locks that serialize the lifecycle, and locking role rows here
   would extend the shared lock protocol (the review and removal stores
   lock exactly one authorization row each) with an order no sibling
   store follows. A role change racing past these reads converges on
   the next lifecycle event, exactly like the audit trail. *)
let top_moderators_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.int)
    "SELECT user_id FROM community_moderators WHERE community_id = $1 AND role \
     = 'top_mod' ORDER BY user_id"

let stewards_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->* Caqti_type.int)
    "SELECT user_id FROM project_stewards WHERE project_id = $1 AND role = \
     'steward' ORDER BY user_id"

let validated_ids rows =
  if List.for_all (fun id -> id > 0) rows then Ok rows
  else Error Inconsistent_data

let community_top_moderator_ids (module C : Caqti_lwt.CONNECTION) ~community_id
    =
  if community_id <= 0 then Lwt.return (Error Inconsistent_data)
  else
    C.collect_list top_moderators_query community_id >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok rows -> Lwt.return (validated_ids rows)

let project_steward_ids (module C : Caqti_lwt.CONNECTION) ~project_id =
  if Int64.compare project_id 0L <= 0 then Lwt.return (Error Inconsistent_data)
  else
    C.collect_list stewards_query project_id >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok rows -> Lwt.return (validated_ids rows)

(* === insertion === *)

(* The structured project-home row shape: no message, no post link —
   the shape CHECK enforces exactly this. RETURNING id, notif_type, and
   user_id lets the caller-side revalidation prove each row landed
   byte-exactly for the intended recipient. *)
let insert_notification_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 int string int) (t3 int64 int int64))
  ->? Caqti_type.(t3 int string int))
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     project_id, community_id, relation_id) VALUES ($1, $2, $3, $4, $5, $6) \
     RETURNING id, notif_type, user_id"

let insert_many (module C : Caqti_lwt.CONNECTION) ~kind ~actor_user_id
    ~project_id ~community_id ~relation_id ~recipient_user_ids =
  let kind_string = string_of_kind kind in
  if
    actor_user_id <= 0
    || Int64.compare project_id 0L <= 0
    || community_id <= 0
    || Int64.compare relation_id 0L <= 0
    || List.exists (fun id -> id <= 0) recipient_user_ids
  then
    (* The enclosing store validated and locked every subject id, and
       resolved every recipient from durable rows, before calling; an
       invalid one here is caller-side corruption, never an input error
       to report outward. *)
    Lwt.return (Error Inconsistent_data)
  else
    (* Deduplicated defensively, and the actor unconditionally dropped:
       no mutation may notify its own actor, whichever resolution path
       produced the id. An empty remainder is a legitimate no-op — zero
       recipients never fails the business transition. *)
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
            ( (recipient, kind_string, actor_user_id),
              (project_id, community_id, relation_id) )
          >>= function
          | Error _ -> Lwt.return (Error Storage_error)
          | Ok None ->
              (* A plain INSERT ... RETURNING cannot return zero rows
                 without an interfering rule or trigger. *)
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
