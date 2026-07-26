(* Append-only audit insertion for the project/community-home lifecycle,
   always inside the enclosing store's already-open transaction — this
   module never starts, commits, or rolls back one, so audit failure and
   business failure share exactly one atomic outcome. The action
   vocabulary is closed on both sides: the variant here and the CHECK
   constraint on project_home_audit_events; no caller-supplied string
   ever reaches the column. See the .mli for the full contract. *)

open Lwt.Infix

type action =
  | Home_requested
  | Home_accepted
  | Home_rejected
  | Home_removed
  | Dedicated_home_provisioned
  | Network_community_published

type error =
  | Inconsistent_data
  | Storage_error

(* Private: the durable vocabulary, byte-for-byte the table's CHECK. *)
let string_of_action = function
  | Home_requested -> "project_home_requested"
  | Home_accepted -> "project_home_accepted"
  | Home_rejected -> "project_home_rejected"
  | Home_removed -> "project_home_removed"
  | Dedicated_home_provisioned -> "dedicated_home_provisioned"
  | Network_community_published -> "network_community_published"

(* created_at rides the database default so the event timestamp comes
   from the same clock as the business row it describes. RETURNING both
   id and action lets the caller-side revalidation prove the row landed
   byte-exactly. *)
let insert_event_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string int) (t3 int64 int int64))
   ->? Caqti_type.(t2 int64 string))
  "INSERT INTO project_home_audit_events \
     (action, actor_user_id, project_id, community_id, relation_id) \
   VALUES ($1, $2, $3, $4, $5) \
   RETURNING id, action"

let insert (module C : Caqti_lwt.CONNECTION) ~action ~actor_user_id
    ~project_id ~community_id ~relation_id =
  let action_string = string_of_action action in
  if
    actor_user_id <= 0
    || Int64.compare project_id 0L <= 0
    || community_id <= 0
    || Int64.compare relation_id 0L <= 0
  then
    (* The enclosing store validated and locked every id before calling;
       an invalid one here is caller-side corruption, never an input
       error to report outward. *)
    Lwt.return (Error Inconsistent_data)
  else
    (* As in the sibling stores, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt insert_event_query
      ((action_string, actor_user_id), (project_id, community_id, relation_id))
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None ->
        (* A plain INSERT ... RETURNING cannot return zero rows without
           an interfering rule or trigger. *)
        Lwt.return (Error Inconsistent_data)
    | Ok (Some (event_id, stored_action)) ->
        if
          Int64.compare event_id 0L > 0
          && String.equal stored_action action_string
        then Lwt.return (Ok ())
        else Lwt.return (Error Inconsistent_data)
