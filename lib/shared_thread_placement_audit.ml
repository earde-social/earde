(* Append-only audit insertion for the shared-thread placement lifecycle,
   always inside the enclosing store's already-open transaction — this
   module never starts, commits, or rolls back one, so audit failure and
   business failure share exactly one atomic outcome. The action vocabulary
   is closed on both sides: the variant here and the CHECK constraint on
   shared_thread_placement_audit_events; no caller-supplied string ever
   reaches the column. See the .mli for the full contract. *)

open Lwt.Infix

type action =
  | Placement_requested
  | Placement_accepted
  | Placement_rejected
  | Placement_removed
  | Placement_withdrawn

type error =
  | Inconsistent_data
  | Storage_error

(* The durable vocabulary, byte-for-byte the table's CHECK. Shared with the
   read side so the five spellings exist in exactly one place. *)
let string_of_action = function
  | Placement_requested -> "shared_thread_requested"
  | Placement_accepted -> "shared_thread_accepted"
  | Placement_rejected -> "shared_thread_rejected"
  | Placement_removed -> "shared_thread_removed"
  | Placement_withdrawn -> "shared_thread_withdrawn"

(* created_at rides the database default so the event timestamp comes from
   the same clock as the business row it describes. RETURNING both id and
   action lets the caller-side revalidation prove the row landed
   byte-exactly. *)
let insert_event_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string int int64) (t3 int int int))
   ->? Caqti_type.(t2 int64 string))
  "INSERT INTO shared_thread_placement_audit_events \
     (action, actor_user_id, placement_id, post_id, \
      origin_community_id, destination_community_id) \
   VALUES ($1, $2, $3, $4, $5, $6) \
   RETURNING id, action"

let insert (module C : Caqti_lwt.CONNECTION) ~action ~actor_user_id
    ~placement_id ~post_id ~origin_community_id ~destination_community_id =
  let action_string = string_of_action action in
  if
    actor_user_id <= 0
    || Int64.compare placement_id 0L <= 0
    || post_id <= 0
    || origin_community_id <= 0
    || destination_community_id <= 0
    || origin_community_id = destination_community_id
  then
    (* The enclosing store validated and locked every id before calling; an
       invalid one — or a same-community pair the durable CHECK cannot hold —
       is caller-side corruption, never an input error to report outward. *)
    Lwt.return (Error Inconsistent_data)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free
       — error payloads can echo SQL parameters. *)
    C.find_opt insert_event_query
      ( (action_string, actor_user_id, placement_id),
        (post_id, origin_community_id, destination_community_id) )
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None ->
        (* A plain INSERT ... RETURNING cannot return zero rows without an
           interfering rule or trigger. *)
        Lwt.return (Error Inconsistent_data)
    | Ok (Some (event_id, stored_action)) ->
        if
          Int64.compare event_id 0L > 0
          && String.equal stored_action action_string
        then Lwt.return (Ok ())
        else Lwt.return (Error Inconsistent_data)
