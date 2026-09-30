(* Read-only access to community↔community mutual connections and their
   audit history. One shared decoder validates every row against the same
   invariants the durable CHECK constraints hold, so no caller has to
   re-derive them and no half-validated row escapes. No locks, no writes, no
   authorization. See the .mli for the full contract. *)

open Lwt.Infix
module Cc = Community_connections
module Audit = Community_connection_audit

type connection = {
  id : int64;
  requester_community_id : int;
  recipient_community_id : int;
  status : Cc.status;
  request_note : string option;
  requested_by_user_id : int option;
  reviewed_by_user_id : int option;
  removed_by_user_id : int option;
  created_at : string;
  updated_at : string;
  reviewed_at : string option;
  removed_at : string option;
}

type audit_event = {
  event_id : int64;
  event_action : Audit.action;
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

(* === row shape === *)

(* The twelve durable columns, in one order every query below reuses.
   Timestamps cross as text, as everywhere else in the read layer. *)
let connection_columns =
  "id, requester_community_id, recipient_community_id, status, request_note, \
   requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
   created_at::text, updated_at::text, reviewed_at::text, removed_at::text"

let connection_row =
  Caqti_type.(
    t2
      (t2 (t2 int64 int) (t2 int string))
      (t2
         (t2 (option string) (t3 (option int) (option int) (option int)))
         (t2 (t2 string string) (t2 (option string) (option string)))))

let positive id = Int64.compare id 0L > 0
let optional_positive = function None -> true | Some id -> id > 0

(* The per-status timestamp and actor shape, mirroring the table's own
   status CHECK: a row that drifted out of it is corruption here too, not a
   fifth shape to interpret. *)
let shape_valid ~status ~reviewed_by ~removed_by ~reviewed_at ~removed_at =
  match status with
  | Cc.Pending ->
      reviewed_by = None && removed_by = None && reviewed_at = None
      && removed_at = None
  | Cc.Accepted | Cc.Rejected ->
      removed_by = None && reviewed_at <> None && removed_at = None
  | Cc.Removed -> reviewed_at <> None && removed_at <> None

let decode_connection
    ( ((id, requester), (recipient, status_raw)),
      ( (note, (requested_by, reviewed_by, removed_by)),
        ((created_at, updated_at), (reviewed_at, removed_at)) ) ) =
  match Cc.status_of_string status_raw with
  | None -> None
  | Some status ->
      (* The note must round-trip byte-exactly through the pure
         canonicalizer, and the pure constructor also re-proves the pair is
         positive and distinct — the two invariants the schema holds. *)
      let pair_and_note_ok =
        match
          Cc.create_pending ~requester_community_id:requester
            ~recipient_community_id:recipient ~request_note:note
        with
        | Error _ -> false
        | Ok value -> Cc.request_note value = note
      in
      if
        positive id && pair_and_note_ok
        && optional_positive requested_by
        && optional_positive reviewed_by
        && optional_positive removed_by
        && String.length created_at > 0
        && String.length updated_at > 0
        && shape_valid ~status ~reviewed_by ~removed_by ~reviewed_at ~removed_at
      then
        Some
          {
            id;
            requester_community_id = requester;
            recipient_community_id = recipient;
            status;
            request_note = note;
            requested_by_user_id = requested_by;
            reviewed_by_user_id = reviewed_by;
            removed_by_user_id = removed_by;
            created_at;
            updated_at;
            reviewed_at;
            removed_at;
          }
      else None

(* All-or-nothing: one incoherent row fails the whole call, so a caller can
   never render a partially validated list. *)
let decode_all rows =
  List.fold_left
    (fun acc raw ->
      match (acc, decode_connection raw) with
      | Some decoded, Some row -> Some (row :: decoded)
      | _ -> None)
    (Some []) rows
  |> Option.map List.rev

(* === queries === *)

let q_by_id =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->* connection_row)
    ("SELECT " ^ connection_columns
   ^ " FROM community_connections WHERE id = $1")

(* The unordered pair, normalized exactly as the partial unique index
   normalizes it, so this read and that index always agree on identity. *)
let q_active_for_pair =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->* connection_row)
    ("SELECT " ^ connection_columns
   ^ " FROM community_connections WHERE LEAST(requester_community_id, \
      recipient_community_id) = $1 AND GREATEST(requester_community_id, \
      recipient_community_id) = $2 AND status IN ('pending', 'accepted')")

(* Symmetric by construction: one predicate over both per-community
   indexes, so an accepted connection appears for either side. *)
let q_accepted_for_community =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* connection_row)
    ("SELECT " ^ connection_columns
   ^ " FROM community_connections WHERE (requester_community_id = $1 OR \
      recipient_community_id = $1) AND status = 'accepted' ORDER BY created_at \
      DESC, id DESC")

let q_incoming_pending =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* connection_row)
    ("SELECT " ^ connection_columns
   ^ " FROM community_connections WHERE recipient_community_id = $1 AND status \
      = 'pending' ORDER BY created_at, id")

let q_outgoing_pending =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* connection_row)
    ("SELECT " ^ connection_columns
   ^ " FROM community_connections WHERE requester_community_id = $1 AND status \
      = 'pending' ORDER BY created_at, id")

let q_audit_for_connection =
  let open Caqti_request.Infix in
  (Caqti_type.int64
  ->* Caqti_type.(
        t2 (t2 (t2 int64 string) (option int)) (t3 int64 int (t2 int string))))
    "SELECT id, action, actor_user_id, connection_id, requester_community_id, \
     recipient_community_id, created_at::text FROM \
     community_connection_audit_events WHERE connection_id = $1 ORDER BY id"

let decode_event
    ( ((event_id, action_raw), actor),
      (connection_id, requester, (recipient, created_at)) ) =
  match Audit.action_of_string action_raw with
  | None -> None
  | Some action ->
      if
        positive event_id && positive connection_id && requester > 0
        && recipient > 0 && requester <> recipient && optional_positive actor
        && String.length created_at > 0
      then
        Some
          {
            event_id;
            event_action = action;
            event_actor_user_id = actor;
            event_connection_id = connection_id;
            event_requester_community_id = requester;
            event_recipient_community_id = recipient;
            event_created_at = created_at;
          }
      else None

(* === reads === *)

(* Every Caqti error is dropped payload-free: error payloads can echo SQL
   parameters. *)
let collect (module C : Caqti_lwt.CONNECTION) query arg =
  C.collect_list query arg >>= function
  | Error _ -> Lwt.return (Error Storage_error)
  | Ok rows -> Lwt.return (Ok rows)

let list_with conn query arg =
  collect conn query arg >>= function
  | Error _ as e -> Lwt.return e
  | Ok rows -> (
      match decode_all rows with
      | None -> Lwt.return (Error Inconsistent_data)
      | Some decoded -> Lwt.return (Ok decoded))

let single_with conn query arg =
  list_with conn query arg >>= function
  | Error _ as e -> Lwt.return e
  | Ok [] -> Lwt.return (Ok None)
  | Ok [ row ] -> Lwt.return (Ok (Some row))
  (* The primary key and the partial unique index each cap their query at
     one row; more than one is durable corruption. *)
  | Ok (_ :: _ :: _) -> Lwt.return (Error Inconsistent_data)

let load conn ~connection_id =
  if not (positive connection_id) then Lwt.return (Error Invalid_connection_id)
  else single_with conn q_by_id connection_id

let active_for_pair conn ~community_a ~community_b =
  if community_a <= 0 || community_b <= 0 || community_a = community_b then
    Lwt.return (Error Invalid_community_id)
  else
    let low = min community_a community_b in
    let high = max community_a community_b in
    single_with conn q_active_for_pair (low, high)

let list_for_community conn query community_id =
  if community_id <= 0 then Lwt.return (Error Invalid_community_id)
  else list_with conn query community_id

let list_accepted conn ~community_id =
  list_for_community conn q_accepted_for_community community_id

let list_incoming_pending conn ~community_id =
  list_for_community conn q_incoming_pending community_id

let list_outgoing_pending conn ~community_id =
  list_for_community conn q_outgoing_pending community_id

let list_audit_events conn ~connection_id =
  if not (positive connection_id) then Lwt.return (Error Invalid_connection_id)
  else
    collect conn q_audit_for_connection connection_id >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok rows -> (
        match
          List.fold_left
            (fun acc raw ->
              match (acc, decode_event raw) with
              | Some decoded, Some row -> Some (row :: decoded)
              | _ -> None)
            (Some []) rows
        with
        | None -> Lwt.return (Error Inconsistent_data)
        | Some decoded -> Lwt.return (Ok (List.rev decoded)))
