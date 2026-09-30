(* The authorized Shared-threads management view and the two mutation-side
   reads (subject binding, withdrawal grant). Authorization is inside the
   SQL — a current top_mod row of the named community, or a session admin
   claim still backed by a durable users.is_admin; the withdrawal gate
   additionally admits the placement's original requester — so an
   unauthorized caller and a missing subject produce the same Ok None.
   Read-only: no locks, no writes, no logging. See the .mli for the full
   contract. *)

open Lwt.Infix

module Stp = Shared_thread_placements
module Cc = Community_connections

type counterpart = { counterpart_name : string; counterpart_slug : string }

let counterpart_name { counterpart_name; _ } = counterpart_name
let counterpart_slug { counterpart_slug; _ } = counterpart_slug

type section_option = {
  section_option_id : int;
  section_option_name : string;
}

let section_option_id { section_option_id; _ } = section_option_id
let section_option_name { section_option_name; _ } = section_option_name

type pending_row = {
  pending_placement_id : int64;
  pending_post_id : int;
  pending_post_title : string;
  pending_counterpart : counterpart;
  pending_note : string option;
  pending_requested_at : string;
}

let pending_placement_id { pending_placement_id; _ } = pending_placement_id
let pending_post_id { pending_post_id; _ } = pending_post_id
let pending_post_title { pending_post_title; _ } = pending_post_title
let pending_counterpart { pending_counterpart; _ } = pending_counterpart
let pending_note { pending_note; _ } = pending_note
let pending_requested_at { pending_requested_at; _ } = pending_requested_at

type accepted_row = {
  accepted_placement_id : int64;
  accepted_post_id : int;
  accepted_post_title : string;
  accepted_counterpart : counterpart;
  accepted_section_name : string option;
  accepted_at : string;
}

let accepted_placement_id { accepted_placement_id; _ } = accepted_placement_id
let accepted_post_id { accepted_post_id; _ } = accepted_post_id
let accepted_post_title { accepted_post_title; _ } = accepted_post_title
let accepted_counterpart { accepted_counterpart; _ } = accepted_counterpart
let accepted_section_name { accepted_section_name; _ } = accepted_section_name
let accepted_at { accepted_at; _ } = accepted_at

type view = {
  view_community_id : int;
  view_community_name : string;
  view_community_slug : string;
  view_community_eligible : bool;
  view_sections_enabled : bool;
  view_section_options : section_option list;
  view_incoming : pending_row list;
  view_outgoing : pending_row list;
  view_shared_into : accepted_row list;
  view_shared_from : accepted_row list;
}

let view_community_id { view_community_id; _ } = view_community_id
let view_community_name { view_community_name; _ } = view_community_name
let view_community_slug { view_community_slug; _ } = view_community_slug

let view_community_eligible { view_community_eligible; _ } =
  view_community_eligible

let view_sections_enabled { view_sections_enabled; _ } = view_sections_enabled
let view_section_options { view_section_options; _ } = view_section_options
let view_incoming { view_incoming; _ } = view_incoming
let view_outgoing { view_outgoing; _ } = view_outgoing
let view_shared_into { view_shared_into; _ } = view_shared_into
let view_shared_from { view_shared_from; _ } = view_shared_from

type subjects = {
  subjects_post_id : int;
  subjects_origin_community_id : int;
  subjects_destination_community_id : int;
}

let subjects_post_id { subjects_post_id; _ } = subjects_post_id

let subjects_origin_community_id { subjects_origin_community_id; _ } =
  subjects_origin_community_id

let subjects_destination_community_id { subjects_destination_community_id; _ }
    =
  subjects_destination_community_id

type withdrawal_grant = {
  grant_community_id : int;
  grant_community_slug : string;
  grant_post_id : int;
}

let grant_community_id { grant_community_id; _ } = grant_community_id
let grant_community_slug { grant_community_slug; _ } = grant_community_slug
let grant_post_id { grant_post_id; _ } = grant_post_id

type error =
  | Invalid_user_id
  | Invalid_community_slug
  | Invalid_placement_id
  | Inconsistent_data
  | Storage_error

let max_rows_per_queue = 100

let addressable_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let nonblank value =
  String.exists
    (fun c ->
      not
        (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

let control_safe value =
  String.for_all
    (fun byte ->
      let code = Char.code byte in
      (code >= 0x20 && code <> 0x7f) || byte = '\t')
    value

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* Identity, eligibility facts, section mode, and authorization in one
   statement: the community is never loaded and checked in OCaml, and the
   role is never queried afterwards. $3 is the caller's session admin state
   — it only enables the durable users.is_admin check beside it, never
   replaces it. *)
let load_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string int) bool)
   ->? Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string string))
           (t3 string bool bool)))
  "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
          c.discoverable, c.sections_enabled \
   FROM communities c \
   WHERE c.slug = $1 \
     AND (EXISTS (SELECT 1 FROM community_moderators m \
                  WHERE m.community_id = c.id AND m.user_id = $2 \
                    AND m.role = 'top_mod') \
          OR ($3 AND EXISTS (SELECT 1 FROM users u \
                             WHERE u.id = $2 AND u.is_admin)))"

(* This community's own sections, loaded once for every accept form on the
   page — never once per placement row. *)
let sections_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 int string))
  "SELECT cs.id, cs.name \
   FROM community_sections cs \
   WHERE cs.community_id = $1 \
   ORDER BY cs.position ASC, cs.name ASC, cs.id ASC"

(* The four queues. Pending queues read oldest first — they are review
   queues; accepted queues read newest acceptance first — they are a
   record. Each is bounded, and each joins the canonical post title and the
   counterpart community identity in the same statement, so the page never
   issues per-row lookups. *)
let incoming_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int)
   ->* Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 string string))
           (t3 string (option string) string)))
  "SELECT sp.id, sp.post_id, other.slug, other.name, p.title, \
          sp.request_note, sp.created_at::text \
   FROM shared_thread_placements sp \
   JOIN posts p ON p.id = sp.post_id \
   JOIN communities other ON other.id = sp.origin_community_id \
   WHERE sp.destination_community_id = $1 AND sp.status = 'pending' \
   ORDER BY sp.created_at ASC, sp.id ASC \
   LIMIT $2"

let outgoing_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int)
   ->* Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 string string))
           (t3 string (option string) string)))
  "SELECT sp.id, sp.post_id, other.slug, other.name, p.title, \
          sp.request_note, sp.created_at::text \
   FROM shared_thread_placements sp \
   JOIN posts p ON p.id = sp.post_id \
   JOIN communities other ON other.id = sp.destination_community_id \
   WHERE sp.origin_community_id = $1 AND sp.status = 'pending' \
   ORDER BY sp.created_at ASC, sp.id ASC \
   LIMIT $2"

let shared_into_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int)
   ->* Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 string string))
           (t3 string (option string) (option string))))
  "SELECT sp.id, sp.post_id, other.slug, other.name, p.title, \
          cs.name, sp.reviewed_at::text \
   FROM shared_thread_placements sp \
   JOIN posts p ON p.id = sp.post_id \
   JOIN communities other ON other.id = sp.origin_community_id \
   LEFT JOIN community_sections cs ON cs.id = sp.destination_section_id \
   WHERE sp.destination_community_id = $1 AND sp.status = 'accepted' \
   ORDER BY sp.reviewed_at DESC, sp.id DESC \
   LIMIT $2"

let shared_from_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int)
   ->* Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 string string))
           (t3 string (option string) (option string))))
  "SELECT sp.id, sp.post_id, other.slug, other.name, p.title, \
          cs.name, sp.reviewed_at::text \
   FROM shared_thread_placements sp \
   JOIN posts p ON p.id = sp.post_id \
   JOIN communities other ON other.id = sp.destination_community_id \
   LEFT JOIN community_sections cs ON cs.id = sp.destination_section_id \
   WHERE sp.origin_community_id = $1 AND sp.status = 'accepted' \
   ORDER BY sp.reviewed_at DESC, sp.id DESC \
   LIMIT $2"

let subjects_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.(t3 int int int))
  "SELECT sp.post_id, sp.origin_community_id, sp.destination_community_id \
   FROM shared_thread_placements sp \
   WHERE sp.id = $1"

(* The withdrawal gate in one statement: the route community must be the
   placement's origin, and the caller its original requester, a current
   origin top_mod, or a durable admin behind a session claim. Deliberately
   status-blind and free of eligibility, connection, membership, and
   tombstone conditions: withdrawal is cleanup. *)
let authorize_withdrawal_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string int64) (t2 int bool))
   ->? Caqti_type.(t3 int string int))
  "SELECT c.id, c.slug, sp.post_id \
   FROM communities c \
   JOIN shared_thread_placements sp ON sp.origin_community_id = c.id \
   WHERE c.slug = $1 AND sp.id = $2 \
     AND (sp.requested_by_user_id = $3 \
          OR EXISTS (SELECT 1 FROM community_moderators m \
                     WHERE m.community_id = c.id AND m.user_id = $3 \
                       AND m.role = 'top_mod') \
          OR ($4 AND EXISTS (SELECT 1 FROM users u \
                             WHERE u.id = $3 AND u.is_admin)))"

(* === decoding === *)

let eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable =
  match
    ( Community_types.community_visibility_of_string visibility_raw,
      Community_types.community_onboarding_state_of_string onboarding_raw )
  with
  | Some visibility, Ok onboarding_state ->
      Some (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
  | None, _ | _, Error _ -> None

let decode_counterpart ~slug ~name =
  if addressable_slug slug && nonblank name && control_safe name then
    Some { counterpart_name = name; counterpart_slug = slug }
  else None

(* The stored note must round-trip byte-exactly through the pure
   canonicalizer: a durable note the canonicalizer would change is
   corruption, not text to repair on the way out. The subject ids are not
   at hand per row here, so the round-trip runs over fixed valid ids — the
   note rule is independent of them. *)
let canonical_note note =
  match
    Stp.create_pending ~post_id:1 ~origin_community_id:1
      ~destination_community_id:2 ~request_note:note
  with
  | Error _ -> false
  | Ok value -> Stp.request_note value = note

let decode_pending
    (((id, post_id), (slug, name)), (title, note, requested_at)) =
  match decode_counterpart ~slug ~name with
  | Some counterpart
    when positive id && post_id > 0 && canonical_note note
         && requested_at <> "" ->
      Some
        {
          pending_placement_id = id;
          pending_post_id = post_id;
          pending_post_title = title;
          pending_counterpart = counterpart;
          pending_note = note;
          pending_requested_at = requested_at;
        }
  | Some _ | None -> None

let decode_accepted
    (((id, post_id), (slug, name)), (title, section_name, accepted_at)) =
  match (decode_counterpart ~slug ~name, accepted_at) with
  (* An accepted row without its review time violates the durable shape. *)
  | Some counterpart, Some accepted_at
    when positive id && post_id > 0 && accepted_at <> "" ->
      Some
        {
          accepted_placement_id = id;
          accepted_post_id = post_id;
          accepted_post_title = title;
          accepted_counterpart = counterpart;
          accepted_section_name = section_name;
          accepted_at;
        }
  | _ -> None

let decode_section (id, name) =
  if id > 0 && nonblank name && control_safe name then
    Some { section_option_id = id; section_option_name = name }
  else None

(* All-or-nothing: one incoherent row fails the whole call, so no caller can
   render a partially validated list. *)
let collect_all decode rows =
  List.fold_left
    (fun acc raw ->
      match (acc, decode raw) with
      | Some decoded, Some row -> Some (row :: decoded)
      | _ -> None)
    (Some []) rows
  |> Option.map List.rev

(* Every Caqti error is dropped payload-free: error payloads can echo SQL
   parameters. *)
let collect (module C : Caqti_lwt.CONNECTION) query arg =
  C.collect_list query arg >>= function
  | Error _ -> Lwt.return (Error Storage_error)
  | Ok rows -> Lwt.return (Ok rows)

(* === reads === *)

let load_for_manager (module C : Caqti_lwt.CONNECTION) ~user_id
    ~session_global_admin ~community_slug =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (addressable_slug community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    C.find_opt load_community_query
      ((community_slug, user_id), session_global_admin)
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    (* Missing community and every unauthorized cause alike. *)
    | Ok None -> Lwt.return (Ok None)
    | Ok
        (Some
          ( ((community_id, stored_slug), (name, visibility_raw)),
            (onboarding_raw, discoverable, sections_enabled) )) -> (
        match
          ( community_id > 0
            && String.equal stored_slug community_slug
            && nonblank name && control_safe name,
            eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable )
        with
        | false, _ | _, None -> Lwt.return (Error Inconsistent_data)
        | true, Some eligible -> (
            (if sections_enabled then
               collect (module C) sections_query community_id
             else Lwt.return (Ok []))
            >>= function
            | Error _ as e -> Lwt.return e
            | Ok section_rows -> (
                collect
                  (module C)
                  incoming_query
                  (community_id, max_rows_per_queue)
                >>= function
                | Error _ as e -> Lwt.return e
                | Ok incoming_rows -> (
                    collect
                      (module C)
                      outgoing_query
                      (community_id, max_rows_per_queue)
                    >>= function
                    | Error _ as e -> Lwt.return e
                    | Ok outgoing_rows -> (
                        collect
                          (module C)
                          shared_into_query
                          (community_id, max_rows_per_queue)
                        >>= function
                        | Error _ as e -> Lwt.return e
                        | Ok shared_into_rows -> (
                            collect
                              (module C)
                              shared_from_query
                              (community_id, max_rows_per_queue)
                            >>= function
                            | Error _ as e -> Lwt.return e
                            | Ok shared_from_rows -> (
                                match
                                  ( collect_all decode_section section_rows,
                                    collect_all decode_pending incoming_rows,
                                    collect_all decode_pending outgoing_rows,
                                    collect_all decode_accepted
                                      shared_into_rows,
                                    collect_all decode_accepted
                                      shared_from_rows )
                                with
                                | ( Some sections,
                                    Some incoming,
                                    Some outgoing,
                                    Some shared_into,
                                    Some shared_from ) ->
                                    Lwt.return
                                      (Ok
                                         (Some
                                            {
                                              view_community_id = community_id;
                                              view_community_name = name;
                                              view_community_slug = stored_slug;
                                              view_community_eligible =
                                                eligible;
                                              view_sections_enabled =
                                                sections_enabled;
                                              view_section_options = sections;
                                              view_incoming = incoming;
                                              view_outgoing = outgoing;
                                              view_shared_into = shared_into;
                                              view_shared_from = shared_from;
                                            }))
                                | _ -> Lwt.return (Error Inconsistent_data))))))))

let load_placement_subjects (module C : Caqti_lwt.CONNECTION) ~placement_id =
  if not (positive placement_id) then
    Lwt.return (Error Invalid_placement_id)
  else
    C.find_opt subjects_query placement_id >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (post_id, origin_id, destination_id)) ->
        if
          post_id > 0 && origin_id > 0 && destination_id > 0
          && origin_id <> destination_id
        then
          Lwt.return
            (Ok
               (Some
                  {
                    subjects_post_id = post_id;
                    subjects_origin_community_id = origin_id;
                    subjects_destination_community_id = destination_id;
                  }))
        else Lwt.return (Error Inconsistent_data)

let authorize_withdrawal (module C : Caqti_lwt.CONNECTION) ~user_id
    ~session_global_admin ~community_slug ~placement_id =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (addressable_slug community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else if not (positive placement_id) then
    Lwt.return (Error Invalid_placement_id)
  else
    C.find_opt authorize_withdrawal_query
      ((community_slug, placement_id), (user_id, session_global_admin))
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    (* Missing community, missing placement, another community's placement,
       and every unauthorized cause alike. *)
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some (community_id, stored_slug, post_id)) ->
        if
          community_id > 0 && post_id > 0
          && String.equal stored_slug community_slug
        then
          Lwt.return
            (Ok
               (Some
                  {
                    grant_community_id = community_id;
                    grant_community_slug = stored_slug;
                    grant_post_id = post_id;
                  }))
        else Lwt.return (Error Inconsistent_data)
