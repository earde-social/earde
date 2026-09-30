(* Community rows in each visibility and lifecycle state, membership,
   moderator roles and global-admin flags. *)

module Phr = Earde.Project_home_relation
let ( let* ) = Lwt.bind
open Caqti_request.Infix

module Rq = Earde.Project_home_request_store
let or_fail = Db_fixture.or_fail
module Store = Earde.Community_connections_store

(* Direct community fixtures with display name and description control:
   lifecycle shapes are written exactly as the durable columns represent
   them today. Defaults are the eligible fully listed published network
   community. *)
let q_insert_community =
  (Caqti_type.(
     t2
       (t2 (t2 string string) (t2 (option string) string))
       (t2 (t2 bool bool) (t2 string bool)))
   ->! Caqti_type.int)
  "INSERT INTO communities \
     (slug, name, description, visibility, indexable, \
      is_network_community, onboarding_state, discoverable) \
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8) RETURNING id"

let insert_community ?name ?description ?(visibility = "public")
    ?(indexable = true) ?(network = true) ?(onboarding = "published")
    ?(discoverable = true) conn slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let name = match name with Some n -> n | None -> slug in
  let* cid =
    C.find q_insert_community
      ( ((slug, name), (description, visibility)),
        ((indexable, network), (onboarding, discoverable)) )
  in
  or_fail ("community " ^ slug) cid

(* Community-side lifecycle drift and targeted durable corruption. *)
let q_make_private =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET visibility = 'private', indexable = FALSE, \
   discoverable = FALSE WHERE id = $1"

let q_make_unlisted =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET indexable = FALSE, discoverable = FALSE \
   WHERE id = $1"

let q_make_listed =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET indexable = TRUE, discoverable = TRUE \
   WHERE id = $1"

let q_make_draft_state =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET onboarding_state = 'draft', \
   visibility = 'private', indexable = FALSE, discoverable = FALSE \
   WHERE id = $1"

let q_make_legacy =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET is_network_community = FALSE WHERE id = $1"

let q_set_name =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
  "UPDATE communities SET name = $2 WHERE id = $1"

(* Pending relations come only through the real transactional store. *)
let request_pending label conn ~user ~slug ~community ?note () =
  let relation = Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:note) in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok created -> Lwt.return (Rq.relation_id created)
  | Error _ -> Alcotest.failf "%s: request fixture failed" label

(* Durable authorization fixtures — the real rows the store reads. *)
let q_insert_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  "INSERT INTO community_moderators (user_id, community_id, role) \
   VALUES ($1, $2, $3)"

let q_remove_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "DELETE FROM community_moderators \
   WHERE user_id = $1 AND community_id = $2"

let q_set_moderator_role =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  "UPDATE community_moderators SET role = $3 \
   WHERE user_id = $1 AND community_id = $2"

let q_insert_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_set_admin =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
  "UPDATE users SET is_admin = $2 WHERE id = $1"

let error_str : Store.error -> string = function
  | Store.Invalid_user_id -> "Invalid_user_id"
  | Store.Invalid_connection_id -> "Invalid_connection_id"
  | Store.Invalid_community_id -> "Invalid_community_id"
  | Store.Invalid_connection -> "Invalid_connection"
  | Store.Community_unavailable -> "Community_unavailable"
  | Store.Requester_ineligible -> "Requester_ineligible"
  | Store.Recipient_ineligible -> "Recipient_ineligible"
  | Store.Active_connection_exists -> "Active_connection_exists"
  | Store.Review_unavailable -> "Review_unavailable"
  | Store.Removal_unavailable -> "Removal_unavailable"
  | Store.Inconsistent_data -> "Inconsistent_data"
  | Store.Storage_error -> "Storage_error"
