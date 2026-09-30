(* Project-home relations and requests, built through the real request
   store, with the counting and signature queries its callers assert on. *)

module Phr = Earde.Project_home_relation

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rq = Earde.Project_home_request_store
module Fin = Earde.Project_finalization_store

let or_fail = Db_fixture.or_fail
let find = Db_fixture.find

let phr_expect_ok = function
  | Ok t -> t
  | Error _ -> Alcotest.fail "expected a valid relation"

let phr_pending ?note () = Phr.create_pending ~request_note:note
let phr_fresh_pending () = phr_expect_ok (phr_pending ())

let phr_fresh_accepted () =
  phr_expect_ok (Phr.apply (phr_fresh_pending ()) Phr.Accept)

let phr_fresh_rejected () =
  phr_expect_ok (Phr.apply (phr_fresh_pending ()) Phr.Reject)

let phr_fresh_removed () =
  phr_expect_ok (Phr.apply (phr_fresh_accepted ()) Phr.Remove)

let error_str : Rq.error -> string = function
  | Rq.Invalid_user_id -> "Invalid_user_id"
  | Rq.Invalid_project_slug -> "Invalid_project_slug"
  | Rq.Invalid_community_id -> "Invalid_community_id"
  | Rq.Invalid_relation -> "Invalid_relation"
  | Rq.Project_unavailable -> "Project_unavailable"
  | Rq.Community_unavailable -> "Community_unavailable"
  | Rq.Active_home_exists -> "Active_home_exists"
  | Rq.Inconsistent_data -> "Inconsistent_data"
  | Rq.Storage_error -> "Storage_error"

(* Direct community fixtures: the dedicated-community provisioning
   store does not exist yet, so lifecycle shapes are written exactly as
   the durable columns represent them today. Defaults are the eligible
   fully listed published network community. *)
let q_insert_community =
  (Caqti_type.(t2 (t2 string string) (t2 (t2 bool bool) (t2 string bool)))
  ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility, indexable, \
     is_network_community, onboarding_state, discoverable) VALUES ($1, $1, $2, \
     $3, $4, $5, $6) RETURNING id"

let insert_community ?(visibility = "public") ?(indexable = true)
    ?(network = true) ?(onboarding = "published") ?(discoverable = true) conn
    slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* cid =
    C.find q_insert_community
      ((slug, visibility), ((indexable, network), (onboarding, discoverable)))
  in
  or_fail ("community " ^ slug) cid

(* Everything durable on one relation row; timestamp columns reduce to
   presence/ordering booleans (their exact values are NOW()-relative). *)
let q_relation_row =
  Caqti_type.(
    int64
    ->! t2
          (t2 (t2 int64 int) (t2 string string))
          (t2
             (t2 (option int) (option int))
             (t2 (option string) (t3 bool bool bool))))
    "SELECT project_id, community_id, relation_type, status, \
     requested_by_user_id, reviewed_by_user_id, request_note, reviewed_at IS \
     NOT NULL, removed_at IS NOT NULL, updated_at >= created_at FROM \
     community_projects WHERE id = $1"

let relation_row conn id = find conn "relation row" q_relation_row id

(* One text signature per relation row, for exact unchanged-history
   assertions. *)
let q_relation_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT project_id::text || '|' || community_id::text || '|' || \
     relation_type || '|' || status || '|' || \
     COALESCE(requested_by_user_id::text, '<null>') || '|' || \
     COALESCE(reviewed_by_user_id::text, '<null>') || '|' || \
     COALESCE(request_note, '<null>') || '|' || (reviewed_at IS NOT \
     NULL)::text || '|' || (removed_at IS NOT NULL)::text FROM \
     community_projects WHERE id = $1"

let q_community_sig =
  (Caqti_type.int ->! Caqti_type.string)
    "SELECT slug || '|' || name || '|' || visibility || '|' || indexable::text \
     || '|' || is_network_community::text || '|' || onboarding_state || '|' || \
     discoverable::text FROM communities WHERE id = $1"

let q_project_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT slug || '|' || verification_status FROM open_source_projects WHERE \
     id = $1"

let q_count_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_projects WHERE project_id = $1"

let q_count_active_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_projects WHERE project_id = $1 AND status \
     IN ('pending', 'accepted')"

let q_count_members =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_members WHERE community_id = $1"

let q_count_moderators =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_moderators WHERE community_id = $1"

let q_count_stewards_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_stewards WHERE project_id = $1"

(* Store-shaped lifecycle transitions with coherent timestamps, used to
   free the active slot and to build accepted/historical fixtures.
   Transition legality itself is Project_home_relation territory. *)
let q_mark_rejected =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET status = 'rejected', reviewed_at = NOW(), \
     updated_at = NOW() WHERE id = $1"

let q_mark_accepted =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET status = 'accepted', reviewed_at = NOW(), \
     updated_at = NOW() WHERE id = $1"

let q_mark_removed =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET status = 'removed', reviewed_at = \
     COALESCE(reviewed_at, NOW()), removed_at = NOW(), updated_at = NOW() \
     WHERE id = $1"

(* Project- and installation-side probes for the authorization cases. *)
let q_set_verification =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET verification_status = $2 WHERE id = $1"

let q_set_created_by =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET created_by_user_id = $2 WHERE id = $1"

let q_delete_steward =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "DELETE FROM project_stewards WHERE project_id = $1 AND user_id = $2"

let q_insert_steward =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
    "INSERT INTO project_stewards (project_id, user_id, \
     github_installation_record_id, role) VALUES ($1, $2, $3, 'steward')"

(* Targeted durable corruption: communities.slug carries no schema
   grammar, so a non-addressable stored slug is directly producible;
   the mixed published flag shape is likewise representable (the
   lifecycle invariant is application-level). visibility and
   onboarding_state off-enum corruption is blocked by their DB CHECKs,
   and a non-positive returned relation id by BIGSERIAL — those
   Inconsistent_data branches stay defensive. *)
let q_corrupt_community_slug =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE communities SET slug = $2 WHERE id = $1"

let q_mix_community_flags =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET indexable = TRUE, discoverable = FALSE WHERE id = \
     $1"

(* Every text column of one relation row, and the same minus the note,
   for the boolean credential-absence checks. *)
let q_relation_text_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT project_id::text || '|' || community_id::text || '|' || \
     relation_type || '|' || status || '|' || \
     COALESCE(requested_by_user_id::text, '') || '|' || \
     COALESCE(reviewed_by_user_id::text, '') || '|' || COALESCE(request_note, \
     '') FROM community_projects WHERE id = $1"

let q_relation_nonnote_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT project_id::text || '|' || community_id::text || '|' || \
     relation_type || '|' || status || '|' || \
     COALESCE(requested_by_user_id::text, '') || '|' || \
     COALESCE(reviewed_by_user_id::text, '') FROM community_projects WHERE id \
     = $1"

(* Verified permanent projects come only through the real chain —
   draft store, selection store, finalization store — never fixture
   INSERTs. Returns the installation record id (for steward fixtures)
   and the permanent project id. *)
let make_project conn ~user ~ext_id ~slug =
  let repo_id = Int64.add ext_id 400000L in
  let* inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        [ Project_fixture.repo ~account_id ~id:repo_id "alpha" ])
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let s1 = List.nth ids 0 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary:s1
      [ s1 ]
  in
  let identity =
    Project_fixture.identity_exn ~slug ~selected:[ s1 ] ~primary:s1 ()
  in
  let* created =
    Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity
  in
  Lwt.return (inst, Fin.project_id created)

let create conn ~user ~slug ~community relation =
  Rq.create conn ~user_id:user ~project_slug:slug ~target_community_id:community
    ~relation

let create_ok label conn ~user ~slug ~community relation =
  let* r = create conn ~user ~slug ~community relation in
  match r with
  | Ok created -> Lwt.return created
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let create_expect label expected conn ~user ~slug ~community relation =
  let* r = create conn ~user ~slug ~community relation in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* One winner plus one exact loser, order unasserted. *)
let ok_and_error label expected (r1, r2) =
  match (r1, r2) with
  | Ok created, Error e when e = expected -> created
  | Error e, Ok created when e = expected -> created
  | Ok _, Ok _ -> Alcotest.failf "%s: both succeeded" label
  | Error a, Error b ->
      Alcotest.failf "%s: both failed (%s, %s)" label (error_str a)
        (error_str b)
  | Ok _, Error e | Error e, Ok _ ->
      Alcotest.failf "%s: unexpected loser error %s" label (error_str e)
