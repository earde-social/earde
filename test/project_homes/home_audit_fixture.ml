(* Project-home lifecycle calls that also assert the audit trail. *)

module Phr = Earde.Project_home_relation

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Rms = Earde.Project_home_removal_store

let find = Db_fixture.find
let collect = Db_fixture.collect
let status_str = Phr.string_of_status

(* One tuple per event, in append (id) order. The project is the query
   key, so each row reduces to action, actor, community, relation. *)
let q_events_for_project =
  (Caqti_type.int64 ->* Caqti_type.(t2 (t2 string (option int)) (t2 int int64)))
    "SELECT action, actor_user_id, community_id, relation_id FROM \
     project_home_audit_events WHERE project_id = $1 ORDER BY id"

let q_count_events =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_home_audit_events WHERE project_id = $1"

(* Timestamp coherence: every event carries a real timestamp no older
   than its subject relation's own creation and no newer than now. *)
let q_coherent_count =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_home_audit_events e JOIN community_projects \
     r ON r.id = e.relation_id WHERE e.project_id = $1 AND e.created_at >= \
     r.created_at AND e.created_at <= NOW()"

let event_t = Alcotest.(pair (pair string (option int)) (pair int int64))

let check_events label conn ~project expected =
  let* rows = collect conn (label ^ ": events") q_events_for_project project in
  Alcotest.(check (list event_t)) (label ^ ": exact events") expected rows;
  let* coherent = find conn (label ^ ": coherent") q_coherent_count project in
  Alcotest.(check int)
    (label ^ ": every event timestamp coherent")
    (List.length expected) coherent;
  Lwt.return_unit

let check_event_count label conn ~project expected =
  let* n = find conn (label ^ ": count") q_count_events project in
  Alcotest.(check int) (label ^ ": event count") expected n;
  Lwt.return_unit

let request_ok label conn ~user ~slug ~community =
  let* created =
    Home_request_fixture.create_ok label conn ~user ~slug ~community
      (Home_request_fixture.phr_fresh_pending ())
  in
  Lwt.return (Rq.relation_id created)

let review conn ~reviewer ~slug ~community decision =
  Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
    ~target_community_slug:community ~decision

let review_ok label conn ~reviewer ~slug ~community decision expected =
  let* r = review conn ~reviewer ~slug ~community decision in
  match r with
  | Ok reviewed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str expected)
        (status_str (Rvs.resulting_status reviewed));
      Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (Home_review_fixture.error_str e)

let review_expect label expected conn ~reviewer ~slug ~community decision =
  let* r = review conn ~reviewer ~slug ~community decision in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Home_review_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Home_review_fixture.error_str expected)
        (Home_review_fixture.error_str e);
      Lwt.return_unit

let remove conn ~actor ~slug ~community =
  Rms.remove conn ~actor_user_id:actor ~project_slug:slug
    ~community_slug:community

let remove_ok label conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok removed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str Phr.Removed)
        (status_str (Rms.resulting_status removed));
      Lwt.return_unit
  | Error e ->
      Alcotest.failf "%s: %s" label (Connected_projects_fixture.error_str e)

let remove_expect label expected conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Connected_projects_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Connected_projects_fixture.error_str expected)
        (Connected_projects_fixture.error_str e);
      Lwt.return_unit

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM community_projects WHERE id = $1"

let check_status label conn relation expected =
  let* status = find conn (label ^ ": status") q_status relation in
  Alcotest.(check string) (label ^ ": relation status") expected status;
  Lwt.return_unit

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_absent_project_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM open_source_projects"

let q_absent_community_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

let q_absent_relation_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM community_projects"

let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

let sole_relation label conn project =
  let* ids =
    collect conn (label ^ ": relation ids")
      Home_provisioning_fixture.q_relation_ids project
  in
  match ids with
  | [ rid ] -> Lwt.return rid
  | l ->
      Alcotest.failf "%s: expected one relation, found %d" label (List.length l)
