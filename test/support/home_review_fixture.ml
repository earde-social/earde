(* Project-home review: accept/reject calls and their fixtures. *)

module Pi = Earde.Project_identity
module Phr = Earde.Project_home_relation
module Phrp = Earde.Project_home_review_pages

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rv = Earde.Project_home_review_store
module Rq = Earde.Project_home_request_store

let status_str = Phr.string_of_status
let exec = Db_fixture.exec

module Hr = Earde.Project_home_review_handlers

let error_str : Rv.error -> string = function
  | Rv.Invalid_user_id -> "Invalid_user_id"
  | Rv.Invalid_project_slug -> "Invalid_project_slug"
  | Rv.Invalid_community_slug -> "Invalid_community_slug"
  | Rv.Project_unavailable -> "Project_unavailable"
  | Rv.Community_unavailable -> "Community_unavailable"
  | Rv.Reviewer_unauthorized -> "Reviewer_unauthorized"
  | Rv.Review_unavailable -> "Review_unavailable"
  | Rv.Target_ineligible -> "Target_ineligible"
  | Rv.Inconsistent_data -> "Inconsistent_data"
  | Rv.Storage_error -> "Storage_error"

let or_fail = Db_fixture.or_fail

(* Role signature so no-mutation assertions cover the reviewer's own
   durable role. *)
let q_role_sig =
  (Caqti_type.(t2 int int) ->! Caqti_type.string)
    "SELECT COALESCE((SELECT role FROM community_moderators WHERE user_id = $1 \
     AND community_id = $2), '<none>')"

(* Review-time coherence, NULL-safe: presence plus ordering against
   created_at (reviewed_at NULL coalesces to FALSE, never a decode
   error). *)
let q_review_times =
  (Caqti_type.int64 ->! Caqti_type.(t3 bool bool bool))
    "SELECT reviewed_at IS NOT NULL, COALESCE(reviewed_at >= created_at, \
     FALSE), updated_at >= created_at FROM community_projects WHERE id = $1"

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM community_projects WHERE id = $1"

(* Community drift the sibling suites do not already provide. *)
let q_make_eligible =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET is_network_community = TRUE, onboarding_state = \
     'published', visibility = 'public', indexable = TRUE, discoverable = TRUE \
     WHERE id = $1"

let q_mix_flags =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET indexable = TRUE, discoverable = FALSE WHERE id = \
     $1"

(* Leaking shapes: a draft still publicly listed, and a private
   community still discoverable — both corruption per the shared
   lifecycle rule, both directly representable (the invariant is
   application-level). *)
let q_leaky_draft =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET onboarding_state = 'draft' WHERE id = $1"

let q_leaky_private =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET visibility = 'private', indexable = FALSE, \
     discoverable = TRUE WHERE id = $1"

(* Targeted pending-row corruption. Notes carry only a length CHECK, so
   a non-canonical (padded) note and a forbidden control byte are both
   directly representable. A reviewer or review/removal timestamp on a
   pending row and a non-'home' relation type are blocked by the
   production status-shape and relation-type CHECKs — those
   Inconsistent_data branches stay defensive and are not reachable
   without dropping production constraints. *)
let q_pad_note =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET request_note = '  padded  ' WHERE id = $1"

let q_control_note =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET request_note = 'phrv' || chr(1) || 'bad' \
     WHERE id = $1"

let q_clear_note =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects SET request_note = NULL WHERE id = $1"

let review conn ~reviewer ~slug ~community decision =
  Rv.review conn ~reviewer_user_id:reviewer ~project_slug:slug
    ~target_community_slug:community ~decision

let review_ok label expected conn ~reviewer ~slug ~community decision =
  let* r = review conn ~reviewer ~slug ~community decision in
  match r with
  | Ok reviewed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str expected)
        (status_str (Rv.resulting_status reviewed));
      Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let review_expect label expected conn ~reviewer ~slug ~community decision =
  let* r = review conn ~reviewer ~slug ~community decision in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* Pending fixtures come only through the real request store and the
   real pure constructor. *)
let request_ok label conn ~user ~slug ~community ?note () =
  let relation =
    Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:note)
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok created -> Lwt.return (Rq.relation_id created)
  | Error e -> Alcotest.failf "%s: %s" label (Home_request_fixture.error_str e)

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let add_top_mod conn ~user ~community = add_role conn ~user ~community "top_mod"

let phrp_repo ?(full_name = "octo-org/widgets")
    ?(url = "https://github.com/octo-org/widgets") ?(primary = false)
    ?(archived = false) () : Phrp.repository =
  {
    Phrp.full_name;
    html_url = url;
    is_primary = primary;
    is_archived = archived;
  }

let phrp_request ?(name = "Widget Kit") ?(slug = "widget-kit")
    ?(kind = Pi.Project) ?(login = "octo-org") ?(verification = Phrp.Verified)
    ?repositories ?requester ?note () : Phrp.pending_request =
  {
    Phrp.project_name = name;
    project_slug = slug;
    project_kind = kind;
    namespace_login = login;
    verification;
    repositories =
      (match repositories with
      | Some r -> r
      | None -> [ phrp_repo ~primary:true () ]);
    requester_name = requester;
    request_note = note;
  }

let phrp_community ?(name = "Alpine Devs") ?(slug = "alpine")
    ?(eligibility = Phrp.Eligible) () : Phrp.community =
  { Phrp.name; slug; host_eligibility = eligibility }

let phrp_state ?community ?(requests = [ phrp_request () ]) () : Phrp.state =
  {
    Phrp.community =
      (match community with Some c -> c | None -> phrp_community ());
    requests;
  }

let make_queue ~mode = Hr.make_project_home_review_queue_handler ~mode
