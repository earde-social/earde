module Pi = Earde.Project_identity
module Phvf = Earde.Project_home_provisioning_form

(* === Dedicated-home creation read model
   (Project_home_provisioning_read_model) ===
   Owner authorization decided in SQL, the active-home exclusion, durable
   revalidation, and the suggested initial identity. Reserved
   external-installation-id range 949000001..949000999 (hence account ids
   949100001..949100999, which also scope the permanent-project cleanup),
   phvr_% usernames, and phvr-% community slugs so no suite shares fixtures.
   Verified projects come only through the real draft/selection/finalization
   chain; relations come only through the real request/review/removal stores.
   Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rm = Earde.Project_home_provisioning_read_model
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Rms = Earde.Project_home_removal_store
module Fin = Earde.Project_finalization_store

let error_str : Rm.error -> string = function
  | Rm.Invalid_user_id -> "Invalid_user_id"
  | Rm.Invalid_project_slug -> "Invalid_project_slug"
  | Rm.Inconsistent_data -> "Inconsistent_data"
  | Rm.Storage_error -> "Storage_error"

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let insert_community = Community_fixture.insert_community

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 949100001 \
       AND 949100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       949100001 AND 949100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 949000001 AND 949000999)";
      "DELETE FROM communities WHERE slug LIKE 'phvr-%'";
      "DELETE FROM users WHERE username LIKE 'phvr_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       949000001 AND 949000999";
    ]

let q_corrupt_name_untrimmed =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = chr(9) || 'Phvr Name' WHERE id = $1"

let q_corrupt_description =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET description = 'Phvr' || chr(1) || 'Body' \
     WHERE id = $1"

let q_set_verification = Home_request_fixture.q_set_verification
let q_delete_steward = Home_request_fixture.q_delete_steward
let q_insert_moderator = Community_fixture.q_insert_moderator

(* Emptying search_path hides the unqualified tables, so the query fails at
   the SQL layer and Caqti returns an Error the read model maps to
   Storage_error — a genuine query failure, not a torn-down connection
   (which the driver signals by raising, not by Error). *)
let q_break_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO ''"

let q_reset_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let remove label conn ~actor ~slug ~community_slug =
  let* r =
    Rms.remove conn ~actor_user_id:actor ~project_slug:slug ~community_slug
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: removal fixture failed" label

(* === call helpers === *)

let load conn ~user ~slug =
  Rm.load_for_steward conn ~user_id:user ~project_slug:slug

let load_view label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok (Some view) -> Lwt.return view
  | Ok None -> Alcotest.failf "%s: unexpectedly absent" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_none label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Alcotest.failf "%s: unexpectedly present" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok None ->
      Alcotest.failf "%s: expected %s, got Ok None" label (error_str expected)
  | Ok (Some _) ->
      Alcotest.failf "%s: expected %s, got Ok Some" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* === pure input validation === *)

let pure_inputs_case =
  db_case "provisioning read: invalid inputs rejected before any SQL"
    (fun _conn ->
      (* A deliberately unusable connection: pure validation must return
         without touching it — were any SQL attempted, the driver would
         raise on the finished connection and fail the test. *)
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let expect label e ~user ~slug = load_expect label e dead ~user ~slug in
      let* () = expect "user 0" Rm.Invalid_user_id ~user:0 ~slug:"phvr-a" in
      let* () = expect "user -1" Rm.Invalid_user_id ~user:(-1) ~slug:"phvr-a" in
      Lwt_list.iter_s
        (fun slug ->
          expect
            ("slug " ^ String.escaped slug)
            Rm.Invalid_project_slug ~user:1 ~slug)
        [
          "";
          " phvr-a";
          "phvr-a ";
          "Phvr-A";
          "phvr_a";
          "-phvr";
          "phvr-";
          "phvr--a";
          "phvr/a";
          "phvr a";
          "phvr\x00";
          String.make 81 'a';
        ])

(* === owner authorization === *)

let authorization_case =
  db_case
    "provisioning read: only a current steward of a verified project sees the \
     view; every other identity is the same absence" (fun conn ->
      let* owner = insert_user conn "phvr_owner" in
      let* second = insert_user conn "phvr_second" in
      let* stranger = insert_user conn "phvr_stranger" in
      let* admin = insert_user conn "phvr_admin" in
      let* () =
        exec conn "admin flag" Community_fixture.q_set_admin (admin, true)
      in
      let* inst, project =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000001L ~slug:"phvr-auth"
      in
      let* view = load_view "owner" conn ~user:owner ~slug:"phvr-auth" in
      Alcotest.(check string)
        "project slug" "phvr-auth"
        (Rm.project_slug (Rm.project view));
      (* A second steward of the same project is equally authorized. *)
      let* () =
        Home_provisioning_fixture.add_steward conn ~project ~user:second
          ~installation:inst
      in
      let* _ = load_view "second steward" conn ~user:second ~slug:"phvr-auth" in
      (* Everyone else — including a durable global admin who is not a
         steward — collapses to the same absence, matching the sibling
         permanent-project setup and choice read models. *)
      let* () = load_none "stranger" conn ~user:stranger ~slug:"phvr-auth" in
      let* () = load_none "durable admin" conn ~user:admin ~slug:"phvr-auth" in
      let* () =
        load_none "missing project" conn ~user:owner ~slug:"phvr-gone"
      in
      (* Removed stewardship revokes access with no distinguishable
         answer. *)
      let* () = exec conn "remove steward" q_delete_steward (project, second) in
      load_none "removed steward" conn ~user:second ~slug:"phvr-auth")

let verification_case =
  db_case "provisioning read: a stale or revoked project is the same absence"
    (fun conn ->
      let* owner = insert_user conn "phvr_vowner" in
      let* _, project =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000002L ~slug:"phvr-verif"
      in
      let* _ = load_view "verified" conn ~user:owner ~slug:"phvr-verif" in
      let* () = exec conn "stale" q_set_verification (project, "stale") in
      let* () = load_none "stale" conn ~user:owner ~slug:"phvr-verif" in
      let* () = exec conn "revoked" q_set_verification (project, "revoked") in
      let* () = load_none "revoked" conn ~user:owner ~slug:"phvr-verif" in
      let* () = exec conn "restore" q_set_verification (project, "verified") in
      let* _ = load_view "restored" conn ~user:owner ~slug:"phvr-verif" in
      Lwt.return_unit)

(* === active-home exclusion === *)

let active_home_case =
  db_case
    "provisioning read: a pending request or accepted home closes the entry, \
     and closed history reopens it" (fun conn ->
      let* owner = insert_user conn "phvr_aowner" in
      let* moderator = insert_user conn "phvr_amod" in
      let* _, _project =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000003L ~slug:"phvr-active"
      in
      let* cid = insert_community ~name:"Phvr Home" conn "phvr-home" in
      let* () =
        Home_provisioning_fixture.add_top_mod conn ~user:moderator
          ~community:cid
      in
      let* _ = load_view "no relation" conn ~user:owner ~slug:"phvr-active" in
      (* Pending closes it. *)
      let* () =
        Home_provisioning_fixture.request_pending "pending" conn ~user:owner
          ~slug:"phvr-active" ~community:cid
      in
      let* () = load_none "pending" conn ~user:owner ~slug:"phvr-active" in
      (* A rejected request is history, not an active relation. *)
      let* () =
        Home_provisioning_fixture.review "reject" conn ~reviewer:moderator
          ~slug:"phvr-active" ~community_slug:"phvr-home" Rvs.Reject
      in
      let* _ = load_view "after reject" conn ~user:owner ~slug:"phvr-active" in
      (* Accepted closes it again. *)
      let* () =
        Home_provisioning_fixture.request_pending "second" conn ~user:owner
          ~slug:"phvr-active" ~community:cid
      in
      let* () =
        Home_provisioning_fixture.review "accept" conn ~reviewer:moderator
          ~slug:"phvr-active" ~community_slug:"phvr-home" Rvs.Accept
      in
      let* () = load_none "accepted" conn ~user:owner ~slug:"phvr-active" in
      (* Removal frees the slot and the entry reopens. *)
      let* () =
        remove "remove" conn ~actor:owner ~slug:"phvr-active"
          ~community_slug:"phvr-home"
      in
      let* _ = load_view "after removal" conn ~user:owner ~slug:"phvr-active" in
      Lwt.return_unit)

(* === suggested identity === *)

let suggestion_case =
  db_case
    "provisioning read: the suggested identity is exactly the project's own \
     validated identity" (fun conn ->
      let* owner = insert_user conn "phvr_sowner" in
      let* _ =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000004L ~slug:"phvr-suggest" ~name:"Phvr Suggested Name"
          ~description:"Suggested body."
      in
      let* view =
        load_view "suggestions" conn ~user:owner ~slug:"phvr-suggest"
      in
      Alcotest.(check string)
        "name suggestion" "Phvr Suggested Name"
        (Rm.suggested_community_name view);
      Alcotest.(check string)
        "slug suggestion" "phvr-suggest"
        (Rm.suggested_community_slug view);
      Alcotest.(check (option string))
        "description suggestion" (Some "Suggested body.")
        (Rm.suggested_community_description view);
      (* Accessors mirror the project itself — no second source of
         truth. *)
      let project = Rm.project view in
      Alcotest.(check string)
        "project name" "Phvr Suggested Name" (Rm.project_name project);
      Alcotest.(check string)
        "project slug" "phvr-suggest" (Rm.project_slug project);
      Alcotest.(check (option string))
        "project description" (Some "Suggested body.")
        (Rm.project_description project);
      Alcotest.(check string)
        "namespace login" "pfin-owner"
        (Rm.project_namespace_login project);
      Alcotest.(check string)
        "kind" "project"
        (Pi.string_of_kind (Rm.project_kind project));
      (* Every accepted suggestion is accepted by the form unchanged: the
         GET can never prefill a value the POST would reject. *)
      (match
         Phvf.of_fields
           [
             ("community_name", Rm.suggested_community_name view);
             ("community_slug", Rm.suggested_community_slug view);
             ( "community_description",
               Option.value ~default:""
                 (Rm.suggested_community_description view) );
           ]
       with
      | Ok parsed ->
          Alcotest.(check string)
            "round-trip name" "Phvr Suggested Name"
            (Phvf.community_name parsed);
          Alcotest.(check string)
            "round-trip slug" "phvr-suggest"
            (Phvf.community_slug parsed)
      | Error e ->
          Alcotest.failf "suggestion rejected by the form: %s"
            (Home_provisioning_fixture.phvf_err e));
      Lwt.return_unit)

let suggestion_shapes_case =
  db_case
    "provisioning read: every canonical project slug is a usable \
     community-slug suggestion today" (fun conn ->
      let* owner = insert_user conn "phvr_shapes" in
      let long = "phvr-" ^ String.make 75 'a' in
      let* _ =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000005L ~slug:long
      in
      let* view = load_view "long slug" conn ~user:owner ~slug:long in
      Alcotest.(check string)
        "80-character slug suggested whole" long
        (Rm.suggested_community_slug view);
      Alcotest.(check int) "exactly 80 characters" 80 (String.length long);
      let* _ =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000006L ~slug:"phvr-a-b-c"
      in
      let* view = load_view "hyphens" conn ~user:owner ~slug:"phvr-a-b-c" in
      Alcotest.(check string)
        "hyphenated slug suggested whole" "phvr-a-b-c"
        (Rm.suggested_community_slug view);
      (* No description means no suggestion, never an empty string. *)
      Alcotest.(check (option string))
        "absent description stays absent" None
        (Rm.suggested_community_description view);
      Lwt.return_unit)

let no_availability_case =
  db_case
    "provisioning read: an already-taken slug is still suggested — \
     availability is the provisioning store's answer, not this GET's"
    (fun conn ->
      let* owner = insert_user conn "phvr_taken" in
      let* _ =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000007L ~slug:"phvr-taken"
      in
      (* A community already occupies exactly that slug. *)
      let* _cid = insert_community ~name:"Phvr Taken" conn "phvr-taken" in
      let* view = load_view "taken" conn ~user:owner ~slug:"phvr-taken" in
      Alcotest.(check string)
        "suggestion unchanged" "phvr-taken"
        (Rm.suggested_community_slug view);
      Lwt.return_unit)

(* === durable revalidation === *)

let corruption_case =
  db_case
    "provisioning read: durable project corruption is Inconsistent_data, never \
     a suggestion" (fun conn ->
      let* owner = insert_user conn "phvr_cowner" in
      let* _, project =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000008L ~slug:"phvr-corrupt"
      in
      let* _ = load_view "healthy" conn ~user:owner ~slug:"phvr-corrupt" in
      let probe label q =
        let* () = exec conn label q project in
        let* () =
          load_expect label Rm.Inconsistent_data conn ~user:owner
            ~slug:"phvr-corrupt"
        in
        exec conn ("restore after " ^ label)
          Home_provisioning_fixture.q_restore_project project
      in
      let* () =
        probe "control byte in name"
          Home_provisioning_fixture.q_corrupt_name_control
      in
      let* () = probe "untrimmed name" q_corrupt_name_untrimmed in
      let* () = probe "control byte in description" q_corrupt_description in
      let* () =
        probe "unaddressable namespace login"
          Home_provisioning_fixture.q_corrupt_login
      in
      (* Restored data loads again — the failure was the row, not the
         query. *)
      let* _ = load_view "restored" conn ~user:owner ~slug:"phvr-corrupt" in
      Lwt.return_unit)

let storage_error_case =
  db_case
    "provisioning read: a storage failure surfaces as the payload-free \
     Storage_error" (fun conn ->
      let* () = exec conn "break search_path" q_break_search_path () in
      let* () =
        load_expect "broken schema" Rm.Storage_error conn ~user:1
          ~slug:"phvr-anything"
      in
      exec conn "reset search_path" q_reset_search_path ())

let privacy_case =
  db_case
    "provisioning read: no internal or GitHub identifier crosses the public \
     surface" (fun conn ->
      let* owner = insert_user conn "phvr_privacy" in
      let* _, project =
        Home_provisioning_fixture.make_project conn ~user:owner
          ~ext_id:949000009L ~slug:"phvr-privacy" ~description:"Privacy body."
      in
      let* view = load_view "privacy" conn ~user:owner ~slug:"phvr-privacy" in
      let p = Rm.project view in
      let surface =
        String.concat "|"
          [
            Rm.project_name p;
            Rm.project_slug p;
            Option.value ~default:"" (Rm.project_description p);
            Rm.project_namespace_login p;
            Pi.string_of_kind (Rm.project_kind p);
            Rm.suggested_community_name view;
            Rm.suggested_community_slug view;
            Option.value ~default:"" (Rm.suggested_community_description view);
          ]
      in
      List.iter
        (fun (what, needle) ->
          Alcotest.(check bool)
            ("surface free of " ^ what)
            false
            (Html_assert.contains surface needle))
        [
          ("permanent project id", Int64.to_string project);
          ("external installation id", "949000009");
          ("external account id", "949100009");
          ("owner user id", string_of_int owner);
        ];
      Lwt.return_unit)

let suite =
  [
    pure_inputs_case;
    authorization_case;
    verification_case;
    active_home_case;
    suggestion_case;
    suggestion_shapes_case;
    no_availability_case;
    corruption_case;
    storage_error_case;
    privacy_case;
  ]

let suites =
  (* Owner-authorized creation read model: pure input validation before
       SQL, steward-only authorization with no admin bypass, verification
       drift, the active-home exclusion and its reopening after closed
       history, suggested-identity mapping and its round-trip through the
       form, durable revalidation, storage failure, and the privacy
       sweep. Database-gated. *)
  [ ("project_home_provisioning_read_model", suite) ]
