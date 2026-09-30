module Phr = Earde.Project_home_relation

(* === Dedicated-home provisioning store
   (Project_home_provisioning_store) ===
   The single atomic transaction behind "Create a community home": pure
   validation before SQL, project/steward locking and its collapse,
   active-home arbitration, the complete private draft (community,
   membership, top_mod, General section, general channel, accepted
   relation), slug and active-home index arbitration under real
   concurrency, staged rollback injection, and the privacy sweep.
   Reserved external-installation-id range 950000001..950000999 (hence
   account ids 950100001..950100999, which also scope the
   permanent-project cleanup), phvs_% usernames, and phvs-% community
   slugs so no suite shares fixtures. Verified projects come only through
   the real draft/selection/finalization chain; competing relations come
   only through the real request/review/removal stores; identities come
   only through the real provisioning form. Every per-case wrapper
   disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Pv = Earde.Project_home_provisioning_store
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Rms = Earde.Project_home_removal_store

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let make_project = Home_provisioning_fixture.make_project
let insert_community = Community_fixture.insert_community
let contains = Html_assert.occurs
let status_of = Http_fixture.status_of

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 950100001 \
       AND 950100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       950100001 AND 950100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 950000001 AND 950000999)";
      "DELETE FROM communities WHERE slug LIKE 'phvs-%'";
      "DELETE FROM users WHERE username LIKE 'phvs_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       950000001 AND 950000999";
    ]

(* Test-only failure injection: one shared RAISE function plus one
   unconditional AFTER INSERT trigger per poisoned table, installed
   immediately before the single poisoned provision call and dropped
   under Lwt.finalize — within a case the store's own inserts are the
   only ones that can fire it. Production migrations are untouched. *)
let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION phvs_fail_fn() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN \
     RAISE EXCEPTION ''phvs fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS phvs_fail_fn()"

let poison_tables =
  [
    "communities";
    "community_members";
    "community_moderators";
    "community_sections";
    "channels";
    "community_projects";
  ]

let q_poison table =
  ddl
    (Printf.sprintf
       "CREATE TRIGGER phvs_fail_insert AFTER INSERT ON %s FOR EACH ROW \
        EXECUTE FUNCTION phvs_fail_fn()"
       table)

let q_unpoison table =
  ddl (Printf.sprintf "DROP TRIGGER IF EXISTS phvs_fail_insert ON %s" table)

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
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let with_second ~url f =
  let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
  let* conn2 = or_fail "second connect" conn2 in
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  Lwt.finalize (fun () -> f conn2) (fun () -> C2.disconnect ())

(* === pure input validation === *)

let pure_inputs_case =
  db_case "provision: invalid inputs rejected before any SQL"
    (fun ~url:_ _conn ->
      (* A deliberately unusable connection: pure validation must return
         without touching it — were any SQL attempted, the result would
         be Storage_error (or a test-failing exception), never the
         expected input error. *)
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let value = Home_provisioning_fixture.identity () in
      let expect label e ~actor ~slug =
        Home_provisioning_fixture.provision_expect label e dead ~actor ~slug
          value
      in
      let* () = expect "user id 0" Pv.Invalid_user_id ~actor:0 ~slug:"phvs-a" in
      let* () =
        expect "negative user id" Pv.Invalid_user_id ~actor:(-7) ~slug:"phvs-a"
      in
      let* () =
        expect "user checked before slug" Pv.Invalid_user_id ~actor:0
          ~slug:"NOT A SLUG"
      in
      Lwt_list.iter_s
        (fun bad ->
          expect "invalid project slug" Pv.Invalid_project_slug ~actor:1
            ~slug:bad)
        [
          "";
          "Phvs-Upper";
          "phvs slug";
          " phvs-a";
          "phvs-a ";
          "phvs_a";
          "phvs/a";
          "-phvs";
          "phvs-";
          "phvs--a";
          String.make 81 'a';
        ])

(* === successful provisioning === *)

let success_case =
  db_case
    "provision: a verified steward atomically creates the complete private \
     draft" (fun ~url conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* unrelated = insert_user conn "phvs_other" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000001L ~slug:"phvs-alpha"
      in
      let* project_before =
        find conn "project sig" Home_request_fixture.q_project_sig project
      in
      let* stewards_before =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project
          project
      in
      let* repos_before =
        find conn "repos" Connected_projects_fixture.q_count_repositories
          project
      in
      let name = "Phvs Alpha Home \xc3\xa8" in
      let description = "Prima riga.\nSeconda con\ttab \xe2\x98\x95" in
      let value =
        Home_provisioning_fixture.identity ~name ~slug:"phvs-alpha-home"
          ~description ()
      in
      let* _ =
        Home_provisioning_fixture.provision_ok "provision" conn ~actor:owner
          ~slug:"phvs-alpha" ~expect_slug:"phvs-alpha-home" value
      in
      let* _ =
        Home_provisioning_fixture.check_provisioned_draft "draft" conn
          ~actor:owner ~project ~slug:"phvs-alpha-home" ~name ~description
      in
      (* The project side is untouched: identity, verification,
         stewardship, and repositories. *)
      let* project_after =
        find conn "project sig after" Home_request_fixture.q_project_sig project
      in
      Alcotest.(check string) "project unchanged" project_before project_after;
      let* stewards_after =
        find conn "stewards after"
          Home_request_fixture.q_count_stewards_for_project project
      in
      Alcotest.(check int)
        "stewardship unchanged" stewards_before stewards_after;
      let* repos_after =
        find conn "repos after" Connected_projects_fixture.q_count_repositories
          project
      in
      Alcotest.(check int) "repositories unchanged" repos_before repos_after;
      (* The real /c/:slug route: the existing private-community
         authorization admits the creating steward and nobody else —
         the anonymous visitor and the unrelated user get the same
         generic 404 a missing community gets. *)
      let* response, body =
        Connected_projects_fixture.visit ~session_user_id:owner ~url
          ~slug:"phvs-alpha-home" ()
      in
      Alcotest.(check int) "steward reads the draft" 200 (status_of response);
      Alcotest.(check bool)
        "draft page carries the community name" true
        (contains ~needle:"Phvs Alpha Home" body);
      let* response, _ =
        Connected_projects_fixture.visit ~session_user_id:unrelated ~url
          ~slug:"phvs-alpha-home" ()
      in
      Alcotest.(check int)
        "unrelated user gets the generic 404" 404 (status_of response);
      let* response, _ =
        Connected_projects_fixture.visit ~url ~slug:"phvs-alpha-home" ()
      in
      Alcotest.(check int)
        "anonymous visitor gets the generic 404" 404 (status_of response);
      Lwt.return_unit)

(* === a second steward provisions === *)

let second_steward_case =
  db_case
    "provision: a second steward provisions and becomes the sole initial \
     member and top moderator" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* second = insert_user conn "phvs_second" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:950000002L ~slug:"phvs-second"
      in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, second, inst)
      in
      let* _ =
        Home_provisioning_fixture.provision_ok "second steward provisions" conn
          ~actor:second ~slug:"phvs-second" ~expect_slug:"phvs-second-home"
          (Home_provisioning_fixture.identity ~name:"Phvs Second Home"
             ~slug:"phvs-second-home" ())
      in
      let* cid, _ =
        Home_provisioning_fixture.check_provisioned_draft "second steward draft"
          conn ~actor:second ~project ~slug:"phvs-second-home"
          ~name:"Phvs Second Home" ~description:"<null>"
      in
      (* The first steward gains nothing automatically. *)
      let* owner_member =
        find conn "owner member" Home_provisioning_fixture.q_member_present
          (cid, owner)
      in
      Alcotest.(check int) "owner not a member" 0 owner_member;
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* owner_role =
        C.find_opt Home_provisioning_fixture.q_moderator_role (cid, owner)
      in
      let* owner_role = or_fail "owner role" owner_role in
      Alcotest.(check (option string)) "owner not a moderator" None owner_role;
      Lwt.return_unit)

(* === authorization collapse === *)

let admin_case =
  db_case
    "provision: a durable global admin without stewardship is \
     indistinguishable from a missing project" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* admin = insert_user conn "phvs_admin" in
      let* () =
        exec conn "make admin" Connected_projects_fixture.q_set_admin
          (admin, true)
      in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000003L ~slug:"phvs-admin"
      in
      let* () =
        Home_provisioning_fixture.provision_expect "durable admin"
          Pv.Project_unavailable conn ~actor:admin ~slug:"phvs-admin"
          (Home_provisioning_fixture.identity ~slug:"phvs-admin-home" ())
      in
      Home_provisioning_fixture.check_no_state "durable admin" conn ~project
        ~slug:"phvs-admin-home")

let unavailable_case =
  db_case
    "provision: missing, foreign, stale, revoked, and unstewarded projects \
     collapse into one error with no partial state" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* other = insert_user conn "phvs_other" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000004L ~slug:"phvs-unavail"
      in
      let value =
        Home_provisioning_fixture.identity ~slug:"phvs-unavail-home" ()
      in
      let expect label ~actor ~slug =
        Home_provisioning_fixture.provision_expect label Pv.Project_unavailable
          conn ~actor ~slug value
      in
      let* () = expect "missing project" ~actor:owner ~slug:"phvs-nowhere" in
      let* () = expect "foreign project" ~actor:other ~slug:"phvs-unavail" in
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* () = expect "stale project" ~actor:owner ~slug:"phvs-unavail" in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      let* () = expect "revoked project" ~actor:owner ~slug:"phvs-unavail" in
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      (* The creator without a steward row does not authorize through
         created_by_user_id. *)
      let* () =
        exec conn "remove stewardship" Home_request_fixture.q_delete_steward
          (project, owner)
      in
      let* () =
        expect "creator without stewardship" ~actor:owner ~slug:"phvs-unavail"
      in
      Home_provisioning_fixture.check_no_state "unavailable variants" conn
        ~project ~slug:"phvs-unavail-home")

(* === requested slug conflicts === *)

let slug_conflict_case =
  db_case
    "provision: an existing community slug loses cleanly, whether legacy or \
     network" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000005L ~slug:"phvs-conflict"
      in
      let probe label cid slug =
        let* sig_before =
          find conn "community sig" Home_request_fixture.q_community_sig cid
        in
        let* () =
          Home_provisioning_fixture.provision_expect label
            Pv.Community_slug_unavailable conn ~actor:owner
            ~slug:"phvs-conflict"
            (Home_provisioning_fixture.identity ~slug ())
        in
        let* sig_after =
          find conn "community sig after" Home_request_fixture.q_community_sig
            cid
        in
        Alcotest.(check string)
          (label ^ ": existing row unchanged")
          sig_before sig_after;
        let* members =
          find conn "members" Home_request_fixture.q_count_members cid
        in
        Alcotest.(check int) (label ^ ": no membership leak") 0 members;
        let* mods =
          find conn "moderators" Home_request_fixture.q_count_moderators cid
        in
        Alcotest.(check int) (label ^ ": no moderator leak") 0 mods;
        let* n =
          find conn "count" Home_provisioning_fixture.q_count_by_slug slug
        in
        Alcotest.(check int) (label ^ ": exactly one row keeps the slug") 1 n;
        let* relations =
          find conn "relations" Home_request_fixture.q_count_for_project project
        in
        Alcotest.(check int) (label ^ ": no relation") 0 relations;
        Lwt.return_unit
      in
      let* legacy = insert_community ~network:false conn "phvs-taken-legacy" in
      let* () = probe "legacy holder" legacy "phvs-taken-legacy" in
      let* network = insert_community conn "phvs-taken-net" in
      probe "network holder" network "phvs-taken-net")

(* === active relations === *)

let active_relation_case =
  db_case
    "provision: pending and accepted homes block, closed history and completed \
     removal do not" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* reviewer = insert_user conn "phvs_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:950000006L ~slug:"phvs-active"
      in
      let* target = insert_community conn "phvs-active-target" in
      let* () =
        exec conn "target top mod" Community_fixture.q_insert_moderator
          (reviewer, target, "top_mod")
      in
      let value =
        Home_provisioning_fixture.identity ~slug:"phvs-active-home" ()
      in
      (* Pending blocks, and the loser leaves no draft community. *)
      let relation =
        Home_request_fixture.phr_expect_ok
          (Phr.create_pending ~request_note:None)
      in
      let* r =
        Rq.create conn ~user_id:owner ~project_slug:"phvs-active"
          ~target_community_id:target ~relation
      in
      let* _ =
        match r with
        | Ok created -> Lwt.return created
        | Error _ -> Alcotest.fail "pending fixture failed"
      in
      let* () =
        Home_provisioning_fixture.provision_expect "pending blocks"
          Pv.Active_home_exists conn ~actor:owner ~slug:"phvs-active" value
      in
      let* n =
        find conn "no draft" Home_provisioning_fixture.q_count_by_slug
          "phvs-active-home"
      in
      Alcotest.(check int) "pending loser leaves no community" 0 n;
      (* Accepted blocks. *)
      let* r =
        Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:"phvs-active"
          ~target_community_slug:"phvs-active-target" ~decision:Rvs.Accept
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error _ -> Alcotest.fail "accept fixture failed"
      in
      let* () =
        Home_provisioning_fixture.provision_expect "accepted blocks"
          Pv.Active_home_exists conn ~actor:owner ~slug:"phvs-active" value
      in
      (* A completed removal frees the slot for a fresh draft. *)
      let* r =
        Rms.remove conn ~actor_user_id:owner ~project_slug:"phvs-active"
          ~community_slug:"phvs-active-target"
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error _ -> Alcotest.fail "removal fixture failed"
      in
      let* _ =
        Home_provisioning_fixture.provision_ok "provision after removal" conn
          ~actor:owner ~slug:"phvs-active" ~expect_slug:"phvs-active-home" value
      in
      (* Rejected history never blocks either. *)
      let* _, project2 =
        make_project conn ~user:owner ~ext_id:950000007L ~slug:"phvs-hist"
      in
      let relation =
        Home_request_fixture.phr_expect_ok
          (Phr.create_pending ~request_note:None)
      in
      let* r =
        Rq.create conn ~user_id:owner ~project_slug:"phvs-hist"
          ~target_community_id:target ~relation
      in
      let* _ =
        match r with
        | Ok created -> Lwt.return created
        | Error _ -> Alcotest.fail "second pending fixture failed"
      in
      let* r =
        Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:"phvs-hist"
          ~target_community_slug:"phvs-active-target" ~decision:Rvs.Reject
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error _ -> Alcotest.fail "reject fixture failed"
      in
      let* _ =
        Home_provisioning_fixture.provision_ok "provision over rejected history"
          conn ~actor:owner ~slug:"phvs-hist" ~expect_slug:"phvs-hist-home"
          (Home_provisioning_fixture.identity ~slug:"phvs-hist-home" ())
      in
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project2
      in
      Alcotest.(check int) "exactly one active relation" 1 active;
      Lwt.return_unit)

(* === staged rollback injection === *)

let rollback_case =
  db_case "provision: a failure at any stage rolls the whole draft back"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000008L ~slug:"phvs-poison"
      in
      let* project_before =
        find conn "project sig" Home_request_fixture.q_project_sig project
      in
      let exec_ddl label q =
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* r = C.exec q () in
        let* _ = or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "create fail fn" q_create_fail_fn in
      Lwt.finalize
        (fun () ->
          let* () =
            Lwt_list.iter_s
              (fun table ->
                let* () = exec_ddl ("poison " ^ table) (q_poison table) in
                Lwt.finalize
                  (fun () ->
                    let* () =
                      Home_provisioning_fixture.provision_expect
                        ("poisoned " ^ table) Pv.Storage_error conn ~actor:owner
                        ~slug:"phvs-poison"
                        (Home_provisioning_fixture.identity
                           ~slug:"phvs-poison-home" ())
                    in
                    let* () =
                      Home_provisioning_fixture.check_no_state
                        ("poisoned " ^ table) conn ~project
                        ~slug:"phvs-poison-home"
                    in
                    let* stewards =
                      find conn "stewards"
                        Home_request_fixture.q_count_stewards_for_project
                        project
                    in
                    Alcotest.(check int)
                      ("poisoned " ^ table ^ ": stewardship intact")
                      1 stewards;
                    let* project_after =
                      find conn "project sig" Home_request_fixture.q_project_sig
                        project
                    in
                    Alcotest.(check string)
                      ("poisoned " ^ table ^ ": project unchanged")
                      project_before project_after;
                    Lwt.return_unit)
                  (fun () -> exec_ddl ("unpoison " ^ table) (q_unpoison table)))
              poison_tables
          in
          (* With every poison removed the same inputs succeed — the
             failures above were the triggers, not the store. *)
          let* _ =
            Home_provisioning_fixture.provision_ok "clean run after poison" conn
              ~actor:owner ~slug:"phvs-poison" ~expect_slug:"phvs-poison-home"
              (Home_provisioning_fixture.identity ~slug:"phvs-poison-home" ())
          in
          Lwt.return_unit)
        (fun () ->
          let* () =
            Lwt_list.iter_s
              (fun table ->
                exec_ddl ("drop trigger " ^ table) (q_unpoison table))
              poison_tables
          in
          exec_ddl "drop fail fn" q_drop_fail_fn))

(* === concurrency === *)

let one_winner label expected_loser (ra, rb) =
  match (ra, rb) with
  | Ok w, Error e | Error e, Ok w ->
      Alcotest.(check string)
        (label ^ ": loser error")
        (Home_provisioning_fixture.error_str expected_loser)
        (Home_provisioning_fixture.error_str e);
      w
  | Ok _, Ok _ -> Alcotest.failf "%s: both succeeded" label
  | Error a, Error b ->
      Alcotest.failf "%s: both failed (%s, %s)" label
        (Home_provisioning_fixture.error_str a)
        (Home_provisioning_fixture.error_str b)

let same_project_race_case =
  db_case "provision: two provisions of one project leave exactly one draft"
    (fun ~url conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000009L ~slug:"phvs-race1"
      in
      with_second ~url (fun conn2 ->
          let* results =
            Lwt.both
              (Home_provisioning_fixture.provision conn ~actor:owner
                 ~slug:"phvs-race1"
                 (Home_provisioning_fixture.identity ~slug:"phvs-race1-a" ()))
              (Home_provisioning_fixture.provision conn2 ~actor:owner
                 ~slug:"phvs-race1"
                 (Home_provisioning_fixture.identity ~slug:"phvs-race1-b" ()))
          in
          let winner =
            one_winner "same project" Pv.Active_home_exists results
          in
          let winner_slug = Pv.community_slug winner in
          let loser_slug =
            if String.equal winner_slug "phvs-race1-a" then "phvs-race1-b"
            else "phvs-race1-a"
          in
          let* n =
            find conn "loser slug" Home_provisioning_fixture.q_count_by_slug
              loser_slug
          in
          Alcotest.(check int) "no orphan loser community" 0 n;
          let* _ =
            Home_provisioning_fixture.check_provisioned_draft "winner draft"
              conn ~actor:owner ~project ~slug:winner_slug
              ~name:"Phvs Community Home" ~description:"<null>"
          in
          let* active =
            find conn "active" Home_request_fixture.q_count_active_for_project
              project
          in
          Alcotest.(check int) "one active relation" 1 active;
          Lwt.return_unit))

let same_slug_race_case =
  db_case "provision: two projects racing for one slug leave one clean winner"
    (fun ~url conn ->
      let* owner_a = insert_user conn "phvs_owner" in
      let* owner_b = insert_user conn "phvs_second" in
      let* _, project_a =
        make_project conn ~user:owner_a ~ext_id:950000010L ~slug:"phvs-race2a"
      in
      let* _, project_b =
        make_project conn ~user:owner_b ~ext_id:950000011L ~slug:"phvs-race2b"
      in
      with_second ~url (fun conn2 ->
          let value =
            Home_provisioning_fixture.identity ~slug:"phvs-shared-home" ()
          in
          let* ra, rb =
            Lwt.both
              (Home_provisioning_fixture.provision conn ~actor:owner_a
                 ~slug:"phvs-race2a" value)
              (Home_provisioning_fixture.provision conn2 ~actor:owner_b
                 ~slug:"phvs-race2b" value)
          in
          let _ =
            one_winner "same slug" Pv.Community_slug_unavailable (ra, rb)
          in
          let winner_actor, winner_project, loser_project =
            match ra with
            | Ok _ -> (owner_a, project_a, project_b)
            | Error _ -> (owner_b, project_b, project_a)
          in
          let* n =
            find conn "one community" Home_provisioning_fixture.q_count_by_slug
              "phvs-shared-home"
          in
          Alcotest.(check int) "exactly one community with the slug" 1 n;
          let* _ =
            Home_provisioning_fixture.check_provisioned_draft "winner draft"
              conn ~actor:winner_actor ~project:winner_project
              ~slug:"phvs-shared-home" ~name:"Phvs Community Home"
              ~description:"<null>"
          in
          let* loser_relations =
            find conn "loser relations" Home_request_fixture.q_count_for_project
              loser_project
          in
          Alcotest.(check int) "loser project has no relation" 0 loser_relations;
          Lwt.return_unit))

let versus_request_case =
  db_case
    "provision: racing an existing-community request leaves one active relation"
    (fun ~url conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000012L ~slug:"phvs-race3"
      in
      let* target = insert_community conn "phvs-race3-target" in
      with_second ~url (fun conn2 ->
          let relation =
            Home_request_fixture.phr_expect_ok
              (Phr.create_pending ~request_note:None)
          in
          let* prov, req =
            Lwt.both
              (Home_provisioning_fixture.provision conn ~actor:owner
                 ~slug:"phvs-race3"
                 (Home_provisioning_fixture.identity ~slug:"phvs-race3-home" ()))
              (Rq.create conn2 ~user_id:owner ~project_slug:"phvs-race3"
                 ~target_community_id:target ~relation)
          in
          let* () =
            match (prov, req) with
            | Ok home, Error Rq.Active_home_exists ->
                Alcotest.(check string)
                  "provisioned home" "phvs-race3-home" (Pv.community_slug home);
                let* _ =
                  Home_provisioning_fixture.check_provisioned_draft
                    "provisioning won" conn ~actor:owner ~project
                    ~slug:"phvs-race3-home" ~name:"Phvs Community Home"
                    ~description:"<null>"
                in
                Lwt.return_unit
            | Error Pv.Active_home_exists, Ok _ ->
                (* The request won: a pending relation on the target and
                   no draft community anywhere. *)
                let* n =
                  find conn "no draft" Home_provisioning_fixture.q_count_by_slug
                    "phvs-race3-home"
                in
                Alcotest.(check int) "no draft community survives" 0 n;
                Lwt.return_unit
            | Ok _, Ok _ -> Alcotest.fail "race: both succeeded"
            | Error a, Error _ ->
                Alcotest.failf "race: both failed (provision %s)"
                  (Home_provisioning_fixture.error_str a)
            | Ok _, Error _ ->
                Alcotest.fail "race: unexpected request loser error"
            | Error e, _ ->
                Alcotest.failf "race: unexpected provision error %s"
                  (Home_provisioning_fixture.error_str e)
          in
          let* active =
            find conn "active" Home_request_fixture.q_count_active_for_project
              project
          in
          Alcotest.(check int) "exactly one active relation" 1 active;
          Lwt.return_unit))

let verification_loss_race_case =
  db_case
    "provision: a verification loss that commits first wins the project lock \
     race" (fun ~url conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000013L ~slug:"phvs-race4"
      in
      with_second ~url (fun conn2 ->
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "stale first" Home_request_fixture.q_set_verification
                  (project, "stale"))
              ~launch:(fun () ->
                Home_provisioning_fixture.provision conn ~actor:owner
                  ~slug:"phvs-race4"
                  (Home_provisioning_fixture.identity ~slug:"phvs-race4-home" ()))
          in
          let* () =
            match r with
            | Error Pv.Project_unavailable -> Lwt.return_unit
            | Ok _ -> Alcotest.fail "provisioned after verification loss"
            | Error e ->
                Alcotest.failf "unexpected error %s"
                  (Home_provisioning_fixture.error_str e)
          in
          Home_provisioning_fixture.check_no_state "verification loss" conn
            ~project ~slug:"phvs-race4-home"))

let steward_loss_race_case =
  db_case
    "provision: a stewardship revocation that commits first wins the lock race"
    (fun ~url conn ->
      let* owner = insert_user conn "phvs_owner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000014L ~slug:"phvs-race5"
      in
      with_second ~url (fun conn2 ->
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "revoke stewardship"
                  Home_request_fixture.q_delete_steward (project, owner))
              ~launch:(fun () ->
                Home_provisioning_fixture.provision conn ~actor:owner
                  ~slug:"phvs-race5"
                  (Home_provisioning_fixture.identity ~slug:"phvs-race5-home" ()))
          in
          let* () =
            match r with
            | Error Pv.Project_unavailable -> Lwt.return_unit
            | Ok _ -> Alcotest.fail "provisioned with revoked authority"
            | Error e ->
                Alcotest.failf "unexpected error %s"
                  (Home_provisioning_fixture.error_str e)
          in
          Home_provisioning_fixture.check_no_state "stewardship loss" conn
            ~project ~slug:"phvs-race5-home"))

(* === privacy sweep === *)

(* Durable corruption defenses that cannot be produced without relaxing
   production constraints stay defensive and undriven here:
   project_stewards.role is CHECK-bound to 'steward',
   open_source_projects.slug and verification_status are CHECK-bound,
   community_projects.status and relation_type are CHECK-bound, and a
   second active home is barred by the partial unique index. The
   reachable corruption shapes on the sibling read paths are already
   exercised by their own suites. *)

let privacy_case =
  db_case
    "provision: no credential-shaped or external-identifier fixture reaches \
     the draft" (fun ~url:_ conn ->
      let* owner = insert_user conn "phvs_priv" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:950000015L ~slug:"phvs-priv"
      in
      let name = "Phvs Private Home" in
      let* _ =
        Home_provisioning_fixture.provision_ok "provision" conn ~actor:owner
          ~slug:"phvs-priv" ~expect_slug:"phvs-priv-home"
          (Home_provisioning_fixture.identity ~name ~slug:"phvs-priv-home" ())
      in
      let* cid, state =
        Home_provisioning_fixture.community_state "community" conn
          "phvs-priv-home"
      in
      let* sections =
        collect conn "sections" Home_provisioning_fixture.q_section_sigs cid
      in
      let* channels =
        collect conn "channels" Home_provisioning_fixture.q_channel_sigs cid
      in
      let* relation_ids =
        collect conn "relations" Home_provisioning_fixture.q_relation_ids
          project
      in
      let* relation_blob =
        match relation_ids with
        | [ rid ] ->
            find conn "blob" Home_request_fixture.q_relation_text_blob rid
        | _ -> Alcotest.fail "expected one relation"
      in
      let blob =
        String.concat "|" ((state :: relation_blob :: sections) @ channels)
      in
      (* The intentionally supplied safe identity is present; nothing
         else from the steward's GitHub records or account is. *)
      Alcotest.(check bool)
        "supplied name present" true
        (contains ~needle:name blob);
      List.iter
        (fun (what, marker) ->
          Alcotest.(check bool)
            ("draft carries no " ^ what)
            false
            (contains ~needle:marker blob))
        [
          ("external installation id", "950000015");
          ("external account id", "950100015");
          ("external repository id", "950400015");
          ("account email", "@test.invalid");
          ("installation login", "pfin-owner");
        ];
      Lwt.return_unit)

let suite =
  [
    pure_inputs_case;
    success_case;
    second_steward_case;
    admin_case;
    unavailable_case;
    slug_conflict_case;
    active_relation_case;
    rollback_case;
    same_project_race_case;
    same_slug_race_case;
    versus_request_case;
    verification_loss_race_case;
    steward_loss_race_case;
    privacy_case;
  ]

let suites =
  (* Dedicated-home provisioning store: pure validation before SQL,
       the complete atomic private draft (community, membership,
       top_mod, shell, accepted relation), authorization collapse, slug
       and active-home arbitration, staged rollback injection,
       deterministic concurrency races, and the privacy sweep.
       Database-gated. *)
  [ ("project_home_provisioning_store", suite) ]
