module Phr = Earde.Project_home_relation

(* ===== Accepted project-home removal (Project_home_removal_store) =====
   Transactional accepted → removed transition of one exact home relation,
   authorized by any of the three current durable authorities (project
   steward, target top moderator, durable global administrator). Database-
   gated (EARDE_TEST_DATABASE_URL, same opt-in as Mod_scope) with its own
   reserved external-installation-id range 947200001..947200999 (hence
   account ids 947300001..947300999, which also scope the permanent-project
   cleanup), phrm_% usernames, and phrm-% community slugs so no suite shares
   fixtures. Accepted relations are built only through production paths: the
   real request store plus the real review store for a reviewed home, and a
   schema-valid production-shaped insert for a provisioned home (the
   provisioning store does not exist yet). Relation domain values come only
   through the public pure constructors. Every per-case wrapper disconnects
   deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rm = Earde.Project_home_removal_store

module Rq = Earde.Project_home_request_store

module Rv = Earde.Project_home_review_store

let status_str = Phr.string_of_status

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let make_project = Home_request_fixture.make_project

let insert_community = Community_fixture.insert_community

let contains = Html_assert.occurs

let relation_row = Home_request_fixture.relation_row

let serialized_mutation_first = Db_fixture.serialized_mutation_first

(* Same dependency order as the sibling suites. Users are deleted after
   communities so moderator rows cascade from either side cleanly. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 947300001 AND 947300999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 947300001 AND 947300999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 947200001 AND 947200999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phrm-%'"
    ; "DELETE FROM users WHERE username LIKE 'phrm_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 947200001 AND 947200999"
    ]

(* An automatically provisioned accepted home: no requester, no reviewer,
   no note — exactly the shape the production status CHECK admits for
   'accepted'. No store writes this yet, so the row is built directly
   rather than through a test-only constructor. *)
let q_provision_accepted =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, reviewed_at) \
   VALUES ($1, $2, 'home', 'accepted', NOW()) RETURNING id"

(* Everything removal must leave byte-identical: both foreign keys, the
   relation type, requester, reviewer, the private note, and the exact
   review and creation instants. Status, removed_at and updated_at are
   deliberately absent — those three are what removal writes. *)
let q_relation_stable_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT project_id::text || '|' || community_id::text || '|' || \
          relation_type || '|' || \
          COALESCE(requested_by_user_id::text, '<null>') || '|' || \
          COALESCE(reviewed_by_user_id::text, '<null>') || '|' || \
          COALESCE(request_note, '<null>') || '|' || \
          COALESCE(reviewed_at::text, '<null>') || '|' || \
          created_at::text \
   FROM community_projects WHERE id = $1"

(* Removal-time coherence, NULL-safe: presence plus ordering against the
   review and creation instants (a NULL comparison coalesces to FALSE,
   never a decode error). *)
let q_removed_times =
  (Caqti_type.int64 ->! Caqti_type.(t2 (t2 bool bool) (t2 bool bool)))
  "SELECT removed_at IS NOT NULL, \
          COALESCE(removed_at >= reviewed_at, FALSE), \
          COALESCE(removed_at >= created_at, FALSE), \
          updated_at >= created_at \
   FROM community_projects WHERE id = $1"

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM community_projects WHERE id = $1"

(* Test-only failure injection for the rollback case: an AFTER UPDATE
   trigger scoped to one reserved note value, installed only once the
   accepted fixture already exists so the review store's own update is
   untouched. Installed and dropped inside that case alone; production
   migrations are untouched. *)
let phrm_poison_note = "phrm poison marker"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION phrm_fail_update_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''phrm fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER phrm_fail_update
   AFTER UPDATE ON community_projects
   FOR EACH ROW WHEN (NEW.request_note = 'phrm poison marker')
   EXECUTE FUNCTION phrm_fail_update_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS phrm_fail_update ON community_projects"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS phrm_fail_update_fn()"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way, and the
   connection is disconnected deterministically. *)
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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () -> Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f conn))

(* === call helpers === *)

let remove conn ~actor ~slug ~community =
  Rm.remove conn ~actor_user_id:actor ~project_slug:slug
    ~community_slug:community

let remove_ok label conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok removed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str Phr.Removed)
        (status_str (Rm.resulting_status removed));
      Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (Connected_projects_fixture.error_str e)

let remove_expect label expected conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label (Connected_projects_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string) label (Connected_projects_fixture.error_str expected) (Connected_projects_fixture.error_str e);
      Lwt.return_unit

(* === fixtures === *)

(* Pending requests come only through the real request store and the real
   pure constructor. *)
let request_ok label conn ~user ~slug ~community ?note () =
  let relation = Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:note) in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok created -> Lwt.return (Rq.relation_id created)
  | Error e -> Alcotest.failf "%s: %s" label (Home_request_fixture.error_str e)

let review_ok label conn ~reviewer ~slug ~community decision =
  let* r =
    Rv.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community ~decision
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (Home_review_fixture.error_str e)

(* A reviewed accepted home, produced exactly as production produces one:
   the steward's request through the request store, then the target
   moderator's acceptance through the review store. *)
let reviewed_home label conn ~owner ~reviewer ~slug ~community_id
    ~community_slug ?note () =
  let* rid =
    request_ok (label ^ ": request") conn ~user:owner ~slug
      ~community:community_id ?note ()
  in
  let* () =
    review_ok (label ^ ": accept") conn ~reviewer ~slug
      ~community:community_slug Rv.Accept
  in
  Lwt.return rid

let provisioned_home label conn ~project ~community_id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rid = C.find q_provision_accepted (project, community_id) in
  or_fail label rid

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, role)

let add_top_mod conn ~user ~community =
  add_role conn ~user ~community "top_mod"

let stable_sig conn id = find conn "stable sig" q_relation_stable_sig id

let status_of conn id = find conn "status" q_status id

let check_accepted_unchanged label conn id before =
  let* after = stable_sig conn id in
  Alcotest.(check string) (label ^ ": relation unchanged") before after;
  let* status = status_of conn id in
  Alcotest.(check string) (label ^ ": still accepted") "accepted" status;
  let* (present, _), (_, _) = find conn "times" q_removed_times id in
  Alcotest.(check bool) (label ^ ": removed_at still NULL") false present;
  Lwt.return_unit

(* The full removed-row contract: exactly the three written columns
   changed, all timestamps coherent, everything else byte-identical. *)
let check_removed label conn id before =
  let* status = status_of conn id in
  Alcotest.(check string) (label ^ ": status removed") "removed" status;
  let* after = stable_sig conn id in
  Alcotest.(check string)
    (label ^ ": requester, reviewer, note and timestamps preserved")
    before after;
  let* (present, after_reviewed), (after_created, updated_ge) =
    find conn "removed times" q_removed_times id
  in
  Alcotest.(check bool) (label ^ ": removed_at present") true present;
  Alcotest.(check bool) (label ^ ": removed_at >= reviewed_at") true
    after_reviewed;
  Alcotest.(check bool) (label ^ ": removed_at >= created_at") true
    after_created;
  Alcotest.(check bool) (label ^ ": updated_at coherent") true updated_ge;
  Lwt.return_unit

(* One winner plus one exact loser, order unasserted. *)
let one_winner label (ra, rb) =
  match (ra, rb) with
  | Ok w, Error Rm.Removal_unavailable | Error Rm.Removal_unavailable, Ok w ->
      w
  | Ok _, Ok _ -> Alcotest.failf "%s: both succeeded" label
  | Error a, Error b ->
      Alcotest.failf "%s: both failed (%s, %s)" label (Connected_projects_fixture.error_str a)
        (Connected_projects_fixture.error_str b)
  | Ok _, Error e | Error e, Ok _ ->
      Alcotest.failf "%s: unexpected loser error %s" label (Connected_projects_fixture.error_str e)

(* === pure input validation === *)

let pure_inputs_case =
  db_case "removal: invalid inputs rejected before any SQL" (fun _conn ->
      (* A deliberately unusable connection: pure validation must return
         without touching it — were any SQL attempted, the result would be
         Storage_error (or a test-failing exception), never the expected
         input error. *)
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let expect label e ~actor ~slug ~community =
        remove_expect label e dead ~actor ~slug ~community
      in
      let* () =
        expect "user id 0" Rm.Invalid_user_id ~actor:0 ~slug:"phrm-a"
          ~community:"phrm-c"
      in
      let* () =
        expect "negative user id" Rm.Invalid_user_id ~actor:(-7)
          ~slug:"phrm-a" ~community:"phrm-c"
      in
      let* () =
        expect "user checked before slugs" Rm.Invalid_user_id ~actor:0
          ~slug:"NOT A SLUG" ~community:"also bad"
      in
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            expect "invalid project slug" Rm.Invalid_project_slug ~actor:1
              ~slug:bad ~community:"phrm-c")
          [ ""
          ; "Phrm-Upper"
          ; "phrm slug"
          ; " phrm-a"
          ; "phrm-a "
          ; "phrm_a"
          ; "phrm/a"
          ; "-phrm"
          ; "phrm-"
          ; "phrm--a"
          ; String.make 81 'a'
          ]
      in
      let* () =
        expect "project slug checked before community slug"
          Rm.Invalid_project_slug ~actor:1 ~slug:"phrm--a"
          ~community:"also bad"
      in
      Lwt_list.iter_s
        (fun bad ->
          expect "invalid community slug" Rm.Invalid_community_slug ~actor:1
            ~slug:"phrm-a" ~community:bad)
        [ ""
        ; "phrm c"
        ; " phrm-c"
        ; "phrm-c "
        ; "phrm/c"
        ; "phrm\tc"
        ; "phrm\nc"
        ; "phrm\x01c"
        ; "phrm\x7fc"
        ])

(* === project-side authorization === *)

let steward_removal_case =
  db_case
    "removal: a project steward removes a reviewed home, changing only \
     status and the removal timestamps" (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200001L ~slug:"phrm-steward"
      in
      let* cid = insert_community conn "phrm-steward-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let note = "Prima riga — nota privata \xe2\x98\x95" in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-steward"
          ~community_id:cid ~community_slug:"phrm-steward-home" ~note ()
      in
      let* before = stable_sig conn rid in
      let* project_before = find conn "project sig" Home_request_fixture.q_project_sig project in
      let* community_before =
        find conn "community sig" Home_request_fixture.q_community_sig cid
      in
      let* role_before = find conn "role" Home_review_fixture.q_role_sig (reviewer, cid) in
      let* repos_before = find conn "repos" Connected_projects_fixture.q_count_repositories project in
      (* The steward is neither a member nor a moderator of the target. *)
      let* () =
        remove_ok "steward removes" conn ~actor:owner ~slug:"phrm-steward"
          ~community:"phrm-steward-home"
      in
      let* () = check_removed "steward removal" conn rid before in
      let* ((rp, rc), (rtype, _)), ((req, rev), (stored_note, _)) =
        relation_row conn rid
      in
      Alcotest.(check int64) "same project fk" project rp;
      Alcotest.(check int) "same community fk" cid rc;
      Alcotest.(check string) "relation type home" "home" rtype;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option int)) "reviewer preserved, not overwritten"
        (Some reviewer) rev;
      Alcotest.(check (option string)) "note preserved byte-exact"
        (Some note) stored_note;
      (* Nothing else in the world moved. *)
      let* project_after =
        find conn "project sig after" Home_request_fixture.q_project_sig project
      in
      let* community_after =
        find conn "community sig after" Home_request_fixture.q_community_sig cid
      in
      Alcotest.(check string) "project unchanged" project_before project_after;
      Alcotest.(check string) "community unchanged" community_before
        community_after;
      let* repos_after = find conn "repos after" Connected_projects_fixture.q_count_repositories project in
      Alcotest.(check int) "project content unchanged" repos_before repos_after;
      let* members = find conn "members" Home_request_fixture.q_count_members cid in
      Alcotest.(check int) "no membership created or removed" 0 members;
      let* mods = find conn "moderators" Home_request_fixture.q_count_moderators cid in
      Alcotest.(check int) "moderator roster unchanged" 1 mods;
      let* role_after = find conn "role after" Home_review_fixture.q_role_sig (reviewer, cid) in
      Alcotest.(check string) "moderator role unchanged" role_before role_after;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
      in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "exactly the one historical row" 1 total;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project project
      in
      Alcotest.(check int) "active home slot freed" 0 active;
      Lwt.return_unit)

let steward_variants_case =
  db_case
    "removal: a second steward removes, and stale or revoked verification \
     never blocks removal" (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* second = insert_user conn "phrm_steward2" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:947200002L ~slug:"phrm-stale"
      in
      let* cid = insert_community conn "phrm-stale-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, second, inst)
      in
      (* Stale verification: the home must still be separable. *)
      let* rid1 =
        reviewed_home "stale home" conn ~owner ~reviewer ~slug:"phrm-stale"
          ~community_id:cid ~community_slug:"phrm-stale-home" ()
      in
      let* before1 = stable_sig conn rid1 in
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification (project, "stale")
      in
      let* () =
        remove_ok "second steward removes a stale project's home" conn
          ~actor:second ~slug:"phrm-stale" ~community:"phrm-stale-home"
      in
      let* () = check_removed "stale removal" conn rid1 before1 in
      (* Revoked verification: likewise. The request store still needs a
         verified project to create the next home, so verification is
         restored first and revoked afterwards — exactly the production
         order. *)
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* rid2 =
        reviewed_home "revoked home" conn ~owner ~reviewer ~slug:"phrm-stale"
          ~community_id:cid ~community_slug:"phrm-stale-home" ()
      in
      let* before2 = stable_sig conn rid2 in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification (project, "revoked")
      in
      let* () =
        remove_ok "original steward removes a revoked project's home" conn
          ~actor:owner ~slug:"phrm-stale" ~community:"phrm-stale-home"
      in
      let* () = check_removed "revoked removal" conn rid2 before2 in
      let* project_sig = find conn "project sig" Home_request_fixture.q_project_sig project in
      Alcotest.(check bool) "verification untouched by removal" true
        (contains ~needle:"revoked" project_sig);
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "both historical rows retained" 2 total;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project project
      in
      Alcotest.(check int) "no active relation left" 0 active;
      Lwt.return_unit)

(* === community-side and administrator authorization === *)

let community_side_case =
  db_case
    "removal: target top moderators and durable admins remove reviewed and \
     provisioned homes" (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* first_mod = insert_user conn "phrm_mod1" in
      let* second_mod = insert_user conn "phrm_mod2" in
      let* admin = insert_user conn "phrm_admin" in
      let* () = exec conn "grant admin" Community_fixture.q_set_admin (admin, true) in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200003L ~slug:"phrm-modside"
      in
      let* cid = insert_community conn "phrm-modside-home" in
      let* () = add_top_mod conn ~user:first_mod ~community:cid in
      let* () = add_top_mod conn ~user:second_mod ~community:cid in
      (* A target top moderator who is not a project steward removes a
         reviewed home. *)
      let* rid1 =
        reviewed_home "reviewed" conn ~owner ~reviewer:first_mod
          ~slug:"phrm-modside" ~community_id:cid
          ~community_slug:"phrm-modside-home" ~note:"Nota della richiesta" ()
      in
      let* before1 = stable_sig conn rid1 in
      let* () =
        remove_ok "target top mod removes" conn ~actor:first_mod
          ~slug:"phrm-modside" ~community:"phrm-modside-home"
      in
      let* () = check_removed "top mod removal" conn rid1 before1 in
      (* A second independently appointed top moderator removes a
         provisioned home — no requester, no reviewer, no note. *)
      let* rid2 = provisioned_home "provisioned" conn ~project ~community_id:cid in
      let* before2 = stable_sig conn rid2 in
      let* () =
        remove_ok "second top mod removes a provisioned home" conn
          ~actor:second_mod ~slug:"phrm-modside"
          ~community:"phrm-modside-home"
      in
      let* () = check_removed "provisioned removal" conn rid2 before2 in
      let* _, ((req, rev), (note, _)) = relation_row conn rid2 in
      Alcotest.(check (option int)) "provisioned requester stays NULL" None
        req;
      Alcotest.(check (option int)) "provisioned reviewer stays NULL" None rev;
      Alcotest.(check (option string)) "provisioned note stays NULL" None note;
      (* A durable global administrator — neither member nor moderator of
         the target, nor a steward of the project — removes. *)
      let* rid3 =
        reviewed_home "admin target" conn ~owner ~reviewer:second_mod
          ~slug:"phrm-modside" ~community_id:cid
          ~community_slug:"phrm-modside-home" ()
      in
      let* before3 = stable_sig conn rid3 in
      let* () =
        remove_ok "durable admin removes" conn ~actor:admin
          ~slug:"phrm-modside" ~community:"phrm-modside-home"
      in
      let* () = check_removed "admin removal" conn rid3 before3 in
      let* mods = find conn "moderators" Home_request_fixture.q_count_moderators cid in
      Alcotest.(check int) "moderator roster unchanged" 2 mods;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
      in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "three historical rows" 3 total;
      Lwt.return_unit)

let multiple_authority_case =
  db_case
    "removal: an actor holding two authorities still performs one removal"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200004L ~slug:"phrm-both"
      in
      let* cid = insert_community conn "phrm-both-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      (* The steward is independently appointed top moderator of the
         target. *)
      let* () = add_top_mod conn ~user:owner ~community:cid in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-both"
          ~community_id:cid ~community_slug:"phrm-both-home" ()
      in
      let* before = stable_sig conn rid in
      let* () =
        remove_ok "steward and top mod removes once" conn ~actor:owner
          ~slug:"phrm-both" ~community:"phrm-both-home"
      in
      let* () = check_removed "dual-authority removal" conn rid before in
      (* Exactly one durable removal, and no authorization-source field
         exists to record which authority qualified. *)
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "exactly one relation row" 1 total;
      let* () =
        remove_expect "replay" Rm.Removal_unavailable conn ~actor:owner
          ~slug:"phrm-both" ~community:"phrm-both-home"
      in
      let* total = find conn "total after replay" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "still exactly one relation row" 1 total;
      Lwt.return_unit)

(* === unauthorized actors === *)

let unauthorized_case =
  db_case "removal: every insufficient authority collapses identically"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* creator_only = insert_user conn "phrm_creator" in
      let* other_owner = insert_user conn "phrm_otherowner" in
      let* member = insert_user conn "phrm_member" in
      let* outsider = insert_user conn "phrm_outsider" in
      let* low_mod = insert_user conn "phrm_lowmod" in
      let* legacy_mod = insert_user conn "phrm_legacymod" in
      let* other_mod = insert_user conn "phrm_othermod" in
      let* removed_mod = insert_user conn "phrm_removedmod" in
      let* downgraded = insert_user conn "phrm_downgraded" in
      let* adminish = insert_user conn "phrm_adminish" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200005L ~slug:"phrm-unauth"
      in
      (* An unrelated project, whose steward has no authority here. *)
      let* _, _other_project =
        make_project conn ~user:other_owner ~ext_id:947200006L
          ~slug:"phrm-unauth-elsewhere"
      in
      let* cid = insert_community conn "phrm-unauth-home" in
      let* other_cid = insert_community conn "phrm-unauth-other" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () = exec conn "member" Community_fixture.q_insert_member (member, cid) in
      let* () = add_role conn ~user:low_mod ~community:cid "mod" in
      let* () = add_role conn ~user:legacy_mod ~community:cid "legacy_mod" in
      let* () = add_top_mod conn ~user:other_mod ~community:other_cid in
      let* () = add_top_mod conn ~user:removed_mod ~community:cid in
      let* () =
        exec conn "remove role" Community_fixture.q_remove_moderator (removed_mod, cid)
      in
      let* () = add_top_mod conn ~user:downgraded ~community:cid in
      let* () =
        exec conn "downgrade role" Community_fixture.q_set_moderator_role
          (downgraded, cid, "mod")
      in
      (* Session/admin-shaped without durable backing: users.is_admin stays
         FALSE. *)
      let* () =
        exec conn "explicit non-admin" Community_fixture.q_set_admin (adminish, false)
      in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-unauth"
          ~community_id:cid ~community_slug:"phrm-unauth-home"
          ~note:"Nota privata" ()
      in
      (* Creation provenance is moved onto a user who never was a steward,
         and the original steward's link is deleted — creator and former
         steward alike must now be refused. *)
      let* () =
        exec conn "creator provenance" Home_request_fixture.q_set_created_by
          (project, creator_only)
      in
      let* () =
        exec conn "delete steward" Home_request_fixture.q_delete_steward (project, owner)
      in
      let* before = stable_sig conn rid in
      let expect label actor =
        remove_expect label Rm.Actor_unauthorized conn ~actor
          ~slug:"phrm-unauth" ~community:"phrm-unauth-home"
      in
      let* () = expect "project creator without stewardship" creator_only in
      let* () = expect "removed steward" owner in
      let* () = expect "steward of an unrelated project" other_owner in
      let* () = expect "ordinary community member" member in
      let* () = expect "non-member" outsider in
      let* () = expect "community mod role" low_mod in
      let* () = expect "legacy_mod role" legacy_mod in
      let* () = expect "moderator of another community" other_mod in
      let* () = expect "removed top moderator" removed_mod in
      let* () = expect "downgraded top moderator" downgraded in
      let* () = expect "admin-shaped without durable flag" adminish in
      let* () = check_accepted_unchanged "after all refusals" conn rid before in
      (* No refusal granted, revoked or altered any role or link. *)
      (* The reviewer's top_mod row, the 'mod' and 'legacy_mod' rows, and
         the downgraded former top moderator; the removed one is gone. *)
      let* mods = find conn "moderators" Home_request_fixture.q_count_moderators cid in
      Alcotest.(check int) "moderator roster unchanged" 4 mods;
      let* members = find conn "members" Home_request_fixture.q_count_members cid in
      Alcotest.(check int) "membership unchanged" 1 members;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
      in
      Alcotest.(check int) "stewardship unchanged" 0 stewards;
      (* The still-qualifying top moderator removes normally. *)
      remove_ok "target top mod still removes" conn ~actor:reviewer
        ~slug:"phrm-unauth" ~community:"phrm-unauth-home")

(* === project and community availability === *)

let availability_case =
  db_case_lifecycle_relaxed
    "removal: missing project and community collapse; valid lifecycle \
     drift stays removable" (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200007L ~slug:"phrm-avail"
      in
      let* cid = insert_community conn "phrm-avail-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-avail"
          ~community_id:cid ~community_slug:"phrm-avail-home" ()
      in
      let* before = stable_sig conn rid in
      let* () =
        remove_expect "missing project" Rm.Project_unavailable conn
          ~actor:owner ~slug:"phrm-absent" ~community:"phrm-avail-home"
      in
      let* () =
        remove_expect "missing community" Rm.Community_unavailable conn
          ~actor:owner ~slug:"phrm-avail" ~community:"phrm-nowhere"
      in
      let* () = check_accepted_unchanged "after availability probes" conn rid before in
      (* Each drifted lifecycle shape in turn: the home was accepted while
         the community was eligible, and must stay removable afterwards.
         The network setup draft is deliberately absent here — see the
         separate probe below. *)
      let drift =
        [ ("fully private", Community_fixture.q_make_private)
        ; ("legacy non-network", Community_fixture.q_make_legacy)
        ]
      in
      let* () =
        Lwt_list.iteri_s
          (fun i (label, q_drift) ->
            (* The first iteration reuses the home already accepted above;
               later ones are re-established on the restored community. *)
            let* rid =
              if i = 0 then Lwt.return rid
              else (
                let* () =
                  exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
                in
                reviewed_home (label ^ ": home") conn ~owner ~reviewer
                  ~slug:"phrm-avail" ~community_id:cid
                  ~community_slug:"phrm-avail-home" ())
            in
            let* before = stable_sig conn rid in
            let* () = exec conn (label ^ ": drift") q_drift cid in
            let* () =
              remove_ok (label ^ ": still removable") conn ~actor:owner
                ~slug:"phrm-avail" ~community:"phrm-avail-home"
            in
            check_removed (label ^ ": removed") conn rid before)
          drift
      in
      (* Drift into the exact unpublished network setup draft is the one
         shape that is not ordinary drift. A dedicated draft's home is
         auto-provisioned with no requester, reviewer or note; a
         *moderator-reviewed* accepted home under that lifecycle is
         contradictory provenance, so removal refuses it as corruption
         rather than detaching it. (The provisioned shape under the same
         lifecycle is the protected case, answered with the collapsed
         Removal_unavailable — covered by the dedicated protection
         suite.) *)
      let* () = exec conn "restore eligible" Home_review_fixture.q_make_eligible cid in
      let* draft_rid =
        reviewed_home "setup draft: home" conn ~owner ~reviewer
          ~slug:"phrm-avail" ~community_id:cid
          ~community_slug:"phrm-avail-home" ()
      in
      let* draft_before = stable_sig conn draft_rid in
      let* () = exec conn "setup draft: drift" Community_fixture.q_make_draft_state cid in
      let* () =
        remove_expect "reviewed home under a setup draft is corruption"
          Rm.Inconsistent_data conn ~actor:owner ~slug:"phrm-avail"
          ~community:"phrm-avail-home"
      in
      let* () =
        check_accepted_unchanged "after the draft refusal" conn draft_rid
          draft_before
      in
      (* Restored to an ordinary lifecycle, the very same row removes
         normally — proving the refusal was about the draft state, not the
         row. *)
      let* () = exec conn "restore eligible" Home_review_fixture.q_make_eligible cid in
      let* () =
        remove_ok "restored lifecycle removes" conn ~actor:owner
          ~slug:"phrm-avail" ~community:"phrm-avail-home"
      in
      let* () = check_removed "restored lifecycle" conn draft_rid draft_before in
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "one historical row per drifted removal" 3 total;
      Lwt.return_unit)

(* === durable corruption === *)

let corruption_case =
  db_case_lifecycle_relaxed "removal: malformed durable project, community and relation data"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200008L ~slug:"phrm-corrupt"
      in
      let* cid = insert_community conn "phrm-corrupt-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-corrupt"
          ~community_id:cid ~community_slug:"phrm-corrupt-home"
          ~note:"Nota canonica" ()
      in
      let* before = stable_sig conn rid in
      let expect label =
        remove_expect label Rm.Inconsistent_data conn ~actor:owner
          ~slug:"phrm-corrupt" ~community:"phrm-corrupt-home"
      in
      (* Community shapes: an empty display name, a mixed published flag
         pair, a draft leaking through public listing, and a private
         community still discoverable. Each is restored before the next. *)
      (* Blank and control-byte names are now barred by the scoped
         identity constraints; they are dropped for these probes alone
         and restored under Lwt.finalize once the name is canonical
         again. *)
      let* () = Network_community_constraints.drop conn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = exec conn "empty name" Community_fixture.q_set_name (cid, "") in
            let* () = expect "blank community name" in
            let* () =
              exec conn "control-byte name" Community_fixture.q_set_name
                (cid, "phrm\x01corrupt")
            in
            expect "control byte in community name")
          (fun () ->
            let* () =
              exec conn "restore name" Community_fixture.q_set_name
                (cid, "phrm-corrupt-home")
            in
            Network_community_constraints.restore conn)
      in
      let* () = exec conn "mix flags" Home_review_fixture.q_mix_flags cid in
      let* () = expect "mixed publication flags" in
      let* () = exec conn "restore eligible" Home_review_fixture.q_make_eligible cid in
      let* () = exec conn "leaky draft" Home_review_fixture.q_leaky_draft cid in
      let* () = expect "publicly listed draft" in
      let* () = exec conn "restore eligible" Home_review_fixture.q_make_eligible cid in
      let* () = exec conn "leaky private" Home_review_fixture.q_leaky_private cid in
      let* () = expect "discoverable private community" in
      let* () = exec conn "restore eligible" Home_review_fixture.q_make_eligible cid in
      (* Relation shapes: a stored note outside the canonical shape cannot
         be reconstructed byte-exactly through the pure constructor. *)
      let* () = exec conn "pad note" Home_review_fixture.q_pad_note rid in
      let* () = expect "non-canonical padded note" in
      let* () = exec conn "control-byte note" Home_review_fixture.q_control_note rid in
      let* () = expect "forbidden control byte in note" in
      let* () = exec conn "clear note" Home_review_fixture.q_clear_note rid in
      (* An unknown project verification status is blocked by the
         production CHECK, so the constraint is dropped and restored around
         this one probe under Lwt.finalize — production migrations are
         untouched and every other suite sees the constraint in place. *)
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      let* () =
        exec_ddl "drop verification check" Connected_projects_fixture.q_drop_verification_check
      in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () =
              exec conn "unknown verification" Home_request_fixture.q_set_verification
                (project, "phrm-unknown")
            in
            expect "unknown project verification status")
          (fun () ->
            let* () =
              exec conn "restore verified" Home_request_fixture.q_set_verification
                (project, "verified")
            in
            exec_ddl "restore verification check"
              Connected_projects_fixture.q_add_verification_check)
      in
      (* A reviewer id or removal timestamp on an accepted row, an absent
         reviewed_at, a non-'home' relation type, and a non-positive
         requester or reviewer id are all blocked by the production
         status-shape, relation-type and foreign-key constraints; those
         validation branches stay defensive and are not reachable without
         weakening production constraints. *)
      let* after_clear = stable_sig conn rid in
      Alcotest.(check bool) "only the note fixture changed the row" true
        (contains ~needle:"<null>" after_clear
        && not (contains ~needle:"Nota canonica" after_clear));
      Alcotest.(check bool) "the original note was there to begin with" true
        (contains ~needle:"Nota canonica" before);
      let* status = status_of conn rid in
      Alcotest.(check string) "still accepted throughout" "accepted" status;
      (* The restored row removes normally. *)
      let* () =
        remove_ok "restored row removes" conn ~actor:owner
          ~slug:"phrm-corrupt" ~community:"phrm-corrupt-home"
      in
      check_removed "restored row" conn rid after_clear)

(* === removal unavailable === *)

let removal_unavailable_case =
  db_case "removal: every zero-row relation cause collapses identically"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200009L ~slug:"phrm-navail"
      in
      let* cid = insert_community conn "phrm-navail-home" in
      let* other_cid = insert_community conn "phrm-navail-other" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () = add_top_mod conn ~user:reviewer ~community:other_cid in
      let expect label community =
        remove_expect label Rm.Removal_unavailable conn ~actor:owner
          ~slug:"phrm-navail" ~community
      in
      (* No relation at all. *)
      let* () = expect "no relation" "phrm-navail-home" in
      (* A pending request is not an accepted home. *)
      let* rid =
        request_ok "pending" conn ~user:owner ~slug:"phrm-navail"
          ~community:cid ()
      in
      let* () = expect "pending request" "phrm-navail-home" in
      (* A rejected history row is not one either. *)
      let* () =
        review_ok "reject" conn ~reviewer ~slug:"phrm-navail"
          ~community:"phrm-navail-home" Rv.Reject
      in
      let* () = expect "rejected relation" "phrm-navail-home" in
      (* The accepted home targets this community, never the other one. *)
      let* rid2 =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-navail"
          ~community_id:cid ~community_slug:"phrm-navail-home" ()
      in
      let* () = expect "accepted home targets another community" "phrm-navail-other" in
      let* status = status_of conn rid2 in
      Alcotest.(check string) "wrong-target probe left it accepted" "accepted"
        status;
      (* Removed, then the replay of a successful removal. *)
      let* () =
        remove_ok "remove" conn ~actor:owner ~slug:"phrm-navail"
          ~community:"phrm-navail-home"
      in
      let* () = expect "already removed" "phrm-navail-home" in
      let* () = expect "replay after success" "phrm-navail-home" in
      let* rejected_status = status_of conn rid in
      Alcotest.(check string) "rejected history intact" "rejected"
        rejected_status;
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "exactly the two historical rows" 2 total;
      Lwt.return_unit)

(* === history and the active-home slot === *)

let history_case =
  db_case
    "removal: history survives and the freed slot admits a fresh request"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200010L ~slug:"phrm-history"
      in
      let* cid = insert_community conn "phrm-history-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let note = "Nota storica conservata" in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-history"
          ~community_id:cid ~community_slug:"phrm-history-home" ~note ()
      in
      let* before = stable_sig conn rid in
      let* repos_before = find conn "repos" Connected_projects_fixture.q_count_repositories project in
      let* community_before =
        find conn "community sig" Home_request_fixture.q_community_sig cid
      in
      let* () =
        remove_ok "remove" conn ~actor:owner ~slug:"phrm-history"
          ~community:"phrm-history-home"
      in
      let* () = check_removed "historical row" conn rid before in
      let* repos_after = find conn "repos after" Connected_projects_fixture.q_count_repositories project in
      Alcotest.(check int) "project content remains" repos_before repos_after;
      let* community_after =
        find conn "community sig after" Home_request_fixture.q_community_sig cid
      in
      Alcotest.(check string) "community content remains" community_before
        community_after;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project project
      in
      Alcotest.(check int) "no active relation" 0 active;
      (* The project is still verified, so the freed slot immediately
         admits a fresh request through the real request store. *)
      let* rid2 =
        request_ok "fresh request after removal" conn ~user:owner
          ~slug:"phrm-history" ~community:cid ()
      in
      let* () =
        check_removed "removed history still intact" conn rid before
      in
      let* status2 = status_of conn rid2 in
      Alcotest.(check string) "the new relation is a fresh pending row"
        "pending" status2;
      Alcotest.(check bool) "and a different row" true (rid2 <> rid);
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "history plus the new request" 2 total;
      let* active =
        find conn "active after" Home_request_fixture.q_count_active_for_project project
      in
      Alcotest.(check int) "exactly one active relation" 1 active;
      Lwt.return_unit)

(* === concurrency === *)

let concurrent_removals_case =
  db_case "removal: concurrent removals leave exactly one removed row"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* second = insert_user conn "phrm_steward2" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* admin = insert_user conn "phrm_admin" in
      let* () = exec conn "grant admin" Community_fixture.q_set_admin (admin, true) in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:947200011L ~slug:"phrm-race"
      in
      let* cid = insert_community conn "phrm-race-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, second, inst)
      in
      let go conn ~actor =
        remove conn ~actor ~slug:"phrm-race" ~community:"phrm-race-home"
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let round label ~a ~b =
            let* rid =
              reviewed_home (label ^ ": home") conn ~owner ~reviewer
                ~slug:"phrm-race" ~community_id:cid
                ~community_slug:"phrm-race-home" ()
            in
            let* before = stable_sig conn rid in
            let* results = Lwt.both (go conn ~actor:a) (go conn2 ~actor:b) in
            let w = one_winner label results in
            Alcotest.(check string)
              (label ^ ": winner's status")
              (status_str Phr.Removed)
              (status_str (Rm.resulting_status w));
            let* () = check_removed (label ^ ": one removal") conn rid before in
            let* active =
              find conn "active" Home_request_fixture.q_count_active_for_project project
            in
            Alcotest.(check int) (label ^ ": slot freed once") 0 active;
            Lwt.return_unit
          in
          (* Two project stewards, then a steward against the target top
             moderator, then the top moderator against a durable admin. *)
          let* () = round "steward vs steward" ~a:owner ~b:second in
          let* () = round "steward vs top mod" ~a:owner ~b:reviewer in
          let* () = round "top mod vs durable admin" ~a:reviewer ~b:admin in
          let* total = find conn "total" Home_request_fixture.q_count_for_project project in
          Alcotest.(check int) "one removed row per round" 3 total;
          Lwt.return_unit))

(* === authorization-revocation races === *)

let revocation_race_case =
  db_case "removal: authority revocation serializes through the locked rows"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* second = insert_user conn "phrm_steward2" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* mod2 = insert_user conn "phrm_mod2" in
      let* admin = insert_user conn "phrm_admin" in
      let* () = exec conn "grant admin" Community_fixture.q_set_admin (admin, true) in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:947200012L ~slug:"phrm-revoke"
      in
      let* cid = insert_community conn "phrm-revoke-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () = add_top_mod conn ~user:mod2 ~community:cid in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, second, inst)
      in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-revoke"
          ~community_id:cid ~community_slug:"phrm-revoke-home" ()
      in
      let* before = stable_sig conn rid in
      (* Outcome 1: a removal that commits first cannot be unwound by a
         later revocation. That is proven at the end; first the reverse
         order, where the revocation's uncommitted row lock is held before
         the removal starts, so the removal must observe it. *)
      Db_fixture.with_second_connection (fun conn2 ->
          let raced label ~mutate ~actor =
            let* r =
              serialized_mutation_first conn2 ~mutate ~launch:(fun () ->
                  remove conn ~actor ~slug:"phrm-revoke"
                    ~community:"phrm-revoke-home")
            in
            (match r with
            | Error Rm.Actor_unauthorized -> ()
            | Ok _ -> Alcotest.failf "%s: revoked actor removed" label
            | Error e -> Alcotest.failf "%s: %s" label (Connected_projects_fixture.error_str e));
            check_accepted_unchanged label conn rid before
          in
          (* Stewardship deletion. *)
          let* () =
            raced "steward deleted first"
              ~mutate:(fun () ->
                exec conn2 "held steward deletion" Home_request_fixture.q_delete_steward
                  (project, second))
              ~actor:second
          in
          (* Top-moderator downgrade, then removal. *)
          let* () =
            raced "top mod downgraded first"
              ~mutate:(fun () ->
                exec conn2 "held downgrade" Community_fixture.q_set_moderator_role
                  (mod2, cid, "mod"))
              ~actor:mod2
          in
          let* () =
            exec conn "requalify mod2" Community_fixture.q_set_moderator_role
              (mod2, cid, "top_mod")
          in
          let* () =
            raced "top mod removed first"
              ~mutate:(fun () ->
                exec conn2 "held role removal" Community_fixture.q_remove_moderator
                  (mod2, cid))
              ~actor:mod2
          in
          (* Durable admin revocation. *)
          let* () =
            raced "durable admin revoked first"
              ~mutate:(fun () ->
                exec conn2 "held revocation" Community_fixture.q_set_admin (admin, false))
              ~actor:admin
          in
          (* No deadlock residue and no lost authority: the untouched
             steward removes, and the committed removal stands even after
             his stewardship is revoked afterwards. *)
          let* () =
            remove_ok "remaining steward removes" conn ~actor:owner
              ~slug:"phrm-revoke" ~community:"phrm-revoke-home"
          in
          let* () = check_removed "removal committed" conn rid before in
          let* () =
            exec conn "revoke afterwards" Home_request_fixture.q_delete_steward
              (project, owner)
          in
          let* status = status_of conn rid in
          Alcotest.(check string) "committed removal stands" "removed" status;
          Lwt.return_unit))

(* === removal against a concurrent new request === *)

let request_race_case =
  db_case "removal: racing a fresh request serializes on the project row"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200013L ~slug:"phrm-reqrace"
      in
      let* cid = insert_community conn "phrm-reqrace-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-reqrace"
          ~community_id:cid ~community_slug:"phrm-reqrace-home" ()
      in
      let* before = stable_sig conn rid in
      Db_fixture.with_second_connection (fun conn2 ->
          let removal =
            remove conn ~actor:owner ~slug:"phrm-reqrace"
              ~community:"phrm-reqrace-home"
          in
          let request =
            Rq.create conn2 ~user_id:owner ~project_slug:"phrm-reqrace"
              ~target_community_id:cid ~relation:(Home_request_fixture.phr_fresh_pending ())
          in
          let* removal_result, request_result = Lwt.both removal request in
          (* Both callers take the project row first, so only the two
             documented serialized outcomes are reachable. *)
          (match removal_result with
          | Ok removed ->
              Alcotest.(check string) "removal result"
                (status_str Phr.Removed)
                (status_str (Rm.resulting_status removed))
          | Error e -> Alcotest.failf "removal lost the race: %s" (Connected_projects_fixture.error_str e));
          let* () = check_removed "removed history" conn rid before in
          let* expected_total =
            match request_result with
            | Ok _ ->
                (* Removal committed first: the freed slot admitted the
                   request. *)
                Lwt.return 2
            | Error Rq.Active_home_exists ->
                (* The request checked first, while the accepted home still
                   held the slot; the removal committed afterwards. *)
                Lwt.return 1
            | Error e ->
                Alcotest.failf "unexpected request outcome: %s"
                  (Home_request_fixture.error_str e)
          in
          let* total = find conn "total" Home_request_fixture.q_count_for_project project in
          Alcotest.(check int) "no lost history, no duplicate row"
            expected_total total;
          let* active =
            find conn "active" Home_request_fixture.q_count_active_for_project project
          in
          Alcotest.(check bool) "at most one active relation" true (active <= 1);
          Lwt.return_unit))

(* === failure rollback === *)

let rollback_case =
  db_case "removal: injected update failure leaves the accepted row intact"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:947200014L ~slug:"phrm-fail"
      in
      let* cid = insert_community conn "phrm-fail-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        reviewed_home "poisoned home" conn ~owner ~reviewer ~slug:"phrm-fail"
          ~community_id:cid ~community_slug:"phrm-fail-home"
          ~note:phrm_poison_note ()
      in
      let* before = stable_sig conn rid in
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      (* Installed only now, so the review store's own update — which ran
         while the fixture was being built — is untouched. *)
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          let* () =
            remove_expect "poisoned removal" Rm.Storage_error conn
              ~actor:owner ~slug:"phrm-fail" ~community:"phrm-fail-home"
          in
          let* () =
            check_accepted_unchanged "after injected failure" conn rid before
          in
          let* members = find conn "members" Home_request_fixture.q_count_members cid in
          Alcotest.(check int) "no membership side effect" 0 members;
          let* mods = find conn "moderators" Home_request_fixture.q_count_moderators cid in
          Alcotest.(check int) "moderator roster untouched" 1 mods;
          let* stewards =
            find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
          in
          Alcotest.(check int) "stewardship untouched" 1 stewards;
          let* repos = find conn "repos" Connected_projects_fixture.q_count_repositories project in
          Alcotest.(check bool) "project content untouched" true (repos > 0);
          let* total = find conn "total" Home_request_fixture.q_count_for_project project in
          Alcotest.(check int) "no relation created or deleted" 1 total;
          Lwt.return_unit)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === credential and privacy sweep === *)

let privacy_case =
  db_case "removal: no credential fixture reaches the removed row"
    (fun conn ->
      let* owner = insert_user conn "phrm_owner" in
      let* reviewer = insert_user conn "phrm_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:947200015L ~slug:"phrm-creds"
      in
      let* cid = insert_community conn "phrm-creds-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let credentials =
        [ "phrm-access-token-A1x"
        ; "phrm-refresh-token-B2x"
        ; "phrm-authorization-code-C3x"
        ; "phrm-pkce-verifier-D4x"
        ; "phrm-client-secret-E5x"
        ; "phrm-oauth-state-F6x"
        ; "phrm-session-binding-G7x"
        ; "947200999" (* installation id fixture *)
        ; "947300999" (* account id fixture *)
        ; "947600999" (* repository id fixture *)
        ; "phrm-private-repo-name-H8x"
        ; "phrm-private-repo-desc-I9x"
        ]
      in
      let* rid =
        reviewed_home "home" conn ~owner ~reviewer ~slug:"phrm-creds"
          ~community_id:cid ~community_slug:"phrm-creds-home"
          ~note:"Ordinary private note." ()
      in
      let* () =
        remove_ok "remove" conn ~actor:owner ~slug:"phrm-creds"
          ~community:"phrm-creds-home"
      in
      let* blob = find conn "row blob" Home_request_fixture.q_relation_text_blob rid in
      List.iter
        (fun credential ->
          Alcotest.(check bool) "credential absent from removed row" false
            (contains ~needle:credential blob))
        credentials;
      (* A credential-shaped value deliberately supplied as the private
         note is legitimate there — and only there, surviving removal
         untouched. *)
      let deliberate = List.hd credentials in
      let* rid2 =
        reviewed_home "deliberate note" conn ~owner ~reviewer
          ~slug:"phrm-creds" ~community_id:cid
          ~community_slug:"phrm-creds-home" ~note:deliberate ()
      in
      let* () =
        remove_ok "remove deliberate" conn ~actor:owner ~slug:"phrm-creds"
          ~community:"phrm-creds-home"
      in
      let* _, (_, (stored_note, _)) = relation_row conn rid2 in
      Alcotest.(check bool) "deliberate note preserved verbatim" true
        (stored_note = Some deliberate);
      let* nonnote =
        find conn "non-note blob" Home_request_fixture.q_relation_nonnote_blob rid2
      in
      Alcotest.(check bool) "note value nowhere else in the row" false
        (contains ~needle:deliberate nonnote);
      Lwt.return_unit)

let suite =
  [ pure_inputs_case; steward_removal_case; steward_variants_case;
    community_side_case; multiple_authority_case; unauthorized_case;
    availability_case; corruption_case; removal_unavailable_case;
    history_case; concurrent_removals_case; revocation_race_case;
    request_race_case; rollback_case; privacy_case ]

let suites =
    (* Accepted project-home removal: pure validation before SQL, the three
       durable authorization sources and their locking, lifecycle-drift
       tolerance, the exact removed row shape, the freed active-home slot,
       concurrency and authorization-revocation races, rollback, and the
       payload-free error and credential-privacy contracts.
       Database-gated. *)
  [ ("project_home_removal_store", suite)
  ]
