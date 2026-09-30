module Phr = Earde.Project_home_relation

let or_fail = Db_fixture.or_fail

(* === Project home review store (Project_home_review_store) ===
   Transactional moderator accept/reject of the exact pending home
   request, driven over verified permanent projects built through the
   real draft/selection/finalization chain and pending relations written
   by the real request store. Database-gated (EARDE_TEST_DATABASE_URL,
   same opt-in as Mod_scope) with its own reserved
   external-installation-id range 945600001..945600999 (hence account
   ids 945700001..945700999, which also scope the permanent-project
   cleanup), phrv_% usernames, and phrv-% community slugs so no suite
   shares fixtures. Pure validation is proven pre-SQL against a
   deliberately disconnected connection. Reviewer authorization uses
   only the real durable rows: community_moderators role 'top_mod' and
   users.is_admin. No DB-free cases exist deliberately: reviewed_request
   has no public constructor, so the one pure accessor
   (resulting_status) is only exercisable — and is exercised throughout
   — on genuinely committed reviews. Credential assertions are boolean,
   so no fixture byte reaches test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rv = Earde.Project_home_review_store
module Rq = Earde.Project_home_request_store

let status_str = Phr.string_of_status
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let make_project = Home_request_fixture.make_project
let insert_community = Community_fixture.insert_community
let contains = Html_assert.occurs

(* Same dependency order as the sibling suites. Users are deleted after
   communities so moderator rows cascade from either side cleanly. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 945700001 \
       AND 945700999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       945700001 AND 945700999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 945600001 AND 945600999)";
      "DELETE FROM communities WHERE slug LIKE 'phrv-%'";
      "DELETE FROM users WHERE username LIKE 'phrv_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       945600001 AND 945600999";
    ]

(* Test-only failure injection for the rollback case: an AFTER UPDATE
   trigger scoped to one reserved note value, so the failure fires only
   after the review update was genuinely attempted (the pending insert
   with the same note is untouched). Installed and dropped inside that
   case alone; production migrations are untouched. *)
let phrv_poison_note = "phrv poison marker"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE FUNCTION phrv_fail_update_fn() RETURNS trigger\n\
    \   LANGUAGE plpgsql\n\
    \   AS 'BEGIN RAISE EXCEPTION ''phrv fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER phrv_fail_update\n\
    \   AFTER UPDATE ON community_projects\n\
    \   FOR EACH ROW WHEN (NEW.request_note = 'phrv poison marker')\n\
    \   EXECUTE FUNCTION phrv_fail_update_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS phrv_fail_update ON community_projects"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS phrv_fail_update_fn()"

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
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () ->
          Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f conn))

let relation_row = Home_request_fixture.relation_row

let relation_sig conn id =
  find conn "relation sig" Home_request_fixture.q_relation_sig id

let status_of conn id = find conn "status" Home_review_fixture.q_status id

let check_pending_unchanged label conn id before =
  let* after = relation_sig conn id in
  Alcotest.(check string) (label ^ ": relation unchanged") before after;
  Lwt.return_unit

(* === pure input validation === *)

let pure_inputs_case =
  db_case "review: invalid inputs rejected before any SQL" (fun _conn ->
      (* A deliberately unusable connection: pure validation must
         return without touching it — were any SQL attempted, the
         result would be Storage_error (or a test-failing exception),
         never the expected input error. *)
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let expect label e ~reviewer ~slug ~community =
        Home_review_fixture.review_expect label e dead ~reviewer ~slug
          ~community Rv.Accept
      in
      let* () =
        expect "user id 0" Rv.Invalid_user_id ~reviewer:0 ~slug:"phrv-a"
          ~community:"phrv-c"
      in
      let* () =
        expect "negative user id" Rv.Invalid_user_id ~reviewer:(-7)
          ~slug:"phrv-a" ~community:"phrv-c"
      in
      let* () =
        expect "user checked before slugs" Rv.Invalid_user_id ~reviewer:0
          ~slug:"NOT A SLUG" ~community:"also bad"
      in
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            expect "invalid project slug" Rv.Invalid_project_slug ~reviewer:1
              ~slug:bad ~community:"phrv-c")
          [
            "";
            "Phrv-Upper";
            "phrv slug";
            " phrv-a";
            "phrv-a ";
            "phrv_a";
            "phrv/a";
            "-phrv";
            "phrv-";
            "phrv--a";
            String.make 81 'a';
          ]
      in
      let* () =
        expect "project slug checked before community slug"
          Rv.Invalid_project_slug ~reviewer:1 ~slug:"phrv--a"
          ~community:"also bad"
      in
      Lwt_list.iter_s
        (fun bad ->
          expect "invalid community slug" Rv.Invalid_community_slug ~reviewer:1
            ~slug:"phrv-a" ~community:bad)
        [
          "";
          "phrv c";
          " phrv-c";
          "phrv-c ";
          "phrv/c";
          "phrv\tc";
          "phrv\nc";
          "phrv\x01c";
          "phrv\x7fc";
        ])

(* === accept success === *)

let accept_public_case =
  db_case "review: acceptance round-trips exactly on a public target"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600001L ~slug:"phrv-accept"
      in
      let* cid = insert_community conn "phrv-accept-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let note = "Prima riga — gi\xc3\xa0 discutiamo qui \xe2\x98\x95" in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-accept" ~community:cid ~note ()
      in
      let* project_before =
        find conn "project sig" Home_request_fixture.q_project_sig project
      in
      let* community_before =
        find conn "community sig" Home_request_fixture.q_community_sig cid
      in
      let* () =
        Home_review_fixture.review_ok "accept" Phr.Accepted conn ~reviewer
          ~slug:"phrv-accept" ~community:"phrv-accept-home" Rv.Accept
      in
      let* ( ((rp, rc), (rtype, status)),
             ((req, rev), (stored_note, (_, has_removed, upd_ge))) ) =
        relation_row conn rid
      in
      Alcotest.(check int64) "same project fk" project rp;
      Alcotest.(check int) "same community fk" cid rc;
      Alcotest.(check string) "relation type home" "home" rtype;
      Alcotest.(check string) "status accepted" "accepted" status;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option int)) "exact reviewer stored" (Some reviewer) rev;
      Alcotest.(check (option string))
        "note preserved byte-exact" (Some note) stored_note;
      Alcotest.(check bool) "removed_at NULL" false has_removed;
      Alcotest.(check bool) "updated_at coherent" true upd_ge;
      let* reviewed_present, reviewed_ge, upd_ge2 =
        find conn "review times" Home_review_fixture.q_review_times rid
      in
      Alcotest.(check bool) "reviewed_at present" true reviewed_present;
      Alcotest.(check bool) "reviewed_at coherent" true reviewed_ge;
      Alcotest.(check bool) "updated_at still coherent" true upd_ge2;
      let* total =
        find conn "total" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "exactly one relation row" 1 total;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "exactly one accepted active home" 1 active;
      (* Reviewing grants and changes nothing else. *)
      let* project_after =
        find conn "project sig after" Home_request_fixture.q_project_sig project
      in
      let* community_after =
        find conn "community sig after" Home_request_fixture.q_community_sig cid
      in
      Alcotest.(check string) "project unchanged" project_before project_after;
      Alcotest.(check string)
        "community unchanged" community_before community_after;
      let* members =
        find conn "members" Home_request_fixture.q_count_members cid
      in
      Alcotest.(check int) "no membership created" 0 members;
      let* mods =
        find conn "moderators" Home_request_fixture.q_count_moderators cid
      in
      Alcotest.(check int) "moderator roster unchanged" 1 mods;
      let* role =
        find conn "role" Home_review_fixture.q_role_sig (reviewer, cid)
      in
      Alcotest.(check string) "reviewer role unchanged" "top_mod" role;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project
          project
      in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      Lwt.return_unit)

let accept_unlisted_case =
  db_case "review: acceptance also permitted on an unlisted target" (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600002L ~slug:"phrv-unlisted"
      in
      let* cid =
        insert_community ~indexable:false ~discoverable:false conn
          "phrv-unlisted-home"
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-unlisted" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "accept unlisted" Phr.Accepted conn
          ~reviewer ~slug:"phrv-unlisted" ~community:"phrv-unlisted-home"
          Rv.Accept
      in
      let* status = status_of conn rid in
      Alcotest.(check string) "accepted" "accepted" status;
      Lwt.return_unit)

(* === reject success === *)

let reject_case =
  db_case "review: rejection preserves history and frees the home slot"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600003L ~slug:"phrv-reject"
      in
      let* cid = insert_community conn "phrv-reject-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let note = "Nota privata della richiesta" in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-reject" ~community:cid ~note ()
      in
      let* () =
        Home_review_fixture.review_ok "reject" Phr.Rejected conn ~reviewer
          ~slug:"phrv-reject" ~community:"phrv-reject-home" Rv.Reject
      in
      let* (_, (_, status)), ((req, rev), (stored_note, (_, has_removed, _))) =
        relation_row conn rid
      in
      Alcotest.(check string) "status rejected" "rejected" status;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option int)) "exact reviewer stored" (Some reviewer) rev;
      Alcotest.(check (option string)) "note preserved" (Some note) stored_note;
      Alcotest.(check bool) "removed_at NULL" false has_removed;
      let* reviewed_present, reviewed_ge, _ =
        find conn "review times" Home_review_fixture.q_review_times rid
      in
      Alcotest.(check bool) "reviewed_at present" true reviewed_present;
      Alcotest.(check bool) "reviewed_at coherent" true reviewed_ge;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "active slot freed" 0 active;
      (* The freed slot immediately admits a fresh request, and that
         request can then become the accepted home. *)
      let* rid2 =
        Home_review_fixture.request_ok "fresh request after rejection" conn
          ~user:owner ~slug:"phrv-reject" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "accept the fresh request" Phr.Accepted
          conn ~reviewer ~slug:"phrv-reject" ~community:"phrv-reject-home"
          Rv.Accept
      in
      let* status2 = status_of conn rid2 in
      Alcotest.(check string) "second relation accepted" "accepted" status2;
      let* total =
        find conn "total" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "history retained" 2 total;
      Lwt.return_unit)

(* === authorization: who may review === *)

let authority_variants_case =
  db_case "review: second top mod, durable admin, and requester-with-authority"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* first_mod = insert_user conn "phrv_mod1" in
      let* second_mod = insert_user conn "phrv_mod2" in
      let* admin = insert_user conn "phrv_admin" in
      let* () =
        exec conn "grant admin" Community_fixture.q_set_admin (admin, true)
      in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600004L ~slug:"phrv-authority"
      in
      let* cid = insert_community conn "phrv-authority-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:first_mod ~community:cid
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:second_mod ~community:cid
      in
      (* A second independently appointed top mod may review. *)
      let* _rid =
        Home_review_fixture.request_ok "pending 1" conn ~user:owner
          ~slug:"phrv-authority" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "second top mod rejects" Phr.Rejected conn
          ~reviewer:second_mod ~slug:"phrv-authority"
          ~community:"phrv-authority-home" Rv.Reject
      in
      (* A durable global administrator — neither member nor moderator
         of the target — may review. *)
      let* _rid =
        Home_review_fixture.request_ok "pending 2" conn ~user:owner
          ~slug:"phrv-authority" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "durable admin rejects" Phr.Rejected conn
          ~reviewer:admin ~slug:"phrv-authority"
          ~community:"phrv-authority-home" Rv.Reject
      in
      (* The requester may review only through independently held
         target-community authority — here granted explicitly. *)
      let* () =
        Home_review_fixture.add_top_mod conn ~user:owner ~community:cid
      in
      let* rid3 =
        Home_review_fixture.request_ok "pending 3" conn ~user:owner
          ~slug:"phrv-authority" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "requester with own top-mod role accepts"
          Phr.Accepted conn ~reviewer:owner ~slug:"phrv-authority"
          ~community:"phrv-authority-home" Rv.Accept
      in
      let* _, ((req, rev), _) = relation_row conn rid3 in
      Alcotest.(check (option int)) "requester recorded" (Some owner) req;
      Alcotest.(check (option int))
        "same user as durable reviewer" (Some owner) rev;
      Lwt.return_unit)

let unauthorized_case =
  db_case "review: every insufficient authority collapses identically"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* member = insert_user conn "phrv_member" in
      let* outsider = insert_user conn "phrv_outsider" in
      let* low_mod = insert_user conn "phrv_lowmod" in
      let* legacy_mod = insert_user conn "phrv_legacymod" in
      let* other_mod = insert_user conn "phrv_othermod" in
      let* removed_mod = insert_user conn "phrv_removedmod" in
      let* downgraded = insert_user conn "phrv_downgraded" in
      let* adminish = insert_user conn "phrv_adminish" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600005L ~slug:"phrv-unauth"
      in
      let* cid = insert_community conn "phrv-unauth-home" in
      let* other_cid = insert_community conn "phrv-unauth-other" in
      let* () =
        exec conn "member" Community_fixture.q_insert_member (member, cid)
      in
      let* () =
        Home_review_fixture.add_role conn ~user:low_mod ~community:cid "mod"
      in
      let* () =
        Home_review_fixture.add_role conn ~user:legacy_mod ~community:cid
          "legacy_mod"
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:other_mod
          ~community:other_cid
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:removed_mod ~community:cid
      in
      let* () =
        exec conn "remove role" Community_fixture.q_remove_moderator
          (removed_mod, cid)
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:downgraded ~community:cid
      in
      let* () =
        exec conn "downgrade role" Community_fixture.q_set_moderator_role
          (downgraded, cid, "mod")
      in
      (* Session/admin-shaped without durable backing: nothing but the
         username suggests administration, and users.is_admin stays
         FALSE. *)
      let* () =
        exec conn "explicit non-admin" Community_fixture.q_set_admin
          (adminish, false)
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-unauth" ~community:cid ()
      in
      let* before = relation_sig conn rid in
      let expect label reviewer decision =
        Home_review_fixture.review_expect label Rv.Reviewer_unauthorized conn
          ~reviewer ~slug:"phrv-unauth" ~community:"phrv-unauth-home" decision
      in
      (* The requester is also creator and steward — none of that
         grants community authority. *)
      let* () = expect "requester/creator/steward" owner Rv.Accept in
      let* () = expect "requester/creator/steward reject" owner Rv.Reject in
      let* () = expect "ordinary member" member Rv.Accept in
      let* () = expect "non-member" outsider Rv.Accept in
      let* () = expect "lower moderator role" low_mod Rv.Accept in
      let* () = expect "legacy moderator role" legacy_mod Rv.Accept in
      let* () = expect "moderator of another community" other_mod Rv.Accept in
      let* () = expect "removed moderator" removed_mod Rv.Accept in
      let* () = expect "downgraded moderator" downgraded Rv.Accept in
      let* () = expect "admin-shaped without durable flag" adminish Rv.Accept in
      let* () = check_pending_unchanged "after all refusals" conn rid before in
      let* status = status_of conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      Lwt.return_unit)

(* === project availability === *)

let project_unavailable_case =
  db_case "review: acceptance needs verification; refusals collapse identically"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600006L ~slug:"phrv-project"
      in
      let* cid = insert_community conn "phrv-project-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-project" ~community:cid ()
      in
      let* before = relation_sig conn rid in
      let expect label slug decision =
        Home_review_fixture.review_expect label Rv.Project_unavailable conn
          ~reviewer ~slug ~community:"phrv-project-home" decision
      in
      (* Missing project: canonical grammar, nothing stored under it,
         refused for both decisions. *)
      let* () = expect "missing project accept" "phrv-absent" Rv.Accept in
      let* () = expect "missing project reject" "phrv-absent" Rv.Reject in
      (* Stale, then revoked verification: acceptance refused through
         the exact same collapsed variant as a missing project — the
         precise status is never distinguishable. *)
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* () = expect "stale project accept" "phrv-project" Rv.Accept in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      let* () = expect "revoked project accept" "phrv-project" Rv.Accept in
      let* () = check_pending_unchanged "after all refusals" conn rid before in
      let* status = status_of conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      (* Restored verification accepts normally. *)
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      Home_review_fixture.review_ok "restored project accepts" Phr.Accepted conn
        ~reviewer ~slug:"phrv-project" ~community:"phrv-project-home" Rv.Accept)

let stale_revoked_rejection_case =
  db_case
    "review: rejection closes stale and revoked requests, freeing the slot"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600017L ~slug:"phrv-lost"
      in
      let* cid = insert_community conn "phrv-lost-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let note = "Nota conservata dopo la verifica" in
      (* Stale: the request predates the verification loss; rejection
         still closes it completely. *)
      let* rid1 =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-lost" ~community:cid ~note ()
      in
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* project_before =
        find conn "project sig" Home_request_fixture.q_project_sig project
      in
      let* () =
        Home_review_fixture.review_ok "reject stale request" Phr.Rejected conn
          ~reviewer ~slug:"phrv-lost" ~community:"phrv-lost-home" Rv.Reject
      in
      let* (_, (_, status)), ((req, rev), (stored_note, (_, has_removed, _))) =
        relation_row conn rid1
      in
      Alcotest.(check string) "status rejected" "rejected" status;
      Alcotest.(check (option int)) "exact reviewer stored" (Some reviewer) rev;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option string)) "note preserved" (Some note) stored_note;
      Alcotest.(check bool) "removed_at NULL" false has_removed;
      let* reviewed_present, reviewed_ge, _ =
        find conn "review times" Home_review_fixture.q_review_times rid1
      in
      Alcotest.(check bool) "reviewed_at present" true reviewed_present;
      Alcotest.(check bool) "reviewed_at coherent" true reviewed_ge;
      (* Rejection neither restores nor modifies project verification. *)
      let* project_after =
        find conn "project sig after" Home_request_fixture.q_project_sig project
      in
      Alcotest.(check string)
        "project still stale, untouched" project_before project_after;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "active slot freed" 0 active;
      (* While stale, the request store still refuses a new request —
         the freed slot only becomes usable through reverification. *)
      let* r =
        Rq.create conn ~user_id:owner ~project_slug:"phrv-lost"
          ~target_community_id:cid
          ~relation:(Home_request_fixture.phr_fresh_pending ())
      in
      let* () =
        match r with
        | Error Rq.Project_unavailable -> Lwt.return_unit
        | Ok _ -> Alcotest.fail "stale project submitted a new request"
        | Error e ->
            Alcotest.failf "stale re-request: %s"
              (Home_request_fixture.error_str e)
      in
      let* rejected_sig = relation_sig conn rid1 in
      (* Reverified: the freed slot admits a fresh request through the
         real request store; history stays untouched. *)
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* _rid2 =
        Home_review_fixture.request_ok "fresh request after reverification" conn
          ~user:owner ~slug:"phrv-lost" ~community:cid ()
      in
      let* () =
        check_pending_unchanged "rejected history unchanged" conn rid1
          rejected_sig
      in
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "exactly one active relation" 1 active;
      (* Revoked: same closure path on the fresh pending request. *)
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      let* () =
        Home_review_fixture.review_expect "revoked accept refused"
          Rv.Project_unavailable conn ~reviewer ~slug:"phrv-lost"
          ~community:"phrv-lost-home" Rv.Accept
      in
      let* () =
        Home_review_fixture.review_ok "reject revoked request" Phr.Rejected conn
          ~reviewer ~slug:"phrv-lost" ~community:"phrv-lost-home" Rv.Reject
      in
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "slot freed again" 0 active;
      (* And the slot reopens once more after reverification. *)
      let* () =
        exec conn "restore verified again"
          Home_request_fixture.q_set_verification (project, "verified")
      in
      let* _rid3 =
        Home_review_fixture.request_ok "fresh request after revocation closure"
          conn ~user:owner ~slug:"phrv-lost" ~community:cid ()
      in
      let* total =
        find conn "total" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "full history retained" 3 total;
      let* active =
        find conn "active" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "exactly one active relation again" 1 active;
      Lwt.return_unit)

(* === community availability and validation === *)

let community_cases =
  db_case_lifecycle_relaxed
    "review: missing community and durable community corruption" (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600007L ~slug:"phrv-community"
      in
      let* cid = insert_community conn "phrv-community-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-community" ~community:cid ()
      in
      let* before = relation_sig conn rid in
      (* Missing community: canonical slug, nothing stored under it. *)
      let* () =
        Home_review_fixture.review_expect "missing community"
          Rv.Community_unavailable conn ~reviewer ~slug:"phrv-community"
          ~community:"phrv-nowhere" Rv.Accept
      in
      let expect_corrupt label =
        Home_review_fixture.review_expect label Rv.Inconsistent_data conn
          ~reviewer ~slug:"phrv-community" ~community:"phrv-community-home"
          Rv.Accept
      in
      (* Structurally invalid durable shapes: an empty display name, a
         mixed published flag pair, a draft leaking through public
         listing, and a private community still discoverable. Each is
         restored before the next probe. *)
      (* The blank name is now barred by the scoped identity
         constraints; they are dropped for this probe alone and restored
         under Lwt.finalize once the name is canonical again. *)
      let* () = Network_community_constraints.drop conn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () =
              exec conn "empty name" Community_fixture.q_set_name (cid, "")
            in
            expect_corrupt "empty community name")
          (fun () ->
            let* () =
              exec conn "restore name" Community_fixture.q_set_name
                (cid, "phrv-community-home")
            in
            Network_community_constraints.restore conn)
      in
      let* () = exec conn "mix flags" Home_review_fixture.q_mix_flags cid in
      let* () = expect_corrupt "mixed publication flags" in
      let* () =
        exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
      in
      let* () = exec conn "leaky draft" Home_review_fixture.q_leaky_draft cid in
      let* () = expect_corrupt "publicly listed draft" in
      let* () =
        exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
      in
      let* () =
        exec conn "leaky private" Home_review_fixture.q_leaky_private cid
      in
      let* () = expect_corrupt "discoverable private community" in
      let* () =
        exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
      in
      let* () = check_pending_unchanged "after all causes" conn rid before in
      (* The restored community reviews normally. *)
      Home_review_fixture.review_ok "restored community rejects" Phr.Rejected
        conn ~reviewer ~slug:"phrv-community" ~community:"phrv-community-home"
        Rv.Reject)

(* === currently ineligible targets === *)

let ineligible_target_case =
  db_case_lifecycle_relaxed
    "review: valid ineligible targets refuse acceptance, allow rejection"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600008L ~slug:"phrv-inelig"
      in
      let* cid = insert_community conn "phrv-inelig-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let drift =
        [
          ("fully private", Community_fixture.q_make_private);
          ("setup draft", Community_fixture.q_make_draft_state);
          ("legacy non-network", Community_fixture.q_make_legacy);
        ]
      in
      Lwt_list.iter_s
        (fun (label, q_drift) ->
          (* Requests are created while the target is eligible — the
             drift happens afterwards, as in production. *)
          let* () =
            exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
          in
          let* rid =
            Home_review_fixture.request_ok (label ^ ": pending") conn
              ~user:owner ~slug:"phrv-inelig" ~community:cid ()
          in
          let* () = exec conn (label ^ ": drift") q_drift cid in
          let* () =
            Home_review_fixture.review_expect
              (label ^ ": accept refused")
              Rv.Target_ineligible conn ~reviewer ~slug:"phrv-inelig"
              ~community:"phrv-inelig-home" Rv.Accept
          in
          let* status = status_of conn rid in
          Alcotest.(check string) (label ^ ": still pending") "pending" status;
          let* () =
            Home_review_fixture.review_ok
              (label ^ ": reject allowed")
              Phr.Rejected conn ~reviewer ~slug:"phrv-inelig"
              ~community:"phrv-inelig-home" Rv.Reject
          in
          let* status = status_of conn rid in
          Alcotest.(check string) (label ^ ": rejected") "rejected" status;
          Lwt.return_unit)
        drift)

(* === review unavailable === *)

let review_unavailable_case =
  db_case "review: every zero-row relation cause collapses identically"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600009L ~slug:"phrv-navail"
      in
      let* cid = insert_community conn "phrv-navail-home" in
      let* other_cid = insert_community conn "phrv-navail-other" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:other_cid
      in
      let expect label community =
        Home_review_fixture.review_expect label Rv.Review_unavailable conn
          ~reviewer ~slug:"phrv-navail" ~community Rv.Accept
      in
      (* No relation at all. *)
      let* () = expect "no relation" "phrv-navail-home" in
      (* The pending request targets the other community. *)
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-navail" ~community:cid ()
      in
      let* () =
        expect "request targets another community" "phrv-navail-other"
      in
      (* Already accepted, and replay after a successful review. *)
      let* () =
        Home_review_fixture.review_ok "accept" Phr.Accepted conn ~reviewer
          ~slug:"phrv-navail" ~community:"phrv-navail-home" Rv.Accept
      in
      let* () = expect "already accepted" "phrv-navail-home" in
      let* () =
        Home_review_fixture.review_expect "reject replay after acceptance"
          Rv.Review_unavailable conn ~reviewer ~slug:"phrv-navail"
          ~community:"phrv-navail-home" Rv.Reject
      in
      (* Removed. *)
      let* () = exec conn "remove" Home_request_fixture.q_mark_removed rid in
      let* () = expect "removed relation" "phrv-navail-home" in
      (* Already rejected. *)
      let* _rid2 =
        Home_review_fixture.request_ok "second pending" conn ~user:owner
          ~slug:"phrv-navail" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "reject" Phr.Rejected conn ~reviewer
          ~slug:"phrv-navail" ~community:"phrv-navail-home" Rv.Reject
      in
      let* () = expect "already rejected" "phrv-navail-home" in
      let* total =
        find conn "total" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "exactly the two historical rows" 2 total;
      Lwt.return_unit)

(* === durable pending corruption === *)

let pending_corruption_case =
  db_case "review: malformed durable pending data is corruption" (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600010L ~slug:"phrv-corrupt"
      in
      let* cid = insert_community conn "phrv-corrupt-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-corrupt" ~community:cid ()
      in
      (* A stored note outside the canonical shape cannot be
         reconstructed byte-exactly through the pure constructor. *)
      let* () = exec conn "pad note" Home_review_fixture.q_pad_note rid in
      let* () =
        Home_review_fixture.review_expect "non-canonical padded note"
          Rv.Inconsistent_data conn ~reviewer ~slug:"phrv-corrupt"
          ~community:"phrv-corrupt-home" Rv.Accept
      in
      let* () =
        exec conn "control-byte note" Home_review_fixture.q_control_note rid
      in
      let* () =
        Home_review_fixture.review_expect "forbidden control byte in note"
          Rv.Inconsistent_data conn ~reviewer ~slug:"phrv-corrupt"
          ~community:"phrv-corrupt-home" Rv.Accept
      in
      let* status = status_of conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      (* A reviewer id, review timestamp, or removal timestamp on a
         pending row and a non-'home' relation type are blocked by the
         production status-shape and relation-type CHECKs; those
         validation branches stay defensive. *)
      let* () = exec conn "restore note" Home_review_fixture.q_clear_note rid in
      Home_review_fixture.review_ok "restored row reviews normally" Phr.Rejected
        conn ~reviewer ~slug:"phrv-corrupt" ~community:"phrv-corrupt-home"
        Rv.Reject)

(* === concurrency === *)

let concurrent_reviews_case =
  db_case "review: concurrent decisions leave exactly one durable review"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* r1 = insert_user conn "phrv_mod1" in
      let* r2 = insert_user conn "phrv_mod2" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600011L ~slug:"phrv-race"
      in
      let* cid = insert_community conn "phrv-race-home" in
      let* () = Home_review_fixture.add_top_mod conn ~user:r1 ~community:cid in
      let* () = Home_review_fixture.add_top_mod conn ~user:r2 ~community:cid in
      let go conn ~reviewer decision =
        Home_review_fixture.review conn ~reviewer ~slug:"phrv-race"
          ~community:"phrv-race-home" decision
      in
      let one_winner label (ra, rb) =
        match (ra, rb) with
        | Ok w, Error Rv.Review_unavailable | Error Rv.Review_unavailable, Ok w
          ->
            w
        | Ok _, Ok _ -> Alcotest.failf "%s: both succeeded" label
        | Error a, Error b ->
            Alcotest.failf "%s: both failed (%s, %s)" label
              (Home_review_fixture.error_str a)
              (Home_review_fixture.error_str b)
        | Ok _, Error e | Error e, Ok _ ->
            Alcotest.failf "%s: unexpected loser error %s" label
              (Home_review_fixture.error_str e)
      in
      Db_fixture.with_second_connection (fun conn2 ->
          (* Same decision, two reviewers: accepted exactly once. *)
          let* rid =
            Home_review_fixture.request_ok "pending" conn ~user:owner
              ~slug:"phrv-race" ~community:cid ()
          in
          let* results =
            Lwt.both
              (go conn ~reviewer:r1 Rv.Accept)
              (go conn2 ~reviewer:r2 Rv.Accept)
          in
          let w = one_winner "two accepts" results in
          Alcotest.(check string)
            "accepted result" (status_str Phr.Accepted)
            (status_str (Rv.resulting_status w));
          let* _, ((_, rev), _) = relation_row conn rid in
          Alcotest.(check bool)
            "one durable reviewer recorded" true
            (rev = Some r1 || rev = Some r2);
          let* status = status_of conn rid in
          Alcotest.(check string) "accepted once" "accepted" status;
          let* total =
            find conn "total" Home_request_fixture.q_count_for_project project
          in
          Alcotest.(check int) "no duplicate relation" 1 total;
          (* Same again for two rejections. *)
          let* () =
            exec conn "free slot" Home_request_fixture.q_mark_removed rid
          in
          let* rid2 =
            Home_review_fixture.request_ok "second pending" conn ~user:owner
              ~slug:"phrv-race" ~community:cid ()
          in
          let* results =
            Lwt.both
              (go conn ~reviewer:r1 Rv.Reject)
              (go conn2 ~reviewer:r2 Rv.Reject)
          in
          let w = one_winner "two rejects" results in
          Alcotest.(check string)
            "rejected result" (status_str Phr.Rejected)
            (status_str (Rv.resulting_status w));
          let* status = status_of conn rid2 in
          Alcotest.(check string) "rejected once" "rejected" status;
          (* Opposite decisions: the final state is exactly the
             winner's, with the winner's reviewer. *)
          let* rid3 =
            Home_review_fixture.request_ok "third pending" conn ~user:owner
              ~slug:"phrv-race" ~community:cid ()
          in
          let* ra, rb =
            Lwt.both
              (go conn ~reviewer:r1 Rv.Accept)
              (go conn2 ~reviewer:r2 Rv.Reject)
          in
          let expected_status, expected_reviewer =
            match (ra, rb) with
            | Ok w, Error Rv.Review_unavailable -> (Rv.resulting_status w, r1)
            | Error Rv.Review_unavailable, Ok w -> (Rv.resulting_status w, r2)
            | Ok _, Ok _ -> Alcotest.fail "accept vs reject: both succeeded"
            | Error a, Error b ->
                Alcotest.failf "accept vs reject: both failed (%s, %s)"
                  (Home_review_fixture.error_str a)
                  (Home_review_fixture.error_str b)
            | Ok _, Error e | Error e, Ok _ ->
                Alcotest.failf "accept vs reject: unexpected loser error %s"
                  (Home_review_fixture.error_str e)
          in
          let* _, ((_, rev), (_, (has_reviewed, has_removed, upd_ge))) =
            relation_row conn rid3
          in
          let* status = status_of conn rid3 in
          Alcotest.(check string)
            "winner's status durable"
            (status_str expected_status)
            status;
          Alcotest.(check (option int))
            "winner's reviewer durable" (Some expected_reviewer) rev;
          Alcotest.(check bool) "reviewed_at present" true has_reviewed;
          Alcotest.(check bool) "removed_at NULL" false has_removed;
          Alcotest.(check bool) "no mixed timestamps" true upd_ge;
          Lwt.return_unit))

(* === role-removal serialization === *)

let role_removal_serialization_case =
  db_case "review: role removal serializes through the locked role row"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* admin = insert_user conn "phrv_admin" in
      let* () =
        exec conn "grant admin" Community_fixture.q_set_admin (admin, true)
      in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600012L ~slug:"phrv-role"
      in
      let* cid = insert_community conn "phrv-role-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      (* Outcome 1: the review completes first; the later role removal
         cannot unwind the committed decision. *)
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-role" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "review before removal" Phr.Rejected conn
          ~reviewer ~slug:"phrv-role" ~community:"phrv-role-home" Rv.Reject
      in
      let* () =
        exec conn "remove role afterwards" Community_fixture.q_remove_moderator
          (reviewer, cid)
      in
      let* status = status_of conn rid in
      Alcotest.(check string) "committed review stands" "rejected" status;
      (* Outcome 2: the removal's uncommitted row lock is held before
         the review starts, so the review serializes behind it and must
         see the removal. *)
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid2 =
        Home_review_fixture.request_ok "second pending" conn ~user:owner
          ~slug:"phrv-role" ~community:cid ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held removal" Community_fixture.q_remove_moderator
                  (reviewer, cid))
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer ~slug:"phrv-role"
                  ~community:"phrv-role-home" Rv.Accept)
          in
          (match r with
          | Error Rv.Reviewer_unauthorized -> ()
          | Ok _ -> Alcotest.fail "removed-first review succeeded"
          | Error e ->
              Alcotest.failf "removed-first review: %s"
                (Home_review_fixture.error_str e));
          let* status = status_of conn rid2 in
          Alcotest.(check string)
            "still pending after removal race" "pending" status;
          (* Downgrade instead of removal, same protocol. *)
          let* () =
            Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
          in
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held downgrade"
                  Community_fixture.q_set_moderator_role (reviewer, cid, "mod"))
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer ~slug:"phrv-role"
                  ~community:"phrv-role-home" Rv.Accept)
          in
          (match r with
          | Error Rv.Reviewer_unauthorized -> ()
          | Ok _ -> Alcotest.fail "downgraded-first review succeeded"
          | Error e ->
              Alcotest.failf "downgraded-first review: %s"
                (Home_review_fixture.error_str e));
          (* Admin revocation serializes through the users row alike. *)
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held revocation" Community_fixture.q_set_admin
                  (admin, false))
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer:admin
                  ~slug:"phrv-role" ~community:"phrv-role-home" Rv.Accept)
          in
          (match r with
          | Error Rv.Reviewer_unauthorized -> ()
          | Ok _ -> Alcotest.fail "revoked-first admin review succeeded"
          | Error e ->
              Alcotest.failf "revoked-first admin review: %s"
                (Home_review_fixture.error_str e));
          let* status = status_of conn rid2 in
          Alcotest.(check string)
            "still pending after all races" "pending" status;
          (* No deadlock residue: a re-qualified reviewer completes. *)
          let* () =
            exec conn "requalify" Community_fixture.q_set_moderator_role
              (reviewer, cid, "top_mod")
          in
          Home_review_fixture.review_ok "requalified reviewer rejects"
            Phr.Rejected conn ~reviewer ~slug:"phrv-role"
            ~community:"phrv-role-home" Rv.Reject))

(* === community-lifecycle serialization === *)

let lifecycle_serialization_case =
  db_case_lifecycle_relaxed
    "review: lifecycle change serializes through the community lock"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600013L ~slug:"phrv-lifecycle"
      in
      let* cid = insert_community conn "phrv-lifecycle-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      (* Outcome 1: acceptance locks the eligible state first and
         commits; the later privacy change cannot unwind it. *)
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-lifecycle" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "accept before drift" Phr.Accepted conn
          ~reviewer ~slug:"phrv-lifecycle" ~community:"phrv-lifecycle-home"
          Rv.Accept
      in
      let* () =
        exec conn "drift afterwards" Community_fixture.q_make_private cid
      in
      let* status = status_of conn rid in
      Alcotest.(check string) "committed acceptance stands" "accepted" status;
      (* Outcome 2: the lifecycle mutation's uncommitted community lock
         is held before the review starts. *)
      let* () =
        exec conn "restore eligible" Home_review_fixture.q_make_eligible cid
      in
      let* () = exec conn "free slot" Home_request_fixture.q_mark_removed rid in
      let* rid2 =
        Home_review_fixture.request_ok "second pending" conn ~user:owner
          ~slug:"phrv-lifecycle" ~community:cid ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held privacy change"
                  Community_fixture.q_make_private cid)
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer ~slug:"phrv-lifecycle"
                  ~community:"phrv-lifecycle-home" Rv.Accept)
          in
          (match r with
          | Error Rv.Target_ineligible -> ()
          | Ok _ -> Alcotest.fail "drift-first acceptance succeeded"
          | Error e ->
              Alcotest.failf "drift-first acceptance: %s"
                (Home_review_fixture.error_str e));
          let* status = status_of conn rid2 in
          Alcotest.(check string)
            "still pending after drift race" "pending" status;
          (* Either serialized order still permits rejection. *)
          Home_review_fixture.review_ok "reject the now-private target"
            Phr.Rejected conn ~reviewer ~slug:"phrv-lifecycle"
            ~community:"phrv-lifecycle-home" Rv.Reject))

(* === project-verification serialization === *)

let verification_serialization_case =
  db_case "review: verification change serializes through the project lock"
    (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600014L ~slug:"phrv-verif"
      in
      let* cid = insert_community conn "phrv-verif-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      (* Outcome 1: the review locks the verified project first and
         commits; the later staleness cannot unwind it. *)
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-verif" ~community:cid ()
      in
      let* () =
        Home_review_fixture.review_ok "review before staleness" Phr.Rejected
          conn ~reviewer ~slug:"phrv-verif" ~community:"phrv-verif-home"
          Rv.Reject
      in
      let* () =
        exec conn "stale afterwards" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* status = status_of conn rid in
      Alcotest.(check string) "committed review stands" "rejected" status;
      (* Outcome 2: the verification mutation's uncommitted project
         lock is held before the review starts. *)
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* rid2 =
        Home_review_fixture.request_ok "second pending" conn ~user:owner
          ~slug:"phrv-verif" ~community:cid ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held staleness"
                  Home_request_fixture.q_set_verification (project, "stale"))
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer ~slug:"phrv-verif"
                  ~community:"phrv-verif-home" Rv.Accept)
          in
          (match r with
          | Error Rv.Project_unavailable -> ()
          | Ok _ -> Alcotest.fail "stale-first acceptance succeeded"
          | Error e ->
              Alcotest.failf "stale-first acceptance: %s"
                (Home_review_fixture.error_str e));
          let* status = status_of conn rid2 in
          Alcotest.(check string)
            "still pending after staleness race" "pending" status;
          (* Rejection succeeds even when a further verification loss —
             here stale → revoked — commits first under the same held
             project lock: the pending request can always be closed. *)
          let* r =
            Db_fixture.serialized_mutation_first conn2
              ~mutate:(fun () ->
                exec conn2 "held revocation"
                  Home_request_fixture.q_set_verification (project, "revoked"))
              ~launch:(fun () ->
                Home_review_fixture.review conn ~reviewer ~slug:"phrv-verif"
                  ~community:"phrv-verif-home" Rv.Reject)
          in
          (match r with
          | Ok w ->
              Alcotest.(check string)
                "revoked-first rejection result" (status_str Phr.Rejected)
                (status_str (Rv.resulting_status w))
          | Error e ->
              Alcotest.failf "revoked-first rejection: %s"
                (Home_review_fixture.error_str e));
          let* _, ((_, rev), _) = relation_row conn rid2 in
          let* status = status_of conn rid2 in
          Alcotest.(check string)
            "rejected despite revocation" "rejected" status;
          Alcotest.(check (option int)) "reviewer recorded" (Some reviewer) rev;
          Lwt.return_unit))

(* === failure rollback === *)

let rollback_case =
  db_case "review: injected update failure leaves the pending row intact"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945600015L ~slug:"phrv-fail"
      in
      let* cid = insert_community conn "phrv-fail-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let* rid =
        Home_review_fixture.request_ok "poisoned pending" conn ~user:owner
          ~slug:"phrv-fail" ~community:cid ~note:phrv_poison_note ()
      in
      let* before = relation_sig conn rid in
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          let* () =
            Home_review_fixture.review_expect "poisoned review" Rv.Storage_error
              conn ~reviewer ~slug:"phrv-fail" ~community:"phrv-fail-home"
              Rv.Accept
          in
          let* () =
            check_pending_unchanged "after injected failure" conn rid before
          in
          let* status = status_of conn rid in
          Alcotest.(check string) "still pending" "pending" status;
          let* members =
            find conn "members" Home_request_fixture.q_count_members cid
          in
          Alcotest.(check int) "no membership side effect" 0 members;
          let* mods =
            find conn "moderators" Home_request_fixture.q_count_moderators cid
          in
          Alcotest.(check int) "moderator roster untouched" 1 mods;
          let* stewards =
            find conn "stewards"
              Home_request_fixture.q_count_stewards_for_project project
          in
          Alcotest.(check int) "stewardship untouched" 1 stewards;
          Lwt.return_unit)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === credential and privacy sweep === *)

let privacy_case =
  db_case "review: no credential fixture reaches the reviewed row" (fun conn ->
      let* owner = insert_user conn "phrv_owner" in
      let* reviewer = insert_user conn "phrv_mod" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945600016L ~slug:"phrv-creds"
      in
      let* cid = insert_community conn "phrv-creds-home" in
      let* () =
        Home_review_fixture.add_top_mod conn ~user:reviewer ~community:cid
      in
      let credentials =
        [
          "phrv-access-token-A1x";
          "phrv-refresh-token-B2x";
          "phrv-authorization-code-C3x";
          "phrv-pkce-verifier-D4x";
          "phrv-client-secret-E5x";
          "phrv-oauth-state-F6x";
          "phrv-session-binding-G7x";
          "945600999" (* installation id fixture *);
          "945700999" (* account id fixture *);
          "946099999" (* repository id fixture *);
          "phrv-private-repo-name-H8x";
          "phrv-private-repo-desc-I9x";
        ]
      in
      let* rid =
        Home_review_fixture.request_ok "pending" conn ~user:owner
          ~slug:"phrv-creds" ~community:cid ~note:"Ordinary private note." ()
      in
      let* () =
        Home_review_fixture.review_ok "accept" Phr.Accepted conn ~reviewer
          ~slug:"phrv-creds" ~community:"phrv-creds-home" Rv.Accept
      in
      let* blob =
        find conn "row blob" Home_request_fixture.q_relation_text_blob rid
      in
      List.iter
        (fun credential ->
          Alcotest.(check bool)
            "credential absent from reviewed row" false
            (contains ~needle:credential blob))
        credentials;
      (* A credential-shaped value deliberately supplied as the note is
         legitimate there — and only there, surviving review
         untouched. *)
      let* () = exec conn "free slot" Home_request_fixture.q_mark_removed rid in
      let deliberate = List.hd credentials in
      let* rid2 =
        Home_review_fixture.request_ok "deliberate note" conn ~user:owner
          ~slug:"phrv-creds" ~community:cid ~note:deliberate ()
      in
      let* () =
        Home_review_fixture.review_ok "reject deliberate note" Phr.Rejected conn
          ~reviewer ~slug:"phrv-creds" ~community:"phrv-creds-home" Rv.Reject
      in
      let* _, (_, (stored_note, _)) = relation_row conn rid2 in
      Alcotest.(check bool)
        "deliberate note preserved verbatim" true
        (stored_note = Some deliberate);
      let* nonnote =
        find conn "non-note blob" Home_request_fixture.q_relation_nonnote_blob
          rid2
      in
      Alcotest.(check bool)
        "note value nowhere else in the row" false
        (contains ~needle:deliberate nonnote);
      Lwt.return_unit)

let suite =
  [
    pure_inputs_case;
    accept_public_case;
    accept_unlisted_case;
    reject_case;
    authority_variants_case;
    unauthorized_case;
    project_unavailable_case;
    stale_revoked_rejection_case;
    community_cases;
    ineligible_target_case;
    review_unavailable_case;
    pending_corruption_case;
    concurrent_reviews_case;
    role_removal_serialization_case;
    lifecycle_serialization_case;
    verification_serialization_case;
    rollback_case;
    privacy_case;
  ]

let suites =
  (* Moderator review of pending home requests: durable top-mod/admin
       authorization, accept/reject lifecycle, eligibility, and
       serialization races. Database-gated. *)
  [ ("project_home_review_store", suite) ]
