module Phr = Earde.Project_home_relation

(* === Project home request store (Project_home_request_store) ===
   Transactional creation of a pending home-relation request, driven over
   verified permanent projects built through the real draft/selection/
   finalization chain. Database-gated (EARDE_TEST_DATABASE_URL, same
   opt-in as Mod_scope) with its own reserved external-installation-id
   range 944200001..944200999 (hence account ids 944300001..944300999,
   which also scope the permanent-project cleanup), phrq_% usernames, and
   phrq-% community slugs so no suite shares fixtures. Pure validation is
   proven pre-SQL against a deliberately disconnected connection. The
   pure relation values come only from Project_home_relation's real
   constructors. Credential assertions are boolean, so no fixture byte
   reaches test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rq = Earde.Project_home_request_store

module Fin = Earde.Project_finalization_store

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let contains = Html_assert.occurs

(* Projects first (stewards, repositories, and home relations cascade
   from them, and stewards RESTRICT-protect installations); drafts next
   (installations are RESTRICT-protected while referenced); communities
   cascade their remaining relations; installations last. The LIKE
   pattern also catches deliberately corrupted phrq- slugs. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 944300001 AND 944300999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 944300001 AND 944300999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 944200001 AND 944200999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phrq-%'"
    ; "DELETE FROM users WHERE username LIKE 'phrq_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 944200001 AND 944200999"
    ]

let q_active_community_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT community_id FROM community_projects \
   WHERE project_id = $1 AND status IN ('pending', 'accepted')"

let q_count_pending_for_community =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_projects \
   WHERE community_id = $1 AND status = 'pending'"

let q_clear_installation_provenance =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE github_installations SET connected_by_user_id = NULL \
   WHERE id = $1"

(* Community-side probes. *)
let q_delete_community =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM communities WHERE id = $1"

(* Positive ids guaranteed absent, for the missing-community probe. *)
let q_absent_community_id =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

(* Test-only failure injection for the rollback case: an AFTER trigger
   scoped to one reserved note value, so the failure fires only after
   the insertion was genuinely attempted. Installed and dropped inside
   that case alone (IF EXISTS drops keep the cleanup idempotent even
   after a mid-case failure); production migrations are untouched. The
   function body uses plain string quoting — Caqti templates reserve
   '$'. *)
let phrq_poison_note = "phrq poison marker"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION phrq_fail_insert_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''phrq fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER phrq_fail_insert
   AFTER INSERT ON community_projects
   FOR EACH ROW WHEN (NEW.request_note = 'phrq poison marker')
   EXECUTE FUNCTION phrq_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS phrq_fail_insert ON community_projects"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS phrq_fail_insert_fn()"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. The
   connection is disconnected deterministically — the gated suite is
   large enough that leaving per-case connections to the GC brushes
   against Postgres's max_connections. *)
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

let count_for_project conn project =
  find conn "relation count" Home_request_fixture.q_count_for_project project

let check_no_relations label conn project =
  let* n = count_for_project conn project in
  Alcotest.(check int) label 0 n;
  Lwt.return_unit

let mark label conn q relation =
  exec conn label q relation

(* === pure input validation === *)

let pure_inputs_case =
  db_case "request: invalid inputs rejected before any SQL" (fun _conn ->
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
      let pending = Home_request_fixture.phr_fresh_pending () in
      let expect label e ~user ~slug ~community relation =
        Home_request_fixture.create_expect label e dead ~user ~slug ~community relation
      in
      let* () =
        expect "user id 0" Rq.Invalid_user_id ~user:0 ~slug:"phrq-a"
          ~community:1 pending
      in
      let* () =
        expect "negative user id" Rq.Invalid_user_id ~user:(-7)
          ~slug:"phrq-a" ~community:1 pending
      in
      let* () =
        expect "user checked before slug" Rq.Invalid_user_id ~user:0
          ~slug:"NOT A SLUG" ~community:0 pending
      in
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            expect "invalid project slug" Rq.Invalid_project_slug ~user:1
              ~slug:bad ~community:1 pending)
          [ ""
          ; "Phrq-Upper"
          ; "phrq slug"
          ; " phrq-a"
          ; "phrq-a "
          ; "phrq_a"
          ; "phrq/a"
          ; "-phrq"
          ; "phrq-"
          ; "phrq--a"
          ; String.make 81 'a'
          ]
      in
      let* () =
        expect "community id 0" Rq.Invalid_community_id ~user:1
          ~slug:"phrq-a" ~community:0 pending
      in
      let* () =
        expect "negative community id" Rq.Invalid_community_id ~user:1
          ~slug:"phrq-a" ~community:(-4) pending
      in
      let* () =
        expect "accepted relation" Rq.Invalid_relation ~user:1
          ~slug:"phrq-a" ~community:1
          (Home_request_fixture.phr_fresh_accepted ())
      in
      let* () =
        expect "rejected relation" Rq.Invalid_relation ~user:1
          ~slug:"phrq-a" ~community:1
          (Home_request_fixture.phr_fresh_rejected ())
      in
      expect "removed relation" Rq.Invalid_relation ~user:1 ~slug:"phrq-a"
        ~community:1
        (Home_request_fixture.phr_fresh_removed ()))

(* === successful request === *)

let success_case =
  db_case "request: complete pending request round-trips exactly"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200001L
          ~slug:"phrq-success"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-home" in
      let note =
        "Prima riga — gi\xc3\xa0 discutiamo qui \xe2\x98\x95\n\
         \tseconda riga indentata"
      in
      let relation =
        Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:(Some note))
      in
      let* created =
        Home_request_fixture.create_ok "request" conn ~user:uid ~slug:"phrq-success" ~community
          relation
      in
      Alcotest.(check bool) "relation id positive" true
        (Rq.relation_id created > 0L);
      let* ( ((rp, rc), (rtype, status))
           , ((req, rev), (stored_note, (has_reviewed, has_removed, upd_ge)))
           ) =
        Home_request_fixture.relation_row conn (Rq.relation_id created)
      in
      Alcotest.(check int64) "exact project fk" project rp;
      Alcotest.(check int) "exact community fk" community rc;
      Alcotest.(check string) "relation type home" "home" rtype;
      Alcotest.(check string) "status pending" "pending" status;
      Alcotest.(check (option int)) "requester is supplied user"
        (Some uid) req;
      Alcotest.(check (option int)) "no reviewer" None rev;
      Alcotest.(check (option string)) "canonical note byte-exact"
        (Some note) stored_note;
      Alcotest.(check bool) "reviewed_at NULL" false has_reviewed;
      Alcotest.(check bool) "removed_at NULL" false has_removed;
      Alcotest.(check bool) "timestamps coherent" true upd_ge;
      let* n = count_for_project conn project in
      Alcotest.(check int) "exactly one relation" 1 n;
      (* Submitting a request grants nothing: no membership, no
         moderation, no stewardship change. *)
      let* members = find conn "members" Home_request_fixture.q_count_members community in
      Alcotest.(check int) "no membership created" 0 members;
      let* mods = find conn "moderators" Home_request_fixture.q_count_moderators community in
      Alcotest.(check int) "no moderator created" 0 mods;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
      in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      Lwt.return_unit)

(* === note variants === *)

let note_variants_case =
  db_case "request: note variants persist through the domain constructor"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, _project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200002L ~slug:"phrq-notes"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-notes-home" in
      let utf8 = "Progetto Citt\xc3\xa0\nseconda riga \xe2\x98\x95" in
      let exact_2000 = String.make 2000 'a' in
      Lwt_list.iter_s
        (fun (label, raw, expected) ->
          let relation =
            Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:raw)
          in
          let* created =
            Home_request_fixture.create_ok label conn ~user:uid ~slug:"phrq-notes" ~community
              relation
          in
          let* _, (_, (stored_note, _)) =
            Home_request_fixture.relation_row conn (Rq.relation_id created)
          in
          Alcotest.(check (option string)) label expected stored_note;
          (* Free the active slot for the next variant; the historical
             row stays behind. *)
          mark "free slot" conn Home_request_fixture.q_mark_rejected (Rq.relation_id created))
        [ ("absent note stays NULL", None, None)
        ; ("blank note canonicalizes to NULL", Some " \t\r\n ", None)
        ; ("utf-8 multiline note", Some utf8, Some utf8)
        ; ("exact 2000-character note", Some exact_2000, Some exact_2000)
        ])

(* === project authorization === *)

let project_unavailable_case =
  db_case "request: every unavailable-project cause collapses identically"
    (fun conn ->
      let* a = insert_user conn "phrq_a" in
      let* b = insert_user conn "phrq_b" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:a ~ext_id:944200003L ~slug:"phrq-auth"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-auth-home" in
      let pending () = Home_request_fixture.phr_fresh_pending () in
      let expect label ~user ~slug =
        Home_request_fixture.create_expect label Rq.Project_unavailable conn ~user ~slug
          ~community (pending ())
      in
      (* Missing project: canonical grammar, nothing stored under it. *)
      let* () = expect "missing project" ~user:a ~slug:"phrq-absent" in
      (* Another user's project. *)
      let* () = expect "foreign project" ~user:b ~slug:"phrq-auth" in
      (* Stale, then revoked verification. *)
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification (project, "stale")
      in
      let* () = expect "stale project" ~user:a ~slug:"phrq-auth" in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification (project, "revoked")
      in
      let* () = expect "revoked project" ~user:a ~slug:"phrq-auth" in
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      (* Deleted stewardship: the creator keeps created_by provenance
         but loses authorization entirely. *)
      let* () = exec conn "drop steward" Home_request_fixture.q_delete_steward (project, a) in
      let* () =
        expect "creator without stewardship" ~user:a ~slug:"phrq-auth"
      in
      check_no_relations "no insert from any cause" conn project)

let project_authorization_case =
  db_case
    "request: stewardship alone authorizes — provenance never does"
    (fun conn ->
      let* a = insert_user conn "phrq_a" in
      let* b = insert_user conn "phrq_b" in
      let* inst, project =
        Home_request_fixture.make_project conn ~user:a ~ext_id:944200004L ~slug:"phrq-auth2"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-auth2-home" in
      (* Reassign creation provenance and erase installation
         provenance: authorization must not move with either. *)
      let* () = exec conn "created_by to b" Home_request_fixture.q_set_created_by (project, b) in
      let* () =
        exec conn "clear provenance" q_clear_installation_provenance inst
      in
      let* created =
        Home_request_fixture.create_ok "steward still authorized" conn ~user:a
          ~slug:"phrq-auth2" ~community (Home_request_fixture.phr_fresh_pending ())
      in
      (* The new creator-of-record is still not a steward. *)
      let* () =
        Home_request_fixture.create_expect "creator-of-record without stewardship"
          Rq.Project_unavailable conn ~user:b ~slug:"phrq-auth2"
          ~community (Home_request_fixture.phr_fresh_pending ())
      in
      let* () =
        mark "free slot" conn Home_request_fixture.q_mark_rejected (Rq.relation_id created)
      in
      (* A second steward may submit. *)
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward (project, b, inst)
      in
      let* created_b =
        Home_request_fixture.create_ok "second steward submits" conn ~user:b ~slug:"phrq-auth2"
          ~community (Home_request_fixture.phr_fresh_pending ())
      in
      let* ((_, rc), (_, status)), ((req, _), _) =
        Home_request_fixture.relation_row conn (Rq.relation_id created_b)
      in
      Alcotest.(check int) "second steward's request targets community"
        community rc;
      Alcotest.(check string) "second steward's request pending" "pending"
        status;
      Alcotest.(check (option int)) "requester is the second steward"
        (Some b) req;
      Lwt.return_unit)

(* === community eligibility === *)

let community_unavailable_case =
  db_case_lifecycle_relaxed
    "request: every ineligible-community cause collapses identically"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200005L ~slug:"phrq-celig"
      in
      let expect label community =
        Home_request_fixture.create_expect label Rq.Community_unavailable conn ~user:uid
          ~slug:"phrq-celig" ~community (Home_request_fixture.phr_fresh_pending ())
      in
      (* Missing community. *)
      let* absent = find conn "absent id" q_absent_community_id () in
      let* () = expect "missing community" absent in
      (* Legacy/general communities, both visibilities. *)
      let* legacy = Home_request_fixture.insert_community ~network:false conn "phrq-legacy" in
      let* () = expect "legacy public community" legacy in
      let* legacy_private =
        Home_request_fixture.insert_community ~network:false ~visibility:"private"
          ~indexable:false ~discoverable:false conn "phrq-legacy-priv"
      in
      let* () = expect "legacy private community" legacy_private in
      (* Network setup draft in its canonical private shape. *)
      let* draft =
        Home_request_fixture.insert_community ~onboarding:"draft" ~visibility:"private"
          ~indexable:false ~discoverable:false conn "phrq-draft"
      in
      let* () = expect "setup draft" draft in
      (* A representable ineligible drift: draft state with public
         flags is still unpublished. *)
      let* draft_public =
        Home_request_fixture.insert_community ~onboarding:"draft" conn "phrq-draft-pub"
      in
      let* () = expect "unpublished public-flagged draft" draft_public in
      (* Fully private published network community (representable —
         the never-private-after-publish invariant is app-level). *)
      let* private_network =
        Home_request_fixture.insert_community ~visibility:"private" ~indexable:false
          ~discoverable:false conn "phrq-private"
      in
      let* () = expect "fully private network community" private_network in
      (* Deletion boundary: an eligible community deleted before the
         request locks it is unavailable, never an FK diagnostic. *)
      let* deleted = Home_request_fixture.insert_community conn "phrq-deleted" in
      let* () = exec conn "delete community" q_delete_community deleted in
      let* () = expect "deleted community" deleted in
      check_no_relations "no insert from any cause" conn project)

let eligible_modes_case =
  db_case "request: public and unlisted published networks both eligible"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, _project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200006L ~slug:"phrq-modes"
      in
      let* public = Home_request_fixture.insert_community conn "phrq-mode-public" in
      let* unlisted =
        Home_request_fixture.insert_community ~indexable:false ~discoverable:false conn
          "phrq-mode-unlisted"
      in
      let* created_public =
        Home_request_fixture.create_ok "public network" conn ~user:uid ~slug:"phrq-modes"
          ~community:public (Home_request_fixture.phr_fresh_pending ())
      in
      let* ((_, rc_pub), _), _ =
        Home_request_fixture.relation_row conn (Rq.relation_id created_public)
      in
      Alcotest.(check int) "public request targets public" public rc_pub;
      let* () =
        mark "free slot" conn Home_request_fixture.q_mark_rejected
          (Rq.relation_id created_public)
      in
      let* created_unlisted =
        Home_request_fixture.create_ok "unlisted network" conn ~user:uid ~slug:"phrq-modes"
          ~community:unlisted (Home_request_fixture.phr_fresh_pending ())
      in
      let* ((_, rc), _), _ =
        Home_request_fixture.relation_row conn (Rq.relation_id created_unlisted)
      in
      Alcotest.(check int) "unlisted request targets unlisted" unlisted rc;
      Lwt.return_unit)

(* === existing active relation === *)

let active_relation_case =
  db_case "request: any active home blocks a new request" (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200007L ~slug:"phrq-active"
      in
      let* c1 = Home_request_fixture.insert_community conn "phrq-active-one" in
      let* c2 = Home_request_fixture.insert_community conn "phrq-active-two" in
      let* created =
        Home_request_fixture.create_ok "first request" conn ~user:uid ~slug:"phrq-active"
          ~community:c1 (Home_request_fixture.phr_fresh_pending ())
      in
      let expect label community =
        Home_request_fixture.create_expect label Rq.Active_home_exists conn ~user:uid
          ~slug:"phrq-active" ~community (Home_request_fixture.phr_fresh_pending ())
      in
      (* Pending blocks, same target or another. *)
      let* () = expect "pending blocks same target" c1 in
      let* () = expect "pending blocks different target" c2 in
      let* n = count_for_project conn project in
      Alcotest.(check int) "still exactly one row" 1 n;
      (* Accepted blocks identically. *)
      let* () =
        mark "accept home" conn Home_request_fixture.q_mark_accepted (Rq.relation_id created)
      in
      let* () = expect "accepted blocks same target" c1 in
      let* () = expect "accepted blocks different target" c2 in
      let* n = count_for_project conn project in
      Alcotest.(check int) "no second row" 1 n;
      Lwt.return_unit)

(* === historical relations === *)

let historical_case =
  db_case "request: rejected and removed history never blocks" (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200008L ~slug:"phrq-hist"
      in
      let* c1 = Home_request_fixture.insert_community conn "phrq-hist-one" in
      let* c2 = Home_request_fixture.insert_community conn "phrq-hist-two" in
      let* first =
        Home_request_fixture.create_ok "first request" conn ~user:uid ~slug:"phrq-hist"
          ~community:c1 (Home_request_fixture.phr_fresh_pending ())
      in
      let* () =
        mark "reject first" conn Home_request_fixture.q_mark_rejected (Rq.relation_id first)
      in
      let* rejected_sig =
        find conn "rejected sig" Home_request_fixture.q_relation_sig (Rq.relation_id first)
      in
      (* Rejected history: a fresh request succeeds. *)
      let* second =
        Home_request_fixture.create_ok "after rejection" conn ~user:uid ~slug:"phrq-hist"
          ~community:c2 (Home_request_fixture.phr_fresh_pending ())
      in
      let* () =
        mark "accept second" conn Home_request_fixture.q_mark_accepted (Rq.relation_id second)
      in
      let* () =
        mark "remove second" conn Home_request_fixture.q_mark_removed (Rq.relation_id second)
      in
      let* removed_sig =
        find conn "removed sig" Home_request_fixture.q_relation_sig (Rq.relation_id second)
      in
      (* Removed history: a fresh request succeeds too. *)
      let* third =
        Home_request_fixture.create_ok "after removal" conn ~user:uid ~slug:"phrq-hist"
          ~community:c1 (Home_request_fixture.phr_fresh_pending ())
      in
      let* rejected_after =
        find conn "rejected sig after" Home_request_fixture.q_relation_sig
          (Rq.relation_id first)
      in
      let* removed_after =
        find conn "removed sig after" Home_request_fixture.q_relation_sig
          (Rq.relation_id second)
      in
      Alcotest.(check string) "rejected history unchanged" rejected_sig
        rejected_after;
      Alcotest.(check string) "removed history unchanged" removed_sig
        removed_after;
      let* total = count_for_project conn project in
      Alcotest.(check int) "full history retained" 3 total;
      let* active =
        find conn "active count" Home_request_fixture.q_count_active_for_project project
      in
      Alcotest.(check int) "one active relation" 1 active;
      let* ((_, rc), (_, status)), _ =
        Home_request_fixture.relation_row conn (Rq.relation_id third)
      in
      Alcotest.(check int) "fresh request target" c1 rc;
      Alcotest.(check string) "fresh request pending" "pending" status;
      Lwt.return_unit)

(* === concurrency === *)

let same_project_same_target_race_case =
  db_case "request: concurrent identical requests leave one pending row"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200009L ~slug:"phrq-race"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-race-home" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* results =
            Lwt.both
              (Home_request_fixture.create conn ~user:uid ~slug:"phrq-race" ~community
                 (Home_request_fixture.phr_fresh_pending ()))
              (Home_request_fixture.create conn2 ~user:uid ~slug:"phrq-race" ~community
                 (Home_request_fixture.phr_fresh_pending ()))
          in
          let _ =
            Home_request_fixture.ok_and_error "identical race" Rq.Active_home_exists results
          in
          let* total = count_for_project conn project in
          Alcotest.(check int) "exactly one row" 1 total;
          let* active =
            find conn "active count" Home_request_fixture.q_count_active_for_project project
          in
          Alcotest.(check int) "exactly one active row" 1 active;
          Lwt.return_unit))

let same_project_different_targets_race_case =
  db_case "request: concurrent different-target requests leave one winner"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200010L ~slug:"phrq-race2"
      in
      let* c1 = Home_request_fixture.insert_community conn "phrq-race2-one" in
      let* c2 = Home_request_fixture.insert_community conn "phrq-race2-two" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* results =
            Lwt.both
              (Home_request_fixture.create conn ~user:uid ~slug:"phrq-race2" ~community:c1
                 (Home_request_fixture.phr_fresh_pending ()))
              (Home_request_fixture.create conn2 ~user:uid ~slug:"phrq-race2" ~community:c2
                 (Home_request_fixture.phr_fresh_pending ()))
          in
          let _ =
            Home_request_fixture.ok_and_error "different-target race" Rq.Active_home_exists
              results
          in
          let* total = count_for_project conn project in
          Alcotest.(check int) "no partial second row" 1 total;
          (* Which target won is deliberately unasserted. *)
          let* winner =
            find conn "winning target" q_active_community_for_project
              project
          in
          Alcotest.(check bool) "winner is one of the two targets" true
            (winner = c1 || winner = c2);
          Lwt.return_unit))

let different_projects_race_case =
  db_case "request: one community accepts concurrent distinct projects"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, p1 =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200011L ~slug:"phrq-multi-a"
      in
      let* _, p2 =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200012L ~slug:"phrq-multi-b"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-multi-home" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Home_request_fixture.create conn ~user:uid ~slug:"phrq-multi-a" ~community
                 (Home_request_fixture.phr_fresh_pending ()))
              (Home_request_fixture.create conn2 ~user:uid ~slug:"phrq-multi-b" ~community
                 (Home_request_fixture.phr_fresh_pending ()))
          in
          let check_ok label = function
            | Ok _ -> ()
            | Error e -> Alcotest.failf "%s: %s" label (Home_request_fixture.error_str e)
          in
          check_ok "first project" r1;
          check_ok "second project" r2;
          let* n1 =
            find conn "p1 active" Home_request_fixture.q_count_active_for_project p1
          in
          let* n2 =
            find conn "p2 active" Home_request_fixture.q_count_active_for_project p2
          in
          Alcotest.(check int) "one active relation each (a)" 1 n1;
          Alcotest.(check int) "one active relation each (b)" 1 n2;
          let* pending =
            find conn "community pending" q_count_pending_for_community
              community
          in
          Alcotest.(check int) "community hosts both requests" 2 pending;
          Lwt.return_unit))

(* === failure rollback === *)

let rollback_case =
  db_case "request: injected insert failure leaves no trace" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200013L ~slug:"phrq-fail"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-fail-home" in
      let* project_before = find conn "project sig" Home_request_fixture.q_project_sig project in
      let* community_before =
        find conn "community sig" Home_request_fixture.q_community_sig community
      in
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
          let relation =
            Home_request_fixture.phr_expect_ok
              (Phr.create_pending ~request_note:(Some phrq_poison_note))
          in
          let* () =
            Home_request_fixture.create_expect "poisoned request" Rq.Storage_error conn
              ~user:uid ~slug:"phrq-fail" ~community relation
          in
          let* () =
            check_no_relations "no relation persists" conn project
          in
          let* project_after =
            find conn "project sig after" Home_request_fixture.q_project_sig project
          in
          let* community_after =
            find conn "community sig after" Home_request_fixture.q_community_sig community
          in
          Alcotest.(check string) "project unchanged" project_before
            project_after;
          Alcotest.(check string) "community unchanged" community_before
            community_after;
          let* members = find conn "members" Home_request_fixture.q_count_members community in
          Alcotest.(check int) "no membership side effect" 0 members;
          let* mods =
            find conn "moderators" Home_request_fixture.q_count_moderators community
          in
          Alcotest.(check int) "no moderator side effect" 0 mods;
          let* stewards =
            find conn "stewards" Home_request_fixture.q_count_stewards_for_project project
          in
          Alcotest.(check int) "stewardship untouched" 1 stewards;
          Lwt.return_unit)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === durable inconsistency === *)

let inconsistent_case =
  db_case_lifecycle_relaxed "request: malformed durable community data is corruption"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200014L ~slug:"phrq-corrupt"
      in
      (* A non-addressable stored slug. The scoped identity constraints
         (migration 20260726120000) now bar this corruption at the
         database boundary, so they are dropped for this probe alone and
         restored under Lwt.finalize once the corrupt row is deleted —
         the store-level defense itself must stay observable. *)
      let* bad_slug = Home_request_fixture.insert_community conn "phrq-bad-slug" in
      let* () = Network_community_constraints.drop conn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () =
              exec conn "corrupt slug" Home_request_fixture.q_corrupt_community_slug
                (bad_slug, "phrq-bad slug")
            in
            Home_request_fixture.create_expect "corrupted slug" Rq.Inconsistent_data conn
              ~user:uid ~slug:"phrq-corrupt" ~community:bad_slug
              (Home_request_fixture.phr_fresh_pending ()))
          (fun () ->
            let* () =
              exec conn "purge corrupt row" q_delete_community bad_slug
            in
            Network_community_constraints.restore conn)
      in
      (* The mixed published flag shape — neither fully listed nor
         fully unlisted — is invalid per the shared lifecycle rule. *)
      let* mixed = Home_request_fixture.insert_community conn "phrq-mixed" in
      let* () = exec conn "mix flags" Home_request_fixture.q_mix_community_flags mixed in
      let* () =
        Home_request_fixture.create_expect "mixed lifecycle flags" Rq.Inconsistent_data conn
          ~user:uid ~slug:"phrq-corrupt" ~community:mixed
          (Home_request_fixture.phr_fresh_pending ())
      in
      (* visibility/onboarding off-enum corruption is blocked by their
         DB CHECKs and a non-positive relation id by BIGSERIAL: those
         store branches stay defensive and untestable without dropping
         production constraints. *)
      check_no_relations "no insert from corruption" conn project)

(* === credential and privacy sweep === *)

let credential_case =
  db_case "request: no credential fixture reaches the relation row"
    (fun conn ->
      let* uid = insert_user conn "phrq_a" in
      let* _, _project =
        Home_request_fixture.make_project conn ~user:uid ~ext_id:944200015L ~slug:"phrq-creds"
      in
      let* community = Home_request_fixture.insert_community conn "phrq-creds-home" in
      let credentials =
        [ "phrq-access-token-A1x"
        ; "phrq-refresh-token-B2x"
        ; "phrq-authorization-code-C3x"
        ; "phrq-pkce-verifier-D4x"
        ; "phrq-client-secret-E5x"
        ; "phrq-oauth-state-F6x"
        ; "phrq-session-binding-G7x"
        ; "944200999" (* installation id fixture *)
        ; "944300999" (* account id fixture *)
        ; "944699999" (* repository id fixture *)
        ; "phrq-private-repo-name-H8x"
        ; "phrq-private-repo-desc-I9x"
        ]
      in
      let* created =
        Home_request_fixture.create_ok "ordinary request" conn ~user:uid ~slug:"phrq-creds"
          ~community
          (Home_request_fixture.phr_expect_ok
             (Phr.create_pending
                ~request_note:(Some "Ordinary private note.")))
      in
      let* blob =
        find conn "row blob" Home_request_fixture.q_relation_text_blob (Rq.relation_id created)
      in
      List.iter
        (fun credential ->
          Alcotest.(check bool) "credential absent from relation row"
            false
            (contains ~needle:credential blob))
        credentials;
      let* () =
        mark "free slot" conn Home_request_fixture.q_mark_rejected (Rq.relation_id created)
      in
      (* A credential-shaped value deliberately supplied as the note is
         legitimate there — and only there. *)
      let deliberate = List.hd credentials in
      let* second =
        Home_request_fixture.create_ok "deliberate note" conn ~user:uid ~slug:"phrq-creds"
          ~community
          (Home_request_fixture.phr_expect_ok
             (Phr.create_pending ~request_note:(Some deliberate)))
      in
      let* _, (_, (stored_note, _)) =
        Home_request_fixture.relation_row conn (Rq.relation_id second)
      in
      Alcotest.(check bool) "deliberate note stored verbatim" true
        (stored_note = Some deliberate);
      let* nonnote =
        find conn "non-note blob" Home_request_fixture.q_relation_nonnote_blob
          (Rq.relation_id second)
      in
      Alcotest.(check bool) "note value nowhere else in the row" false
        (contains ~needle:deliberate nonnote);
      Lwt.return_unit)

let suite =
  [ pure_inputs_case; success_case; note_variants_case;
    project_unavailable_case; project_authorization_case;
    community_unavailable_case; eligible_modes_case;
    active_relation_case; historical_case;
    same_project_same_target_race_case;
    same_project_different_targets_race_case;
    different_projects_race_case; rollback_case; inconsistent_case;
    credential_case ]

let suites =
    (* Project home request store: transactional owner-authorized pending
       request against an existing eligible published network community,
       with index-arbitrated active-home races. Database-gated. *)
  [ ("project_home_request_store", suite)
  ]
