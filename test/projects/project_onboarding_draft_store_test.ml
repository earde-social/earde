(* === Project-onboarding draft store (Project_onboarding_draft_store) ===
   The whole contract lives in one explicit transaction — authoritative
   installation lookup, index-arbitrated create-or-refresh, complete
   snapshot replacement — so only Postgres can pin it down. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope. Autocommit on purpose:
   the store manages its own transaction on the connection (an outer test
   transaction would collide with its START) and the concurrency cases need
   rows visible across two connections. Fixtures use a reserved
   external-installation-id range and podstore_% usernames, cleaned before
   and after each case. Abstract inputs go through the real clients —
   Github_oauth_token_exchange.exchange, Github_user_installations.verify,
   Github_user_installation_repositories.list_public — against scripted
   transports; no test-only constructor exists. Credential-fixture
   assertions are boolean, so no token bytes reach test output on
   failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Store = Earde.Project_onboarding_draft_store

(* Fixtures — reserved external-installation-id range
   938000001..938000999 and podstore_% usernames so cleanup is targeted
   and idempotent. Drafts go first (installations are RESTRICT-protected
   while referenced); snapshots cascade from drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 938000001 AND 938000999)";
      "DELETE FROM users WHERE username LIKE 'podstore_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       938000001 AND 938000999";
    ]

(* A positive user id guaranteed absent from users, for the FK-failure
   case. *)
let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

let q_lifecycle_epochs =
  Caqti_type.(int64 ->! t2 (option float) (option float))
    "SELECT EXTRACT(EPOCH FROM completed_at)::float8,\n\
    \          EXTRACT(EPOCH FROM cancelled_at)::float8\n\
    \   FROM project_onboarding_drafts WHERE id = $1"

(* Old-snapshot fixture: flags a refresh must reset. Primary rides on
   position 1 so primary-implies-selected holds. *)
let q_mark_selected =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_draft_repositories\n\
    \   SET is_selected = TRUE, is_primary = (position = 1)\n\
    \   WHERE draft_id = $1"

(* Test-only failure injection for the rollback case: a trigger scoped to
   one reserved fixture repository id. Installed and dropped inside that
   case alone (IF EXISTS drops keep the cleanup idempotent even after a
   mid-case failure); production migrations are untouched. The function
   body uses plain string quoting — Caqti templates reserve '$'. *)
let podstore_poison_repo_id = 938999999L

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE FUNCTION podstore_fail_insert_fn() RETURNS trigger\n\
    \   LANGUAGE plpgsql\n\
    \   AS 'BEGIN RAISE EXCEPTION ''podstore fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER podstore_fail_insert\n\
    \   BEFORE INSERT ON project_onboarding_draft_repositories\n\
    \   FOR EACH ROW WHEN (NEW.github_repository_id = 938999999)\n\
    \   EXECUTE FUNCTION podstore_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS podstore_fail_insert\n\
    \   ON project_onboarding_draft_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS podstore_fail_insert_fn()"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. *)
let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = Db_fixture.or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = Db_fixture.or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let refresh_expect label expected conn ~user v set =
  let* r = Project_fixture.refresh conn ~user v set in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Project_fixture.draft_error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Project_fixture.draft_error_str expected)
        (Project_fixture.draft_error_str e);
      Lwt.return_unit

let count_active_for_user conn uid =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find Project_fixture.q_count_active_for_user uid in
  Db_fixture.or_fail "active draft count" r

(* === input validation === *)

let invalid_user_case =
  db_case "refresh: non-positive user ids rejected before SQL" (fun conn ->
      let* v =
        Project_fixture.verified ~installation_id:938000001L
          ~account_id:938100001L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100001L
              ~owner_login:"podstore-owner" ~id:938600001L ~name:"alpha" ();
          ]
      in
      let* () =
        refresh_expect "user id 0" Store.Invalid_user_id conn ~user:0 v set
      in
      refresh_expect "negative user id" Store.Invalid_user_id conn ~user:(-7) v
        set)

(* === installation authorization === *)

(* One rejected-authorization scaffold: [prepare] installs (or omits) the
   local row variant; the refresh must classify as unavailable and leave
   the user draftless. *)
let auth_reject_case name ~ext_id ~target prepare =
  db_case name (fun conn ->
      let account_id = Int64.add ext_id 100000L in
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* v =
        Project_fixture.verified ~installation_id:ext_id ~account_id
          ~login:"podstore-owner" ~target ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:account_id
              ~owner_login:"podstore-owner" ~id:(Int64.add ext_id 600000L)
              ~name:"alpha" ();
          ]
      in
      let* () = prepare conn ~ext_id ~account_id in
      let* () =
        refresh_expect name Store.Installation_unavailable conn ~user:uid v set
      in
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "no draft created or modified" 0 n;
      Lwt.return_unit)

let missing_installation_case =
  auth_reject_case "refresh: missing installation row is unavailable"
    ~ext_id:938000011L ~target:"User" (fun _conn ~ext_id:_ ~account_id:_ ->
      Lwt.return_unit)

let inaccessible_installation_case =
  auth_reject_case "refresh: inaccessible installation is unavailable"
    ~ext_id:938000012L ~target:"User" (fun conn ~ext_id ~account_id ->
      let* _ =
        Project_fixture.insert_installation ~status:"inaccessible" conn ~ext_id
          ~account_id
      in
      Lwt.return_unit)

let revoked_installation_case =
  auth_reject_case "refresh: revoked installation is unavailable"
    ~ext_id:938000013L ~target:"User" (fun conn ~ext_id ~account_id ->
      let* _ =
        Project_fixture.insert_installation ~status:"revoked" ~revoked:true conn
          ~ext_id ~account_id
      in
      Lwt.return_unit)

let account_id_mismatch_case =
  auth_reject_case "refresh: account-id mismatch is unavailable"
    ~ext_id:938000014L ~target:"User" (fun conn ~ext_id ~account_id ->
      let* _ =
        Project_fixture.insert_installation conn ~ext_id
          ~account_id:(Int64.add account_id 1L)
      in
      Lwt.return_unit)

let account_type_mismatch_case =
  auth_reject_case "refresh: account-type mismatch is unavailable"
    ~ext_id:938000015L ~target:"User" (fun conn ~ext_id ~account_id ->
      let* _ =
        Project_fixture.insert_installation ~account_type:"organization" conn
          ~ext_id ~account_id
      in
      Lwt.return_unit)

let login_difference_case =
  db_case "refresh: a changed account login never blocks authorization"
    (fun conn ->
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* inst =
        Project_fixture.insert_installation ~login:"stale-stored-login" conn
          ~ext_id:938000016L ~account_id:938100016L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000016L
          ~account_id:938100016L ~login:"fresh-login" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100016L
              ~owner_login:"fresh-login" ~id:938600016L ~name:"alpha" ();
          ]
      in
      let* draft = Project_fixture.refresh_ok "refresh" conn ~user:uid v set in
      let* stored =
        Project_fixture.active_draft_id conn ~user:uid ~installation:inst
      in
      Alcotest.(check (option int64))
        "draft created despite login drift"
        (Some (Store.draft_id draft))
        stored;
      Lwt.return_unit)

let provenance_case =
  db_case "refresh: connected_by_user_id neither authorizes nor owns"
    (fun conn ->
      let* a = Db_fixture.insert_user conn "podstore_a" in
      let* b = Db_fixture.insert_user conn "podstore_b" in
      (* B connected the installation; A refreshes. *)
      let* inst =
        Project_fixture.insert_installation ~connected_by:b conn
          ~ext_id:938000017L ~account_id:938100017L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000017L
          ~account_id:938100017L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100017L
              ~owner_login:"podstore-owner" ~id:938600017L ~name:"alpha" ();
          ]
      in
      let* draft =
        Project_fixture.refresh_ok "refresh as A" conn ~user:a v set
      in
      let* (owner, _, _), _ =
        Project_fixture.draft_row conn (Store.draft_id draft)
      in
      Alcotest.(check int)
        "draft owned by the caller, not the connector" a owner;
      let* stored =
        Project_fixture.active_draft_id conn ~user:a ~installation:inst
      in
      Alcotest.(check (option int64))
        "slot keyed on the caller"
        (Some (Store.draft_id draft))
        stored;
      let* n = Project_fixture.count_for_user conn b in
      Alcotest.(check int) "connector holds no draft" 0 n;
      Lwt.return_unit)

(* === fresh create === *)

let fresh_create_case =
  db_case "refresh: fresh create stores draft and snapshot exactly" (fun conn ->
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* inst =
        Project_fixture.insert_installation conn ~ext_id:938000021L
          ~account_id:938100021L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000021L
          ~account_id:938100021L ~login:"podstore-owner" ~target:"User" ()
      in
      (* NULL description, UTF-8 description, slash-containing branch,
         archived — every metadata shape the schema admits. *)
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100021L
              ~owner_login:"podstore-owner" ~id:938600021L ~name:"alpha"
              ~description:{|"Prima descrizione — byte exact"|} ();
            Github_fixture.gur_repo ~owner_id:938100021L
              ~owner_login:"podstore-owner" ~id:938600022L ~name:"beta"
              ~default_branch:"release/v1" ~archived:true ();
          ]
      in
      let* draft = Project_fixture.refresh_ok "create" conn ~user:uid v set in
      let* stored =
        Project_fixture.active_draft_id conn ~user:uid ~installation:inst
      in
      Alcotest.(check (option int64))
        "returned id is the stored id"
        (Some (Store.draft_id draft))
        stored;
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "exactly one draft" 1 n;
      let* ( (owner, installation_record, status),
             ((no_completed, no_cancelled), (created, updated, expires)) ) =
        Project_fixture.draft_row conn (Store.draft_id draft)
      in
      Alcotest.(check int) "owner is the supplied Earde user" uid owner;
      Alcotest.(check int64)
        "local installation record stored" inst installation_record;
      Alcotest.(check string) "status active" "active" status;
      Alcotest.(check bool) "completed_at NULL" true no_completed;
      Alcotest.(check bool) "cancelled_at NULL" true no_cancelled;
      Alcotest.(check (float 0.5))
        "expiry is 24h from database creation" 86400.0 (expires -. created);
      Alcotest.(check (float 0.5))
        "updated_at rides the same clock" created updated;
      let* stored_sigs = Project_fixture.sigs conn (Store.draft_id draft) in
      Alcotest.(check (list string))
        "complete snapshot in source order, metadata byte-exact, unselected \
         and non-primary"
        [
          Project_fixture.sig_of ~position:1 ~id:938600021L
            ~account_id:938100021L ~login:"podstore-owner"
            ~description:"Prima descrizione — byte exact" "alpha";
          Project_fixture.sig_of ~position:2 ~id:938600022L
            ~account_id:938100021L ~login:"podstore-owner" ~branch:"release/v1"
            ~archived:true "beta";
        ]
        stored_sigs;
      Lwt.return_unit)

(* === active refresh === *)

let active_refresh_case =
  db_case "refresh: active draft refreshes in place, snapshot replaced"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000022L
          ~account_id:938100022L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000022L
          ~account_id:938100022L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set1 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100022L
              ~owner_login:"podstore-owner" ~id:938600201L ~name:"alpha" ();
            Github_fixture.gur_repo ~owner_id:938100022L
              ~owner_login:"podstore-owner" ~id:938600202L ~name:"beta" ();
          ]
      in
      let* d1 = Project_fixture.refresh_ok "create" conn ~user:uid v set1 in
      (* Selection state a refresh must wipe. *)
      let* r = C.exec q_mark_selected (Store.draft_id d1) in
      let* () = Db_fixture.or_fail "mark selected" r in
      let* _, (_, (created1, updated1, expires1)) =
        Project_fixture.draft_row conn (Store.draft_id d1)
      in
      (* Same repository id with fully changed metadata, plus a new
         repository, in a new order; beta disappears. *)
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100022L
              ~owner_login:"podstore-owner" ~id:938600203L ~name:"gamma" ();
            Github_fixture.gur_repo ~owner_id:938100022L
              ~owner_login:"podstore-owner" ~id:938600201L ~name:"alpha-renamed"
              ~description:{|"Nuova descrizione"|} ~default_branch:"release/v2"
              ~archived:true ();
          ]
      in
      let* d2 = Project_fixture.refresh_ok "refresh" conn ~user:uid v set2 in
      Alcotest.(check int64)
        "draft id unchanged" (Store.draft_id d1) (Store.draft_id d2);
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "still exactly one draft" 1 n;
      let* ( (_, _, status),
             ((no_completed, no_cancelled), (created2, updated2, expires2)) ) =
        Project_fixture.draft_row conn (Store.draft_id d1)
      in
      Alcotest.(check string) "still active" "active" status;
      Alcotest.(check bool) "completed_at still NULL" true no_completed;
      Alcotest.(check bool) "cancelled_at still NULL" true no_cancelled;
      Alcotest.(check (float 0.)) "created_at unchanged" created1 created2;
      Alcotest.(check bool)
        "updated_at advances or stays database-equal" true (updated2 >= updated1);
      Alcotest.(check bool) "expires_at renewed" true (expires2 >= expires1);
      Alcotest.(check (float 0.5))
        "renewed expiry is 24h from the refresh" 86400.0 (expires2 -. updated2);
      let* stored_sigs = Project_fixture.sigs conn (Store.draft_id d1) in
      Alcotest.(check (list string))
        "only the new complete set, positions rebuilt from 1, metadata \
         refreshed exactly, selection reset"
        [
          Project_fixture.sig_of ~position:1 ~id:938600203L
            ~account_id:938100022L ~login:"podstore-owner" "gamma";
          Project_fixture.sig_of ~position:2 ~id:938600201L
            ~account_id:938100022L ~login:"podstore-owner"
            ~description:"Nuova descrizione" ~branch:"release/v2" ~archived:true
            "alpha-renamed";
        ]
        stored_sigs;
      Lwt.return_unit)

(* === expired active refresh === *)

let expired_refresh_case =
  db_case "refresh: expired-but-active draft is refreshed in place" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000023L
          ~account_id:938100023L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000023L
          ~account_id:938100023L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set1 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100023L
              ~owner_login:"podstore-owner" ~id:938600231L ~name:"alpha" ();
          ]
      in
      let* d1 = Project_fixture.refresh_ok "create" conn ~user:uid v set1 in
      let* r = C.exec Project_fixture.q_backdate_draft (Store.draft_id d1) in
      let* () = Db_fixture.or_fail "backdate" r in
      let* _, (_, (created1, _, expires1)) =
        Project_fixture.draft_row conn (Store.draft_id d1)
      in
      let* expired = C.find Project_fixture.q_is_expired (Store.draft_id d1) in
      let* expired = Db_fixture.or_fail "expired probe" expired in
      Alcotest.(check bool) "fixture is expired while still active" true expired;
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100023L
              ~owner_login:"podstore-owner" ~id:938600232L ~name:"beta" ();
          ]
      in
      let* d2 = Project_fixture.refresh_ok "refresh" conn ~user:uid v set2 in
      Alcotest.(check int64)
        "same draft id reused" (Store.draft_id d1) (Store.draft_id d2);
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "no second draft" 1 n;
      let* (_, _, status), (_, (created2, updated2, expires2)) =
        Project_fixture.draft_row conn (Store.draft_id d1)
      in
      Alcotest.(check string) "still active" "active" status;
      Alcotest.(check (float 0.))
        "backdated created_at preserved" created1 created2;
      Alcotest.(check bool)
        "expiry renewed into the future" true (expires2 > expires1);
      Alcotest.(check (float 0.5))
        "renewed expiry is 24h from the refresh" 86400.0 (expires2 -. updated2);
      let* stored_sigs = Project_fixture.sigs conn (Store.draft_id d1) in
      Alcotest.(check (list string))
        "snapshot replaced"
        [
          Project_fixture.sig_of ~position:1 ~id:938600232L
            ~account_id:938100023L ~login:"podstore-owner" "beta";
        ]
        stored_sigs;
      Lwt.return_unit)

(* === terminal drafts === *)

let terminal_case =
  db_case "refresh: terminal drafts stay untouched, a new active is made"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000024L
          ~account_id:938100024L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000024L
          ~account_id:938100024L ~login:"podstore-owner" ~target:"User" ()
      in
      let set_for ~id ~name =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100024L
              ~owner_login:"podstore-owner" ~id ~name ();
          ]
      in
      (* One round per terminal state: seal the current draft, refresh,
         and pin that only a new active draft appeared. *)
      let terminal_round label seal previous =
        let* r = C.exec seal (Store.draft_id previous) in
        let* () = Db_fixture.or_fail (label ^ ": seal") r in
        let* before =
          Project_fixture.draft_row conn (Store.draft_id previous)
        in
        let* lifecycle_before =
          C.find q_lifecycle_epochs (Store.draft_id previous)
        in
        let* lifecycle_before =
          Db_fixture.or_fail "lifecycle" lifecycle_before
        in
        let* sigs_before =
          Project_fixture.sigs conn (Store.draft_id previous)
        in
        let* set =
          set_for
            ~id:(Int64.add 938600240L (Store.draft_id previous))
            ~name:(label ^ "-repo")
        in
        let* next =
          Project_fixture.refresh_ok (label ^ ": refresh") conn ~user:uid v set
        in
        Alcotest.(check bool)
          (label ^ ": fresh draft id")
          false
          (Int64.equal (Store.draft_id previous) (Store.draft_id next));
        let* after = Project_fixture.draft_row conn (Store.draft_id previous) in
        Project_fixture.check_same_draft_row (label ^ ": terminal row") before
          after;
        let* lifecycle_after =
          C.find q_lifecycle_epochs (Store.draft_id previous)
        in
        let* lifecycle_after = Db_fixture.or_fail "lifecycle" lifecycle_after in
        let lb_c, lb_x = lifecycle_before in
        let la_c, la_x = lifecycle_after in
        Alcotest.(check (option (float 0.)))
          (label ^ ": completed_at preserved")
          lb_c la_c;
        Alcotest.(check (option (float 0.)))
          (label ^ ": cancelled_at preserved")
          lb_x la_x;
        let* sigs_after = Project_fixture.sigs conn (Store.draft_id previous) in
        Alcotest.(check (list string))
          (label ^ ": old snapshot kept")
          sigs_before sigs_after;
        let* new_sigs = Project_fixture.sigs conn (Store.draft_id next) in
        Alcotest.(check int)
          (label ^ ": new draft has its own snapshot")
          1 (List.length new_sigs);
        let* active = count_active_for_user conn uid in
        Alcotest.(check int) (label ^ ": exactly one active draft") 1 active;
        Lwt.return next
      in
      let* set1 = set_for ~id:938600241L ~name:"first" in
      let* d1 = Project_fixture.refresh_ok "create" conn ~user:uid v set1 in
      let* d2 =
        terminal_round "completed" Project_fixture.q_complete_draft d1
      in
      let* _ = terminal_round "cancelled" Project_fixture.q_cancel_draft d2 in
      let* total = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "two terminal drafts plus one active" 3 total;
      Lwt.return_unit)

(* === ownership combinations === *)

let ownership_case =
  db_case "refresh: drafts key on caller and installation independently"
    (fun conn ->
      let* a = Db_fixture.insert_user conn "podstore_a" in
      let* b = Db_fixture.insert_user conn "podstore_b" in
      (* Provenance deliberately points at B for both installations. *)
      let* i1 =
        Project_fixture.insert_installation ~connected_by:b conn
          ~ext_id:938000031L ~account_id:938100031L
      in
      let* i2 =
        Project_fixture.insert_installation ~connected_by:b conn
          ~ext_id:938000032L ~account_id:938100032L
      in
      let* v1 =
        Project_fixture.verified ~installation_id:938000031L
          ~account_id:938100031L ~login:"podstore-owner" ~target:"User" ()
      in
      let* v2 =
        Project_fixture.verified ~installation_id:938000032L
          ~account_id:938100032L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set1 =
        Project_fixture.repo_set ~installation:v1
          [
            Github_fixture.gur_repo ~owner_id:938100031L
              ~owner_login:"podstore-owner" ~id:938600311L ~name:"alpha" ();
          ]
      in
      let* set2 =
        Project_fixture.repo_set ~installation:v2
          [
            Github_fixture.gur_repo ~owner_id:938100032L
              ~owner_login:"podstore-owner" ~id:938600321L ~name:"beta" ();
          ]
      in
      let* da1 = Project_fixture.refresh_ok "A on I1" conn ~user:a v1 set1 in
      let* db1 = Project_fixture.refresh_ok "B on I1" conn ~user:b v1 set1 in
      let* da2 = Project_fixture.refresh_ok "A on I2" conn ~user:a v2 set2 in
      Alcotest.(check bool)
        "same installation, two users, two drafts" false
        (Int64.equal (Store.draft_id da1) (Store.draft_id db1));
      Alcotest.(check bool)
        "same user, two installations, two drafts" false
        (Int64.equal (Store.draft_id da1) (Store.draft_id da2));
      let* sa1 =
        Project_fixture.active_draft_id conn ~user:a ~installation:i1
      in
      let* sb1 =
        Project_fixture.active_draft_id conn ~user:b ~installation:i1
      in
      let* sa2 =
        Project_fixture.active_draft_id conn ~user:a ~installation:i2
      in
      Alcotest.(check (option int64))
        "A's I1 slot"
        (Some (Store.draft_id da1))
        sa1;
      Alcotest.(check (option int64))
        "B's I1 slot"
        (Some (Store.draft_id db1))
        sb1;
      Alcotest.(check (option int64))
        "A's I2 slot"
        (Some (Store.draft_id da2))
        sa2;
      let* na = count_active_for_user conn a in
      let* nb = count_active_for_user conn b in
      Alcotest.(check int) "A holds two active drafts" 2 na;
      (* B connected both installations but called refresh once: exactly
         one draft — ownership never follows connected_by_user_id. *)
      Alcotest.(check int) "B holds one active draft" 1 nb;
      Lwt.return_unit)

let concurrent_identical_case =
  db_case "refresh: concurrent identical refreshes converge on one draft"
    (fun conn ->
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* inst =
        Project_fixture.insert_installation conn ~ext_id:938000041L
          ~account_id:938100041L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000041L
          ~account_id:938100041L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100041L
              ~owner_login:"podstore-owner" ~id:938600411L ~name:"alpha" ();
            Github_fixture.gur_repo ~owner_id:938100041L
              ~owner_login:"podstore-owner" ~id:938600412L ~name:"beta" ();
          ]
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Project_fixture.refresh conn ~user:uid v set)
              (Project_fixture.refresh conn2 ~user:uid v set)
          in
          let id_of label = function
            | Ok draft -> Store.draft_id draft
            | Error e ->
                Alcotest.failf "%s: %s" label
                  (Project_fixture.draft_error_str e)
          in
          let id1 = id_of "first refresher" r1 in
          let id2 = id_of "second refresher" r2 in
          Alcotest.(check int64) "both name the same draft" id1 id2;
          let* n = count_active_for_user conn uid in
          Alcotest.(check int) "exactly one active draft" 1 n;
          let* stored =
            Project_fixture.active_draft_id conn ~user:uid ~installation:inst
          in
          Alcotest.(check (option int64))
            "it is the returned draft" (Some id1) stored;
          let* stored_sigs = Project_fixture.sigs conn id1 in
          Alcotest.(check (list string))
            "one complete valid snapshot"
            [
              Project_fixture.sig_of ~position:1 ~id:938600411L
                ~account_id:938100041L ~login:"podstore-owner" "alpha";
              Project_fixture.sig_of ~position:2 ~id:938600412L
                ~account_id:938100041L ~login:"podstore-owner" "beta";
            ]
            stored_sigs;
          Lwt.return_unit))

let concurrent_competing_case =
  db_case "refresh: competing snapshots leave one complete winner" (fun conn ->
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000042L
          ~account_id:938100042L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000042L
          ~account_id:938100042L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set1 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100042L
              ~owner_login:"podstore-owner" ~id:938600421L ~name:"alpha" ();
            Github_fixture.gur_repo ~owner_id:938100042L
              ~owner_login:"podstore-owner" ~id:938600422L ~name:"beta" ();
          ]
      in
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100042L
              ~owner_login:"podstore-owner" ~id:938600423L ~name:"gamma" ();
          ]
      in
      let sigs1 =
        [
          Project_fixture.sig_of ~position:1 ~id:938600421L
            ~account_id:938100042L ~login:"podstore-owner" "alpha";
          Project_fixture.sig_of ~position:2 ~id:938600422L
            ~account_id:938100042L ~login:"podstore-owner" "beta";
        ]
      in
      let sigs2 =
        [
          Project_fixture.sig_of ~position:1 ~id:938600423L
            ~account_id:938100042L ~login:"podstore-owner" "gamma";
        ]
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Project_fixture.refresh conn ~user:uid v set1)
              (Project_fixture.refresh conn2 ~user:uid v set2)
          in
          let id_of label = function
            | Ok draft -> Store.draft_id draft
            | Error e ->
                Alcotest.failf "%s: %s" label
                  (Project_fixture.draft_error_str e)
          in
          let id1 = id_of "set1 refresher" r1 in
          let id2 = id_of "set2 refresher" r2 in
          Alcotest.(check int64) "both identify the same draft" id1 id2;
          let* n = count_active_for_user conn uid in
          Alcotest.(check int) "exactly one active draft" 1 n;
          let* stored_sigs = Project_fixture.sigs conn id1 in
          (* Which transaction wins is not asserted; the surviving
             snapshot must be one COMPLETE input set — never a mixture,
             duplicates, or stale leftovers (the exact-signature-list
             comparison rules all three out). *)
          let is_set1 = stored_sigs = sigs1 in
          let is_set2 = stored_sigs = sigs2 in
          Alcotest.(check bool)
            "snapshot equals one complete input set" true (is_set1 || is_set2);
          Lwt.return_unit))

(* === failure rollback === *)

let rollback_case =
  db_case "refresh: a failed snapshot insert rolls back everything" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000051L
          ~account_id:938100051L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000051L
          ~account_id:938100051L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set1 =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100051L
              ~owner_login:"podstore-owner" ~id:938600511L ~name:"alpha" ();
            Github_fixture.gur_repo ~owner_id:938100051L
              ~owner_login:"podstore-owner" ~id:938600512L ~name:"beta" ();
          ]
      in
      let* d1 = Project_fixture.refresh_ok "create" conn ~user:uid v set1 in
      let* before = Project_fixture.draft_row conn (Store.draft_id d1) in
      let* sigs_before = Project_fixture.sigs conn (Store.draft_id d1) in
      (* Test-only trigger, scoped to the reserved poison repository id;
         dropped in the finalizer even when an assertion fails. *)
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = Db_fixture.or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          let* set2 =
            Project_fixture.repo_set ~installation:v
              [
                Github_fixture.gur_repo ~owner_id:938100051L
                  ~owner_login:"podstore-owner" ~id:938600513L ~name:"gamma" ();
                Github_fixture.gur_repo ~owner_id:938100051L
                  ~owner_login:"podstore-owner" ~id:podstore_poison_repo_id
                  ~name:"poison" ();
              ]
          in
          let* () =
            refresh_expect "poisoned refresh" Store.Storage_error conn ~user:uid
              v set2
          in
          let* after = Project_fixture.draft_row conn (Store.draft_id d1) in
          Project_fixture.check_same_draft_row "draft after rollback" before
            after;
          let* sigs_after = Project_fixture.sigs conn (Store.draft_id d1) in
          Alcotest.(check (list string))
            "previous complete snapshot intact, no partial new rows" sigs_before
            sigs_after;
          let* n = Project_fixture.count_for_user conn uid in
          Alcotest.(check int) "still exactly one draft" 1 n;
          Lwt.return_unit)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === user foreign-key absence === *)

let ghost_user_case =
  db_case "refresh: nonexistent positive user id is Storage_error, no draft"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* ghost = C.find q_absent_user_id () in
      let* ghost = Db_fixture.or_fail "absent user id" ghost in
      let* inst =
        Project_fixture.insert_installation conn ~ext_id:938000052L
          ~account_id:938100052L
      in
      let* v =
        Project_fixture.verified ~installation_id:938000052L
          ~account_id:938100052L ~login:"podstore-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100052L
              ~owner_login:"podstore-owner" ~id:938600521L ~name:"alpha" ();
          ]
      in
      let* () =
        refresh_expect "ghost user" Store.Storage_error conn ~user:ghost v set
      in
      let* n = Project_fixture.count_for_user conn ghost in
      Alcotest.(check int) "no draft row" 0 n;
      let* stored =
        Project_fixture.active_draft_id conn ~user:ghost ~installation:inst
      in
      Alcotest.(check (option int64)) "slot empty" None stored;
      Lwt.return_unit)

(* === credential prohibition === *)

let credential_case =
  db_case "refresh: no credential material reaches stored values" (fun conn ->
      let* uid = Db_fixture.insert_user conn "podstore_a" in
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:938000053L
          ~account_id:938100053L
      in
      (* Every fixture in the chain rides the refresh-token exchange, so
         an access token, refresh token, code, verifier, and secret all
         exist to leak — and must not. *)
      let* v =
        Project_fixture.verified ~token_body:Project_fixture.refresh_token_body
          ~installation_id:938000053L ~account_id:938100053L
          ~login:"podstore-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~token_body:Project_fixture.refresh_token_body
          ~installation:v
          [
            Github_fixture.gur_repo ~owner_id:938100053L
              ~owner_login:"podstore-owner" ~id:938600531L ~name:"alpha"
              ~description:{|"benign description"|} ();
          ]
      in
      let* draft = Project_fixture.refresh_ok "refresh" conn ~user:uid v set in
      let* (_, _, status), _ =
        Project_fixture.draft_row conn (Store.draft_id draft)
      in
      let* stored_sigs = Project_fixture.sigs conn (Store.draft_id draft) in
      (* Every text value either table stores for this draft. *)
      let blob = String.concat "|" (status :: stored_sigs) in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool)
            (label ^ " absent from stored values")
            false
            (Html_assert.contains_nonempty ~needle blob))
        [
          ("access token", Project_fixture.pods_access_fixture);
          ("refresh token", Project_fixture.pods_refresh_fixture);
          ("authorization code", Github_fixture.gte_code_string);
          ("PKCE verifier", Github_fixture.gte_verifier_string);
          ("client secret", Github_fixture.gte_client_secret);
        ];
      Lwt.return_unit)

let suite =
  [
    invalid_user_case;
    missing_installation_case;
    inaccessible_installation_case;
    revoked_installation_case;
    account_id_mismatch_case;
    account_type_mismatch_case;
    login_difference_case;
    provenance_case;
    fresh_create_case;
    active_refresh_case;
    expired_refresh_case;
    terminal_case;
    ownership_case;
    concurrent_identical_case;
    concurrent_competing_case;
    rollback_case;
    ghost_user_case;
    credential_case;
  ]

let suites =
  (* Draft store: the transactional create-or-refresh plus complete
       snapshot replacement lives in Postgres — installation
       authorization, index-arbitrated slot, rollback atomicity,
       concurrency; same EARDE_TEST_DATABASE_URL gate (each case skips
       without it). *)
  [ ("project_onboarding_draft_store", suite) ]
