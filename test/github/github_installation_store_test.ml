module GTE = Earde.Github_oauth_token_exchange
module GUI = Earde.Github_user_installations

(* === GitHub installation persistence (Github_installation_store) ===
   The whole contract lives in one atomic upsert — insert-or-reactivate,
   identity pinned to the GitHub account id, revoked rows terminal — so only
   a DB-backed check can pin it down. Same EARDE_TEST_DATABASE_URL opt-in
   gate as Mod_scope; fixtures use a reserved installation-id range and
   fixed ghinstall_* usernames, removed before and after each case
   (autocommit on purpose: the concurrency cases need rows visible across
   two connections, and the FK-failure INSERT is atomic on its own).
   Verified-installation fixtures go through the real
   Github_user_installations.verify against scripted transports — no
   test-only constructor exists — and credential-fixture assertions are
   boolean, so no token bytes reach test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Store = Earde.Github_installation_store

let error_str : Store.error -> string = function
  | Store.Invalid_connected_by_user_id -> "Invalid_connected_by_user_id"
  | Store.Installation_unavailable -> "Installation_unavailable"
  | Store.Storage_error -> "Storage_error"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let gis_access_fixture = "gis-access.TOKEN~1"

let gis_refresh_fixture = "gis-refresh.TOKEN~2"

let default_token_body =
  {|{"access_token":"gis-access.TOKEN~1","token_type":"bearer","scope":""}|}

(* An expiring configuration, so the credential-absence case also holds a
   refresh token that must never reach the table. *)
let refresh_token_body =
  {|{"access_token":"gis-access.TOKEN~1","token_type":"bearer","scope":"","expires_in":28800,"refresh_token":"gis-refresh.TOKEN~2","refresh_token_expires_in":15811200}|}

(* Lwt-native token fixture: gui_token_set wraps its own Lwt_main.run and
   cannot run inside a db case's Lwt context. *)
let token_set_lwt body =
  let captured = ref None in
  let* outcome =
    GTE.exchange
      ~transport:(Github_fixture.gte_transport (Ok (200, body)) captured)
      ~config:(Github_fixture.gte_config ())
      ~credentials:(Github_fixture.gte_credentials ())
      ~code:(Github_fixture.gte_code_exn Github_fixture.gte_code_string)
      ~verifier:(Github_fixture.gte_verifier ())
  in
  match outcome with
  | Ok tokens -> Lwt.return tokens
  | Error e -> Alcotest.failf "token fixture: %s" (Github_fixture.gte_show_error e)

(* Abstract verified-installation fixture through the real verify against
   a one-page scripted listing. [target] is GitHub's installation-level
   target_type spelling: "User" or "Organization". *)
let verified ?(token_body = default_token_body) ~installation_id
    ~account_id ~login ~target () =
  let* token_set = token_set_lwt token_body in
  let body =
    Printf.sprintf
      {|{"total_count":1,"installations":[{"id":%Ld,"account":{"id":%Ld,"login":"%s"},"target_type":"%s"}]}|}
      installation_id account_id login target
  in
  let captured = ref [] in
  let* outcome =
    GUI.verify
      ~transport:(Github_fixture.gui_transport [ Ok (200, body) ] captured)
      ~token_set ~installation_id
  in
  match outcome with
  | Ok v -> Lwt.return v
  | Error e ->
      Alcotest.failf "verified fixture: %s" (Github_fixture.gui_show_error e)

(* Fixtures — a reserved installation-id range and fixed usernames so
   cleanup is targeted and idempotent. github_installations rows survive
   user deletion (ON DELETE SET NULL), so they need their own cleanup. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 935000001 AND 935000999"
    ; "DELETE FROM users WHERE username IN ('ghinstall_a', 'ghinstall_b')"
    ]

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

(* A positive user id guaranteed absent from users, for the FK-failure
   case. *)
let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

(* Everything persisted for one installation: identity, provenance,
   lifecycle, and the timestamp epochs — enough to pin both the inserted
   values and that an untouched row stayed byte-for-byte identical. *)
let q_row =
  (Caqti_type.(int64 ->! t2 (t4 int64 string string (option int))
                           (t2 (t2 string bool) (t2 float float))))
  "SELECT github_account_id, github_account_login, github_account_type,
          connected_by_user_id, status, revoked_at IS NULL,
          EXTRACT(EPOCH FROM created_at)::float8,
          EXTRACT(EPOCH FROM updated_at)::float8
   FROM github_installations WHERE github_installation_id = $1"

let q_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM github_installations
   WHERE github_installation_id = $1"

let q_fixture_count =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COUNT(*) FROM github_installations
   WHERE github_installation_id BETWEEN 935000001 AND 935000999"

let q_mark_inaccessible =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE github_installations SET status = 'inaccessible'
   WHERE github_installation_id = $1"

let q_mark_revoked =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE github_installations
   SET status = 'revoked', revoked_at = NOW()
   WHERE github_installation_id = $1"

let q_revoked_at_epoch =
  (Caqti_type.(int64 ->! option float))
  "SELECT EXTRACT(EPOCH FROM revoked_at)::float8
   FROM github_installations WHERE github_installation_id = $1"

(* All schema-facing values of one row rendered to a single text blob for
   the structural credential-absence check. *)
let q_row_text =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT github_installation_id::text || '|' ||
          github_account_id::text || '|' ||
          github_account_login || '|' ||
          github_account_type || '|' ||
          status || '|' ||
          COALESCE(connected_by_user_id::text, '')
   FROM github_installations WHERE github_installation_id = $1"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. *)
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

let insert_user conn username =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_user username in
  or_fail "user" uid

let record conn ~user v =
  Store.record_verified conn ~connected_by_user_id:user v

let record_ok label conn ~user v =
  let* r = record conn ~user v in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let record_unavailable label conn ~user v =
  let* r = record conn ~user v in
  match r with
  | Error Store.Installation_unavailable -> Lwt.return_unit
  | Error e ->
      Alcotest.failf "%s: expected Installation_unavailable, got %s" label
        (error_str e)
  | Ok () ->
      Alcotest.failf "%s: expected Installation_unavailable, got Ok" label

let row conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_row id in
  or_fail "row" r

let count conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_count id in
  or_fail "count" r

(* Componentwise identity: pins that an operation left every persisted
   field of the row exactly as captured. *)
let check_same_row label
    ( (acct_b, login_b, type_b, connected_b),
      ((status_b, revoked_null_b), (created_b, updated_b)) )
    ( (acct_a, login_a, type_a, connected_a),
      ((status_a, revoked_null_a), (created_a, updated_a)) ) =
  Alcotest.(check int64) (label ^ ": account id") acct_b acct_a;
  Alcotest.(check string) (label ^ ": login") login_b login_a;
  Alcotest.(check string) (label ^ ": account type") type_b type_a;
  Alcotest.(check (option int)) (label ^ ": connected user") connected_b
    connected_a;
  Alcotest.(check string) (label ^ ": status") status_b status_a;
  Alcotest.(check bool) (label ^ ": revoked_at NULL-ness") revoked_null_b
    revoked_null_a;
  Alcotest.(check (float 0.)) (label ^ ": created_at") created_b created_a;
  Alcotest.(check (float 0.)) (label ^ ": updated_at") updated_b updated_a

let fresh_insert_case =
  db_case "record: fresh insert persists both account types" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "ghinstall_a" in
      let check ~installation_id ~account_id ~login ~target
          ~expected_type =
        let* v = verified ~installation_id ~account_id ~login ~target () in
        let* () = record_ok "record" conn ~user:uid v in
        let* n = count conn installation_id in
        Alcotest.(check int) "one row for the installation id" 1 n;
        let* ( (acct, stored_login, stored_type, connected),
               ((status, revoked_null), (created, updated)) ) =
          row conn installation_id
        in
        Alcotest.(check int64) "account id" account_id acct;
        Alcotest.(check string) "login byte-for-byte" login stored_login;
        Alcotest.(check string) "canonical lowercase account type"
          expected_type stored_type;
        Alcotest.(check (option int)) "connected user is the caller"
          (Some uid) connected;
        Alcotest.(check string) "status" "active" status;
        Alcotest.(check bool) "revoked_at IS NULL" true revoked_null;
        Alcotest.(check bool) "created_at exists" true (created > 0.);
        Alcotest.(check bool) "updated_at exists" true (updated > 0.);
        Lwt.return_unit
      in
      let* () =
        check ~installation_id:935000001L ~account_id:935100001L
          ~login:"Café-Owner_1" ~target:"User" ~expected_type:"user"
      in
      let* () =
        check ~installation_id:935000002L ~account_id:935100002L
          ~login:"earde-org" ~target:"Organization"
          ~expected_type:"organization"
      in
      let* total = C.find q_fixture_count () in
      let* total = or_fail "fixture count" total in
      Alcotest.(check int) "no additional row" 2 total;
      Lwt.return_unit)

let invalid_user_case =
  db_case "record: non-positive user ids rejected before SQL" (fun conn ->
      let* v =
        verified ~installation_id:935000011L ~account_id:935100011L
          ~login:"invalid-caller" ~target:"User" ()
      in
      let check_rejected uid =
        let* r = record conn ~user:uid v in
        match r with
        | Error Store.Invalid_connected_by_user_id -> Lwt.return_unit
        | Error e ->
            Alcotest.failf
              "user id %d: expected Invalid_connected_by_user_id, got %s"
              uid (error_str e)
        | Ok () ->
            Alcotest.failf
              "user id %d: expected Invalid_connected_by_user_id, got Ok"
              uid
      in
      let* () = check_rejected 0 in
      let* () = check_rejected (-1) in
      let* n = count conn 935000011L in
      Alcotest.(check int) "no row inserted" 0 n;
      Lwt.return_unit)

let missing_user_case =
  db_case "record: nonexistent user id is Storage_error, no row"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* ghost = C.find q_absent_user_id () in
      let* ghost = or_fail "absent user id" ghost in
      let* v =
        verified ~installation_id:935000012L ~account_id:935100012L
          ~login:"ghost-caller" ~target:"User" ()
      in
      let* r = record conn ~user:ghost v in
      (match r with
      | Error Store.Storage_error -> ()
      | Error e ->
          Alcotest.failf "expected Storage_error, got %s" (error_str e)
      | Ok () -> Alcotest.fail "expected Storage_error, got Ok");
      let* n = count conn 935000012L in
      Alcotest.(check int) "no row inserted" 0 n;
      Lwt.return_unit)

let idempotent_case =
  db_case "record: identical retry is idempotent" (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v =
        verified ~installation_id:935000021L ~account_id:935100021L
          ~login:"retry-owner" ~target:"Organization" ()
      in
      let* () = record_ok "first" conn ~user:uid v in
      let* first = row conn 935000021L in
      let* () = record_ok "retry" conn ~user:uid v in
      let* n = count conn 935000021L in
      Alcotest.(check int) "exactly one row" 1 n;
      let* ( (acct, login, account_type, connected),
             ((status, _), (created, _)) ) =
        row conn 935000021L
      in
      let (acct_b, login_b, type_b, connected_b), (_, (created_b, _)) =
        first
      in
      Alcotest.(check string) "status remains active" "active" status;
      Alcotest.(check (option int)) "ownership remains the original user"
        connected_b connected;
      Alcotest.(check (option int)) "owner is the fixture user" (Some uid)
        connected;
      Alcotest.(check int64) "account id unchanged" acct_b acct;
      Alcotest.(check string) "login unchanged" login_b login;
      Alcotest.(check string) "account type unchanged" type_b account_type;
      Alcotest.(check (float 0.)) "created_at unchanged" created_b created;
      Lwt.return_unit)

let login_refresh_case =
  db_case "record: reverification refreshes the login" (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000022L ~account_id:935100022L
          ~login:"Old-Login" ~target:"User" ()
      in
      let* () = record_ok "first" conn ~user:uid v1 in
      (* Same installation, account, and type — the account id, not the
         mutable login, is the identity. *)
      let* v2 =
        verified ~installation_id:935000022L ~account_id:935100022L
          ~login:"new-login" ~target:"User" ()
      in
      let* () = record_ok "refresh" conn ~user:uid v2 in
      let* n = count conn 935000022L in
      Alcotest.(check int) "no second row" 1 n;
      let* (acct, login, account_type, connected), _ =
        row conn 935000022L
      in
      Alcotest.(check string) "login updated exactly" "new-login" login;
      Alcotest.(check int64) "account id unchanged" 935100022L acct;
      Alcotest.(check string) "account type unchanged" "user" account_type;
      Alcotest.(check (option int)) "connected user unchanged" (Some uid)
        connected;
      Lwt.return_unit)

let reconnecting_user_case =
  db_case "record: a different reconnecting user never takes ownership"
    (fun conn ->
      let* uid_a = insert_user conn "ghinstall_a" in
      let* uid_b = insert_user conn "ghinstall_b" in
      let* v =
        verified ~installation_id:935000023L ~account_id:935100023L
          ~login:"shared-owner" ~target:"Organization" ()
      in
      let* () = record_ok "user A" conn ~user:uid_a v in
      let* () = record_ok "user B" conn ~user:uid_b v in
      let* n = count conn 935000023L in
      Alcotest.(check int) "no second row" 1 n;
      let* (_, _, _, connected), _ = row conn 935000023L in
      Alcotest.(check (option int)) "provenance stays with user A"
        (Some uid_a) connected;
      Lwt.return_unit)

let inaccessible_reactivation_case =
  db_case "record: inaccessible rows reactivate in place" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000024L ~account_id:935100024L
          ~login:"suspended-owner" ~target:"User" ()
      in
      let* () = record_ok "first" conn ~user:uid v1 in
      let* r = C.exec q_mark_inaccessible 935000024L in
      let* () = or_fail "mark inaccessible" r in
      let* before = row conn 935000024L in
      let* v2 =
        verified ~installation_id:935000024L ~account_id:935100024L
          ~login:"recovered-owner" ~target:"User" ()
      in
      let* () = record_ok "reactivate" conn ~user:uid v2 in
      let* n = count conn 935000024L in
      Alcotest.(check int) "no second row" 1 n;
      let* ( (acct, login, account_type, connected),
             ((status, revoked_null), (created, _)) ) =
        row conn 935000024L
      in
      let (acct_b, _, type_b, connected_b), (_, (created_b, _)) = before in
      Alcotest.(check string) "status becomes active" "active" status;
      Alcotest.(check string) "login refreshes" "recovered-owner" login;
      Alcotest.(check bool) "revoked_at stays NULL" true revoked_null;
      Alcotest.(check int64) "account id unchanged" acct_b acct;
      Alcotest.(check string) "account type unchanged" type_b account_type;
      Alcotest.(check (option int)) "connected user unchanged" connected_b
        connected;
      Alcotest.(check (float 0.)) "created_at unchanged" created_b created;
      Lwt.return_unit)

let revoked_terminal_case =
  db_case "record: revoked rows are terminal and untouched" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000025L ~account_id:935100025L
          ~login:"revoked-owner" ~target:"Organization" ()
      in
      let* () = record_ok "first" conn ~user:uid v1 in
      let* r = C.exec q_mark_revoked 935000025L in
      let* () = or_fail "mark revoked" r in
      let* before = row conn 935000025L in
      let* revoked_at_before = C.find q_revoked_at_epoch 935000025L in
      let* revoked_at_before = or_fail "revoked_at" revoked_at_before in
      Alcotest.(check bool) "fixture has revoked_at" true
        (revoked_at_before <> None);
      let* v2 =
        verified ~installation_id:935000025L ~account_id:935100025L
          ~login:"reinstalled-owner" ~target:"Organization" ()
      in
      let* () = record_unavailable "revoked" conn ~user:uid v2 in
      let* after = row conn 935000025L in
      check_same_row "revoked row" before after;
      let* revoked_at_after = C.find q_revoked_at_epoch 935000025L in
      let* revoked_at_after = or_fail "revoked_at after" revoked_at_after in
      Alcotest.(check (option (float 0.))) "revoked_at unchanged"
        revoked_at_before revoked_at_after;
      Lwt.return_unit)

let conflicting_account_id_case =
  db_case "record: same installation, different account id is rejected"
    (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000026L ~account_id:935100026L
          ~login:"true-owner" ~target:"User" ()
      in
      let* () = record_ok "first" conn ~user:uid v1 in
      let* before = row conn 935000026L in
      let* v2 =
        verified ~installation_id:935000026L ~account_id:935100027L
          ~login:"impostor" ~target:"User" ()
      in
      let* () = record_unavailable "account id conflict" conn ~user:uid v2
      in
      let* n = count conn 935000026L in
      Alcotest.(check int) "still one row" 1 n;
      let* after = row conn 935000026L in
      check_same_row "conflicted row" before after;
      Lwt.return_unit)

let conflicting_account_type_case =
  db_case "record: same installation, different account type is rejected"
    (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000028L ~account_id:935100028L
          ~login:"typed-owner" ~target:"User" ()
      in
      let* () = record_ok "first" conn ~user:uid v1 in
      let* before = row conn 935000028L in
      let* v2 =
        verified ~installation_id:935000028L ~account_id:935100028L
          ~login:"typed-owner" ~target:"Organization" ()
      in
      let* () =
        record_unavailable "account type conflict" conn ~user:uid v2
      in
      let* n = count conn 935000028L in
      Alcotest.(check int) "still one row" 1 n;
      let* after = row conn 935000028L in
      check_same_row "conflicted row" before after;
      Lwt.return_unit)

let same_account_two_installations_case =
  db_case "record: one account may hold several installations" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000029L ~account_id:935100029L
          ~login:"multi-owner" ~target:"Organization" ()
      in
      let* v2 =
        verified ~installation_id:935000030L ~account_id:935100029L
          ~login:"multi-owner" ~target:"Organization" ()
      in
      let* () = record_ok "first installation" conn ~user:uid v1 in
      let* () = record_ok "second installation" conn ~user:uid v2 in
      let* n1 = count conn 935000029L in
      let* n2 = count conn 935000030L in
      Alcotest.(check int) "first row accepted" 1 n1;
      Alcotest.(check int) "second row accepted" 1 n2;
      let* total = C.find q_fixture_count () in
      let* total = or_fail "fixture count" total in
      Alcotest.(check int) "exactly the two rows" 2 total;
      Lwt.return_unit)

(* Second connection for the concurrency races; db_case only runs under
   the gate, so the URL is present. *)
let with_second_connection f =
  let url =
    match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
    | Some url -> url
    | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
  in
  let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
  let* conn2 = or_fail "second connect" conn2 in
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  Lwt.finalize (fun () -> f conn2) (fun () -> C2.disconnect ())

let concurrent_identical_case =
  db_case "record: concurrent identical writes both succeed" (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v =
        verified ~installation_id:935000031L ~account_id:935100031L
          ~login:"race-owner" ~target:"User" ()
      in
      with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both (record conn ~user:uid v) (record conn2 ~user:uid v)
          in
          let check_ok label = function
            | Ok () -> ()
            | Error e ->
                Alcotest.failf "%s: unexpected %s" label (error_str e)
          in
          check_ok "first writer" r1;
          check_ok "second writer" r2;
          let* n = count conn 935000031L in
          Alcotest.(check int) "exactly one row" 1 n;
          let* (acct, login, account_type, _), ((status, _), _) =
            row conn 935000031L
          in
          Alcotest.(check string) "row is active" "active" status;
          Alcotest.(check int64) "stored account id" 935100031L acct;
          Alcotest.(check string) "stored login" "race-owner" login;
          Alcotest.(check string) "stored account type" "user"
            account_type;
          Lwt.return_unit))

let concurrent_conflicting_case =
  db_case "record: concurrent conflicting identities persist exactly one"
    (fun conn ->
      let* uid = insert_user conn "ghinstall_a" in
      let* v1 =
        verified ~installation_id:935000032L ~account_id:935100032L
          ~login:"conflict-a" ~target:"User" ()
      in
      let* v2 =
        verified ~installation_id:935000032L ~account_id:935100033L
          ~login:"conflict-b" ~target:"Organization" ()
      in
      with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both (record conn ~user:uid v1) (record conn2 ~user:uid v2)
          in
          (* Which caller wins the insert race is not deterministic; the
             contract is one winner, one rejection, one coherent row. *)
          let winner =
            match (r1, r2) with
            | Ok (), Error Store.Installation_unavailable -> `First
            | Error Store.Installation_unavailable, Ok () -> `Second
            | Ok (), Ok () -> Alcotest.fail "both writers won"
            | r1, r2 ->
                let render = function
                  | Ok () -> "Ok"
                  | Error e -> error_str e
                in
                Alcotest.failf "unexpected outcome pair: %s / %s"
                  (render r1) (render r2)
          in
          let* n = count conn 935000032L in
          Alcotest.(check int) "exactly one row" 1 n;
          let* (acct, login, account_type, _), _ = row conn 935000032L in
          let expected_acct, expected_login, expected_type =
            match winner with
            | `First -> (935100032L, "conflict-a", "user")
            | `Second -> (935100033L, "conflict-b", "organization")
          in
          (* The complete winning identity, never a mixture. *)
          Alcotest.(check int64) "winner's account id" expected_acct acct;
          Alcotest.(check string) "winner's login" expected_login login;
          Alcotest.(check string) "winner's account type" expected_type
            account_type;
          Lwt.return_unit))

let credential_absence_case =
  db_case "record: no credential material reaches stored values"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "ghinstall_a" in
      let* v =
        verified ~token_body:refresh_token_body
          ~installation_id:935000033L ~account_id:935100034L
          ~login:"credential-check" ~target:"User" ()
      in
      let* () = record_ok "record" conn ~user:uid v in
      let* stored = C.find q_row_text 935000033L in
      let* stored = or_fail "row text" stored in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool) (label ^ " absent from stored values")
            false
            (Html_assert.contains_nonempty ~needle stored))
        [ ("access token", gis_access_fixture)
        ; ("refresh token", gis_refresh_fixture)
        ; ("authorization code", Github_fixture.gte_code_string)
        ; ("PKCE verifier", Github_fixture.gte_verifier_string)
        ; ("client secret", Github_fixture.gte_client_secret)
        ];
      Lwt.return_unit)

let suite =
  [ fresh_insert_case; invalid_user_case; missing_user_case;
    idempotent_case; login_refresh_case; reconnecting_user_case;
    inaccessible_reactivation_case; revoked_terminal_case;
    conflicting_account_id_case; conflicting_account_type_case;
    same_account_two_installations_case; concurrent_identical_case;
    concurrent_conflicting_case; credential_absence_case ]

let suites =
    (* Verified-installation persistence: the whole contract lives in one
       atomic upsert, so only Postgres can pin it down; same
       EARDE_TEST_DATABASE_URL gate (each case skips without it). *)
  [ ( "github_installation_store", suite )
  ]
