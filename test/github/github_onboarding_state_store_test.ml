module GO = Earde.Github_onboarding
module GOC = Earde.Github_onboarding_crypto

(* === GitHub onboarding state issuance (Github_onboarding_state_store) ===
   The security contract lives in the SQL — only hashes reach the table,
   expiry comes from Postgres NOW(), issuance never disturbs earlier states —
   so only a DB-backed check can pin it down. Same EARDE_TEST_DATABASE_URL
   opt-in gate as Mod_scope; each fixture-writing case runs inside a
   transaction that is rolled back (the FK-failure case relies on the failed
   INSERT's own atomicity instead), so no rows outlive a run. Raw generated
   state/binding values never reach assertion messages or test output. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Store = Earde.Github_onboarding_state_store

(* attach_error shares constructor names with issue_error, so both
   stringifiers need explicit domains. *)
let issue_error_str : Store.issue_error -> string = function
  | Store.Invalid_user_id -> "Invalid_user_id"
  | Store.Storage_error -> "Storage_error"

let attach_error_str : Store.attach_error -> string = function
  | Store.Invalid_pending_installation_id -> "Invalid_pending_installation_id"
  | Store.State_unavailable -> "State_unavailable"
  | Store.Storage_error -> "Storage_error"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let q_insert_user =
  (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ('ghstate_user', 'ghstate_user@test.invalid', 'x', TRUE) \
     RETURNING id"

(* Everything stored for a user's states: the three text columns (no other
   column in the table can hold token material), NULL-ness of the two
   lifecycle columns, and the Postgres-computed expiry delta. *)
let q_rows_for_user =
  Caqti_type.(int ->* t2 (t3 string string string) (t3 bool bool float))
    "SELECT state_hash, session_binding_hash, flow,\n\
    \          pending_github_installation_id IS NULL,\n\
    \          consumed_at IS NULL,\n\
    \          EXTRACT(EPOCH FROM (expires_at - created_at))::float8\n\
    \   FROM github_onboarding_states WHERE user_id = $1 ORDER BY id"

let q_count_for_user =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM github_onboarding_states WHERE user_id = $1"

(* A positive user id guaranteed absent from users, for the FK-failure
   case. *)
let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             Lwt.finalize (fun () -> f conn) (fun () -> C.disconnect ())))

(* Fixture user and issued rows live only inside this transaction; the
   rollback runs even when an assertion fails mid-case. *)
let tx_case name f =
  db_case name (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* r = C.start () in
      let* () = or_fail "begin" r in
      Lwt.finalize
        (fun () -> f conn)
        (fun () ->
          let* r = C.rollback () in
          let* () = or_fail "rollback" r in
          Lwt.return_unit))

let issue_ok conn ~user_id ~session_binding_hash =
  let* r =
    Store.issue conn ~user_id ~session_binding_hash ~flow:GO.Project_onboarding
  in
  match r with
  | Ok state -> Lwt.return state
  | Error e -> Alcotest.failf "issue: %s" (issue_error_str e)

let single_issue_case =
  tx_case "issue: one row, hashes only, Postgres 15-minute expiry" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = C.find q_insert_user () in
      let* uid = or_fail "user" uid in
      let binding = GOC.generate_session_binding () in
      let binding_hash = GOC.hash_session_binding binding in
      let* state =
        issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
      in
      let raw_state = GOC.state_to_string state in
      (match GOC.state_of_callback raw_state with
      | Ok _ -> ()
      | Error GOC.Invalid_format ->
          Alcotest.fail "issued state does not pass state_of_callback");
      let* rows = C.collect_list q_rows_for_user uid in
      let* rows = or_fail "rows" rows in
      (match rows with
      | [
       ( (state_hash, stored_binding_hash, flow),
         (pending_null, consumed_null, ttl) );
      ] ->
          Alcotest.(check string)
            "stored state_hash is the derived hash"
            (GOC.state_hash_to_string (GOC.hash_state state))
            state_hash;
          Alcotest.(check string)
            "stored binding hash is the supplied hash"
            (GOC.session_binding_hash_to_string binding_hash)
            stored_binding_hash;
          Alcotest.(check string) "flow" "project_onboarding" flow;
          let text_columns =
            String.concat "|" [ state_hash; stored_binding_hash; flow ]
          in
          Alcotest.(check bool)
            "raw state absent from text columns" false
            (Html_assert.contains_nonempty ~needle:raw_state text_columns);
          Alcotest.(check bool)
            "raw binding absent from text columns" false
            (Html_assert.contains_nonempty
               ~needle:(GOC.session_binding_to_string binding)
               text_columns);
          Alcotest.(check bool) "pending installation id NULL" true pending_null;
          Alcotest.(check bool) "consumed_at NULL" true consumed_null;
          Alcotest.(check bool)
            "expiry ~15 minutes after creation" true
            (Float.abs (ttl -. 900.) <= 5.)
      | rows ->
          Alcotest.failf "expected exactly one row, found %d" (List.length rows));
      Lwt.return_unit)

let multiplicity_case =
  tx_case "issue: repeat issuance leaves independent unconsumed rows"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = C.find q_insert_user () in
      let* uid = or_fail "user" uid in
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* first =
        issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
      in
      let* second =
        issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
      in
      let hash_of s = GOC.state_hash_to_string (GOC.hash_state s) in
      let* rows = C.collect_list q_rows_for_user uid in
      let* rows = or_fail "rows" rows in
      (match rows with
      | [
       ((hash_a, _, _), (_, consumed_a, _)); ((hash_b, _, _), (_, consumed_b, _));
      ] ->
          Alcotest.(check bool) "first row unconsumed" true consumed_a;
          Alcotest.(check bool) "second row unconsumed" true consumed_b;
          Alcotest.(check bool)
            "distinct state hashes" false
            (String.equal hash_a hash_b);
          (* Rows are id-ordered, so they pair with issuance order. *)
          Alcotest.(check string)
            "first row is the first state" (hash_of first) hash_a;
          Alcotest.(check string)
            "second row is the second state" (hash_of second) hash_b
      | rows ->
          Alcotest.failf "expected exactly two rows, found %d"
            (List.length rows));
      Lwt.return_unit)

let invalid_user_case =
  db_case "issue: non-positive user ids rejected before SQL" (fun conn ->
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let check_rejected uid =
        let* r =
          Store.issue conn ~user_id:uid ~session_binding_hash:binding_hash
            ~flow:GO.Project_onboarding
        in
        match r with
        | Error Store.Invalid_user_id -> Lwt.return_unit
        | Error Store.Storage_error ->
            Alcotest.failf
              "user id %d: expected Invalid_user_id, got Storage_error" uid
        | Ok _ ->
            Alcotest.failf "user id %d: expected Invalid_user_id, got Ok" uid
      in
      let* () = check_rejected 0 in
      check_rejected (-1))

(* Autocommit on purpose: the failed INSERT is atomic on its own, which is
   exactly the "no partial row" contract; a wrapping transaction would be
   aborted by the FK error and block the follow-up count. *)
let missing_user_case =
  db_case "issue: nonexistent user id is Storage_error, no partial row"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* ghost = C.find q_absent_user_id () in
      let* ghost = or_fail "absent user id" ghost in
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* r =
        Store.issue conn ~user_id:ghost ~session_binding_hash:binding_hash
          ~flow:GO.Project_onboarding
      in
      (match r with
      | Error Store.Storage_error -> ()
      | Error Store.Invalid_user_id ->
          Alcotest.fail "expected Storage_error, got Invalid_user_id"
      | Ok _ -> Alcotest.fail "expected Storage_error, got Ok");
      let* count = C.find q_count_for_user ghost in
      let* count = or_fail "count" count in
      Alcotest.(check int) "no partial row" 0 count;
      Lwt.return_unit)

(* === attach_pending_installation ===
   Same gate and rollback discipline as issuance. Attachment takes no user
   id: the GitHub setup return is a cross-site redirect the SameSite=Strict
   login-session cookie need not accompany, so authorization is the state
   hash + the session-binding hash from the SameSite=Lax per-flow cookie +
   the flow; ownership stays pinned to the user_id written at issuance.
   Flow mismatch has no dedicated case: the model has a single flow variant
   and the schema CHECK forbids any other flow string, so a mismatched-flow
   row cannot exist even as a SQL fixture; the flow comparison sits in the
   same conjunctive WHERE as the binding column that is tested. *)

let q_insert_second_user =
  (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ('ghstate_user_b', 'ghstate_user_b@test.invalid', 'x', TRUE) \
     RETURNING id"

(* Full persisted row for attach assertions: the three text columns, the
   actual pending id, consumed_at NULL-ness, and the creation/expiry
   epochs — pinning that attachment changes nothing else. *)
let q_attach_rows_for_user =
  Caqti_type.(
    int
    ->* t2 (t3 string string string) (t3 (option int64) bool (t2 float float)))
    "SELECT state_hash, session_binding_hash, flow,\n\
    \          pending_github_installation_id,\n\
    \          consumed_at IS NULL,\n\
    \          EXTRACT(EPOCH FROM created_at)::float8,\n\
    \          EXTRACT(EPOCH FROM expires_at)::float8\n\
    \   FROM github_onboarding_states WHERE user_id = $1 ORDER BY id"

(* Fixture: push a user's states into the past. created_at moves too,
   both because expires_at > created_at is a CHECK and because NOW() is
   frozen for the whole rolled-back transaction — merely shrinking the
   TTL could never make a row expired inside the test. *)
let q_expire_states_for_user =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE github_onboarding_states\n\
    \   SET created_at = NOW() - INTERVAL '1 hour',\n\
    \       expires_at = NOW() - INTERVAL '30 minutes'\n\
    \   WHERE user_id = $1"

let q_consume_states_for_user =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE github_onboarding_states SET consumed_at = NOW()\n\
    \   WHERE user_id = $1"

let attach conn ~state ~binding_hash id =
  Store.attach_pending_installation conn ~state
    ~session_binding_hash:binding_hash ~flow:GO.Project_onboarding
    ~pending_github_installation_id:id

let attach_ok conn ~state ~binding_hash id =
  let* r = attach conn ~state ~binding_hash id in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "attach: %s" (attach_error_str e)

let attach_unavailable label conn ~state ~binding_hash id =
  let* r = attach conn ~state ~binding_hash id in
  match r with
  | Error Store.State_unavailable -> Lwt.return_unit
  | Error e ->
      Alcotest.failf "%s: expected State_unavailable, got %s" label
        (attach_error_str e)
  | Ok () -> Alcotest.failf "%s: expected State_unavailable, got Ok" label

(* Standard fixture: one user holding one freshly issued state. *)
let issued_fixture conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_user () in
  let* uid = or_fail "user" uid in
  let binding = GOC.generate_session_binding () in
  let binding_hash = GOC.hash_session_binding binding in
  let* state = issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash in
  Lwt.return (uid, state, binding, binding_hash)

let single_row conn uid =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rows = C.collect_list q_attach_rows_for_user uid in
  let* rows = or_fail "rows" rows in
  match rows with
  | [ row ] -> Lwt.return row
  | rows ->
      Alcotest.failf "expected exactly one row, found %d" (List.length rows)

let check_untouched_unconsumed label (_, (pending, consumed_null, _)) =
  Alcotest.(check (option int64)) (label ^ ": pending still NULL") None pending;
  Alcotest.(check bool) (label ^ ": still unconsumed") true consumed_null

let attach_validation_case =
  db_case "attach: invalid installation ids rejected before SQL" (fun conn ->
      let state = GOC.generate_state () in
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let check_rejected label id =
        let* r = attach conn ~state ~binding_hash id in
        match r with
        | Error Store.Invalid_pending_installation_id -> Lwt.return_unit
        | Error e ->
            Alcotest.failf
              "%s: expected Invalid_pending_installation_id, got %s" label
              (attach_error_str e)
        | Ok () ->
            Alcotest.failf
              "%s: expected Invalid_pending_installation_id, got Ok" label
      in
      let* () = check_rejected "installation id 0" 0L in
      check_rejected "negative installation id" (-42L))

let attach_success_case =
  tx_case "attach: first attachment sets only the pending id" (fun conn ->
      let* uid, state, binding, binding_hash = issued_fixture conn in
      let* before = single_row conn uid in
      let (state_hash0, binding_hash0, flow0), (pending0, _, times0) = before in
      Alcotest.(check (option int64)) "pending NULL before attach" None pending0;
      (* The call carries only the state, binding hash, flow, and
         installation id — no user id exists in the attach API at all. *)
      let* () = attach_ok conn ~state ~binding_hash 123456789L in
      (* single_row filters on user_id = uid: getting the row back at all
         proves the stored ownership still points at the issuing user. *)
      let* row = single_row conn uid in
      let ( (state_hash, stored_binding_hash, flow),
            (pending, consumed_null, (created, expires)) ) =
        row
      in
      Alcotest.(check (option int64)) "attached id" (Some 123456789L) pending;
      Alcotest.(check bool) "consumed_at still NULL" true consumed_null;
      Alcotest.(check string) "state_hash unchanged" state_hash0 state_hash;
      Alcotest.(check string)
        "binding hash unchanged" binding_hash0 stored_binding_hash;
      Alcotest.(check string) "flow unchanged" flow0 flow;
      let created0, expires0 = times0 in
      Alcotest.(check (float 0.001)) "created_at unchanged" created0 created;
      Alcotest.(check (float 0.001)) "expires_at unchanged" expires0 expires;
      (* Attachment sends only hashes over SQL; the text columns must
         still hold no raw token material. *)
      let text = String.concat "|" [ state_hash; stored_binding_hash; flow ] in
      Alcotest.(check bool)
        "raw state absent from text columns" false
        (Html_assert.contains_nonempty ~needle:(GOC.state_to_string state) text);
      Alcotest.(check bool)
        "raw binding absent from text columns" false
        (Html_assert.contains_nonempty
           ~needle:(GOC.session_binding_to_string binding)
           text);
      Lwt.return_unit)

let attach_idempotent_case =
  tx_case "attach: identical retry succeeds on the same single row" (fun conn ->
      let* uid, state, _, binding_hash = issued_fixture conn in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      (* single_row also proves the retry created no extra row. *)
      let* _, (pending, consumed_null, _) = single_row conn uid in
      Alcotest.(check (option int64)) "still the same id" (Some 42L) pending;
      Alcotest.(check bool) "still unconsumed" true consumed_null;
      Lwt.return_unit)

let attach_conflict_case =
  tx_case "attach: a different id never overwrites the first" (fun conn ->
      let* uid, state, _, binding_hash = issued_fixture conn in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let* () =
        attach_unavailable "conflicting id" conn ~state ~binding_hash 43L
      in
      let* _, (pending, consumed_null, _) = single_row conn uid in
      Alcotest.(check (option int64)) "first id retained" (Some 42L) pending;
      Alcotest.(check bool) "conflict did not consume" true consumed_null;
      Lwt.return_unit)

let attach_missing_state_case =
  tx_case "attach: unknown state is State_unavailable, creates no row"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = C.find q_insert_user () in
      let* uid = or_fail "user" uid in
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      (* Generated but never issued: no row anywhere carries its hash. *)
      let state = GOC.generate_state () in
      let* () =
        attach_unavailable "unknown state" conn ~state ~binding_hash 42L
      in
      let* count = C.find q_count_for_user uid in
      let* count = or_fail "count" count in
      Alcotest.(check int) "no row created" 0 count;
      Lwt.return_unit)

(* Ownership preservation: with no user id in the attach API, prove that
   possession-based attachment stays scoped to the one row the state hash
   names, and that each row's user_id — written at issuance — survives
   attachment untouched. single_row filters on user_id, so retrieving each
   row under its own user is itself the ownership assertion. *)
let attach_ownership_case =
  tx_case "attach: touches only its own state, ownership stays put" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid_a, state_a, _, binding_hash_a = issued_fixture conn in
      let* uid_b = C.find q_insert_second_user () in
      let* uid_b = or_fail "second user" uid_b in
      let binding_hash_b =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* _state_b =
        issue_ok conn ~user_id:uid_b ~session_binding_hash:binding_hash_b
      in
      let* () =
        attach_ok conn ~state:state_a ~binding_hash:binding_hash_a 42L
      in
      let* _, (pending_a, consumed_a, _) = single_row conn uid_a in
      Alcotest.(check (option int64)) "state A attached" (Some 42L) pending_a;
      Alcotest.(check bool) "state A unconsumed" true consumed_a;
      let* row_b = single_row conn uid_b in
      check_untouched_unconsumed "state B" row_b;
      Lwt.return_unit)

let attach_wrong_binding_case =
  tx_case "attach: session-binding mismatch does not attach" (fun conn ->
      let* uid, state, _, _ = issued_fixture conn in
      let other_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* () =
        attach_unavailable "wrong binding" conn ~state ~binding_hash:other_hash
          42L
      in
      let* row = single_row conn uid in
      check_untouched_unconsumed "wrong binding" row;
      Lwt.return_unit)

let attach_expired_case =
  tx_case "attach: expired state does not attach" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash = issued_fixture conn in
      let* r = C.exec q_expire_states_for_user uid in
      let* () = or_fail "expire fixture" r in
      let* () = attach_unavailable "expired" conn ~state ~binding_hash 42L in
      let* row = single_row conn uid in
      check_untouched_unconsumed "expired" row;
      Lwt.return_unit)

let attach_consumed_case =
  tx_case "attach: consumed state does not attach" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash = issued_fixture conn in
      let* r = C.exec q_consume_states_for_user uid in
      let* () = or_fail "consume fixture" r in
      let* () = attach_unavailable "consumed" conn ~state ~binding_hash 42L in
      let* _, (pending, consumed_null, _) = single_row conn uid in
      Alcotest.(check (option int64)) "pending still NULL" None pending;
      Alcotest.(check bool) "row stayed consumed" false consumed_null;
      Lwt.return_unit)

(* === consume ===
   consume starts (and commits) its own transaction, so these cases cannot
   run inside the rolled-back tx_case wrapper: a nested BEGIN would make
   consume's COMMIT commit the outer fixture transaction. Instead each case
   runs in autocommit with per-case usernames, deleting its fixture users
   both before (stale rows from a crashed run) and after — the users FK is
   ON DELETE CASCADE, so the state rows die with them. As above, raw
   states/bindings never reach assertion messages; hash comparisons are
   boolean so hash values cannot appear in failure output either.

   Like attach, consume takes no user id: the final callback is a
   cross-site redirect the SameSite=Strict login-session cookie need not
   accompany, so authorization is the state hash + the session-binding
   hash from the SameSite=Lax per-flow cookie + the flow, and the
   authoritative user comes back from the locked row itself.

   Flow mismatch has no case for the same reason as attach: the branch is
   structurally implemented, but with a single domain variant and the
   schema CHECK admitting only 'project_onboarding', no valid current data
   can produce it. *)

let consume_error_str : Store.consume_error -> string = function
  | Store.State_not_found -> "State_not_found"
  | Store.State_expired -> "State_expired"
  | Store.State_already_consumed -> "State_already_consumed"
  | Store.Session_binding_mismatch -> "Session_binding_mismatch"
  | Store.Flow_mismatch -> "Flow_mismatch"
  | Store.Missing_pending_installation -> "Missing_pending_installation"
  | Store.Storage_error -> "Storage_error"

let q_insert_named_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let q_delete_named_user =
  (Caqti_type.string ->. Caqti_type.unit)
    "DELETE FROM users WHERE username = $1"

let q_consumed_epoch_for_user =
  Caqti_type.(int ->! option float)
    "SELECT EXTRACT(EPOCH FROM consumed_at)::float8\n\
    \   FROM github_onboarding_states WHERE user_id = $1"

let q_consumed_count_for_user =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM github_onboarding_states\n\
    \   WHERE user_id = $1 AND consumed_at IS NOT NULL"

let consume_case name ~users f =
  db_case name (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let delete_users () =
        Lwt_list.iter_s
          (fun u ->
            let* r = C.exec q_delete_named_user u in
            or_fail "cleanup" r)
          users
      in
      let* () = delete_users () in
      Lwt.finalize (fun () -> f conn) delete_users)

(* Fixture: a named committed user holding one freshly issued state. *)
let consume_fixture conn username =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_named_user username in
  let* uid = or_fail "user" uid in
  let binding = GOC.generate_session_binding () in
  let binding_hash = GOC.hash_session_binding binding in
  let* state = issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash in
  Lwt.return (uid, state, binding, binding_hash)

let do_consume conn ~state ~binding_hash =
  Store.consume conn ~state ~session_binding_hash:binding_hash
    ~flow:GO.Project_onboarding

let consume_ok conn ~state ~binding_hash =
  let* r = do_consume conn ~state ~binding_hash in
  match r with
  | Ok consumed -> Lwt.return consumed
  | Error e -> Alcotest.failf "consume: %s" (consume_error_str e)

let consume_expect label expected conn ~state ~binding_hash =
  let* r = do_consume conn ~state ~binding_hash in
  match r with
  | Error e ->
      Alcotest.(check string) label expected (consume_error_str e);
      Lwt.return_unit
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label expected

let consume_success_case =
  consume_case "consume: valid state returns stored row, burns once"
    ~users:[ "ghconsume_ok" ] (fun conn ->
      let* uid, state, binding, binding_hash =
        consume_fixture conn "ghconsume_ok"
      in
      let* () = attach_ok conn ~state ~binding_hash 123456789L in
      let* before = single_row conn uid in
      let (state_hash0, binding_hash0, flow0), (_, _, times0) = before in
      (* The call carries only the state, binding hash, and flow — no
         caller user id exists in the consume API at all; the owner comes
         back from the locked row. *)
      let* consumed = consume_ok conn ~state ~binding_hash in
      Alcotest.(check int)
        "returned user_id is the stored one" uid consumed.Store.user_id;
      (match consumed.Store.flow with GO.Project_onboarding -> ());
      Alcotest.(check int64)
        "returned pending id is the stored one" 123456789L
        consumed.Store.pending_github_installation_id;
      let* row = single_row conn uid in
      let ( (state_hash, stored_binding_hash, flow),
            (pending, consumed_null, (created, expires)) ) =
        row
      in
      Alcotest.(check bool) "consumed_at now set" false consumed_null;
      Alcotest.(check bool)
        "state_hash unchanged" true
        (String.equal state_hash0 state_hash);
      Alcotest.(check bool)
        "binding hash unchanged" true
        (String.equal binding_hash0 stored_binding_hash);
      Alcotest.(check string) "flow unchanged" flow0 flow;
      Alcotest.(check (option int64))
        "pending id unchanged" (Some 123456789L) pending;
      let created0, expires0 = times0 in
      Alcotest.(check (float 0.001)) "created_at unchanged" created0 created;
      Alcotest.(check (float 0.001)) "expires_at unchanged" expires0 expires;
      (* Consumption sends only hashes over SQL; the text columns must
         still hold no raw token material. *)
      let text = String.concat "|" [ state_hash; stored_binding_hash; flow ] in
      Alcotest.(check bool)
        "raw state absent from text columns" false
        (Html_assert.contains_nonempty ~needle:(GOC.state_to_string state) text);
      Alcotest.(check bool)
        "raw binding absent from text columns" false
        (Html_assert.contains_nonempty
           ~needle:(GOC.session_binding_to_string binding)
           text);
      Lwt.return_unit)

let consume_replay_case =
  consume_case "consume: replay preserves the original consumed_at"
    ~users:[ "ghconsume_replay" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash =
        consume_fixture conn "ghconsume_replay"
      in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let* _ = consume_ok conn ~state ~binding_hash in
      let* first = C.find q_consumed_epoch_for_user uid in
      let* first = or_fail "consumed_at" first in
      let first =
        match first with
        | Some epoch -> epoch
        | None -> Alcotest.fail "first consume left consumed_at NULL"
      in
      let* () =
        consume_expect "replay" "State_already_consumed" conn ~state
          ~binding_hash
      in
      let* second = C.find q_consumed_epoch_for_user uid in
      let* second = or_fail "consumed_at after replay" second in
      (* Exact float equality: the stored microsecond timestamp must
         round-trip untouched — any rewrite by the replay would differ. *)
      Alcotest.(check (option (float 0.)))
        "original consumed_at kept" (Some first) second;
      Lwt.return_unit)

let consume_unknown_state_case =
  consume_case "consume: unknown state is State_not_found, creates no row"
    ~users:[ "ghconsume_unknown" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = C.find q_insert_named_user "ghconsume_unknown" in
      let* uid = or_fail "user" uid in
      let binding_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      (* Generated but never issued: no row anywhere carries its hash. *)
      let state = GOC.generate_state () in
      let* () =
        consume_expect "unknown state" "State_not_found" conn ~state
          ~binding_hash
      in
      let* count = C.find q_count_for_user uid in
      let* count = or_fail "count" count in
      Alcotest.(check int) "no row created" 0 count;
      Lwt.return_unit)

let consume_expired_case =
  consume_case "consume: expired state stays unconsumed for cleanup"
    ~users:[ "ghconsume_expired" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash =
        consume_fixture conn "ghconsume_expired"
      in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let* r = C.exec q_expire_states_for_user uid in
      let* () = or_fail "expire fixture" r in
      let* () =
        consume_expect "expired" "State_expired" conn ~state ~binding_hash
      in
      let* _, (_, consumed_null, _) = single_row conn uid in
      Alcotest.(check bool) "consumed_at still NULL" true consumed_null;
      Lwt.return_unit)

(* Ownership preservation: with no user id in the consume API, consuming
   A's state must yield A's stored identity while B's independent state —
   and its stored ownership — stay completely untouched. single_row
   filters on user_id, so retrieving each row under its own user is
   itself the ownership assertion. *)
let consume_ownership_case =
  consume_case "consume: yields the stored owner, other states stay put"
    ~users:[ "ghconsume_own_a"; "ghconsume_own_b" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid_a, state_a, _, binding_hash_a =
        consume_fixture conn "ghconsume_own_a"
      in
      let* uid_b = C.find q_insert_named_user "ghconsume_own_b" in
      let* uid_b = or_fail "second user" uid_b in
      let binding_hash_b =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* _state_b =
        issue_ok conn ~user_id:uid_b ~session_binding_hash:binding_hash_b
      in
      let* () =
        attach_ok conn ~state:state_a ~binding_hash:binding_hash_a 42L
      in
      (* Nothing about B — no id, session, or state — enters this call. *)
      let* consumed =
        consume_ok conn ~state:state_a ~binding_hash:binding_hash_a
      in
      Alcotest.(check int)
        "consumed state belongs to user A" uid_a consumed.Store.user_id;
      let* _, (pending_a, consumed_a, _) = single_row conn uid_a in
      Alcotest.(check bool) "state A consumed" false consumed_a;
      Alcotest.(check (option int64))
        "state A pending id kept" (Some 42L) pending_a;
      (* single_row on uid_b proves B still owns its row. *)
      let* row_b = single_row conn uid_b in
      check_untouched_unconsumed "state B" row_b;
      Lwt.return_unit)

let consume_wrong_binding_case =
  consume_case "consume: session-binding mismatch burns the state"
    ~users:[ "ghconsume_wb" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash = consume_fixture conn "ghconsume_wb" in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let other_hash =
        GOC.hash_session_binding (GOC.generate_session_binding ())
      in
      let* () =
        consume_expect "wrong binding" "Session_binding_mismatch" conn ~state
          ~binding_hash:other_hash
      in
      let* burned_at = C.find q_consumed_epoch_for_user uid in
      let* burned_at = or_fail "consumed_at" burned_at in
      let burned_at =
        match burned_at with
        | Some epoch -> epoch
        | None -> Alcotest.fail "mismatch left consumed_at NULL"
      in
      (* The burn is durable: even the rightful binding is locked out,
         and the retry must not rewrite the original burn timestamp. *)
      let* () =
        consume_expect "correct retry after burn" "State_already_consumed" conn
          ~state ~binding_hash
      in
      let* after = C.find q_consumed_epoch_for_user uid in
      let* after = or_fail "consumed_at after retry" after in
      Alcotest.(check (option (float 0.)))
        "original consumed_at kept" (Some burned_at) after;
      Lwt.return_unit)

let consume_missing_pending_case =
  consume_case "consume: missing installation id burns, blocks attach"
    ~users:[ "ghconsume_mp" ] (fun conn ->
      let* uid, state, _, binding_hash = consume_fixture conn "ghconsume_mp" in
      (* Deliberately no attach: the flow never reached the setup return. *)
      let* () =
        consume_expect "missing pending" "Missing_pending_installation" conn
          ~state ~binding_hash
      in
      let* _, (pending, consumed_null, _) = single_row conn uid in
      Alcotest.(check bool) "state burned" false consumed_null;
      Alcotest.(check (option int64)) "pending still NULL" None pending;
      attach_unavailable "attach after burn" conn ~state ~binding_hash 42L)

(* Two independent connections race on one state: the second blocks on the
   first's FOR UPDATE row lock, then re-reads the committed row and must
   see it consumed. Exactly one winner, one burn. *)
let consume_concurrent_case =
  consume_case "consume: concurrent consumers serialize on the row lock"
    ~users:[ "ghconsume_race" ] (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid, state, _, binding_hash =
        consume_fixture conn "ghconsume_race"
      in
      let* () = attach_ok conn ~state ~binding_hash 42L in
      let url =
        (* db_case only runs under the gate, so the URL is present. *)
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* conn2 = or_fail "second connect" conn2 in
      let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
      Lwt.finalize
        (fun () ->
          let* r1, r2 =
            Lwt.both
              (do_consume conn ~state ~binding_hash)
              (do_consume conn2 ~state ~binding_hash)
          in
          let classify label = function
            | Ok consumed ->
                Alcotest.(check int64)
                  (label ^ ": winner sees stored pending id")
                  42L consumed.Store.pending_github_installation_id;
                `Won
            | Error Store.State_already_consumed -> `Lost
            | Error e ->
                Alcotest.failf "%s: unexpected %s" label (consume_error_str e)
          in
          (match (classify "first" r1, classify "second" r2) with
          | `Won, `Lost | `Lost, `Won -> ()
          | `Won, `Won -> Alcotest.fail "both consumers won"
          | `Lost, `Lost -> Alcotest.fail "no consumer won");
          let* burned = C.find q_consumed_count_for_user uid in
          let* burned = or_fail "burn count" burned in
          Alcotest.(check int) "exactly one non-null consumed_at" 1 burned;
          Lwt.return_unit)
        (fun () -> C2.disconnect ()))

let suite =
  [
    single_issue_case;
    multiplicity_case;
    invalid_user_case;
    missing_user_case;
    attach_validation_case;
    attach_success_case;
    attach_idempotent_case;
    attach_conflict_case;
    attach_missing_state_case;
    attach_ownership_case;
    attach_wrong_binding_case;
    attach_expired_case;
    attach_consumed_case;
    consume_success_case;
    consume_replay_case;
    consume_unknown_state_case;
    consume_expired_case;
    consume_ownership_case;
    consume_wrong_binding_case;
    consume_missing_pending_case;
    consume_concurrent_case;
  ]

let suites =
  (* State issuance: the TTL constant is checkable DB-free; the SQL
       contract needs Postgres and follows the EARDE_TEST_DATABASE_URL
       gate (each DB case skips without it). *)
  [
    ( "github_state_store",
      Case.quick "ttl_seconds is 900" (fun () ->
          Alcotest.(check int)
            "ttl" 900 Earde.Github_onboarding_state_store.ttl_seconds)
      :: suite );
  ]
