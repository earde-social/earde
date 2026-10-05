(* === TERMINAL ACCOUNT DELETION ===
   Database-gated (EARDE_TEST_DATABASE_URL). A deleted account must stay
   deleted. Deletion used to leave the account's password-reset links and its
   is_admin flag in place, so a link requested before the deletion set a new
   password on the '[deleted_<id>]' tombstone, the tombstone logged in, and
   /admin answered 200.

   Covered here: the routed reproduction; a tombstone that still holds a link
   (rows deleted before the fix); the upgrade migration's repair of
   pre-existing tombstones, including a revived one; the terminal CHECK; and
   the three races between deletion and a reset or password change, each
   paused at a known point by a held session-row lock (deletion and reset both
   revoke sessions last) and confirmed waiting through pg_stat_activity before
   the next step, so the interleaving is the one named, not a timing guess. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let url () =
  match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
  | Some url -> url
  | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"

let connect () =
  let* c = Caqti_lwt_unix.connect (Uri.of_string (url ())) in
  or_fail "connect" c

(* Users are tracked by id: deletion renames them away from the fixture
   prefix. *)
let created : int list ref = ref []

let q_cleanup_by_id =
  List.map
    (fun sql -> (Caqti_type.int ->. Caqti_type.unit) sql)
    [
      "DELETE FROM dream_session WHERE payload::jsonb ->> 'user_id' = $1::text";
      "DELETE FROM password_resets WHERE user_id = $1";
      "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id = 'user_' || \
       $1::text";
      "DELETE FROM users WHERE id = $1";
    ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS tdel_fail_reset_delete ON password_resets";
      "DELETE FROM rate_limits WHERE ip_address LIKE 'tdel-%'";
      "DELETE FROM communities WHERE slug LIKE 'tdel-%'";
      "DELETE FROM dream_session WHERE id LIKE 'tdel-%'";
    ]

let cleanup (module C : Caqti_lwt.CONNECTION) =
  let* () =
    Lwt_list.iter_s
      (fun q ->
        let* r = C.exec q () in
        or_fail "cleanup" r)
      q_cleanup
  in
  let ids = !created in
  created := [];
  Lwt_list.iter_s
    (fun id ->
      Lwt_list.iter_s
        (fun q ->
          let* r = C.exec q id in
          or_fail "cleanup user" r)
        q_cleanup_by_id)
    ids

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = connect () in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let* () = cleanup conn in
             Lwt.finalize
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize (fun () -> cleanup conn) C.disconnect)))

(* === fixtures === *)

let q_user =
  (Caqti_type.(t4 string string string bool) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified, \
     is_admin)\n\
    \   VALUES ($1, $2, $3, TRUE, $4) RETURNING id"

let user ?(admin = false) conn name ~password =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* hash = App_fixture.hash_password password in
  let* r = C.find q_user (name, name ^ "@tdel.invalid", hash, admin) in
  let* id = or_fail ("user " ^ name) r in
  created := id :: !created;
  Lwt.return id

let q_private_community =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility) VALUES ($1, $1, \
     'private') RETURNING id"

let q_generation =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COALESCE((SELECT generation FROM community_realtime_generations \
     WHERE community_id = $1), 0)"

let q_session =
  (Caqti_type.(t2 string int) ->. Caqti_type.unit)
    "INSERT INTO dream_session (id, label, expires_at, payload) VALUES ($1, \
     'tdel', 4000000000, json_build_object('user_id', $2::text)::text)"

let q_state =
  (Caqti_type.int ->! Caqti_type.(t4 string string bool int))
    "SELECT username, password_hash, is_admin, (SELECT COUNT(*)::int FROM \
     password_resets WHERE user_id = $1) FROM users WHERE id = $1"

let q_sessions =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM dream_session WHERE payload::jsonb ->> \
     'user_id' = $1::text"

let state conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_state id in
  or_fail "state" r

let sessions conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_sessions id in
  or_fail "sessions" r

let exec conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.exec q arg in
  or_fail label r

let link conn ~email raw =
  let* r = Earde.Credential_store.create_token conn email raw in
  let* issued = or_fail_s "create link" r in
  Alcotest.(check bool) "link issued" true issued;
  Lwt.return_unit

let tombstone id = Printf.sprintf "[deleted_%d]" id

let check_terminal conn label id =
  let* name, hash, admin, links = state conn id in
  Alcotest.(check string) (label ^ ": tombstone") (tombstone id) name;
  Alcotest.(check string) (label ^ ": no password") "" hash;
  Alcotest.(check bool) (label ^ ": no admin") false admin;
  Alcotest.(check int) (label ^ ": no reset links") 0 links;
  let* n = sessions conn id in
  Alcotest.(check int) (label ^ ": no sessions") 0 n;
  Lwt.return_unit

(* === routed helpers === *)

let login app browser ~csrf identifier password =
  App_fixture.post app browser "/login"
    [ ("dream.csrf", csrf); ("identifier", identifier); ("password", password) ]

let reset app browser ~csrf token password =
  App_fixture.post app browser "/reset-password"
    [
      ("dream.csrf", csrf);
      ("token", token);
      ("password", password);
      ("confirm_password", password);
    ]

let contains = Html_assert.contains

(* === 1. the reproduction, through the production routes === *)

let routed_case =
  db_case
    "deleting an account kills its reset links and admin authority: the old \
     link, the tombstone login and /admin all fail" (fun ~url c ->
      let* victim = user ~admin:true c "tdel_victim" ~password:"tdel old pw" in
      let* bystander = user c "tdel_bystander" ~password:"tdel by pw" in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* r = C.find q_private_community "tdel-private" in
      let* community = or_fail "community" r in
      let* gen_before =
        let* r = C.find q_generation community in
        or_fail "generation" r
      in
      let* () = link c ~email:"tdel_victim@tdel.invalid" "tdel-victim-link" in
      let* () = link c ~email:"tdel_bystander@tdel.invalid" "tdel-by-link" in
      let* () =
        exec c "bystander session" q_session ("tdel-by-sess", bystander)
      in
      let app = App_fixture.app ~url ~client:"tdel-routed" in
      (* The victim logs in and deletes the account. *)
      let owner = App_fixture.browser () in
      let* csrf = App_fixture.csrf_from app owner "/login" in
      let* status, _, _ = login app owner ~csrf "tdel_victim" "tdel old pw" in
      Alcotest.(check int) "victim login" 303 status;
      let* status, _, _ = App_fixture.get app owner "/admin" in
      Alcotest.(check int) "admin before deletion" 200 status;
      let* csrf = App_fixture.csrf_from app owner "/settings" in
      let* status, _, _ =
        App_fixture.post app owner "/delete-account" [ ("dream.csrf", csrf) ]
      in
      Alcotest.(check int) "deletion redirects" 303 status;
      let* () = check_terminal c "after deletion" victim in
      let* gen_after =
        let* r = C.find q_generation community in
        or_fail "generation" r
      in
      Alcotest.(check bool)
        "private community moved to a new generation" true
        (gen_after > gen_before);
      (* Whoever holds the old link tries it. *)
      let holder = App_fixture.browser () in
      let* _, _, page =
        App_fixture.get app holder "/reset-password?token=tdel-victim-link"
      in
      Alcotest.(check bool)
        "reset page offers no form" false
        (contains page "name='password'");
      let* csrf = App_fixture.csrf_from app holder "/login" in
      let* status, _, body =
        reset app holder ~csrf "tdel-victim-link" "tdel new pw"
      in
      Alcotest.(check int) "reset status" 200 status;
      Alcotest.(check bool)
        "reset refused as an expired link" true
        (contains body "Link Expired");
      Alcotest.(check bool)
        "no success" false
        (contains body "Password Updated");
      let* () = check_terminal c "after the old link" victim in
      let* status, response, _ =
        login app holder ~csrf (tombstone victim) "tdel new pw"
      in
      Alcotest.(check int) "tombstone login refused" 200 status;
      Alcotest.(check (option string))
        "no redirect" None
        (Dream.header response "Location");
      let* status, _, _ = App_fixture.get app holder "/admin" in
      Alcotest.(check bool) "no /admin" true (status <> 200);
      (* The deleted owner's old browser lost its session and its admin. *)
      let* status, _, _ = App_fixture.get app owner "/admin" in
      Alcotest.(check bool) "old cookie: no /admin" true (status <> 200);
      let* () = check_terminal c "at the end" victim in
      (* The bystander's link and session were never touched, and an active
         account's reset still works. *)
      let* name, _, _, links = state c bystander in
      Alcotest.(check string) "bystander intact" "tdel_bystander" name;
      Alcotest.(check int) "bystander link kept" 1 links;
      let* n = sessions c bystander in
      Alcotest.(check int) "bystander session kept" 1 n;
      let* status, _, body =
        reset app holder ~csrf "tdel-by-link" "tdel by new"
      in
      Alcotest.(check int) "bystander reset" 200 status;
      Alcotest.(check bool)
        "bystander reset works" true
        (contains body "Password Updated");
      Lwt.return_unit)

(* === 2. a tombstone that already holds a link === *)

let q_old_style_tombstone =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET username = '[deleted_' || id || ']', email = 'deleted_' \
     || id || '@earde.local', password_hash = '', bio = NULL, avatar_url = \
     NULL WHERE id = $1"

let q_raw_link =
  (Caqti_type.(t2 string int) ->. Caqti_type.unit)
    "INSERT INTO password_resets (token, user_id, expires_at) VALUES \
     (encode(sha256(convert_to($1, 'UTF8')), 'hex'), $2, NOW() + INTERVAL '2 \
     hours')"

let surviving_link_case =
  db_case
    "a link that outlived its account's deletion is refused and sets nothing"
    (fun ~url c ->
      let* id = user c "tdel_ghost" ~password:"tdel ghost pw" in
      let* () = exec c "tombstone" q_old_style_tombstone id in
      let* () = exec c "surviving link" q_raw_link ("tdel-ghost-link", id) in
      let* r = Earde.Credential_store.validate_token c "tdel-ghost-link" in
      let* valid = or_fail_s "validate" r in
      Alcotest.(check (option int)) "not a valid link" None valid;
      let app = App_fixture.app ~url ~client:"tdel-ghost" in
      let b = App_fixture.browser () in
      let* csrf = App_fixture.csrf_from app b "/login" in
      let* _, _, body = reset app b ~csrf "tdel-ghost-link" "tdel ghost new" in
      Alcotest.(check bool) "refused" true (contains body "Link Expired");
      let* _, hash, _, links = state c id in
      Alcotest.(check string) "no password set" "" hash;
      Alcotest.(check int) "the dead link was not consumed" 1 links;
      let* status, _, _ = login app b ~csrf (tombstone id) "tdel ghost new" in
      Alcotest.(check int) "tombstone login refused" 200 status;
      (* No new link can be issued for the tombstone either. *)
      let* r =
        Earde.Credential_store.create_token c
          (Printf.sprintf "deleted_%d@earde.local" id)
          "tdel-ghost-second"
      in
      let* issued = or_fail_s "create" r in
      Alcotest.(check bool) "no link for a tombstone" false issued;
      Lwt.return_unit)

(* === 3. the upgrade repairs tombstones written before the fix === *)

(* The migration's own up-section, executed statement by statement inside a
   transaction that the case rolls back: the shared test database keeps its
   constraint whatever happens. *)
let migration_up () =
  let path =
    Filename.concat
      (if Sys.file_exists "../db/migrations" then "../db/migrations"
       else "db/migrations")
      "20261005120000_terminal_deleted_accounts.sql"
  in
  let text = In_channel.with_open_bin path In_channel.input_all in
  let up =
    match Html_assert.index_from text "-- migrate:down" 0 with
    | Some i -> String.sub text 0 i
    | None -> Alcotest.fail "migration has no down section"
  in
  let without_comments =
    String.split_on_char '\n' up
    |> List.filter (fun l ->
        not (String.length l >= 2 && String.sub l 0 2 = "--"))
    |> String.concat "\n"
  in
  String.split_on_char ';' without_comments
  |> List.map String.trim
  |> List.filter (( <> ) "")

let q_drop_constraint =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE users DROP CONSTRAINT users_deleted_account_terminal"

let q_revive =
  (Caqti_type.(t2 string int) ->. Caqti_type.unit)
    "UPDATE users SET password_hash = $1, is_admin = TRUE WHERE id = $2"

let q_admin_keep =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

let q_constraint_present =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM pg_constraint WHERE conname = \
     'users_deleted_account_terminal' AND convalidated"

let migration_case =
  db_case
    "the upgrade strips links, sessions, passwords and admin from existing \
     tombstones and leaves live accounts alone" (fun ~url:_ c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* stale = user c "tdel_stale" ~password:"tdel stale pw" in
      let* revived = user c "tdel_revived" ~password:"tdel revived pw" in
      let* live = user ~admin:true c "tdel_live" ~password:"tdel live pw" in
      let* r = C.find q_private_community "tdel-mig" in
      let* community = or_fail "community" r in
      let* r = C.start () in
      let* () = or_fail "begin" r in
      Lwt.finalize
        (fun () ->
          let* () = exec c "drop constraint" q_drop_constraint () in
          (* What the old code left: a deleted admin with its link, and a
             tombstone some old link already revived and logged into. *)
          let* () = exec c "stale admin" q_admin_keep stale in
          let* () = exec c "stale tombstone" q_old_style_tombstone stale in
          let* () = exec c "stale link" q_raw_link ("tdel-stale-link", stale) in
          let* () = exec c "revived tombstone" q_old_style_tombstone revived in
          let* () = exec c "revived" q_revive ("$argon2id$revived", revived) in
          let* () =
            exec c "revived session" q_session ("tdel-revived-sess", revived)
          in
          let* () = exec c "live link" q_raw_link ("tdel-live-link", live) in
          let* () = exec c "live session" q_session ("tdel-live-sess", live) in
          let* r = C.find q_generation community in
          let* gen_before = or_fail "generation" r in
          let* () =
            Lwt_list.iter_s
              (fun sql ->
                exec c "migration statement"
                  ((Caqti_type.unit ->. Caqti_type.unit) sql)
                  ())
              (migration_up ())
          in
          let* () = check_terminal c "stale" stale in
          let* () = check_terminal c "revived" revived in
          let* name, hash, admin, links = state c live in
          Alcotest.(check string) "live name" "tdel_live" name;
          Alcotest.(check bool) "live password kept" true (hash <> "");
          Alcotest.(check bool) "live admin kept" true admin;
          Alcotest.(check int) "live link kept" 1 links;
          let* n = sessions c live in
          Alcotest.(check int) "live session kept" 1 n;
          let* r = C.find q_generation community in
          let* gen_after = or_fail "generation" r in
          Alcotest.(check bool)
            "private community bumped" true (gen_after > gen_before);
          let* r = C.find q_constraint_present () in
          let* present = or_fail "constraint" r in
          Alcotest.(check int) "constraint restored and validated" 1 present;
          Lwt.return_unit)
        (fun () ->
          let* _ = C.rollback () in
          Lwt.return_unit))

(* === 4. the constraint === *)

let q_set_hash =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET password_hash = 'tdel-not-a-hash' WHERE id = $1"

let q_set_admin =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

let constraint_case =
  db_case "no write can give a tombstone a password or admin authority"
    (fun ~url:_ c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* id = user c "tdel_check" ~password:"tdel check pw" in
      let* r = Earde.Posthog_deletion_job_store.anonymize_and_enqueue c id in
      let* _ = or_fail_s "delete" r in
      let refused label q =
        let* r = C.exec q id in
        match r with
        | Ok () -> Alcotest.failf "%s: the write was accepted" label
        | Error e ->
            Alcotest.(check bool)
              (label ^ ": check violation")
              true
              (contains (Caqti_error.show e) "users_deleted_account_terminal");
            Lwt.return_unit
      in
      let* () = refused "password" q_set_hash in
      let* () = refused "admin" q_set_admin in
      let* r =
        Earde.Credential_store.update_password_revoking_sessions c id
          "tdel-hash"
      in
      (match r with
      | Ok () -> Alcotest.fail "password change on a tombstone succeeded"
      | Error _ -> ());
      check_terminal c "after refused writes" id)

(* === 5. races === *)

let q_backend_pid =
  (Caqti_type.unit ->! Caqti_type.int) "SELECT pg_backend_pid()"

let q_waiting =
  (Caqti_type.int ->! Caqti_type.bool)
    "SELECT COALESCE((SELECT wait_event_type = 'Lock' FROM pg_stat_activity \
     WHERE pid = $1), FALSE)"

let q_hold_session =
  (Caqti_type.string ->! Caqti_type.string)
    "SELECT id FROM dream_session WHERE id = $1 FOR UPDATE"

let pid conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_backend_pid () in
  or_fail "pid" r

(* Polls until [pid] is waiting on a lock, so the next step happens only
   once the previous one is parked exactly where the case says. *)
let wait_blocked observer label pid =
  let (module O : Caqti_lwt.CONNECTION) = observer in
  let rec go n =
    if n = 0 then Alcotest.failf "%s never blocked" label
    else
      let* r = O.find q_waiting pid in
      let* waiting = or_fail "waiting" r in
      if waiting then Lwt.return_unit
      else
        let* () = Lwt_unix.sleep 0.02 in
        go (n - 1)
  in
  go 500

(* A third connection holds one of the user's session rows, so whichever
   transaction reaches session revocation first parks there with everything
   before it done and locked. *)
let with_paused_sessions c ~session_id f =
  let* c1 = connect () in
  let* c2 = connect () in
  let* hold = connect () in
  let (module H : Caqti_lwt.CONNECTION) = hold in
  Lwt.finalize
    (fun () ->
      let* r = H.start () in
      let* () = or_fail "hold begin" r in
      let* r = H.find q_hold_session session_id in
      let* _ = or_fail "hold session row" r in
      let release () =
        let* r = H.rollback () in
        or_fail "release" r
      in
      f ~c1 ~c2 ~release ~observer:c)
    (fun () ->
      let disconnect (module X : Caqti_lwt.CONNECTION) = X.disconnect () in
      let* () = disconnect hold in
      let* () = disconnect c1 in
      disconnect c2)

let deletion_first_case =
  db_case "race: a reset that waits on an in-flight deletion finds a dead link"
    (fun ~url:_ c ->
      let* id = user ~admin:true c "tdel_race1" ~password:"tdel race pw" in
      let* () = link c ~email:"tdel_race1@tdel.invalid" "tdel-race1-link" in
      let* () = exec c "session" q_session ("tdel-race1-sess", id) in
      with_paused_sessions c ~session_id:"tdel-race1-sess"
        (fun ~c1 ~c2 ~release ~observer ->
          let* p1 = pid c1 in
          let* p2 = pid c2 in
          (* Deletion locks and anonymizes the user, then parks on the held
             session row. *)
          let deletion =
            Earde.Posthog_deletion_job_store.anonymize_and_enqueue c2 id
          in
          let* () = wait_blocked observer "deletion" p2 in
          (* The reset reads the token's owner and waits for the user lock. *)
          let resetting =
            Earde.Credential_store.reset_password_atomically c1
              "tdel-race1-link" "tdel-race-hash"
          in
          let* () = wait_blocked observer "reset" p1 in
          let* () = release () in
          let* r = deletion in
          let* _ = or_fail_s "deletion" r in
          let* r = resetting in
          let* updated = or_fail_s "reset" r in
          Alcotest.(check bool) "reset refused" false updated;
          check_terminal c "deletion first" id))

let reset_first_case =
  db_case
    "race: a deletion that waits on an in-flight reset still ends terminal"
    (fun ~url:_ c ->
      let* id = user ~admin:true c "tdel_race2" ~password:"tdel race pw" in
      let* () = link c ~email:"tdel_race2@tdel.invalid" "tdel-race2-link" in
      let* () = exec c "session" q_session ("tdel-race2-sess", id) in
      with_paused_sessions c ~session_id:"tdel-race2-sess"
        (fun ~c1 ~c2 ~release ~observer ->
          let* p1 = pid c1 in
          let* p2 = pid c2 in
          (* The reset locks the user, takes the link, writes the hash and
             parks on the held session row. *)
          let resetting =
            Earde.Credential_store.reset_password_atomically c1
              "tdel-race2-link" "tdel-race-hash"
          in
          let* () = wait_blocked observer "reset" p1 in
          (* Deletion waits for the same user lock: no opposite order, so no
             deadlock. *)
          let deletion =
            Earde.Posthog_deletion_job_store.anonymize_and_enqueue c2 id
          in
          let* () = wait_blocked observer "deletion" p2 in
          let* () = release () in
          let* r = resetting in
          let* updated = or_fail_s "reset" r in
          Alcotest.(check bool) "the reset committed first" true updated;
          let* r = deletion in
          let* _ = or_fail_s "deletion" r in
          check_terminal c "reset first" id))

let password_change_case =
  db_case
    "race: a password change that waits on an in-flight deletion writes nothing"
    (fun ~url:_ c ->
      let* id = user c "tdel_race3" ~password:"tdel race pw" in
      let* () = exec c "session" q_session ("tdel-race3-sess", id) in
      with_paused_sessions c ~session_id:"tdel-race3-sess"
        (fun ~c1 ~c2 ~release ~observer ->
          let* p1 = pid c1 in
          let* p2 = pid c2 in
          let deletion =
            Earde.Posthog_deletion_job_store.anonymize_and_enqueue c2 id
          in
          let* () = wait_blocked observer "deletion" p2 in
          (* The change already re-authenticated (outside any transaction)
             before the deletion committed; its write waits for the lock. *)
          let changing =
            Earde.Credential_store.update_password_revoking_sessions c1 id
              "tdel-race-hash"
          in
          let* () = wait_blocked observer "password change" p1 in
          let* () = release () in
          let* r = deletion in
          let* _ = or_fail_s "deletion" r in
          let* r = changing in
          (match r with
          | Ok () -> Alcotest.fail "the password change wrote a tombstone"
          | Error _ -> ());
          check_terminal c "password change" id))

(* === 6. rollback === *)

let q_fail_reset_delete =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER tdel_fail_reset_delete BEFORE DELETE ON password_resets \
     FOR EACH ROW WHEN (OLD.token = encode(sha256('tdel-rollback-link'), \
     'hex')) EXECUTE FUNCTION tdel_raise()"

let q_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE OR REPLACE FUNCTION tdel_raise() RETURNS trigger LANGUAGE plpgsql \
     AS $$ BEGIN RAISE EXCEPTION 'tdel injected failure'; END $$"

let q_drop_fail =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS tdel_fail_reset_delete ON password_resets";
      "DROP FUNCTION IF EXISTS tdel_raise()";
    ]

let rollback_case =
  db_case
    "a deletion that fails revoking links rolls back whole: account, link and \
     sessions intact" (fun ~url:_ c ->
      let* id = user ~admin:true c "tdel_rollback" ~password:"tdel rb pw" in
      let* () =
        link c ~email:"tdel_rollback@tdel.invalid" "tdel-rollback-link"
      in
      let* () = exec c "session" q_session ("tdel-rollback-sess", id) in
      let* () = exec c "fail fn" q_fail_fn () in
      let* () = exec c "fail trigger" q_fail_reset_delete () in
      Lwt.finalize
        (fun () ->
          let* r =
            Earde.Posthog_deletion_job_store.anonymize_and_enqueue c id
          in
          (match r with
          | Ok _ -> Alcotest.fail "deletion committed despite the failure"
          | Error _ -> ());
          let* name, hash, admin, links = state c id in
          Alcotest.(check string) "name kept" "tdel_rollback" name;
          Alcotest.(check bool) "password kept" true (hash <> "");
          Alcotest.(check bool) "admin kept" true admin;
          Alcotest.(check int) "link kept" 1 links;
          let* n = sessions c id in
          Alcotest.(check int) "session kept" 1 n;
          Lwt.return_unit)
        (fun () ->
          Lwt_list.iter_s (fun q -> exec c "drop fail" q ()) q_drop_fail))

(* === 7. a login whose verification overlaps a deletion, ban or password change ===
   Argon2 runs with no lock held, between the account lookup and the session
   write. The injected verifier performs the competing change on its own
   connection, committed, and then verifies normally, so the change lands
   exactly inside that window. *)

let q_ban =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

let login_race_app ~url ~during =
  Dream.sql_pool ~size:2 url
  @@ Dream.set_secret App_fixture.secret
  @@ Dream.sql_sessions
  @@ Dream.router
       [
         Dream.get "/token" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.post "/login"
           (Earde.Auth_handlers.make_login_handler
              ~verify:(fun ~password ~hash ->
                let* () = during () in
                Earde.Login_verification.argon2_verifier ~password ~hash));
       ]

let login_race_case label ~change ~expect_login =
  db_case label (fun ~url c ->
      let* id = user c "tdel_login" ~password:"tdel login pw" in
      let* other = connect () in
      let (module O : Caqti_lwt.CONNECTION) = other in
      Lwt.finalize
        (fun () ->
          let app = login_race_app ~url ~during:(fun () -> change other id) in
          let b = App_fixture.browser () in
          let* _, _, csrf = App_fixture.get app b "/token" in
          let* status, _, _ =
            App_fixture.post app b "/login"
              [
                ("dream.csrf", csrf);
                ("identifier", "tdel_login");
                ("password", "tdel login pw");
              ]
          in
          let* n = sessions c id in
          if expect_login then begin
            Alcotest.(check int) "logged in" 303 status;
            Alcotest.(check int) "one session" 1 n
          end
          else begin
            Alcotest.(check bool) "no login" true (status <> 303);
            Alcotest.(check int) "no session for the account" 0 n
          end;
          Lwt.return_unit)
        O.disconnect)

let login_control_case =
  login_race_case "login race control: nothing changes, the login succeeds"
    ~change:(fun _ _ -> Lwt.return_unit)
    ~expect_login:true

let login_deletion_case =
  login_race_case
    "login race: an account deleted during verification gets no session"
    ~change:(fun other id ->
      let* r =
        Earde.Posthog_deletion_job_store.anonymize_and_enqueue other id
      in
      let* _ = or_fail_s "delete" r in
      Lwt.return_unit)
    ~expect_login:false

let login_password_change_case =
  login_race_case
    "login race: a password changed during verification gets no session"
    ~change:(fun other id ->
      let* r =
        Earde.Credential_store.update_password_revoking_sessions other id
          "tdel-replaced-hash"
      in
      or_fail_s "change" r)
    ~expect_login:false

let login_ban_case =
  login_race_case
    "login race: an account banned during verification gets no session"
    ~change:(fun other id -> exec other "ban" q_ban id)
    ~expect_login:false

let suites =
  [
    ( "terminal_deletion",
      [
        routed_case;
        surviving_link_case;
        migration_case;
        constraint_case;
        deletion_first_case;
        reset_first_case;
        password_change_case;
        rollback_case;
        login_control_case;
        login_deletion_case;
        login_password_change_case;
        login_ban_case;
      ] );
  ]
