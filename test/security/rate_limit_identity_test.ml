(* === RATE-LIMIT IDENTITY (routed) ===
   Database-gated (EARDE_TEST_DATABASE_URL). Drives the production route table
   (Earde.App_routes.router) through the real limiter, real login handler,
   real Argon2 and real SQL sessions.

   A bucket used to be keyed on the request target. Dream's router decodes and
   normalizes the path, so /%6cogin, ///login and //////login all reached the
   login handler with a fresh five-attempt allowance each: online password
   guessing had no bound, and after /login itself was blocked the correct
   password still logged in through another spelling. Buckets are now one per
   logical operation; these cases exhaust the canonical route and then try
   every alias. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let client_prefix = "rli-"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM rate_limits WHERE ip_address LIKE 'rli-%'";
      "DELETE FROM dream_session WHERE payload::jsonb ->> 'user_id' IN \
       (SELECT id::text FROM users WHERE username LIKE 'rli\\_%')";
      "DELETE FROM users WHERE username LIKE 'rli\\_%'";
    ]

let q_user =
  (Caqti_type.(t3 string string string) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $2, $3, TRUE) RETURNING id"

let q_buckets =
  (Caqti_type.string ->* Caqti_type.(t2 string int))
    "SELECT endpoint, attempts FROM rate_limits WHERE ip_address = $1 ORDER BY \
     endpoint"

let q_sessions_for =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM dream_session WHERE payload::jsonb ->> \
     'user_id' = $1::text"

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
                   or_fail "cleanup" r)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let blocked body = Html_assert.contains body "Too Many Attempts"
let invalid body = Html_assert.contains body "Invalid username or password"

let login app browser ~target ~csrf password =
  App_fixture.post app browser target
    [
      ("dream.csrf", csrf);
      ("identifier", "rli_victim");
      ("password", password);
    ]

let aliases =
  [
    "/%6cogin";
    "/lo%67in";
    "/logi%6e";
    "/%6C%6F%67%69%6E";
    "///login";
    "//////login";
    "/login?next=/settings";
    "///%6cogin?x=1";
  ]

(* The pass-4 reproduction, end to end: bad guesses exhaust /login, every
   alias is then refused without reaching the handler, and the CORRECT
   password through an alias neither redirects nor creates a session. *)
let login_alias_case =
  db_case
    "login: after /login is exhausted, encoded, multi-slash and query \
     spellings stay blocked, even with the correct password" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = App_fixture.hash_password "rli correct horse" in
      let* r = C.find q_user ("rli_victim", "rli_victim@rli.invalid", hash) in
      let* uid = or_fail "user" r in
      let client = client_prefix ^ "login" in
      let app = App_fixture.app ~url ~client in
      let browser = App_fixture.browser () in
      let* csrf = App_fixture.csrf_from app browser "/login" in
      let rec bad n =
        if n = 0 then Lwt.return_unit
        else
          let* status, _, body =
            login app browser ~target:"/login" ~csrf "rli wrong guess"
          in
          Alcotest.(check int) "bad guess status" 200 status;
          Alcotest.(check bool) "bad guess reached the handler" true
            (invalid body);
          bad (n - 1)
      in
      let* () = bad 5 in
      let* _, _, body =
        login app browser ~target:"/login" ~csrf "rli wrong guess"
      in
      Alcotest.(check bool) "sixth attempt on /login is blocked" true
        (blocked body);
      let* () =
        Lwt_list.iter_s
          (fun target ->
            let* status, _, body =
              login app browser ~target ~csrf "rli wrong guess"
            in
            Alcotest.(check int) (target ^ ": status") 200 status;
            Alcotest.(check bool) (target ^ ": blocked") true (blocked body);
            Alcotest.(check bool)
              (target ^ ": handler not reached")
              false (invalid body);
            Lwt.return_unit)
          aliases
      in
      let* () =
        Lwt_list.iter_s
          (fun target ->
            let* status, response, body =
              login app browser ~target ~csrf "rli correct horse"
            in
            Alcotest.(check int) (target ^ ": correct password, 200") 200 status;
            Alcotest.(check (option string))
              (target ^ ": no redirect") None
              (Dream.header response "Location");
            Alcotest.(check bool) (target ^ ": blocked") true (blocked body);
            Lwt.return_unit)
          [ "/logi%6e"; "///login"; "/login" ]
      in
      let* r = C.find q_sessions_for uid in
      let* sessions = or_fail "sessions" r in
      Alcotest.(check int) "no session was created for the victim" 0 sessions;
      let* r = C.collect_list q_buckets client in
      let* buckets = or_fail "buckets" r in
      Alcotest.(check (list (pair string int)))
        "one bucket, the operation's, holding every attempt"
        [
          ( Earde.Rate_limit_middleware.bucket Earde.Rate_limit_middleware.Login,
            6 + List.length aliases + 3 );
        ]
        buckets;
      Lwt.return_unit)

(* Control for the case above: the same correct password through the
   canonical route logs in while the bucket has room, so the refusal there is
   the limiter's, not a broken fixture. *)
let login_control_case =
  db_case "login: the correct password logs in while the bucket has room"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = App_fixture.hash_password "rli correct horse" in
      let* r = C.find q_user ("rli_victim", "rli_victim@rli.invalid", hash) in
      let* uid = or_fail "user" r in
      let app = App_fixture.app ~url ~client:(client_prefix ^ "control") in
      let browser = App_fixture.browser () in
      let* csrf = App_fixture.csrf_from app browser "/login" in
      let* status, response, _ =
        login app browser ~target:"/logi%6e" ~csrf "rli correct horse"
      in
      Alcotest.(check int) "redirect" 303 status;
      Alcotest.(check (option string))
        "to the feed" (Some "/") (Dream.header response "Location");
      let* r = C.find q_sessions_for uid in
      let* sessions = or_fail "sessions" r in
      Alcotest.(check int) "one session" 1 sessions;
      Lwt.return_unit)

(* Operations keep separate budgets: exhausting login leaves the reset request
   and signup forms untouched for the same client. *)
let distinct_operations_case =
  db_case "distinct operations keep separate buckets" (fun ~url _c ->
      let app = App_fixture.app ~url ~client:(client_prefix ^ "distinct") in
      let browser = App_fixture.browser () in
      let* csrf = App_fixture.csrf_from app browser "/login" in
      let rec exhaust n =
        if n = 0 then Lwt.return_unit
        else
          let* _ = login app browser ~target:"/login" ~csrf "rli wrong" in
          exhaust (n - 1)
      in
      let* () = exhaust 6 in
      let* _, _, body = login app browser ~target:"/login" ~csrf "rli wrong" in
      Alcotest.(check bool) "login exhausted" true (blocked body);
      let* _, _, body =
        App_fixture.post app browser "/forgot-password"
          [ ("dream.csrf", csrf); ("email", "rli_nobody@rli.invalid") ]
      in
      Alcotest.(check bool) "reset request still allowed" false (blocked body);
      Lwt.return_unit)

(* Route parameters name the resource, not the operation: different slugs
   and ids on the same mutation share one bucket, and the sibling mutation
   keeps its own. The handlers refuse these anonymous requests; the limiter
   counts them first, as it counts every attempt. *)
let parameterized_case =
  db_case "parameterized routes share their operation's bucket" (fun ~url _c ->
      let app = App_fixture.app ~url ~client:(client_prefix ^ "params") in
      let browser = App_fixture.browser () in
      let accept slug id =
        Printf.sprintf "/c/%s/settings/shared-threads/%d/accept" slug id
      in
      let* () =
        Lwt_list.iter_s
          (fun target ->
            let* _, _, body = App_fixture.post app browser target [] in
            Alcotest.(check bool) (target ^ ": counted, not blocked") false
              (blocked body);
            Lwt.return_unit)
          [
            accept "rli-a" 1;
            accept "rli-b" 2;
            accept "rli-c" 3;
            "/c/rli-d/settings/shared-threads/4/accept?x=y";
            "///c/rli-e/settings/shared-threads/5/accept";
          ]
      in
      let* _, _, body = App_fixture.post app browser (accept "rli-z" 99) [] in
      Alcotest.(check bool) "sixth accept, new slug and id: blocked" true
        (blocked body);
      let* _, _, body =
        App_fixture.post app browser "/c/%72li-y/settings/shared-threads/7/accept"
          []
      in
      Alcotest.(check bool) "encoded slug: blocked" true (blocked body);
      let* _, _, body =
        App_fixture.post app browser "/c/rli-a/settings/shared-threads/1/reject"
          []
      in
      Alcotest.(check bool) "sibling operation: own bucket" false (blocked body);
      Lwt.return_unit)

let suites =
  [
    ( "rate_limit_identity",
      [
        login_alias_case;
        login_control_case;
        distinct_operations_case;
        parameterized_case;
      ] );
  ]
