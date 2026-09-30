(* === RATE-LIMIT BLOCKED PAGE ===
   Database-gated (EARDE_TEST_DATABASE_URL): drives the real
   Handlers.Rate_limit.middleware past its limit over Postgres and asserts the
   blocked page's return link is the path only — no query value from the
   original target may reach the rendered HTML. Fixture "secrets" are obviously
   fake, and assertions on them are boolean so a failure never prints them. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let q_cleanup =
  (Caqti_type.string ->. Caqti_type.unit)
  "DELETE FROM rate_limits WHERE endpoint = $1"

(* One case = one endpoint bucket, cleared before and after so repeated runs
   start from a fresh window. *)
let db_case name ~endpoint f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               let* r = C.exec q_cleanup endpoint in
               or_fail "cleanup" r
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* Repeats the same GET through the real middleware until the limiter
   blocks (bounded well past the production limit, which stays private to
   Rate_limit_store), returning the blocked response's status and body. *)
let run_until_blocked ~url ~target =
  let handler =
    Dream.sql_pool url @@ Dream.memory_sessions
    @@ Earde.Handlers.Rate_limit.middleware (fun _ ->
           Dream.respond "rlbp-allowed")
  in
  let rec go n =
    if n > 20 then Alcotest.fail "limiter never blocked within 20 attempts"
    else
      let* response = handler (Dream.request ~method_:`GET ~target "") in
      let* body = Dream.body response in
      if Html_assert.contains body "rlbp-allowed" then go (n + 1)
      else Lwt.return (Dream.status_to_int (Dream.status response), body)
  in
  go 1

let callback_case =
  db_case "blocked callback page links to the path only"
    ~endpoint:"/integrations/github/authorize/callback" (fun ~url ->
      let* status, body =
        run_until_blocked ~url
          ~target:
            "/integrations/github/authorize/callback?code=fake_rl_code_21&state=fake_rl_state_22"
      in
      Alcotest.(check int) "blocked status" 200 status;
      Alcotest.(check bool) "normal blocked page" true
        (Html_assert.contains body "Too Many Attempts");
      Alcotest.(check bool) "return link is path only" true
        (Html_assert.contains body "href='/integrations/github/authorize/callback'");
      Alcotest.(check bool) "raw code absent" false
        (Html_assert.contains body "fake_rl_code_21");
      Alcotest.(check bool) "raw state absent" false
        (Html_assert.contains body "fake_rl_state_22");
      Alcotest.(check bool) "no code parameter" false (Html_assert.contains body "code=");
      Alcotest.(check bool) "no state parameter" false
        (Html_assert.contains body "state=");
      Alcotest.(check bool) "no query on the callback path" false
        (Html_assert.contains body "/integrations/github/authorize/callback?");
      Lwt.return_unit)

let login_case =
  db_case "blocked /login?next=/settings page returns to /login"
    ~endpoint:"/login" (fun ~url ->
      let* status, body =
        run_until_blocked ~url ~target:"/login?next=/settings"
      in
      Alcotest.(check int) "blocked status" 200 status;
      Alcotest.(check bool) "normal blocked page" true
        (Html_assert.contains body "Too Many Attempts");
      Alcotest.(check bool) "return link is /login" true
        (Html_assert.contains body "href='/login'");
      Alcotest.(check bool) "original query absent" false
        (Html_assert.contains body "next=/settings");
      Lwt.return_unit)

let suite = [ callback_case; login_case ]

let suites =
    (* Blocked-page return link over the real middleware + Postgres. *)
  [ ("rate_limit_blocked_page", suite)
  ]
