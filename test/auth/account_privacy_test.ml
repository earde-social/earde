(* Account privacy and fail-closed authentication boundaries. DB-free: the
   login dummy-verification contract, the fail-closed rate-limit decision
   and the strict response/cookie comparator. Gated
   (EARDE_TEST_DATABASE_URL): the real signup, login, password-reset and
   limiter handlers over routed pipelines against PostgreSQL, including
   paired target/probe sequences at the capacity edge. Fixture names use
   the b2x_ prefix and the reserved b2.invalid domain; every gated case
   cleans before and after. *)

open Auth_mail_fixture
open Caqti_request.Infix

(* ------------------------------------------------------------------ *)
(* Login verification contract                                          *)
(* ------------------------------------------------------------------ *)

let dummy_fixture_case =
  pure_case
    "dummy hash: a valid Argon2id encoding with the production cost that \
     verifies its public password with full work" (fun () ->
      let expected_prefix =
        Printf.sprintf "$argon2id$v=19$m=%d,t=%d,p=%d$" Earde.Auth.m_cost
          Earde.Auth.t_cost Earde.Auth.parallelism
      in
      Alcotest.(check string)
        "production parameters" expected_prefix
        (String.sub LV.dummy_hash 0 (String.length expected_prefix));
      Alcotest.(check (list int))
        "production cost is unchanged" [ 65536; 2; 1 ]
        [ Earde.Auth.m_cost; Earde.Auth.t_cost; Earde.Auth.parallelism ];
      (* A malformed dummy would fail instantly instead of doing the work:
         only a true positive proves the encoding is complete. *)
      Alcotest.(check bool)
        "fixture verifies its password" true
        (Argon2.verify ~pwd:LV.dummy_password ~encoded:LV.dummy_hash
           ~kind:Argon2.ID
        = Ok true);
      let* wrong =
        LV.argon2_verifier ~password:"not the dummy password"
          ~hash:LV.dummy_hash
      in
      Alcotest.(check bool) "other passwords do not" false wrong;
      let* garbage = LV.argon2_verifier ~password:"x" ~hash:"not-a-hash" in
      Alcotest.(check bool) "malformed hash is false" false garbage;
      Lwt.return_unit)

let counting () =
  let calls = ref [] in
  let verify ~answer ~password:_ ~hash =
    calls := hash :: !calls;
    Lwt.return answer
  in
  (calls, verify)

let authenticate_case =
  pure_case
    "authenticate: exactly one verification on every path; a missing account \
     never authenticates, even on a dummy match" (fun () ->
      let calls, verify = counting () in
      let* r =
        LV.authenticate ~verify:(verify ~answer:true)
          ~password:LV.dummy_password None
      in
      Alcotest.(check bool) "dummy match is not a login" true (r = None);
      Alcotest.(check (list string))
        "missing: verified against the dummy" [ LV.dummy_hash ] !calls;
      calls := [];
      let* r =
        LV.authenticate ~verify:(verify ~answer:false) ~password:"p" None
      in
      Alcotest.(check bool) "missing: none" true (r = None);
      Alcotest.(check (list string))
        "missing: one call" [ LV.dummy_hash ] !calls;
      calls := [];
      let* r =
        LV.authenticate ~verify:(verify ~answer:false) ~password:"p"
          (Some ("stored", 7))
      in
      Alcotest.(check bool) "wrong password: none" true (r = None);
      Alcotest.(check (list string)) "wrong: own hash" [ "stored" ] !calls;
      calls := [];
      let* r =
        LV.authenticate ~verify:(verify ~answer:true) ~password:"p"
          (Some ("stored", 7))
      in
      Alcotest.(check bool) "right password: the account" true (r = Some 7);
      Alcotest.(check (list string)) "right: own hash" [ "stored" ] !calls;
      let raising ~password:_ ~hash:_ = failwith "verifier crashed" in
      let* r =
        LV.authenticate ~verify:raising ~password:"p" (Some ("stored", 7))
      in
      Alcotest.(check bool)
        "crashing verifier authenticates nobody" true (r = None);
      Lwt.return_unit)

let login_pure_suite = [ dummy_fixture_case; authenticate_case ]

(* ------------------------------------------------------------------ *)
(* Rate limiter decision, DB-free                                        *)
(* ------------------------------------------------------------------ *)

let limiter_app ~check ~cleanup ~hits
    ?(inner =
      fun _ ->
        incr hits;
        Dream.respond "b2-inner-ran") () =
  Dream.set_secret "b2-limiter-secret"
  @@ Dream.memory_sessions
  @@ Earde.Rate_limit_middleware.make_middleware ~check ~cleanup (fun req ->
      inner req)

let run_limited app ~target =
  let* response = app (Dream.request ~method_:`POST ~target "") in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

let no_cleanup _ = ()

let limiter_decision_case =
  pure_case
    "limiter: only Allowed invokes the handler; Error, raise and rejection \
     refuse with a generic 503" (fun () ->
      let seen_endpoint = ref "" in
      let check_with result request ~ip:_ ~endpoint =
        ignore request;
        seen_endpoint := endpoint;
        result ()
      in
      let cases =
        [
          ("allowed", (fun () -> Lwt.return (Ok `Allowed)), 200, 1);
          ("blocked", (fun () -> Lwt.return (Ok `Blocked)), 200, 0);
          ( "result error",
            (fun () -> Lwt.return (Error "relation rate_limits does not exist")),
            503,
            0 );
          ("sync raise", (fun () -> failwith "pool exhausted"), 503, 0);
          ( "rejected promise",
            (fun () -> Lwt.fail (Failure "connection refused")),
            503,
            0 );
        ]
      in
      Lwt_list.iter_s
        (fun (label, result, status, expected_hits) ->
          let hits = ref 0 in
          let app =
            limiter_app ~check:(check_with result) ~cleanup:no_cleanup ~hits ()
          in
          let* s, body =
            run_limited app ~target:"/login?token=b2-query-secret"
          in
          Alcotest.(check int) (label ^ ": status") status s;
          Alcotest.(check int)
            (label ^ ": handler invocations")
            expected_hits !hits;
          Alcotest.(check string)
            (label ^ ": bucket is the path only")
            "/login" !seen_endpoint;
          must_not (label ^ ": no query secret") body "b2-query-secret";
          if status = 503 then begin
            must label body "Temporarily unavailable";
            List.iter (must_not label body)
              [
                "rate_limits";
                "relation";
                "pool exhausted";
                "connection refused";
                "Failure";
              ]
          end;
          if label = "blocked" then must label body "Too Many Attempts";
          Lwt.return_unit)
        cases)

let limiter_cleanup_case =
  pure_case
    "limiter: a failing cleanup never changes the decision in either direction"
    (fun () ->
      let raising_cleanup _ = failwith "cleanup exploded" in
      let async_failing_cleanup _ = Lwt.async (fun () -> Lwt.return_unit) in
      Lwt_list.iter_s
        (fun cleanup ->
          let hits = ref 0 in
          let allowed =
            limiter_app
              ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed))
              ~cleanup ~hits ()
          in
          let* s, _ = run_limited allowed ~target:"/login" in
          Alcotest.(check (pair int int))
            "allowed still runs" (200, 1) (s, !hits);
          let hits = ref 0 in
          let failing =
            limiter_app
              ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Error "x"))
              ~cleanup ~hits ()
          in
          let* s, _ = run_limited failing ~target:"/login" in
          Alcotest.(check (pair int int))
            "error still refuses" (503, 0) (s, !hits);
          let hits = ref 0 in
          let blocked =
            limiter_app
              ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Blocked))
              ~cleanup ~hits ()
          in
          let* s, body = run_limited blocked ~target:"/login" in
          Alcotest.(check (pair int int))
            "blocked still blocks" (200, 0) (s, !hits);
          must "blocked page" body "Too Many Attempts";
          Lwt.return_unit)
        [ raising_cleanup; async_failing_cleanup ])

let limiter_inner_exception_case =
  pure_case
    "limiter: an exception raised by the allowed handler is its own, not a \
     limiter outage" (fun () ->
      let hits = ref 0 in
      let app =
        limiter_app
          ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed))
          ~cleanup:no_cleanup ~hits
          ~inner:(fun _ ->
            incr hits;
            failwith "b2 handler bug")
          ()
      in
      let* r =
        Lwt.catch
          (fun () ->
            Lwt.map (fun _ -> "responded") (run_limited app ~target:"/login"))
          (function
            | Failure m when m = "b2 handler bug" -> Lwt.return "propagated"
            | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "handler exception propagates" "propagated" r;
      Alcotest.(check int) "handler ran once" 1 !hits;
      Lwt.return_unit)

let limiter_pure_suite =
  [ limiter_decision_case; limiter_cleanup_case; limiter_inner_exception_case ]

(* ================================================================== *)
(* Gated suites                                                          *)
(* ================================================================== *)

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP SCHEMA IF EXISTS b2_shadow CASCADE";
      "DELETE FROM password_resets WHERE user_id IN (SELECT id FROM users \
       WHERE username LIKE 'b2x\\_%')";
      "DELETE FROM pending_signups WHERE email LIKE '%@b2.invalid' OR username \
       LIKE 'b2x\\_%'";
      "DELETE FROM rate_limits WHERE endpoint LIKE '/b2-%'";
      "DELETE FROM users WHERE username LIKE 'b2x\\_%'";
    ]

let env_keys =
  [
    "EARDE_SIGNUPS_ENABLED";
    "TURNSTILE_SITE_KEY";
    "TURNSTILE_SECRET_KEY";
    "EARDE_TURNSTILE_REQUIRED";
  ]

(* Signup open, Turnstile disabled — exactly the dev configuration; every
   variable is emptied again afterwards (an empty value reads as unset). *)
let with_signup_env f =
  List.iter (fun k -> Unix.putenv k "") env_keys;
  Unix.putenv "EARDE_SIGNUPS_ENABLED" "1";
  Lwt.finalize f (fun () ->
      List.iter (fun k -> Unix.putenv k "") env_keys;
      Lwt.return_unit)

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
               (fun () ->
                 with_signup_env (fun () ->
                     f ~url (module C : Caqti_lwt.CONNECTION)))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === fixtures === *)

let strip_nuls s =
  match String.index_opt s '\000' with Some i -> String.sub s 0 i | None -> s

let real_hash password =
  let* h = Earde.Auth.hash_password password in
  let* h = or_fail_s "hash" h in
  Lwt.return (strip_nuls h)

let q_user =
  (Caqti_type.(t3 string string string) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $2, $3, TRUE) RETURNING id"

let q_ban =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

(* hours may be negative: an already-expired reservation. *)
let q_pending =
  (Caqti_type.(t4 string string string float) ->. Caqti_type.unit)
    "INSERT INTO pending_signups (username, email, password_hash, token_hash, \
     expires_at)\n\
    \   VALUES ($1, $2, 'b2-fixture-not-a-hash', $3, NOW() + ($4 * INTERVAL '1 \
     hour'))"

let q_pending_rows =
  (Caqti_type.unit ->* Caqti_type.(t3 string string string))
    "SELECT LOWER(username), LOWER(email), token_hash FROM pending_signups\n\
    \    WHERE consumed_at IS NULL AND (email LIKE '%@b2.invalid' OR username \
     LIKE 'b2x\\_%')\n\
    \    ORDER BY 1, 2"

let q_users_snapshot =
  (Caqti_type.unit ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(id::text || '|' || username || '|' || email || \
     '|' || password_hash\n\
    \            || '|' || is_banned::text || '|' || is_email_verified::text, \
     ';' ORDER BY id), '')\n\
    \     FROM users WHERE username LIKE 'b2x\\_%'"

let q_user_email =
  (Caqti_type.string ->? Caqti_type.string)
    "SELECT email FROM users WHERE username = $1"

let q_reset_rows =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM password_resets\n\
    \    WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')"

let q_reset_token_exists =
  (Caqti_type.string ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM password_resets WHERE token = $1)"

let q_pending_token_exists =
  (Caqti_type.string ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM pending_signups WHERE token_hash = $1 AND \
     consumed_at IS NULL)"

let q_pending_json =
  (Caqti_type.unit ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(row_to_json(p)::text, ';'), '') FROM \
     pending_signups p\n\
    \    WHERE email LIKE '%@b2.invalid' OR username LIKE 'b2x\\_%'"

let q_reset_json =
  (Caqti_type.unit ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(row_to_json(r)::text, ';'), '') FROM \
     password_resets r\n\
    \    WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')"

let q_lock_waiters =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM pg_locks l JOIN pg_stat_activity a ON a.pid = \
     l.pid\n\
    \    WHERE NOT l.granted AND a.datname = current_database()"

let sha256 s = Digestif.SHA256.(digest_string s |> to_hex)

(* === mail doubles === *)

type fake_mail = {
  clock : Vclock.t;
  mutable started : Earde.Email.message list;
  mutable delivered : Earde.Email.message list;
  mutable gate : unit Lwt.t;
  mutable at_start : Earde.Email.message -> unit Lwt.t;
  mutable in_flight : int;
}

(* Virtual time stands still unless the test moves it, so a stuck delivery
   stays stuck (like one lost with a restarted process) and a slot ends only
   when [drained] or the test advances the clock. *)
let fake_dispatcher ?(gate = Lwt.return_unit) () =
  let fm =
    {
      clock = Vclock.create ();
      started = [];
      delivered = [];
      gate;
      at_start = (fun _ -> Lwt.return_unit);
      in_flight = 0;
    }
  in
  let transport m =
    fm.started <- fm.started @ [ m ];
    fm.in_flight <- fm.in_flight + 1;
    Lwt.finalize
      (fun () ->
        let* () = fm.at_start m in
        let* () = fm.gate in
        fm.delivered <- fm.delivered @ [ m ];
        Lwt.return (Ok ()))
      (fun () ->
        fm.in_flight <- fm.in_flight - 1;
        Lwt.return_unit)
  in
  let d =
    D.create ~sleep:(Vclock.sleep fm.clock) ~label:Earde.Email.label ~transport
      ()
  in
  (fm, d)

(* Runs the schedule to the end: lets every started delivery finish (it may
   query the database on another connection), then moves virtual time one
   slot forward, until nothing is left. Caqti connections are not
   shareable, so tests wait for this before touching their own again. *)
let drained fm d =
  let rec go n =
    let* () = eventually "deliveries finished" (fun () -> fm.in_flight = 0) in
    let s = D.stats d in
    if s.D.outstanding = 0 then Lwt.return_unit
    else if n > 1000 then Alcotest.fail "dispatcher never drained"
    else if s.D.running = 0 && s.D.queued = 0 then
      (* Only reservations: requests still doing their work. *)
      Lwt.bind (Lwt_unix.sleep 0.005) (fun () -> go (n + 1))
    else begin
      Vclock.advance_to fm.clock
        (fm.clock.Vclock.now +. D.default_config.D.timeout_seconds);
      go (n + 1)
    end
  in
  go 0

(* === the routed pipeline === *)

let current_mail : Earde.Email.message D.t option ref = ref None
let verified_hashes : string list ref = ref []

let counting_verifier ~password ~hash =
  verified_hashes := hash :: !verified_hashes;
  LV.argon2_verifier ~password ~hash

let mail () =
  match !current_mail with
  | Some d -> d
  | None -> Alcotest.fail "no dispatcher installed"

let pipelines : (string, Dream.handler) Hashtbl.t = Hashtbl.create 4
let limited_hits = ref 0

(* The request session as the server holds it: its id and every field. *)
let session_state request =
  let fields =
    Dream.all_session_fields request
    |> List.sort compare
    |> List.map (fun (k, v) -> k ^ "=" ^ v)
  in
  String.concat ";" (("id=" ^ Dream.session_id request) :: fields)

let build_pipeline url =
  Dream.sql_pool ~size:4 url
  @@ Dream.set_secret "b2-test-secret-value"
  @@ Dream.memory_sessions
  @@ Dream.router
       [
         (* Each browser session carries a random marker, so a later look at
            the same cookie shows whether the session survived unchanged. *)
         Dream.get "/token" (fun req ->
             let* () =
               Dream.set_session_field req "b2_marker"
                 (Dream.to_base64url (Dream.random 9))
             in
             Dream.respond (Dream.csrf_token req));
         Dream.get "/b2-session" (fun req -> Dream.respond (session_state req));
         Dream.get "/whoami" (fun req ->
             match Dream.session_field req "user_id" with
             | Some uid -> Dream.respond ("uid:" ^ uid)
             | None -> Dream.respond "anon");
         Dream.post "/signup" (fun req ->
             Earde.Auth_handlers.make_signup_handler ~mail:(mail ()) req);
         Dream.post "/forgot-password" (fun req ->
             Earde.Auth_handlers.make_forgot_password_handler ~mail:(mail ())
               req);
         Dream.post "/login"
           (Earde.Auth_handlers.make_login_handler ~verify:counting_verifier);
         Dream.post "/login-prod" Earde.Auth_handlers.login_handler;
         Dream.get "/confirm-email" Earde.Auth_handlers.confirm_email_handler;
         Dream.post "/reset-password" Earde.Auth_handlers.reset_password_handler;
         (* The production limiter in front of real handlers, at paths whose
            buckets this suite owns. *)
         Dream.post "/b2-rl/login"
           (Earde.Rate_limit_middleware.middleware (fun req ->
                incr limited_hits;
                Earde.Auth_handlers.make_login_handler ~verify:counting_verifier
                  req));
         Dream.post "/b2-rl/forgot"
           (Earde.Rate_limit_middleware.middleware (fun req ->
                incr limited_hits;
                Earde.Auth_handlers.make_forgot_password_handler ~mail:(mail ())
                  req));
         Dream.post "/b2-rl/signup"
           (Earde.Rate_limit_middleware.middleware (fun req ->
                incr limited_hits;
                Earde.Auth_handlers.make_signup_handler ~mail:(mail ()) req));
       ]

let pipeline url =
  match Hashtbl.find_opt pipelines url with
  | Some p -> p
  | None ->
      let p = build_pipeline url in
      Hashtbl.replace pipelines url p;
      p

let with_search_path url schema =
  Uri.to_string
    (Uri.add_query_param' (Uri.of_string url)
       ("options", "-csearch_path=" ^ schema))

type reply = {
  status : int;
  body : string;
  headers : (string * string) list;
  sent_cookie : string option; (* the request's own session cookie *)
  at : float; (* when the response was captured *)
  session_before : string;
      (* [session_state] of the request's session, "" if unobserved *)
  session_after : string;
      (* the same cookie looked up again after the response *)
}

let session_cookie response =
  match
    List.find_opt
      (fun v -> contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | Some v -> (
      match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)
  | None -> Alcotest.fail "no session cookie"

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let observe_session ~url cookie =
  let* r =
    (pipeline url)
      (Dream.request ~method_:`GET ~target:"/b2-session"
         ~headers:[ ("Cookie", cookie) ]
         "")
  in
  Dream.body r

(* Every POST comes from a fresh anonymous browser session with its own valid
   CSRF token, like a first visit to the form. The session is looked at
   before and after, through the same cookie. *)
let post ~url ~target fields =
  let p = pipeline url in
  let* minted = p (Dream.request ~method_:`GET ~target:"/token" "") in
  let cookie = session_cookie minted in
  let* token = Dream.body minted in
  let* session_before = observe_session ~url cookie in
  let request =
    Dream.request ~method_:`POST ~target
      ~headers:
        [
          ("Content-Type", "application/x-www-form-urlencoded");
          ("Cookie", cookie);
        ]
      ""
  in
  Dream.set_body request (form_body (("dream.csrf", token) :: fields));
  let* response = p request in
  let at = Unix.gettimeofday () in
  let* body = Dream.body response in
  let* session_after = observe_session ~url cookie in
  Lwt.return
    ( {
        status = Dream.status_to_int (Dream.status response);
        body;
        headers = Dream.all_headers response;
        sent_cookie = Some cookie;
        at;
        session_before;
        session_after;
      },
      cookie )

let get ~url ?cookie target =
  let p = pipeline url in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = p (Dream.request ~method_:`GET ~target ~headers "") in
  let at = Unix.gettimeofday () in
  let* body = Dream.body response in
  Lwt.return
    {
      status = Dream.status_to_int (Dream.status response);
      body;
      headers = Dream.all_headers response;
      sent_cookie = cookie;
      at;
      session_before = "";
      session_after = "";
    }

(* === the response comparator === *)

(* CSRF values in bodies are per-session random by design. *)
let mask_csrf body =
  let key = "dream.csrf" in
  let b = Buffer.create (String.length body) in
  let n = String.length body in
  let rec go i =
    if i >= n then ()
    else if
      i + String.length key <= n && String.sub body i (String.length key) = key
    then begin
      Buffer.add_string b key;
      let j = i + String.length key in
      (* Mask the next value="..." or value='...' within a short window. *)
      let rec find k =
        if k + 7 > n || k > j + 80 then None
        else if
          String.sub body k 6 = "value="
          && (body.[k + 6] = '"' || body.[k + 6] = '\'')
        then Some k
        else find (k + 1)
      in
      match find j with
      | None -> go j
      | Some k ->
          let q = body.[k + 6] in
          let close =
            try String.index_from body (k + 7) q with Not_found -> n - 1
          in
          Buffer.add_string b (String.sub body j (k + 7 - j));
          Buffer.add_string b "MASKED";
          go close
    end
    else begin
      Buffer.add_char b body.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

(* Cookies whose values are random by design: Dream's signed session id.
   Only these values are masked, and only down to their session meaning
   (cleared, reused, rotated); every other cookie value is compared as is. *)
let random_cookie_names = [ "dream.session" ]

type cookie_view = {
  name : string;
  value : string;
  attributes : (string * string) list; (* lower-cased names, in order *)
  expires : float option; (* absolute time of a parseable Expires *)
  max_age : int option; (* a parseable Max-Age, compared by meaning *)
}

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

let weekdays = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

(* IMF-fixdate ("Wed, 21 Oct 2015 07:28:00 GMT") to Unix time, by the
   days-from-civil algorithm: independent of the local time zone. *)
let parse_http_date s =
  try
    Scanf.sscanf s "%_s@, %d %s %d %d:%d:%d GMT%!" (fun day mon year h mi se ->
        let rec index i = if months.(i) = mon then i + 1 else index (i + 1) in
        let m = index 0 in
        let y = if m <= 2 then year - 1 else year in
        let era = (if y >= 0 then y else y - 399) / 400 in
        let yoe = y - (era * 400) in
        let mp = (m + 9) mod 12 in
        let doy = (((153 * mp) + 2) / 5) + day - 1 in
        let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
        let days = (era * 146097) + doe - 719468 in
        Some (float_of_int ((days * 86400) + (h * 3600) + (mi * 60) + se)))
  with _ -> None

let http_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT"
    weekdays.(tm.Unix.tm_wday) tm.Unix.tm_mday months.(tm.Unix.tm_mon)
    (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* One Set-Cookie header. Each cookie arrives as its own header, so an
   Expires date's comma never splits anything; attributes split on ';'. *)
let cookie_view ~sent_cookie raw =
  let parts = String.split_on_char ';' raw |> List.map String.trim in
  let nv, attrs =
    match parts with nv :: attrs -> (nv, attrs) | [] -> ("", [])
  in
  let name, value =
    match String.index_opt nv '=' with
    | Some i ->
        (String.sub nv 0 i, String.sub nv (i + 1) (String.length nv - i - 1))
    | None -> (nv, "")
  in
  let expires = ref None and max_age = ref None in
  let attributes =
    List.map
      (fun a ->
        let k, v =
          match String.index_opt a '=' with
          | Some i ->
              ( String.lowercase_ascii (String.sub a 0 i),
                String.sub a (i + 1) (String.length a - i - 1) )
          | None -> (String.lowercase_ascii a, "")
        in
        match (k, parse_http_date v, int_of_string_opt v) with
        | "expires", Some t, _ ->
            expires := Some t;
            (k, "<date>")
        | "max-age", _, Some n ->
            max_age := Some n;
            (k, "<seconds>")
        | _ -> (k, v))
      attrs
  in
  let value =
    if not (List.mem name random_cookie_names) then value
    else if value = "" then "<cleared>"
    else if Some (name ^ "=" ^ value) = sent_cookie then "<reused>"
    else "<rotated>"
  in
  { name; value; attributes; expires = !expires; max_age = !max_age }

(* A pair of responses is captured seconds apart, HTTP dates have 1 s
   resolution, and Dream derives a live session's Max-Age from its
   remaining lifetime; a real lifetime difference is far larger. Expired or
   clearing (Max-Age <= 0) versus live is never tolerated. *)
let expires_tolerance = 5.0

let set_cookies r =
  List.filter_map
    (fun (k, v) ->
      if String.lowercase_ascii k = "set-cookie" then Some v else None)
    r.headers

let cookie_views r =
  List.map (cookie_view ~sent_cookie:r.sent_cookie) (set_cookies r)

let cookie_diff (ra, a) (rb, b) =
  if a.name <> b.name then Some (Printf.sprintf "cookie %s vs %s" a.name b.name)
  else if a.value <> b.value then
    Some (Printf.sprintf "cookie %s: value %s vs %s" a.name a.value b.value)
  else if a.attributes <> b.attributes then
    Some (Printf.sprintf "cookie %s: attributes differ" a.name)
  else
    match (a.max_age, b.max_age) with
    | Some ma, Some mb when ma > 0 <> (mb > 0) ->
        Some (Printf.sprintf "cookie %s: cleared vs live Max-Age" a.name)
    | Some ma, Some mb
      when ma > 0 && Float.abs (float_of_int (ma - mb)) > expires_tolerance ->
        Some (Printf.sprintf "cookie %s: Max-Age %d vs %d" a.name ma mb)
    | _ -> (
        match (a.expires, b.expires) with
        | None, None -> None
        | Some ea, Some eb ->
            let live_a = ea > ra.at and live_b = eb > rb.at in
            if live_a <> live_b then
              Some (Printf.sprintf "cookie %s: expired vs live" a.name)
            else if
              live_a
              && Float.abs (ea -. ra.at -. (eb -. rb.at)) > expires_tolerance
            then Some (Printf.sprintf "cookie %s: lifetimes differ" a.name)
            else None
        | _ ->
            Some (Printf.sprintf "cookie %s: Expires on one side only" a.name))

(* What the request did to its own session, seen through its cookie. *)
let session_effect r =
  if r.session_before = "" then "unobserved"
  else
    let id s = List.hd (String.split_on_char ';' s) in
    let authority =
      List.exists
        (fun k -> contains r.session_after (k ^ "="))
        [ "user_id"; "username"; "is_admin" ]
    in
    Printf.sprintf "%s%s"
      (if r.session_after = r.session_before then "unchanged"
       else if id r.session_after = id r.session_before then "modified"
       else "replaced")
      (if authority then "+authority" else "")

let other_headers r =
  List.filter_map
    (fun (k, v) ->
      let k = String.lowercase_ascii k in
      if k = "set-cookie" then None else Some (k, v))
    r.headers
  |> List.sort compare

let reply_diff a b =
  if a.status <> b.status then
    Some (Printf.sprintf "status %d vs %d" a.status b.status)
  else if other_headers a <> other_headers b then Some "headers differ"
  else
    let ca = cookie_views a and cb = cookie_views b in
    if List.length ca <> List.length cb then
      Some
        (Printf.sprintf "%d vs %d Set-Cookie headers" (List.length ca)
           (List.length cb))
    else
      match
        List.find_map
          (fun (x, y) -> cookie_diff (a, x) (b, y))
          (List.combine ca cb)
      with
      | Some d -> Some d
      | None ->
          if session_effect a <> session_effect b then
            Some
              (Printf.sprintf "session %s vs %s" (session_effect a)
                 (session_effect b))
          else if mask_csrf a.body <> mask_csrf b.body then Some "bodies differ"
          else None

let same_shape label a b =
  match reply_diff a b with
  | None -> ()
  | Some d -> Alcotest.failf "%s: %s" label d

(* A private-equivalent outcome must leave the browser exactly as it was:
   no cookie, the same session (id and fields), no identity or authority. *)
let private_session_preserved label r =
  (match set_cookies r with
  | [] -> ()
  | _ -> Alcotest.failf "%s: sets a cookie" label);
  if r.session_before = "" then Alcotest.failf "%s: session not observed" label;
  Alcotest.(check string)
    (label ^ ": request session reused unchanged")
    r.session_before r.session_after;
  Alcotest.(check string)
    (label ^ ": session effect")
    "unchanged" (session_effect r)

(* The comparator's own controls, on synthetic responses: what must count
   as different, and the legitimately random values that must not. *)
let comparator_case =
  pure_case
    "comparator: attribute-only, lifetime, count and session differences are \
     rejected; random session values and date jitter are not" (fun () ->
      let at = 1_800_000_000.0 in
      let base =
        {
          status = 200;
          body = "b";
          headers = [ ("Content-Type", "text/html") ];
          sent_cookie = Some "dream.session=REQ";
          at;
          session_before = "id=s1;b2_marker=m";
          session_after = "id=s1;b2_marker=m";
        }
      in
      let with_cookies cs =
        {
          base with
          headers = base.headers @ List.map (fun c -> ("Set-Cookie", c)) cs;
        }
      in
      let std = "; Max-Age=1209599; Path=/; HttpOnly; SameSite=Lax" in
      let session v rest = "dream.session=" ^ v ^ rest in
      let equal label a b =
        match reply_diff (with_cookies a) (with_cookies b) with
        | None -> ()
        | Some d -> Alcotest.failf "%s: wrongly rejected (%s)" label d
      in
      let differ label a b =
        match reply_diff (with_cookies a) (with_cookies b) with
        | Some _ -> ()
        | None -> Alcotest.failf "%s: difference not detected" label
      in
      equal "rotated session ids are random"
        [ session "AAA" std ]
        [ session "BBB" std ];
      equal "no cookies" [] [];
      (* The mutant the earlier name-only reduction missed. *)
      let names r = List.map (fun c -> c.name) (cookie_views r) in
      let m0 = with_cookies [ session "AAA" "; Max-Age=0; Path=/" ] in
      let m1 = with_cookies [ session "BBB" "; Max-Age=3600; Path=/" ] in
      Alcotest.(check bool)
        "control: names alone cannot tell them apart" true
        (names m0 = names m1);
      Alcotest.(check bool)
        "Max-Age only: rejected" true
        (reply_diff m0 m1 <> None);
      differ "Max-Age cleared vs short-lived"
        [ session "A" "; Max-Age=0" ]
        [ session "B" "; Max-Age=5" ];
      differ "Max-Age lifetimes"
        [ session "A" "; Max-Age=3600" ]
        [ session "B" "; Max-Age=60" ];
      equal "Max-Age jitter of a live session"
        [ session "A" "; Max-Age=1209599" ]
        [ session "B" "; Max-Age=1209600" ];
      differ "Path" [ session "A" "; Path=/" ] [ session "B" "; Path=/x" ];
      differ "HttpOnly dropped"
        [ session "A" std ]
        [ session "B" "; Max-Age=1209599; Path=/; SameSite=Lax" ];
      differ "SameSite"
        [ session "A" std ]
        [ session "B" "; Max-Age=1209599; Path=/; HttpOnly; SameSite=Strict" ];
      differ "Secure added"
        [ session "A" std ]
        [ session "B" (std ^ "; Secure") ];
      differ "Domain added"
        [ session "A" std ]
        [ session "B" (std ^ "; Domain=earde.invalid") ];
      differ "reused vs rotated" [ session "REQ" std ] [ session "B" std ];
      differ "cleared vs set"
        [ session "" "; Max-Age=0; Path=/" ]
        [ session "B" "; Max-Age=0; Path=/" ];
      differ "one cookie vs two"
        [ session "A" std ]
        [ session "B" std; "other=1; Path=/" ];
      differ "a non-random value is literal" [ "consent=yes; Path=/" ]
        [ "consent=no; Path=/" ];
      let expires t = "; Path=/; Expires=" ^ http_date t in
      Alcotest.(check (option (float 0.0)))
        "an Expires date parses, comma and all"
        (Some (at +. 3600.0))
        (List.hd
           (cookie_views
              (with_cookies [ session "A" (expires (at +. 3600.0)) ])))
          .expires;
      Alcotest.(check int)
        "a dated cookie stays one cookie" 1
        (List.length
           (cookie_views
              (with_cookies [ session "A" (expires (at +. 3600.0)) ])));
      differ "expired vs live"
        [ session "A" (expires 0.0) ]
        [ session "B" (expires (at +. 3600.0)) ];
      differ "different lifetimes"
        [ session "A" (expires (at +. 3600.0)) ]
        [ session "B" (expires (at +. 60.0)) ];
      differ "Expires on one side"
        [ session "A" (expires (at +. 3600.0)) ]
        [ session "B" "; Path=/" ];
      equal "the same lifetime, clock jitter"
        [ session "A" (expires (at +. 3600.0)) ]
        [ session "B" (expires (at +. 3602.0)) ];
      equal "two expired clearings"
        [ session "" (expires 0.0) ]
        [ session "" (expires 86400.0) ];
      (match
         reply_diff base { base with session_after = "id=s2;b2_marker=m" }
       with
      | Some _ -> ()
      | None -> Alcotest.fail "a replaced session was not detected");
      (match
         reply_diff base
           { base with session_after = "id=s1;b2_marker=m;user_id=7" }
       with
      | Some _ -> ()
      | None -> Alcotest.fail "an authenticated session was not detected");
      Lwt.return_unit)

let comparator_suite = [ comparator_case ]

let signup ~url ?(target = "/signup") ~username ~email
    ?(password = "b2 long password") () =
  Lwt.map fst
    (post ~url ~target
       [ ("username", username); ("email", email); ("password", password) ])

let forgot ~url ?(target = "/forgot-password") email =
  Lwt.map fst (post ~url ~target [ ("email", email) ])

let pending_rows (module C : Caqti_lwt.CONNECTION) =
  let* r = C.collect_list q_pending_rows () in
  or_fail "pending rows" r

let find_one (module C : Caqti_lwt.CONNECTION) q v label =
  let* r = C.find q v in
  or_fail label r

(* Apostrophes are HTML-escaped in the rendered page: match around them. *)
let neutral_signup = "email you a confirmation link. Click it within 24 hours"
let username_taken = "That username is already taken."
let neutral_reset = "If an account with that email exists, we"

(* ------------------------------------------------------------------ *)
(* Login                                                                 *)
(* ------------------------------------------------------------------ *)

let login ~url ?(target = "/login") identifier password =
  post ~url ~target [ ("identifier", identifier); ("password", password) ]

let login_equivalence_case =
  db_case
    "login: a missing account and a wrong password get the same response after \
     one full verification each; the dummy password never logs in"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = real_hash "b2 correct horse" in
      let* _uid =
        find_one c q_user ("b2x_login", "login@b2.invalid", hash) "user"
      in
      verified_hashes := [];
      let* wrong, wrong_cookie = login ~url "b2x_login" "b2 wrong password" in
      Alcotest.(check (list string))
        "wrong password: own hash verified" [ hash ] !verified_hashes;
      verified_hashes := [];
      let* missing, missing_cookie =
        login ~url "b2x_nobody" "b2 wrong password"
      in
      Alcotest.(check (list string))
        "missing account: dummy verified" [ LV.dummy_hash ] !verified_hashes;
      same_shape "miss vs wrong" missing wrong;
      must "generic failure" wrong.body "Invalid username or password.";
      verified_hashes := [];
      let* dummy, _ = login ~url "b2x_nobody" LV.dummy_password in
      Alcotest.(check (list string))
        "dummy password: dummy verified" [ LV.dummy_hash ] !verified_hashes;
      same_shape "dummy password vs wrong" dummy wrong;
      List.iter
        (fun (l, r) -> private_session_preserved l r)
        [
          ("wrong password", wrong);
          ("missing account", missing);
          ("dummy password", dummy);
        ];
      (* The production-wired handler too: its verifier is the real Argon2. *)
      let* prod_dummy, prod_cookie =
        login ~url ~target:"/login-prod" "b2x_nobody" LV.dummy_password
      in
      must "production handler refuses the dummy password" prod_dummy.body
        "Invalid username or password.";
      let* who = get ~url ~cookie:prod_cookie "/whoami" in
      Alcotest.(check string) "no session identity" "anon" who.body;
      let* who = get ~url ~cookie:wrong_cookie "/whoami" in
      Alcotest.(check string) "wrong password: anon" "anon" who.body;
      let* who = get ~url ~cookie:missing_cookie "/whoami" in
      Alcotest.(check string) "missing: anon" "anon" who.body;
      Lwt.return_unit)

let login_success_and_ban_case =
  db_case
    "login: valid credentials rotate into a fresh session; a ban is disclosed \
     only after valid credentials" (fun ~url c ->
      let* hash = real_hash "b2 correct horse" in
      let* uid = find_one c q_user ("b2x_ok", "ok@b2.invalid", hash) "user" in
      let* banned_hash = real_hash "b2 banned pass" in
      let* banned =
        find_one c q_user
          ("b2x_banned", "banned@b2.invalid", banned_hash)
          "banned"
      in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* r = C.exec q_ban banned in
      let* () = or_fail "ban" r in
      verified_hashes := [];
      let* ok, pre_cookie = login ~url "b2x_ok" "b2 correct horse" in
      Alcotest.(check int) "redirect" 303 ok.status;
      Alcotest.(check (list string))
        "one verification" [ hash ] !verified_hashes;
      let rotated =
        match List.assoc_opt "Set-Cookie" ok.headers with
        | Some v -> (
            match String.index_opt v ';' with
            | Some i -> String.sub v 0 i
            | None -> v)
        | None -> Alcotest.fail "no rotated session"
      in
      Alcotest.(check bool) "session id rotated" true (rotated <> pre_cookie);
      (match cookie_views ok with
      | [ c ] -> (
          Alcotest.(check string) "rotated, not reused" "<rotated>" c.value;
          Alcotest.(check (list (pair string string)))
            "session cookie attributes"
            [
              ("max-age", "<seconds>");
              ("path", "/");
              ("httponly", "");
              ("samesite", "Lax");
            ]
            c.attributes;
          match c.max_age with
          | Some n when n > 1_209_590 && n <= 1_209_600 -> ()
          | _ ->
              Alcotest.fail "session cookie lifetime is not the 14-day session")
      | cs -> Alcotest.failf "login success sets %d cookies" (List.length cs));
      Alcotest.(check string)
        "the pre-login session is replaced" "replaced" (session_effect ok);
      let* who = get ~url ~cookie:rotated "/whoami" in
      Alcotest.(check string)
        "authenticated"
        ("uid:" ^ string_of_int uid)
        who.body;
      let* who = get ~url ~cookie:pre_cookie "/whoami" in
      Alcotest.(check string)
        "pre-login session holds no identity" "anon" who.body;
      (* Also by email identifier. *)
      let* by_email, _ = login ~url "ok@b2.invalid" "b2 correct horse" in
      Alcotest.(check int) "email identifier" 303 by_email.status;
      let* banned_ok, banned_cookie =
        login ~url "b2x_banned" "b2 banned pass"
      in
      must "ban disclosed after valid credentials" banned_ok.body
        "Account Banned";
      let* who = get ~url ~cookie:banned_cookie "/whoami" in
      Alcotest.(check string) "banned: no session" "anon" who.body;
      let* banned_wrong, _ = login ~url "b2x_banned" "b2 wrong password" in
      let* missing, _ = login ~url "b2x_nobody" "b2 wrong password" in
      must_not "ban hidden on wrong password" banned_wrong.body "Banned";
      same_shape "banned+wrong vs missing" banned_wrong missing;
      Lwt.return_unit)

let login_db_suite = [ login_equivalence_case; login_success_and_ban_case ]

(* ------------------------------------------------------------------ *)
(* Signup privacy                                                        *)
(* ------------------------------------------------------------------ *)

let holder_old_token = "b2-holder-old-token"
let holder_old_hash = sha256 holder_old_token

let signup_matrix_case =
  db_case
    "signup: username feedback depends on the username alone; every private \
     email/reservation state gets the neutral response and only legitimate \
     submissions mint a confirmation" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* owner_hash = real_hash "b2 owner password" in
      let* _ =
        find_one c q_user ("b2x_owner", "owner@b2.invalid", owner_hash) "owner"
      in
      let exec q v label =
        let* r = C.exec q v in
        or_fail label r
      in
      let* () =
        exec q_pending
          ("b2x_reserved", "holder@b2.invalid", holder_old_hash, 24.0)
          "reserved"
      in
      let* () =
        exec q_pending
          ("b2x_mine", "mine@b2.invalid", "mine-old-hash", 24.0)
          "own"
      in
      let* () =
        exec q_pending
          ("b2x_stale", "gone@b2.invalid", "stale-old-hash", -1.0)
          "expired"
      in
      let* users_before = find_one c q_users_snapshot () "users before" in
      (* A real, confirmable token for the holder's reservation, so its later
         death proves the retry replaced it. *)
      let* live =
        find_one c q_pending_token_exists holder_old_hash "holder token"
      in
      Alcotest.(check bool) "control: the holder's old token is live" true live;
      with_captured_logs (fun logs ->
          (* --- public: a real account's username --- *)
          let* t_new =
            signup ~url ~username:"b2x_owner" ~email:"fresh1@b2.invalid" ()
          in
          let* t_reg =
            signup ~url ~username:"b2x_owner" ~email:"owner@b2.invalid" ()
          in
          let* t_pend =
            signup ~url ~username:"b2x_owner" ~email:"mine@b2.invalid" ()
          in
          must "taken is explicit" t_new.body username_taken;
          same_shape "taken: registered email" t_reg t_new;
          same_shape "taken: pending email" t_pend t_new;
          Alcotest.(check int) "taken: no mail" 0 (List.length fm.started);
          (* --- private-equivalent class --- *)
          let* n_new =
            signup ~url ~username:"b2x_new" ~email:"new@b2.invalid" ()
          in
          let* n_reg =
            signup ~url ~username:"b2x_regmail" ~email:"owner@b2.invalid" ()
          in
          let* n_foreign =
            signup ~url ~username:"b2x_reserved" ~email:"intruder@b2.invalid" ()
          in
          let* n_foreign_reg =
            signup ~url ~username:"b2x_reserved" ~email:"owner@b2.invalid" ()
          in
          let* n_holder =
            signup ~url ~username:"b2x_reserved" ~email:"holder@b2.invalid" ()
          in
          let* n_mine_again =
            signup ~url ~username:"b2x_mine2" ~email:"mine@b2.invalid" ()
          in
          let* n_stale =
            signup ~url ~username:"b2x_stale" ~email:"late@b2.invalid" ()
          in
          must "neutral" n_new.body neutral_signup;
          must_not "no delivery claim" n_new.body "we've sent";
          List.iter
            (fun (label, r) -> same_shape label r n_new)
            [
              ("registered email", n_reg);
              ("foreign reservation", n_foreign);
              ("foreign reservation + registered email", n_foreign_reg);
              ("holder retry", n_holder);
              ("same email, new username", n_mine_again);
              ("expired reservation", n_stale);
            ];
          List.iter
            (fun (label, r) -> private_session_preserved label r)
            [
              ("new email", n_new);
              ("registered email", n_reg);
              ("foreign reservation", n_foreign);
              ("holder retry", n_holder);
              ("expired reservation", n_stale);
            ];
          (* Every entry, real or no-send, runs through its slot. *)
          let* () = drained fm d in
          (* --- durable state --- *)
          let* rows = pending_rows c in
          let row_for name = List.find_opt (fun (u, _, _) -> u = name) rows in
          let email_of name = Option.map (fun (_, e, _) -> e) (row_for name) in
          let hash_of name = Option.map (fun (_, _, h) -> h) (row_for name) in
          Alcotest.(check (option string))
            "new reservation" (Some "new@b2.invalid") (email_of "b2x_new");
          Alcotest.(check (option string))
            "no reservation for a registered email" None
            (email_of "b2x_regmail");
          Alcotest.(check (option string))
            "foreign reservation kept by its holder" (Some "holder@b2.invalid")
            (email_of "b2x_reserved");
          Alcotest.(check bool)
            "holder retry replaced the token" true
            (hash_of "b2x_reserved" <> Some holder_old_hash);
          Alcotest.(check (option string))
            "own earlier signup replaced" None (email_of "b2x_mine");
          Alcotest.(check (option string))
            "resubmission recorded" (Some "mine@b2.invalid")
            (email_of "b2x_mine2");
          Alcotest.(check (option string))
            "expired squatter replaced" (Some "late@b2.invalid")
            (email_of "b2x_stale");
          Alcotest.(check bool)
            "no pending row names the registered email" false
            (List.exists (fun (_, e, _) -> e = "owner@b2.invalid") rows);
          let* users_after = find_one c q_users_snapshot () "users after" in
          Alcotest.(check string)
            "existing accounts untouched, none created" users_before users_after;
          (* --- mail: only the four legitimate submissions, to their own addresses --- *)
          let recipients =
            List.map Earde.Email.recipient fm.delivered |> List.sort compare
          in
          Alcotest.(check (list string))
            "confirmations"
            [
              "holder@b2.invalid";
              "late@b2.invalid";
              "mine@b2.invalid";
              "new@b2.invalid";
            ]
            recipients;
          List.iter
            (fun m ->
              let t = token_of_message m in
              match
                List.find_opt
                  (fun (_, e, _) -> e = Earde.Email.recipient m)
                  rows
              with
              | Some (_, _, h) ->
                  Alcotest.(check string)
                    "mail token matches its own row" (sha256 t) h
              | None -> Alcotest.fail "mail for a row that does not exist")
            fm.delivered;
          (* --- confirmation authority --- *)
          let msg_to e =
            List.find (fun m -> Earde.Email.recipient m = e) fm.delivered
          in
          let* ok =
            get ~url
              ("/confirm-email?token="
              ^ token_of_message (msg_to "new@b2.invalid"))
          in
          must "confirmed" ok.body "Email Confirmed!";
          let* email = C.find_opt q_user_email "b2x_new" in
          let* email = or_fail "new user" email in
          Alcotest.(check (option string))
            "account created for its own email" (Some "new@b2.invalid") email;
          let* who = get ~url "/whoami" in
          Alcotest.(check string) "no auto-login" "anon" who.body;
          let* still = C.find q_pending_token_exists holder_old_hash in
          let* still = or_fail "old holder token" still in
          Alcotest.(check bool) "the holder's old token row is gone" false still;
          let* old = get ~url ("/confirm-email?token=" ^ holder_old_token) in
          must "superseded holder token is dead" old.body "Confirmation Failed";
          let* holder =
            get ~url
              ("/confirm-email?token="
              ^ token_of_message (msg_to "holder@b2.invalid"))
          in
          must "holder's fresh token confirms" holder.body "Email Confirmed!";
          let* email = C.find_opt q_user_email "b2x_reserved" in
          let* email = or_fail "holder user" email in
          Alcotest.(check (option string))
            "reservation goes to its holder" (Some "holder@b2.invalid") email;
          (* --- nothing credential-bearing persisted or logged --- *)
          let* json = find_one c q_pending_json () "pending json" in
          must_not "no raw password in pending rows" json "b2 long password";
          List.iter
            (fun m ->
              must_not "no raw token in pending rows" json (token_of_message m))
            fm.delivered;
          let text = logs () in
          must_not "no raw password in logs" text "b2 long password";
          List.iter
            (fun m ->
              must_not "no token in logs" text (token_of_message m);
              must_not "no recipient in logs" text (Earde.Email.recipient m))
            fm.delivered;
          Lwt.return_unit))

(* A concurrent submission that commits the same username or email while
   this one is in flight: the insert finds the row the moment the other
   transaction commits, so it writes nothing and mails nothing. *)
let race_case =
  db_case
    "signup: losing a uniqueness race (same username or same email) is the \
     neutral response with no row and no mail" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* n_ref =
        signup ~url ~username:"b2x_refname" ~email:"ref@b2.invalid" ()
      in
      let delivered_before = List.length fm.delivered in
      let race ~held_username ~held_email ~held_hash ~username ~email =
        let* r = C.start () in
        let* () = or_fail "start" r in
        let* r =
          C.exec q_pending (held_username, held_email, held_hash, 24.0)
        in
        let* () = or_fail "held insert" r in
        let request = signup ~url ~username ~email () in
        let rec wait_blocked n =
          let* w = C.find q_lock_waiters () in
          let* w = or_fail "lock waiters" w in
          if w > 0 then Lwt.return_unit
          else if n > 400 then
            Alcotest.fail "submission never reached the contested insert"
          else Lwt.bind (Lwt_unix.sleep 0.025) (fun () -> wait_blocked (n + 1))
        in
        let* () = wait_blocked 0 in
        let* r = C.commit () in
        let* () = or_fail "commit winner" r in
        request
      in
      let* by_name =
        race ~held_username:"b2x_contested" ~held_email:"winner@b2.invalid"
          ~held_hash:"race-winner-hash-1" ~username:"b2x_contested"
          ~email:"loser@b2.invalid"
      in
      same_shape "username race" by_name n_ref;
      let* by_email =
        race ~held_username:"b2x_winner2" ~held_email:"contested@b2.invalid"
          ~held_hash:"race-winner-hash-2" ~username:"b2x_loser2"
          ~email:"contested@b2.invalid"
      in
      same_shape "email race" by_email n_ref;
      let* rows = pending_rows c in
      Alcotest.(check bool)
        "losers wrote nothing" false
        (List.exists
           (fun (u, e, _) -> e = "loser@b2.invalid" || u = "b2x_loser2")
           rows);
      Alcotest.(check bool)
        "winners intact" true
        (List.exists
           (fun (u, _, h) -> u = "b2x_contested" && h = "race-winner-hash-1")
           rows
        && List.exists
             (fun (u, _, h) -> u = "b2x_winner2" && h = "race-winner-hash-2")
             rows);
      let* () = drained fm d in
      Alcotest.(check int)
        "no mail for a lost race" delivered_before (List.length fm.delivered);
      Lwt.return_unit)

let signup_db_suite = [ signup_matrix_case; race_case ]

(* ------------------------------------------------------------------ *)
(* Asynchronous, bounded, durable-first mail                             *)
(* ------------------------------------------------------------------ *)

(* The request must answer while the provider is still stalled. The 5 s
   pick is only a safety net so an awaiting implementation fails instead of
   hanging the suite; the assertion is on ordering, not on elapsed time. *)
let answers_before_provider label request =
  let* r =
    Lwt.pick
      [
        Lwt.map (fun r -> `Answered r) request;
        Lwt.map (fun () -> `Stalled) (Lwt_unix.sleep 5.0);
      ]
  in
  match r with
  | `Answered r -> Lwt.return r
  | `Stalled ->
      Alcotest.failf "%s: response waited for the stalled provider" label

let stalled_provider_case =
  db_case
    "mail: signup and reset requests (known and unknown) answer while the \
     provider is stalled; releasing it then delivers working links"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let gate, release = Lwt.wait () in
      let fm, d = fake_dispatcher ~gate () in
      current_mail := Some d;
      let* hash = real_hash "b2 before reset" in
      let* uid =
        find_one c q_user ("b2x_resetme", "resetme@b2.invalid", hash) "user"
      in
      ignore uid;
      let* _ =
        find_one c q_user ("b2x_taken", "taken@b2.invalid", hash) "registered"
      in
      let* s_new =
        answers_before_provider "signup new"
          (signup ~url ~username:"b2x_async" ~email:"async@b2.invalid" ())
      in
      let* s_reg =
        answers_before_provider "signup registered"
          (signup ~url ~username:"b2x_async2" ~email:"taken@b2.invalid" ())
      in
      let* r_known =
        answers_before_provider "reset known" (forgot ~url "resetme@b2.invalid")
      in
      let* r_unknown =
        answers_before_provider "reset unknown"
          (forgot ~url "nobody@b2.invalid")
      in
      same_shape "signup registered vs new" s_reg s_new;
      same_shape "reset unknown vs known" r_unknown r_known;
      must "reset neutral" r_known.body neutral_reset;
      List.iter
        (fun (l, r) -> private_session_preserved l r)
        [
          ("signup new", s_new);
          ("signup registered", s_reg);
          ("reset known", r_known);
          ("reset unknown", r_unknown);
        ];
      (* Two slots: the new signup's mail (stalled) and the registered one's
         no-send entry. The reset entries wait for the next deadline. *)
      Alcotest.(check int)
        "provider entered for the first real job" 1 (List.length fm.started);
      check_stats "four admitted" d ~outstanding:4 ~queued:2 ~running:2;
      Alcotest.(check int) "nothing delivered yet" 0 (List.length fm.delivered);
      Lwt.wakeup release ();
      let* () = settle 20 in
      Alcotest.(check int)
        "delivered once released" 1 (List.length fm.delivered);
      check_stats "delivery does not end its slot" d ~outstanding:4 ~queued:2
        ~running:2;
      let* () = drained fm d in
      Alcotest.(check (list string))
        "both delivered, on the slot schedule"
        [ "async@b2.invalid"; "resetme@b2.invalid" ]
        (List.map Earde.Email.recipient fm.delivered |> List.sort compare);
      let find kind_path =
        List.find
          (fun m -> contains (Earde.Email.link m) kind_path)
          fm.delivered
      in
      let* ok =
        get ~url
          ("/confirm-email?token=" ^ token_of_message (find "/confirm-email"))
      in
      must "confirmation link works" ok.body "Email Confirmed!";
      let* reset, _ =
        post ~url ~target:"/reset-password"
          [
            ("token", token_of_message (find "/reset-password"));
            ("password", "b2 after reset");
            ("confirm_password", "b2 after reset");
          ]
      in
      must "reset link works" reset.body "Password Updated";
      let* logged, _ = login ~url "b2x_resetme" "b2 after reset" in
      Alcotest.(check int) "new password logs in" 303 logged.status;
      Lwt.return_unit)

(* 64 admitted requests are held open; every further signup/reset — whatever
   the account state — gets the same 503 and writes nothing. *)
let overload_case =
  db_case
    "mail: a full dispatcher refuses signup and reset identically across \
     account states, before any write; capacity then recovers" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 overload" in
      let* _ = find_one c q_user ("b2x_full", "full@b2.invalid", hash) "user" in
      let* r =
        C.exec q_pending
          ("b2x_fullpend", "fullpend@b2.invalid", "fullpend-hash", 24.0)
      in
      let* () = or_fail "pending" r in
      let holds =
        List.init 64 (fun _ ->
            let p, u = Lwt.wait () in
            (u, D.admit d (fun () -> p)))
      in
      Alcotest.(check int) "saturated" 64 (D.stats d).D.outstanding;
      let* rows_before = pending_rows c in
      let* resets_before = find_one c q_reset_rows () "resets" in
      let* a =
        signup ~url ~username:"b2x_fullnew" ~email:"fullnew@b2.invalid" ()
      in
      let* b =
        signup ~url ~username:"b2x_fullreg" ~email:"full@b2.invalid" ()
      in
      let* c1 =
        signup ~url ~username:"b2x_fullpend" ~email:"fullpend@b2.invalid" ()
      in
      let* c2 =
        signup ~url ~username:"b2x_fullpend" ~email:"other@b2.invalid" ()
      in
      Alcotest.(check int) "503" 503 a.status;
      must "generic" a.body "Temporarily unavailable";
      List.iter
        (fun (l, r) -> same_shape l r a)
        [
          ("registered email", b);
          ("own reservation", c1);
          ("foreign reservation", c2);
        ];
      let* k = forgot ~url "full@b2.invalid" in
      let* u = forgot ~url "nobody@b2.invalid" in
      Alcotest.(check int) "reset 503" 503 k.status;
      same_shape "reset known vs unknown" u k;
      (* The public username answer does not depend on the dispatcher. *)
      let* t =
        signup ~url ~username:"b2x_full" ~email:"fullnew@b2.invalid" ()
      in
      must "taken still answered" t.body username_taken;
      let* rows_after = pending_rows c in
      let* resets_after = find_one c q_reset_rows () "resets" in
      Alcotest.(check bool) "no pending write" true (rows_before = rows_after);
      Alcotest.(check int) "no reset token" resets_before resets_after;
      Alcotest.(check int) "no job" 0 (List.length fm.started);
      Alcotest.(check int) "still exactly full" 64 (D.stats d).D.outstanding;
      List.iter (fun (u, _) -> Lwt.wakeup u ((), None)) holds;
      let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) holds) in
      (* Settling without mail frees nothing: each place is held for a full
         slot, exactly as a mail job would hold it. *)
      check_stats "no-mail settlements keep their places" d ~outstanding:64
        ~queued:62 ~running:2;
      let* still = forgot ~url "nobody@b2.invalid" in
      same_shape "still full until a slot ends" still k;
      Vclock.advance_to fm.clock 14.999;
      let* still = forgot ~url "nobody@b2.invalid" in
      same_shape "still full just before the deadline" still k;
      Vclock.advance_to fm.clock 15.0;
      check_stats "two slots ended" d ~outstanding:62 ~queued:60 ~running:2;
      let* again =
        signup ~url ~username:"b2x_fullnew" ~email:"fullnew@b2.invalid" ()
      in
      must "recovered" again.body neutral_signup;
      let* () = drained fm d in
      Alcotest.(check int) "and mails" 1 (List.length fm.delivered);
      Alcotest.(check int) "drained" 0 (D.stats d).D.outstanding;
      Lwt.return_unit)

let durable_first_case =
  db_case
    "mail: the token row is committed before its job runs; storage failures \
     send nothing and answer like success" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let seen = ref [] in
      (* Runs inside the delivery, on a different connection: it sees only
         committed state. *)
      fm.at_start <-
        (fun m ->
          let t = token_of_message m in
          let q =
            if contains (Earde.Email.link m) "/confirm-email" then
              q_pending_token_exists
            else q_reset_token_exists
          in
          let* r = C.find q (sha256 t) in
          let* present = or_fail "durable check" r in
          seen := present :: !seen;
          Lwt.return_unit);
      let* hash = real_hash "b2 durable" in
      let* _ =
        find_one c q_user ("b2x_durable", "durable@b2.invalid", hash) "user"
      in
      let* s_ok =
        signup ~url ~username:"b2x_dnew" ~email:"dnew@b2.invalid" ()
      in
      let* () = drained fm d in
      let* r_ok = forgot ~url "durable@b2.invalid" in
      let* () = drained fm d in
      Alcotest.(check (list bool))
        "both rows committed before delivery" [ true; true ] !seen;
      (* Shadow schema: users resolves, pending_signups and password_resets do
         not — the private write itself fails. *)
      let exec sql =
        let* r = C.exec ((Caqti_type.unit ->. Caqti_type.unit) sql) () in
        or_fail sql r
      in
      let* () = exec "CREATE SCHEMA b2_shadow" in
      let* () =
        exec "CREATE VIEW b2_shadow.users AS SELECT * FROM public.users"
      in
      let shadow = with_search_path url "b2_shadow" in
      let before = List.length fm.started in
      let* s_fail =
        with_captured_logs (fun _ ->
            signup ~url:shadow ~username:"b2x_dfail" ~email:"dfail@b2.invalid"
              ())
      in
      let* r_fail =
        with_captured_logs (fun _ -> forgot ~url:shadow "durable@b2.invalid")
      in
      same_shape "signup storage failure looks like success" s_fail s_ok;
      same_shape "reset storage failure looks like success" r_fail r_ok;
      List.iter
        (must_not "no storage detail" s_fail.body)
        [ "b2_shadow"; "relation"; "pending_signups" ];
      let* () = drained fm d in
      Alcotest.(check int)
        "no mail after a failed write" before (List.length fm.started);
      let* rows = pending_rows c in
      Alcotest.(check bool)
        "no row for the failed signup" false
        (List.exists (fun (u, _, _) -> u = "b2x_dfail") rows);
      (* Reset token rows hold only the hash. *)
      let* json = find_one c q_reset_json () "reset json" in
      List.iter
        (fun m ->
          if contains (Earde.Email.link m) "/reset-password" then
            must_not "no raw reset token stored" json (token_of_message m))
        fm.delivered;
      Lwt.return_unit)

let volatile_resend_case =
  db_case
    "mail: a job lost with the dispatcher is recovered by a fresh request; the \
     superseded signup link dies, the new ones work" (fun ~url c ->
      let lost_gate, _never = Lwt.wait () in
      let lost, lost_d = fake_dispatcher ~gate:lost_gate () in
      current_mail := Some lost_d;
      let* hash = real_hash "b2 volatile" in
      let* _ = find_one c q_user ("b2x_vol", "vol@b2.invalid", hash) "user" in
      let* _ =
        signup ~url ~username:"b2x_volnew" ~email:"volnew@b2.invalid" ()
      in
      let* _ = forgot ~url "vol@b2.invalid" in
      Alcotest.(check int)
        "both stuck in the lost dispatcher" 2 (List.length lost.started);
      Alcotest.(check int) "never delivered" 0 (List.length lost.delivered);
      let lost_signup =
        List.find
          (fun m -> contains (Earde.Email.link m) "/confirm-email")
          lost.started
      in
      (* "Restart": a fresh dispatcher; the user simply asks again. *)
      let fresh, fresh_d = fake_dispatcher () in
      current_mail := Some fresh_d;
      let* again =
        signup ~url ~username:"b2x_volnew" ~email:"volnew@b2.invalid" ()
      in
      must "resend accepted" again.body neutral_signup;
      let* _ = forgot ~url "vol@b2.invalid" in
      Alcotest.(check int)
        "fresh links delivered" 2
        (List.length fresh.delivered);
      let* dead =
        get ~url ("/confirm-email?token=" ^ token_of_message lost_signup)
      in
      must "lost token superseded" dead.body "Confirmation Failed";
      let find p =
        List.find (fun m -> contains (Earde.Email.link m) p) fresh.delivered
      in
      let* ok =
        get ~url
          ("/confirm-email?token=" ^ token_of_message (find "/confirm-email"))
      in
      must "fresh confirmation works" ok.body "Email Confirmed!";
      let* reset, _ =
        post ~url ~target:"/reset-password"
          [
            ("token", token_of_message (find "/reset-password"));
            ("password", "b2 volatile new");
            ("confirm_password", "b2 volatile new");
          ]
      in
      must "fresh reset works" reset.body "Password Updated";
      let* replay, _ =
        post ~url ~target:"/reset-password"
          [
            ("token", token_of_message (find "/reset-password"));
            ("password", "b2 volatile again");
            ("confirm_password", "b2 volatile again");
          ]
      in
      must "reset token single-use" replay.body "Link Expired";
      Lwt.return_unit)

let mail_db_suite =
  [
    stalled_provider_case;
    overload_case;
    durable_first_case;
    volatile_resend_case;
  ]

(* ------------------------------------------------------------------ *)
(* Rate limiter over the real routed boundary                            *)
(* ------------------------------------------------------------------ *)

let limiter_routed_case =
  db_case
    "limiter: a failing enforcement lookup and an unreachable pool refuse with \
     503 and invoke no protected handler; Allowed and Blocked controls still \
     hold" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 limiter" in
      let* _ = find_one c q_user ("b2x_rl", "rl@b2.invalid", hash) "user" in
      let run_all url =
        verified_hashes := [];
        limited_hits := 0;
        let* a, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
        let* b = forgot ~url ~target:"/b2-rl/forgot" "rl@b2.invalid" in
        let* s =
          signup ~url ~target:"/b2-rl/signup" ~username:"b2x_rlsign"
            ~email:"rlsign@b2.invalid" ()
        in
        Lwt.return [ a; b; s ]
      in
      (* 1. Result error: the lookup's relation cannot be resolved. *)
      let* broken = run_all (with_search_path url "b2_void") in
      (* 2. Rejected promise: the pool cannot connect at all. *)
      let* dbs =
        C.find
          ((Caqti_type.unit ->! Caqti_type.int)
             "SELECT COUNT(*)::int FROM pg_database WHERE datname = \
              'earde_b2_absent_db'")
          ()
      in
      let* dbs = or_fail "absent db check" dbs in
      Alcotest.(check int) "the target database really is absent" 0 dbs;
      let absent =
        Uri.to_string (Uri.with_path (Uri.of_string url) "/earde_b2_absent_db")
      in
      let* unreachable = run_all absent in
      List.iter
        (fun r ->
          Alcotest.(check int) "503" 503 r.status;
          must "generic page" r.body "Temporarily unavailable";
          List.iter
            (must_not "no detail" r.body)
            [
              "b2_void";
              "relation";
              "earde_b2_absent_db";
              "Caqti";
              "rate_limits";
            ])
        (broken @ unreachable);
      same_shape "result error vs pool failure" (List.hd broken)
        (List.hd unreachable);
      Alcotest.(check int) "no protected handler ran" 0 !limited_hits;
      Alcotest.(check (list string)) "no verification ran" [] !verified_hashes;
      Alcotest.(check int) "no mail admitted" 0 (D.stats d).D.outstanding;
      Alcotest.(check int) "no mail" 0 (List.length fm.started);
      let* rows = pending_rows c in
      Alcotest.(check bool)
        "no signup write" false
        (List.exists (fun (u, _, _) -> u = "b2x_rlsign") rows);
      let* resets = find_one c q_reset_rows () "resets" in
      Alcotest.(check int) "no reset write" 0 resets;
      (* 3. Controls on the healthy pool: Allowed reaches the handler, and the
         sixth attempt in the window is blocked without reaching it. *)
      limited_hits := 0;
      let* first, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
      Alcotest.(check int) "allowed login proceeds" 303 first.status;
      let rec attempts n =
        if n = 0 then Lwt.return_unit
        else
          let* _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 wrong" in
          attempts (n - 1)
      in
      let* () = attempts 4 in
      Alcotest.(check int) "five allowed" 5 !limited_hits;
      let* sixth, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
      must "blocked" sixth.body "Too Many Attempts";
      Alcotest.(check int) "blocked never reaches the handler" 5 !limited_hits;
      Lwt.return_unit)

let limiter_db_suite = [ limiter_routed_case ]

(* ------------------------------------------------------------------ *)
(* Paired sequences at the capacity edge                                 *)
(* ------------------------------------------------------------------ *)

(* Promoted from the independent review's reproduction. Each pair is two
   arms that start from the same state: 63 of 64 places taken (2 entries in
   service, 56 queued entries, 5 reservations still working), a controlled
   clock at t=0. The queued entries are either all real mail (the review's
   shape) or a mix of real and no-send entries. Then a target request whose private
   outcome differs between the arms (mail vs no mail), then harmless probes
   through the same routed handlers, CSRF and production limiter. Before
   fixed slots, the no-mail arm gave its place back at once, so the next
   probe got 200 there and 503 in the mail arm. Now every public outcome
   and the exact release schedule must match, whatever the provider does. *)

let q_arm_reset =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM rate_limits WHERE endpoint LIKE '/b2-%'";
      "DELETE FROM password_resets WHERE user_id IN (SELECT id FROM users \
       WHERE username LIKE 'b2x\\_%')";
      "DELETE FROM pending_signups WHERE email LIKE '%@b2.invalid' OR username \
       LIKE 'b2x\\_%'";
    ]

type arm = {
  target : reply;
  probes : reply list;
  points : (int * int * int) list;
  sched : (float * (int * int * int)) list;
  hits : int;
  mails : int;
  resets : int;
}

let fill_arm ~mixed d =
  let fill_msg i =
    Earde.Email.password_reset
      ~to_email:(Printf.sprintf "fill%d@b2.invalid" i)
      ~token:"b2-fill-token"
  in
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let job =
          if (not mixed) || i < 2 || i mod 3 <> 0 then Some (fill_msg i)
          else None
        in
        Lwt.map ignore (D.admit d (fun () -> Lwt.return ((), job))))
      (List.init 58 Fun.id)
  in
  Lwt.return
    (List.init 5 (fun _ ->
         let p, u = Lwt.wait () in
         (u, D.admit d (fun () -> p))))

let run_arm ~url c ~mixed ~behaviour ~prepare ~target =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* () =
    Lwt_list.iter_s
      (fun q ->
        let* r = C.exec q () in
        or_fail "arm reset" r)
      q_arm_reset
  in
  let* () = prepare () in
  let clock = Vclock.create () in
  let mails = ref 0 in
  let d =
    D.create ~sleep:(Vclock.sleep clock) ~label:Earde.Email.label
      ~transport:
        (behaviour_transport clock ~record:(fun _ -> incr mails) behaviour)
      ()
  in
  current_mail := Some d;
  let* holds = fill_arm ~mixed d in
  let at_fill = occupancy d in
  limited_hits := 0;
  let* resets0 = find_one c q_reset_rows () "resets" in
  let* t = target () in
  let after_target = occupancy d in
  let probe name =
    forgot ~url ~target:"/b2-rl/forgot" (name ^ "-probe@b2.invalid")
  in
  let* p1 = probe "first" in
  Vclock.advance_to clock 14.999;
  let before_deadline = occupancy d in
  let* p2 = probe "early" in
  Vclock.advance_to clock 15.0;
  let at_deadline = occupancy d in
  let* p3 = probe "deadline" in
  let after_probes = occupancy d in
  (* The working reservations settle without mail, identically in each arm. *)
  List.iter (fun (u, _) -> Lwt.wakeup u ((), None)) holds;
  let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) holds) in
  let sched = trace clock d 100_000.0 in
  let* resets1 = find_one c q_reset_rows () "resets" in
  current_mail := None;
  Lwt.return
    {
      target = t;
      probes = [ p1; p2; p3 ];
      points =
        [
          at_fill;
          after_target;
          before_deadline;
          at_deadline;
          after_probes;
          occupancy d;
        ];
      sched;
      hits = !limited_hits;
      mails = !mails;
      resets = resets1 - resets0;
    }

let capacity_sequence_case =
  db_case
    "capacity: from the same near-full state, a target's private outcome \
     changes no later probe and no release time, for reset, signup and \
     reservation pairs under every provider behaviour" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = real_hash "b2 sequence victim" in
      let* _ =
        find_one c q_user
          ("b2x_seqvictim", "seqvictim@b2.invalid", hash)
          "victim"
      in
      let no_prepare () = Lwt.return_unit in
      let held_reservation () =
        let* r =
          C.exec q_pending
            ("b2x_seqheld", "seqholder@b2.invalid", sha256 "b2-seq-held", 24.0)
        in
        or_fail "reservation" r
      in
      let sign ~username ~email () =
        signup ~url ~target:"/b2-rl/signup" ~username ~email ()
      in
      let pairs =
        [
          ( "reset known vs unknown",
            no_prepare,
            (fun () ->
              forgot ~url ~target:"/b2-rl/forgot" "seqvictim@b2.invalid"),
            fun () -> forgot ~url ~target:"/b2-rl/forgot" "seqnobody@b2.invalid"
          );
          ( "signup new vs registered email",
            no_prepare,
            sign ~username:"b2x_seqnew" ~email:"seqnew@b2.invalid",
            sign ~username:"b2x_seqnew" ~email:"seqvictim@b2.invalid" );
          ( "signup own vs foreign reservation",
            held_reservation,
            sign ~username:"b2x_seqheld" ~email:"seqholder@b2.invalid",
            sign ~username:"b2x_seqheld" ~email:"seqintruder@b2.invalid" );
        ]
      in
      let behaviours = [ Stall; After (7.0, Ok ()); Fast_ok; Fast_error ] in
      print_newline ();
      Lwt_list.iter_s
        (fun (fill, mixed) ->
          Lwt_list.iter_s
            (fun behaviour ->
              Lwt_list.iter_s
                (fun (pair, prepare, mail_target, quiet_target) ->
                  let label =
                    pair ^ " / " ^ behaviour_name behaviour ^ " / " ^ fill
                  in
                  let* m =
                    run_arm ~url c ~mixed ~behaviour ~prepare
                      ~target:mail_target
                  in
                  let* q =
                    run_arm ~url c ~mixed ~behaviour ~prepare
                      ~target:quiet_target
                  in
                  let st r = string_of_int r.status in
                  Printf.printf
                    "[B2] %-64s probes mail=%s quiet=%s releases=%d last=%.0fs \
                     mails=%d/%d limiter=%d/%d\n\
                     %!"
                    label
                    (String.concat "," (List.map st m.probes))
                    (String.concat "," (List.map st q.probes))
                    (List.length m.sched)
                    (fst (List.nth m.sched (List.length m.sched - 1)))
                    m.mails q.mails m.hits q.hits;
                  (* The arms really differ privately... *)
                  Alcotest.(check int)
                    (label ^ ": only the mail arm's target produced mail")
                    (q.mails + 1) m.mails;
                  if pair = "reset known vs unknown" then
                    Alcotest.(check (pair int int))
                      (label ^ ": token rows") (1, 0) (m.resets, q.resets);
                  (* ...and nothing public does. *)
                  same_shape (label ^ ": target") m.target q.target;
                  private_session_preserved (label ^ ": mail target") m.target;
                  private_session_preserved (label ^ ": quiet target") q.target;
                  List.iteri
                    (fun i (a, b) ->
                      same_shape
                        (Printf.sprintf "%s: probe %d" label (i + 1))
                        a b)
                    (List.combine m.probes q.probes);
                  Alcotest.(check (list (triple int int int)))
                    (label ^ ": occupancy at each point")
                    m.points q.points;
                  Alcotest.check schedule
                    (label ^ ": release schedule")
                    m.sched q.sched;
                  (* Absolute expectations, after the paired ones. *)
                  Alcotest.(check int)
                    (label ^ ": limiter allowed every request")
                    4 m.hits;
                  Alcotest.(check int)
                    (label ^ ": limiter allowed every request (quiet)")
                    4 q.hits;
                  Alcotest.(check (list int))
                    (label
                   ^ ": probes full, full just before, admitted at the deadline"
                    )
                    [ 503; 503; 200 ]
                    (List.map (fun r -> r.status) m.probes);
                  Alcotest.(check (list (triple int int int)))
                    (label ^ ": expected occupancy")
                    [
                      (63, 56, 2);
                      (64, 57, 2);
                      (64, 57, 2);
                      (62, 55, 2);
                      (63, 56, 2);
                      (0, 0, 0);
                    ]
                    m.points;
                  Lwt.return_unit)
                pairs)
            behaviours)
        [ ("real-only fill", false); ("mixed fill", true) ])

let capacity_sequence_db_suite = [ capacity_sequence_case ]

(* ------------------------------------------------------------------ *)
(* Cookies and sessions over the routed boundary                         *)
(* ------------------------------------------------------------------ *)

let cookie_session_case =
  db_case
    "cookies: private-equivalent pairs set no cookie, keep the request session \
     and authenticate nobody; valid logins rotate identically" (fun ~url c ->
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 cookie pass" in
      let* _ =
        find_one c q_user ("b2x_cookie", "cookie@b2.invalid", hash) "user"
      in
      let* _ =
        find_one c q_user ("b2x_cookie2", "cookie2@b2.invalid", hash) "user 2"
      in
      let pair label (a, ca) (b, cb) =
        same_shape label a b;
        private_session_preserved (label ^ " (first)") a;
        private_session_preserved (label ^ " (second)") b;
        let* wa = get ~url ~cookie:ca "/whoami" in
        let* wb = get ~url ~cookie:cb "/whoami" in
        Alcotest.(check (pair string string))
          (label ^ ": nobody authenticated")
          ("anon", "anon") (wa.body, wb.body);
        Lwt.return_unit
      in
      let* k =
        post ~url ~target:"/forgot-password" [ ("email", "cookie@b2.invalid") ]
      in
      let* u =
        post ~url ~target:"/forgot-password" [ ("email", "nobody@b2.invalid") ]
      in
      let* () = pair "reset known vs unknown" k u in
      let* sn =
        post ~url ~target:"/signup"
          [
            ("username", "b2x_ck1");
            ("email", "cknew@b2.invalid");
            ("password", "b2 long password");
          ]
      in
      let* sr =
        post ~url ~target:"/signup"
          [
            ("username", "b2x_ck2");
            ("email", "cookie@b2.invalid");
            ("password", "b2 long password");
          ]
      in
      let* () = pair "signup new vs registered email" sn sr in
      let* lw = login ~url "b2x_cookie" "b2 wrong" in
      let* lm = login ~url "b2x_nobody" "b2 wrong" in
      let* () = pair "login wrong password vs missing account" lw lm in
      (* Controls: two valid logins differ from the failures, and from each
         other only by their random session ids. *)
      let* s1, pre1 = login ~url "b2x_cookie" "b2 cookie pass" in
      let* s2, _ = login ~url "b2x_cookie2" "b2 cookie pass" in
      same_shape "two valid logins" s1 s2;
      Alcotest.(check bool)
        "a valid login is distinguishable" true
        (reply_diff s1 (fst lw) <> None);
      Alcotest.(check string)
        "valid login replaces the session" "replaced" (session_effect s1);
      let* who = get ~url ~cookie:pre1 "/whoami" in
      Alcotest.(check string)
        "the pre-login cookie holds no identity" "anon" who.body;
      let* () = drained fm d in
      Lwt.return_unit)

let cookie_db_suite = [ cookie_session_case ]

let suites =
  [
    ("b2_login_verification", login_pure_suite);
    ("b2_rate_limit_decision", limiter_pure_suite);
    ("b2_login_equivalence", login_db_suite);
    ("b2_signup_privacy", signup_db_suite);
    ("b2_auth_mail_async", mail_db_suite);
    ("b2_rate_limit_routed", limiter_db_suite);
    ("b2_response_comparator", comparator_suite);
    ("b2_capacity_sequences", capacity_sequence_db_suite);
    ("b2_cookie_session", cookie_db_suite);
  ]
