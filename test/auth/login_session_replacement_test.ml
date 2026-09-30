(* === Login session replacement (stale-admin privilege regression) ===
   The successful-login path must derive the session exclusively from the
   newly authenticated user: it invalidates whatever session the browser
   presented (rotating the session id) and rewrites every canonical field —
   user_id, username, and is_admin unconditionally, including the false
   case. Before the fix, is_admin was written only when true, so an
   admin → non-admin re-login in the same browser session silently kept
   admin authorization. These cases drive the real handlers through one
   pipeline instance with a cookie jar, exactly like a browser reusing a
   session cookie across logins. Database-gated. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'asr_%')"
    ; "DELETE FROM pending_signups WHERE username LIKE 'asr_%'"
    ; "DELETE FROM users WHERE username LIKE 'asr_%'"
    ]

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

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
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_user =
  (Caqti_type.(t3 string string bool) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified, is_admin)
     VALUES ($1, $1 || '@test.invalid', $2, TRUE, $3) RETURNING id"

let q_is_banned =
  (Caqti_type.int ->! Caqti_type.bool)
    "SELECT is_banned FROM users WHERE id = $1"

let q_set_banned =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

let q_insert_pending =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
     VALUES ($1, $1 || '@test.invalid', 'x', $2, NOW() + INTERVAL '1 hour')"

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

(* One [client] models one browser: a single pipeline instance (so
   memory_sessions state persists across requests) plus a cookie jar
   carried between requests. POST forms get a valid dream.csrf minted
   inside the pipeline, under the session the jar cookie selects — the
   same token a served form would carry. The extra probe routes are
   test-only: /whoami serializes the full session dictionary (field
   names and fixture values only), and /poison plants another user's
   authenticated fields to model the pre-fix stale state directly. *)
type client = {
  pipeline : Dream.request -> Dream.response Dream.promise;
  jar : (string * string) list ref;
  pending_form : (string * string) list option ref;
}

let make_client ~url ~poison_fields =
  let pending_form = ref None in
  let with_form handler req =
    (match !pending_form with
    | None -> ()
    | Some fields ->
        pending_form := None;
        let csrf = Dream.csrf_token req in
        Dream.set_body req (form_body (("dream.csrf", csrf) :: fields)));
    handler req
  in
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions
    @@ Dream.router
         [ Dream.get "/login" Earde.Auth_handlers.login_page
         ; Dream.post "/login" (with_form Earde.Auth_handlers.login_handler)
         ; Dream.post "/logout" Earde.Auth_handlers.logout_handler
         ; Dream.get "/confirm" Earde.Auth_handlers.confirm_email_handler
         ; Dream.get "/admin" Earde.Admin_handlers.admin_dashboard_handler
         ; Dream.post "/admin/ban/user/:id"
             (with_form Earde.Admin_handlers.ban_user_handler)
         ; Dream.post "/admin/unban/user/:id"
             (with_form Earde.Admin_handlers.unban_user_global_handler)
         ; Dream.get "/whoami" (fun req ->
               Dream.respond
                 (String.concat "\n"
                    (List.sort compare
                       (List.map
                          (fun (k, v) -> k ^ "=" ^ v)
                          (Dream.all_session_fields req)))))
         ; Dream.get "/poison" (fun req ->
               let* () =
                 Lwt_list.iter_s
                   (fun (k, v) -> Dream.set_session_field req k v)
                   poison_fields
               in
               Dream.respond "poisoned")
         ]
  in
  { pipeline; jar = ref []; pending_form }

let update_jar jar response =
  List.iter
    (fun header ->
      match String.split_on_char ';' header with
      | pair :: _ -> (
          match String.index_opt pair '=' with
          | Some i ->
              let name = String.sub pair 0 i in
              let value =
                String.sub pair (i + 1) (String.length pair - i - 1)
              in
              jar := (name, value) :: List.remove_assoc name !jar
          | None -> ())
      | [] -> ())
    (Dream.headers response "Set-Cookie")

let send client ?(method_ = `GET) ?form target =
  client.pending_form := form;
  let headers =
    (match !(client.jar) with
    | [] -> []
    | pairs ->
        [ ( "Cookie",
            String.concat "; "
              (List.map (fun (n, v) -> n ^ "=" ^ v) pairs) ) ])
    @
    match form with
    | Some _ -> [ ("Content-Type", "application/x-www-form-urlencoded") ]
    | None -> []
  in
  let request = Dream.request ~method_ ~target ~headers "" in
  let* response = client.pipeline request in
  update_jar client.jar response;
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

(* The session id is material: comparisons are boolean, and failure
   messages never print a cookie value. *)
let session_cookie client =
  match List.assoc_opt "dream.session" !(client.jar) with
  | Some v -> v
  | None -> Alcotest.fail "no dream.session cookie in the jar"

let check_session label expected body =
  Alcotest.(check string) (label ^ ": exact session fields") expected body

let session_of ~uid ~username ~is_admin =
  Printf.sprintf "is_admin=%b\nuser_id=%d\nusername=%s" is_admin uid
    username

let login client ~identifier ~password =
  send client ~method_:`POST
    ~form:[ ("identifier", identifier); ("password", password) ]
    "/login"

let fixture_users (module C : Caqti_lwt.CONNECTION) =
  let* hash = Earde.Auth.hash_password "asr password" in
  let* hash = or_fail_s "hash" hash in
  let* admin = C.find q_insert_user ("asr_admin", hash, true) in
  let* admin = or_fail "admin" admin in
  let* user = C.find q_insert_user ("asr_user", hash, false) in
  let* user = or_fail "user" user in
  let* victim = C.find q_insert_user ("asr_victim", hash, false) in
  let* victim = or_fail "victim" victim in
  Lwt.return (admin, user, victim)

let admin_then_user_case =
  db_case
    "admin then non-admin login in one browser session drops all admin \
     authorization"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* admin, user, victim = fixture_users c in
      let client = make_client ~url ~poison_fields:[] in
      (* Pre-auth browsing creates the pre-session the login must replace. *)
      let* status, _ = send client "/login" in
      Alcotest.(check int) "login page" 200 status;
      let pre_auth_cookie = session_cookie client in
      let* status, _ =
        login client ~identifier:"asr_admin" ~password:"asr password"
      in
      Alcotest.(check bool) "admin login redirects" true (status / 100 = 3);
      let admin_cookie = session_cookie client in
      Alcotest.(check bool) "login rotates the pre-auth session id" false
        (String.equal pre_auth_cookie admin_cookie);
      let* _, body = send client "/whoami" in
      check_session "admin session"
        (session_of ~uid:admin ~username:"asr_admin" ~is_admin:true)
        body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "admin dashboard allowed" 200 status;
      (* Reversible protected POST: ban then unban really hit the DB. The
         empty form still carries the session's minted dream.csrf — the
         hardened handlers validate it before mutating. *)
      let* status, _ =
        send client ~method_:`POST ~form:[]
          (Printf.sprintf "/admin/ban/user/%d" victim)
      in
      Alcotest.(check bool) "ban redirects" true (status / 100 = 3);
      let* banned = C.find q_is_banned victim in
      let* banned = or_fail "banned" banned in
      Alcotest.(check bool) "victim banned by admin" true banned;
      let* status, _ =
        send client ~method_:`POST ~form:[]
          (Printf.sprintf "/admin/unban/user/%d" victim)
      in
      Alcotest.(check bool) "unban redirects" true (status / 100 = 3);
      let* banned = C.find q_is_banned victim in
      let* banned = or_fail "unbanned" banned in
      Alcotest.(check bool) "victim unbanned again" false banned;
      (* The same browser session now logs in as a non-admin. *)
      let* status, _ =
        login client ~identifier:"asr_user" ~password:"asr password"
      in
      Alcotest.(check bool) "user login redirects" true (status / 100 = 3);
      let user_cookie = session_cookie client in
      Alcotest.(check bool) "re-login rotates the session id" false
        (String.equal admin_cookie user_cookie);
      let* _, body = send client "/whoami" in
      check_session "non-admin session inherits nothing"
        (session_of ~uid:user ~username:"asr_user" ~is_admin:false)
        body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "admin dashboard now denied" 403 status;
      let* status, body =
        send client ~method_:`POST
          (Printf.sprintf "/admin/ban/user/%d" victim)
      in
      Alcotest.(check int) "ban denied status" 200 status;
      Alcotest.(check bool) "ban denied body" true
        (Html_assert.contains body "not an Admin");
      let* banned = C.find q_is_banned victim in
      let* banned = or_fail "still unbanned" banned in
      Alcotest.(check bool) "denied ban wrote nothing" false banned;
      Lwt.return_unit)

let user_then_admin_case =
  db_case
    "non-admin then admin login gains admin; logout clears the session"
    (fun ~url c ->
      let* admin, user, _victim = fixture_users c in
      let client = make_client ~url ~poison_fields:[] in
      let* _ =
        login client ~identifier:"asr_user" ~password:"asr password"
      in
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "user denied admin" 403 status;
      let* status, _ =
        login client ~identifier:"asr_admin" ~password:"asr password"
      in
      Alcotest.(check bool) "admin login redirects" true (status / 100 = 3);
      let* _, body = send client "/whoami" in
      check_session "admin session after upgrade"
        (session_of ~uid:admin ~username:"asr_admin" ~is_admin:true)
        body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "admin allowed after upgrade" 200 status;
      (* Logout must leave no user-derived field behind. *)
      let* status, _ = send client ~method_:`POST ~form:[] "/logout" in
      Alcotest.(check bool) "logout redirects" true (status / 100 = 3);
      let* _, body = send client "/whoami" in
      check_session "logged-out session is empty" "" body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "admin denied after logout" 403 status;
      (* Login after logout derives only from the new user. *)
      let* _ =
        login client ~identifier:"asr_user" ~password:"asr password"
      in
      let* _, body = send client "/whoami" in
      check_session "post-logout login is only the new user"
        (session_of ~uid:user ~username:"asr_user" ~is_admin:false)
        body;
      Lwt.return_unit)

let failed_login_case =
  db_case
    "failed and banned logins authenticate no one and leave the prior \
     session unmixed"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* admin, _user, victim = fixture_users c in
      (* Failed attempt from a clean browser: no identity, no admin. *)
      let clean = make_client ~url ~poison_fields:[] in
      let* status, _ =
        login clean ~identifier:"asr_user" ~password:"wrong"
      in
      Alcotest.(check int) "failed login status" 200 status;
      let* _, body = send clean "/whoami" in
      check_session "failed login leaves no identity" "" body;
      let* status, _ = send clean "/admin" in
      Alcotest.(check int) "failed login grants nothing" 403 status;
      (* Failed attempt while an admin session exists: the old session
         survives intact — no mixed identity, no privilege change. *)
      let client = make_client ~url ~poison_fields:[] in
      let* _ =
        login client ~identifier:"asr_admin" ~password:"asr password"
      in
      let* status, _ =
        login client ~identifier:"asr_user" ~password:"wrong"
      in
      Alcotest.(check int) "failed re-login status" 200 status;
      let* _, body = send client "/whoami" in
      check_session "prior admin session unchanged"
        (session_of ~uid:admin ~username:"asr_admin" ~is_admin:true)
        body;
      (* A banned account with the right password gets no session either. *)
      let* r = C.exec q_set_banned victim in
      let* () = or_fail "ban victim" r in
      let banned_client = make_client ~url ~poison_fields:[] in
      let* status, _ =
        login banned_client ~identifier:"asr_victim"
          ~password:"asr password"
      in
      Alcotest.(check int) "banned login status" 200 status;
      let* _, body = send banned_client "/whoami" in
      check_session "banned login leaves no identity" "" body;
      Lwt.return_unit)

let poisoned_session_case =
  db_case
    "stale pre-authentication session fields cannot survive a successful \
     login"
    (fun ~url c ->
      let* admin, user, _victim = fixture_users c in
      (* Plant the exact pre-fix stale state: an authenticated admin
         session, then a non-admin login over it. *)
      let client =
        make_client ~url
          ~poison_fields:
            [ ("user_id", string_of_int admin)
            ; ("username", "asr_admin")
            ; ("is_admin", "true")
            ]
      in
      let* status, _ = send client "/poison" in
      Alcotest.(check int) "poison probe" 200 status;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "poisoned session is admin-capable" 200 status;
      let* _ =
        login client ~identifier:"asr_user" ~password:"asr password"
      in
      let* _, body = send client "/whoami" in
      check_session "login replaced every stale field"
        (session_of ~uid:user ~username:"asr_user" ~is_admin:false)
        body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "stale admin capability gone" 403 status;
      Lwt.return_unit)

let confirm_no_session_case =
  db_case "signup email confirmation establishes no authenticated session"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let hash = Earde.Pending_signup_store.hash_token "asr_confirm_tok" in
      let* r = C.exec q_insert_pending ("asr_signup", hash) in
      let* () = or_fail "pending" r in
      let client = make_client ~url ~poison_fields:[] in
      let* status, _ = send client "/confirm?token=asr_confirm_tok" in
      Alcotest.(check int) "confirm status" 200 status;
      let* _, body = send client "/whoami" in
      check_session "confirmation leaves no identity" "" body;
      let* status, _ = send client "/admin" in
      Alcotest.(check int) "confirmation grants nothing" 403 status;
      Lwt.return_unit)

let suite =
  [ admin_then_user_case; user_then_admin_case; failed_login_case;
    poisoned_session_case; confirm_no_session_case
  ]

let suites =
    (* Successful login replaces the presented session wholesale: fresh
       session id, every canonical field rewritten from the new user's
       row (is_admin unconditionally, including false), so an
       admin → non-admin re-login in the same browser keeps no admin
       authorization, logout leaves nothing behind, and failed, banned,
       and signup-confirmation paths authenticate no one. Database-gated. *)
  [ ("login_session_replacement", suite)
  ]
