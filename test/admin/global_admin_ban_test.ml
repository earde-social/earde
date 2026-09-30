(* === Global-admin ban/unban POST hardening ===
   Two pre-existing defects, both regressed here through the real routes over
   a real database: (1) the forms rendered Dream's CSRF tag but the handlers
   never parsed the body, so any cross-site POST from an admin's browser
   mutated ban state; (2) the post-action redirect fed the browser's absolute
   Referer into a helper that only accepted local paths, so every successful
   action bounced to "/" instead of the originating /admin or /u/:username
   surface. Database-gated (EARDE_TEST_DATABASE_URL). *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let status_of response = Dream.status_to_int (Dream.status response)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications WHERE user_id IN \
       (SELECT id FROM users WHERE username LIKE 'gab_%')"
    ; "DELETE FROM users WHERE username LIKE 'gab_%'"
    ]

let q_insert_user =
  (Caqti_type.(t3 string bool bool) ->! Caqti_type.int)
    "INSERT INTO users \
       (username, email, password_hash, is_email_verified, is_admin, is_banned) \
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE, $2, $3) RETURNING id"

let q_set_banned =
  (Caqti_type.(t2 bool int) ->. Caqti_type.unit)
    "UPDATE users SET is_banned = $1 WHERE id = $2"

let q_is_banned =
  (Caqti_type.int ->! Caqti_type.bool)
    "SELECT is_banned FROM users WHERE id = $1"

let q_notif_count =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE user_id = $1"

let q_mod_action_notifs =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications \
     WHERE user_id = $1 AND notif_type = 'mod_action'"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

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

let insert_user ?(admin = false) ?(banned = false) (module C : Caqti_lwt.CONNECTION)
    username =
  let* id = C.find q_insert_user (username, admin, banned) in
  or_fail username id

let set_banned (module C : Caqti_lwt.CONNECTION) ~banned id =
  let* r = C.exec q_set_banned (banned, id) in
  or_fail "set_banned" r

let is_banned (module C : Caqti_lwt.CONNECTION) id =
  let* r = C.find q_is_banned id in
  or_fail "is_banned" r

let notif_count (module C : Caqti_lwt.CONNECTION) id =
  let* r = C.find q_notif_count id in
  or_fail "notif_count" r

let mod_action_notifs (module C : Caqti_lwt.CONNECTION) id =
  let* r = C.find q_mod_action_notifs id in
  or_fail "mod_action_notifs" r

(* One shared pipeline: real routes, real handlers, real session and CSRF
   machinery. The identity middleware plants the session the way login
   would, once per fresh cookie. *)
let shared_identity : (int * string * bool) option ref = ref None

let shared_pipeline = ref None

let identity_middleware handler request =
  match Dream.session_field request "user_id" with
  | Some _ -> handler request
  | None -> (
      match !shared_identity with
      | None -> handler request
      | Some (uid, username, is_admin) ->
          let* () =
            Dream.set_session_field request "user_id" (string_of_int uid)
          in
          let* () = Dream.set_session_field request "username" username in
          let* () =
            if is_admin then Dream.set_session_field request "is_admin" "true"
            else Lwt.return_unit
          in
          handler request)

let build_pipeline ~url =
  Dream.sql_pool ~size:2 url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions @@ identity_middleware
  @@ Dream.router
       [ Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.get "/mint-expired" (fun req ->
             Dream.respond (Dream.csrf_token ~valid_for:(-60.) req));
         Dream.get "/u/:username" Earde.Handlers.view_profile_handler;
         Dream.get "/admin" Earde.Handlers.admin_dashboard_handler;
         Dream.post "/admin/ban/user/:id" Earde.Handlers.ban_user_handler;
         Dream.post "/admin/unban/user/:id"
           Earde.Handlers.unban_user_global_handler
       ]

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline = build_pipeline ~url in
      shared_pipeline := Some pipeline;
      pipeline

let as_user identity = shared_identity := Some identity

let as_anonymous () = shared_identity := None

(* Every request carries the Host header a real browser sends — the
   same-origin Referer reduction compares against it. *)
let host = "earde.com"

let do_get ?cookie ~url ~target () =
  let pipeline = pipeline_for ~url in
  let headers =
    ("Host", host)
    :: (match cookie with Some c -> [ ("Cookie", c) ] | None -> [])
  in
  let* response = pipeline (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* [omit_token] posts an empty urlencoded body — exactly what a scripted
   cross-site POST without the token would carry. *)
let do_post ?cookie ?referer ?(omit_token = false) ~url ~target ~token () =
  let pipeline = pipeline_for ~url in
  let headers =
    [ ("Host", host); ("Content-Type", "application/x-www-form-urlencoded") ]
    @ (match cookie with Some c -> [ ("Cookie", c) ] | None -> [])
    @ (match referer with Some r -> [ ("Referer", r) ] | None -> [])
  in
  let body =
    if omit_token then "" else Http_fixture.form_body [ ("dream.csrf", token) ]
  in
  let* response =
    pipeline (Dream.request ~method_:`POST ~target ~headers body)
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* One cookie-less GET that opens a fresh session for this identity and
   returns its cookie plus a live same-session CSRF token. *)
let open_session label ~url identity =
  as_user identity;
  let* response, token = do_get ~url ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return (Http_fixture.session_cookie label response, token)

let mint_expired label ~url ~cookie =
  let* response, body = do_get ~url ~cookie ~target:"/mint-expired" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return body

let ban_target id = Printf.sprintf "/admin/ban/user/%d" id

let unban_target id = Printf.sprintf "/admin/unban/user/%d" id

let check_redirect label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some expected)
    (Dream.header response "Location")

let check_form_error label response body =
  Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
  Alcotest.(check bool) (label ^ ": form-error copy") true
    (Html_assert.contains body "Invalid form submission.");
  Alcotest.(check (option string)) (label ^ ": no redirect") None
    (Dream.header response "Location")

(* --- 13. the rendered forms are the contract the browser submits --- *)
let form_contract_case =
  db_case "profile ban form and /admin unban form keep route, method, CSRF \
           field and confirm hook" (fun ~url conn ->
      let* admin = insert_user ~admin:true conn "gab_admin" in
      let* target = insert_user conn "gab_target" in
      let* banned = insert_user ~banned:true conn "gab_banned" in
      let* cookie, _ = open_session "forms" ~url (admin, "gab_admin", true) in
      let* response, body = do_get ~url ~cookie ~target:"/u/gab_target" () in
      Alcotest.(check int) "profile 200" 200 (status_of response);
      Alcotest.(check bool) "ban form action" true
        (Html_assert.contains body
           (Printf.sprintf "<form action='/admin/ban/user/%d' method='POST'"
              target));
      Alcotest.(check bool) "ban confirm hook" true
        (Html_assert.contains body "confirmModal(event, 'Permanently ban u/gab_target?");
      let* _ = Lwt.return (Http_fixture.csrf_of_page "profile ban form CSRF" body) in
      let* response, body = do_get ~url ~cookie ~target:"/admin" () in
      Alcotest.(check int) "/admin 200" 200 (status_of response);
      Alcotest.(check bool) "unban form action" true
        (Html_assert.contains body
           (Printf.sprintf
              "<form class='admin-act-form' action='/admin/unban/user/%d' \
               method='POST'"
              banned));
      Alcotest.(check bool) "unban confirm hook" true
        (Html_assert.contains body "confirmModal(event, 'Lift global ban on u/gab_banned?");
      let* _ = Lwt.return (Http_fixture.csrf_of_page "/admin unban form CSRF" body) in
      Lwt.return_unit)

(* --- 1, 2, 14, 15: the happy paths, returning to the real surfaces --- *)
let happy_path_case =
  db_case "valid same-session token bans and unbans; absolute same-origin \
           Referers return to the profile and /admin" (fun ~url conn ->
      let* admin = insert_user ~admin:true conn "gab_admin" in
      let* target = insert_user conn "gab_target" in
      let* cookie, token =
        open_session "happy" ~url (admin, "gab_admin", true)
      in
      let* response, _ =
        do_post ~url ~cookie ~token
          ~referer:"https://earde.com/u/gab_target"
          ~target:(ban_target target) ()
      in
      check_redirect "ban" "/u/gab_target" response;
      let* banned = is_banned conn target in
      Alcotest.(check bool) "target banned" true banned;
      let* notifs = mod_action_notifs conn target in
      Alcotest.(check int) "one mod_action notification" 1 notifs;
      let* response, _ =
        do_post ~url ~cookie ~token ~referer:"https://earde.com/admin"
          ~target:(unban_target target) ()
      in
      check_redirect "unban" "/admin" response;
      let* banned = is_banned conn target in
      Alcotest.(check bool) "target unbanned" false banned;
      (* Unban never notifies — the count is still the ban's single row. *)
      let* notifs = notif_count conn target in
      Alcotest.(check int) "no unban notification" 1 notifs;
      Lwt.return_unit)

(* --- 3-9: every rejected token leaves zero side effects --- *)
let rejected_tokens_case label ~action ~initially_banned =
  db_case label (fun ~url conn ->
      let* admin = insert_user ~admin:true conn "gab_admin" in
      let* target =
        insert_user ~banned:initially_banned conn "gab_target"
      in
      let* cookie, live =
        open_session "rejects" ~url (admin, "gab_admin", true)
      in
      let* expired = mint_expired "rejects" ~url ~cookie in
      let* _other_cookie, foreign =
        open_session "foreign" ~url (admin, "gab_admin", true)
      in
      as_user (admin, "gab_admin", true);
      let post_target =
        if action = `Ban then ban_target target else unban_target target
      in
      let attempts =
        [ ("missing token", None, true); ("forged token", Some "not-a-token", false);
          ("stale token", Some expired, false);
          ("foreign-session token", Some foreign, false)
        ]
      in
      let* () =
        Lwt_list.iter_s
          (fun (name, token, omit_token) ->
            let* response, body =
              do_post ~url ~cookie ~omit_token
                ~token:(Option.value token ~default:"")
                ~referer:"https://earde.com/admin" ~target:post_target ()
            in
            check_form_error name response body;
            let* banned = is_banned conn target in
            Alcotest.(check bool) (name ^ ": ban state untouched")
              initially_banned banned;
            let* notifs = notif_count conn target in
            Alcotest.(check int) (name ^ ": no notification") 0 notifs;
            Lwt.return_unit)
          attempts
      in
      (* The live token still works afterwards — rejection is per-request,
         not a session poison. *)
      let* response, _ =
        do_post ~url ~cookie ~token:live ~referer:"https://earde.com/admin"
          ~target:post_target ()
      in
      check_redirect "live token still accepted" "/admin" response;
      let* banned = is_banned conn target in
      Alcotest.(check bool) "action applied" (action = `Ban) banned;
      Lwt.return_unit)

let ban_rejects_case =
  rejected_tokens_case
    "ban: missing, forged, stale and foreign-session tokens are rejected \
     with zero side effects"
    ~action:`Ban ~initially_banned:false

let unban_rejects_case =
  rejected_tokens_case
    "unban: missing, forged, stale and foreign-session tokens are rejected \
     with zero side effects"
    ~action:`Unban ~initially_banned:true

(* --- 10, 11: authorization comes first and is not bought by a token --- *)
let authorization_case =
  db_case "non-admin with a valid token and anonymous POSTs keep the \
           existing denial; no mutation" (fun ~url conn ->
      let* _admin = insert_user ~admin:true conn "gab_admin" in
      let* peon = insert_user conn "gab_peon" in
      let* target = insert_user conn "gab_target" in
      let* banned_user = insert_user ~banned:true conn "gab_banned" in
      let* cookie, token =
        open_session "peon" ~url (peon, "gab_peon", false)
      in
      let* response, body =
        do_post ~url ~cookie ~token ~target:(ban_target target) ()
      in
      Alcotest.(check int) "non-admin ban: 200 page" 200 (status_of response);
      Alcotest.(check bool) "non-admin ban: denial copy" true
        (Html_assert.contains body "You are not an Admin.");
      let* response, body =
        do_post ~url ~cookie ~token ~target:(unban_target banned_user) ()
      in
      Alcotest.(check int) "non-admin unban: 200 page" 200
        (status_of response);
      Alcotest.(check bool) "non-admin unban: denial copy" true
        (Html_assert.contains body "You are not an Admin.");
      as_anonymous ();
      let* response, body =
        do_post ~url ~token:"" ~omit_token:true ~target:(ban_target target) ()
      in
      Alcotest.(check int) "anonymous ban: 200 page" 200 (status_of response);
      Alcotest.(check bool) "anonymous ban: denial copy" true
        (Html_assert.contains body "You are not an Admin.");
      let* banned = is_banned conn target in
      Alcotest.(check bool) "target never banned" false banned;
      let* still = is_banned conn banned_user in
      Alcotest.(check bool) "banned user still banned" true still;
      let* notifs = notif_count conn target in
      Alcotest.(check int) "no notification" 0 notifs;
      Lwt.return_unit)

(* --- 12: target resolution stays authoritative and post-CSRF --- *)
let unknown_target_case =
  db_case "unknown and malformed target ids keep the existing post-CSRF \
           behavior" (fun ~url conn ->
      let* admin = insert_user ~admin:true conn "gab_admin" in
      let* cookie, token =
        open_session "unknown" ~url (admin, "gab_admin", true)
      in
      (* A nonexistent id: the 0-row UPDATE still reads as success, the
         best-effort notification insert fails silently, and the redirect
         follows the usual fallback. Pinned as the pre-existing contract. *)
      let* response, _ =
        do_post ~url ~cookie ~token ~target:(ban_target 999999999) ()
      in
      check_redirect "unknown ban target" "/" response;
      let* response, _ =
        do_post ~url ~cookie ~token ~target:(unban_target 999999999) ()
      in
      check_redirect "unknown unban target" "/admin" response;
      (* A non-numeric id parses to 0 and stays the plain 400. *)
      let* response, body =
        do_post ~url ~cookie ~token ~target:"/admin/ban/user/abc" ()
      in
      Alcotest.(check int) "malformed id: 400" 400 (status_of response);
      Alcotest.(check bool) "malformed id: copy" true
        (Html_assert.contains body "Invalid user ID.");
      Lwt.return_unit)

(* --- 16-21 + missing-Referer fallbacks, through the real handlers --- *)
let redirect_grammar_case =
  db_case "Referer handling: local and same-origin values return to their \
           surface, hostile values fall back, no response ever leaves the \
           origin" (fun ~url conn ->
      let* admin = insert_user ~admin:true conn "gab_admin" in
      let* target = insert_user conn "gab_target" in
      let* cookie, token =
        open_session "grammar" ~url (admin, "gab_admin", true)
      in
      let ban ?referer () =
        let* () = set_banned conn ~banned:false target in
        do_post ~url ~cookie ~token ?referer ~target:(ban_target target) ()
      in
      let unban ?referer () =
        let* () = set_banned conn ~banned:true target in
        do_post ~url ~cookie ~token ?referer ~target:(unban_target target) ()
      in
      (* 16: relative local destinations stay accepted. *)
      let* response, _ = unban ~referer:"/admin" () in
      check_redirect "relative Referer" "/admin" response;
      (* 19: same-origin query strings survive. *)
      let* response, _ =
        ban ~referer:"https://earde.com/u/gab_target?tab=posts" ()
      in
      check_redirect "query preserved" "/u/gab_target?tab=posts" response;
      (* 20: fragments never reach the Location header. *)
      let* response, _ = unban ~referer:"https://earde.com/admin#banned" () in
      check_redirect "fragment dropped" "/admin" response;
      (* 17: a foreign origin falls back to each action's local default. *)
      let* response, _ = ban ~referer:"https://evil.example/u/gab_target" () in
      check_redirect "foreign Referer, ban fallback" "/" response;
      let* response, _ = unban ~referer:"https://evil.example/admin" () in
      check_redirect "foreign Referer, unban fallback" "/admin" response;
      (* 18: protocol-relative and malformed values fall back safely. *)
      let* response, _ = ban ~referer:"//evil.example/admin" () in
      check_redirect "protocol-relative Referer" "/" response;
      let* response, _ = unban ~referer:"earde.com/admin" () in
      check_redirect "malformed Referer" "/admin" response;
      (* Missing Referer: the documented local fallbacks. *)
      let* response, _ = ban () in
      check_redirect "no Referer, ban" "/" response;
      let* response, _ = unban () in
      check_redirect "no Referer, unban" "/admin" response;
      (* 21: sweep — no redirect above may carry a foreign origin; spot-
         check the worst offender end to end. *)
      let* response, _ =
        ban ~referer:"https://earde.com@evil.example/admin" ()
      in
      (match Dream.header response "Location" with
      | Some l ->
          Alcotest.(check bool) "userinfo trick: local Location" true
            (String.length l > 0 && l.[0] = '/'
            && not (String.length l >= 2 && l.[1] = '/'));
          Alcotest.(check bool) "userinfo trick: no evil.example" false
            (Html_assert.contains l "evil.example")
      | None -> Alcotest.fail "userinfo trick: no redirect");
      Lwt.return_unit)

let suite =
  [ form_contract_case; happy_path_case; ban_rejects_case;
    unban_rejects_case; authorization_case; unknown_target_case;
    redirect_grammar_case ]

let suites =
  [ ("global_admin_ban_actions", suite)
  ]
