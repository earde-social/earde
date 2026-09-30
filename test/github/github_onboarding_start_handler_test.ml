module An = Earde.Analytics
module Ob = Earde.Project_onboarding
module GOC = Earde.Github_onboarding_crypto
module GPK = Earde.Github_onboarding_pkce
module GOU = Earde.Github_onboarding_urls
module GSD = Earde.Github_onboarding_session_data

(* === GitHub onboarding start handler (Github_onboarding_handlers) ===
   The factory takes the mode and config loader by injection, so gate cases
   run DB-free with fixed values and never touch the process environment.
   A request that passes every gate stops at the missing sql_pool — reaching
   that boundary IS the assertion that the gates let it through with no SQL
   executed. Database-gated cases run the real pipeline (sql_pool + secret +
   memory sessions) under the usual EARDE_TEST_DATABASE_URL opt-in with
   'ghstart_%' fixtures cleaned up around each case. Raw states, bindings,
   verifiers, cookie values, and hashes never reach assertion output —
   material comparisons are boolean. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let case = Case.quick

let off_case =
  case "Off: controlled 404, loader and cookies untouched" (fun () ->
      let loader, calls = Http_fixture.counting_loader (Http_fixture.ok_loader ()) in
      let response =
        Http_fixture.gate_response "off"
          (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in ~mode:Ob.Off ~load_config:loader ())
      in
      Alcotest.(check int) "404" 404 (Http_fixture.status_of response);
      Alcotest.(check int) "loader never called" 0 !calls;
      Alcotest.(check int) "no flow cookie" 0
        (List.length (Github_handler_fixture.flow_cookies response)))

let anonymous_case =
  case "Public: anonymous POST redirects to /login" (fun () ->
      let loader, calls = Http_fixture.counting_loader (Http_fixture.ok_loader ()) in
      let response =
        Http_fixture.gate_response "anonymous"
          (Github_handler_fixture.gate_run ~mode:Ob.Public ~load_config:loader ())
      in
      Alcotest.(check int) "redirect" 303 (Http_fixture.status_of response);
      Alcotest.(check (option string)) "to /login" (Some "/login")
        (Dream.header response "Location");
      Alcotest.(check int) "loader never called" 0 !calls)

let malformed_session_case =
  case "Public: malformed session user_id redirects to /login" (fun () ->
      List.iter
        (fun raw ->
          let response =
            Http_fixture.gate_response "malformed"
              (Github_handler_fixture.gate_run
                 ~session:[ ("user_id", raw) ]
                 ~mode:Ob.Public
                 ~load_config:(fun () -> Http_fixture.ok_loader ())
                 ())
          in
          Alcotest.(check (option string)) "to /login" (Some "/login")
            (Dream.header response "Location"))
        [ "not-a-number"; ""; "0"; "-3" ])

let admins_non_admin_case =
  case "Admins: authenticated non-admin is 403, loader untouched"
    (fun () ->
      let loader, calls = Http_fixture.counting_loader (Http_fixture.ok_loader ()) in
      List.iter
        (fun session ->
          let response =
            Http_fixture.gate_response "non-admin"
              (Github_handler_fixture.gate_run ~session ~mode:Ob.Admins ~load_config:loader ())
          in
          Alcotest.(check int) "403" 403 (Http_fixture.status_of response))
        [ Http_fixture.logged_in; Http_fixture.logged_in @ [ ("is_admin", "false") ] ];
      Alcotest.(check int) "loader never called" 0 !calls)

let config_failure_case =
  case "Public: configuration error is a generic 503" (fun () ->
      let loader, calls =
        Http_fixture.counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      let response =
        Http_fixture.gate_response "config failure"
          (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in ~mode:Ob.Public ~load_config:loader
             ())
      in
      Alcotest.(check int) "503" 503 (Http_fixture.status_of response);
      Alcotest.(check int) "loader called once" 1 !calls;
      Alcotest.(check int) "no flow cookie" 0
        (List.length (Github_handler_fixture.flow_cookies response));
      let body = Lwt_main.run (Dream.body response) in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("body does not leak " ^ needle)
            false
            (Html_assert.contains_nonempty ~needle body))
        [ "EARDE_PUBLIC_ORIGIN"; "Missing"; "Invalid"; "public_origin" ])

(* One rejected-origin run: Public mode, valid session, fixed config. *)
let origin_rejected label ?(sec_fetch_site = None) origin =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @
    match sec_fetch_site with
    | Some v -> [ ("Sec-Fetch-Site", v) ]
    | None -> []
  in
  let response =
    Http_fixture.gate_response label
      (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in ~headers ~mode:Ob.Public
         ~load_config:(fun () -> Http_fixture.ok_loader ())
         ())
  in
  Alcotest.(check int) (label ^ ": 403") 403 (Http_fixture.status_of response);
  Alcotest.(check int)
    (label ^ ": no flow cookie")
    0
    (List.length (Github_handler_fixture.flow_cookies response))

let cross_origin_case =
  case "origin gate: cross-origin Origin is 403" (fun () ->
      origin_rejected "cross-origin" (Some "https://evil.example");
      origin_rejected "same-site subdomain" (Some "https://www.earde.com");
      origin_rejected "wrong scheme" (Some "http://earde.com");
      origin_rejected "wrong port" (Some "https://earde.com:8443"))

let malformed_origin_case =
  case "origin gate: malformed Origin is 403" (fun () ->
      origin_rejected "null" (Some "null");
      origin_rejected "blank" (Some "");
      origin_rejected "no scheme" (Some "earde.com");
      origin_rejected "trailing path" (Some "https://earde.com/");
      origin_rejected "userinfo" (Some "https://u@earde.com");
      origin_rejected "query" (Some "https://earde.com?x=1"))

let origin_beats_fetch_site_case =
  case "origin gate: mismatching Origin loses to Sec-Fetch-Site"
    (fun () ->
      origin_rejected "mismatch + same-origin"
        ~sec_fetch_site:(Some "same-origin")
        (Some "https://evil.example");
      origin_rejected "malformed + same-origin"
        ~sec_fetch_site:(Some "same-origin") (Some "null"))

let missing_signals_case =
  case "origin gate: missing Origin and Sec-Fetch-Site is 403" (fun () ->
      origin_rejected "no signals" None;
      origin_rejected "cross-site" ~sec_fetch_site:(Some "cross-site") None;
      origin_rejected "same-site" ~sec_fetch_site:(Some "same-site") None;
      origin_rejected "none" ~sec_fetch_site:(Some "none") None)

let matching_origin_case =
  case "origin gate: matching Origin passes" (fun () ->
      Http_fixture.check_db_boundary "exact origin"
        (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in
           ~headers:[ ("Origin", "https://earde.com") ]
           ~mode:Ob.Public
           ~load_config:(fun () -> Http_fixture.ok_loader ())
           ());
      (* Effective-port normalization: an explicit default port is the
         same origin. *)
      Http_fixture.check_db_boundary "explicit default port"
        (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in
           ~headers:[ ("Origin", "https://earde.com:443") ]
           ~mode:Ob.Public
           ~load_config:(fun () -> Http_fixture.ok_loader ())
           ()))

let fetch_metadata_pass_case =
  case "origin gate: no Origin + Sec-Fetch-Site same-origin passes"
    (fun () ->
      Http_fixture.check_db_boundary "fetch metadata"
        (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in
           ~headers:[ ("Sec-Fetch-Site", "same-origin") ]
           ~mode:Ob.Public
           ~load_config:(fun () -> Http_fixture.ok_loader ())
           ()))

let gate_suite =
  [ off_case; anonymous_case; malformed_session_case;
    admins_non_admin_case; config_failure_case; cross_origin_case;
    malformed_origin_case; origin_beats_fetch_site_case;
    missing_signals_case; matching_origin_case; fetch_metadata_pass_case ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM github_onboarding_states WHERE user_id IN \
       (SELECT id FROM users WHERE username LIKE 'ghstart_%')"
    ; "DELETE FROM users WHERE username LIKE 'ghstart_%'"
    ]

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = Github_handler_fixture.or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* () = Github_handler_fixture.or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

(* Everything stored for one issued state, keyed by its lookup hash. *)
let q_row_by_state_hash =
  (Caqti_type.(string ->* t2 (t3 int string string) (t3 bool bool float)))
  "SELECT user_id, session_binding_hash, flow,
          pending_github_installation_id IS NULL,
          consumed_at IS NULL,
          EXTRACT(EPOCH FROM (expires_at - created_at))::float8
   FROM github_onboarding_states WHERE state_hash = $1"

let q_count_for_user =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM github_onboarding_states WHERE user_id = $1"

(* The stored row for a state, as (binding_hash, all text columns). *)
let row_of_state (module C : Caqti_lwt.CONNECTION) label ~uid state =
  let state_hash = GOC.state_hash_to_string (GOC.hash_state state) in
  let* rows = C.collect_list q_row_by_state_hash state_hash in
  let* rows = Github_handler_fixture.or_fail (label ^ ": row") rows in
  match rows with
  | [ ( (row_uid, binding_hash, flow),
        (pending_null, consumed_null, ttl) ) ] ->
      Alcotest.(check int) (label ^ ": row belongs to the session user")
        uid row_uid;
      Alcotest.(check string) (label ^ ": flow") "project_onboarding" flow;
      Alcotest.(check bool) (label ^ ": pending installation NULL") true
        pending_null;
      Alcotest.(check bool) (label ^ ": consumed_at NULL") true
        consumed_null;
      Alcotest.(check bool) (label ^ ": expiry ~15 minutes") true
        (Float.abs (ttl -. 900.) <= 5.);
      Lwt.return
        ( binding_hash,
          String.concat "|" [ state_hash; binding_hash; flow ] )
  | rows ->
      Alcotest.failf "%s: expected exactly one row, found %d" label
        (List.length rows)

let success_case =
  db_case "successful start: 303 + hashed row + encrypted per-flow cookie"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find Github_handler_fixture.q_insert_user "ghstart_user" in
      let* uid = Github_handler_fixture.or_fail "user" uid in
      let config = Github_fixture.gck_https_config () in
      let* response = Github_handler_fixture.run_start ~url ~session_user_id:uid in
      let location, state, name, value =
        Github_handler_fixture.successful_start "start" response
      in
      Alcotest.(check bool) "Location is exactly installation_url" true
        (String.equal location (GOU.installation_url config ~state));
      let* stored_binding_hash, text_columns =
        row_of_state (module C) "start" ~uid state
      in
      Alcotest.(check string) "browser-visible name derives from the state"
        ("__Secure-" ^ GSD.cookie_name state)
        name;
      let* loaded = Github_handler_fixture.load_cookie config state [ (name, value) ] in
      Alcotest.(check bool) "cookie binding hash matches the stored row"
        true
        (String.equal (Github_fixture.gsd_binding_hash loaded) stored_binding_hash);
      let verifier =
        match GPK.verifier_of_string (Github_fixture.gsd_verifier loaded) with
        | Ok v -> v
        | Error GPK.Invalid_format ->
            Alcotest.fail "cookie verifier is not canonical"
      in
      Alcotest.(check bool) "challenge is S256 of the verifier" true
        (String.equal (Github_fixture.gsd_challenge loaded)
           (GPK.challenge_to_string (GPK.challenge_of_verifier verifier)));
      (* Only hashes cross the SQL boundary: neither raw token from the
         cookie plaintext appears in any stored text column. *)
      (match String.split_on_char '.' (GSD.encode loaded) with
      | [ _version; raw_binding; raw_verifier ] ->
          Alcotest.(check bool) "raw binding absent from the row" false
            (Html_assert.contains_nonempty ~needle:raw_binding text_columns);
          Alcotest.(check bool) "raw verifier absent from the row" false
            (Html_assert.contains_nonempty ~needle:raw_verifier text_columns)
      | _ -> Alcotest.fail "unexpected cookie plaintext shape");
      Lwt.return_unit)

(* FK failure on a positive-but-nonexistent session user: real storage
   error at the handler boundary without weakening any constraint. *)
let storage_failure_case =
  db_case "storage failure: 503, no cookie, no GitHub Location"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* ghost = C.find q_absent_user_id () in
      let* ghost = Github_handler_fixture.or_fail "absent user id" ghost in
      let* response = Github_handler_fixture.run_start ~url ~session_user_id:ghost in
      Alcotest.(check int) "503" 503 (Http_fixture.status_of response);
      Alcotest.(check (option string)) "no Location" None
        (Dream.header response "Location");
      Alcotest.(check int) "no flow cookie" 0
        (List.length (Github_handler_fixture.flow_cookies response));
      let* count = C.find q_count_for_user ghost in
      let* count = Github_handler_fixture.or_fail "count" count in
      Alcotest.(check int) "no orphan row" 0 count;
      Lwt.return_unit)

let multiple_starts_case =
  db_case "two starts: independent rows and per-flow cookies"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find Github_handler_fixture.q_insert_user "ghstart_multi" in
      let* uid = Github_handler_fixture.or_fail "user" uid in
      let config = Github_fixture.gck_https_config () in
      let start label =
        let* response = Github_handler_fixture.run_start ~url ~session_user_id:uid in
        let _, state, name, value = Github_handler_fixture.successful_start label response in
        Lwt.return (state, name, value)
      in
      let* state_a, name_a, value_a = start "first" in
      let* state_b, name_b, value_b = start "second" in
      Alcotest.(check bool) "distinct state rows" false
        (String.equal
           (GOC.state_hash_to_string (GOC.hash_state state_a))
           (GOC.state_hash_to_string (GOC.hash_state state_b)));
      Alcotest.(check bool) "distinct cookie names" false
        (String.equal name_a name_b);
      let* count = C.find q_count_for_user uid in
      let* count = Github_handler_fixture.or_fail "count" count in
      Alcotest.(check int) "two rows" 2 count;
      (* Neither flow overwrites the other: with both cookies in one
         browser, each state still loads its own material, bound to its
         own row. *)
      let jar = [ (name_a, value_a); (name_b, value_b) ] in
      let* loaded_a = Github_handler_fixture.load_cookie config state_a jar in
      let* loaded_b = Github_handler_fixture.load_cookie config state_b jar in
      let* binding_a, _ = row_of_state (module C) "row A" ~uid state_a in
      let* binding_b, _ = row_of_state (module C) "row B" ~uid state_b in
      Alcotest.(check bool) "A bound to its row" true
        (String.equal (Github_fixture.gsd_binding_hash loaded_a) binding_a);
      Alcotest.(check bool) "B bound to its row" true
        (String.equal (Github_fixture.gsd_binding_hash loaded_b) binding_b);
      Alcotest.(check bool) "flows use distinct bindings" false
        (String.equal binding_a binding_b);
      Lwt.return_unit)

(* --- Analytics: github_app_install_started ---

   The success boundary is exactly "the state row committed AND the valid
   GitHub redirect is the response being returned". Everything below drives
   the real pipeline; the fake sink replaces the HTTP transport, so no
   PostHog request can leave the process. *)

(* run_start with a browser cookie jar, so the plaintext consent cookie
   reaches the handler exactly as a real browser would send it. Session
   identity still comes from the memory-session middleware, never a
   cookie. *)
let run_start_with_cookies ~url ~session_user_id ~cookies ?(mode = Ob.Public)
    ?(origin = Some "https://earde.com") () =
  let handler = Github_handler_fixture.make_start_handler ~mode ~load_config:(fun () -> Http_fixture.ok_loader ()) in
  let pipeline =
    Dream.sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Dream.set_session_field req "user_id" (string_of_int session_user_id)
    in
    handler req
  in
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ match cookies with [] -> [] | c -> [ ("Cookie", Github_fixture.gck_cookie_header c) ]
  in
  pipeline (Dream.request ~method_:`POST ~target:Github_handler_fixture.target ~headers "")

let analytics_success_case =
  db_case
    "analytics: a successful start captures exactly one \
     github_app_install_started, after the state row and with no GitHub \
     material"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find Github_handler_fixture.q_insert_user "ghstart_an_ok" in
      let* uid = Github_handler_fixture.or_fail "user" uid in
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_start_with_cookies ~url ~session_user_id:uid
              ~cookies:[ Analytics_fixture.an_consent_granted ] ())
      in
      let _ = Github_handler_fixture.successful_start "analytics start" response in
      Analytics_fixture.check_single_capture "granted" ~name:"github_app_install_started"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      (* The state row exists, and nothing about it — hash, binding,
         cookie name, or GitHub URL — reached the payload. *)
      let* count = C.find q_count_for_user uid in
      let* count = Github_handler_fixture.or_fail "count" count in
      Alcotest.(check int) "one state row" 1 count;
      Lwt.return_unit)

let analytics_consent_case =
  db_case
    "analytics: denied, missing, and disabled configurations capture \
     nothing while the redirect and the state row are unchanged"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find Github_handler_fixture.q_insert_user "ghstart_an_consent" in
      let* uid = Github_handler_fixture.or_fail "user" uid in
      let run label ?(enabled = true) cookies =
        let* response, captured =
          Analytics_fixture.with_sink_lwt ~enabled (fun () ->
              run_start_with_cookies ~url ~session_user_id:uid ~cookies ())
        in
        (* The business outcome is identical in every configuration. *)
        let _ = Github_handler_fixture.successful_start label response in
        Analytics_fixture.check_no_capture label captured;
        Lwt.return_unit
      in
      let* () = run "denied" [ Analytics_fixture.an_consent_denied ] in
      let* () = run "missing" [] in
      let* () = run "unrelated cookies" [ ("theme", "dark") ] in
      let* () = run "malformed value" [ (An.consent_cookie_name, "yes") ] in
      let* () = run "analytics disabled" ~enabled:false [ Analytics_fixture.an_consent_granted ] in
      let* count = C.find q_count_for_user uid in
      let* count = Github_handler_fixture.or_fail "count" count in
      Alcotest.(check int) "five successful starts, five rows" 5 count;
      Lwt.return_unit)

let analytics_no_event_case =
  db_case
    "analytics: every refused or failed start captures nothing, even with \
     granted consent"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* ghost = C.find q_absent_user_id () in
      let* ghost = Github_handler_fixture.or_fail "absent user id" ghost in
      let* uid = C.find Github_handler_fixture.q_insert_user "ghstart_an_none" in
      let* uid = Github_handler_fixture.or_fail "user" uid in
      (* Storage failure: the state row never commits, so no redirect and
         no event. *)
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_start_with_cookies ~url ~session_user_id:ghost
              ~cookies:[ Analytics_fixture.an_consent_granted ] ())
      in
      Alcotest.(check int) "storage failure 503" 503 (Http_fixture.status_of response);
      Analytics_fixture.check_no_capture "storage failure" captured;
      (* Rollout gate: a non-admin in Admins mode never reaches issuance. *)
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_start_with_cookies ~url ~session_user_id:uid
              ~cookies:[ Analytics_fixture.an_consent_granted ] ~mode:Ob.Admins ())
      in
      Alcotest.(check int) "rollout 403" 403 (Http_fixture.status_of response);
      Analytics_fixture.check_no_capture "rollout gate" captured;
      (* Kill switch. *)
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_start_with_cookies ~url ~session_user_id:uid
              ~cookies:[ Analytics_fixture.an_consent_granted ] ~mode:Ob.Off ())
      in
      Alcotest.(check int) "off 404" 404 (Http_fixture.status_of response);
      Analytics_fixture.check_no_capture "kill switch" captured;
      (* Origin gate. *)
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_start_with_cookies ~url ~session_user_id:uid
              ~cookies:[ Analytics_fixture.an_consent_granted ] ~origin:(Some "https://evil.example")
              ())
      in
      Alcotest.(check int) "cross-origin 403" 403 (Http_fixture.status_of response);
      Analytics_fixture.check_no_capture "origin gate" captured;
      let* count = C.find q_count_for_user uid in
      let* count = Github_handler_fixture.or_fail "count" count in
      Alcotest.(check int) "no state row from a refused start" 0 count;
      Lwt.return_unit)

let db_suite =
  [ success_case; storage_failure_case; multiple_starts_case;
    analytics_success_case; analytics_consent_case; analytics_no_event_case
  ]

let suites =
    (* Start-installation handler gates: DB-free with injected mode and
       config — rejections must produce controlled statuses without
       configuration reads, SQL, or cookies. *)
  [ ( "github_start_handler_gates", gate_suite )
    (* Start-installation handler over the real pipeline (sql_pool +
       secret + memory sessions); EARDE_TEST_DATABASE_URL gate. *)
  ; ( "github_start_handler_db", db_suite )
  ]
