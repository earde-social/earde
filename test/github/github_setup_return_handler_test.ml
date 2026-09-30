module Ob = Earde.Project_onboarding
module GOC = Earde.Github_onboarding_crypto
module GAC = Earde.Github_app_config
module GOU = Earde.Github_onboarding_urls
module GSD = Earde.Github_onboarding_session_data

(* === GitHub onboarding setup-return handler (Github_onboarding_handlers) ===
   GET /integrations/github/install/return: every outcome must be a clean 303
   (no-store / no-cache / no-referrer, empty body) — the browser sits at a
   state-bearing URL, so nothing may render, cache, or leak. DB-free cases
   use injected mode/config and the fixed test secret for real encrypted
   cookies; requests never install session middleware, because the handler
   must not need one. Reaching the missing sql_pool boundary IS the
   assertion that parsing and the cookie gate passed with no SQL. Raw
   states, bindings, verifiers, and cookie values never reach assertion
   output — material comparisons are boolean. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let case = Case.quick
let counting_loader = Http_fixture.counting_loader
let ok_loader = Http_fixture.ok_loader
let status_of = Http_fixture.status_of

(* Deterministic printable state fixture, distinct from the cookie-suite
   fixtures. *)
let fixture_state_string = Github_fixture.goc_fixture 'R'
let fixture_state () = Github_fixture.goc_state_exn fixture_state_string

let ok_target =
  Github_handler_fixture.target_of
    [ "state=" ^ fixture_state_string; "installation_id=12345" ]

(* DB-free run under the fixed test secret (real encrypted-cookie
   behavior), with NO session middleware — the setup return arrives on a
   cross-site redirect and must not depend on one. An exception marks the
   missing sql_pool boundary. *)
let gate_run ?(cookies = []) ?(mode = Ob.Public)
    ?(load_config = fun () -> ok_loader ()) ~target () =
  let handler =
    Github_handler_fixture.make_setup_return_handler ~mode ~load_config
  in
  let headers =
    match cookies with
    | [] -> []
    | pairs -> [ ("Cookie", Github_fixture.gck_cookie_header pairs) ]
  in
  let request = Dream.request ~method_:`GET ~target ~headers "" in
  match
    Lwt_main.run (Dream.set_secret Github_fixture.cookie_secret handler request)
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let gate_response label = function
  | `Response response -> response
  | `Db_boundary -> Alcotest.failf "%s: unexpectedly reached the DB" label

let check_db_boundary label = function
  | `Db_boundary -> ()
  | `Response response ->
      Alcotest.failf "%s: rejected with status %d" label (status_of response)

(* The full clean-redirect contract; the Location is always the literal
   "/bring", so nothing sensitive can be printed. *)
let check_clean label response =
  Alcotest.(check int) (label ^ ": exactly 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some "/bring")
    (Dream.header response "Location");
  Github_handler_fixture.check_safety_headers label response;
  Alcotest.(check string)
    (label ^ ": empty body") ""
    (Lwt_main.run (Dream.body response))

let rejected label target =
  let response = gate_response label (gate_run ~target ()) in
  check_clean label response;
  Alcotest.(check int)
    (label ^ ": no Set-Cookie")
    0
    (List.length (Dream.headers response "Set-Cookie"))

let off_case =
  case "Off: clean /bring redirect, loader and cookie untouched" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      let state = fixture_state () in
      let name, value, _ =
        Github_fixture.gck_stored "off cookie"
          (Github_fixture.gck_https_config ())
          state (GSD.create ())
      in
      let response =
        gate_response "off"
          (gate_run ~mode:Ob.Off ~load_config:loader
             ~cookies:[ (name, value) ]
             ~target:ok_target ())
      in
      check_clean "off" response;
      Alcotest.(check int) "loader never called" 0 !calls;
      Alcotest.(check int)
        "no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie")))

let config_failure_case =
  case "configuration failure: clean /bring redirect" (fun () ->
      let loader, calls =
        counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      let response =
        gate_response "config failure"
          (gate_run ~load_config:loader ~target:ok_target ())
      in
      check_clean "config failure" response;
      Alcotest.(check int) "loader called once" 1 !calls)

let parse_rejection_case =
  case "strict parsing: malformed targets redirect cleanly" (fun () ->
      List.iter
        (fun (label, params) ->
          rejected label (Github_handler_fixture.target_of params))
        [
          ("missing state", [ "installation_id=5" ]);
          ("blank state", [ "state="; "installation_id=5" ]);
          ("bare state key", [ "state"; "installation_id=5" ]);
          ("malformed state", [ "state=not+canonical"; "installation_id=5" ]);
          ( "padded state",
            [ "state=" ^ fixture_state_string ^ "="; "installation_id=5" ] );
          ( "duplicate state",
            [
              "state=" ^ fixture_state_string;
              "state=" ^ fixture_state_string;
              "installation_id=5";
            ] );
          ("missing installation id", [ "state=" ^ fixture_state_string ]);
          ( "blank installation id",
            [ "state=" ^ fixture_state_string; "installation_id=" ] );
          ( "bare installation id key",
            [ "state=" ^ fixture_state_string; "installation_id" ] );
          ( "duplicate installation id",
            [
              "state=" ^ fixture_state_string;
              "installation_id=5";
              "installation_id=5";
            ] );
          ( "uppercase keys do not satisfy the lowercase ones",
            [ "State=" ^ fixture_state_string; "Installation_Id=5" ] );
          ( "fragment cuts the query",
            [ "state=" ^ fixture_state_string ^ "#installation_id=5" ] );
        ];
      rejected "no query at all" Github_handler_fixture.path;
      rejected "empty query" (Github_handler_fixture.path ^ "?"))

let bad_installation_id_case =
  case "strict parsing: invalid installation ids redirect cleanly" (fun () ->
      List.iteri
        (fun i raw ->
          rejected
            (Printf.sprintf "invalid installation id #%d" i)
            (Github_handler_fixture.target_of
               [ "state=" ^ fixture_state_string; "installation_id=" ^ raw ]))
        [
          "0";
          "000";
          "-5";
          "+5";
          " 5";
          "5 ";
          "1 2";
          "1.5";
          "1e3";
          "0x10";
          "abc";
          "9223372036854775808";
          "99999999999999999999";
        ])

(* A valid encrypted cookie plus a passing target must reach the SQL
   boundary: proves acceptance without widening any production API. *)
let with_fixture_cookie label f =
  let state = fixture_state () in
  let name, value, _ =
    Github_fixture.gck_stored (label ^ " cookie")
      (Github_fixture.gck_https_config ())
      state (GSD.create ())
  in
  f (name, value)

let accepted_id_case =
  case "leading zeroes and int64 max parse as positive ids" (fun () ->
      with_fixture_cookie "accepted ids" (fun cookie ->
          List.iteri
            (fun i raw ->
              check_db_boundary
                (Printf.sprintf "accepted installation id #%d" i)
                (gate_run ~cookies:[ cookie ]
                   ~target:
                     (Github_handler_fixture.target_of
                        [
                          "state=" ^ fixture_state_string;
                          "installation_id=" ^ raw;
                        ])
                   ()))
            [ "007"; "9223372036854775807" ]))

let extra_params_case =
  case "unrelated extra query parameters are accepted" (fun () ->
      with_fixture_cookie "extra params" (fun cookie ->
          check_db_boundary "extras ignored"
            (gate_run ~cookies:[ cookie ]
               ~target:
                 (Github_handler_fixture.target_of
                    [
                      "ref=x";
                      "state=" ^ fixture_state_string;
                      "setup_action=install";
                      "State=UP";
                      "bare";
                      "installation_id=12345";
                      "installation_id2=9";
                    ])
               ())))

let no_session_case =
  case "no Dream session is required in Public or Admins mode" (fun () ->
      with_fixture_cookie "no session" (fun cookie ->
          (* No session middleware is installed anywhere in this suite;
             reaching the SQL boundary proves no session field gated the
             way. *)
          check_db_boundary "Public"
            (gate_run ~mode:Ob.Public ~cookies:[ cookie ] ~target:ok_target ());
          check_db_boundary "Admins"
            (gate_run ~mode:Ob.Admins ~cookies:[ cookie ] ~target:ok_target ())))

let missing_cookie_case =
  case "missing per-flow cookie: /bring with no SQL and no deletion" (fun () ->
      let response =
        gate_response "missing cookie" (gate_run ~target:ok_target ())
      in
      check_clean "missing cookie" response;
      Alcotest.(check int)
        "no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie"));
      (* Another state's cookie does not satisfy this flow either. *)
      let other =
        Github_fixture.goc_state_exn (Github_fixture.goc_fixture 'T')
      in
      let name, value, _ =
        Github_fixture.gck_stored "other flow"
          (Github_fixture.gck_https_config ())
          other (GSD.create ())
      in
      let response =
        gate_response "other state's cookie"
          (gate_run ~cookies:[ (name, value) ] ~target:ok_target ())
      in
      check_clean "other state's cookie" response;
      Alcotest.(check int)
        "still no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie")))

let invalid_cookie_case =
  case "invalid per-flow cookie: /bring plus matching deletion" (fun () ->
      let state = fixture_state () in
      let name, value, _ =
        Github_fixture.gck_stored "victim"
          (Github_fixture.gck_https_config ())
          state (GSD.create ())
      in
      let response =
        gate_response "invalid cookie"
          (gate_run ~cookies:[ (name, "AAAA" ^ value) ] ~target:ok_target ())
      in
      check_clean "invalid cookie" response;
      Github_handler_fixture.check_deletion "invalid cookie" ~cookie_name:name
        ~stored_value:value response)

let no_leakage_case =
  case "failure responses never echo state or installation id" (fun () ->
      let response =
        gate_response "leak probe"
          (gate_run
             ~target:
               (Github_handler_fixture.target_of
                  [ "state=" ^ fixture_state_string; "installation_id=987654" ])
             ())
      in
      let body = Lwt_main.run (Dream.body response) in
      let location =
        Option.value (Dream.header response "Location") ~default:""
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            "absent from the body" false
            (Html_assert.contains_nonempty ~needle body);
          Alcotest.(check bool)
            "absent from the Location" false
            (Html_assert.contains_nonempty ~needle location))
        [ fixture_state_string; "987654" ];
      Alcotest.(check bool)
        "local Location carries no query" false
        (Html_assert.contains_nonempty ~needle:"?" location))

let gate_suite =
  [
    off_case;
    config_failure_case;
    parse_rejection_case;
    bad_installation_id_case;
    accepted_id_case;
    extra_params_case;
    no_session_case;
    missing_cookie_case;
    invalid_cookie_case;
    no_leakage_case;
  ]

(* --- Database-gated: real start handler first, then the setup return
   over sql_pool + secret, still with no session middleware. --- *)

let or_fail = Github_handler_fixture.or_fail

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM github_onboarding_states WHERE user_id IN (SELECT id FROM \
       users WHERE username LIKE 'ghsetup_%')";
      "DELETE FROM users WHERE username LIKE 'ghsetup_%'";
    ]

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
                   let* () = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_row =
  Caqti_type.(string ->! t2 (t3 int string string) (t2 (option int64) bool))
    "SELECT user_id, session_binding_hash, flow,\n\
    \          pending_github_installation_id, consumed_at IS NULL\n\
    \   FROM github_onboarding_states WHERE state_hash = $1"

(* Everything except the pending installation id, as one comparable text
   value: proves attach touches nothing else. Compared with boolean
   equality — it contains stored hashes. *)
let q_row_signature =
  Caqti_type.(string ->! string)
    "SELECT ROW(user_id, session_binding_hash, flow, created_at, expires_at, \
     consumed_at)::text\n\
    \   FROM github_onboarding_states WHERE state_hash = $1"

(* The real authenticated start, yielding the state and the per-flow
   cookie pair exactly as the browser would hold them. *)
let start_flow ~url ~uid label =
  let* response = Github_handler_fixture.run_start ~url ~session_user_id:uid in
  let _, state, name, value =
    Github_handler_fixture.successful_start label response
  in
  Lwt.return (state, (name, value))

(* The setup return over the real pipeline. Deliberately no session
   middleware and no session fields: possession of state + cookie must be
   enough. *)
let run_return ~url ?(jar = []) ~state ~installation () =
  let target =
    Github_handler_fixture.target_of
      [
        "state=" ^ GOC.state_to_string state; "installation_id=" ^ installation;
      ]
  in
  let handler =
    Github_handler_fixture.make_setup_return_handler ~mode:Ob.Public
      ~load_config:(fun () -> ok_loader ())
  in
  let pipeline =
    Dream.sql_pool url
    @@ Dream.set_secret Github_fixture.cookie_secret
    @@ handler
  in
  let headers =
    match jar with
    | [] -> []
    | pairs -> [ ("Cookie", Github_fixture.gck_cookie_header pairs) ]
  in
  pipeline (Dream.request ~method_:`GET ~target ~headers "")

(* Full contract of one successful return: exact 303 to the exact
   authorization URL (five parameters, challenge from this flow's cookie
   data), safety headers, empty body, and the per-flow cookie untouched. *)
let check_authorization label ~config ~state ~data response =
  Alcotest.(check int) (label ^ ": exactly 303") 303 (status_of response);
  Github_handler_fixture.check_safety_headers label response;
  let location =
    match Dream.header response "Location" with
    | Some l -> l
    | None -> Alcotest.fail (label ^ ": no Location header")
  in
  Alcotest.(check bool)
    (label ^ ": exact authorization URL")
    true
    (String.equal location
       (GOU.authorization_url config ~state
          ~code_challenge:(GSD.code_challenge data)));
  let uri = Uri.of_string location in
  Alcotest.(check (option string))
    (label ^ ": GitHub host") (Some "github.com") (Uri.host uri);
  Alcotest.(check string)
    (label ^ ": OAuth path") "/login/oauth/authorize" (Uri.path uri);
  Alcotest.(check (list string))
    (label ^ ": exactly the five expected parameters")
    (List.sort compare Github_fixture.gou_authorization_keys)
    (List.sort compare (Github_fixture.gou_keys uri));
  Alcotest.(check bool)
    (label ^ ": state echoes the original")
    true
    (String.equal
       (Github_fixture.gou_single label "state" uri)
       (GOC.state_to_string state));
  Alcotest.(check bool)
    (label ^ ": challenge from the cookie data")
    true
    (String.equal
       (Github_fixture.gou_single label "code_challenge" uri)
       (Github_fixture.gsd_challenge data));
  Alcotest.(check string)
    (label ^ ": S256") "S256"
    (Github_fixture.gou_single label "code_challenge_method" uri);
  Alcotest.(check bool)
    (label ^ ": redirect_uri is the callback")
    true
    (String.equal
       (Github_fixture.gou_single label "redirect_uri" uri)
       (GAC.callback_url config));
  Alcotest.(check bool)
    (label ^ ": configured client id")
    true
    (String.equal
       (Github_fixture.gou_single label "client_id" uri)
       (GAC.client_id config));
  Alcotest.(check bool)
    (label ^ ": raw verifier absent")
    false
    (Html_assert.contains_nonempty
       ~needle:(Github_fixture.gsd_verifier data)
       location);
  Alcotest.(check int)
    (label ^ ": per-flow cookie untouched")
    0
    (List.length (Dream.headers response "Set-Cookie"));
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return location

(* Lwt-safe clean-failure contract for DB cases. *)
let check_clean_lwt label response =
  Alcotest.(check int) (label ^ ": exactly 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some "/bring")
    (Dream.header response "Location");
  Github_handler_fixture.check_safety_headers label response;
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

let fixture_user (module C : Caqti_lwt.CONNECTION) name =
  let* uid = C.find Github_handler_fixture.q_insert_user name in
  or_fail "fixture user" uid

let row_of (module C : Caqti_lwt.CONNECTION) label state =
  let* row = C.find q_row (Github_handler_fixture.state_hash_of state) in
  or_fail (label ^ ": row") row

let signature_of (module C : Caqti_lwt.CONNECTION) label state =
  let* s =
    C.find q_row_signature (Github_handler_fixture.state_hash_of state)
  in
  or_fail (label ^ ": signature") s

let success_case =
  db_case "successful return: attach + exact OAuth authorization redirect"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_user" in
      let config = Github_fixture.gck_https_config () in
      let* state, cookie = start_flow ~url ~uid "setup start" in
      let* sig_before = signature_of (module C) "before" state in
      let* data = Github_handler_fixture.load_cookie config state [ cookie ] in
      let* response =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"987654321" ()
      in
      let* (_ : string) =
        check_authorization "success" ~config ~state ~data response
      in
      let* (row_uid, binding_hash, flow), (pending, consumed_null) =
        row_of (module C) "success" state
      in
      Alcotest.(check int) "ownership stays the fixture user" uid row_uid;
      Alcotest.(check string) "flow" "project_onboarding" flow;
      Alcotest.(check bool)
        "exact pending installation id" true
        (pending = Some 987654321L);
      Alcotest.(check bool) "consumed_at IS NULL" true consumed_null;
      Alcotest.(check bool)
        "cookie binding hash matches the row" true
        (String.equal (Github_fixture.gsd_binding_hash data) binding_hash);
      let* sig_after = signature_of (module C) "after" state in
      Alcotest.(check bool)
        "no other row field changed" true
        (String.equal sig_before sig_after);
      (* Only hashes cross the SQL boundary. *)
      (match String.split_on_char '.' (GSD.encode data) with
      | [ _version; raw_binding; raw_verifier ] ->
          let columns =
            String.concat "|"
              [ Github_handler_fixture.state_hash_of state; binding_hash; flow ]
          in
          Alcotest.(check bool)
            "raw binding absent from the row" false
            (Html_assert.contains_nonempty ~needle:raw_binding columns);
          Alcotest.(check bool)
            "raw verifier absent from the row" false
            (Html_assert.contains_nonempty ~needle:raw_verifier columns)
      | _ -> Alcotest.fail "unexpected cookie plaintext shape");
      Lwt.return_unit)

let retry_case =
  db_case "idempotent retry: same redirect, one pending id, cookie kept"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_retry" in
      let config = Github_fixture.gck_https_config () in
      let* state, cookie = start_flow ~url ~uid "retry start" in
      let* data = Github_handler_fixture.load_cookie config state [ cookie ] in
      let* first =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"424242" ()
      in
      let* first_location =
        check_authorization "first return" ~config ~state ~data first
      in
      let* second =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"424242" ()
      in
      let* second_location =
        check_authorization "second return" ~config ~state ~data second
      in
      Alcotest.(check bool)
        "identical redirects" true
        (String.equal first_location second_location);
      let* _, (pending, consumed_null) = row_of (module C) "retry" state in
      Alcotest.(check bool)
        "still the one pending id" true (pending = Some 424242L);
      Alcotest.(check bool) "still unconsumed" true consumed_null;
      Lwt.return_unit)

let conflict_case =
  db_case "conflicting installation id: /bring + deletion, first id kept"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_conflict" in
      let config = Github_fixture.gck_https_config () in
      let* state, cookie = start_flow ~url ~uid "conflict start" in
      let* data = Github_handler_fixture.load_cookie config state [ cookie ] in
      let* first =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"111" ()
      in
      let* (_ : string) =
        check_authorization "attach A" ~config ~state ~data first
      in
      let* second =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"222" ()
      in
      let* () = check_clean_lwt "conflicting B" second in
      Github_handler_fixture.check_deletion "conflicting B"
        ~cookie_name:(fst cookie) ~stored_value:(snd cookie) second;
      let* _, (pending, consumed_null) = row_of (module C) "conflict" state in
      Alcotest.(check bool) "A retained" true (pending = Some 111L);
      Alcotest.(check bool) "consumed_at remains NULL" true consumed_null;
      Lwt.return_unit)

let expired_case =
  db_case "expired state: /bring + deletion, row untouched"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_expired" in
      let* state, cookie = start_flow ~url ~uid "expired start" in
      let* r =
        C.exec Github_handler_fixture.q_expire
          (Github_handler_fixture.state_hash_of state)
      in
      let* () = or_fail "expire" r in
      let* sig_before = signature_of (module C) "expired before" state in
      let* response =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"333" ()
      in
      let* () = check_clean_lwt "expired" response in
      Github_handler_fixture.check_deletion "expired" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) response;
      let* _, (pending, consumed_null) = row_of (module C) "expired" state in
      Alcotest.(check bool) "no pending id attached" true (pending = None);
      Alcotest.(check bool) "consumed_at remains NULL" true consumed_null;
      let* sig_after = signature_of (module C) "expired after" state in
      Alcotest.(check bool)
        "row untouched" true
        (String.equal sig_before sig_after);
      Lwt.return_unit)

let consumed_case =
  db_case "consumed state: /bring + deletion, row untouched"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_consumed" in
      let* state, cookie = start_flow ~url ~uid "consumed start" in
      let* r =
        C.exec Github_handler_fixture.q_consume_now
          (Github_handler_fixture.state_hash_of state)
      in
      let* () = or_fail "consume" r in
      let* sig_before = signature_of (module C) "consumed before" state in
      let* response =
        run_return ~url ~jar:[ cookie ] ~state ~installation:"444" ()
      in
      let* () = check_clean_lwt "consumed" response in
      Github_handler_fixture.check_deletion "consumed" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) response;
      let* _, (pending, consumed_null) = row_of (module C) "consumed" state in
      Alcotest.(check bool) "no pending id attached" true (pending = None);
      Alcotest.(check bool) "consumed_at preserved" false consumed_null;
      let* sig_after = signature_of (module C) "consumed after" state in
      Alcotest.(check bool)
        "row untouched" true
        (String.equal sig_before sig_after);
      Lwt.return_unit)

let db_missing_cookie_case =
  db_case "missing cookie with a live state: no attachment, no deletion"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_nocookie" in
      let* state, _cookie = start_flow ~url ~uid "cookieless start" in
      let* sig_before = signature_of (module C) "cookieless before" state in
      let* response = run_return ~url ~jar:[] ~state ~installation:"555" () in
      let* () = check_clean_lwt "cookieless" response in
      Alcotest.(check int)
        "no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie"));
      let* _, (pending, consumed_null) = row_of (module C) "cookieless" state in
      Alcotest.(check bool)
        "pending installation id remains NULL" true (pending = None);
      Alcotest.(check bool) "state remains unconsumed" true consumed_null;
      let* sig_after = signature_of (module C) "cookieless after" state in
      Alcotest.(check bool)
        "row untouched" true
        (String.equal sig_before sig_after);
      Lwt.return_unit)

let parallel_case =
  db_case "parallel flows: only flow A attaches, B stays intact"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghsetup_parallel" in
      let config = Github_fixture.gck_https_config () in
      let* state_a, cookie_a = start_flow ~url ~uid "flow A" in
      let* state_b, cookie_b = start_flow ~url ~uid "flow B" in
      let jar = [ cookie_a; cookie_b ] in
      let* data_a = Github_handler_fixture.load_cookie config state_a jar in
      let* sig_b_before = signature_of (module C) "B before" state_b in
      let* response =
        run_return ~url ~jar ~state:state_a ~installation:"777" ()
      in
      (* A's challenge must come from A's cookie material. *)
      let* (_ : string) =
        check_authorization "flow A" ~config ~state:state_a ~data:data_a
          response
      in
      let* _, (pending_a, consumed_a_null) =
        row_of (module C) "row A" state_a
      in
      Alcotest.(check bool)
        "A carries its pending id" true (pending_a = Some 777L);
      Alcotest.(check bool) "A unconsumed" true consumed_a_null;
      let* (_, binding_b, _), (pending_b, consumed_b_null) =
        row_of (module C) "row B" state_b
      in
      Alcotest.(check bool) "B untouched" true (pending_b = None);
      Alcotest.(check bool) "B unconsumed" true consumed_b_null;
      let* sig_b_after = signature_of (module C) "B after" state_b in
      Alcotest.(check bool)
        "B row unchanged" true
        (String.equal sig_b_before sig_b_after);
      (* B's cookie still loads and still matches B's row. *)
      let* data_b = Github_handler_fixture.load_cookie config state_b jar in
      Alcotest.(check bool)
        "B's cookie remains valid for B's row" true
        (String.equal (Github_fixture.gsd_binding_hash data_b) binding_b);
      Lwt.return_unit)

let db_suite =
  [
    success_case;
    retry_case;
    conflict_case;
    expired_case;
    consumed_case;
    db_missing_cookie_case;
    parallel_case;
  ]

let suites =
  (* Setup-return handler gates: DB-free with injected mode/config and
       real encrypted cookies — every outcome must be a clean 303 with the
       no-store/no-cache/no-referrer headers, never a rendered page. *)
  [
    ("github_setup_return_gates", gate_suite)
    (* Setup-return over the real pipeline (start handler first, then
       sql_pool + secret, no session middleware); EARDE_TEST_DATABASE_URL
       gate. *);
    ("github_setup_return_db", db_suite);
  ]
