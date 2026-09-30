module Ob = Earde.Project_onboarding
module Psp = Earde.Project_setup_pages

(* === Project setup handlers (Project_setup_handlers) ===
   GET /projects/new and POST /projects/new/repositories. The factories take
   the mode (and, for the POST, the config loader) by injection, so gate
   cases run DB-free with fixed values and never touch the process
   environment; a request that passes every gate stops at the missing
   sql_pool — reaching that boundary IS the assertion that the gates let it
   through with no SQL executed. The POST's CSRF cases run DB-free too:
   Dream's own form verification rejects before any SQL, and tokens are
   minted through the real Dream API against the same memory-session store.
   Database-gated cases run the real pipeline (sql_pool + secret + memory
   sessions) under the usual EARDE_TEST_DATABASE_URL opt-in, with their own
   reserved external-installation-id range 941000001..941000999 and
   psetup_% usernames so no suite shares fixtures. Raw query fixtures and
   form values are asserted only through boolean containment, so no
   submitted value can reach test output. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let case = Case.quick

module Psh = Earde.Project_setup_handlers
module Sel = Earde.Project_onboarding_draft_selection_store
module Store = Earde.Project_onboarding_draft_store

let get_target = "/projects/new"
let post_target = "/projects/new/repositories"
let make_get ~mode = Psh.make_new_project_handler ~mode

let make_post ~mode ~load_config =
  Psh.make_repository_selection_handler ~mode ~load_config

let counting_loader = Http_fixture.counting_loader
let ok_loader = Http_fixture.ok_loader
let status_of = Http_fixture.status_of

(* --- DB-free gate harness (mirrors Gh_start_handler) --- *)

let gate_run ?session ?(headers = []) ~method_ ~target handler =
  let pipeline =
    match session with
    | None -> handler
    | Some fields ->
        Dream.memory_sessions (fun req ->
            let* () =
              Lwt_list.iter_s
                (fun (k, v) -> Dream.set_session_field req k v)
                fields
            in
            handler req)
  in
  let request = Dream.request ~method_ ~target ~headers "" in
  match Lwt_main.run (pipeline request) with
  | response -> `Response response
  | exception _ -> `Db_boundary

let gate_response label = function
  | `Response response -> response
  | `Db_boundary -> Alcotest.failf "%s: unexpectedly reached the DB" label

let check_db_boundary label = function
  | `Db_boundary -> ()
  | `Response response ->
      Alcotest.failf "%s: gate rejected with status %d" label
        (status_of response)

let logged_in = [ ("user_id", "42") ]
let admin_session = [ ("user_id", "42"); ("is_admin", "true") ]

(* Every redirect in this feature: explicit 303, empty body, and the full
   privacy header set. *)
let check_clean_redirect label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string))
    (label ^ ": no-referrer") (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  Alcotest.(check string)
    (label ^ ": empty body") ""
    (Lwt_main.run (Dream.body response))

(* --- GET access gates --- *)

let get_off_case =
  case "GET off: clean /bring redirect before query or SQL" (fun () ->
      (* A hostile query rides along: Off must not read it. *)
      let response =
        gate_response "off"
          (gate_run ~session:admin_session ~method_:`GET
             ~target:(get_target ^ "?draft=zz9zz&selection=saved")
             (make_get ~mode:Ob.Off))
      in
      check_clean_redirect "off" "/bring" response;
      let body = Lwt_main.run (Dream.body response) in
      Alcotest.(check bool)
        "query never reflected" false
        (Html_assert.contains body "zz9zz"))

let get_anonymous_case =
  case "GET: anonymous and invalid sessions redirect to /login" (fun () ->
      let expect_login label run =
        check_clean_redirect label "/login" (gate_response label run)
      in
      expect_login "no session"
        (gate_run ~method_:`GET ~target:get_target (make_get ~mode:Ob.Public));
      List.iter
        (fun raw ->
          expect_login ("user_id " ^ raw)
            (gate_run
               ~session:[ ("user_id", raw) ]
               ~method_:`GET ~target:get_target (make_get ~mode:Ob.Public)))
        [ "not-a-number"; ""; "0"; "-3" ];
      (* is_admin has no meaning without a valid user id. *)
      expect_login "is_admin only"
        (gate_run
           ~session:[ ("is_admin", "true") ]
           ~method_:`GET ~target:get_target (make_get ~mode:Ob.Admins)))

let get_rollout_case =
  case "GET admins mode: non-admin to /bring, admin continues" (fun () ->
      check_clean_redirect "non-admin" "/bring"
        (gate_response "non-admin"
           (gate_run ~session:logged_in ~method_:`GET ~target:get_target
              (make_get ~mode:Ob.Admins)));
      check_clean_redirect "is_admin false" "/bring"
        (gate_response "is_admin false"
           (gate_run
              ~session:(logged_in @ [ ("is_admin", "false") ])
              ~method_:`GET ~target:get_target (make_get ~mode:Ob.Admins)));
      check_db_boundary "admin continues"
        (gate_run ~session:admin_session ~method_:`GET ~target:get_target
           (make_get ~mode:Ob.Admins));
      check_db_boundary "public user continues"
        (gate_run ~session:logged_in ~method_:`GET ~target:get_target
           (make_get ~mode:Ob.Public)))

let get_gate_suite = [ get_off_case; get_anonymous_case; get_rollout_case ]

(* --- POST access gates and same-origin policy --- *)

let post_run ?session ?(headers = []) ~mode ~load_config () =
  gate_run ?session ~headers ~method_:`POST ~target:post_target
    (make_post ~mode ~load_config)

let post_off_case =
  case "POST off: clean /bring redirect, loader untouched" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      let response =
        gate_response "off"
          (post_run ~session:admin_session ~mode:Ob.Off ~load_config:loader ())
      in
      check_clean_redirect "off" "/bring" response;
      Alcotest.(check int) "loader never called" 0 !calls)

let post_anonymous_case =
  case "POST: anonymous and invalid sessions to /login, loader untouched"
    (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      check_clean_redirect "anonymous" "/login"
        (gate_response "anonymous"
           (post_run ~mode:Ob.Public ~load_config:loader ()));
      List.iter
        (fun raw ->
          check_clean_redirect ("user_id " ^ raw) "/login"
            (gate_response ("user_id " ^ raw)
               (post_run
                  ~session:[ ("user_id", raw) ]
                  ~mode:Ob.Public ~load_config:loader ())))
        [ "not-a-number"; ""; "0"; "-3" ];
      check_clean_redirect "is_admin only" "/login"
        (gate_response "is_admin only"
           (post_run
              ~session:[ ("is_admin", "true") ]
              ~mode:Ob.Admins ~load_config:loader ()));
      Alcotest.(check int) "loader never called" 0 !calls)

let post_rollout_case =
  case "POST admins mode: non-admin to /bring before configuration" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      check_clean_redirect "non-admin" "/bring"
        (gate_response "non-admin"
           (post_run ~session:logged_in ~mode:Ob.Admins ~load_config:loader ()));
      Alcotest.(check int) "loader never called" 0 !calls;
      (* Passing the gates and the origin check, the missing content type
         is the next rejection: reaching that 400 proves mode,
         authentication, rollout, configuration, and origin all passed —
         with no SQL (no sql_pool is installed). *)
      let continues label session mode =
        let response =
          gate_response label
            (post_run ~session
               ~headers:[ ("Origin", "https://earde.com") ]
               ~mode
               ~load_config:(fun () -> ok_loader ())
               ())
        in
        Alcotest.(check int)
          (label ^ ": 400 content-type")
          400 (status_of response)
      in
      continues "admin continues" admin_session Ob.Admins;
      continues "public user continues" logged_in Ob.Public)

let post_config_failure_case =
  case "POST: configuration error is a generic 503, no form, no SQL" (fun () ->
      let loader, calls =
        counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      let response =
        gate_response "config failure"
          (post_run ~session:logged_in
             ~headers:
               [
                 ("Origin", "https://earde.com");
                 ("Content-Type", "application/x-www-form-urlencoded");
               ]
             ~mode:Ob.Public ~load_config:loader ())
      in
      Alcotest.(check int) "503" 503 (status_of response);
      Alcotest.(check int) "loader called once" 1 !calls;
      let body = Lwt_main.run (Dream.body response) in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("body does not leak " ^ needle)
            false
            (Html_assert.contains body needle))
        [ "EARDE_PUBLIC_ORIGIN"; "Missing"; "Invalid"; "public_origin" ])

(* One origin-gate run: Public mode, valid session, fixed config. *)
let origin_run label ?sec_fetch_site origin =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @
    match sec_fetch_site with
    | Some v -> [ ("Sec-Fetch-Site", v) ]
    | None -> []
  in
  post_run ~session:logged_in ~headers ~mode:Ob.Public
    ~load_config:(fun () -> ok_loader ())
    ()
  |> gate_response label

let origin_rejected label ?sec_fetch_site origin =
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int) (label ^ ": 403") 403 (status_of response)

(* Passing the origin gate surfaces as the content-type 400 (no form
   Content-Type is sent), never as the origin 403. *)
let origin_accepted label ?sec_fetch_site origin =
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int)
    (label ^ ": passes the origin gate")
    400 (status_of response)

let post_origin_case =
  case "POST origin gate: exact policy of the GitHub start endpoint" (fun () ->
      origin_accepted "exact origin" (Some "https://earde.com");
      origin_accepted "explicit default port" (Some "https://earde.com:443");
      origin_accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
      origin_rejected "cross-origin" (Some "https://evil.example");
      origin_rejected "same-site subdomain" (Some "https://www.earde.com");
      origin_rejected "wrong scheme" (Some "http://earde.com");
      origin_rejected "wrong port" (Some "https://earde.com:8443");
      origin_rejected "null" (Some "null");
      origin_rejected "blank" (Some "");
      origin_rejected "trailing path" (Some "https://earde.com/");
      origin_rejected "mismatch beats same-origin metadata"
        ~sec_fetch_site:"same-origin" (Some "https://evil.example");
      origin_rejected "no signals" None;
      origin_rejected "cross-site" ~sec_fetch_site:"cross-site" None;
      origin_rejected "same-site" ~sec_fetch_site:"same-site" None;
      origin_rejected "none" ~sec_fetch_site:"none" None;
      (* The rejection page never reflects the supplied origin. *)
      let response = origin_run "reflection" (Some "https://evil.example") in
      let body = Lwt_main.run (Dream.body response) in
      Alcotest.(check bool)
        "origin not reflected" false
        (Html_assert.contains body "evil.example"))

let post_gate_suite =
  [
    post_off_case;
    post_anonymous_case;
    post_rollout_case;
    post_config_failure_case;
    post_origin_case;
  ]

(* --- POST CSRF handling (DB-free: every rejection precedes SQL) --- *)

let form_body fields =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ v) fields)

let session_cookie label response =
  match
    List.find_opt
      (fun v -> Html_assert.contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | None -> Alcotest.fail (label ^ ": no session cookie")
  | Some v -> (
      match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)

(* One shared pipeline so memory sessions persist across requests: GET
   mints a fresh and a pre-expired CSRF token for the session through the
   real Dream API; POST runs the real handler. *)
let csrf_pipeline () =
  Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n" ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ -> make_post ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) req

let mint_tokens label pipeline =
  let response =
    Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target:"/mint" ""))
  in
  let cookie = session_cookie label response in
  match String.split_on_char '\n' (Lwt_main.run (Dream.body response)) with
  | [ fresh; expired ] -> (cookie, fresh, expired)
  | _ -> Alcotest.fail (label ^ ": unexpected mint body")

let csrf_post ?cookie ?(content_type = true) pipeline fields =
  let headers =
    [ ("Origin", "https://earde.com") ]
    @ (if content_type then
         [ ("Content-Type", "application/x-www-form-urlencoded") ]
       else [])
    @ match cookie with Some c -> [ ("Cookie", c) ] | None -> []
  in
  match
    Lwt_main.run
      (pipeline
         (Dream.request ~method_:`POST ~target:post_target ~headers
            (form_body fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  let response = gate_response label result in
  Alcotest.(check int) (label ^ ": status") expected (status_of response)

let csrf_case =
  case "POST CSRF: Dream verification gates every submission" (fun () ->
      let pipeline = csrf_pipeline () in
      let cookie, fresh, expired = mint_tokens "mint" pipeline in
      (* Missing, invalid, expired, duplicated, wrong-session: one
         generic 403 each; wrong content type is the generic 400. *)
      csrf_rejected "missing token" 403
        (csrf_post ~cookie pipeline [ ("draft_id", "1") ]);
      csrf_rejected "invalid token" 403
        (csrf_post ~cookie pipeline
           [ ("draft_id", "1"); ("dream.csrf", "not-a-token") ]);
      csrf_rejected "expired token" 403
        (csrf_post ~cookie pipeline
           [ ("draft_id", "1"); ("dream.csrf", expired) ]);
      csrf_rejected "duplicate tokens" 403
        (csrf_post ~cookie pipeline
           [ ("draft_id", "1"); ("dream.csrf", fresh); ("dream.csrf", fresh) ]);
      (* A fresh session (no cookie) has a different session label. *)
      csrf_rejected "wrong session" 403
        (csrf_post pipeline [ ("draft_id", "1"); ("dream.csrf", fresh) ]);
      csrf_rejected "wrong content type" 400
        (csrf_post ~cookie ~content_type:false pipeline
           [ ("draft_id", "1"); ("dream.csrf", fresh) ]);
      (* A verified token with valid application fields reaches the DB
         boundary: CSRF passed and Dream stripped its own field before
         the strict parser (which rejects any unknown field). *)
      check_db_boundary "verified form continues"
        (csrf_post ~cookie pipeline
           [ ("draft_id", "1"); ("repository", "5"); ("dream.csrf", fresh) ]))

let csrf_invalid_form_case =
  case "POST: parser rejection is a clean ?selection=invalid redirect"
    (fun () ->
      let pipeline = csrf_pipeline () in
      let cookie, fresh, _ = mint_tokens "mint" pipeline in
      List.iter
        (fun (label, fields) ->
          let response =
            gate_response label
              (csrf_post ~cookie pipeline (fields @ [ ("dream.csrf", fresh) ]))
          in
          check_clean_redirect label "/projects/new?selection=invalid" response)
        [
          ("unknown field", [ ("draft_id", "1"); ("primary", "2") ]);
          ("duplicate draft id", [ ("draft_id", "1"); ("draft_id", "1") ]);
          ("malformed draft id", [ ("draft_id", "zz9zz") ]);
          ( "duplicate repository",
            [ ("draft_id", "1"); ("repository", "4"); ("repository", "4") ] );
          ("malformed repository", [ ("draft_id", "1"); ("repository", "x") ]);
          ("missing draft id", [ ("repository", "4") ]);
        ])

let csrf_suite = [ csrf_case; csrf_invalid_form_case ]

(* --- Database-gated: the real read and write paths --- *)

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect

(* Fixtures — reserved external-installation-id range
   941000001..941000999 and psetup_% usernames so cleanup is targeted and
   idempotent. Drafts go first (installations are RESTRICT-protected while
   referenced); snapshots cascade from drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 941000001 AND 941000999)";
      "DELETE FROM users WHERE username LIKE 'psetup_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       941000001 AND 941000999";
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
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let repo ~account_id ?description ?default_branch ?archived ~id name =
  Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"psetup-owner"
    ?description ?default_branch ?archived ~id ~name ()

(* Draft fixtures go through the real store against the real client chain,
   exactly as production writes them. *)
let make_draft ?connected_by ?(account_type = "user") ?(target = "User")
    ?(login = "psetup-owner") conn ~user ~ext_id repos =
  let account_id = Int64.add ext_id 100000L in
  let* inst =
    Project_fixture.insert_installation ~login ~account_type ?connected_by conn
      ~ext_id ~account_id
  in
  let* v =
    Project_fixture.verified ~installation_id:ext_id ~account_id ~login ~target
      ()
  in
  let* set = Project_fixture.repo_set ~installation:v (repos account_id) in
  let* draft = Project_fixture.refresh_ok "fixture refresh" conn ~user v set in
  Lwt.return (inst, Store.draft_id draft)

(* Rebuilds the snapshot through the real refresh, as a new GitHub
   verification would: new rows, new local ids. *)
let refresh_snapshot ?(login = "psetup-owner") ?(target = "User") conn ~user
    ~ext_id repos =
  let account_id = Int64.add ext_id 100000L in
  let* v =
    Project_fixture.verified ~installation_id:ext_id ~account_id ~login ~target
      ()
  in
  let* set = Project_fixture.repo_set ~installation:v (repos account_id) in
  let* _ = Project_fixture.refresh_ok "snapshot refresh" conn ~user v set in
  Lwt.return_unit

let snapshot_ids conn draft =
  collect conn "snapshot ids" Project_fixture.q_snapshot_ids draft

let flags conn draft = collect conn "flags" Project_fixture.q_flags draft
let flag_sig = Project_fixture.flag_sig

let check_flags label expected conn draft =
  let* stored = flags conn draft in
  Alcotest.(check (list string)) label expected stored;
  Lwt.return_unit

(* The real production shape for one authenticated user: sql_pool +
   secret + memory sessions, GET and POST dispatched to the two real
   handlers, Public mode, fixed valid configuration. *)
let app_pipeline ~url ~session_user_id =
  Dream.sql_pool url
  @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ fun req ->
  let* () =
    Dream.set_session_field req "user_id" (string_of_int session_user_id)
  in
  match Dream.method_ req with
  | `GET -> make_get ~mode:Ob.Public req
  | _ -> make_post ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) req

let do_get ?cookie ?(target = get_target) pipeline =
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = pipeline (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

let check_page label response =
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy")

(* The rendered feature fragment of a 200 page. *)
let get_fragment label ?cookie ?target pipeline =
  let* response, body = do_get ?cookie ?target pipeline in
  check_page label response;
  Lwt.return (Html_assert.panel_fragment body)

let csrf_field_marker = "name=\"dream.csrf\" type=\"hidden\" value=\""

let csrf_of_page label html =
  match Html_assert.index_from html csrf_field_marker 0 with
  | None -> Alcotest.fail (label ^ ": no framework CSRF field")
  | Some i -> (
      let start = i + String.length csrf_field_marker in
      match String.index_from_opt html start '"' with
      | None -> Alcotest.fail (label ^ ": unterminated CSRF value")
      | Some e -> String.sub html start (e - start))

(* One GET that opens the configuration form: page, session cookie, and
   CSRF token for the follow-up POST. *)
let open_form label ?target pipeline =
  let* response, body = do_get ?target pipeline in
  check_page label response;
  let cookie = session_cookie label response in
  let token = csrf_of_page label body in
  Lwt.return (cookie, token, Html_assert.panel_fragment body)

let do_post ?(origin = Some "https://earde.com") ~cookie ~fields pipeline =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [
        ("Content-Type", "application/x-www-form-urlencoded"); ("Cookie", cookie);
      ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target:post_target ~headers
       (form_body fields))

let check_redirect_lwt label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string))
    (label ^ ": no-referrer") (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

let must frag s =
  Alcotest.(check bool) ("contains: " ^ s) true (Html_assert.contains frag s)

let must_not frag s =
  Alcotest.(check bool)
    ("must not contain: " ^ s) false
    (Html_assert.contains frag s)

(* === GET behavior === *)

let get_empty_and_grammar_case =
  db_case "GET: empty state, feedback grammar, strict draft selector"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* frag = get_fragment "plain" pipeline in
      must frag "ps-empty";
      must_not frag "<form";
      must_not frag "ps-alert";
      (* Recognized one-time feedback values, cosmetic only. *)
      let feedback_expectations =
        [
          ("?selection=saved", Some "Repository selection saved.");
          ("?selection=stale", Some "repository list changed");
          ("?selection=invalid", Some "We couldn't save");
          ("?selection=unavailable", Some "no longer available");
          ("?selection=required", Some "Select at least one public repository");
          ("?selection=saved&selection=saved", None);
          ("?selection=required&selection=required", None);
          ("?selection=", None);
          ("?selection", None);
          ("?selection=Saved", None);
          ("?selection=Required", None);
          ("?selection=zzfeedbackzz", None);
          ("?unrelated=zz9zz", None);
        ]
      in
      let* () =
        Lwt_list.iter_s
          (fun (query, expected) ->
            let* frag =
              get_fragment ("feedback " ^ query) ~target:(get_target ^ query)
                pipeline
            in
            (match expected with
            | Some copy -> must frag copy
            | None -> must_not frag "ps-alert");
            (* The raw query value never enters the page. *)
            must_not frag "zzfeedbackzz";
            must_not frag "zz9zz";
            Lwt.return_unit)
          feedback_expectations
      in
      (* Invalid explicit draft selectors: never a database id, always the
         normal state with the generic unavailable feedback — which also
         overrides a supplied success value. *)
      let invalid_selectors =
        [
          "?draft=";
          "?draft";
          "?draft=zz9zz";
          "?draft=0";
          "?draft=000";
          "?draft=-1";
          "?draft=%2B1";
          "?draft=%201";
          "?draft=1.0";
          "?draft=0x10";
          "?draft=9223372036854775808";
          "?draft=1&draft=1";
          "?draft=%31";
          "?draft=zz9zz&selection=saved";
        ]
      in
      let* () =
        Lwt_list.iter_s
          (fun query ->
            let* frag =
              get_fragment ("selector " ^ query) ~target:(get_target ^ query)
                pipeline
            in
            must frag "no longer available";
            must_not frag "Repository selection saved.";
            must_not frag "zz9zz";
            must_not frag "0x10";
            must_not frag "%31";
            Lwt.return_unit)
          invalid_selectors
      in
      (* A well-formed id that names no available draft collapses to the
         same unavailable feedback. *)
      let* absent =
        find conn "absent id" Project_fixture.q_absent_draft_id ()
      in
      let* frag =
        get_fragment "absent draft"
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target absent)
          pipeline
      in
      must frag "no longer available";
      Lwt.return_unit)

let get_single_draft_case =
  db_case "GET: one draft renders its configuration, ids stay private"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000011L (fun account_id ->
            [
              repo ~account_id ~id:941600111L "alpha";
              repo ~account_id ~id:941600112L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let check_configuration label frag =
        must frag "<form method='POST' action='/projects/new/repositories'";
        must frag
          (Printf.sprintf "<input type='hidden' name='draft_id' value='%Ld'>"
             draft);
        must frag (Printf.sprintf "name='repository' value='%Ld'" s1);
        must frag (Printf.sprintf "name='repository' value='%Ld'" s2);
        must frag "psetup-owner/alpha";
        must frag "psetup-owner/beta";
        must frag "Personal account";
        must frag csrf_field_marker;
        (* No installation, account, or GitHub repository id leaks. *)
        must_not frag "941000011";
        must_not frag "941100011";
        must_not frag "941600111";
        must_not frag "941600112";
        must_not frag " checked";
        ignore label
      in
      let* frag = get_fragment "implicit" pipeline in
      check_configuration "implicit" frag;
      let* frag =
        get_fragment "explicit"
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target draft)
          pipeline
      in
      check_configuration "explicit" frag;
      (* Leading zeroes cannot alias a different draft. *)
      let* frag =
        get_fragment "leading zeroes"
          ~target:(Printf.sprintf "%s?draft=00%Ld" get_target draft)
          pipeline
      in
      check_configuration "leading zeroes" frag;
      Lwt.return_unit)

let get_chooser_case =
  db_case "GET: several drafts render the chooser in read-model order"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* other = insert_user conn "psetup_b" in
      let* _, older =
        make_draft ~login:"psetup-older" conn ~user:uid ~ext_id:941000021L
          (fun account_id -> [ repo ~account_id ~id:941600211L "older-repo" ])
      in
      let* _, newer =
        make_draft ~login:"psetup-newer" ~account_type:"organization"
          ~target:"Organization" conn ~user:uid ~ext_id:941000022L
          (fun account_id -> [ repo ~account_id ~id:941600221L "newer-repo" ])
      in
      let* _, foreign =
        make_draft ~login:"psetup-foreign" conn ~user:other ~ext_id:941000023L
          (fun account_id -> [ repo ~account_id ~id:941600231L "foreign-repo" ])
      in
      (* Backdate the first draft's activity so updated_at DESC decides. *)
      let* () =
        exec conn "backdate" Project_fixture.q_backdate_updated (older, 3)
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* frag = get_fragment "chooser" pipeline in
      must frag "psetup-newer";
      must frag "psetup-older";
      must frag "Organization";
      must frag "Personal account";
      must frag (Printf.sprintf "href='/projects/new?draft=%Ld'" newer);
      must frag (Printf.sprintf "href='/projects/new?draft=%Ld'" older);
      must_not frag "<form";
      must_not frag " checked";
      must_not frag "psetup-foreign";
      (match
         ( Html_assert.index_from frag "psetup-newer" 0,
           Html_assert.index_from frag "psetup-older" 0 )
       with
      | Some i, Some j ->
          Alcotest.(check bool) "newest activity first" true (i < j)
      | _ -> Alcotest.fail "both chooser options expected");
      (* A foreign explicit draft collapses to the normal chooser with the
         generic unavailable feedback. *)
      let* frag =
        get_fragment "foreign explicit"
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target foreign)
          pipeline
      in
      must frag "no longer available";
      must frag "psetup-newer";
      must_not frag "psetup-foreign";
      must_not frag "<form";
      Lwt.return_unit)

let get_hidden_states_case =
  db_case "GET: expired, terminal, and revoked drafts stay hidden"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, expired =
        make_draft ~login:"psetup-expired" conn ~user:uid ~ext_id:941000031L
          (fun account_id -> [ repo ~account_id ~id:941600311L "expired-repo" ])
      in
      let* _, kept =
        make_draft ~login:"psetup-kept" conn ~user:uid ~ext_id:941000032L
          (fun account_id -> [ repo ~account_id ~id:941600321L "kept-repo" ])
      in
      let* rev_inst, _ =
        make_draft ~login:"psetup-revoked" conn ~user:uid ~ext_id:941000033L
          (fun account_id -> [ repo ~account_id ~id:941600331L "revoked-repo" ])
      in
      let* () = exec conn "expire" Project_fixture.q_backdate_draft expired in
      let* () =
        exec conn "revoke" Project_fixture.q_set_installation_status
          (rev_inst, "revoked", true)
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      (* Only the kept draft survives, so the single-draft path renders
         its configuration directly — nothing hidden resurfaces. *)
      let* frag = get_fragment "one left" pipeline in
      must frag "psetup-kept";
      must frag "<form";
      must_not frag "psetup-expired";
      must_not frag "psetup-revoked";
      let* frag =
        get_fragment "expired explicit"
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target expired)
          pipeline
      in
      must frag "no longer available";
      must_not frag "psetup-expired";
      (* Completing the last draft leaves the plain empty state. *)
      let* () = exec conn "complete" Project_fixture.q_complete_draft kept in
      let* frag = get_fragment "none left" pipeline in
      must frag "ps-empty";
      must_not frag "<form";
      must_not frag "psetup-kept";
      Lwt.return_unit)

let get_selection_state_case =
  db_case "GET: checkbox state mirrors the durable selection" (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000041L (fun account_id ->
            [
              repo ~account_id ~id:941600411L "alpha";
              repo ~account_id ~id:941600412L "beta";
              repo ~account_id ~id:941600413L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s3 = List.nth ids 2 in
      let* r =
        Sel.replace conn ~user_id:uid ~draft_id:draft
          ~selected_snapshot_ids:[ s1; s3 ] ~primary_snapshot_id:None
      in
      (match r with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "seed selection failed");
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* frag = get_fragment "selection" pipeline in
      must frag (Printf.sprintf "value='%Ld' checked" s1);
      must frag (Printf.sprintf "value='%Ld' checked" s3);
      Alcotest.(check int)
        "exactly two checked" 2
        (Html_assert.occurrences frag " checked");
      Lwt.return_unit)

let get_server_error_case =
  db_case "GET: durable inconsistency is the generic 500" (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let account_id = 941100051L in
      let* inst =
        Project_fixture.insert_installation ~login:"psetup-owner" conn
          ~ext_id:941000051L ~account_id
      in
      (* The zero-snapshot corruption: an otherwise available owned draft
         the store can never produce. *)
      let* bare =
        find conn "bare draft" Project_fixture.q_insert_bare_draft (uid, inst)
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* response, body =
        do_get ~target:(Printf.sprintf "%s?draft=%Ld" get_target bare) pipeline
      in
      Alcotest.(check int) "500" 500 (status_of response);
      Alcotest.(check (option string))
        "no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      List.iter (must_not body) [ "Caqti"; "PostgreSQL"; "SELECT " ];
      (* The corrupted draft is invisible to the list, so the plain page
         still renders the safe empty state. *)
      let* frag = get_fragment "list unaffected" pipeline in
      must frag "ps-empty";
      Lwt.return_unit)

(* === step=details: raw grammar, identity rendering, isolation === *)

let seed_selection label conn ~user_id ~draft_id ids =
  let* r =
    Sel.replace conn ~user_id ~draft_id ~selected_snapshot_ids:ids
      ~primary_snapshot_id:None
  in
  (match r with
  | Ok () -> ()
  | Error _ -> Alcotest.fail (label ^ ": seed selection failed"));
  Lwt.return_unit

(* The identity step's stable markers. "action='/projects'" needs the
   closing quote so it can never match the repository form's action. *)
let check_identity_step label frag =
  must frag "Project details";
  must frag "<form method='POST' action='/projects'";
  must_not frag "action='/projects/new/repositories'";
  ignore label

let check_repository_step label frag =
  must frag "<form method='POST' action='/projects/new/repositories'";
  must_not frag "Project details";
  must_not frag "ps-id-form";
  must_not frag "name='slug'";
  must_not frag "name='kind'";
  ignore label

let get_step_grammar_case =
  db_case "GET: strict step=details grammar, raw values never reflected"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000081L (fun account_id ->
            [
              repo ~account_id ~id:941600811L "alpha";
              repo ~account_id ~id:941600812L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let* () =
        seed_selection "grammar" conn ~user_id:uid ~draft_id:draft
          [ List.nth ids 0 ]
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let target query =
        Printf.sprintf "%s?draft=%Ld%s" get_target draft query
      in
      (* Exactly one lowercase step=details enters the identity step;
         unrelated keys ride along ignored. *)
      let* () =
        Lwt_list.iter_s
          (fun query ->
            let* frag =
              get_fragment ("details " ^ query) ~target:(target query) pipeline
            in
            check_identity_step ("details " ^ query) frag;
            must_not frag "zz9zz";
            Lwt.return_unit)
          [ "&step=details"; "&step=details&unrelated=zz9zz" ]
      in
      (* Every other shape is the normal repository step — and produces
         no parsing diagnostic (no alert at all). *)
      let* () =
        Lwt_list.iter_s
          (fun query ->
            let* frag =
              get_fragment ("repository " ^ query) ~target:(target query)
                pipeline
            in
            check_repository_step ("repository " ^ query) frag;
            must_not frag "ps-alert";
            (* Raw step fixtures never enter the page. *)
            must_not frag "zzstepzz";
            must_not frag "%64etails";
            must_not frag "details%20";
            Lwt.return_unit)
          [
            "";
            "&step=";
            "&step";
            "&step=details&step=details";
            "&step=zzstepzz";
            "&step=Details";
            "&Step=details";
            "&step=%64etails";
            "&step=details%20";
          ]
      in
      Lwt.return_unit)

let get_identity_step_case =
  db_case "GET step=details: identity form maps only selected rows"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000082L (fun account_id ->
            [
              repo ~account_id ~id:941600821L "alpha";
              repo ~account_id ~id:941600822L "beta";
              repo ~account_id ~archived:true ~id:941600823L "legacy";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 and s3 = List.nth ids 2 in
      (* Submitted out of snapshot order: the page must follow the
         snapshot, not the submission. *)
      let* () =
        seed_selection "identity" conn ~user_id:uid ~draft_id:draft [ s3; s1 ]
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* frag =
        get_fragment "identity"
          ~target:(Printf.sprintf "%s?draft=%Ld&step=details" get_target draft)
          pipeline
      in
      check_identity_step "identity" frag;
      must frag "Verified through GitHub";
      Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
      (* Selected rows only, snapshot order, archived state preserved;
         the unselected row is absent everywhere. *)
      must frag
        (Printf.sprintf "<option value='%Ld'>psetup-owner/alpha</option>" s1);
      must frag
        (Printf.sprintf
           "<option value='%Ld'>psetup-owner/legacy (Archived)</option>" s3);
      Html_assert.order frag
        (Printf.sprintf "value='%Ld'" s1)
        (Printf.sprintf "value='%Ld'" s3);
      Alcotest.(check int)
        "one summary archived marker" 1
        (Html_assert.occurrences frag "ps-repo-archived");
      must_not frag "psetup-owner/beta";
      must_not frag (Printf.sprintf "'%Ld'" s2);
      (* Initial identity values: kind project, everything blank, the
         blank primary option selected — never an automatic primary. *)
      must frag "<option value='project' selected>";
      must frag "name='name' maxlength='120' value=''";
      must frag "name='slug' maxlength='80' value=''";
      must frag
        "<textarea name='description' maxlength='2000' rows='6'></textarea>";
      must frag "name='website_url' maxlength='2048' value=''";
      must frag "<option value='' selected>No primary repository</option>";
      Alcotest.(check int)
        "selected kind and blank primary only" 2
        (Html_assert.occurrences frag " selected");
      (* No prefills derived from the account or repositories. *)
      must_not frag "value='psetup-owner'";
      must_not frag "value='alpha'";
      (* Dream's CSRF field plus exactly one application hidden field;
         no selected-snapshot hidden fields or checkboxes. *)
      must frag csrf_field_marker;
      Alcotest.(check int)
        "one application hidden field" 1
        (Html_assert.occurrences frag "type='hidden'");
      must frag
        (Printf.sprintf "<input type='hidden' name='draft_id' value='%Ld'>"
           draft);
      must_not frag "type='checkbox'";
      must_not frag "name='repository'";
      (* No GitHub repository, owner, installation, account, or
         provenance identifiers. *)
      must_not frag "941000082";
      must_not frag "941100082";
      must_not frag "941600821";
      must_not frag "941600822";
      must_not frag "941600823";
      must_not frag "installation";
      must_not frag "connected_by";
      Lwt.return_unit)

let get_details_zero_selection_case =
  db_case "GET step=details: zero selected rows fall back with feedback"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000083L (fun account_id ->
            [
              repo ~account_id ~id:941600831L "alpha";
              repo ~account_id ~id:941600832L "beta";
            ])
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* frag =
        get_fragment "zero selected"
          ~target:(Printf.sprintf "%s?draft=%Ld&step=details" get_target draft)
          pipeline
      in
      check_repository_step "zero selected" frag;
      must frag "Select at least one public repository before continuing.";
      must frag "ps-alert--error";
      Alcotest.(check int)
        "two checkboxes" 2
        (Html_assert.occurrences frag "type='checkbox'");
      (* No permanent identity fields exist on the fallback. *)
      must_not frag "name='name'";
      must_not frag "name='website_url'";
      must_not frag "primary_snapshot_id";
      Lwt.return_unit)

let get_details_isolation_case =
  db_case "GET step=details: foreign and unavailable drafts stay hidden"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* other = insert_user conn "psetup_b" in
      (* Every probed draft has a saved selection, so any authorization
         slip would render the identity form. *)
      let* _, foreign =
        make_draft ~login:"psetup-foreign" conn ~user:other ~ext_id:941000084L
          (fun account_id -> [ repo ~account_id ~id:941600841L "foreign-repo" ])
      in
      let* foreign_ids = snapshot_ids conn foreign in
      let* () =
        seed_selection "foreign" conn ~user_id:other ~draft_id:foreign
          foreign_ids
      in
      let* _, expired =
        make_draft ~login:"psetup-expired" conn ~user:uid ~ext_id:941000085L
          (fun account_id -> [ repo ~account_id ~id:941600851L "expired-repo" ])
      in
      let* expired_ids = snapshot_ids conn expired in
      let* () =
        seed_selection "expired" conn ~user_id:uid ~draft_id:expired expired_ids
      in
      let* () = exec conn "expire" Project_fixture.q_backdate_draft expired in
      let* _, completed =
        make_draft ~login:"psetup-completed" conn ~user:uid ~ext_id:941000086L
          (fun account_id ->
            [ repo ~account_id ~id:941600861L "completed-repo" ])
      in
      let* completed_ids = snapshot_ids conn completed in
      let* () =
        seed_selection "completed" conn ~user_id:uid ~draft_id:completed
          completed_ids
      in
      let* () =
        exec conn "complete" Project_fixture.q_complete_draft completed
      in
      let* rev_inst, revoked =
        make_draft ~login:"psetup-revoked" conn ~user:uid ~ext_id:941000087L
          (fun account_id -> [ repo ~account_id ~id:941600871L "revoked-repo" ])
      in
      let* revoked_ids = snapshot_ids conn revoked in
      let* () =
        seed_selection "revoked" conn ~user_id:uid ~draft_id:revoked revoked_ids
      in
      let* () =
        exec conn "revoke" Project_fixture.q_set_installation_status
          (rev_inst, "revoked", true)
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* () =
        Lwt_list.iter_s
          (fun (label, draft_id, probed_ids) ->
            let* frag =
              get_fragment label
                ~target:
                  (Printf.sprintf "%s?draft=%Ld&step=details" get_target
                     draft_id)
                pipeline
            in
            (* The existing anti-enumeration state: generic unavailable
               feedback, no identity form, no form values, and nothing
               about the probed draft. *)
            must frag "no longer available";
            must_not frag "Project details";
            must_not frag "ps-id-form";
            must_not frag "action='/projects'";
            must_not frag (label ^ "-repo");
            must_not frag ("psetup-" ^ label);
            List.iter
              (fun id -> must_not frag (Printf.sprintf "value='%Ld'" id))
              probed_ids;
            Lwt.return_unit)
          [
            ("foreign", foreign, foreign_ids);
            ("expired", expired, expired_ids);
            ("completed", completed, completed_ids);
            ("revoked", revoked, revoked_ids);
          ]
      in
      Lwt.return_unit)

let get_details_list_case =
  db_case "GET step=details without a draft: list behavior applies"
    (fun ~url conn ->
      let details_target = get_target ^ "?step=details" in
      (* Zero drafts: the plain empty state. *)
      let* u0 = insert_user conn "psetup_a" in
      let* frag =
        get_fragment "zero drafts" ~target:details_target
          (app_pipeline ~url ~session_user_id:u0)
      in
      must frag "ps-empty";
      must_not frag "Project details";
      must_not frag "<form";
      (* One draft with a saved selection: loaded and taken to the
         identity step. *)
      let* u1 = insert_user conn "psetup_b" in
      let* _, d1 =
        make_draft conn ~user:u1 ~ext_id:941000088L (fun account_id ->
            [
              repo ~account_id ~id:941600881L "alpha";
              repo ~account_id ~id:941600882L "beta";
            ])
      in
      let* ids = snapshot_ids conn d1 in
      let* () =
        seed_selection "one selected" conn ~user_id:u1 ~draft_id:d1
          [ List.nth ids 0 ]
      in
      let pipeline1 = app_pipeline ~url ~session_user_id:u1 in
      let* frag =
        get_fragment "one with selection" ~target:details_target pipeline1
      in
      check_identity_step "one with selection" frag;
      (* One draft without a selection: the selected-repository rule
         keeps the repository step with the dedicated feedback. *)
      let* u2 = insert_user conn "psetup_c" in
      let* _, _ =
        make_draft ~login:"psetup-bare" conn ~user:u2 ~ext_id:941000089L
          (fun account_id -> [ repo ~account_id ~id:941600891L "gamma" ])
      in
      let* frag =
        get_fragment "one without selection" ~target:details_target
          (app_pipeline ~url ~session_user_id:u2)
      in
      check_repository_step "one without selection" frag;
      must frag "Select at least one public repository before continuing.";
      (* Several drafts: always the chooser — never the most recent one,
         and nothing selected automatically. *)
      let* _, d2 =
        make_draft ~login:"psetup-second" conn ~user:u1 ~ext_id:941000090L
          (fun account_id -> [ repo ~account_id ~id:941600901L "delta" ])
      in
      let* ids2 = snapshot_ids conn d2 in
      let* () = seed_selection "second" conn ~user_id:u1 ~draft_id:d2 ids2 in
      let* frag = get_fragment "several" ~target:details_target pipeline1 in
      must frag "ps-draft-list";
      must_not frag "Project details";
      must_not frag "<form";
      must_not frag " checked";
      must frag (Printf.sprintf "href='/projects/new?draft=%Ld'" d1);
      must frag (Printf.sprintf "href='/projects/new?draft=%Ld'" d2);
      Lwt.return_unit)

let get_db_suite =
  [
    get_empty_and_grammar_case;
    get_single_draft_case;
    get_chooser_case;
    get_hidden_states_case;
    get_selection_state_case;
    get_server_error_case;
    get_step_grammar_case;
    get_identity_step_case;
    get_details_zero_selection_case;
    get_details_isolation_case;
    get_details_list_case;
  ]

(* === POST behavior === *)

let post_success_case =
  db_case "POST: selection replaced, PRG redirect to the details step"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000061L (fun account_id ->
            [
              repo ~account_id ~id:941600611L "alpha";
              repo ~account_id ~id:941600612L "beta";
              repo ~account_id ~id:941600613L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 and s3 = List.nth ids 2 in
      let* _, (_, (_, _, expires_before)) =
        Project_fixture.draft_row conn draft
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      let* response =
        do_post ~cookie
          ~fields:
            [
              ("draft_id", Int64.to_string draft);
              ("repository", Int64.to_string s1);
              ("repository", Int64.to_string s3);
              ("dream.csrf", token);
            ]
          pipeline
      in
      let location =
        Printf.sprintf "/projects/new?draft=%Ld&step=details" draft
      in
      let* () = check_redirect_lwt "details" location response in
      let* () =
        check_flags "complete replacement"
          [
            flag_sig s1 ~selected:true ~primary:false;
            flag_sig s2 ~selected:false ~primary:false;
            flag_sig s3 ~selected:true ~primary:false;
          ]
          conn draft
      in
      let* _, (_, (_, _, expires_after)) =
        Project_fixture.draft_row conn draft
      in
      Alcotest.(check (float 0.))
        "expiry unchanged" expires_before expires_after;
      (* The redirect target is the project-details step over the two
         selected rows — no repository-success alert — and, being a GET,
         repeating it mutates nothing. *)
      let* frag = get_fragment "follow-up" ~cookie ~target:location pipeline in
      must frag "Project details";
      must frag "<form method='POST' action='/projects'";
      must_not frag "Repository selection saved.";
      must frag (Printf.sprintf "<option value='%Ld'>" s1);
      must frag (Printf.sprintf "<option value='%Ld'>" s3);
      must_not frag (Printf.sprintf "'%Ld'" s2);
      (* The repository step still reflects the durable selection. *)
      let* frag =
        get_fragment "repository step" ~cookie
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target draft)
          pipeline
      in
      must frag (Printf.sprintf "value='%Ld' checked" s1);
      must frag (Printf.sprintf "value='%Ld' checked" s3);
      Alcotest.(check int)
        "two checked" 2
        (Html_assert.occurrences frag " checked");
      check_flags "GET did not mutate"
        [
          flag_sig s1 ~selected:true ~primary:false;
          flag_sig s2 ~selected:false ~primary:false;
          flag_sig s3 ~selected:true ~primary:false;
        ]
        conn draft)

let post_empty_selection_case =
  db_case "POST: empty selection clears every repository, stays on the step"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000062L (fun account_id ->
            [
              repo ~account_id ~id:941600621L "alpha";
              repo ~account_id ~id:941600622L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* r =
        Sel.replace conn ~user_id:uid ~draft_id:draft
          ~selected_snapshot_ids:[ s1; s2 ] ~primary_snapshot_id:None
      in
      (match r with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "seed selection failed");
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      let* response =
        do_post ~cookie
          ~fields:[ ("draft_id", Int64.to_string draft); ("dream.csrf", token) ]
          pipeline
      in
      (* The empty selection is saved, but the flow stays on the
         repository step with the dedicated feedback — never step=details. *)
      let location =
        Printf.sprintf "/projects/new?draft=%Ld&selection=required" draft
      in
      let* () = check_redirect_lwt "cleared" location response in
      let* () =
        check_flags "everything unselected"
          [
            flag_sig s1 ~selected:false ~primary:false;
            flag_sig s2 ~selected:false ~primary:false;
          ]
          conn draft
      in
      let* frag = get_fragment "follow-up" ~cookie ~target:location pipeline in
      must frag "Select at least one public repository before continuing.";
      must frag "<form method='POST' action='/projects/new/repositories'";
      must_not frag "Project details";
      must_not frag " checked";
      Lwt.return_unit)

let post_stale_case =
  db_case "POST: refreshed snapshot turns old ids into stale feedback"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000063L (fun account_id ->
            [
              repo ~account_id ~id:941600631L "old-alpha";
              repo ~account_id ~id:941600632L "old-beta";
            ])
      in
      let* old_ids = snapshot_ids conn draft in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      (* A GitHub re-verification replaces the snapshot behind the open
         page: same draft row, brand-new snapshot rows. *)
      let* () =
        refresh_snapshot conn ~user:uid ~ext_id:941000063L (fun account_id ->
            [
              repo ~account_id ~id:941600633L "fresh-alpha";
              repo ~account_id ~id:941600634L "fresh-beta";
            ])
      in
      let* response =
        do_post ~cookie
          ~fields:
            (("draft_id", Int64.to_string draft)
             :: List.map (fun id -> ("repository", Int64.to_string id)) old_ids
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      let* () =
        check_redirect_lwt "stale"
          (Printf.sprintf "/projects/new?draft=%Ld&selection=stale" draft)
          response
      in
      (* Nothing on the fresh snapshot was selected accidentally. *)
      let* fresh_ids = snapshot_ids conn draft in
      let* () =
        check_flags "fresh snapshot untouched"
          (List.map
             (fun id -> flag_sig id ~selected:false ~primary:false)
             fresh_ids)
          conn draft
      in
      let* frag =
        get_fragment "follow-up" ~cookie
          ~target:
            (Printf.sprintf "/projects/new?draft=%Ld&selection=stale" draft)
          pipeline
      in
      must frag "repository list changed";
      must frag "psetup-owner/fresh-alpha";
      must frag "psetup-owner/fresh-beta";
      must_not frag "old-alpha";
      must_not frag " checked";
      Lwt.return_unit)

let post_invalid_form_db_case =
  db_case "POST: malformed forms redirect clean and mutate nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000064L (fun account_id ->
            [
              repo ~account_id ~id:941600641L "alpha";
              repo ~account_id ~id:941600642L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* r =
        Sel.replace conn ~user_id:uid ~draft_id:draft
          ~selected_snapshot_ids:[ s1 ] ~primary_snapshot_id:None
      in
      (match r with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "seed selection failed");
      let seeded =
        [
          flag_sig s1 ~selected:true ~primary:false;
          flag_sig s2 ~selected:false ~primary:false;
        ]
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      let d = Int64.to_string draft in
      let r1 = Int64.to_string s1 in
      let submissions =
        [
          ( "unknown field",
            [ ("draft_id", d); ("repository", r1); ("primary", r1) ] );
          ("duplicate draft field", [ ("draft_id", d); ("draft_id", d) ]);
          ("malformed draft", [ ("draft_id", "zz9zz") ]);
          ( "duplicate repository",
            [ ("draft_id", d); ("repository", r1); ("repository", r1) ] );
          ("malformed repository", [ ("draft_id", d); ("repository", "x") ]);
        ]
      in
      let* () =
        Lwt_list.iter_s
          (fun (label, fields) ->
            let* response =
              do_post ~cookie
                ~fields:(fields @ [ ("dream.csrf", token) ])
                pipeline
            in
            (* The untrusted draft id is never retained. *)
            let* () =
              check_redirect_lwt label "/projects/new?selection=invalid"
                response
            in
            check_flags (label ^ ": untouched") seeded conn draft)
          submissions
      in
      Lwt.return_unit)

let post_unavailable_case =
  db_case "POST: foreign, expired, terminal, revoked all collapse"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* other = insert_user conn "psetup_b" in
      (* The poster keeps one live draft: it renders the form that mints
         the CSRF token, and it must survive every rejected POST. *)
      let* _, live =
        make_draft conn ~user:uid ~ext_id:941000071L (fun account_id ->
            [ repo ~account_id ~id:941600711L "live-repo" ])
      in
      let* _, foreign =
        make_draft ~login:"psetup-foreign" conn ~user:other ~ext_id:941000072L
          (fun account_id -> [ repo ~account_id ~id:941600721L "foreign-repo" ])
      in
      let* foreign_ids = snapshot_ids conn foreign in
      let* _, expired =
        make_draft ~login:"psetup-expired" conn ~user:uid ~ext_id:941000073L
          (fun account_id -> [ repo ~account_id ~id:941600731L "expired-repo" ])
      in
      let* expired_ids = snapshot_ids conn expired in
      let* () = exec conn "expire" Project_fixture.q_backdate_draft expired in
      let* _, completed =
        make_draft ~login:"psetup-completed" conn ~user:uid ~ext_id:941000074L
          (fun account_id ->
            [ repo ~account_id ~id:941600741L "completed-repo" ])
      in
      let* completed_ids = snapshot_ids conn completed in
      let* () =
        exec conn "complete" Project_fixture.q_complete_draft completed
      in
      let* rev_inst, revoked =
        make_draft ~login:"psetup-revoked" conn ~user:uid ~ext_id:941000075L
          (fun account_id -> [ repo ~account_id ~id:941600751L "revoked-repo" ])
      in
      let* revoked_ids = snapshot_ids conn revoked in
      let* () =
        exec conn "revoke" Project_fixture.q_set_installation_status
          (rev_inst, "revoked", true)
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ =
        open_form "form"
          ~target:(Printf.sprintf "%s?draft=%Ld" get_target live)
          pipeline
      in
      let* () =
        Lwt_list.iter_s
          (fun (label, draft, ids) ->
            let* response =
              do_post ~cookie
                ~fields:
                  (("draft_id", Int64.to_string draft)
                   :: List.map
                        (fun id -> ("repository", Int64.to_string id))
                        ids
                  @ [ ("dream.csrf", token) ])
                pipeline
            in
            (* Identical redirect for every cause; the submitted id is
               dropped from the URL. *)
            let* () =
              check_redirect_lwt label "/projects/new?selection=unavailable"
                response
            in
            check_flags (label ^ ": no mutation")
              (List.map
                 (fun id -> flag_sig id ~selected:false ~primary:false)
                 ids)
              conn draft)
          [
            ("foreign", foreign, foreign_ids);
            ("expired", expired, expired_ids);
            ("completed", completed, completed_ids);
            ("revoked", revoked, revoked_ids);
          ]
      in
      Lwt.return_unit)

let post_rejected_no_mutation_case =
  db_case "POST: CSRF and origin rejections modify nothing" (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000076L (fun account_id ->
            [
              repo ~account_id ~id:941600761L "alpha";
              repo ~account_id ~id:941600762L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* r =
        Sel.replace conn ~user_id:uid ~draft_id:draft
          ~selected_snapshot_ids:[ s1 ] ~primary_snapshot_id:None
      in
      (match r with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "seed selection failed");
      let seeded =
        [
          flag_sig s1 ~selected:true ~primary:false;
          flag_sig s2 ~selected:false ~primary:false;
        ]
      in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      let flip_fields =
        [
          ("draft_id", Int64.to_string draft); ("repository", Int64.to_string s2);
        ]
      in
      let* bad_token =
        do_post ~cookie
          ~fields:(flip_fields @ [ ("dream.csrf", "not-a-token") ])
          pipeline
      in
      Alcotest.(check int) "invalid token: 403" 403 (status_of bad_token);
      let* cross =
        do_post ~origin:(Some "https://evil.example") ~cookie
          ~fields:(flip_fields @ [ ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "cross-origin: 403" 403 (status_of cross);
      check_flags "selection untouched" seeded conn draft)

(* Test-only failure injection, scoped to one reserved fixture repository
   id — the same established trigger mechanism as the draft/selection
   store suites; production migrations are untouched. *)
let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE FUNCTION psetup_fail_update_fn() RETURNS trigger\n\
    \   LANGUAGE plpgsql\n\
    \   AS 'BEGIN RAISE EXCEPTION ''psetup fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER psetup_fail_update\n\
    \   BEFORE UPDATE ON project_onboarding_draft_repositories\n\
    \   FOR EACH ROW\n\
    \   WHEN (NEW.is_selected AND NEW.github_repository_id = 941999999)\n\
    \   EXECUTE FUNCTION psetup_fail_update_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS psetup_fail_update\n\
    \   ON project_onboarding_draft_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS psetup_fail_update_fn()"

let post_storage_error_case =
  db_case "POST: storage failure is the generic 500, nothing partial"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000077L (fun account_id ->
            [
              repo ~account_id ~id:941600771L "alpha";
              repo ~account_id ~id:941999999L "poison";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* () = exec conn "fail fn" q_create_fail_fn () in
      let* () = exec conn "fail trigger" q_create_fail_trigger () in
      Lwt.finalize
        (fun () ->
          let pipeline = app_pipeline ~url ~session_user_id:uid in
          let* cookie, token, _ = open_form "form" pipeline in
          let* response =
            do_post ~cookie
              ~fields:
                [
                  ("draft_id", Int64.to_string draft);
                  ("repository", Int64.to_string s1);
                  ("repository", Int64.to_string s2);
                  ("dream.csrf", token);
                ]
              pipeline
          in
          Alcotest.(check int) "500" 500 (status_of response);
          Alcotest.(check (option string))
            "no Location" None
            (Dream.header response "Location");
          let* body = Dream.body response in
          List.iter (must_not body)
            [ "psetup fixture failure"; "Caqti"; "PostgreSQL" ];
          (* The transaction rolled back: no partial selection. *)
          check_flags "rolled back"
            [
              flag_sig s1 ~selected:false ~primary:false;
              flag_sig s2 ~selected:false ~primary:false;
            ]
            conn draft)
        (fun () ->
          let* () = exec conn "drop trigger" q_drop_fail_trigger () in
          exec conn "drop fn" q_drop_fail_fn ()))

let post_inconsistent_case =
  db_case "POST: durable inconsistency is the generic 500, no mutation"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000078L (fun account_id ->
            [
              repo ~account_id ~id:941600781L "alpha";
              repo ~account_id ~id:941600782L "beta";
              repo ~account_id ~id:941600783L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s3 = List.nth ids 2 in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      (* Break position contiguity — an invariant only the store's
         re-check can catch. *)
      let* () = exec conn "gap" Project_fixture.q_delete_position (draft, 2) in
      let* response =
        do_post ~cookie
          ~fields:
            [
              ("draft_id", Int64.to_string draft);
              ("repository", Int64.to_string s1);
              ("dream.csrf", token);
            ]
          pipeline
      in
      Alcotest.(check int) "500" 500 (status_of response);
      let* body = Dream.body response in
      List.iter (must_not body) [ "Caqti"; "PostgreSQL" ];
      check_flags "untouched"
        [
          flag_sig s1 ~selected:false ~primary:false;
          flag_sig s3 ~selected:false ~primary:false;
        ]
        conn draft)

let post_details_race_case =
  db_case "POST then refresh: redirected GET falls back to repositories"
    (fun ~url conn ->
      let* uid = insert_user conn "psetup_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:941000091L (fun account_id ->
            [
              repo ~account_id ~id:941600911L "old-alpha";
              repo ~account_id ~id:941600912L "old-beta";
            ])
      in
      let* old_ids = snapshot_ids conn draft in
      let pipeline = app_pipeline ~url ~session_user_id:uid in
      let* cookie, token, _ = open_form "form" pipeline in
      let* response =
        do_post ~cookie
          ~fields:
            (("draft_id", Int64.to_string draft)
             :: List.map (fun id -> ("repository", Int64.to_string id)) old_ids
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      let location =
        Printf.sprintf "/projects/new?draft=%Ld&step=details" draft
      in
      let* () = check_redirect_lwt "details" location response in
      (* A GitHub refresh replaces the snapshot between the successful
         POST and the redirected GET: the saved selection is gone. *)
      let* () =
        refresh_snapshot conn ~user:uid ~ext_id:941000091L (fun account_id ->
            [
              repo ~account_id ~id:941600913L "fresh-alpha";
              repo ~account_id ~id:941600914L "fresh-beta";
            ])
      in
      let* frag =
        get_fragment "raced follow-up" ~cookie ~target:location pipeline
      in
      (* The database is authoritative: back to the repository step with
         the dedicated feedback, no identity form, no stale snapshot ids,
         nothing checked. *)
      must frag "Select at least one public repository before continuing.";
      must frag "<form method='POST' action='/projects/new/repositories'";
      must_not frag "Project details";
      must_not frag "ps-id-form";
      must frag "psetup-owner/fresh-alpha";
      must frag "psetup-owner/fresh-beta";
      must_not frag "old-alpha";
      must_not frag " checked";
      List.iter
        (fun id -> must_not frag (Printf.sprintf "value='%Ld'" id))
        old_ids;
      Lwt.return_unit)

let post_db_suite =
  [
    post_success_case;
    post_empty_selection_case;
    post_stale_case;
    post_invalid_form_db_case;
    post_unavailable_case;
    post_rejected_no_mutation_case;
    post_storage_error_case;
    post_inconsistent_case;
    post_details_race_case;
  ]

(* --- Page rendering with a live request: the framework CSRF field.
   Complements the pure psp_* suites above, which render without a request
   and must stay possible. --- *)

let psc_case name f = Alcotest.test_case name `Quick f

let psc_cases =
  [
    psc_case "configure with request: one framework field, one app field"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.psc_render
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        (* Exactly one framework CSRF field (double-quoted, Dream's own
           markup) and exactly one application hidden field. *)
        Alcotest.(check int)
          "one framework field" 1
          (Html_assert.occurrences frag "name=\"dream.csrf\"");
        Alcotest.(check int)
          "framework field is hidden" 1
          (Html_assert.occurrences frag "type=\"hidden\"");
        Alcotest.(check int)
          "one application hidden field" 1
          (Html_assert.occurrences frag "type='hidden'");
        Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
        Alcotest.(check int)
          "three checkboxes" 3
          (Html_assert.occurrences frag "type='checkbox'");
        (* The framework field sits inside the one POST form. *)
        (match
           ( Html_assert.index_from frag "<form" 0,
             Html_assert.index_from frag "name=\"dream.csrf\"" 0,
             Html_assert.index_from frag "</form>" 0 )
         with
        | Some f, Some c, Some e ->
            Alcotest.(check bool) "CSRF inside the form" true (f < c && c < e)
        | _ -> Alcotest.fail "form or CSRF field missing");
        (* Still no hidden user/installation/account/repository fields. *)
        Html_assert.must_not frag "name='user_id'";
        Html_assert.must_not frag "installation_id";
        Html_assert.must_not frag "github_repository_id";
        Html_assert.must_not frag "account_id");
    psc_case "form-free states emit no framework field even with a request"
      (fun () ->
        List.iter
          (fun state ->
            let frag =
              Html_assert.panel_fragment
                (Project_setup_fixture.psc_render state)
            in
            Html_assert.must_not frag "dream.csrf")
          [
            Psp.No_available_drafts;
            Psp.Choose_draft
              [
                Project_setup_fixture.ps_chooser_a;
                Project_setup_fixture.ps_chooser_b;
              ];
          ]);
    psc_case "pure rendering without a request stays CSRF-free" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Html_assert.must_not frag "dream.csrf";
        Alcotest.(check int)
          "one application hidden field" 1
          (Html_assert.occurrences frag "type='hidden'"));
  ]

let suites =
  (* /projects/new renderer with a live request: Dream's framework CSRF
       field appears exactly once inside the one POST form, next to the
       single application hidden field; pure rendering stays CSRF-free. *)
  [
    ("project_setup_page_csrf", psc_cases)
    (* Project-setup handlers: DB-free access gates for both routes, the
       POST's same-origin and CSRF gates (all rejections precede SQL), and
       the database-gated GET/POST behavior over real drafts. *);
    ("project_setup_get_gates", get_gate_suite);
    ("project_setup_post_gates", post_gate_suite);
    ("project_setup_post_csrf", csrf_suite);
    ("project_setup_get_db", get_db_suite);
    ("project_setup_post_db", post_db_suite);
  ]
