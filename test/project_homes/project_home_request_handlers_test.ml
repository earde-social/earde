module Ob = Earde.Project_onboarding

(* === Existing-community home request handlers
   (Project_home_request_handlers) ===
   GET/POST /projects/:slug/request-home: DB-free access, origin, and CSRF
   gates (every rejection precedes SQL — no sql_pool is installed in those
   harnesses), and the database-gated end-to-end flow over verified
   permanent projects built through the real finalization chain, real
   communities, and the transactional request store. Reserved
   external-installation-id range 944800001..944800999 (hence account ids
   944900001..944900999, which also scope the permanent-project cleanup),
   phch_% usernames, and phch-% community slugs so no suite shares
   fixtures. Credential and privacy assertions are boolean, so no fixture
   byte reaches test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module H = Earde.Project_home_request_handlers
module Rq = Earde.Project_home_request_store

let case = Case.quick
let counting_loader = Http_fixture.counting_loader
let ok_loader = Http_fixture.ok_loader
let status_of = Http_fixture.status_of
let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let make_project = Home_request_fixture.make_project
let insert_community = Community_fixture.insert_community
let must = Html_assert.must
let must_not = Html_assert.must_not
let target slug = Printf.sprintf "/projects/%s/request-home" slug
let make_get ~mode = H.make_project_home_choice_handler ~mode

let make_post ~mode ~load_config =
  H.make_project_home_request_handler ~mode ~load_config

(* --- Fixtures --- *)

(* Same dependency order as the sibling suites. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 944900001 \
       AND 944900999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       944900001 AND 944900999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 944800001 AND 944800999)";
      "DELETE FROM communities WHERE slug LIKE 'phch-%'";
      "DELETE FROM users WHERE username LIKE 'phch_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       944800001 AND 944800999";
    ]

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way, and the
   connection is disconnected deterministically. *)
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
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun ~url conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () ->
          Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f ~url conn))

let q_pending_id =
  (Caqti_type.int64 ->! Caqti_type.int64)
    "SELECT id FROM community_projects WHERE project_id = $1 AND status = \
     'pending'"

(* Test-only failure injection for the handler-level store failure: an
   AFTER trigger scoped to one reserved note value, so the failure fires
   only after the insertion was genuinely attempted. Installed and
   dropped inside that case alone; production migrations are
   untouched. *)
let phch_poison_note = "phch poison marker"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE FUNCTION phch_fail_insert_fn() RETURNS trigger\n\
    \   LANGUAGE plpgsql\n\
    \   AS 'BEGIN RAISE EXCEPTION ''phch fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER phch_fail_insert\n\
    \   AFTER INSERT ON community_projects\n\
    \   FOR EACH ROW WHEN (NEW.request_note = 'phch poison marker')\n\
    \   EXECUTE FUNCTION phch_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS phch_fail_insert ON community_projects"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS phch_fail_insert_fn()"

(* === DB-free: GET access gates === *)

let gate_target = target "phch-any"

let get_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:gate_target
    (make_get ~mode)

let get_off_case =
  case "GET request-home off: clean /bring redirect before params or SQL"
    (fun () ->
      Http_fixture.check_clean_redirect "off" "/bring"
        (Http_fixture.gate_response "off"
           (get_run ~session:Http_fixture.admin_session ~mode:Ob.Off ())))

let get_anonymous_case =
  case "GET request-home: anonymous and invalid sessions to /login" (fun () ->
      Http_fixture.check_clean_redirect "anonymous" "/login"
        (Http_fixture.gate_response "anonymous" (get_run ~mode:Ob.Public ()));
      List.iter
        (fun raw ->
          Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
            (Http_fixture.gate_response ("user_id " ^ raw)
               (get_run ~session:[ ("user_id", raw) ] ~mode:Ob.Public ())))
        [ "not-a-number"; ""; "0"; "-3" ];
      Http_fixture.check_clean_redirect "is_admin only" "/login"
        (Http_fixture.gate_response "is_admin only"
           (get_run ~session:[ ("is_admin", "true") ] ~mode:Ob.Admins ())))

let get_rollout_case =
  case "GET request-home admins mode: non-admin to /bring, admin continues"
    (fun () ->
      Http_fixture.check_clean_redirect "non-admin" "/bring"
        (Http_fixture.gate_response "non-admin"
           (get_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ()));
      (* Past the gates, the routerless harness has no :slug parameter,
         which answers as the generic 404 — reaching it proves the gates
         passed with no SQL (no sql_pool is installed). *)
      let continues label session mode =
        let response =
          Http_fixture.gate_response label (get_run ~session ~mode ())
        in
        Alcotest.(check int) (label ^ ": generic 404") 404 (status_of response)
      in
      continues "admin continues" Http_fixture.admin_session Ob.Admins;
      continues "public user continues" Http_fixture.logged_in Ob.Public)

let get_gate_suite = [ get_off_case; get_anonymous_case; get_rollout_case ]

(* === DB-free: POST access gates, configuration, and origin policy === *)

let post_run ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target:gate_target
    (make_post ~mode ~load_config)

let post_off_case =
  case "POST request-home off: clean /bring redirect, loader untouched"
    (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      let response =
        Http_fixture.gate_response "off"
          (post_run ~session:Http_fixture.admin_session ~mode:Ob.Off
             ~load_config:loader ())
      in
      Http_fixture.check_clean_redirect "off" "/bring" response;
      Alcotest.(check int) "loader never called" 0 !calls)

let post_anonymous_case =
  case
    "POST request-home: anonymous and invalid sessions to /login, loader \
     untouched" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      Http_fixture.check_clean_redirect "anonymous" "/login"
        (Http_fixture.gate_response "anonymous"
           (post_run ~mode:Ob.Public ~load_config:loader ()));
      List.iter
        (fun raw ->
          Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
            (Http_fixture.gate_response ("user_id " ^ raw)
               (post_run
                  ~session:[ ("user_id", raw) ]
                  ~mode:Ob.Public ~load_config:loader ())))
        [ "not-a-number"; ""; "0"; "-3" ];
      Http_fixture.check_clean_redirect "is_admin only" "/login"
        (Http_fixture.gate_response "is_admin only"
           (post_run
              ~session:[ ("is_admin", "true") ]
              ~mode:Ob.Admins ~load_config:loader ()));
      Alcotest.(check int) "loader never called" 0 !calls)

let post_rollout_case =
  case
    "POST request-home admins mode: non-admin to /bring before the route or \
     configuration" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      Http_fixture.check_clean_redirect "non-admin" "/bring"
        (Http_fixture.gate_response "non-admin"
           (post_run ~session:Http_fixture.logged_in ~mode:Ob.Admins
              ~load_config:loader ()));
      Alcotest.(check int) "loader never called" 0 !calls;
      (* Past the gates, the routerless harness has no :slug parameter:
         the defensive 404 answers before the configuration is ever
         loaded — with no SQL (no sql_pool is installed). *)
      let continues label session mode =
        let loader, calls = counting_loader (ok_loader ()) in
        let response =
          Http_fixture.gate_response label
            (post_run ~session ~mode ~load_config:loader ())
        in
        Alcotest.(check int)
          (label ^ ": defensive 404")
          404 (status_of response);
        Alcotest.(check int) (label ^ ": loader after missing route") 0 !calls
      in
      continues "admin continues" Http_fixture.admin_session Ob.Admins;
      continues "public user continues" Http_fixture.logged_in Ob.Public)

(* The routed DB-free harness: the real route shape supplies :slug, so
   the configuration and origin gates are reachable; rejections still
   precede SQL (no sql_pool is installed). *)
let routed_post ?session ?(headers = []) ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target:gate_target
    (Dream.router
       [
         Dream.post "/projects/:slug/request-home" (fun req ->
             make_post ~mode:Ob.Public ~load_config req);
       ])

let post_config_failure_case =
  case "POST request-home: configuration error is a generic 503, no form"
    (fun () ->
      let loader, calls =
        counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      let response =
        Http_fixture.gate_response "config failure"
          (routed_post ~session:Http_fixture.logged_in
             ~headers:
               [
                 ("Origin", "https://earde.com");
                 ("Content-Type", "application/x-www-form-urlencoded");
               ]
             ~load_config:loader ())
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

let origin_run label ?sec_fetch_site origin =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @
    match sec_fetch_site with
    | Some v -> [ ("Sec-Fetch-Site", v) ]
    | None -> []
  in
  routed_post ~session:Http_fixture.logged_in ~headers
    ~load_config:(fun () -> ok_loader ())
    ()
  |> Http_fixture.gate_response label

let origin_rejected label ?sec_fetch_site origin =
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int) (label ^ ": 403") 403 (status_of response)

let origin_accepted label ?sec_fetch_site origin =
  (* Passing the origin gate, the missing content type is the next
     rejection: reaching that 400 proves the origin policy accepted. *)
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int)
    (label ^ ": passes the origin gate")
    400 (status_of response)

let post_origin_case =
  case "POST request-home origin gate: exact policy of the project POSTs"
    (fun () ->
      origin_accepted "exact origin" (Some "https://earde.com");
      origin_accepted "explicit default port" (Some "https://earde.com:443");
      origin_accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
      origin_rejected "cross-origin" (Some "https://evil.example");
      origin_rejected "same-site subdomain" (Some "https://www.earde.com");
      origin_rejected "wrong scheme" (Some "http://earde.com");
      origin_rejected "wrong port" (Some "https://earde.com:8443");
      origin_rejected "null" (Some "null");
      origin_rejected "blank" (Some "");
      origin_rejected "mismatch beats same-origin metadata"
        ~sec_fetch_site:"same-origin" (Some "https://evil.example");
      origin_rejected "no signals" None;
      origin_rejected "cross-site" ~sec_fetch_site:"cross-site" None;
      origin_rejected "same-site" ~sec_fetch_site:"same-site" None;
      origin_rejected "none" ~sec_fetch_site:"none" None;
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

(* === DB-free: POST CSRF handling (rejections precede SQL) === *)

let request_fields ?(target_id = "1") ?(note = "ciao") () =
  [ ("target_community_id", target_id); ("request_note", note) ]

let csrf_pipeline () =
  Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n" ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ ->
      Dream.router
        [
          Dream.post "/projects/:slug/request-home" (fun r ->
              make_post ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) r);
        ]
        req

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
         (Dream.request ~method_:`POST ~target:(target "phch-csrf") ~headers
            (Http_fixture.form_body fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  let response = Http_fixture.gate_response label result in
  Alcotest.(check int) (label ^ ": status") expected (status_of response)

let csrf_case =
  case "POST request-home CSRF: Dream verification gates every submission"
    (fun () ->
      let pipeline = csrf_pipeline () in
      let cookie, fresh, expired = Http_fixture.mint_tokens "mint" pipeline in
      let fields = request_fields () in
      (* Every CSRF failure refuses the submission, but it is answered by
         reloading the authorized current state (a fresh token, nothing
         reflected) rather than a terminal page — so each one reaches the
         read model's DB boundary and none reaches the store. A page whose
         one-hour token outlived its fourteen-day session must not become
         permanently unsubmittable. *)
      Http_fixture.check_db_boundary "missing token re-renders"
        (csrf_post ~cookie pipeline fields);
      Http_fixture.check_db_boundary "invalid token re-renders"
        (csrf_post ~cookie pipeline
           (fields @ [ ("dream.csrf", "not-a-token") ]));
      Http_fixture.check_db_boundary "expired token re-renders"
        (csrf_post ~cookie pipeline (fields @ [ ("dream.csrf", expired) ]));
      Http_fixture.check_db_boundary "duplicate tokens re-render"
        (csrf_post ~cookie pipeline
           (fields @ [ ("dream.csrf", fresh); ("dream.csrf", fresh) ]));
      Http_fixture.check_db_boundary "wrong session re-renders"
        (csrf_post pipeline (fields @ [ ("dream.csrf", fresh) ]));
      (* A wrong content type is still answered without any SQL. *)
      csrf_rejected "wrong content type" 400
        (csrf_post ~cookie ~content_type:false pipeline
           (fields @ [ ("dream.csrf", fresh) ]));
      (* A verified token with valid application fields reaches the DB
         boundary: CSRF passed and Dream stripped its own field before
         the strict parser. *)
      Http_fixture.check_db_boundary "verified form continues"
        (csrf_post ~cookie pipeline (fields @ [ ("dream.csrf", fresh) ]));
      (* A verified token with a malformed application form also reaches
         the DB boundary — the current-state re-render — never a direct
         rejection that skips reauthorization. *)
      Http_fixture.check_db_boundary "malformed form re-render continues"
        (csrf_post ~cookie pipeline
           [ ("request_note", "phch-zz9"); ("dream.csrf", fresh) ]))

let csrf_suite = [ csrf_case ]

(* === Database-gated: the real GET/POST flow === *)

(* The real production shape: sql_pool + secret + memory sessions + the
   real router paths, so both handlers read :slug exactly as in bin/main.
   /mint exists only to hand replay cases a fresh CSRF token for an
   already-established session. *)
let app_pipeline ?session_user_id ~url () =
  Dream.sql_pool url
  @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ (fun handler request ->
    match session_user_id with
    | None -> handler request
    | Some uid ->
        let* () =
          Dream.set_session_field request "user_id" (string_of_int uid)
        in
        handler request)
  @@ Dream.router
       [
         Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.get "/projects/:slug/setup" (fun req ->
             Http_fixture.make_setup ~mode:Ob.Public req);
         Dream.get "/projects/:slug/request-home" (fun req ->
             make_get ~mode:Ob.Public req);
         Dream.post "/projects/:slug/request-home" (fun req ->
             make_post ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) req);
       ]

(* One GET that opens the chooser: page, session cookie, and CSRF token
   for the follow-up POST. *)
let open_choice_page label ~slug pipeline =
  let* response, body = Http_fixture.do_get ~target:(target slug) pipeline in
  Http_fixture.check_page label response;
  let cookie = Http_fixture.session_cookie label response in
  let token = Http_fixture.csrf_of_page label body in
  Lwt.return (cookie, token, body)

let do_post ?(origin = Some "https://earde.com") ~cookie ~slug ~fields pipeline
    =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [
        ("Content-Type", "application/x-www-form-urlencoded"); ("Cookie", cookie);
      ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target:(target slug) ~headers
       (Http_fixture.form_body fields))

let count_relations conn project =
  find conn "relation count" Home_request_fixture.q_count_for_project project

let check_no_relations label conn project =
  let* n = count_relations conn project in
  Alcotest.(check int) label 0 n;
  Lwt.return_unit

let radio_marker id = Printf.sprintf "value='%d'" id

(* === GET states === *)

let get_chooser_case =
  db_case "GET: eligible communities render the chooser with a CSRF form"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:944800001L ~slug:"phch-states"
      in
      let* pub =
        insert_community ~name:"Phch States Pub"
          ~description:"Descrizione pubblica" conn "phch-states-pub"
      in
      let* unl =
        insert_community ~name:"Phch States Unl" ~indexable:false
          ~discoverable:false conn "phch-states-unl"
      in
      let* _legacy =
        insert_community ~network:false conn "phch-states-legacy"
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* _cookie, _token, body =
        open_choice_page "chooser" ~slug:"phch-states" pipeline
      in
      (* check_page (inside open_choice_page) already asserted 200 +
         no-store + no-referrer; csrf_of_page asserted the framework
         field. *)
      Alcotest.(check bool) "noindex" true (Html_assert.contains body "noindex");
      let frag = Html_assert.panel_fragment body in
      must frag "Connect to an existing community";
      must frag "action='/projects/phch-states/request-home'";
      must frag (radio_marker pub);
      must frag (radio_marker unl);
      must frag "Phch States Pub";
      must frag "Descrizione pubblica";
      must frag "phc-community-visibility'>Public";
      must frag "phc-community-visibility'>Unlisted";
      must_not frag "phch-states-legacy";
      Lwt.return_unit)

let get_no_eligible_case =
  db_case "GET: zero eligible communities render the no-eligible state"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:944800002L ~slug:"phch-none"
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-none") pipeline
      in
      Http_fixture.check_page "no eligible" response;
      let frag = Html_assert.panel_fragment body in
      must frag "No eligible published network community";
      must frag "href='/projects/phch-none/setup'";
      must_not frag "<form";
      must_not frag "phc-request-form";
      Lwt.return_unit)

let get_active_states_case =
  db_case_lifecycle_relaxed
    "GET: pending, accepted, and drifted active targets render"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:944800003L ~slug:"phch-active"
      in
      let* home =
        insert_community ~name:"Phch Active Home" conn "phch-active-home"
      in
      let* rid =
        Community_fixture.request_pending "pending fixture" conn ~user:uid
          ~slug:"phch-active" ~community:home ~note:"phch secret note" ()
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-active") pipeline
      in
      Http_fixture.check_page "pending" response;
      let frag = Html_assert.panel_fragment body in
      must frag "Home request pending";
      must frag "Phch Active Home";
      must frag "/c/phch-active-home";
      must_not frag "phc-request-form";
      must_not frag "<form";
      (* The private note never renders on the workflow page, and an
         eligible target carries no availability marker. *)
      must_not body "phch secret note";
      must_not frag "Currently unavailable";
      (* Drift: an ineligible active target stays visible with only the
         generic availability marker — never the reason. *)
      let* () =
        exec conn "make private" Community_fixture.q_make_private home
      in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-active") pipeline
      in
      Http_fixture.check_page "drifted pending" response;
      let frag = Html_assert.panel_fragment body in
      must frag "Home request pending";
      must frag "Phch Active Home";
      must frag "Currently unavailable";
      must_not frag "private";
      must_not frag "<form";
      (* Accepted relation. *)
      let* () = exec conn "restore" Community_fixture.q_make_listed home in
      let* () = exec conn "accept" Home_request_fixture.q_mark_accepted rid in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-active") pipeline
      in
      Http_fixture.check_page "accepted" response;
      let frag = Html_assert.panel_fragment body in
      must frag "Community home connected";
      must frag "Phch Active Home";
      (* No request form: the accepted state cannot solicit a second home.
         Its one form is the steward removal control, whose action is the
         removal route built from both canonical slugs. *)
      must_not frag "phc-request-form";
      Alcotest.(check int)
        "exactly one form" 1
        (Html_assert.occurrences frag "<form");
      must frag
        "action='/projects/phch-active/community-home/phch-active-home/remove'";
      must frag "Remove community home";
      Lwt.return_unit)

let get_not_found_case =
  db_case "GET: every unavailable project collapses to the generic 404"
    (fun ~url conn ->
      let* a = insert_user conn "phch_a" in
      let* b = insert_user conn "phch_b" in
      let* _inst, project =
        make_project conn ~user:a ~ext_id:944800004L ~slug:"phch-auth"
      in
      let owner = app_pipeline ~session_user_id:a ~url () in
      let foreign = app_pipeline ~session_user_id:b ~url () in
      let expect_404 label pipeline slug =
        let* response, body =
          Http_fixture.do_get ~target:(target slug) pipeline
        in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        Alcotest.(check (option string))
          (label ^ ": no-store") (Some "no-store")
          (Dream.header response "Cache-Control");
        must body "This page does not exist.";
        (* No project identity leaks through the collapse. *)
        must_not body "Pfin Fixture Project";
        Lwt.return_unit
      in
      let* () = expect_404 "missing slug" owner "phch-absent" in
      let* () = expect_404 "malformed slug" owner "Phch--Bad" in
      let* () = expect_404 "foreign project" foreign "phch-auth" in
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* () = expect_404 "stale project" owner "phch-auth" in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      let* () = expect_404 "revoked project" owner "phch-auth" in
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* () =
        exec conn "drop steward" Home_request_fixture.q_delete_steward
          (project, a)
      in
      expect_404 "creator without stewardship" owner "phch-auth")

let get_inconsistent_case =
  db_case_lifecycle_relaxed
    "GET: durable corruption is one generic 500, never a page state"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:944800005L ~slug:"phch-mix"
      in
      let* home = insert_community conn "phch-mix-home" in
      let* () =
        exec conn "mix flags" Home_request_fixture.q_mix_community_flags home
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-mix") pipeline
      in
      Alcotest.(check int) "500" 500 (status_of response);
      Alcotest.(check (option string))
        "no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      must body "Something went wrong on our side.";
      List.iter (must_not body)
        [ "Inconsistent"; "indexable"; "discoverable"; "PostgreSQL"; "Caqti" ];
      Lwt.return_unit)

let get_storage_case =
  db_case "GET: a real database failure collapses to the generic 500"
    (fun ~url _conn ->
      (* A pool whose connections resolve no unqualified table names: a
         real PostgreSQL request failure surfaced through the read model,
         collapsed payload-free by the handler. *)
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=phch_void"))
      in
      let pipeline = app_pipeline ~session_user_id:42 ~url:poisoned () in
      let* response, body =
        Http_fixture.do_get ~target:(target "phch-any") pipeline
      in
      Alcotest.(check int) "500" 500 (status_of response);
      must body "Something went wrong on our side.";
      List.iter (must_not body)
        [ "phch_void"; "search_path"; "PostgreSQL"; "Caqti"; "relation" ];
      Lwt.return_unit)

let get_db_suite =
  [
    get_chooser_case;
    get_no_eligible_case;
    get_active_states_case;
    get_not_found_case;
    get_inconsistent_case;
    get_storage_case;
  ]

(* === POST: PRG, replay, and privacy === *)

let credential_needles =
  [
    ("access token", Project_fixture.pods_access_fixture);
    ("refresh token", Project_fixture.pods_refresh_fixture);
    ("authorization code", Github_fixture.gte_code_string);
    ("PKCE verifier", Github_fixture.gte_verifier_string);
    ("client secret", Github_fixture.gte_client_secret);
    ("OAuth state", Github_fixture.goc_fixture 'S');
    ("session binding", Github_fixture.goc_fixture 'B');
    ("private repository name", Github_fixture.gur_private_name);
    ("private repository description", Github_fixture.gur_private_description);
  ]

let post_prg_case =
  db_case "POST: request, PRG redirect, pending state, refresh, replay"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800011L ~slug:"phch-flow"
      in
      let* home =
        insert_community ~name:"Phch Flow Home" conn "phch-flow-home"
      in
      let* other =
        insert_community ~name:"Phch Flow Other" conn "phch-flow-other"
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, body =
        open_choice_page "chooser" ~slug:"phch-flow" pipeline
      in
      let frag = Html_assert.panel_fragment body in
      must frag "action='/projects/phch-flow/request-home'";
      must frag (radio_marker home);
      (* The submitted note carries CRLF endings and outer whitespace;
         the domain canonicalizes both. *)
      let note_raw = "  Prima riga \xe2\x98\x95\r\nseconda riga\t \r\n" in
      let note_canonical = "Prima riga \xe2\x98\x95\nseconda riga" in
      let* response =
        do_post ~cookie ~slug:"phch-flow"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", note_raw);
              ("dream.csrf", token);
            ]
          pipeline
      in
      (* Exact PRG: the same permanent route, no query string, no id. *)
      let* () =
        Http_fixture.check_redirect_lwt "created"
          "/projects/phch-flow/request-home" response
      in
      let redirect_cookies =
        String.concat "|" (Dream.headers response "Set-Cookie")
      in
      (* Exactly one pending relation with the canonical note and the
         session requester; nothing else changed. *)
      let* n = count_relations conn project in
      Alcotest.(check int) "one relation" 1 n;
      let* rid = find conn "pending id" q_pending_id project in
      let* ((_, rc), (rtype, status)), ((req, rev), (stored_note, _)) =
        find conn "relation row" Home_request_fixture.q_relation_row rid
      in
      Alcotest.(check int) "target community" home rc;
      Alcotest.(check string) "relation type home" "home" rtype;
      Alcotest.(check string) "status pending" "pending" status;
      Alcotest.(check (option int))
        "requester is the session user" (Some uid) req;
      Alcotest.(check (option int)) "no reviewer" None rev;
      Alcotest.(check (option string))
        "canonical note" (Some note_canonical) stored_note;
      let* members =
        find conn "members" Home_request_fixture.q_count_members home
      in
      Alcotest.(check int) "no membership created" 0 members;
      let* mods =
        find conn "moderators" Home_request_fixture.q_count_moderators home
      in
      Alcotest.(check int) "no moderator created" 0 mods;
      let* stewards =
        find conn "stewards" Home_request_fixture.q_count_stewards_for_project
          project
      in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      (* The redirected GET observes the durable pending state. *)
      let* response, body =
        Http_fixture.do_get ~cookie ~target:(target "phch-flow") pipeline
      in
      Http_fixture.check_page "pending state" response;
      let frag = Html_assert.panel_fragment body in
      must frag "Home request pending";
      must frag "Phch Flow Home";
      must_not frag "phc-request-form";
      must_not frag "<form";
      (* The private note and internal ids never render. *)
      must_not body "Prima riga";
      must_not frag (Int64.to_string rid);
      must_not frag (radio_marker other);
      (* No credential fixture reaches the page, the redirect cookies,
         or the relation row. *)
      let* blob =
        find conn "relation blob" Home_request_fixture.q_relation_text_blob rid
      in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool)
            (label ^ " absent from page")
            false
            (Html_assert.contains_nonempty ~needle body);
          Alcotest.(check bool)
            (label ^ " absent from cookies")
            false
            (Html_assert.contains_nonempty ~needle redirect_cookies);
          Alcotest.(check bool)
            (label ^ " absent from relation row")
            false
            (Html_assert.contains_nonempty ~needle blob))
        (credential_needles
        @ [
            ("installation id", "944800011");
            ("account id", "944900011");
            ("repository id", "945200011");
          ]);
      (* Refreshing the destination is a plain GET: no second relation. *)
      let* response, _ =
        Http_fixture.do_get ~cookie ~target:(target "phch-flow") pipeline
      in
      Http_fixture.check_page "refresh" response;
      let* n = count_relations conn project in
      Alcotest.(check int) "still one relation" 1 n;
      (* Replay with a fresh CSRF token: the authoritative active state
         answers 409 with no form and no second relation. *)
      let* fresh = Http_fixture.mint_token "replay mint" ~cookie pipeline in
      let* response =
        do_post ~cookie ~slug:"phch-flow"
          ~fields:
            [
              ("target_community_id", string_of_int other);
              ("request_note", "");
              ("dream.csrf", fresh);
            ]
          pipeline
      in
      Alcotest.(check int) "replay 409" 409 (status_of response);
      Alcotest.(check (option string))
        "replay no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      Alcotest.(check (option string))
        "replay no redirect" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      must frag "already has a pending request or community home";
      must frag "Home request pending";
      must frag "Phch Flow Home";
      must_not frag "phc-request-form";
      must_not frag "<form";
      must_not frag (radio_marker other);
      must_not frag (Int64.to_string rid);
      let* n = count_relations conn project in
      Alcotest.(check int) "replay adds nothing" 1 n;
      let* active =
        find conn "active count" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "one active row" 1 active;
      Lwt.return_unit)

(* === POST: structural and domain failures === *)

let post_invalid_form_case =
  db_case "POST: malformed forms re-render the current state as 400"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800012L ~slug:"phch-bad"
      in
      let* home = insert_community conn "phch-bad-home" in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _body =
        open_choice_page "chooser" ~slug:"phch-bad" pipeline
      in
      (* A cross-origin submission is rejected before the form. *)
      let* response =
        do_post ~origin:(Some "https://evil.example") ~cookie ~slug:"phch-bad"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", "phch-origin-zz9");
              ("dream.csrf", token);
            ]
          pipeline
      in
      Alcotest.(check int) "cross-origin 403" 403 (status_of response);
      let* () =
        check_no_relations "origin rejection inserts nothing" conn project
      in
      let* () =
        Lwt_list.iter_s
          (fun (label, fields) ->
            let* response =
              do_post ~cookie ~slug:"phch-bad"
                ~fields:(fields @ [ ("dream.csrf", token) ])
                pipeline
            in
            Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
            Alcotest.(check (option string))
              (label ^ ": no redirect") None
              (Dream.header response "Location");
            let* body = Dream.body response in
            let frag = Html_assert.panel_fragment body in
            (* The current chooser re-renders with generic feedback;
               nothing malformed is reflected. *)
            must frag "We couldn't read that request";
            must frag "action='/projects/phch-bad/request-home'";
            must frag (radio_marker home);
            must_not body "phch-zz9";
            Lwt.return_unit)
          [
            ("missing target", [ ("request_note", "phch-zz9") ]);
            ( "malformed target",
              [ ("target_community_id", "phch-zz9"); ("request_note", "") ] );
            ( "duplicate target",
              ("target_community_id", string_of_int home)
              :: request_fields ~target_id:(string_of_int home) ~note:"phch-zz9"
                   () );
            ( "unknown field",
              request_fields ~target_id:(string_of_int home) ~note:"" ()
              @ [ ("phch-zz9", "x") ] );
          ]
      in
      check_no_relations "no malformed submission inserts" conn project)

let post_invalid_note_case =
  db_case "POST: invalid notes are 422; only safe notes are preserved"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800013L ~slug:"phch-note"
      in
      let* home = insert_community conn "phch-note-home" in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _body =
        open_choice_page "chooser" ~slug:"phch-note" pipeline
      in
      (* A safe over-length note (2004 scalars) is rejected by the
         domain but preserved untruncated — escaped — in the textarea. *)
      let overlong = "<b>" ^ String.make 2000 'x' in
      let* response =
        do_post ~cookie ~slug:"phch-note"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", overlong);
              ("dream.csrf", token);
            ]
          pipeline
      in
      Alcotest.(check int) "overlength 422" 422 (status_of response);
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      must frag "We couldn't read that request";
      must frag ("&lt;b&gt;" ^ String.make 2000 'x');
      must_not frag "<b>";
      (* An unsafe control-byte note is rejected and never reflected. *)
      let* response =
        do_post ~cookie ~slug:"phch-note"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", "phch\x01unsafe-zz9");
              ("dream.csrf", token);
            ]
          pipeline
      in
      Alcotest.(check int) "unsafe 422" 422 (status_of response);
      let* body = Dream.body response in
      must_not body "unsafe-zz9";
      let frag = Html_assert.panel_fragment body in
      must frag "We couldn't read that request";
      must frag (radio_marker home);
      check_no_relations "no invalid note inserts" conn project)

(* === POST: community becomes unavailable === *)

let post_community_unavailable_case =
  db_case_lifecycle_relaxed
    "POST: a drifted target is a 409 reload without the target"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800014L ~slug:"phch-cu"
      in
      let* home = insert_community ~name:"Phch Cu Home" conn "phch-cu-home" in
      let* other =
        insert_community ~name:"Phch Cu Other" conn "phch-cu-other"
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, body =
        open_choice_page "chooser" ~slug:"phch-cu" pipeline
      in
      must (Html_assert.panel_fragment body) (radio_marker home);
      (* The target drifts to a valid but ineligible lifecycle between
         the render and the submission. *)
      let* () =
        exec conn "make private" Community_fixture.q_make_private home
      in
      let* response =
        do_post ~cookie ~slug:"phch-cu"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", "Nota da conservare phch");
              ("dream.csrf", token);
            ]
          pipeline
      in
      Alcotest.(check int) "409" 409 (status_of response);
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      must frag "no longer available for project requests";
      (* The reloaded chooser no longer offers the drifted target, the
         other community remains, and the safe note survives in the
         still-present textarea. *)
      must frag "action='/projects/phch-cu/request-home'";
      must_not frag (radio_marker home);
      must frag (radio_marker other);
      must frag "Nota da conservare phch";
      must_not frag "private";
      check_no_relations "no relation inserted" conn project)

(* === POST: concurrency === *)

let post_race_case =
  db_case "POST: racing submissions leave one pending row and a 409 loser"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800015L ~slug:"phch-race"
      in
      let* home = insert_community conn "phch-race-home" in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token1, _body =
        open_choice_page "chooser" ~slug:"phch-race" pipeline
      in
      let* token2 = Http_fixture.mint_token "second token" ~cookie pipeline in
      let post token =
        do_post ~cookie ~slug:"phch-race"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", "");
              ("dream.csrf", token);
            ]
          pipeline
      in
      let* r1, r2 = Lwt.both (post token1) (post token2) in
      let winner, loser =
        match (status_of r1, status_of r2) with
        | 303, 409 -> (r1, r2)
        | 409, 303 -> (r2, r1)
        | a, b -> Alcotest.failf "race statuses: %d and %d" a b
      in
      Alcotest.(check (option string))
        "winner PRG" (Some "/projects/phch-race/request-home")
        (Dream.header winner "Location");
      let* body = Dream.body loser in
      let frag = Html_assert.panel_fragment body in
      must frag "already has a pending request or community home";
      must frag "Home request pending";
      must_not frag "phc-request-form";
      let* total = count_relations conn project in
      Alcotest.(check int) "exactly one row" 1 total;
      let* active =
        find conn "active count" Home_request_fixture.q_count_active_for_project
          project
      in
      Alcotest.(check int) "exactly one active row" 1 active;
      Lwt.return_unit)

(* === POST: authorization changes between GET and POST === *)

let post_authorization_change_case =
  db_case "POST: lost stewardship or verification is the generic 404"
    (fun ~url conn ->
      let* uid = insert_user conn "phch_a" in
      let* inst, project =
        make_project conn ~user:uid ~ext_id:944800016L ~slug:"phch-drop"
      in
      let* home = insert_community conn "phch-drop-home" in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _body =
        open_choice_page "chooser" ~slug:"phch-drop" pipeline
      in
      let post () =
        do_post ~cookie ~slug:"phch-drop"
          ~fields:
            [
              ("target_community_id", string_of_int home);
              ("request_note", "");
              ("dream.csrf", token);
            ]
          pipeline
      in
      let expect_404 label =
        let* response = post () in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        let* body = Dream.body response in
        must body "This page does not exist.";
        check_no_relations (label ^ ": no relation") conn project
      in
      (* Stewardship removed between the render and the submission. *)
      let* () =
        exec conn "drop steward" Home_request_fixture.q_delete_steward
          (project, uid)
      in
      let* () = expect_404 "removed stewardship" in
      let* () =
        exec conn "restore steward" Home_request_fixture.q_insert_steward
          (project, uid, inst)
      in
      (* Verification lost between the render and the submission. *)
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* () = expect_404 "stale project" in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      expect_404 "revoked project")

(* === POST: storage failure === *)

let post_storage_case =
  db_case "POST: a store failure is one generic 500 with no partial row"
    (fun ~url conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "phch_a" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:944800017L ~slug:"phch-fail"
      in
      let* home = insert_community conn "phch-fail-home" in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _body =
        open_choice_page "chooser" ~slug:"phch-fail" pipeline
      in
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          let* response =
            do_post ~cookie ~slug:"phch-fail"
              ~fields:
                [
                  ("target_community_id", string_of_int home);
                  ("request_note", phch_poison_note);
                  ("dream.csrf", token);
                ]
              pipeline
          in
          Alcotest.(check int) "500" 500 (status_of response);
          Alcotest.(check (option string))
            "no-store" (Some "no-store")
            (Dream.header response "Cache-Control");
          let* body = Dream.body response in
          must body "Something went wrong on our side.";
          List.iter (must_not body)
            [
              "phch fixture failure";
              "RAISE";
              "community_projects";
              "PostgreSQL";
              "Caqti";
            ];
          check_no_relations "no partial relation" conn project)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

let post_db_suite =
  [
    post_prg_case;
    post_invalid_form_case;
    post_invalid_note_case;
    post_community_unavailable_case;
    post_race_case;
    post_authorization_change_case;
    post_storage_case;
  ]

let suites =
  (* Existing-community home request handlers: DB-free access gates
       for both routes, the POST's configuration/origin/CSRF gates (all
       rejections precede SQL), and the database-gated GET states and
       POST flow (PRG, replay, drifted targets, races, lost
       authorization, storage failure). *)
  [
    ("project_home_request_get_gates", get_gate_suite);
    ("project_home_request_post_gates", post_gate_suite);
    ("project_home_request_post_csrf", csrf_suite);
    ("project_home_request_get_db", get_db_suite);
    ("project_home_request_post_db", post_db_suite);
  ]
