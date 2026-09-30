module Ob = Earde.Project_onboarding

(* === Dedicated-home creation handler (Project_home_provisioning_handlers) ===
   GET /projects/:slug/community-home/new: the DB-free rollout and
   authentication gates (every rejection precedes any route read or SQL — no
   sql_pool is installed in that harness), and the database-gated
   authorization, suggested prefill, generic collapse, and header contract
   over verified permanent projects and real relation stores. Reserved
   external-installation-id range 949200001..949200999 (hence account ids
   949300001..949300999), phvh_% usernames, and phvh-% community slugs so no
   suite shares fixtures. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Hd = Earde.Project_home_provisioning_handlers
module Rvs = Earde.Project_home_review_store

let case = Case.quick
let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let insert_community = Community_fixture.insert_community
let status_of = Http_fixture.status_of
let route_pattern = "/projects/:slug/community-home/new"
let target slug = Printf.sprintf "/projects/%s/community-home/new" slug
let make ~mode = Hd.make_project_home_provisioning_page_handler ~mode

(* === DB-free: rollout and authentication gates === *)

(* Unrouted: the handler runs with no "slug" parameter, so a request that
   passes every gate lands on the generic 404 without touching SQL. *)
let unrouted_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:(target "phvh-any")
    (make ~mode)

(* Routed but with no sql_pool installed: a request that passes every gate
   reaches Dream.sql and raises, which the harness reports as the DB
   boundary. That is the strong proof that the gates themselves ran no
   SQL. *)
let routed_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:(target "phvh-any")
    (Dream.router [ Dream.get route_pattern (fun req -> make ~mode req) ])

let gate_cases =
  [
    case
      "GET provisioning off: clean /bring redirect before any route read or SQL"
      (fun () ->
        Http_fixture.check_clean_redirect "off" "/bring"
          (Http_fixture.gate_response "off"
             (unrouted_run ~session:Http_fixture.admin_session ~mode:Ob.Off ()));
        Http_fixture.check_clean_redirect "off routed" "/bring"
          (Http_fixture.gate_response "off routed"
             (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Off ())));
    case "GET provisioning: anonymous and malformed sessions to /login"
      (fun () ->
        Http_fixture.check_clean_redirect "anonymous" "/login"
          (Http_fixture.gate_response "anonymous"
             (routed_run ~mode:Ob.Public ()));
        List.iter
          (fun raw ->
            Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
              (Http_fixture.gate_response ("user_id " ^ raw)
                 (routed_run ~session:[ ("user_id", raw) ] ~mode:Ob.Public ())))
          [ "not-a-number"; ""; "0"; "-3"; " 42"; "42x" ];
        Http_fixture.check_clean_redirect "is_admin only" "/login"
          (Http_fixture.gate_response "is_admin only"
             (routed_run ~session:[ ("is_admin", "true") ] ~mode:Ob.Admins ())));
    case
      "GET provisioning admins mode: a non-admin is redirected before any \
       route read or SQL" (fun () ->
        Http_fixture.check_clean_redirect "non-admin" "/bring"
          (Http_fixture.gate_response "non-admin"
             (routed_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ())));
    case
      "GET provisioning: authorized modes pass the gates and reach the \
       database boundary, never before it" (fun () ->
        Http_fixture.check_db_boundary "admin in admins mode"
          (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Admins ());
        Http_fixture.check_db_boundary "user in public mode"
          (routed_run ~session:Http_fixture.logged_in ~mode:Ob.Public ());
        Http_fixture.check_db_boundary "admin in public mode"
          (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Public ()));
    case
      "GET provisioning: a missing route parameter is the generic 404, with no \
       SQL" (fun () ->
        let response =
          Http_fixture.gate_response "no route"
            (unrouted_run ~session:Http_fixture.logged_in ~mode:Ob.Public ())
        in
        Alcotest.(check int) "404" 404 (status_of response);
        Alcotest.(check (option string))
          "no-store" (Some "no-store")
          (Dream.header response "Cache-Control"));
  ]

(* === Database-gated integration === *)

(* Distinctive credential-shaped fixtures. None may appear in any page,
   redirect, header, or cookie this feature produces. *)
let credential_markers =
  [
    ("access token", "gho_PHVH_ACCESS_TOKEN_SECRET");
    ("refresh token", "ghr_PHVH_REFRESH_TOKEN");
    ("client secret", "PHVH_CLIENT_SECRET_VALUE");
    ("external installation id", "949200001");
    ("external account id", "949300001");
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 949300001 \
       AND 949300999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       949300001 AND 949300999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 949200001 AND 949200999)";
      "DELETE FROM communities WHERE slug LIKE 'phvh-%'";
      "DELETE FROM users WHERE username LIKE 'phvh_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       949200001 AND 949200999";
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
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* One shared single-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, and this suite issues many requests across many
   identities, so a fresh pool per request would exhaust Postgres
   max_connections. The session identity is swapped per request through a
   ref instead; cases run sequentially. The real router path is bound to
   the real handler, alongside the sibling setup GET so the two project
   routes are proven to coexist. *)
let shared_identity : (int * bool) option ref = ref None
let shared_pipeline = ref None

let build_pipeline ~url =
  Dream.sql_pool ~size:1 url
  @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ (fun handler request ->
    match !shared_identity with
    | None -> handler request
    | Some (uid, is_admin) ->
        let* () =
          Dream.set_session_field request "user_id" (string_of_int uid)
        in
        let* () =
          if is_admin then Dream.set_session_field request "is_admin" "true"
          else Lwt.return_unit
        in
        handler request)
  @@ Dream.router
       [
         Dream.get route_pattern (fun req -> make ~mode:Ob.Public req);
         Dream.get "/projects/:slug/setup" (fun req ->
             Earde.Project_creation_handlers.make_project_home_setup_handler
               ~mode:Ob.Public req);
       ]

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline = build_pipeline ~url in
      shared_pipeline := Some pipeline;
      pipeline

let as_user ?(admin_session = false) uid =
  shared_identity := Some (uid, admin_session)

let do_get ?pipeline ~url ~target () =
  let pipeline =
    match pipeline with Some p -> p | None -> pipeline_for ~url
  in
  let* response = pipeline (Dream.request ~method_:`GET ~target "") in
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

let check_generic_404 label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool)
    (label ^ ": generic copy") true
    (Html_assert.contains body "This page does not exist.")

let check_generic_500 label response body =
  Alcotest.(check int) (label ^ ": 500") 500 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool)
    (label ^ ": generic copy") true
    (Html_assert.contains body "Something went wrong on our side.");
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no detail " ^ needle)
        false
        (Html_assert.contains body needle))
    [
      "open_source_projects";
      "community_projects";
      "Caqti";
      "PostgreSQL";
      "SELECT";
      "search_path";
      "Inconsistent";
      "phvh_void";
    ]

let check_no_credentials label response body =
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool)
        (label ^ ": body free of " ^ what)
        false
        (Html_assert.contains body needle);
      Alcotest.(check bool)
        (label ^ ": headers free of " ^ what)
        false
        (Html_assert.contains headers needle))
    credential_markers

(* === fixtures === *)

let make_project = Home_provisioning_fixture.make_project
let add_steward = Home_provisioning_fixture.add_steward
let add_top_mod = Home_provisioning_fixture.add_top_mod
let request_pending = Home_provisioning_fixture.request_pending
let review = Home_provisioning_fixture.review

(* === cases === *)

let steward_page_case =
  db_case
    "GET provisioning: a steward gets the page with the project's own identity \
     suggested" (fun ~url conn ->
      let* owner = insert_user conn "phvh_owner" in
      let* inst, _project =
        make_project conn ~user:owner ~ext_id:949200001L ~slug:"phvh-alpha"
          ~name:"Phvh Alpha" ~description:"Alpha body."
      in
      as_user owner;
      let* response, body = do_get ~url ~target:(target "phvh-alpha") () in
      check_page "steward" response;
      (* The page itself. *)
      Alcotest.(check bool)
        "heading" true
        (Html_assert.contains body "Create a community home");
      Alcotest.(check bool)
        "setup-draft copy" true
        (Html_assert.contains body "private setup draft");
      Alcotest.(check bool)
        "verification wording" true
        (Html_assert.contains body "Project connected through GitHub");
      Alcotest.(check bool)
        "noindex" true
        (Html_assert.contains body "content='noindex'");
      (* The one form, its exact action, and a live CSRF field. *)
      Alcotest.(check bool)
        "exact action" true
        (Html_assert.contains body
           "action='/projects/phvh-alpha/community-home'");
      Alcotest.(check bool)
        "framework CSRF field" true
        (Html_assert.contains body "name=\"dream.csrf\"");
      (* Suggestions prefilled from the project. *)
      Alcotest.(check bool)
        "name suggested" true
        (Html_assert.contains body "value='Phvh Alpha'");
      Alcotest.(check bool)
        "slug suggested" true
        (Html_assert.contains body "value='phvh-alpha'");
      Alcotest.(check bool)
        "description suggested" true
        (Html_assert.contains body ">Alpha body.</textarea>");
      (* No flash or query state, no officiality language. Script and
         style are checked over the feature fragment: the shared shell
         owns its own chrome. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("no " ^ needle) false
            (Html_assert.contains body needle))
        [
          "Official";
          "GitHub-approved";
          "GitHub-endorsed";
          "?feedback=";
          "?error=";
        ];
      let frag = Html_assert.panel_fragment body in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("fragment free of " ^ needle)
            false
            (Html_assert.contains frag needle))
        [ "<script"; "style='"; "onclick"; "http-equiv" ];
      Alcotest.(check int)
        "exactly one form in the fragment" 1
        (Html_assert.occurrences frag "<form");
      check_no_credentials "steward page" response body;
      (* A second steward is served identically. *)
      let* second = insert_user conn "phvh_second" in
      let* () =
        add_steward conn ~project:_project ~user:second ~installation:inst
      in
      as_user second;
      let* response, body = do_get ~url ~target:(target "phvh-alpha") () in
      check_page "second steward" response;
      Alcotest.(check bool)
        "second steward sees the form" true
        (Html_assert.contains body
           "action='/projects/phvh-alpha/community-home'");
      Lwt.return_unit)

let generic_404_case =
  db_case "GET provisioning: every unavailable project is the same generic 404"
    (fun ~url conn ->
      let* owner = insert_user conn "phvh_gowner" in
      let* stranger = insert_user conn "phvh_gstranger" in
      let* moderator = insert_user conn "phvh_gmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:949200010L ~slug:"phvh-gen"
      in
      let* cid = insert_community ~name:"Phvh Home" conn "phvh-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let bodies = ref [] in
      let expect_404 label ~user slug =
        as_user user;
        let* response, body = do_get ~url ~target:(target slug) () in
        check_generic_404 label response body;
        check_no_credentials label response body;
        bodies := body :: !bodies;
        Lwt.return_unit
      in
      let* () = expect_404 "missing project" ~user:owner "phvh-missing" in
      let* () = expect_404 "foreign project" ~user:stranger "phvh-gen" in
      let* () = expect_404 "malformed slug" ~user:owner "Phvh-Gen" in
      (* Stale and revoked. *)
      let* () =
        exec conn "stale" Home_request_fixture.q_set_verification
          (project, "stale")
      in
      let* () = expect_404 "stale project" ~user:owner "phvh-gen" in
      let* () =
        exec conn "revoked" Home_request_fixture.q_set_verification
          (project, "revoked")
      in
      let* () = expect_404 "revoked project" ~user:owner "phvh-gen" in
      let* () =
        exec conn "verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      (* Pending, then accepted. *)
      let* () =
        request_pending "pending" conn ~user:owner ~slug:"phvh-gen"
          ~community:cid
      in
      let* () = expect_404 "pending request" ~user:owner "phvh-gen" in
      let* () =
        review "accept" conn ~reviewer:moderator ~slug:"phvh-gen"
          ~community_slug:"phvh-home" Rvs.Accept
      in
      let* () = expect_404 "accepted home" ~user:owner "phvh-gen" in
      (* Unstewarded: the steward row is gone but the project remains. *)
      let* () =
        exec conn "unsteward" Home_request_fixture.q_delete_steward
          (project, owner)
      in
      let* () = expect_404 "removed stewardship" ~user:owner "phvh-gen" in
      (* Byte-identical answers: no state is distinguishable. *)
      (match !bodies with
      | [] -> Alcotest.fail "no 404 bodies captured"
      | first :: rest ->
          List.iteri
            (fun i body ->
              Alcotest.(check bool)
                (Printf.sprintf "404 body %d identical" i)
                true (String.equal first body))
            rest);
      Lwt.return_unit)

let inconsistent_case =
  db_case
    "GET provisioning: durable project corruption is one generic non-cacheable \
     500" (fun ~url conn ->
      let* owner = insert_user conn "phvh_icowner" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:949200020L ~slug:"phvh-ic"
      in
      let* () =
        exec conn "corrupt name"
          Home_provisioning_fixture.q_corrupt_name_control project
      in
      as_user owner;
      let* response, body = do_get ~url ~target:(target "phvh-ic") () in
      check_generic_500 "corrupt project" response body;
      check_no_credentials "corrupt project" response body;
      Lwt.return_unit)

let storage_case =
  db_case
    "GET provisioning: a real database failure is one generic non-cacheable 500"
    (fun ~url _conn ->
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=phvh_void"))
      in
      (* A dedicated single-connection pool for this one case: the shared
         pipeline must keep talking to the real schema. *)
      let saved = !shared_pipeline in
      shared_pipeline := None;
      let poison_pipe = build_pipeline ~url:poisoned in
      shared_pipeline := saved;
      as_user 42;
      let* response, body =
        do_get ~pipeline:poison_pipe ~url ~target:(target "phvh-anything") ()
      in
      check_generic_500 "poisoned schema" response body;
      Lwt.return_unit)

let navigation_case =
  db_case
    "setup page: the two distinct navigation links point at the real routes, \
     and the create link leads to the real page" (fun ~url conn ->
      let* owner = insert_user conn "phvh_navowner" in
      let* _ =
        make_project conn ~user:owner ~ext_id:949200030L ~slug:"phvh-nav"
          ~name:"Phvh Nav"
      in
      as_user owner;
      let* response, body = do_get ~url ~target:"/projects/phvh-nav/setup" () in
      Alcotest.(check int) "setup 200" 200 (status_of response);
      Alcotest.(check bool)
        "connect link" true
        (Html_assert.contains body "href='/projects/phvh-nav/request-home'");
      Alcotest.(check bool)
        "create link" true
        (Html_assert.contains body
           "href='/projects/phvh-nav/community-home/new'");
      Alcotest.(check bool)
        "distinct labels" true
        (Html_assert.contains body "Connect to an existing community"
        && Html_assert.contains body ">Create a community home</a>");
      Alcotest.(check bool)
        "no form in the setup fragment" false
        (Html_assert.contains (Html_assert.panel_fragment body) "<form");
      (* The advertised destination really serves the creation page. *)
      let* response, body = do_get ~url ~target:(target "phvh-nav") () in
      check_page "followed create link" response;
      Alcotest.(check bool)
        "creation page" true
        (Html_assert.contains body "action='/projects/phvh-nav/community-home'");
      Lwt.return_unit)

let db_suite =
  [
    steward_page_case;
    generic_404_case;
    inconsistent_case;
    storage_case;
    navigation_case;
  ]

let suites =
  (* The GET route: DB-free rollout/authentication gates (every
       rejection precedes any route read or SQL), and the database-gated
       steward page, identical generic 404s, generic 500s, and the real
       setup-page navigation into it. *)
  [
    ("project_home_provisioning_get_gates", gate_cases);
    ("project_home_provisioning_handlers_db", db_suite);
  ]
