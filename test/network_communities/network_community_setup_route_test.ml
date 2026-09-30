module Ob = Earde.Project_onboarding

(* === GET /c/:slug/setup and the settings navigation into it ===
   The publisher-only setup route: DB-free rollout/authentication gates
   (every rejection precedes any route read or SQL), the DB-free settings
   navigation and the suppression of the legacy identity controls, and the
   database-gated page, identical generic 404s, generic 500s, and the real
   navigation from the settings surface. Own reserved external-installation-
   id range 953000001..953000999 (hence account and forge-namespace ids
   953100001..953100999), ncph_% usernames, and ncph-% community slugs. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Hd = Earde.Network_community_publication_handlers

module Pv = Earde.Project_home_provisioning_store

let case = Case.quick

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let status_of = Http_fixture.status_of

let make_project = Home_provisioning_fixture.make_project

let route_pattern = "/c/:slug/setup"

let target slug = Printf.sprintf "/c/%s/setup" slug

let settings_target slug = Printf.sprintf "/c/%s/settings" slug

let make ~mode = Hd.make_network_community_publication_page_handler ~mode

(* === DB-free: the route shape === *)

(* The real router with the setup pattern alongside the existing settings
   GET, and nothing else: the setup target is dispatched exactly once and
   no alias or method variant of it exists. The publication POST is
   registered by its own suite (Ncpb) and deliberately absent here, so
   these assertions stay about the GET alone. Mode Off makes each dispatch
   observable as the clean /bring redirect, with no SQL. *)
let route_router () =
  Dream.router
    [ Dream.get route_pattern (fun req -> make ~mode:Ob.Off req);
      Dream.get "/c/:slug/settings" (fun _req -> Dream.html "settings")
    ]

let route_run ~method_ ~target =
  Http_fixture.gate_response "route"
    (Http_fixture.gate_run ~method_ ~target (route_router ()))

let route_cases =
  [ case "setup route: the registered GET pattern is exactly the one the \
          settings link points at, and coexists with the settings GET"
      (fun () ->
        Http_fixture.check_clean_redirect "setup dispatched" "/bring"
          (route_run ~method_:`GET ~target:(target "ncph-any"));
        let settings = route_run ~method_:`GET ~target:(settings_target "ncph-any") in
        Alcotest.(check int) "settings unshadowed" 200 (status_of settings))
  ; case "setup route: no alias and no method variant of the setup GET"
      (fun () ->
        List.iter
          (fun (method_, target) ->
            let response = route_run ~method_ ~target in
            Alcotest.(check int)
              (Printf.sprintf "unregistered %s" target)
              404 (status_of response))
          [ (`POST, target "ncph-any");
            (`POST, "/c/ncph-any/publish");
            (`GET, "/c/ncph-any/publish");
            (`GET, "/c/ncph-any/setup/extra");
            (`GET, "/setup");
            (`GET, "/c/setup")
          ])
  ]

(* === DB-free: rollout and authentication gates === *)

(* Unrouted: the handler runs with no "slug" parameter, so a request that
   passes every gate lands on the generic 404 without touching SQL. *)
let unrouted_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:(target "ncph-any") (make ~mode)

(* Routed but with no sql_pool installed: a request that passes every gate
   reaches Dream.sql and raises, which the harness reports as the DB
   boundary. That is the strong proof that the gates themselves ran no
   SQL. *)
let routed_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:(target "ncph-any")
    (Dream.router [ Dream.get route_pattern (fun req -> make ~mode req) ])

let gate_cases =
  [ case "GET setup off: clean /bring redirect before any route read or SQL"
      (fun () ->
        Http_fixture.check_clean_redirect "off" "/bring"
          (Http_fixture.gate_response "off"
             (unrouted_run ~session:Http_fixture.admin_session ~mode:Ob.Off ()));
        Http_fixture.check_clean_redirect "off routed" "/bring"
          (Http_fixture.gate_response "off routed"
             (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Off ())))
  ; case "GET setup: anonymous and malformed sessions to /login" (fun () ->
        Http_fixture.check_clean_redirect "anonymous" "/login"
          (Http_fixture.gate_response "anonymous" (routed_run ~mode:Ob.Public ()));
        List.iter
          (fun raw ->
            Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
              (Http_fixture.gate_response ("user_id " ^ raw)
                 (routed_run ~session:[ ("user_id", raw) ] ~mode:Ob.Public ())))
          [ "not-a-number"; ""; "0"; "-3"; " 42"; "42x" ];
        Http_fixture.check_clean_redirect "is_admin only" "/login"
          (Http_fixture.gate_response "is_admin only"
             (routed_run ~session:[ ("is_admin", "true") ] ~mode:Ob.Admins ())))
  ; case "GET setup admins mode: a non-admin is redirected before any route \
          read or SQL" (fun () ->
        Http_fixture.check_clean_redirect "non-admin" "/bring"
          (Http_fixture.gate_response "non-admin"
             (routed_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ())))
  ; case "GET setup: authorized modes pass the gates and reach the database \
          boundary, never before it" (fun () ->
        Http_fixture.check_db_boundary "admin in admins mode"
          (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Admins ());
        Http_fixture.check_db_boundary "user in public mode"
          (routed_run ~session:Http_fixture.logged_in ~mode:Ob.Public ());
        Http_fixture.check_db_boundary "admin in public mode"
          (routed_run ~session:Http_fixture.admin_session ~mode:Ob.Public ()))
  ; case "GET setup: a missing route parameter is the generic 404, with no \
          SQL" (fun () ->
        let response =
          Http_fixture.gate_response "no route"
            (unrouted_run ~session:Http_fixture.logged_in ~mode:Ob.Public ())
        in
        Alcotest.(check int) "404" 404 (status_of response);
        Alcotest.(check (option string)) "no-store" (Some "no-store")
          (Dream.header response "Cache-Control"))
  ]

(* === DB-free: settings navigation and legacy-control suppression === *)

let settings_community ?(slug = "ncph-nav") ?(network = true)
    ?(onboarding = Earde.Community_types.Community_draft)
    ?(visibility = Earde.Community_types.Community_private) ?(indexable = false)
    ?(discoverable = false) () : Earde.Community_types.community =
  { id = 4242; slug; name = "Ncph Nav"; description = None; rules = None;
    avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = true; visibility; indexable;
    is_network_community = network; onboarding_state = onboarding;
    discoverable }

(* The settings page renders a framework CSRF field, so it needs a live
   request under a secret + sessions pipeline; no SQL is touched. *)
let render_settings ?(panel = "visibility") ~community ~is_admin ~is_top_mod
    () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.community_settings_page ~is_admin ~is_top_mod
           ~open_reports_count:0 ~community ~mods:[] ~banned_users:[]
           ~members:[] ~sections:[] ~channels:[] req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline
          (Dream.request ~method_:`GET
             ~target:("/c/" ^ community.slug ^ "/settings?panel=" ^ panel)
             "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "settings renderer did not run"

let nav_link = "href='/c/ncph-nav/setup'"

let published_network =
  settings_community ~onboarding:Earde.Community_types.Community_published
    ~visibility:Earde.Community_types.Community_public ~indexable:true ~discoverable:true
    ()

let legacy_community =
  settings_community ~network:false
    ~onboarding:Earde.Community_types.Community_published
    ~visibility:Earde.Community_types.Community_public ~indexable:true ~discoverable:true
    ()

let nav_cases =
  [ case "settings nav: a network setup draft's top-mod and admin surfaces \
          expose the setup link, canonically and exactly once" (fun () ->
        let community = settings_community () in
        let top_mod = render_settings ~community ~is_admin:false ~is_top_mod:true () in
        Alcotest.(check bool) "top_mod link present" true
          (Html_assert.contains top_mod nav_link);
        Alcotest.(check bool) "exact link text" true
          (Html_assert.contains top_mod ">Complete setup and publish</a>");
        (* Exactly one navigation entry; the visibility panel additionally
           explains the suppression with its own inline pointer, so the
           href itself legitimately appears twice. *)
        Alcotest.(check int) "one nav entry, not two" 1
          (Html_assert.occurrences top_mod ("class='cm-index-link' " ^ nav_link));
        Alcotest.(check int) "one inline pointer" 1
          (Html_assert.occurrences top_mod ("<a " ^ nav_link ^ ">complete setup and publish</a>"));
        let admin = render_settings ~community ~is_admin:true ~is_top_mod:false () in
        Alcotest.(check bool) "admin link present" true
          (Html_assert.contains admin nav_link);
        (* Canonical slug only — never the community id, a lifecycle
           value, a form, or script. The framework CSRF fields are dropped
           first: their random token bytes could otherwise spell the id by
           chance. *)
        let clean = Html_assert.without_csrf_inputs top_mod in
        Alcotest.(check bool) "no community id" false (Html_assert.contains clean "4242");
        List.iter
          (fun needle ->
            Alcotest.(check bool) ("no " ^ needle) false (Html_assert.contains clean needle))
          [ "onboarding_state"; "is_network_community"; "draft'" ])
  ; case "settings nav: an unauthorized settings surface never gains the \
          link" (fun () ->
        let community = settings_community () in
        let regular = render_settings ~community ~is_admin:false ~is_top_mod:false () in
        Alcotest.(check bool) "no setup link" false (Html_assert.contains regular nav_link))
  ; case "settings nav: legacy and already-published communities never gain \
          the link" (fun () ->
        List.iter
          (fun (label, community) ->
            List.iter
              (fun (is_admin, is_top_mod) ->
                let html = render_settings ~community ~is_admin ~is_top_mod () in
                Alcotest.(check bool) (label ^ ": no setup link") false
                  (Html_assert.contains html nav_link))
              [ (true, true); (true, false); (false, true); (false, false) ])
          [ ("published network", published_network);
            ("legacy", legacy_community)
          ])
  ; case "settings nav: a draft slug outside the canonical network grammar \
          suppresses the link" (fun () ->
        List.iter
          (fun slug ->
            let community = settings_community ~slug () in
            let html = render_settings ~community ~is_admin:true ~is_top_mod:true () in
            Alcotest.(check bool)
              ("no link for " ^ String.escaped slug)
              false
              (Html_assert.contains html "/setup'"))
          [ ""; "Ncph-Nav"; "ncph nav"; "ncph--nav"; "ncph-"; "-ncph";
            "ncph_nav"; String.make 81 'a'
          ])
  ; case "settings suppression: a network setup draft renders none of the \
          legacy identity, visibility, or discovery controls" (fun () ->
        let community = settings_community () in
        List.iter
          (fun panel ->
            let html =
              render_settings ~panel ~community ~is_admin:true ~is_top_mod:true ()
            in
            List.iter
              (fun needle ->
                Alcotest.(check bool)
                  (panel ^ ": no " ^ needle)
                  false (Html_assert.contains html needle))
              [ "action='/update-community'";
                "action='/c/ncph-nav/settings/visibility'";
                "action='/c/ncph-nav/settings/indexability'";
                "name='description'"; "name='visibility'"; "name='indexable'"
              ])
          [ "visibility"; "profile" ];
        (* The suppression is explained, not silent, and it points at the
           canonical surface. *)
        let html = render_settings ~community ~is_admin:true ~is_top_mod:true () in
        Alcotest.(check bool) "explains the draft state" true
          (Html_assert.contains html "still a private setup draft");
        Alcotest.(check bool) "points at the setup surface" true
          (Html_assert.contains html "complete setup and publish</a>"))
  ; case "settings suppression: a published network community keeps its \
          visibility control but offers no independent discovery toggle"
      (fun () ->
        let html =
          render_settings ~community:published_network ~is_admin:true
            ~is_top_mod:true ()
        in
        Alcotest.(check bool) "visibility control kept" true
          (Html_assert.contains html "action='/c/ncph-nav/settings/visibility'");
        Alcotest.(check bool) "no discovery toggle" false
          (Html_assert.contains html "action='/c/ncph-nav/settings/indexability'");
        Alcotest.(check bool) "explains why" true
          (Html_assert.contains html
             "Discovery for this community follows the Public or Unlisted \
              choice"))
  ; case "settings suppression: a legacy community keeps every legacy \
          control exactly as before" (fun () ->
        let profile =
          render_settings ~panel:"profile" ~community:legacy_community
            ~is_admin:true ~is_top_mod:true ()
        in
        Alcotest.(check bool) "profile form kept" true
          (Html_assert.contains profile "action='/update-community'");
        let visibility =
          render_settings ~community:legacy_community ~is_admin:true
            ~is_top_mod:true ()
        in
        Alcotest.(check bool) "visibility control kept" true
          (Html_assert.contains visibility "action='/c/ncph-nav/settings/visibility'");
        Alcotest.(check bool) "indexability control kept" true
          (Html_assert.contains visibility "action='/c/ncph-nav/settings/indexability'"))
  ]

(* === Database-gated integration === *)

(* Distinctive credential-shaped fixtures. None may appear in any page,
   redirect, header, or cookie this feature produces. *)
let credential_markers =
  [ ("access token", "gho_NCPH_ACCESS_TOKEN_SECRET");
    ("refresh token", "ghr_NCPH_REFRESH_TOKEN");
    ("client secret", "NCPH_CLIENT_SECRET_VALUE");
    ("external installation id", "953000001");
    ("external account id", "953100001")
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 953100001 AND 953100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 953100001 AND 953100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 953000001 AND 953000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'ncph-%'"
    ; "DELETE FROM users WHERE username LIKE 'ncph_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 953000001 AND 953000999"
    ]

let q_community_id = Network_community_fixture.q_community_id

let q_publish = Network_community_fixture.q_publish

let q_delete_sections = Network_community_fixture.q_delete_sections

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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* One shared single-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, and this suite issues many requests across many
   identities, so a fresh pool per request would exhaust Postgres
   max_connections. The session identity is swapped per request through a
   ref instead; cases run sequentially. The real router binds the setup GET
   alongside the existing settings GET, so the navigation between them is
   exercised end to end. *)
let shared_identity : (int * bool) option ref = ref None

let shared_pipeline = ref None

let build_pipeline ~url =
  Dream.sql_pool ~size:1 url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ (fun handler request ->
       match !shared_identity with
       | None -> handler request
       | Some (uid, is_admin) ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           let* () =
             Dream.set_session_field request "username"
               ("ncph_user_" ^ string_of_int uid)
           in
           let* () =
             if is_admin then Dream.set_session_field request "is_admin" "true"
             else Lwt.return_unit
           in
           handler request)
  @@ Dream.router
       [ Dream.get route_pattern (fun req -> make ~mode:Ob.Public req);
         Dream.get "/c/:slug/settings"
           Earde.Handlers.community_settings_handler
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
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string)) (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy")

let check_generic_404 label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool) (label ^ ": generic copy") true
    (Html_assert.contains body "This page does not exist.")

let check_generic_500 label response body =
  Alcotest.(check int) (label ^ ": 500") 500 (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool) (label ^ ": generic copy") true
    (Html_assert.contains body "Something went wrong on our side.");
  List.iter
    (fun needle ->
      Alcotest.(check bool) (label ^ ": no detail " ^ needle) false
        (Html_assert.contains body needle))
    [ "open_source_projects"; "community_projects"; "Caqti"; "PostgreSQL";
      "SELECT"; "search_path"; "Inconsistent"; "ncph_void"
    ]

let check_no_credentials label response body =
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool) (label ^ ": body free of " ^ what) false
        (Html_assert.contains body needle);
      Alcotest.(check bool) (label ^ ": headers free of " ^ what) false
        (Html_assert.contains headers needle))
    credential_markers

(* === fixtures === *)

let provision_draft ?name ?description conn ~actor ~project_slug ~slug =
  let* r =
    Pv.provision conn ~actor_user_id:actor ~project_slug
      ~identity:(Network_community_fixture.identity ?name ?description ~slug ())
  in
  match r with
  | Ok _ -> find conn "community id" q_community_id slug
  | Error _ -> Alcotest.failf "provisioning fixture failed for %s" slug

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, role)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

(* === cases === *)

let publisher_page_case =
  db_case "GET setup: the creating top moderator, a second top moderator, \
           and a durable admin all get the page prefilled with the draft's \
           own identity" (fun ~url conn ->
      let* owner = insert_user conn "ncph_owner" in
      let* second = insert_user conn "ncph_second" in
      let* admin = insert_user conn "ncph_admin" in
      let* () = set_admin conn ~user:admin true in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:953000001L ~slug:"ncph-alpha"
          ~name:"Ncph Alpha"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncph-alpha"
          ~slug:"ncph-alpha-home" ~name:"Ncph Alpha Home"
          ~description:"Alpha body."
      in
      as_user owner;
      let* response, body = do_get ~url ~target:(target "ncph-alpha-home") () in
      check_page "creating top mod" response;
      check_no_credentials "creating top mod" response body;
      Alcotest.(check bool) "heading" true
        (Html_assert.contains body "Complete setup and publish");
      Alcotest.(check bool) "noindex" true (Html_assert.contains body "content='noindex'");
      Alcotest.(check bool) "exact action" true
        (Html_assert.contains body "action='/c/ncph-alpha-home/publish'");
      Alcotest.(check bool) "name prefilled" true
        (Html_assert.contains body "value='Ncph Alpha Home'");
      Alcotest.(check bool) "slug prefilled" true
        (Html_assert.contains body "value='ncph-alpha-home'");
      Alcotest.(check bool) "description prefilled" true
        (Html_assert.contains body "Alpha body.");
      Alcotest.(check bool) "public preselected" true
        (Html_assert.contains body "value='public' checked");
      Alcotest.(check bool) "project identity" true
        (Html_assert.contains body "Ncph Alpha");
      Alcotest.(check bool) "framework CSRF field" true
        (Html_assert.contains body Html_assert.csrf_input_prefix);
      (* No feedback, no flash, no query state. *)
      Alcotest.(check bool) "no alert" false (Html_assert.contains body "ncp-alert");
      (* A second current top moderator sees the same page. *)
      let* () = add_role conn ~user:second ~community "top_mod" in
      as_user second;
      let* response, body = do_get ~url ~target:(target "ncph-alpha-home") () in
      check_page "second top mod" response;
      Alcotest.(check bool) "second sees the form" true
        (Html_assert.contains body "action='/c/ncph-alpha-home/publish'");
      (* A durable global admin does too — with no local role at all. *)
      as_user admin;
      let* response, body = do_get ~url ~target:(target "ncph-alpha-home") () in
      check_page "durable admin" response;
      Alcotest.(check bool) "admin sees the form" true
        (Html_assert.contains body "action='/c/ncph-alpha-home/publish'");
      Lwt.return_unit)

let generic_404_case =
  db_case "GET setup: every unavailable or unauthorized community is one \
           byte-identical generic 404" (fun ~url conn ->
      let* owner = insert_user conn "ncph_genowner" in
      let* member = insert_user conn "ncph_genmember" in
      let* moddy = insert_user conn "ncph_genmod" in
      let* stranger = insert_user conn "ncph_genstranger" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:953000002L ~slug:"ncph-gen"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncph-gen"
          ~slug:"ncph-gen-home"
      in
      let bodies = ref [] in
      let expect_404 label ?(admin_session = false) ~user slug =
        as_user ~admin_session user;
        let* response, body = do_get ~url ~target:(target slug) () in
        check_generic_404 label response body;
        check_no_credentials label response body;
        bodies := body :: !bodies;
        Lwt.return_unit
      in
      let* () = expect_404 "missing community" ~user:owner "ncph-nothing" in
      let* () = expect_404 "malformed slug" ~user:owner "ncph gen home" in
      let* () = expect_404 "stranger" ~user:stranger "ncph-gen-home" in
      let* () =
        Lwt.bind (Db_fixture.exec conn "member" Community_fixture.q_insert_member (member, community))
          (fun () -> expect_404 "ordinary member" ~user:member "ncph-gen-home")
      in
      let* () = add_role conn ~user:moddy ~community "mod" in
      let* () = expect_404 "mod" ~user:moddy "ncph-gen-home" in
      (* A session is_admin claim never authorizes: only users.is_admin
         does, and this user has no durable row. *)
      let* () =
        expect_404 "session-shaped admin" ~admin_session:true ~user:stranger
          "ncph-gen-home"
      in
      (* A legacy community is as absent as a nonexistent one, even to its
         own top moderator. *)
      let* legacy = insert_community ~network:false conn "ncph-legacy" in
      let* () = add_role conn ~user:owner ~community:legacy "top_mod" in
      let* () = expect_404 "legacy community" ~user:owner "ncph-legacy" in
      (* Publication ends the surface. *)
      let* () = exec conn "publish" q_publish community in
      let* () = expect_404 "published community" ~user:owner "ncph-gen-home" in
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
  db_case "GET setup: durable draft corruption is one generic non-cacheable \
           500" (fun ~url conn ->
      let* owner = insert_user conn "ncph_icowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:953000003L ~slug:"ncph-ic"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncph-ic"
          ~slug:"ncph-ic-home"
      in
      let* () = exec conn "drop sections" q_delete_sections community in
      as_user owner;
      let* response, body = do_get ~url ~target:(target "ncph-ic-home") () in
      check_generic_500 "corrupt draft" response body;
      check_no_credentials "corrupt draft" response body;
      Lwt.return_unit)

let storage_case =
  db_case "GET setup: a real database failure is one generic non-cacheable \
           500" (fun ~url _conn ->
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=ncph_void"))
      in
      (* A dedicated single-connection pool for this one case: the shared
         pipeline must keep talking to the real schema. *)
      let saved = !shared_pipeline in
      shared_pipeline := None;
      let poison_pipe = build_pipeline ~url:poisoned in
      shared_pipeline := saved;
      as_user 42;
      let* response, body =
        do_get ~pipeline:poison_pipe ~url ~target:(target "ncph-anything") ()
      in
      check_generic_500 "poisoned schema" response body;
      Lwt.return_unit)

let navigation_case =
  db_case "settings navigation: the draft's settings surface links to the \
           real setup route, which serves the real page" (fun ~url conn ->
      let* owner = insert_user conn "ncph_navowner" in
      let* moddy = insert_user conn "ncph_navmod" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:953000004L ~slug:"ncph-nav"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncph-nav"
          ~slug:"ncph-nav-home" ~name:"Ncph Nav Home"
      in
      as_user owner;
      let* response, body =
        do_get ~url ~target:(settings_target "ncph-nav-home") ()
      in
      Alcotest.(check int) "settings 200" 200 (status_of response);
      Alcotest.(check bool) "setup link" true
        (Html_assert.contains body "href='/c/ncph-nav-home/setup'");
      Alcotest.(check bool) "exact label" true
        (Html_assert.contains body ">Complete setup and publish</a>");
      (* The legacy identity and discovery controls are gone from the
         draft's own settings surface. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("suppressed " ^ needle) false
            (Html_assert.contains body needle))
        [ "action='/update-community'";
          "action='/c/ncph-nav-home/settings/visibility'";
          "action='/c/ncph-nav-home/settings/indexability'"
        ];
      (* The advertised destination really serves the setup page. *)
      let* response, body = do_get ~url ~target:(target "ncph-nav-home") () in
      check_page "followed setup link" response;
      Alcotest.(check bool) "setup page" true
        (Html_assert.contains body "action='/c/ncph-nav-home/publish'");
      (* A regular mod reaches the settings page but never the link, and
         the setup route independently refuses them. *)
      let* () = add_role conn ~user:moddy ~community "mod" in
      as_user moddy;
      let* _response, body =
        do_get ~url ~target:(settings_target "ncph-nav-home") ()
      in
      Alcotest.(check bool) "mod sees no setup link" false
        (Html_assert.contains body "href='/c/ncph-nav-home/setup'");
      let* response, body = do_get ~url ~target:(target "ncph-nav-home") () in
      check_generic_404 "mod at the setup route" response body;
      Lwt.return_unit)

let db_suite =
  [ publisher_page_case; generic_404_case; inconsistent_case; storage_case;
    navigation_case ]

let suites =
    (* GET /c/:slug/setup: the exact registered route with no alias and no
       method variant, the DB-free rollout/authentication gates (every
       rejection precedes any route read or SQL), the settings navigation
       and the suppression of the legacy identity controls, and the
       database-gated page, byte-identical generic 404s, generic 500s, and
       the real navigation from the settings surface. *)
  [ ("network_community_publication_route", route_cases)
  ; ("network_community_publication_get_gates", gate_cases)
  ; ("network_community_publication_settings_nav", nav_cases)
  ; ("network_community_publication_handlers_db", db_suite)
  ]
