module Ob = Earde.Project_onboarding
module Pi = Earde.Project_identity

(* === Network-community publication POST (POST /c/:slug/publish) ===
   The HTTP integration that completes the dedicated-community journey: the
   review-and-publish form the setup GET renders, submitted through the real
   router, the shared authenticated-mutation rate limiter, the same-origin
   policy, Dream's framework CSRF, the strict publication parser, and the
   atomic publication store.

   DB-free coverage is the route shape, the rollout/authentication gates,
   the configuration and origin sequence, and CSRF. Database-gated coverage
   (EARDE_TEST_DATABASE_URL, the same opt-in as Mod_scope) drives the whole
   GET -> POST -> 303 -> GET sequence over real rows, with its own reserved
   external-installation-id range 956000001..956000999 (hence account ids
   956100001..956100999), ncpb_% usernames, and ncpb-% community slugs so no
   suite shares fixtures. Verified projects come only through the real
   draft/selection/finalization chain and drafts only through the real
   provisioning store. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Hd = Earde.Network_community_publication_handlers
module Pgs = Earde.Network_community_publication_pages
module Rmh = Earde.Project_home_removal_handlers

let case = Case.quick
let counting_loader = Http_fixture.counting_loader
let ok_loader = Http_fixture.ok_loader
let status_of = Http_fixture.status_of
let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let insert_community = Community_fixture.insert_community
let make_draft = Network_community_fixture.make_draft
let snapshot = Network_community_fixture.snapshot
let check_unchanged = Network_community_fixture.check_unchanged
let check_published = Network_community_fixture.check_published
let search_lists = Network_community_fixture.search_lists

(* The two patterns this feature owns, and the neighbours the flow lands
   on. *)
let get_pattern = "/c/:slug/setup"
let post_pattern = "/c/:slug/publish"
let get_target slug = Printf.sprintf "/c/%s/setup" slug
let post_target slug = Printf.sprintf "/c/%s/publish" slug
let community_target slug = Printf.sprintf "/c/%s" slug
let settings_target slug = Printf.sprintf "/c/%s/settings" slug

let project_side_remove ~project ~community =
  Printf.sprintf "/projects/%s/community-home/%s/remove" project community

let community_side_remove ~community ~project =
  Printf.sprintf "/c/%s/projects/%s/remove-home" community project

let request_home_target slug = Printf.sprintf "/projects/%s/request-home" slug

let settings_projects_target slug =
  Printf.sprintf "/c/%s/settings?panel=projects" slug

let make_get ~mode = Hd.make_network_community_publication_page_handler ~mode

let make ~mode ~load_config =
  Hd.make_network_community_publication_handler ~mode ~load_config

(* Exactly the four application fields, in the page's own order. *)
let fields ?(name = "Ncpb Community Home") ?(slug = "ncpb-home")
    ?(description = "") ?(visibility = "public") () =
  [
    ("community_name", name);
    ("community_slug", slug);
    ("community_description", description);
    ("publication_visibility", visibility);
  ]

(* ============ DB-free: the route shape ============ *)

(* The real router with both patterns and the neighbouring settings GET,
   and nothing else: the POST target is dispatched exactly once, and no
   alias, sub-path, or method variant exists. Mode Off makes each dispatch
   observable as the clean /bring redirect, with no configuration read and
   no SQL. *)
let route_router () =
  Dream.router
    [
      Dream.get get_pattern (fun req -> make_get ~mode:Ob.Off req);
      Dream.post post_pattern (fun req ->
          make ~mode:Ob.Off ~load_config:(fun () -> ok_loader ()) req);
      Dream.get "/c/:slug/settings" (fun _req -> Dream.html "settings");
    ]

let route_run ~method_ ~target =
  Http_fixture.gate_response "route"
    (Http_fixture.gate_run ~method_ ~target (route_router ()))

let route_cases =
  [
    case
      "publication route: the registered POST pattern is exactly the action \
       the setup page's form emits" (fun () ->
        let html =
          Pgs.network_community_publication_page
            ~community:
              {
                Pgs.name = "Ncpb Home";
                slug = "ncpb-alpha-home";
                description = None;
              }
            ~project:
              {
                Pgs.name = "Ncpb Project";
                slug = "ncpb-alpha";
                namespace_login = "ncpb-owner";
                kind = Pi.Project;
              }
            ~values:
              {
                Pgs.community_name = "";
                community_slug = "";
                community_description = "";
                publication_visibility = "";
              }
            ~feedback:None ()
        in
        Alcotest.(check bool)
          "the page posts to the registered route" true
          (Html_assert.contains html
             (Printf.sprintf "action='%s'" (post_target "ncpb-alpha-home")));
        Alcotest.(check int)
          "exactly one form" 1
          (Html_assert.occurrences (Html_assert.panel_fragment html) "<form"));
    case
      "publication route: the POST target resolves once, the setup GET stays \
       registered, and no alias exists" (fun () ->
        Http_fixture.check_clean_redirect "post dispatches" "/bring"
          (route_run ~method_:`POST ~target:(post_target "ncpb-alpha-home"));
        Http_fixture.check_clean_redirect "get dispatches" "/bring"
          (route_run ~method_:`GET ~target:(get_target "ncpb-alpha-home"));
        (* The neighbouring settings GET is unshadowed by either. *)
        Alcotest.(check int)
          "settings unshadowed" 200
          (status_of
             (route_run ~method_:`GET ~target:(settings_target "ncpb-any")));
        List.iter
          (fun (label, method_, target) ->
            Alcotest.(check int)
              (label ^ ": unrouted") 404
              (status_of (route_run ~method_ ~target)))
          [
            ("GET on the POST path", `GET, post_target "ncpb-alpha-home");
            ("POST on the GET path", `POST, get_target "ncpb-alpha-home");
            ("trailing slash", `POST, post_target "ncpb-alpha-home" ^ "/");
            ("sub-path", `POST, post_target "ncpb-alpha-home" ^ "/confirm");
            ("plural alias", `POST, "/c/ncpb-alpha-home/publishes");
            ("legacy alias", `POST, "/c/ncpb-alpha-home/publication");
            ("bare", `POST, "/publish");
            ("segmentless", `POST, "/c/publish");
          ]);
  ]

(* ======== DB-free: shared rollout and authentication gates ======== *)

let post_run ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST
    ~target:(post_target "ncpb-any") (make ~mode ~load_config)

let routed_post ?session ?(headers = []) ?(mode = Ob.Public) ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST
    ~target:(post_target "ncpb-any")
    (Dream.router
       [ Dream.post post_pattern (fun req -> make ~mode ~load_config req) ])

let gate_cases =
  [
    case
      "POST publish off: clean /bring redirect before any route read, \
       configuration load, or SQL" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "off" "/bring"
          (Http_fixture.gate_response "off"
             (post_run ~session:Http_fixture.admin_session ~mode:Ob.Off
                ~load_config:loader ()));
        Http_fixture.check_clean_redirect "off routed" "/bring"
          (Http_fixture.gate_response "off routed"
             (routed_post ~session:Http_fixture.admin_session ~mode:Ob.Off
                ~load_config:loader ()));
        Alcotest.(check int) "loader never called" 0 !calls);
    case
      "POST publish: anonymous and malformed sessions to /login, loader \
       untouched" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "anonymous" "/login"
          (Http_fixture.gate_response "anonymous"
             (routed_post ~load_config:loader ()));
        List.iter
          (fun raw ->
            Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
              (Http_fixture.gate_response ("user_id " ^ raw)
                 (routed_post
                    ~session:[ ("user_id", raw) ]
                    ~load_config:loader ())))
          [ "not-a-number"; ""; "0"; "-3"; " 42"; "42x" ];
        (* A session admin claim alone is not an identity. *)
        Http_fixture.check_clean_redirect "is_admin only" "/login"
          (Http_fixture.gate_response "is_admin only"
             (routed_post
                ~session:[ ("is_admin", "true") ]
                ~mode:Ob.Admins ~load_config:loader ()));
        Alcotest.(check int) "loader never called" 0 !calls);
    case
      "POST publish admins mode: a non-admin is redirected before the \
       configuration load" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "non-admin" "/bring"
          (Http_fixture.gate_response "non-admin"
             (routed_post ~session:Http_fixture.logged_in ~mode:Ob.Admins
                ~load_config:loader ()));
        Alcotest.(check int) "loader never called" 0 !calls);
    case
      "POST publish: authorized sessions continue past the gates, and an \
       unrouted request is the generic 404 before configuration" (fun () ->
        (* Routerless: past the gates the missing route parameter is the
           next rejection, so reaching that defensive 404 proves the gates
           passed with no configuration read and no SQL (no sql_pool is
           installed). *)
        let continues label session mode =
          let loader, calls = counting_loader (ok_loader ()) in
          let response =
            Http_fixture.gate_response label
              (post_run ~session ~mode ~load_config:loader ())
          in
          Alcotest.(check int)
            (label ^ ": defensive 404")
            404 (status_of response);
          Alcotest.(check (option string))
            (label ^ ": no-store") (Some "no-store")
            (Dream.header response "Cache-Control");
          Alcotest.(check int) (label ^ ": loader untouched") 0 !calls
        in
        continues "admin in admins mode" Http_fixture.admin_session Ob.Admins;
        continues "user in public mode" Http_fixture.logged_in Ob.Public;
        continues "admin in public mode" Http_fixture.admin_session Ob.Public;
        (* Routed and authorized with a good configuration and origin: the
           next rejection is the missing content type, proving the whole
           prefix ran and still no SQL happened. *)
        Alcotest.(check int)
          "routed: 400 content type" 400
          (status_of
             (Http_fixture.gate_response "routed"
                (routed_post ~session:Http_fixture.logged_in
                   ~headers:[ ("Origin", "https://earde.com") ]
                   ~load_config:(fun () -> ok_loader ())
                   ()))));
  ]

(* ============ DB-free: configuration and origin ============ *)

let config_origin_cases =
  [
    case
      "POST publish: a configuration failure is a generic 503 that names \
       nothing" (fun () ->
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
        Alcotest.(check (option string))
          "no-store" (Some "no-store")
          (Dream.header response "Cache-Control");
        Alcotest.(check (option string))
          "referrer policy" (Some Earde.Request_origin.referrer_policy)
          (Dream.header response "Referrer-Policy");
        let body = Lwt_main.run (Dream.body response) in
        List.iter
          (fun needle ->
            Alcotest.(check bool)
              ("no leak " ^ needle) false
              (Html_assert.contains body needle))
          [
            "EARDE_PUBLIC_ORIGIN";
            "GITHUB_APP";
            "Missing";
            "Invalid";
            "public_origin";
          ]);
    case
      "POST publish origin gate: the exact same-origin policy, before any form \
       parse" (fun () ->
        let origin_run label ?sec_fetch_site origin =
          let headers =
            (match origin with Some o -> [ ("Origin", o) ] | None -> [])
            @
            match sec_fetch_site with
            | Some v -> [ ("Sec-Fetch-Site", v) ]
            | None -> []
          in
          Http_fixture.gate_response label
            (routed_post ~session:Http_fixture.logged_in ~headers
               ~load_config:(fun () -> ok_loader ())
               ())
        in
        let rejected label ?sec_fetch_site origin =
          Alcotest.(check int)
            (label ^ ": 403") 403
            (status_of (origin_run label ?sec_fetch_site origin))
        in
        (* Past the origin gate the missing content type is the next
           rejection (400) — reaching it proves the origin was accepted and
           that the form was not parsed before the check. *)
        let accepted label ?sec_fetch_site origin =
          Alcotest.(check int)
            (label ^ ": passes origin")
            400
            (status_of (origin_run label ?sec_fetch_site origin))
        in
        accepted "exact origin" (Some "https://earde.com");
        accepted "explicit default port" (Some "https://earde.com:443");
        accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
        rejected "cross-origin" (Some "https://evil.example");
        rejected "same-site subdomain" (Some "https://www.earde.com");
        rejected "wrong scheme" (Some "http://earde.com");
        rejected "null origin" (Some "null");
        rejected "mismatch beats metadata" ~sec_fetch_site:"same-origin"
          (Some "https://evil.example");
        rejected "no signals" None;
        rejected "cross-site" ~sec_fetch_site:"cross-site" None;
        rejected "same-site metadata" ~sec_fetch_site:"same-site" None;
        let leak = origin_run "reflection" (Some "https://evil.example") in
        Alcotest.(check bool)
          "origin never reflected" false
          (Html_assert.contains (Lwt_main.run (Dream.body leak)) "evil.example"));
  ]

(* ==================== DB-free: CSRF ==================== *)

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
          Dream.post post_pattern (fun r ->
              make ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) r);
        ]
        req

let csrf_post ?cookie ?(content_type = true) pipeline body_fields =
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
         (Dream.request ~method_:`POST ~target:(post_target "ncpb-any") ~headers
            (Http_fixture.form_body body_fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  Alcotest.(check int)
    (label ^ ": status") expected
    (status_of (Http_fixture.gate_response label result))

let csrf_cases =
  [
    case
      "POST publish CSRF: Dream verification gates every submission; only a \
       verified form reaches the database" (fun () ->
        let pipeline = csrf_pipeline () in
        let cookie, fresh, expired = Http_fixture.mint_tokens "mint" pipeline in
        let valid = fields () in
        (* Every CSRF failure refuses the submission, but it is answered by
           re-rendering the owner-authorized page (a fresh token, nothing
           reflected) rather than a terminal page — so each one reaches the
           read model's DB boundary and none reaches the store. A page
           whose one-hour token outlived its fourteen-day session must not
           become permanently unsubmittable. *)
        Http_fixture.check_db_boundary "missing token re-renders"
          (csrf_post ~cookie pipeline valid);
        Http_fixture.check_db_boundary "invalid token re-renders"
          (csrf_post ~cookie pipeline (("dream.csrf", "not-a-token") :: valid));
        Http_fixture.check_db_boundary "expired token re-renders"
          (csrf_post ~cookie pipeline (("dream.csrf", expired) :: valid));
        Http_fixture.check_db_boundary "duplicate tokens re-render"
          (csrf_post ~cookie pipeline
             (("dream.csrf", fresh) :: ("dream.csrf", fresh) :: valid));
        Http_fixture.check_db_boundary "wrong session re-renders"
          (csrf_post pipeline (("dream.csrf", fresh) :: valid));
        (* A wrong content type is still answered without any SQL. *)
        csrf_rejected "wrong content type" 400
          (csrf_post ~cookie ~content_type:false pipeline
             (("dream.csrf", fresh) :: valid));
        (* A rejected token still answers before the application fields are
           even looked at: the re-render is the same whatever was sent. *)
        Http_fixture.check_db_boundary "bad token beats bad form"
          (csrf_post ~cookie pipeline
             [ ("dream.csrf", "not-a-token"); ("ncpb_unknown", "x") ]);
        (* A verified token reaches Dream.sql, which raises without a pool:
           CSRF passed, Dream stripped its own field, and the store call is
           the next thing to run. *)
        Http_fixture.check_db_boundary "verified form continues"
          (csrf_post ~cookie pipeline (("dream.csrf", fresh) :: valid)));
    case
      "POST publish CSRF: a verified token with an invalid form still \
       re-authorizes through the database" (fun () ->
        let pipeline = csrf_pipeline () in
        let cookie, fresh, _ = Http_fixture.mint_tokens "mint" pipeline in
        (* Every parser rejection re-renders the owner-authorized page,
           which needs the read model — so it too reaches the database
           boundary rather than answering from the submission alone. *)
        List.iter
          (fun (label, body_fields) ->
            Http_fixture.check_db_boundary label
              (csrf_post ~cookie pipeline
                 (("dream.csrf", fresh) :: body_fields)))
          [
            ("unknown field", ("ncpb_unknown", "x") :: fields ());
            ("blank name", fields ~name:"   " ());
            ("uppercase slug", fields ~slug:"Ncpb-Home" ());
            ( "control byte in description",
              fields ~description:"ncpb\001body" () );
            ("unknown visibility", fields ~visibility:"private" ());
          ]);
  ]

(* ================= Database-gated integration ================= *)

(* Distinctive credential-shaped fixtures. None may appear in any page,
   redirect, header, or cookie this feature produces. *)
let access_marker = "gho_NCPB_ACCESS_TOKEN_SECRET"
let refresh_marker = "ghr_NCPB_REFRESH_TOKEN"
let pkce_marker = "NCPB_PKCE_VERIFIER_VALUE"
let state_marker = "NCPB_OAUTH_STATE_VALUE"
let secret_marker = "NCPB_CLIENT_SECRET_VALUE"

let credential_markers =
  [
    ("access token", access_marker);
    ("refresh token", refresh_marker);
    ("PKCE verifier", pkce_marker);
    ("OAuth state", state_marker);
    ("client secret", secret_marker);
    ("external installation id", "956000001");
    ("external account id", "956100001");
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 956100001 \
       AND 956100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       956100001 AND 956100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 956000001 AND 956000999)";
      "DELETE FROM communities WHERE slug LIKE 'ncpb-%'";
      "DELETE FROM users WHERE username LIKE 'ncpb_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       956000001 AND 956000999";
      "DELETE FROM rate_limits WHERE endpoint LIKE '/c/ncpb-%'";
      "DELETE FROM rate_limits WHERE endpoint LIKE '/projects/ncpb-%'";
    ]

(* The shared authenticated-mutation limiter buckets by (client ip, path).
   Every case but the limiter's own drives more submissions against one
   path than a 60-second window allows, so the bucket is cleared between
   attempts; the limiter itself stays installed exactly as production
   wires it. *)
let q_clear_limits =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DELETE FROM rate_limits WHERE endpoint LIKE '/c/ncpb-%' OR endpoint LIKE \
     '/projects/ncpb-%'"

let q_count_by_slug = Network_community_fixture.q_count_by_slug
let q_community_state = Network_community_fixture.q_community_state
let q_delete_sections = Network_community_fixture.q_delete_sections
let q_set_verification = Home_request_fixture.q_set_verification
let q_mark_removed = Home_request_fixture.q_mark_removed
let q_insert_moderator = Community_fixture.q_insert_moderator
let q_remove_moderator = Community_fixture.q_remove_moderator
let q_set_moderator_role = Community_fixture.q_set_moderator_role
let q_insert_member = Community_fixture.q_insert_member
let q_set_admin = Community_fixture.q_set_admin

let q_verification =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT verification_status FROM open_source_projects WHERE id = $1"

let q_relation_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM community_projects WHERE id = $1"

let q_insert_post =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id) VALUES ('Ncpb \
     Post', 'Ncpb body', $1, $2) RETURNING id"

let q_count_posts =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM posts WHERE community_id = $1"

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

(* === the real production pipeline shape ===

   One shared two-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, so a fresh pool per request would exhaust
   Postgres max_connections; two connections are the minimum that lets the
   concurrency cases run two requests genuinely at once. Everything else is
   the real production shape — secret, memory sessions, the real router
   paths bound to the real handlers, and every mutation POST wrapped in the
   same authenticated-mutation rate limiter main.ml applies. Session
   identity is sticky: a request that already carries a session keeps its
   own user, so independent cookies stay independent under concurrency. *)
let shared_identity : (int * bool) option ref = ref None
let shared_pipeline = ref None

let identity_middleware handler request =
  match Dream.session_field request "user_id" with
  | Some _ -> handler request
  | None -> (
      match !shared_identity with
      | None -> handler request
      | Some (uid, is_admin) ->
          let* () =
            Dream.set_session_field request "user_id" (string_of_int uid)
          in
          let* () =
            Dream.set_session_field request "username"
              ("ncpb_user_" ^ string_of_int uid)
          in
          let* () =
            if is_admin then Dream.set_session_field request "is_admin" "true"
            else Lwt.return_unit
          in
          handler request)

let limited inner = Earde.Rate_limit_middleware.middleware inner

(* Stands in for the production limiter's Allowed decision, so a case can
   reach the handler over a pool the real limiter (which fails closed)
   cannot query. *)
let allowing_limiter =
  Earde.Rate_limit_middleware.make_middleware
    ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed))
    ~cleanup:ignore

let build_pipeline_with ~limited ~url =
  Dream.sql_pool ~size:2 url
  @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions @@ identity_middleware
  @@ Dream.router
       [
         Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.get get_pattern (fun req -> make_get ~mode:Ob.Public req);
         Dream.post post_pattern
           (limited (fun req ->
                make ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) req));
         Dream.post
           "/projects/:project_slug/community-home/:community_slug/remove"
           (limited (fun req ->
                Rmh.make_project_side_home_removal_handler ~mode:Ob.Public
                  ~load_config:(fun () -> ok_loader ())
                  req));
         Dream.post "/c/:community_slug/projects/:project_slug/remove-home"
           (limited (fun req ->
                Rmh.make_community_side_home_removal_handler ~mode:Ob.Public
                  ~load_config:(fun () -> ok_loader ())
                  req));
         Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
         Dream.get "/c/:slug/settings"
           Earde.Community_settings_handlers.community_settings_handler;
       ]

let build_pipeline ~url = build_pipeline_with ~limited ~url

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline = build_pipeline ~url in
      shared_pipeline := Some pipeline;
      pipeline

let as_user ?(admin_session = false) uid =
  shared_identity := Some (uid, admin_session)

let as_anonymous () = shared_identity := None
let clear_limits conn = exec conn "clear rate limits" q_clear_limits ()

let do_get ?pipeline ?cookie ~url ~target () =
  let pipeline =
    match pipeline with Some p -> p | None -> pipeline_for ~url
  in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = pipeline (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* [?conn] clears this feature's limiter buckets first, so a case that
   drives many submissions against one path stays deterministic without
   removing the limiter from the pipeline. *)
let do_post ?pipeline ?conn ?(origin = Some "https://earde.com") ~url ~cookie
    ~target ~token ~body_fields () =
  let* () =
    match conn with Some c -> clear_limits c | None -> Lwt.return_unit
  in
  let pipeline =
    match pipeline with Some p -> p | None -> pipeline_for ~url
  in
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [
        ("Content-Type", "application/x-www-form-urlencoded"); ("Cookie", cookie);
      ]
  in
  let* response =
    pipeline
      (Dream.request ~method_:`POST ~target ~headers
         (Http_fixture.form_body (("dream.csrf", token) :: body_fields)))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* A field-free POST, for the removal routes this flow interacts with. *)
let do_bare_post ?conn ~url ~cookie ~target ~token () =
  do_post ?conn ~url ~cookie ~target ~token ~body_fields:[] ()

(* One cookie-less GET that opens a fresh session for this user and returns
   its cookie plus a live CSRF token. *)
let open_session ?(admin_session = false) label ~url uid =
  as_user ~admin_session uid;
  let* response, token = do_get ~url ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return (Http_fixture.session_cookie label response, token)

let mint_token label ~url ~cookie =
  let* response, body = do_get ~url ~cookie ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return body

(* One cookie-less GET of the setup page: the page, its session cookie, and
   the live CSRF token its own form carries. *)
let open_setup label ~url ~slug uid =
  as_user uid;
  let* response, body = do_get ~url ~target:(get_target slug) () in
  Alcotest.(check int) (label ^ ": setup page 200") 200 (status_of response);
  Lwt.return
    ( Http_fixture.session_cookie label response,
      Http_fixture.csrf_of_page label body,
      body )

let check_clean_redirect label expected response body =
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
  Alcotest.(check string) (label ^ ": empty body") "" body

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
      "communities_slug_key";
      "community_projects";
      "open_source_projects";
      "Caqti";
      "PostgreSQL";
      "SELECT";
      "search_path";
      "Inconsistent";
      "Storage_error";
      "ncpb_void";
    ]

(* The re-rendered publication form: the requested status, the exact
   feedback copy, a live CSRF field, and the one form pointing back at this
   route under the community's current slug. *)
let check_rerender label ~status ~feedback ~slug response body =
  Alcotest.(check int) (label ^ ": status") status (status_of response);
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy");
  Alcotest.(check bool)
    (label ^ ": feedback copy")
    true
    (Html_assert.contains body feedback);
  Alcotest.(check bool)
    (label ^ ": fresh CSRF field")
    true
    (Html_assert.contains body "name=\"dream.csrf\"");
  Alcotest.(check bool)
    (label ^ ": posts back to this route")
    true
    (Html_assert.contains body
       (Printf.sprintf "action='%s'" (post_target slug)));
  Alcotest.(check int)
    (label ^ ": exactly one form")
    1
    (Html_assert.occurrences (Html_assert.panel_fragment body) "<form")

(* The existing community-page unavailable answer, asserted exactly as the
   legacy handler already produces it: this slice deliberately changes no
   community authorization. *)
let check_community_unavailable label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check bool)
    (label ^ ": generic copy") true
    (Html_assert.contains body "This community does not exist.")

let check_body_no_credentials label body =
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool)
        (label ^ ": body free of " ^ what)
        false
        (Html_assert.contains body needle))
    credential_markers

let check_no_credentials label response body =
  check_body_no_credentials label body;
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool)
        (label ^ ": headers free of " ^ what)
        false
        (Html_assert.contains headers needle))
    credential_markers

let location response =
  match Dream.header response "Location" with Some l -> l | None -> ""

(* Masks one intentionally-varying substring so two answers can be compared
   for byte-identity everywhere else. *)
let replace_substring haystack ~needle ~replacement =
  let nl = String.length needle in
  if nl = 0 then haystack
  else
    let buf = Buffer.create (String.length haystack) in
    let rec go i =
      match Html_assert.index_from haystack needle i with
      | None ->
          Buffer.add_string buf
            (String.sub haystack i (String.length haystack - i))
      | Some j ->
          Buffer.add_string buf (String.sub haystack i (j - i));
          Buffer.add_string buf replacement;
          go (j + nl)
    in
    go 0;
    Buffer.contents buf

(* === success: Public === *)

let public_success_case =
  db_case
    "POST publish: a top moderator's Public submission publishes the community \
     and redirects to its final public home" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_owner" in
      let* reader = insert_user conn "ncpb_reader" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000001L
          ~project_slug:"ncpb-alpha" ~slug:"ncpb-alpha-home"
          ~name:"Ncpb Alpha Home"
      in
      (* Real content inside the draft, so "content remains" is asserted
         against something. *)
      let* _post = find conn "post" q_insert_post (cid, owner) in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, page =
        open_setup "setup" ~url ~slug:"ncpb-alpha-home" owner
      in
      Alcotest.(check bool)
        "the form posts to the POST route" true
        (Html_assert.contains page
           (Printf.sprintf "action='%s'" (post_target "ncpb-alpha-home")));
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-alpha-home")
          ~token
          ~body_fields:
            (fields ~name:"Ncpb Alpha Hub" ~slug:"ncpb-alpha-hub"
               ~description:"A durable hub description." ~visibility:"public" ())
          ()
      in
      check_clean_redirect "published"
        (community_target "ncpb-alpha-hub")
        response body;
      check_no_credentials "published redirect" response body;
      (* No query, fragment, result token, or internal identifier — and
         never the settings route. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("Location free of " ^ needle)
            false
            (Html_assert.contains (location response) needle))
        [ "?"; "#"; "published"; "ok="; "settings" ];
      (* The exact durable lifecycle: published, public, indexable,
         discoverable, with everything else byte-unchanged. *)
      let* () =
        check_published "durable" conn ~cid ~project ~rid ~before
          ~name:"Ncpb Alpha Hub" ~description:"A durable hub description."
          ~slug:"ncpb-alpha-hub" ~old_slug:"ncpb-alpha-home" ~indexable:true
          ~discoverable:true
      in
      let* posts = find conn "posts" q_count_posts cid in
      Alcotest.(check int) "the draft's content survives" 1 posts;
      let* status = find conn "relation" q_relation_status rid in
      Alcotest.(check string) "relation still accepted" "accepted" status;
      (* The advertised destination really serves the published community,
         to its publisher, to an unrelated user, and anonymously. *)
      let* response, page_body =
        do_get ~url ~cookie ~target:(community_target "ncpb-alpha-hub") ()
      in
      Alcotest.(check int)
        "publisher reads the community" 200 (status_of response);
      Alcotest.(check bool)
        "the published name renders" true
        (Html_assert.contains page_body "Ncpb Alpha Hub");
      Alcotest.(check bool)
        "a Public community is indexable" false
        (Html_assert.contains page_body "content='noindex'");
      check_no_credentials "published community page" response page_body;
      let* reader_cookie, _ = open_session "reader" ~url reader in
      let* response, _ =
        do_get ~url ~cookie:reader_cookie
          ~target:(community_target "ncpb-alpha-hub")
          ()
      in
      Alcotest.(check int) "an unrelated user reads it" 200 (status_of response);
      as_anonymous ();
      let* response, _ =
        do_get ~url ~target:(community_target "ncpb-alpha-hub") ()
      in
      Alcotest.(check int)
        "an anonymous visitor reads it" 200 (status_of response);
      (* The old slug is gone: no alias, no redirect. *)
      let* response, body =
        do_get ~url ~target:(community_target "ncpb-alpha-home") ()
      in
      check_community_unavailable "old slug" response body;
      (* Public discovery lists it. *)
      let* () =
        search_lists "discovery" conn ~name:"Ncpb Alpha Hub"
          ~slug:"ncpb-alpha-hub" true
      in
      (* The setup surface is over: the draft no longer exists. *)
      as_user owner;
      let* response, body =
        do_get ~url ~cookie ~target:(get_target "ncpb-alpha-hub") ()
      in
      check_generic_404 "setup after publication" response body;
      Lwt.return_unit)

(* === success: Unlisted === *)

let unlisted_success_case =
  db_case
    "POST publish: an Unlisted submission stays reachable by URL but out of \
     indexing and discovery" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_unowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000002L ~project_slug:"ncpb-unl"
          ~slug:"ncpb-unl-home" ~name:"Ncpb Unlisted Home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, _ =
        open_setup "setup" ~url ~slug:"ncpb-unl-home" owner
      in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-unl-home")
          ~token
          ~body_fields:
            (fields ~name:"Ncpb Unlisted Home" ~slug:"ncpb-unl-home"
               ~visibility:"unlisted" ())
          ()
      in
      (* Publishing under the unchanged slug: normal success, no false
         conflict handling. *)
      check_clean_redirect "published unlisted"
        (community_target "ncpb-unl-home")
        response body;
      let* () =
        check_published "durable" conn ~cid ~project ~rid ~before
          ~name:"Ncpb Unlisted Home" ~description:"<null>" ~slug:"ncpb-unl-home"
          ~old_slug:"ncpb-unl-home" ~indexable:false ~discoverable:false
      in
      (* Direct anonymous reachability under existing public
         authorization. *)
      as_anonymous ();
      let* response, page_body =
        do_get ~url ~target:(community_target "ncpb-unl-home") ()
      in
      Alcotest.(check int) "anonymous direct URL works" 200 (status_of response);
      Alcotest.(check bool)
        "but it is marked noindex" true
        (Html_assert.contains page_body "content='noindex'");
      (* And it is absent from discovery. *)
      let* () =
        search_lists "discovery" conn ~name:"Ncpb Unlisted Home"
          ~slug:"ncpb-unl-home" false
      in
      Lwt.return_unit)

(* === form parsing: 422 rendering and safe value preservation === *)

let long_name = "Ncpb " ^ String.make 130 'n'
let long_description = String.make 2100 'd'

let parser_rejection_case =
  db_case
    "POST publish: every parser rejection is a 422 re-render that preserves \
     only provably safe values and mutates nothing" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_fowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000003L ~project_slug:"ncpb-form"
          ~slug:"ncpb-form-home" ~name:"Ncpb Form Home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, _ =
        open_setup "setup" ~url ~slug:"ncpb-form-home" owner
      in
      let target = post_target "ncpb-form-home" in
      (* 1. Structural: an unknown field carrying a credential-shaped value
         is Invalid_form, and nothing submitted is reflected back. *)
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (("ncpb_planted", access_marker)
            :: fields ~name:"Ncpb Planted Name" ~slug:"ncpb-planted"
                 ~description:"Planted body." ~visibility:"unlisted" ())
          ()
      in
      check_rerender "invalid form" ~status:422
        ~feedback:"We couldn't read that submission." ~slug:"ncpb-form-home"
        response body;
      check_no_credentials "invalid form" response body;
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("invalid form reflects no " ^ needle)
            false
            (Html_assert.contains body needle))
        [ "ncpb_planted"; "Ncpb Planted Name"; "ncpb-planted"; "Planted body." ];
      Alcotest.(check bool)
        "the controls are blank" true
        (Html_assert.contains body
           "name='community_slug' maxlength='80' value=''");
      (* A duplicated known field is structurally invalid too, and reflects
         nothing. *)
      let* token = mint_token "duplicate" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (("community_slug", "ncpb-dup-one")
            :: fields ~slug:"ncpb-dup-two" ())
          ()
      in
      check_rerender "duplicate field" ~status:422
        ~feedback:"We couldn't read that submission." ~slug:"ncpb-form-home"
        response body;
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("duplicate reflects no " ^ needle)
            false
            (Html_assert.contains body needle))
        [ "ncpb-dup-one"; "ncpb-dup-two" ];
      (* 2. Semantic name: the exact submitted values survive. *)
      let* token = mint_token "name" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:long_name ~slug:"ncpb-kept-slug"
               ~description:"Kept body." ~visibility:"unlisted" ())
          ()
      in
      check_rerender "invalid name" ~status:422
        ~feedback:"Enter a community name we can use." ~slug:"ncpb-form-home"
        response body;
      Alcotest.(check bool)
        "name preserved" true
        (Html_assert.contains body long_name);
      Alcotest.(check bool)
        "slug preserved" true
        (Html_assert.contains body "value='ncpb-kept-slug'");
      Alcotest.(check bool)
        "description preserved" true
        (Html_assert.contains body ">Kept body.</textarea>");
      Alcotest.(check bool)
        "choice preserved" true
        (Html_assert.contains body "value='unlisted' checked");
      (* 3. Semantic slug. *)
      let* token = mint_token "slug" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Kept Name" ~slug:"Ncpb-Bad-Slug"
               ~description:"Kept body." ())
          ()
      in
      check_rerender "invalid slug" ~status:422
        ~feedback:"Enter a community address using lowercase letters"
        ~slug:"ncpb-form-home" response body;
      Alcotest.(check bool)
        "slug preserved verbatim" true
        (Html_assert.contains body "value='Ncpb-Bad-Slug'");
      Alcotest.(check bool)
        "name preserved" true
        (Html_assert.contains body "value='Ncpb Kept Name'");
      (* 4. Semantic description. *)
      let* token = mint_token "description" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Kept Name" ~slug:"ncpb-kept-slug"
               ~description:long_description ())
          ()
      in
      check_rerender "invalid description" ~status:422
        ~feedback:"That description can't be used." ~slug:"ncpb-form-home"
        response body;
      Alcotest.(check bool)
        "description preserved" true
        (Html_assert.contains body long_description);
      (* 5. Publication choice: every rejected spelling, including the one
         shape a network community can never have. *)
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            let* token = mint_token "visibility" ~url ~cookie in
            let* response, body =
              do_post ~conn ~url ~cookie ~target ~token
                ~body_fields:
                  (fields ~name:"Ncpb Kept Name" ~slug:"ncpb-kept-slug"
                     ~visibility:bad ())
                ()
            in
            check_rerender
              ("invalid visibility " ^ String.escaped bad)
              ~status:422
              ~feedback:
                "Choose whether the community should be public or unlisted."
              ~slug:"ncpb-form-home" response body;
            Lwt.return_unit)
          [ "private"; "Public"; " public"; ""; "listed" ]
      in
      (* A rejected non-empty choice preselects neither radio: it never
         moves the publisher onto the more exposed option. Only the empty
         value — no choice at all — falls back to the documented default. *)
      let* token = mint_token "preselection" ~url ~cookie in
      let* _response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Kept Name" ~slug:"ncpb-kept-slug"
               ~visibility:"private" ())
          ()
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("no preselection: " ^ needle)
            false
            (Html_assert.contains body needle))
        [ "value='public' checked"; "value='unlisted' checked" ];
      Alcotest.(check bool)
        "the rejected value is not reflected as markup" false
        (Html_assert.contains body "value='private'");
      let* token = mint_token "empty preselection" ~url ~cookie in
      let* _response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Kept Name" ~slug:"ncpb-kept-slug" ~visibility:""
               ())
          ()
      in
      Alcotest.(check bool)
        "no choice falls back to public" true
        (Html_assert.contains body "value='public' checked");
      (* Escaping: preserved values are text, never markup. *)
      let* token = mint_token "escaping" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"<script>ncpb_x()</script>" ~slug:"Ncpb-Bad"
               ~description:"<b>ncpb</b>" ())
          ()
      in
      Alcotest.(check int) "422" 422 (status_of response);
      Alcotest.(check bool)
        "escaped name" true
        (Html_assert.contains body "&lt;script&gt;ncpb_x()&lt;/script&gt;");
      Alcotest.(check bool)
        "raw name absent" false
        (Html_assert.contains body "<script>ncpb_x()</script>");
      Alcotest.(check bool)
        "escaped description" true
        (Html_assert.contains body "&lt;b&gt;ncpb&lt;/b&gt;");
      Alcotest.(check bool)
        "raw description absent" false
        (Html_assert.contains body "<b>ncpb</b>");
      (* Nothing durable moved through any of them. *)
      check_unchanged "after every rejection" conn ~cid ~project ~rid before)

(* === community-slug conflict and retry === *)

let slug_conflict_case =
  db_case
    "POST publish: a taken final slug is a safe 409 re-render that keeps the \
     draft intact and stays retryable" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_cowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000004L ~project_slug:"ncpb-conf"
          ~slug:"ncpb-conf-home" ~name:"Ncpb Conf Home"
      in
      (* Three shapes of "taken": a legacy community, an unpublished
         network draft, and a published network community. *)
      let* _legacy =
        insert_community ~name:"Ncpb Legacy" ~network:false conn
          "ncpb-taken-legacy"
      in
      let* _draft =
        insert_community ~name:"Ncpb Other Draft" ~visibility:"private"
          ~indexable:false ~discoverable:false ~onboarding:"draft" conn
          "ncpb-taken-draft"
      in
      let* _published =
        insert_community ~name:"Ncpb Other Published" conn
          "ncpb-taken-published"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, _ =
        open_setup "setup" ~url ~slug:"ncpb-conf-home" owner
      in
      let target = post_target "ncpb-conf-home" in
      (* The three answers may differ only in the value the publisher
         themselves submitted, so that one value is masked before they are
         compared; everything else — copy, structure, status — must be
         byte-identical, or the 409 would be a conflicting-community
         oracle. *)
      let bodies = ref [] in
      let normalized ~slug body =
        replace_substring
          (Html_assert.without_csrf_inputs body)
          ~needle:slug ~replacement:"SUBMITTED-SLUG"
      in
      let attempt label ~token ~slug =
        let* response, body =
          do_post ~conn ~url ~cookie ~target ~token
            ~body_fields:
              (fields ~name:"Ncpb Conf Home" ~slug ~description:"Conflict body."
                 ~visibility:"unlisted" ())
            ()
        in
        check_rerender label ~status:409
          ~feedback:"That community address is already taken."
          ~slug:"ncpb-conf-home" response body;
        Alcotest.(check bool)
          (label ^ ": slug preserved")
          true
          (Html_assert.contains body (Printf.sprintf "value='%s'" slug));
        Alcotest.(check bool)
          (label ^ ": name preserved")
          true
          (Html_assert.contains body "value='Ncpb Conf Home'");
        Alcotest.(check bool)
          (label ^ ": description preserved")
          true
          (Html_assert.contains body ">Conflict body.</textarea>");
        Alcotest.(check bool)
          (label ^ ": choice preserved")
          true
          (Html_assert.contains body "value='unlisted' checked");
        (* Nothing about the conflicting community leaks. *)
        List.iter
          (fun needle ->
            Alcotest.(check bool)
              (label ^ ": no leak " ^ needle)
              false
              (Html_assert.contains body needle))
          [
            "Ncpb Legacy";
            "Ncpb Other Draft";
            "Ncpb Other Published";
            "onboarding_state";
            "is_network_community";
            "visibility =";
          ];
        check_no_credentials label response body;
        bodies := normalized ~slug body :: !bodies;
        Lwt.return_unit
      in
      let* () = attempt "legacy conflict" ~token ~slug:"ncpb-taken-legacy" in
      let* token = mint_token "draft" ~url ~cookie in
      let* () = attempt "draft conflict" ~token ~slug:"ncpb-taken-draft" in
      let* token = mint_token "published" ~url ~cookie in
      let* () =
        attempt "published conflict" ~token ~slug:"ncpb-taken-published"
      in
      (* A legacy community, an unpublished network draft, and a published
         network community are one indistinguishable safe answer. *)
      (match !bodies with
      | [] -> Alcotest.fail "no 409 bodies captured"
      | first :: rest ->
          List.iteri
            (fun i body ->
              Alcotest.(check bool)
                (Printf.sprintf
                   "409 body %d identical once the submitted slug is masked" i)
                true (String.equal first body))
            rest);
      (* The draft is byte-unchanged: old slug, private lifecycle,
         identity, relation, membership, moderation, shell. *)
      let* () =
        check_unchanged "after conflicts" conn ~cid ~project ~rid before
      in
      (* And a free slug still publishes. *)
      let* token = mint_token "retry" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Conf Home" ~slug:"ncpb-conf-final"
               ~description:"Conflict body." ~visibility:"unlisted" ())
          ()
      in
      check_clean_redirect "retry succeeds"
        (community_target "ncpb-conf-final")
        response body;
      check_published "retry" conn ~cid ~project ~rid ~before
        ~name:"Ncpb Conf Home" ~description:"Conflict body."
        ~slug:"ncpb-conf-final" ~old_slug:"ncpb-conf-home" ~indexable:false
        ~discoverable:false)

(* === durable authority variants === *)

let authority_case =
  db_case
    "POST publish: every durable publication authority works, and only durable \
     authority does" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_aowner" in
      let* second = insert_user conn "ncpb_asecond" in
      let* admin = insert_user conn "ncpb_aadmin" in
      let* both = insert_user conn "ncpb_aboth" in
      let* () = exec conn "durable admin" q_set_admin (admin, true) in
      let* () = exec conn "durable admin" q_set_admin (both, true) in
      let publish_as label ~ext_id ~project_slug ~slug ~final ~actor ~setup =
        let* project, cid, rid =
          make_draft conn ~user:owner ~ext_id ~project_slug ~slug
            ~name:"Ncpb Authority Home"
        in
        let* () = setup ~community:cid in
        let* before = snapshot label conn ~cid ~project ~rid in
        let* cookie, token, _ = open_setup label ~url ~slug actor in
        let* response, body =
          do_post ~conn ~url ~cookie ~target:(post_target slug) ~token
            ~body_fields:(fields ~name:"Ncpb Authority Home" ~slug:final ())
            ()
        in
        check_clean_redirect label (community_target final) response body;
        check_published label conn ~cid ~project ~rid ~before
          ~name:"Ncpb Authority Home" ~description:"<null>" ~slug:final
          ~old_slug:slug ~indexable:true ~discoverable:true
      in
      (* The creating top moderator. *)
      let* () =
        publish_as "creating top mod" ~ext_id:956000005L
          ~project_slug:"ncpb-auth-a" ~slug:"ncpb-auth-a-home"
          ~final:"ncpb-auth-a-live" ~actor:owner ~setup:(fun ~community:_ ->
            Lwt.return_unit)
      in
      (* A second current top moderator with no creation provenance. *)
      let* () =
        publish_as "second top mod" ~ext_id:956000006L
          ~project_slug:"ncpb-auth-b" ~slug:"ncpb-auth-b-home"
          ~final:"ncpb-auth-b-live" ~actor:second ~setup:(fun ~community ->
            exec conn "second top_mod" q_insert_moderator
              (second, community, "top_mod"))
      in
      (* A durable global administrator with no local role at all. *)
      let* () =
        publish_as "durable admin" ~ext_id:956000007L
          ~project_slug:"ncpb-auth-c" ~slug:"ncpb-auth-c-home"
          ~final:"ncpb-auth-c-live" ~actor:admin ~setup:(fun ~community:_ ->
            Lwt.return_unit)
      in
      (* An actor holding both authorities at once. *)
      publish_as "top mod and durable admin" ~ext_id:956000008L
        ~project_slug:"ncpb-auth-d" ~slug:"ncpb-auth-d-home"
        ~final:"ncpb-auth-d-live" ~actor:both ~setup:(fun ~community ->
          exec conn "both top_mod" q_insert_moderator
            (both, community, "top_mod")))

(* === unavailable and unauthorized === *)

let unavailable_case =
  db_case
    "POST publish: every unavailable or unauthorized state is one \
     byte-identical generic 404" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_uowner" in
      let* member = insert_user conn "ncpb_umember" in
      let* moddy = insert_user conn "ncpb_umod" in
      let* legacy_mod = insert_user conn "ncpb_ulegacymod" in
      let* foreign_mod = insert_user conn "ncpb_uforeign" in
      let* demoted = insert_user conn "ncpb_udemoted" in
      let* removed = insert_user conn "ncpb_uremoved" in
      let* stranger = insert_user conn "ncpb_ustranger" in
      let* _project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000009L ~project_slug:"ncpb-un"
          ~slug:"ncpb-un-home"
      in
      (* Everyone below opens their own session; nobody but a durable
         top_mod or admin can even see the setup page, so tokens come from
         /mint rather than the form. *)
      let bodies = ref [] in
      let expect_404 label ?(admin_session = false) ~user ~slug () =
        as_user ~admin_session user;
        let* cookie, token = open_session label ~url user in
        let* response, body =
          do_post ~conn ~url ~cookie ~target:(post_target slug) ~token
            ~body_fields:(fields ~slug:"ncpb-un-final" ())
            ()
        in
        check_generic_404 label response body;
        check_no_credentials label response body;
        bodies := body :: !bodies;
        Lwt.return_unit
      in
      let* () = exec conn "member" q_insert_member (member, cid) in
      let* () =
        expect_404 "ordinary member" ~user:member ~slug:"ncpb-un-home" ()
      in
      let* () = exec conn "mod" q_insert_moderator (moddy, cid, "mod") in
      let* () = expect_404 "mod" ~user:moddy ~slug:"ncpb-un-home" () in
      let* () =
        exec conn "legacy_mod" q_insert_moderator (legacy_mod, cid, "legacy_mod")
      in
      let* () =
        expect_404 "legacy_mod" ~user:legacy_mod ~slug:"ncpb-un-home" ()
      in
      (* A top moderator of a different community. *)
      let* other = insert_community ~name:"Ncpb Other" conn "ncpb-un-other" in
      let* () =
        exec conn "foreign top_mod" q_insert_moderator
          (foreign_mod, other, "top_mod")
      in
      let* () =
        expect_404 "moderator of another community" ~user:foreign_mod
          ~slug:"ncpb-un-home" ()
      in
      (* A downgraded top moderator, and a removed one. *)
      let* () =
        exec conn "downgrade seed" q_insert_moderator (demoted, cid, "top_mod")
      in
      let* () =
        exec conn "downgrade" q_set_moderator_role (demoted, cid, "mod")
      in
      let* () =
        expect_404 "downgraded top moderator" ~user:demoted ~slug:"ncpb-un-home"
          ()
      in
      let* () =
        exec conn "removal seed" q_insert_moderator (removed, cid, "top_mod")
      in
      let* () = exec conn "remove" q_remove_moderator (removed, cid) in
      let* () =
        expect_404 "removed top moderator" ~user:removed ~slug:"ncpb-un-home" ()
      in
      (* A session-shaped admin with no durable users.is_admin row. *)
      let* () =
        expect_404 "session-shaped admin" ~admin_session:true ~user:stranger
          ~slug:"ncpb-un-home" ()
      in
      (* A nonexistent community, and a legacy one, even to its own top
         moderator. *)
      let* () =
        expect_404 "missing community" ~user:owner ~slug:"ncpb-un-nothing" ()
      in
      let* legacy =
        insert_community ~name:"Ncpb Legacy" ~network:false conn
          "ncpb-un-legacy"
      in
      let* () =
        exec conn "legacy top_mod" q_insert_moderator (owner, legacy, "top_mod")
      in
      let* () =
        expect_404 "legacy community" ~user:owner ~slug:"ncpb-un-legacy" ()
      in
      (match !bodies with
      | [] -> Alcotest.fail "no 404 bodies captured"
      | first :: rest ->
          List.iteri
            (fun i body ->
              Alcotest.(check bool)
                (Printf.sprintf "404 body %d identical" i)
                true (String.equal first body))
            rest);
      (* Nothing published, and the draft is otherwise untouched apart from
         the fixtures this case installed. *)
      let* n = find conn "no final slug" q_count_by_slug "ncpb-un-final" in
      Alcotest.(check int) "nothing published" 0 n;
      let* state = find conn "state" q_community_state cid in
      Alcotest.(check bool)
        "still a private draft" true
        (Html_assert.contains state "|private|draft|true|false|false");
      (* An already-published community is the same generic 404 as every
         state above — the replay suite drives that end to end, and here it
         is only the durable precondition that differs. *)
      (* A draft whose accepted home relation is already gone at candidate
         resolution is deliberately NOT this 404: the provisioning store
         writes the community and its accepted relation atomically and the
         draft-protection rule forbids detaching it before publication, so
         the store classifies that shape as durable corruption. Reaching it
         at all needs direct SQL. Draft_unavailable still covers the real
         race — a removal that commits between candidate resolution and the
         lock. *)
      let* () = exec conn "remove relation" q_mark_removed rid in
      let* cookie, token = open_session "relation removed" ~url owner in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-un-home")
          ~token
          ~body_fields:(fields ~slug:"ncpb-un-final" ())
          ()
      in
      check_generic_500 "home relation removed first" response body;
      check_no_credentials "home relation removed first" response body;
      let* n = find conn "still nothing" q_count_by_slug "ncpb-un-final" in
      Alcotest.(check int) "still nothing published" 0 n;
      Lwt.return_unit)

(* === project verification drift === *)

let verification_case =
  db_case
    "POST publish: publication survives every known project verification state"
    (fun ~url conn ->
      let* owner = insert_user conn "ncpb_vowner" in
      let publish_with label ~ext_id ~project_slug ~slug ~final ~status
          ~visibility ~indexable =
        let* project, cid, rid =
          make_draft conn ~user:owner ~ext_id ~project_slug ~slug
            ~name:"Ncpb Verification Home"
        in
        let* cookie, token, _ = open_setup label ~url ~slug owner in
        (* The drift lands between the GET and the POST. *)
        let* () = exec conn label q_set_verification (project, status) in
        let* before = snapshot label conn ~cid ~project ~rid in
        let* response, body =
          do_post ~conn ~url ~cookie ~target:(post_target slug) ~token
            ~body_fields:
              (fields ~name:"Ncpb Verification Home" ~slug:final ~visibility ())
            ()
        in
        check_clean_redirect label (community_target final) response body;
        let* () =
          check_published label conn ~cid ~project ~rid ~before
            ~name:"Ncpb Verification Home" ~description:"<null>" ~slug:final
            ~old_slug:slug ~indexable ~discoverable:indexable
        in
        (* Publication never touches verification. *)
        let* after =
          find conn (label ^ ": verification") q_verification project
        in
        Alcotest.(check string)
          (label ^ ": verification unchanged")
          status after;
        Lwt.return_unit
      in
      let* () =
        publish_with "verified" ~ext_id:956000010L ~project_slug:"ncpb-ver-a"
          ~slug:"ncpb-ver-a-home" ~final:"ncpb-ver-a-live" ~status:"verified"
          ~visibility:"public" ~indexable:true
      in
      let* () =
        publish_with "stale" ~ext_id:956000011L ~project_slug:"ncpb-ver-b"
          ~slug:"ncpb-ver-b-home" ~final:"ncpb-ver-b-live" ~status:"stale"
          ~visibility:"unlisted" ~indexable:false
      in
      publish_with "revoked" ~ext_id:956000012L ~project_slug:"ncpb-ver-c"
        ~slug:"ncpb-ver-c-home" ~final:"ncpb-ver-c-live" ~status:"revoked"
        ~visibility:"public" ~indexable:true)

(* === replay === *)

let replay_case =
  db_case
    "POST publish: replaying a succeeded submission publishes nothing more and \
     is the generic 404" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_rowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000013L
          ~project_slug:"ncpb-replay" ~slug:"ncpb-replay-home"
          ~name:"Ncpb Replay Home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, _ =
        open_setup "setup" ~url ~slug:"ncpb-replay-home" owner
      in
      let target = post_target "ncpb-replay-home" in
      let body_fields =
        fields ~name:"Ncpb Replay Home" ~slug:"ncpb-replay-live" ()
      in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token ~body_fields ()
      in
      check_clean_redirect "first"
        (community_target "ncpb-replay-live")
        response body;
      (* The very same token replayed. Dream's CSRF tokens are stateless and
         session-bound rather than one-shot, so the token still verifies:
         the store observing a non-draft community, not token consumption,
         is what stops the second publication. *)
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token ~body_fields ()
      in
      check_generic_404 "same-token replay" response body;
      Alcotest.(check (option string))
        "no redirect" None
        (Dream.header response "Location");
      Alcotest.(check bool)
        "no final slug leaks" false
        (Html_assert.contains body "ncpb-replay-live");
      (* A freshly minted token replays to the same place. *)
      let* fresh = mint_token "fresh" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token:fresh ~body_fields ()
      in
      check_generic_404 "fresh-token replay" response body;
      (* Replaying against the new slug is the same answer: the community
         is no longer a draft. *)
      let* fresh = mint_token "new slug" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-replay-live")
          ~token:fresh ~body_fields ()
      in
      check_generic_404 "replay under the final slug" response body;
      (* A genuinely unusable token no longer dead-ends: it is refused
         without opening the store and answered by re-rendering the
         owner-authorized page. This community is no longer a draft, so
         that re-render is exactly the generic 404 its own GET gives. *)
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token:"ncpb-not-a-token"
          ~body_fields ()
      in
      check_generic_404 "bad token" response body;
      (* Exactly one publication, and no duplicate or partial state. *)
      let* n = find conn "final" q_count_by_slug "ncpb-replay-live" in
      Alcotest.(check int) "exactly one community owns the final slug" 1 n;
      let* old_n = find conn "old" q_count_by_slug "ncpb-replay-home" in
      Alcotest.(check int) "the old slug stays released" 0 old_n;
      check_published "after replays" conn ~cid ~project ~rid ~before
        ~name:"Ncpb Replay Home" ~description:"<null>" ~slug:"ncpb-replay-live"
        ~old_slug:"ncpb-replay-home" ~indexable:true ~discoverable:true)

(* === durable failures === *)

let inconsistent_case =
  db_case
    "POST publish: durable draft corruption is one generic non-cacheable 500 \
     with no partial publication" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_icowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000014L ~project_slug:"ncpb-ic"
          ~slug:"ncpb-ic-home"
      in
      (* The setup page is opened while the draft is still sound; the shell
         is corrupted underneath it. *)
      let* cookie, token, _ =
        open_setup "setup" ~url ~slug:"ncpb-ic-home" owner
      in
      let* () = exec conn "drop sections" q_delete_sections cid in
      let* before = snapshot "corrupt" conn ~cid ~project ~rid in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-ic-home")
          ~token
          ~body_fields:(fields ~slug:"ncpb-ic-live" ())
          ()
      in
      check_generic_500 "corrupt draft" response body;
      check_no_credentials "corrupt draft" response body;
      let* n = find conn "no publication" q_count_by_slug "ncpb-ic-live" in
      Alcotest.(check int) "nothing published" 0 n;
      check_unchanged "rolled back" conn ~cid ~project ~rid before)

let storage_case =
  db_case
    "POST publish: a real database failure is one generic non-cacheable 500 on \
     both the store and the re-render path" (fun ~url _conn ->
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=ncpb_void"))
      in
      (* A dedicated pool for this one case: the shared pipeline must keep
         talking to the real schema. *)
      let saved = !shared_pipeline in
      shared_pipeline := None;
      (* The production limiter fronts this route and fails closed on a
         pool it cannot query, refusing before the handler runs; the
         handler's own storage-error collapse is exercised behind an
         allowing limiter — the only way a request can reach it here. *)
      let limited_pipe = build_pipeline ~url:poisoned in
      let poison_pipe =
        build_pipeline_with ~limited:allowing_limiter ~url:poisoned
      in
      shared_pipeline := saved;
      as_user 424244;
      let* response, token =
        do_get ~pipeline:limited_pipe ~url ~target:"/mint" ()
      in
      let cookie = Http_fixture.session_cookie "poisoned limiter" response in
      let* response, _ =
        do_post ~pipeline:limited_pipe ~url ~cookie
          ~target:(post_target "ncpb-anything")
          ~token
          ~body_fields:(fields ~slug:"ncpb-void-live" ())
          ()
      in
      Alcotest.(check int) "limiter refuses first" 503 (status_of response);
      let* response, token =
        do_get ~pipeline:poison_pipe ~url ~target:"/mint" ()
      in
      Alcotest.(check int) "mint 200" 200 (status_of response);
      let cookie = Http_fixture.session_cookie "poisoned" response in
      let* response, body =
        do_post ~pipeline:poison_pipe ~url ~cookie
          ~target:(post_target "ncpb-anything")
          ~token
          ~body_fields:(fields ~slug:"ncpb-void-live" ())
          ()
      in
      check_generic_500 "poisoned store call" response body;
      let* response, body =
        do_post ~pipeline:poison_pipe ~url ~cookie
          ~target:(post_target "ncpb-anything")
          ~token
          ~body_fields:(fields ~slug:"Ncpb-Bad" ())
          ()
      in
      check_generic_500 "poisoned re-render" response body;
      Lwt.return_unit)

(* === concurrency through HTTP === *)

let same_draft_race_case =
  db_case
    "POST publish: two concurrent submissions for one draft publish it exactly \
     once; the loser is the generic 404" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_p1owner" in
      let* second = insert_user conn "ncpb_p1second" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000015L ~project_slug:"ncpb-race"
          ~slug:"ncpb-race-home" ~name:"Ncpb Race Home"
      in
      let* () =
        exec conn "second top_mod" q_insert_moderator (second, cid, "top_mod")
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie_a, token_a, _ =
        open_setup "publisher a" ~url ~slug:"ncpb-race-home" owner
      in
      let* cookie_b, token_b, _ =
        open_setup "publisher b" ~url ~slug:"ncpb-race-home" second
      in
      let* () = clear_limits conn in
      let target = post_target "ncpb-race-home" in
      (* Two independent sessions and tokens genuinely in flight at once
         over two pooled connections; the community lock inside the store
         serializes them without any sleep. *)
      let* (response_a, body_a), (response_b, body_b) =
        Lwt.both
          (do_post ~url ~cookie:cookie_a ~target ~token:token_a
             ~body_fields:(fields ~name:"Ncpb Race Home" ~slug:"ncpb-race-a" ())
             ())
          (do_post ~url ~cookie:cookie_b ~target ~token:token_b
             ~body_fields:(fields ~name:"Ncpb Race Home" ~slug:"ncpb-race-b" ())
             ())
      in
      let a_won = status_of response_a = 303 in
      let win, win_body, lose, lose_body, winning_slug, losing_slug =
        if a_won then
          (response_a, body_a, response_b, body_b, "ncpb-race-a", "ncpb-race-b")
        else
          (response_b, body_b, response_a, body_a, "ncpb-race-b", "ncpb-race-a")
      in
      check_clean_redirect "winner" (community_target winning_slug) win win_body;
      check_generic_404 "loser" lose lose_body;
      let* winners = find conn "winner row" q_count_by_slug winning_slug in
      Alcotest.(check int) "the winning slug exists" 1 winners;
      let* losers = find conn "loser row" q_count_by_slug losing_slug in
      Alcotest.(check int) "the losing slug was never taken" 0 losers;
      check_published "one publication only" conn ~cid ~project ~rid ~before
        ~name:"Ncpb Race Home" ~description:"<null>" ~slug:winning_slug
        ~old_slug:"ncpb-race-home" ~indexable:true ~discoverable:true)

let same_slug_race_case =
  db_case
    "POST publish: two drafts racing for one final slug leave one winner and a \
     clean, retryable 409 loser" (fun ~url conn ->
      let* owner_a = insert_user conn "ncpb_s1owner" in
      let* owner_b = insert_user conn "ncpb_s2owner" in
      let* project_a, cid_a, rid_a =
        make_draft conn ~user:owner_a ~ext_id:956000016L
          ~project_slug:"ncpb-slug-a" ~slug:"ncpb-slug-a-home"
          ~name:"Ncpb Slug A"
      in
      let* project_b, cid_b, rid_b =
        make_draft conn ~user:owner_b ~ext_id:956000017L
          ~project_slug:"ncpb-slug-b" ~slug:"ncpb-slug-b-home"
          ~name:"Ncpb Slug B"
      in
      let* before_a =
        snapshot "before a" conn ~cid:cid_a ~project:project_a ~rid:rid_a
      in
      let* before_b =
        snapshot "before b" conn ~cid:cid_b ~project:project_b ~rid:rid_b
      in
      let* cookie_a, token_a, _ =
        open_setup "draft a" ~url ~slug:"ncpb-slug-a-home" owner_a
      in
      let* cookie_b, token_b, _ =
        open_setup "draft b" ~url ~slug:"ncpb-slug-b-home" owner_b
      in
      let* () = clear_limits conn in
      let body_fields =
        fields ~name:"Ncpb Shared Home" ~slug:"ncpb-shared-live" ()
      in
      let* (response_a, body_a), (response_b, body_b) =
        Lwt.both
          (do_post ~url ~cookie:cookie_a
             ~target:(post_target "ncpb-slug-a-home")
             ~token:token_a ~body_fields ())
          (do_post ~url ~cookie:cookie_b
             ~target:(post_target "ncpb-slug-b-home")
             ~token:token_b ~body_fields ())
      in
      let a_won = status_of response_a = 303 in
      let ( winner,
            winner_body,
            loser,
            loser_body,
            loser_slug,
            loser_cookie,
            loser_cid,
            loser_project,
            loser_rid,
            loser_before ) =
        if a_won then
          ( response_a,
            body_a,
            response_b,
            body_b,
            "ncpb-slug-b-home",
            cookie_b,
            cid_b,
            project_b,
            rid_b,
            before_b )
        else
          ( response_b,
            body_b,
            response_a,
            body_a,
            "ncpb-slug-a-home",
            cookie_a,
            cid_a,
            project_a,
            rid_a,
            before_a )
      in
      check_clean_redirect "winner"
        (community_target "ncpb-shared-live")
        winner winner_body;
      check_rerender "loser" ~status:409
        ~feedback:"That community address is already taken." ~slug:loser_slug
        loser loser_body;
      Alcotest.(check bool)
        "loser keeps its submission" true
        (Html_assert.contains loser_body "value='ncpb-shared-live'");
      let* n = find conn "shared" q_count_by_slug "ncpb-shared-live" in
      Alcotest.(check int) "exactly one community owns the slug" 1 n;
      (* The loser is byte-unchanged and still publishable. *)
      let* () =
        check_unchanged "loser unchanged" conn ~cid:loser_cid
          ~project:loser_project ~rid:loser_rid loser_before
      in
      let* token = mint_token "loser retry" ~url ~cookie:loser_cookie in
      let* response, body =
        do_post ~conn ~url ~cookie:loser_cookie ~target:(post_target loser_slug)
          ~token
          ~body_fields:
            (fields ~name:"Ncpb Shared Home" ~slug:"ncpb-shared-other" ())
          ()
      in
      check_clean_redirect "loser retries successfully"
        (community_target "ncpb-shared-other")
        response body;
      Lwt.return_unit)

(* === draft protection and post-publication removal === *)

let removal_case =
  db_case
    "POST publish: home removal is refused while the community is a draft and \
     works once it is published" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_remowner" in
      (* Two drafts, so each existing removal route is exercised on its
         own. *)
      let* project_a, cid_a, rid_a =
        make_draft conn ~user:owner ~ext_id:956000018L
          ~project_slug:"ncpb-rem-a" ~slug:"ncpb-rem-a-home" ~name:"Ncpb Rem A"
      in
      let* _project_b, _cid_b, rid_b =
        make_draft conn ~user:owner ~ext_id:956000019L
          ~project_slug:"ncpb-rem-b" ~slug:"ncpb-rem-b-home" ~name:"Ncpb Rem B"
      in
      let* _post = find conn "post" q_insert_post (cid_a, owner) in
      let* before_a =
        snapshot "before a" conn ~cid:cid_a ~project:project_a ~rid:rid_a
      in
      let* cookie, token, _ =
        open_setup "setup a" ~url ~slug:"ncpb-rem-a-home" owner
      in
      (* 1. Forged removal while the community is still a draft: the
         existing safe redirect, and no mutation on either surface. *)
      let* response, body =
        do_bare_post ~conn ~url ~cookie
          ~target:
            (project_side_remove ~project:"ncpb-rem-a"
               ~community:"ncpb-rem-a-home")
          ~token ()
      in
      check_clean_redirect "project-side refusal"
        (request_home_target "ncpb-rem-a")
        response body;
      let* token = mint_token "community side" ~url ~cookie in
      let* response, body =
        do_bare_post ~conn ~url ~cookie
          ~target:
            (community_side_remove ~community:"ncpb-rem-a-home"
               ~project:"ncpb-rem-a")
          ~token ()
      in
      check_clean_redirect "community-side refusal"
        (settings_projects_target "ncpb-rem-a-home")
        response body;
      let* status = find conn "relation" q_relation_status rid_a in
      Alcotest.(check string) "relation still accepted" "accepted" status;
      let* () =
        check_unchanged "draft untouched by refused removals" conn ~cid:cid_a
          ~project:project_a ~rid:rid_a before_a
      in
      (* 2. Setup remains publishable, and publishing succeeds. *)
      let* token = mint_token "publish a" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-rem-a-home")
          ~token
          ~body_fields:(fields ~name:"Ncpb Rem A" ~slug:"ncpb-rem-a-live" ())
          ()
      in
      check_clean_redirect "published a"
        (community_target "ncpb-rem-a-live")
        response body;
      (* 3. Removal after publication succeeds through the project-side
         route, and the published community and its content remain. *)
      let* token = mint_token "remove a" ~url ~cookie in
      let* response, body =
        do_bare_post ~conn ~url ~cookie
          ~target:
            (project_side_remove ~project:"ncpb-rem-a"
               ~community:"ncpb-rem-a-live")
          ~token ()
      in
      check_clean_redirect "project-side removal"
        (request_home_target "ncpb-rem-a")
        response body;
      let* status = find conn "relation after" q_relation_status rid_a in
      Alcotest.(check string) "relation removed" "removed" status;
      let* state = find conn "community after" q_community_state cid_a in
      Alcotest.(check bool)
        "the community stays published and public" true
        (Html_assert.contains state "|public|published|true|true|true");
      let* posts = find conn "posts" q_count_posts cid_a in
      Alcotest.(check int) "content survives removal" 1 posts;
      (* 4. The community-side route behaves the same on the second
         draft. *)
      let* cookie_b, token_b, _ =
        open_setup "setup b" ~url ~slug:"ncpb-rem-b-home" owner
      in
      let* response, body =
        do_post ~conn ~url ~cookie:cookie_b
          ~target:(post_target "ncpb-rem-b-home")
          ~token:token_b
          ~body_fields:(fields ~name:"Ncpb Rem B" ~slug:"ncpb-rem-b-live" ())
          ()
      in
      check_clean_redirect "published b"
        (community_target "ncpb-rem-b-live")
        response body;
      let* token_b = mint_token "remove b" ~url ~cookie:cookie_b in
      let* response, body =
        do_bare_post ~conn ~url ~cookie:cookie_b
          ~target:
            (community_side_remove ~community:"ncpb-rem-b-live"
               ~project:"ncpb-rem-b")
          ~token:token_b ()
      in
      check_clean_redirect "community-side removal"
        (settings_projects_target "ncpb-rem-b-live")
        response body;
      let* status = find conn "relation b" q_relation_status rid_b in
      Alcotest.(check string) "relation b removed" "removed" status;
      Lwt.return_unit)

(* === the mutation rate limiter really wraps this route === *)

let rate_limit_case =
  db_case
    "POST publish: the route carries the shared authenticated-mutation rate \
     limit" (fun ~url conn ->
      let* () = clear_limits conn in
      let* cookie, token = open_session "rate limit" ~url 424245 in
      let target = post_target "ncpb-ratelimit" in
      (* The community does not exist, so every allowed attempt is the
         generic 404 and nothing durable is ever published. *)
      let rec drive n =
        if n > 12 then
          Alcotest.fail "the limiter never blocked within 12 attempts"
        else
          let* response, body =
            do_post ~url ~cookie ~target ~token
              ~body_fields:(fields ~slug:"ncpb-ratelimit-live" ())
              ()
          in
          if Html_assert.contains body "Too Many Attempts" then
            Lwt.return (n, response)
          else (
            Alcotest.(check int)
              (Printf.sprintf "attempt %d is the generic 404" n)
              404 (status_of response);
            drive (n + 1))
      in
      let* attempts, response = drive 1 in
      Alcotest.(check bool)
        "blocked only after several attempts" true (attempts > 1);
      Alcotest.(check int) "blocked page status" 200 (status_of response);
      clear_limits conn)

(* === privacy sweep === *)

let privacy_case =
  db_case
    "POST publish: no credential-shaped fixture reaches any response, \
     redirect, cookie, or the published community" (fun ~url conn ->
      let* owner = insert_user conn "ncpb_privowner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:956000001L ~project_slug:"ncpb-priv"
          ~slug:"ncpb-priv-home" ~name:"Ncpb Priv Home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* cookie, token, page =
        open_setup "setup" ~url ~slug:"ncpb-priv-home" owner
      in
      check_body_no_credentials "setup page" page;
      let target = post_target "ncpb-priv-home" in
      (* A rejected submission carrying every marker in fields the parser
         does not know. *)
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (List.map
               (fun (_, marker) -> ("ncpb_" ^ marker, marker))
               credential_markers
            @ fields ())
          ()
      in
      Alcotest.(check int) "422" 422 (status_of response);
      check_no_credentials "rejected submission" response body;
      (* A 409 body: only the intentionally submitted safe identity. *)
      let* _taken =
        insert_community ~name:"Ncpb Taken" conn "ncpb-priv-taken"
      in
      let* token = mint_token "conflict" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Priv Home" ~slug:"ncpb-priv-taken" ())
          ()
      in
      Alcotest.(check int) "409" 409 (status_of response);
      check_no_credentials "conflict" response body;
      (* A generic 404 body. *)
      let* token = mint_token "missing" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie
          ~target:(post_target "ncpb-priv-nothing")
          ~token
          ~body_fields:(fields ~slug:"ncpb-priv-live" ())
          ()
      in
      check_generic_404 "missing community" response body;
      check_no_credentials "missing community" response body;
      (* The success redirect and the published community itself. *)
      let* token = mint_token "success" ~url ~cookie in
      let* response, body =
        do_post ~conn ~url ~cookie ~target ~token
          ~body_fields:
            (fields ~name:"Ncpb Priv Home" ~slug:"ncpb-priv-live"
               ~description:"Safe body." ())
          ()
      in
      check_clean_redirect "success"
        (community_target "ncpb-priv-live")
        response body;
      check_no_credentials "success redirect" response body;
      let* response, page_body =
        do_get ~url ~cookie ~target:(community_target "ncpb-priv-live") ()
      in
      check_no_credentials "published community" response page_body;
      (* The intentionally supplied public identity is what survives. *)
      Alcotest.(check bool)
        "the supplied name is public" true
        (Html_assert.contains page_body "Ncpb Priv Home");
      check_published "privacy" conn ~cid ~project ~rid ~before
        ~name:"Ncpb Priv Home" ~description:"Safe body." ~slug:"ncpb-priv-live"
        ~old_slug:"ncpb-priv-home" ~indexable:true ~discoverable:true)

let db_suite =
  [
    public_success_case;
    unlisted_success_case;
    parser_rejection_case;
    slug_conflict_case;
    authority_case;
    unavailable_case;
    verification_case;
    replay_case;
    inconsistent_case;
    storage_case;
    same_draft_race_case;
    same_slug_race_case;
    removal_case;
    rate_limit_case;
    privacy_case;
  ]

let suites =
  (* POST /c/:slug/publish: the exact registered route (once, no alias,
       no method variant) beside the setup GET, the DB-free
       rollout/authentication gates, the configuration/origin/CSRF
       sequence, and the database-gated end-to-end journey — Public and
       Unlisted PRG into the final public home, 422 parser re-renders with
       safe value preservation, the safe 409 slug conflict and its retry,
       every durable authority, byte-identical generic 404s, verification
       drift, replay, durable failures, HTTP-level concurrency, draft
       protection and post-publication removal, the shared mutation rate
       limit, and the privacy sweep. *)
  [
    ("network_community_publication_post_route", route_cases);
    ("network_community_publication_post_gates", gate_cases);
    ("network_community_publication_post_config_origin", config_origin_cases);
    ("network_community_publication_post_csrf", csrf_cases);
    ("network_community_publication_post_db", db_suite);
  ]
