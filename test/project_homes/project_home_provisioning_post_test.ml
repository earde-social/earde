module Ob = Earde.Project_onboarding
module Pi = Earde.Project_identity

(* === Dedicated-home provisioning POST integration
   (Project_home_provisioning_handlers) ===
   POST /projects/:slug/community-home end to end: the exact route shape and
   its coexistence with the sibling GET, the shared rollout/authentication
   gates (every rejection precedes any route read, configuration load, origin
   inspection, form parse, or SQL), the configuration/origin/CSRF sequence,
   the four parser rejections with their safe value preservation, and the
   database-gated provisioning flow — the PRG into the new private settings
   draft, slug conflict, active-home and replay behaviour, authorization
   drift, durable failure, real concurrency, destination authorization, the
   mutation rate limit, and the privacy sweep. Reserved
   external-installation-id range 951000001..951000999 (hence account ids
   951100001..951100999, which also scope the permanent-project cleanup),
   phvv_% usernames, and phvv-% community slugs so no suite shares fixtures.
   Verified projects come only through the real draft/selection/finalization
   chain; competing relations come only through the real request/review
   stores. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Hd = Earde.Project_home_provisioning_handlers

module Pgs = Earde.Project_home_provisioning_pages

module Rvs = Earde.Project_home_review_store

let case = Case.quick

let counting_loader = Http_fixture.counting_loader

let ok_loader = Http_fixture.ok_loader

let status_of = Http_fixture.status_of

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let make_project = Home_provisioning_fixture.make_project

let add_steward = Home_provisioning_fixture.add_steward

let add_top_mod = Home_provisioning_fixture.add_top_mod

let request_pending = Home_provisioning_fixture.request_pending

let review = Home_provisioning_fixture.review

(* The one POST pattern this slice registers, and the GET it completes. *)
let get_pattern = "/projects/:slug/community-home/new"

let post_pattern = "/projects/:slug/community-home"

let get_target slug = Printf.sprintf "/projects/%s/community-home/new" slug

let post_target slug = Printf.sprintf "/projects/%s/community-home" slug

let request_home_target slug = Printf.sprintf "/projects/%s/request-home" slug

let settings_target slug = Printf.sprintf "/c/%s/settings" slug

let community_target slug = Printf.sprintf "/c/%s" slug

let make_get ~mode = Hd.make_project_home_provisioning_page_handler ~mode

let make ~mode ~load_config =
  Hd.make_project_home_provisioning_handler ~mode ~load_config

(* Exactly the three application fields, in the page's own order. *)
let fields ?(name = "Phvv Community Home") ?(slug = "phvv-home")
    ?(description = "") () =
  [ ("community_name", name);
    ("community_slug", slug);
    ("community_description", description)
  ]

(* ============ DB-free: the route shape ============ *)

(* The real router with both patterns and nothing else: the POST target is
   dispatched exactly once, and no alias, sub-path, or method variant
   exists. Mode Off makes each dispatch observable as the clean /bring
   redirect, with no configuration read and no SQL. *)
let two_route_router () =
  Dream.router
    [ Dream.get get_pattern (fun req -> make_get ~mode:Ob.Off req);
      Dream.post post_pattern (fun req ->
          make ~mode:Ob.Off ~load_config:(fun () -> ok_loader ()) req)
    ]

let route_run ~method_ ~target =
  Http_fixture.gate_response "route"
    (Http_fixture.gate_run ~method_ ~target (two_route_router ()))

let route_cases =
  [ case "provisioning route: the registered POST pattern is exactly the \
          action the creation page emits" (fun () ->
        let html =
          Pgs.project_home_provisioning_page
            ~project:
              { Pgs.name = "Phvv Project";
                slug = "phvv-alpha";
                description = None;
                kind = Pi.Project;
                namespace_login = "phvv-owner"
              }
            ~values:
              { Pgs.community_name = "";
                community_slug = "";
                community_description = ""
              }
            ~feedback:None ()
        in
        Alcotest.(check bool) "the page posts to the registered route" true
          (Html_assert.contains html
             (Printf.sprintf "action='%s'" (post_target "phvv-alpha")));
        (* Scoped to the feature fragment: the shared shell owns its own
           chrome forms. *)
        Alcotest.(check int) "exactly one form" 1
          (Html_assert.occurrences (Html_assert.panel_fragment html) "<form"))
  ; case "provisioning route: the POST target resolves once, with no alias \
          and no extra GET" (fun () ->
        Http_fixture.check_clean_redirect "post dispatches" "/bring"
          (route_run ~method_:`POST ~target:(post_target "phvv-alpha"));
        Http_fixture.check_clean_redirect "get dispatches" "/bring"
          (route_run ~method_:`GET ~target:(get_target "phvv-alpha"));
        List.iter
          (fun (label, method_, target) ->
            Alcotest.(check int)
              (label ^ ": unrouted")
              404
              (status_of (route_run ~method_ ~target)))
          [ ("GET on the POST path", `GET, post_target "phvv-alpha");
            ("POST on the GET path", `POST, get_target "phvv-alpha");
            ("trailing slash", `POST, post_target "phvv-alpha" ^ "/");
            ("sub-path", `POST, post_target "phvv-alpha" ^ "/create");
            ("plural alias", `POST, "/projects/phvv-alpha/community-homes");
            ("legacy alias", `POST, "/projects/phvv-alpha/home")
          ])
  ]

(* ======== DB-free: shared rollout and authentication gates ======== *)

let post_run ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST
    ~target:(post_target "phvv-any")
    (make ~mode ~load_config)

let routed_post ?session ?(headers = []) ?(mode = Ob.Public) ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST
    ~target:(post_target "phvv-any")
    (Dream.router
       [ Dream.post post_pattern (fun req -> make ~mode ~load_config req) ])

let gate_cases =
  [ case "POST provisioning off: clean /bring redirect before any route \
          read, configuration load, or SQL" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "off" "/bring"
          (Http_fixture.gate_response "off"
             (post_run ~session:Http_fixture.admin_session ~mode:Ob.Off
                ~load_config:loader ()));
        Http_fixture.check_clean_redirect "off routed" "/bring"
          (Http_fixture.gate_response "off routed"
             (routed_post ~session:Http_fixture.admin_session ~mode:Ob.Off
                ~load_config:loader ()));
        Alcotest.(check int) "loader never called" 0 !calls)
  ; case "POST provisioning: anonymous and malformed sessions to /login, \
          loader untouched" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "anonymous" "/login"
          (Http_fixture.gate_response "anonymous" (routed_post ~load_config:loader ()));
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
        Alcotest.(check int) "loader never called" 0 !calls)
  ; case "POST provisioning admins mode: a non-admin is redirected before \
          the configuration load" (fun () ->
        let loader, calls = counting_loader (ok_loader ()) in
        Http_fixture.check_clean_redirect "non-admin" "/bring"
          (Http_fixture.gate_response "non-admin"
             (routed_post ~session:Http_fixture.logged_in ~mode:Ob.Admins
                ~load_config:loader ()));
        Alcotest.(check int) "loader never called" 0 !calls)
  ; case "POST provisioning: authorized sessions continue past the gates, \
          and an unrouted request is the generic 404 before configuration"
      (fun () ->
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
          Alcotest.(check int) (label ^ ": defensive 404") 404
            (status_of response);
          Alcotest.(check (option string))
            (label ^ ": no-store")
            (Some "no-store")
            (Dream.header response "Cache-Control");
          Alcotest.(check int) (label ^ ": loader untouched") 0 !calls
        in
        continues "admin in admins mode" Http_fixture.admin_session Ob.Admins;
        continues "user in public mode" Http_fixture.logged_in Ob.Public;
        continues "admin in public mode" Http_fixture.admin_session Ob.Public;
        (* Routed and authorized with a good configuration and origin: the
           next rejection is the missing content type, proving the whole
           prefix ran and still no SQL happened. *)
        Alcotest.(check int) "routed: 400 content type" 400
          (status_of
             (Http_fixture.gate_response "routed"
                (routed_post ~session:Http_fixture.logged_in
                   ~headers:[ ("Origin", "https://earde.com") ]
                   ~load_config:(fun () -> ok_loader ())
                   ()))))
  ]

(* ============ DB-free: configuration and origin ============ *)

let config_origin_cases =
  [ case "POST provisioning: a configuration failure is a generic 503 that \
          names nothing" (fun () ->
        let loader, calls = counting_loader (Github_fixture.gac_of_values ~origin:None ()) in
        let response =
          Http_fixture.gate_response "config failure"
            (routed_post ~session:Http_fixture.logged_in
               ~headers:
                 [ ("Origin", "https://earde.com");
                   ("Content-Type", "application/x-www-form-urlencoded")
                 ]
               ~load_config:loader ())
        in
        Alcotest.(check int) "503" 503 (status_of response);
        Alcotest.(check int) "loader called once" 1 !calls;
        Alcotest.(check (option string)) "no-store" (Some "no-store")
          (Dream.header response "Cache-Control");
        Alcotest.(check (option string)) "referrer policy"
          (Some Earde.Request_origin.referrer_policy)
          (Dream.header response "Referrer-Policy");
        let body = Lwt_main.run (Dream.body response) in
        List.iter
          (fun needle ->
            Alcotest.(check bool) ("no leak " ^ needle) false
              (Html_assert.contains body needle))
          [ "EARDE_PUBLIC_ORIGIN"; "GITHUB_APP"; "Missing"; "Invalid";
            "public_origin"
          ])
  ; case "POST provisioning origin gate: the exact same-origin policy, \
          before any form parse" (fun () ->
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
          Alcotest.(check int) (label ^ ": 403") 403
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
        Alcotest.(check bool) "origin never reflected" false
          (Html_assert.contains (Lwt_main.run (Dream.body leak)) "evil.example"))
  ]

(* ==================== DB-free: CSRF ==================== *)

let csrf_pipeline () =
  Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n" ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ ->
      Dream.router
        [ Dream.post post_pattern (fun r ->
              make ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) r)
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
         (Dream.request ~method_:`POST ~target:(post_target "phvv-any")
            ~headers (Http_fixture.form_body body_fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  Alcotest.(check int) (label ^ ": status") expected
    (status_of (Http_fixture.gate_response label result))

let csrf_cases =
  [ case "POST provisioning CSRF: Dream verification gates every \
          submission; only a verified form reaches the database" (fun () ->
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
          (csrf_post ~cookie pipeline
             (("dream.csrf", "not-a-token") :: valid));
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
             [ ("dream.csrf", "not-a-token"); ("phvv_unknown", "x") ]);
        (* A verified token reaches Dream.sql, which raises without a pool:
           CSRF passed, Dream stripped its own field, and the store call is
           the next thing to run. *)
        Http_fixture.check_db_boundary "verified form continues"
          (csrf_post ~cookie pipeline (("dream.csrf", fresh) :: valid)))
  ; case "POST provisioning CSRF: a verified token with an invalid form \
          still re-authorizes through the database" (fun () ->
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
          [ ("unknown field", ("phvv_unknown", "x") :: fields ());
            ("blank name", fields ~name:"   " ());
            ("uppercase slug", fields ~slug:"Phvv-Home" ());
            ( "control byte in description",
              fields ~description:"phvv\001body" () )
          ])
  ]

(* ================= Database-gated integration ================= *)

(* Distinctive credential-shaped fixtures. None may appear in any page,
   redirect, header, or cookie this feature produces. *)
let access_marker = "gho_PHVV_ACCESS_TOKEN_SECRET"

let refresh_marker = "ghr_PHVV_REFRESH_TOKEN"

let pkce_marker = "PHVV_PKCE_VERIFIER_VALUE"

let state_marker = "PHVV_OAUTH_STATE_VALUE"

let secret_marker = "PHVV_CLIENT_SECRET_VALUE"

let credential_markers =
  [ ("access token", access_marker);
    ("refresh token", refresh_marker);
    ("PKCE verifier", pkce_marker);
    ("OAuth state", state_marker);
    ("client secret", secret_marker);
    ("external installation id", "951000001");
    ("external account id", "951100001")
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 951100001 AND 951100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 951100001 AND 951100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 951000001 AND 951000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phvv-%'"
    ; "DELETE FROM users WHERE username LIKE 'phvv_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 951000001 AND 951000999"
    ; "DELETE FROM rate_limits WHERE endpoint LIKE '/projects/phvv-%'"
    ]

let q_count_by_slug = Home_provisioning_fixture.q_count_by_slug

let q_count_like =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM communities WHERE slug LIKE $1"

let q_active_relations =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_projects \
   WHERE project_id = $1 AND status IN ('pending', 'accepted')"

let q_set_admin = Community_fixture.q_set_admin

(* Test-only identity drift, scoped to one reserved slug: the community row
   the store inserts comes back different from the identity it parsed, so
   the store's own bounded revalidation must roll the whole draft back as
   Inconsistent_data. Installed and dropped inside that one case under
   Lwt.finalize; production migrations are untouched, and the mutated name
   still satisfies every scoped network-identity CHECK. *)
let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_drift_fn =
  ddl
    "CREATE FUNCTION phvv_drift_fn() RETURNS trigger \
     LANGUAGE plpgsql \
     AS 'BEGIN NEW.name := NEW.name || '' Drift''; RETURN NEW; END'"

let q_create_drift_trigger =
  ddl
    "CREATE TRIGGER phvv_drift BEFORE INSERT ON communities \
     FOR EACH ROW WHEN (NEW.slug = 'phvv-drift-home') \
     EXECUTE FUNCTION phvv_drift_fn()"

let q_drop_drift_trigger =
  ddl "DROP TRIGGER IF EXISTS phvv_drift ON communities"

let q_drop_drift_fn = ddl "DROP FUNCTION IF EXISTS phvv_drift_fn()"

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

(* === the real production pipeline shape ===

   One shared two-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, so a fresh pool per request would exhaust
   Postgres max_connections; two connections are the minimum that lets the
   concurrency cases run two requests genuinely at once. Everything else is
   the real production shape — secret, memory sessions, the real router
   paths bound to the real handlers, and the POST wrapped in the same
   authenticated-mutation rate limiter main.ml applies. Session identity is
   sticky: a request that already carries a session keeps its own user, so
   independent cookies stay independent under concurrency. *)
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
            if is_admin then Dream.set_session_field request "is_admin" "true"
            else Lwt.return_unit
          in
          handler request)

(* Stands in for the production limiter's Allowed decision, so a case can
   reach the handler over a pool the real limiter (which fails closed)
   cannot query. *)
let allowing_limiter =
  Earde.Handlers.Rate_limit.make_middleware
    ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed))
    ~cleanup:ignore

let build_pipeline_with ~limit ~url =
  Dream.sql_pool ~size:2 url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions @@ identity_middleware
  @@ Dream.router
       [ Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         (* The exact production failure this suite regresses: an
            authentic, correctly signed, same-session token whose one-hour
            lifetime ran out while the page sat open. *)
         Dream.get "/mint-expired" (fun req ->
             Dream.respond (Dream.csrf_token ~valid_for:(-60.) req));
         Dream.get get_pattern (fun req -> make_get ~mode:Ob.Public req);
         Dream.post post_pattern
           (limit (fun req ->
                make ~mode:Ob.Public
                  ~load_config:(fun () -> ok_loader ())
                  req));
         Dream.get "/projects/:slug/request-home" (fun req ->
             Earde.Project_home_request_handlers
             .make_project_home_choice_handler ~mode:Ob.Public req);
         Dream.get "/c/:slug" Earde.Handlers.community_page_handler;
         Dream.get "/c/:slug/settings"
           Earde.Handlers.community_settings_handler
       ]

let build_pipeline ~url =
  build_pipeline_with ~limit:Earde.Handlers.Rate_limit.middleware ~url

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

let do_get ?pipeline ?cookie ~url ~target () =
  let pipeline =
    match pipeline with Some p -> p | None -> pipeline_for ~url
  in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = pipeline (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* [omit_token] posts a body with no framework CSRF field at all — exactly
   what a page rendered without one would send; [extra_headers] carries the
   rest of the real navigation metadata a browser attaches. *)
let do_post ?pipeline ?(origin = Some "https://earde.com")
    ?(extra_headers = []) ?(omit_token = false) ~url ~cookie ~target ~token
    ~body_fields () =
  let pipeline =
    match pipeline with Some p -> p | None -> pipeline_for ~url
  in
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [ ("Content-Type", "application/x-www-form-urlencoded");
        ("Cookie", cookie)
      ]
    @ extra_headers
  in
  let fields =
    if omit_token then body_fields else ("dream.csrf", token) :: body_fields
  in
  let* response =
    pipeline
      (Dream.request ~method_:`POST ~target ~headers (Http_fixture.form_body fields))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* One cookie-less GET that opens a fresh session for this user and returns
   its cookie plus a live CSRF token. *)
let open_session ?pipeline label ~url uid =
  as_user uid;
  let* response, token = do_get ?pipeline ~url ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return (Http_fixture.session_cookie label response, token)

let mint_token label ~url ~cookie =
  let* response, body = do_get ~url ~cookie ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return body

let mint_expired_token label ~url ~cookie =
  let* response, body = do_get ~url ~cookie ~target:"/mint-expired" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return body

(* One cookie-less GET of the creation page: the page, its session cookie,
   and the live CSRF token its own form carries. *)
let open_form label ~url ~slug uid =
  as_user uid;
  let* response, body = do_get ~url ~target:(get_target slug) () in
  Alcotest.(check int) (label ^ ": creation page 200") 200
    (status_of response);
  Lwt.return (Http_fixture.session_cookie label response, Http_fixture.csrf_of_page label body,
              body)

let check_clean_redirect label expected response body =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string)) (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string)) (label ^ ": no-referrer")
    (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  Alcotest.(check string) (label ^ ": empty body") "" body

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
    [ "communities_slug_key"; "community_projects"; "open_source_projects";
      "Caqti"; "PostgreSQL"; "SELECT"; "search_path"; "Inconsistent";
      "Storage_error"; "phvv_void"; "phvv_drift"
    ]

(* The re-rendered creation form: the requested status, the exact feedback
   copy, a live CSRF field, and the one form pointing back at this route. *)
let check_rerender label ~status ~feedback ~slug response body =
  Alcotest.(check int) (label ^ ": status") status (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string)) (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy");
  Alcotest.(check bool) (label ^ ": feedback copy") true
    (Html_assert.contains body feedback);
  Alcotest.(check bool) (label ^ ": fresh CSRF field") true
    (Html_assert.contains body "name=\"dream.csrf\"");
  Alcotest.(check bool) (label ^ ": posts back to this route") true
    (Html_assert.contains body (Printf.sprintf "action='%s'" (post_target slug)));
  Alcotest.(check int) (label ^ ": exactly one form") 1
    (Html_assert.occurrences (Html_assert.panel_fragment body) "<form")

(* The existing community-page unavailable answer, asserted exactly as the
   legacy handler already produces it: this slice deliberately changes no
   community authorization, so the check records that behaviour rather than
   imposing the provisioning handler's own header contract on it. *)
let check_community_unavailable label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check bool) (label ^ ": generic copy") true
    (Html_assert.contains body "This community does not exist.")

let check_body_no_credentials label body =
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool) (label ^ ": body free of " ^ what) false
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
      Alcotest.(check bool) (label ^ ": headers free of " ^ what) false
        (Html_assert.contains headers needle))
    credential_markers

(* === success: the PRG into the new private settings draft === *)

let success_case =
  db_case "POST provisioning: a steward's valid submission creates the \
           complete private draft and redirects into its settings"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000001L ~slug:"phvv-alpha"
          ~name:"Phvv Alpha"
      in
      let* cookie, token, page =
        open_form "creation" ~url ~slug:"phvv-alpha" owner
      in
      Alcotest.(check bool) "the form posts to the POST route" true
        (Html_assert.contains page
           (Printf.sprintf "action='%s'" (post_target "phvv-alpha")));
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-alpha") ~token
          ~body_fields:
            (fields ~name:"Phvv Provisioned Home" ~slug:"phvv-provisioned"
               ~description:"A durable home description." ())
          ()
      in
      check_clean_redirect "provisioned" (settings_target "phvv-provisioned")
        response body;
      check_no_credentials "provisioned redirect" response body;
      (* No query, fragment, result token, or internal identifier. *)
      let location =
        match Dream.header response "Location" with Some l -> l | None -> ""
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("Location free of " ^ needle) false
            (Html_assert.contains location needle))
        [ "?"; "#"; "created"; "ok=" ];
      (* The complete durable draft, asserted through the store suite's own
         production-shape checker. *)
      let* cid, _rid =
        Home_provisioning_fixture.check_provisioned_draft "durable" conn ~actor:owner ~project
          ~slug:"phvv-provisioned" ~name:"Phvv Provisioned Home"
          ~description:"A durable home description."
      in
      Alcotest.(check bool) "a community id exists" true (cid > 0);
      (* The advertised destination really serves the new draft's settings
         to its creator. *)
      let* settings_response, settings_body =
        do_get ~url ~cookie ~target:(settings_target "phvv-provisioned") ()
      in
      Alcotest.(check int) "settings 200" 200 (status_of settings_response);
      (* The settings surface addresses the community by its canonical
         slug, which is exactly what the redirect built. *)
      Alcotest.(check bool) "settings is the new community's" true
        (Html_assert.contains settings_body "phvv-provisioned");
      check_no_credentials "settings" settings_response settings_body;
      Lwt.return_unit)

(* === form parsing: 422 rendering and safe value preservation === *)

let long_name = "Phvv " ^ String.make 130 'n'

let long_description = String.make 2100 'd'

let parser_rejection_case =
  db_case "POST provisioning: every parser rejection is a 422 re-render \
           that preserves only provably safe values" (fun ~url conn ->
      let* owner = insert_user conn "phvv_fowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000002L ~slug:"phvv-form"
          ~name:"Phvv Form"
      in
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-form" owner
      in
      (* 1. Structural: an unknown field carrying a credential-shaped value
         is Invalid_form, and nothing submitted is reflected back. *)
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-form") ~token
          ~body_fields:
            (("phvv_planted", access_marker)
            :: fields ~name:"Phvv Planted Name" ~slug:"phvv-planted"
                 ~description:"Planted body." ())
          ()
      in
      check_rerender "invalid form" ~status:422
        ~feedback:"We couldn't read that submission." ~slug:"phvv-form"
        response body;
      check_no_credentials "invalid form" response body;
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("invalid form reflects no " ^ needle) false
            (Html_assert.contains body needle))
        [ "phvv_planted"; "Phvv Planted Name"; "phvv-planted";
          "Planted body."
        ];
      Alcotest.(check bool) "the controls are blank" true
        (Html_assert.contains body "name='community_slug' maxlength='80' value=''");
      (* 2. Semantic name: the exact submitted values survive. *)
      let* token = mint_token "name" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-form") ~token
          ~body_fields:
            (fields ~name:long_name ~slug:"phvv-kept-slug"
               ~description:"Kept body." ())
          ()
      in
      check_rerender "invalid name" ~status:422
        ~feedback:"Enter a community name we can use." ~slug:"phvv-form"
        response body;
      Alcotest.(check bool) "name preserved" true (Html_assert.contains body long_name);
      Alcotest.(check bool) "slug preserved" true
        (Html_assert.contains body "value='phvv-kept-slug'");
      Alcotest.(check bool) "description preserved" true
        (Html_assert.contains body ">Kept body.</textarea>");
      (* 3. Semantic slug. *)
      let* token = mint_token "slug" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-form") ~token
          ~body_fields:
            (fields ~name:"Phvv Kept Name" ~slug:"Phvv-Bad-Slug"
               ~description:"Kept body." ())
          ()
      in
      check_rerender "invalid slug" ~status:422
        ~feedback:"Enter a community address using lowercase letters"
        ~slug:"phvv-form" response body;
      Alcotest.(check bool) "slug preserved verbatim" true
        (Html_assert.contains body "value='Phvv-Bad-Slug'");
      Alcotest.(check bool) "name preserved" true
        (Html_assert.contains body "value='Phvv Kept Name'");
      (* 4. Semantic description. *)
      let* token = mint_token "description" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-form") ~token
          ~body_fields:
            (fields ~name:"Phvv Kept Name" ~slug:"phvv-kept-slug"
               ~description:long_description ())
          ()
      in
      check_rerender "invalid description" ~status:422
        ~feedback:"That description can't be used." ~slug:"phvv-form"
        response body;
      Alcotest.(check bool) "description preserved" true
        (Html_assert.contains body long_description);
      (* Nothing durable was created by any of the four. *)
      let* communities = find conn "communities" q_count_like "phvv-%" in
      Alcotest.(check int) "no community created" 0 communities;
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "no relation created" 0 relations;
      Lwt.return_unit)

let escaping_case =
  db_case "POST provisioning: every preserved value is escaped, never \
           markup" (fun ~url conn ->
      let* owner = insert_user conn "phvv_escowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:951000003L ~slug:"phvv-esc"
          ~name:"Phvv Esc"
      in
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-esc" owner
      in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-esc") ~token
          ~body_fields:
            (fields ~name:"<script>phvv_x()</script>" ~slug:"Phvv-Bad"
               ~description:"<b>phvv</b>" ())
          ()
      in
      Alcotest.(check int) "422" 422 (status_of response);
      Alcotest.(check bool) "escaped name" true
        (Html_assert.contains body "&lt;script&gt;phvv_x()&lt;/script&gt;");
      Alcotest.(check bool) "raw name absent" false
        (Html_assert.contains body "<script>phvv_x()</script>");
      Alcotest.(check bool) "escaped description" true
        (Html_assert.contains body "&lt;b&gt;phvv&lt;/b&gt;");
      Alcotest.(check bool) "raw description absent" false
        (Html_assert.contains body "<b>phvv</b>");
      Lwt.return_unit)

(* === community-slug conflict === *)

let slug_conflict_case =
  db_case "POST provisioning: a taken community slug is a 409 re-render \
           that preserves the submission and stays retryable"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_cowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000004L ~slug:"phvv-conflict"
          ~name:"Phvv Conflict"
      in
      (* A legacy community and a network community, both simply "taken". *)
      let* _legacy =
        insert_community ~name:"Phvv Legacy" ~network:false conn "phvv-legacy"
      in
      let* _network =
        insert_community ~name:"Phvv Network" conn "phvv-network"
      in
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-conflict" owner
      in
      let attempt label ~token ~slug =
        let* response, body =
          do_post ~url ~cookie ~target:(post_target "phvv-conflict") ~token
            ~body_fields:
              (fields ~name:"Phvv Conflict Home" ~slug
                 ~description:"Conflict body." ())
            ()
        in
        check_rerender label ~status:409
          ~feedback:"That community address is already taken."
          ~slug:"phvv-conflict" response body;
        Alcotest.(check bool) (label ^ ": slug preserved") true
          (Html_assert.contains body (Printf.sprintf "value='%s'" slug));
        Alcotest.(check bool) (label ^ ": name preserved") true
          (Html_assert.contains body "value='Phvv Conflict Home'");
        Alcotest.(check bool) (label ^ ": description preserved") true
          (Html_assert.contains body ">Conflict body.</textarea>");
        (* Nothing about the conflicting community leaks. *)
        List.iter
          (fun needle ->
            Alcotest.(check bool) (label ^ ": no leak " ^ needle) false
              (Html_assert.contains body needle))
          [ "Phvv Legacy"; "Phvv Network"; "visibility"; "onboarding_state";
            "is_network_community"
          ];
        check_no_credentials label response body;
        Lwt.return_unit
      in
      let* () = attempt "legacy conflict" ~token ~slug:"phvv-legacy" in
      let* token = mint_token "network" ~url ~cookie in
      let* () = attempt "network conflict" ~token ~slug:"phvv-network" in
      (* Neither attempt created anything, and the project can still win a
         free slug. *)
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "no relation after conflicts" 0 relations;
      let* token = mint_token "retry" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-conflict") ~token
          ~body_fields:
            (fields ~name:"Phvv Conflict Home" ~slug:"phvv-conflict-home"
               ~description:"Conflict body." ())
          ()
      in
      check_clean_redirect "retry succeeds"
        (settings_target "phvv-conflict-home") response body;
      let* _ =
        Home_provisioning_fixture.check_provisioned_draft "retry" conn ~actor:owner ~project
          ~slug:"phvv-conflict-home" ~name:"Phvv Conflict Home"
          ~description:"Conflict body."
      in
      Lwt.return_unit)

(* === active home, and replay === *)

let active_home_case =
  db_case "POST provisioning: a pending request or accepted home sends the \
           steward to the authoritative home page, never a stale form"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_aowner" in
      let* moderator = insert_user conn "phvv_amod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000005L ~slug:"phvv-active"
          ~name:"Phvv Active"
      in
      let* cid = insert_community ~name:"Phvv Target" conn "phvv-target" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      (* The form is opened while the project is still eligible; the
         relation appears underneath it. *)
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-active" owner
      in
      let* () =
        request_pending "pending" conn ~user:owner ~slug:"phvv-active"
          ~community:cid
      in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-active") ~token
          ~body_fields:(fields ~slug:"phvv-active-home" ())
          ()
      in
      check_clean_redirect "pending" (request_home_target "phvv-active")
        response body;
      check_no_credentials "pending redirect" response body;
      let* communities =
        find conn "no draft" q_count_by_slug "phvv-active-home"
      in
      Alcotest.(check int) "no community draft survives" 0 communities;
      (* Accepted behaves identically. *)
      let* () =
        review "accept" conn ~reviewer:moderator ~slug:"phvv-active"
          ~community_slug:"phvv-target" Rvs.Accept
      in
      let* token = mint_token "accepted" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-active") ~token
          ~body_fields:(fields ~slug:"phvv-active-home" ())
          ()
      in
      check_clean_redirect "accepted" (request_home_target "phvv-active")
        response body;
      let* communities =
        find conn "still no draft" q_count_by_slug "phvv-active-home"
      in
      Alcotest.(check int) "still no community draft" 0 communities;
      let* active = find conn "active" q_active_relations project in
      Alcotest.(check int) "exactly one active relation" 1 active;
      Lwt.return_unit)

let replay_case =
  db_case "POST provisioning: replaying a succeeded submission creates \
           nothing and lands on the current home page" (fun ~url conn ->
      let* owner = insert_user conn "phvv_rowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000006L ~slug:"phvv-replay"
          ~name:"Phvv Replay"
      in
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-replay" owner
      in
      let body_fields =
        fields ~name:"Phvv Replay Home" ~slug:"phvv-replay-home" ()
      in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-replay") ~token
          ~body_fields ()
      in
      check_clean_redirect "first" (settings_target "phvv-replay-home")
        response body;
      (* The very same token replayed. Dream's CSRF tokens are stateless
         and session-bound rather than one-shot, so the token still
         verifies: durable idempotency, not token consumption, is what
         protects this mutation. *)
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-replay") ~token
          ~body_fields ()
      in
      check_clean_redirect "same-token replay"
        (request_home_target "phvv-replay") response body;
      (* A freshly minted token replays to the same place. *)
      let* fresh = mint_token "fresh" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-replay") ~token:fresh
          ~body_fields ()
      in
      check_clean_redirect "fresh-token replay"
        (request_home_target "phvv-replay") response body;
      (* A genuinely unusable token no longer dead-ends: it is refused
         without opening the store and answered by reloading the
         owner-authorized page. This project now has an active home, so
         that reload is exactly the generic 404 its own GET gives. *)
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-replay")
          ~token:"phvv-not-a-token" ~body_fields ()
      in
      check_generic_404 "bad token" response body;
      (* Exactly one of everything. *)
      let* communities =
        find conn "communities" q_count_by_slug "phvv-replay-home"
      in
      Alcotest.(check int) "exactly one community" 1 communities;
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "exactly one relation" 1 relations;
      let* active = find conn "active" q_active_relations project in
      Alcotest.(check int) "exactly one active home" 1 active;
      let* _ =
        Home_provisioning_fixture.check_provisioned_draft "after replays" conn ~actor:owner
          ~project ~slug:"phvv-replay-home" ~name:"Phvv Replay Home"
          ~description:"<null>"
      in
      Lwt.return_unit)

(* === authorization loss and lifecycle drift between GET and POST === *)

let drift_case =
  db_case "POST provisioning: authorization loss and verification drift \
           between the GET and the POST are one identical generic 404"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_downer" in
      let* stranger = insert_user conn "phvv_dstranger" in
      let* admin = insert_user conn "phvv_dadmin" in
      let* () = exec conn "durable admin" q_set_admin (admin, true) in
      let* _inst, drifting =
        make_project conn ~user:owner ~ext_id:951000007L ~slug:"phvv-drift"
          ~name:"Phvv Drift"
      in
      let* _inst2, healthy =
        make_project conn ~user:owner ~ext_id:951000008L ~slug:"phvv-sound"
          ~name:"Phvv Sound"
      in
      let bodies = ref [] in
      let expect_404 label ~cookie ~token ~slug =
        let* response, body =
          do_post ~url ~cookie ~target:(post_target slug) ~token
            ~body_fields:(fields ~slug:"phvv-drift-target" ())
            ()
        in
        check_generic_404 label response body;
        check_no_credentials label response body;
        bodies := body :: !bodies;
        Lwt.return_unit
      in
      (* The steward opens the form while everything is still fine. *)
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-drift" owner
      in
      let* () =
        exec conn "stale" Home_request_fixture.q_set_verification (drifting, "stale")
      in
      let* () = expect_404 "stale project" ~cookie ~token ~slug:"phvv-drift" in
      let* () =
        exec conn "revoked" Home_request_fixture.q_set_verification (drifting, "revoked")
      in
      let* token = mint_token "revoked" ~url ~cookie in
      let* () =
        expect_404 "revoked project" ~cookie ~token ~slug:"phvv-drift"
      in
      let* () =
        exec conn "verified" Home_request_fixture.q_set_verification (drifting, "verified")
      in
      let* () = exec conn "unsteward" Home_request_fixture.q_delete_steward (drifting, owner) in
      let* token = mint_token "unstewarded" ~url ~cookie in
      let* () =
        expect_404 "removed stewardship" ~cookie ~token ~slug:"phvv-drift"
      in
      (* A durable admin who is not a steward, and an unrelated user, each
         on their own session, against the still-healthy project. *)
      let* admin_cookie, admin_token = open_session "admin" ~url admin in
      let* () =
        expect_404 "durable admin without stewardship" ~cookie:admin_cookie
          ~token:admin_token ~slug:"phvv-sound"
      in
      let* stranger_cookie, stranger_token =
        open_session "stranger" ~url stranger
      in
      let* () =
        expect_404 "unrelated user" ~cookie:stranger_cookie
          ~token:stranger_token ~slug:"phvv-sound"
      in
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
      let* () =
        Home_provisioning_fixture.check_no_state "drifting" conn ~project:drifting
          ~slug:"phvv-drift-target"
      in
      Home_provisioning_fixture.check_no_state "healthy" conn ~project:healthy
        ~slug:"phvv-drift-target")

(* === durable failure === *)

let inconsistent_case =
  db_case "POST provisioning: a durable identity drift inside the \
           transaction is one generic non-cacheable 500, with no partial \
           draft" (fun ~url conn ->
      let* owner = insert_user conn "phvv_icowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000009L ~slug:"phvv-ic"
          ~name:"Phvv Ic"
      in
      let* cookie, token, _ = open_form "creation" ~url ~slug:"phvv-ic" owner in
      let* () = exec conn "create drift fn" q_create_drift_fn () in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "create drift trigger" q_create_drift_trigger ()
          in
          let* response, body =
            do_post ~url ~cookie ~target:(post_target "phvv-ic") ~token
              ~body_fields:
                (fields ~name:"Phvv Ic Home" ~slug:"phvv-drift-home" ())
              ()
          in
          check_generic_500 "identity drift" response body;
          check_no_credentials "identity drift" response body;
          Home_provisioning_fixture.check_no_state "identity drift" conn ~project
            ~slug:"phvv-drift-home")
        (fun () ->
          let* () = exec conn "drop trigger" q_drop_drift_trigger () in
          exec conn "drop fn" q_drop_drift_fn ()))

let storage_case =
  db_case "POST provisioning: a real database failure is one generic \
           non-cacheable 500 on both the store and the re-render path"
    (fun ~url _conn ->
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=phvv_void"))
      in
      (* The production limiter fronts this route and fails closed on a
         pool it cannot query: the request is refused before the handler
         runs. The handler's own storage-error collapse is then exercised
         behind an allowing limiter — the only way a request can reach it
         over this pool. Dedicated pools: the shared pipeline must keep
         talking to the real schema. *)
      let limited_pipe = build_pipeline ~url:poisoned in
      let* cookie, token =
        open_session ~pipeline:limited_pipe "poisoned limiter" ~url 424242
      in
      let* response, _ =
        do_post ~pipeline:limited_pipe ~url ~cookie
          ~target:(post_target "phvv-anything") ~token
          ~body_fields:(fields ~slug:"phvv-void-home" ())
          ()
      in
      Alcotest.(check int) "limiter refuses first" 503 (status_of response);
      let poison_pipe = build_pipeline_with ~limit:allowing_limiter ~url:poisoned in
      let* cookie, token =
        open_session ~pipeline:poison_pipe "poisoned" ~url 424242
      in
      let* response, body =
        do_post ~pipeline:poison_pipe ~url ~cookie
          ~target:(post_target "phvv-anything") ~token
          ~body_fields:(fields ~slug:"phvv-void-home" ())
          ()
      in
      check_generic_500 "poisoned store call" response body;
      let* response, body =
        do_post ~pipeline:poison_pipe ~url ~cookie
          ~target:(post_target "phvv-anything") ~token
          ~body_fields:(fields ~slug:"Phvv-Bad" ())
          ()
      in
      check_generic_500 "poisoned re-render" response body;
      Lwt.return_unit)

(* === concurrency through HTTP === *)

let location response =
  match Dream.header response "Location" with Some l -> l | None -> ""

let same_project_race_case =
  db_case "POST provisioning: two concurrent submissions for one project \
           leave exactly one community; the loser lands on the home page"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_p1owner" in
      let* second = insert_user conn "phvv_p1second" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:951000010L ~slug:"phvv-race"
          ~name:"Phvv Race"
      in
      let* () = add_steward conn ~project ~user:second ~installation:inst in
      let* cookie_a, token_a, _ =
        open_form "steward a" ~url ~slug:"phvv-race" owner
      in
      let* cookie_b, token_b, _ =
        open_form "steward b" ~url ~slug:"phvv-race" second
      in
      (* Two independent sessions and tokens genuinely in flight at once
         over two pooled connections; the project lock inside the store
         serializes them without any sleep. *)
      let* (response_a, body_a), (response_b, body_b) =
        Lwt.both
          (do_post ~url ~cookie:cookie_a ~target:(post_target "phvv-race")
             ~token:token_a
             ~body_fields:(fields ~name:"Phvv Race A" ~slug:"phvv-race-a" ())
             ())
          (do_post ~url ~cookie:cookie_b ~target:(post_target "phvv-race")
             ~token:token_b
             ~body_fields:(fields ~name:"Phvv Race B" ~slug:"phvv-race-b" ())
             ())
      in
      let a_lost =
        String.equal (location response_a) (request_home_target "phvv-race")
      in
      let win, win_body, lose, lose_body, winning_slug, losing_slug =
        if a_lost then
          (response_b, body_b, response_a, body_a, "phvv-race-b",
           "phvv-race-a")
        else
          (response_a, body_a, response_b, body_b, "phvv-race-a",
           "phvv-race-b")
      in
      check_clean_redirect "winner" (settings_target winning_slug) win
        win_body;
      check_clean_redirect "loser" (request_home_target "phvv-race") lose
        lose_body;
      let* winners = find conn "winner row" q_count_by_slug winning_slug in
      Alcotest.(check int) "winning community exists" 1 winners;
      let* losers = find conn "loser row" q_count_by_slug losing_slug in
      Alcotest.(check int) "loser community absent" 0 losers;
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "exactly one relation" 1 relations;
      let* active = find conn "active" q_active_relations project in
      Alcotest.(check int) "exactly one active home" 1 active;
      Lwt.return_unit)

let same_slug_race_case =
  db_case "POST provisioning: two projects racing for one community slug \
           leave one winner and a clean 409 loser" (fun ~url conn ->
      let* owner_a = insert_user conn "phvv_s1owner" in
      let* owner_b = insert_user conn "phvv_s2owner" in
      let* _inst_a, project_a =
        make_project conn ~user:owner_a ~ext_id:951000011L ~slug:"phvv-slug-a"
          ~name:"Phvv Slug A"
      in
      let* _inst_b, project_b =
        make_project conn ~user:owner_b ~ext_id:951000012L ~slug:"phvv-slug-b"
          ~name:"Phvv Slug B"
      in
      let* cookie_a, token_a, _ =
        open_form "project a" ~url ~slug:"phvv-slug-a" owner_a
      in
      let* cookie_b, token_b, _ =
        open_form "project b" ~url ~slug:"phvv-slug-b" owner_b
      in
      let body_fields =
        fields ~name:"Phvv Shared Home" ~slug:"phvv-shared-home" ()
      in
      let* (response_a, body_a), (response_b, body_b) =
        Lwt.both
          (do_post ~url ~cookie:cookie_a ~target:(post_target "phvv-slug-a")
             ~token:token_a ~body_fields ())
          (do_post ~url ~cookie:cookie_b ~target:(post_target "phvv-slug-b")
             ~token:token_b ~body_fields ())
      in
      let a_won = status_of response_a = 303 in
      let ( winner, winner_body, winner_project, loser, loser_body,
            loser_project, loser_slug ) =
        if a_won then
          (response_a, body_a, project_a, response_b, body_b, project_b,
           "phvv-slug-b")
        else
          (response_b, body_b, project_b, response_a, body_a, project_a,
           "phvv-slug-a")
      in
      check_clean_redirect "winner" (settings_target "phvv-shared-home")
        winner winner_body;
      check_rerender "loser" ~status:409
        ~feedback:"That community address is already taken." ~slug:loser_slug
        loser loser_body;
      Alcotest.(check bool) "loser keeps its submission" true
        (Html_assert.contains loser_body "value='phvv-shared-home'");
      let* communities =
        find conn "communities" q_count_by_slug "phvv-shared-home"
      in
      Alcotest.(check int) "exactly one community" 1 communities;
      let* winner_relations =
        find conn "winner relations" Home_request_fixture.q_count_for_project winner_project
      in
      Alcotest.(check int) "winner has one relation" 1 winner_relations;
      let* loser_relations =
        find conn "loser relations" Home_request_fixture.q_count_for_project loser_project
      in
      Alcotest.(check int) "loser has no relation" 0 loser_relations;
      Lwt.return_unit)

(* === destination authorization and draft privacy === *)

let destination_case =
  db_case "POST provisioning: the new draft stays private to its creator \
           and out of public discovery" (fun ~url conn ->
      let* owner = insert_user conn "phvv_destowner" in
      let* stranger = insert_user conn "phvv_deststranger" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:951000013L ~slug:"phvv-dest"
          ~name:"Phvv Dest"
      in
      let* cookie, token, _ =
        open_form "creation" ~url ~slug:"phvv-dest" owner
      in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-dest") ~token
          ~body_fields:
            (fields ~name:"Phvv Dest Home" ~slug:"phvv-dest-home" ())
          ()
      in
      check_clean_redirect "provisioned" (settings_target "phvv-dest-home")
        response body;
      (* The creator: settings and the community page are both authorized,
         and the draft is not indexable. *)
      let* settings_response, settings_body =
        do_get ~url ~cookie ~target:(settings_target "phvv-dest-home") ()
      in
      Alcotest.(check int) "creator settings 200" 200
        (status_of settings_response);
      Alcotest.(check bool) "settings is the draft's own" true
        (Html_assert.contains settings_body "phvv-dest-home");
      let* page_response, page_body =
        do_get ~url ~cookie ~target:(community_target "phvv-dest-home") ()
      in
      Alcotest.(check int) "creator community page 200" 200
        (status_of page_response);
      Alcotest.(check bool) "the draft is noindex" true
        (Html_assert.contains page_body "content='noindex'");
      (* An unrelated authenticated user. *)
      let* stranger_cookie, _ = open_session "stranger" ~url stranger in
      let* response, body =
        do_get ~url ~cookie:stranger_cookie
          ~target:(community_target "phvv-dest-home") ()
      in
      check_community_unavailable "stranger community page" response body;
      Alcotest.(check bool) "stranger page carries no draft identity" false
        (Html_assert.contains body "phvv-dest-home");
      let* response, body =
        do_get ~url ~cookie:stranger_cookie
          ~target:(settings_target "phvv-dest-home") ()
      in
      Alcotest.(check int) "stranger settings refused" 403
        (status_of response);
      Alcotest.(check bool) "stranger sees no draft content" false
        (Html_assert.contains body "Phvv Dest Home");
      Alcotest.(check bool) "stranger sees no draft slug" false
        (Html_assert.contains body "phvv-dest-home");
      (* An anonymous visitor. *)
      as_anonymous ();
      let* response, body =
        do_get ~url ~target:(community_target "phvv-dest-home") ()
      in
      check_community_unavailable "anonymous community page" response body;
      Alcotest.(check bool) "anonymous page carries no draft identity" false
        (Html_assert.contains body "phvv-dest-home");
      let* response, body =
        do_get ~url ~target:(settings_target "phvv-dest-home") ()
      in
      Alcotest.(check bool) "anonymous settings never render the draft" true
        (status_of response = 302 || status_of response = 303);
      Alcotest.(check bool) "anonymous body carries no draft identity" false
        (Html_assert.contains body "Phvv Dest Home");
      (* Public discovery: the real search query cannot see it. *)
      let* found = Earde.Db.search_communities conn "Phvv Dest" 20 0 in
      (match found with
      | Ok rows ->
          Alcotest.(check int) "absent from public discovery" 0
            (List.length rows)
      | Error e -> Alcotest.failf "discovery query failed: %s" e);
      Lwt.return_unit)

(* === the mutation rate limiter really wraps this route === *)

let rate_limit_case =
  db_case "POST provisioning: the route carries the shared \
           authenticated-mutation rate limit" (fun ~url _conn ->
      let* cookie, token = open_session "rate limit" ~url 424243 in
      let target = post_target "phvv-ratelimit" in
      (* The project does not exist, so every allowed attempt is the
         generic 404 and nothing durable is ever created. *)
      let rec drive n =
        if n > 12 then
          Alcotest.fail "the limiter never blocked within 12 attempts"
        else
          let* response, body =
            do_post ~url ~cookie ~target ~token
              ~body_fields:(fields ~slug:"phvv-ratelimit-home" ())
              ()
          in
          if Html_assert.contains body "Too Many Attempts" then Lwt.return (n, response)
          else (
            Alcotest.(check int)
              (Printf.sprintf "attempt %d is the generic 404" n)
              404 (status_of response);
            drive (n + 1))
      in
      let* attempts, response = drive 1 in
      Alcotest.(check bool) "blocked only after several attempts" true
        (attempts > 1);
      Alcotest.(check int) "blocked page status" 200 (status_of response);
      Lwt.return_unit)

(* === privacy sweep === *)

let privacy_case =
  db_case "POST provisioning: no credential-shaped fixture reaches any \
           response, redirect, cookie, or the created draft"
    (fun ~url conn ->
      let* owner = insert_user conn "phvv_privowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000001L ~slug:"phvv-priv"
          ~name:"Phvv Priv"
      in
      let* cookie, token, page =
        open_form "creation" ~url ~slug:"phvv-priv" owner
      in
      check_body_no_credentials "creation page" page;
      (* A rejected submission carrying every marker in fields the parser
         does not know. *)
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-priv") ~token
          ~body_fields:
            (List.map
               (fun (_, marker) -> ("phvv_" ^ marker, marker))
               credential_markers
            @ fields ())
          ()
      in
      Alcotest.(check int) "422" 422 (status_of response);
      check_no_credentials "rejected submission" response body;
      (* A successful submission: redirect, settings page, and the durable
         draft. *)
      let* token = mint_token "success" ~url ~cookie in
      let* response, body =
        do_post ~url ~cookie ~target:(post_target "phvv-priv") ~token
          ~body_fields:
            (fields ~name:"Phvv Priv Home" ~slug:"phvv-priv-home"
               ~description:"Safe body." ())
          ()
      in
      check_clean_redirect "success" (settings_target "phvv-priv-home")
        response body;
      check_no_credentials "success redirect" response body;
      let* settings_response, settings_body =
        do_get ~url ~cookie ~target:(settings_target "phvv-priv-home") ()
      in
      check_no_credentials "settings page" settings_response settings_body;
      (* The intentionally supplied safe identity is what survives —
         the slug on the settings surface, the full identity durably. *)
      Alcotest.(check bool) "the supplied slug is present" true
        (Html_assert.contains settings_body "phvv-priv-home");
      let* _ =
        Home_provisioning_fixture.check_provisioned_draft "privacy" conn ~actor:owner ~project
          ~slug:"phvv-priv-home" ~name:"Phvv Priv Home"
          ~description:"Safe body."
      in
      Lwt.return_unit)

(* === the real browser contract ===

   A regression for the launch smoke-test blocker: the steward opened
   /projects/<slug>/community-home/new, filled it in, and submitted hours
   later. Dream's framework CSRF token is valid for one hour while
   sql_sessions keeps the login for two weeks, so the still-authenticated
   page posted an authentic but expired token and the handler answered a
   terminal generic 403 — no form, no explanation, no way back into the
   flow, and the typed values gone.

   Everything here goes through the markup the browser actually receives:
   the control names are read out of the rendered form rather than
   hard-coded, and the token is the one the page itself carries. *)

(* Every control name the rendered creation form submits, in document
   order — the framework CSRF field Dream emits plus the page's own
   application fields. *)
let form_control_names label ~slug html =
  let opening = Printf.sprintf "action='%s'" (post_target slug) in
  let form_start =
    match Html_assert.index_from html opening 0 with
    | Some i -> i
    | None -> Alcotest.fail (label ^ ": the creation form is not rendered")
  in
  let form_end =
    match Html_assert.index_from html "</form>" form_start with
    | Some i -> i
    | None -> Alcotest.fail (label ^ ": unterminated creation form")
  in
  let form = String.sub html form_start (form_end - form_start) in
  let rec scan from acc =
    match Html_assert.index_from form "name=" from with
    | None -> List.rev acc
    | Some i ->
        let quote_at = i + String.length "name=" in
        if quote_at >= String.length form then List.rev acc
        else
          let quote = form.[quote_at] in
          let start = quote_at + 1 in
          (match String.index_from_opt form start quote with
          | None -> List.rev acc
          | Some e -> scan (e + 1) (String.sub form start (e - start) :: acc))
  in
  scan 0 []

(* The navigation metadata a same-origin form POST really carries. *)
let browser_headers =
  [ ("Sec-Fetch-Site", "same-origin");
    ("Sec-Fetch-Mode", "navigate");
    ("Referer", "https://earde.com" ^ get_target "phvv-browser")
  ]

(* The authenticated-mutation rate limiter allows five attempts per IP and
   endpoint per minute, and the browser-contract case deliberately makes
   more than that: the limiter itself is covered by [rate_limit_case], so
   this one resets its window between phases rather than re-proving it. *)
let q_clear_rate_limits =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DELETE FROM rate_limits WHERE endpoint LIKE '/projects/phvv-%'"

let clear_rate_limit conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* cleared = C.exec q_clear_rate_limits () in
  let* () = or_fail "clear rate limits" cleared in
  Lwt.return_unit

let q_count_audit =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_home_audit_events WHERE project_id = $1"

let q_count_notifs =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM notifications WHERE project_id = $1"

let q_count_members_like =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_members m \
   JOIN communities c ON c.id = m.community_id WHERE c.slug LIKE $1"

let q_count_mods_like =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_moderators m \
   JOIN communities c ON c.id = m.community_id WHERE c.slug LIKE $1"

let q_count_sections_like =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_sections s \
   JOIN communities c ON c.id = s.community_id WHERE c.slug LIKE $1"

let q_count_channels_like =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM channels ch \
   JOIN communities c ON c.id = ch.community_id WHERE c.slug LIKE $1"

(* Nothing of a community home exists anywhere: not the community, not the
   relation, not the membership, role, section, channel, audit event, or
   notification a real provision would have committed together. *)
let check_nothing_created label conn ~project =
  let* communities = find conn "communities" q_count_like "phvv-%" in
  Alcotest.(check int) (label ^ ": no community") 0 communities;
  let* relations = find conn "relations" q_active_relations project in
  Alcotest.(check int) (label ^ ": no relation") 0 relations;
  let* members = find conn "members" q_count_members_like "phvv-%" in
  Alcotest.(check int) (label ^ ": no membership") 0 members;
  let* mods = find conn "moderators" q_count_mods_like "phvv-%" in
  Alcotest.(check int) (label ^ ": no moderator") 0 mods;
  let* sections = find conn "sections" q_count_sections_like "phvv-%" in
  Alcotest.(check int) (label ^ ": no section") 0 sections;
  let* channels = find conn "channels" q_count_channels_like "phvv-%" in
  Alcotest.(check int) (label ^ ": no channel") 0 channels;
  let* audit = find conn "audit" q_count_audit project in
  Alcotest.(check int) (label ^ ": no audit event") 0 audit;
  let* notifs = find conn "notifications" q_count_notifs project in
  Alcotest.(check int) (label ^ ": no notification") 0 notifs;
  Lwt.return_unit

let browser_contract_case =
  db_case "POST provisioning: the rendered form's own fields and token \
           redirect, a stale or forged token re-renders a usable form \
           without creating anything, and exactly one complete draft \
           results" (fun ~url conn ->
      let* owner = insert_user conn "phvv_browser" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:951000002L
          ~slug:"phvv-browser" ~name:"Phvv Browser"
      in
      let* cookie, page_token, page =
        open_form "browser" ~url ~slug:"phvv-browser" owner
      in

      (* --- 1. the page really is the contract the browser follows ---

         Its referrer policy must keep the real Origin on the POST it
         hosts: a "no-referrer" document would make the browser send
         Origin: null, which the same-origin gate rejects. *)
      let* page_response, _ =
        do_get ~url ~cookie ~target:(get_target "phvv-browser") ()
      in
      Alcotest.(check (option string))
        "the page keeps its own POST's Origin"
        (Some Earde.Request_origin.referrer_policy)
        (Dream.header page_response "Referrer-Policy");
      let names = form_control_names "browser" ~slug:"phvv-browser" page in
      Alcotest.(check (list string))
        "exactly the framework field and the three application fields"
        [ "dream.csrf"; "community_name"; "community_slug";
          "community_description"
        ]
        names;
      (* The submitted body is built from those names alone. *)
      let typed =
        [ ("community_name", "Phvv Browser Home");
          ("community_slug", "phvv-browser-home");
          ("community_description", "Private draft for local smoke testing.")
        ]
      in
      let body_fields =
        List.filter_map
          (fun name ->
            if String.equal name "dream.csrf" then None
            else
              match List.assoc_opt name typed with
              | Some value -> Some (name, value)
              | None -> Alcotest.failf "browser: unexpected control %s" name)
          names
      in
      let post ?origin ?omit_token ~token () =
        do_post ?origin ?omit_token ~extra_headers:browser_headers ~url
          ~cookie ~target:(post_target "phvv-browser") ~token ~body_fields ()
      in

      (* --- 2. the exact production failure: an authentic same-session
         token that outlived its one-hour validity --- *)
      let* expired = mint_expired_token "expired" ~url ~cookie in
      let* response, body = post ~token:expired () in
      check_rerender "expired token" ~status:403
        ~feedback:"This page had been open too long"
        ~slug:"phvv-browser" response body;
      check_no_credentials "expired token" response body;
      let* () = check_nothing_created "expired token" conn ~project in

      (* --- 3. a forged, absent, duplicated, or foreign-session token is
         refused exactly as hard, and still creates nothing --- *)
      let* response, body = post ~token:"not-a-token" () in
      check_rerender "forged token" ~status:403
        ~feedback:"This page had been open too long"
        ~slug:"phvv-browser" response body;
      let* response, body = post ~omit_token:true ~token:"" () in
      check_rerender "absent token" ~status:403
        ~feedback:"This page had been open too long"
        ~slug:"phvv-browser" response body;
      (* Another session's live token is not this session's. *)
      let* other_cookie, _ = open_session "other" ~url owner in
      let* foreign = mint_token "foreign" ~url ~cookie:other_cookie in
      as_user owner;
      let* response, body = post ~token:foreign () in
      check_rerender "foreign-session token" ~status:403
        ~feedback:"This page had been open too long"
        ~slug:"phvv-browser" response body;
      let* () = check_nothing_created "rejected tokens" conn ~project in

      (* --- 4. the origin gate is untouched: a cross-origin POST carrying
         the page's own live token is still the terminal 403, with no form
         and no fresh token handed out --- *)
      let* () = clear_rate_limit conn in
      let* response, body =
        post ~origin:(Some "https://evil.example") ~token:page_token ()
      in
      Alcotest.(check int) "cross-origin: 403" 403 (status_of response);
      Alcotest.(check bool) "cross-origin: no form" false
        (Html_assert.contains body "phv-form");
      Alcotest.(check bool) "cross-origin: no fresh token" false
        (Html_assert.contains body "name=\"dream.csrf\"");
      Alcotest.(check bool) "cross-origin: generic refusal" true
        (Html_assert.contains body "This request is not allowed.");
      Alcotest.(check bool) "cross-origin: origin never reflected" false
        (Html_assert.contains body "evil.example");
      let* () = check_nothing_created "cross-origin" conn ~project in

      (* --- 5. the steward recovers from the re-rendered page itself:
         its fresh token, the same typed values, one redirect --- *)
      let* () = clear_rate_limit conn in
      let* response, body = post ~token:expired () in
      let recovered = Http_fixture.csrf_of_page "recovered" body in
      Alcotest.(check bool) "the re-render carries a different token" true
        (not (String.equal recovered expired));
      Alcotest.(check int) "the re-render is still a refusal" 403
        (status_of response);
      let* response, body = post ~token:recovered () in
      check_clean_redirect "recovered submission"
        (settings_target "phvv-browser-home") response body;

      (* --- 6. exactly one complete draft, and no second one --- *)
      let* _cid, _rid =
        Home_provisioning_fixture.check_provisioned_draft "browser" conn ~actor:owner ~project
          ~slug:"phvv-browser-home" ~name:"Phvv Browser Home"
          ~description:"Private draft for local smoke testing."
      in
      let* communities = find conn "communities" q_count_like "phvv-%" in
      Alcotest.(check int) "exactly one community" 1 communities;
      let* relations = find conn "relations" q_active_relations project in
      Alcotest.(check int) "exactly one active relation" 1 relations;
      let* named = find conn "named" q_count_by_slug "phvv-browser-home" in
      Alcotest.(check int) "and it is the one just created" 1 named;
      Lwt.return_unit)

let db_suite =
  [ success_case; browser_contract_case; parser_rejection_case;
    escaping_case; slug_conflict_case; active_home_case; replay_case;
    drift_case; inconsistent_case; storage_case; same_project_race_case;
    same_slug_race_case; destination_case; rate_limit_case; privacy_case
  ]

let suites =
    (* The provisioning POST route: the exact registered pattern with no
       alias, the DB-free rollout/authentication gates (every rejection
       precedes any route read, configuration load, origin check, form
       parse, or SQL), the configuration/origin/CSRF sequence, and the
       database-gated flow — the PRG into the new private settings draft,
       the four 422 parser rejections and their safe value preservation,
       slug conflict, active-home and replay behaviour, authorization
       drift, durable failure, real two-connection concurrency, destination
       authorization and draft privacy, the mutation rate limit, and the
       privacy sweep. *)
  [ ("project_home_provisioning_post_route", route_cases)
  ; ("project_home_provisioning_post_gates", gate_cases)
  ; ("project_home_provisioning_post_config_origin", config_origin_cases)
  ; ("project_home_provisioning_post_csrf", csrf_cases)
  ; ("project_home_provisioning_post_db", db_suite)
  ]
