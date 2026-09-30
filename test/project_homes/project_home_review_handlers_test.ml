module Ob = Earde.Project_onboarding

(* === Moderator project-home review handlers
   (Project_home_review_handlers) ===
   GET /c/:slug/project-home-requests and the accept/reject POSTs: the
   settings navigation link, the DB-free access/origin/CSRF gates (every
   rejection precedes SQL — no sql_pool is installed in those harnesses),
   and the database-gated end-to-end queue and review flow over verified
   permanent projects, real communities, real durable top-mod/admin rows,
   and the transactional review store. Reserved external-installation-id
   range 946000001..946000999 (hence account ids 946100001..946100999,
   which also scope the permanent-project cleanup), phhr_% usernames, and
   phhr-% community slugs so no suite shares fixtures. Credential and
   privacy assertions are boolean, so no fixture byte reaches test output on
   failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Hr = Earde.Project_home_review_handlers

module Phrp = Earde.Project_home_review_pages

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

let request_pending = Community_fixture.request_pending

let make_accept ~mode ~load_config =
  Hr.make_project_home_accept_handler ~mode ~load_config

let make_reject ~mode ~load_config =
  Hr.make_project_home_reject_handler ~mode ~load_config

let queue_target slug = Printf.sprintf "/c/%s/project-home-requests" slug

let accept_target ~slug ~project =
  Printf.sprintf "/c/%s/projects/%s/accept" slug project

let reject_target ~slug ~project =
  Printf.sprintf "/c/%s/projects/%s/reject" slug project

(* --- Fixtures --- *)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 946100001 AND 946100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 946100001 AND 946100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 946000001 AND 946000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phhr-%'"
    ; "DELETE FROM users WHERE username LIKE 'phhr_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 946000001 AND 946000999"
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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun ~url conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () -> Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f ~url conn))

(* Durable authorization fixtures reused from the store suite: the real
   rows the read model and review store read. *)
let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" Community_fixture.q_insert_moderator
    (user, community, "top_mod")

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, role)

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag = exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

let remove_moderator conn ~user ~community =
  exec conn "remove mod" Community_fixture.q_remove_moderator (user, community)

let set_role conn ~user ~community role =
  exec conn "downgrade" Community_fixture.q_set_moderator_role (user, community, role)

let status_of_relation conn id = find conn "status" Home_review_fixture.q_status id

(* === DB-free: settings navigation link === *)

let settings_community : Earde.Db.community =
  { id = 4242; slug = "phhr-nav"; name = "Phhr Nav"; description = None;
    rules = None; avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = false; visibility = Earde.Db.Community_public;
    indexable = true; is_network_community = true;
    onboarding_state = Earde.Db.Community_published; discoverable = true }

(* The settings page renders a framework CSRF field, so it needs a live
   request under a secret + sessions pipeline; no SQL is touched. *)
let render_settings ~is_admin ~is_top_mod =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.community_settings_page ~is_admin ~is_top_mod
           ~open_reports_count:0 ~community:settings_community ~mods:[]
           ~banned_users:[] ~members:[] ~sections:[] ~channels:[] req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline
          (Dream.request ~method_:`GET ~target:"/c/phhr-nav/settings" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "settings renderer did not run"

let nav_link = "href='/c/phhr-nav/project-home-requests'"

let nav_cases =
  [ case "settings nav: top-mod and durable-admin surfaces expose the queue \
          link, canonically, with no id or count" (fun () ->
        let top_mod = render_settings ~is_admin:false ~is_top_mod:true in
        Alcotest.(check bool) "top_mod link present" true
          (Html_assert.contains top_mod nav_link);
        Alcotest.(check bool) "exact link text" true
          (Html_assert.contains top_mod ">Project home requests</a>");
        let admin = render_settings ~is_admin:true ~is_top_mod:false in
        Alcotest.(check bool) "admin link present" true
          (Html_assert.contains admin nav_link);
        (* Canonical slug only — never the community id, a count, a form,
           or script. The framework CSRF fields are dropped first: their
           random token bytes could otherwise spell the id by chance. *)
        Alcotest.(check bool) "no community id in link" false
          (Html_assert.contains (Html_assert.without_csrf_inputs top_mod) "4242");
        Alcotest.(check bool) "one queue link, not two" true
          (Html_assert.occurrences top_mod nav_link = 1))
  ; case "settings nav: an unauthorized (regular-mod) settings surface never \
          gains the link" (fun () ->
        let regular = render_settings ~is_admin:false ~is_top_mod:false in
        Alcotest.(check bool) "no queue link" false
          (Html_assert.contains regular nav_link);
        Alcotest.(check bool) "no queue label" false
          (Html_assert.contains regular "Project home requests"))
  ]

(* === DB-free: GET access gates === *)

let gate_slug = "phhr-any"

let gate_project = "phhr-proj"

let get_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:(queue_target gate_slug)
    (Home_review_fixture.make_queue ~mode)

let get_gate_cases =
  [ case "GET queue off: clean /bring redirect before params or SQL"
      (fun () ->
        Http_fixture.check_clean_redirect "off" "/bring"
          (Http_fixture.gate_response "off"
             (get_run ~session:Http_fixture.admin_session ~mode:Ob.Off ())))
  ; case "GET queue: anonymous and invalid sessions to /login" (fun () ->
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
  ; case "GET queue admins mode: non-admin to /bring; authorized reach the \
          no-route 404 with no SQL" (fun () ->
        Http_fixture.check_clean_redirect "non-admin" "/bring"
          (Http_fixture.gate_response "non-admin"
             (get_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ()));
        let continues label session mode =
          let response = Http_fixture.gate_response label (get_run ~session ~mode ()) in
          Alcotest.(check int) (label ^ ": generic 404") 404
            (status_of response)
        in
        continues "admin continues" Http_fixture.admin_session Ob.Admins;
        continues "public user continues" Http_fixture.logged_in Ob.Public)
  ]

(* === DB-free: POST access gates, configuration, and origin (both verbs) === *)

(* Route pattern + target + factory, parametrized over the accept and
   reject handlers so both get the same coverage. *)
let verbs =
  [ ("accept", make_accept, "/c/:slug/projects/:project_slug/accept",
     accept_target ~slug:gate_slug ~project:gate_project)
  ; ("reject", make_reject, "/c/:slug/projects/:project_slug/reject",
     reject_target ~slug:gate_slug ~project:gate_project)
  ]

let post_run ~make ~target ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target
    (make ~mode ~load_config)

let routed_post ~make ~pattern ~target ?session ?(headers = []) ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target
    (Dream.router
       [ Dream.post pattern (fun req -> make ~mode:Ob.Public ~load_config req) ])

let post_gate_cases =
  List.concat_map
    (fun (verb, make, pattern, target) ->
      [ case (verb ^ " off: clean /bring redirect, loader untouched")
          (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            let response =
              Http_fixture.gate_response "off"
                (post_run ~make ~target ~session:Http_fixture.admin_session ~mode:Ob.Off
                   ~load_config:loader ())
            in
            Http_fixture.check_clean_redirect "off" "/bring" response;
            Alcotest.(check int) "loader never called" 0 !calls)
      ; case
          (verb ^ ": anonymous and invalid sessions to /login, loader \
           untouched") (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            Http_fixture.check_clean_redirect "anonymous" "/login"
              (Http_fixture.gate_response "anonymous"
                 (post_run ~make ~target ~mode:Ob.Public ~load_config:loader ()));
            List.iter
              (fun raw ->
                Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
                  (Http_fixture.gate_response ("user_id " ^ raw)
                     (post_run ~make ~target
                        ~session:[ ("user_id", raw) ]
                        ~mode:Ob.Public ~load_config:loader ())))
              [ "not-a-number"; ""; "0"; "-3" ];
            Alcotest.(check int) "loader never called" 0 !calls)
      ; case
          (verb ^ " admins mode: non-admin to /bring before route or \
           configuration") (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            Http_fixture.check_clean_redirect "non-admin" "/bring"
              (Http_fixture.gate_response "non-admin"
                 (post_run ~make ~target ~session:Http_fixture.logged_in ~mode:Ob.Admins
                    ~load_config:loader ()));
            Alcotest.(check int) "loader never called" 0 !calls;
            (* Past the gates, the routerless harness has no route params:
               the defensive 404 answers before the configuration load. *)
            let continues label session mode =
              let loader, calls = counting_loader (ok_loader ()) in
              let response =
                Http_fixture.gate_response label
                  (post_run ~make ~target ~session ~mode ~load_config:loader ())
              in
              Alcotest.(check int) (label ^ ": defensive 404") 404
                (status_of response);
              Alcotest.(check int) (label ^ ": loader untouched") 0 !calls
            in
            continues "admin continues" Http_fixture.admin_session Ob.Admins;
            continues "public user continues" Http_fixture.logged_in Ob.Public)
      ; case (verb ^ ": configuration error is a generic 503, no form")
          (fun () ->
            let loader, calls =
              counting_loader (Github_fixture.gac_of_values ~origin:None ())
            in
            let response =
              Http_fixture.gate_response "config failure"
                (routed_post ~make ~pattern ~target ~session:Http_fixture.logged_in
                   ~headers:
                     [ ("Origin", "https://earde.com");
                       ("Content-Type", "application/x-www-form-urlencoded");
                     ]
                   ~load_config:loader ())
            in
            Alcotest.(check int) "503" 503 (status_of response);
            Alcotest.(check int) "loader called once" 1 !calls;
            let body = Lwt_main.run (Dream.body response) in
            List.iter
              (fun needle ->
                Alcotest.(check bool) ("no leak " ^ needle) false
                  (Html_assert.contains body needle))
              [ "EARDE_PUBLIC_ORIGIN"; "Missing"; "Invalid"; "public_origin" ])
      ; case (verb ^ " origin gate: exact policy, before any form parse")
          (fun () ->
            let origin_run label ?sec_fetch_site origin =
              let headers =
                (match origin with Some o -> [ ("Origin", o) ] | None -> [])
                @
                match sec_fetch_site with
                | Some v -> [ ("Sec-Fetch-Site", v) ]
                | None -> []
              in
              routed_post ~make ~pattern ~target ~session:Http_fixture.logged_in
                ~headers ~load_config:(fun () -> ok_loader ()) ()
              |> Http_fixture.gate_response label
            in
            let rejected label ?sec_fetch_site origin =
              Alcotest.(check int) (label ^ ": 403") 403
                (status_of (origin_run label ?sec_fetch_site origin))
            in
            (* Passing origin, the missing content type is the next
               rejection (400) — reaching it proves origin accepted. *)
            let accepted label ?sec_fetch_site origin =
              Alcotest.(check int) (label ^ ": passes origin") 400
                (status_of (origin_run label ?sec_fetch_site origin))
            in
            accepted "exact origin" (Some "https://earde.com");
            accepted "explicit default port" (Some "https://earde.com:443");
            accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
            rejected "cross-origin" (Some "https://evil.example");
            rejected "same-site subdomain" (Some "https://www.earde.com");
            rejected "wrong scheme" (Some "http://earde.com");
            rejected "null" (Some "null");
            rejected "mismatch beats metadata" ~sec_fetch_site:"same-origin"
              (Some "https://evil.example");
            rejected "no signals" None;
            rejected "cross-site" ~sec_fetch_site:"cross-site" None;
            let leak = origin_run "reflection" (Some "https://evil.example") in
            Alcotest.(check bool) "origin not reflected" false
              (Html_assert.contains (Lwt_main.run (Dream.body leak)) "evil.example"))
      ])
    verbs

(* === DB-free: POST CSRF and the zero-application-field rule (both verbs) === *)

let csrf_pipeline ~make ~pattern () =
  Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n" ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ ->
      Dream.router
        [ Dream.post pattern (fun r ->
              make ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) r) ]
        req

let csrf_post ~target ?cookie ?(content_type = true) pipeline fields =
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
         (Dream.request ~method_:`POST ~target ~headers
            (Http_fixture.form_body fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  Alcotest.(check int) (label ^ ": status") expected
    (status_of (Http_fixture.gate_response label result))

let csrf_cases =
  List.map
    (fun (verb, make, pattern, target) ->
      case
        (verb ^ " CSRF: Dream verification gates every submission; only a \
         zero-field verified form reaches the store")
        (fun () ->
          let pipeline = csrf_pipeline ~make ~pattern () in
          let cookie, fresh, expired = Http_fixture.mint_tokens "mint" pipeline in
          let post = csrf_post ~target in
          (* Every CSRF failure refuses the submission, but it is answered
             by reloading the reviewer-authorized queue with a fresh token
             rather than a terminal page — so each one reaches the read
             model's DB boundary and none reaches the store. A queue whose
             one-hour token outlived its fourteen-day session must not
             become permanently unactionable. *)
          Http_fixture.check_db_boundary "missing token re-renders"
            (post ~cookie pipeline []);
          Http_fixture.check_db_boundary "invalid token re-renders"
            (post ~cookie pipeline [ ("dream.csrf", "not-a-token") ]);
          Http_fixture.check_db_boundary "expired token re-renders"
            (post ~cookie pipeline [ ("dream.csrf", expired) ]);
          Http_fixture.check_db_boundary "duplicate tokens re-render"
            (post ~cookie pipeline
               [ ("dream.csrf", fresh); ("dream.csrf", fresh) ]);
          Http_fixture.check_db_boundary "wrong session re-renders"
            (post pipeline [ ("dream.csrf", fresh) ]);
          (* A wrong content type is still answered without any SQL. *)
          csrf_rejected "wrong content type" 400
            (post ~cookie ~content_type:false pipeline
               [ ("dream.csrf", fresh) ]);
          (* An unexpected application field is a generic 400 that never
             reaches the store (a direct response, not the DB boundary). *)
          csrf_rejected "unexpected decision field" 400
            (post ~cookie pipeline
               [ ("decision", "accept"); ("dream.csrf", fresh) ]);
          csrf_rejected "unexpected id field" 400
            (post ~cookie pipeline
               [ ("relation_id", "1"); ("dream.csrf", fresh) ]);
          (* A verified token with no application field reaches the DB
             boundary: CSRF passed, Dream stripped its own field, and the
             store call is the next thing to run. *)
          Http_fixture.check_db_boundary "empty verified form continues"
            (post ~cookie pipeline [ ("dream.csrf", fresh) ]))
      )
    verbs

(* === Database-gated: the real queue and review flow === *)

(* Production shape: sql_pool + secret + memory sessions + the real router
   paths, so all three handlers read :slug / :project_slug exactly as in
   bin/main. /mint hands replay cases a fresh CSRF token for an
   established session. is_admin is set into the session only when asked —
   durable admin authorization is the users.is_admin column, never the
   session flag. *)
let app_pipeline ?session_user_id ?(session_admin = false) ~url () =
  Dream.sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ (fun handler request ->
       match session_user_id with
       | None -> handler request
       | Some uid ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           let* () =
             if session_admin then
               Dream.set_session_field request "is_admin" "true"
             else Lwt.return_unit
           in
           handler request)
  @@ Dream.router
       [ Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.get "/c/:slug/project-home-requests" (fun req ->
             Home_review_fixture.make_queue ~mode:Ob.Public req);
         Dream.post "/c/:slug/projects/:project_slug/accept" (fun req ->
             make_accept ~mode:Ob.Public
               ~load_config:(fun () -> ok_loader ())
               req);
         Dream.post "/c/:slug/projects/:project_slug/reject" (fun req ->
             make_reject ~mode:Ob.Public
               ~load_config:(fun () -> ok_loader ())
               req);
       ]

let get_queue ?cookie ~slug pipeline =
  Http_fixture.do_get ?cookie ~target:(queue_target slug) pipeline

(* One GET that opens the queue: page, session cookie, and a CSRF token
   for the follow-up POST (either form's token is session-bound, so the
   first serves both routes). *)
let open_queue label ~slug pipeline =
  let* response, body = get_queue ~slug pipeline in
  Http_fixture.check_page label response;
  let cookie = Http_fixture.session_cookie label response in
  let token = Http_fixture.csrf_of_page label body in
  Lwt.return (cookie, token, body)

let do_post ?(origin = Some "https://earde.com") ~cookie ~target ~token
    pipeline =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [ ("Content-Type", "application/x-www-form-urlencoded");
        ("Cookie", cookie);
      ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target ~headers
       (Http_fixture.form_body [ ("dream.csrf", token) ]))

(* === GET authorization === *)

let get_authz_case =
  db_case "GET queue: top-mods and durable admins see it; every other \
           identity is one generic 404" (fun ~url conn ->
      let* owner = insert_user conn "phhr_owner" in
      let* top1 = insert_user conn "phhr_top1" in
      let* top2 = insert_user conn "phhr_top2" in
      let* admin = insert_user conn "phhr_admin" in
      let* member = insert_user conn "phhr_member" in
      let* stranger = insert_user conn "phhr_stranger" in
      let* modu = insert_user conn "phhr_mod" in
      let* legacy = insert_user conn "phhr_legacy" in
      let* othermod = insert_user conn "phhr_othermod" in
      let* removed = insert_user conn "phhr_removed" in
      let* sonly = insert_user conn "phhr_sonly" in
      let* _, _ =
        make_project conn ~user:owner ~ext_id:946000001L ~slug:"phhr-authz"
      in
      let* cid = insert_community ~name:"Phhr Authz Home" conn "phhr-authz-home" in
      let* other = insert_community conn "phhr-authz-other" in
      let* _rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-authz"
          ~community:cid ~note:"phhr authz note" ()
      in
      let* () = add_top_mod conn ~user:top1 ~community:cid in
      let* () = add_top_mod conn ~user:top2 ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* () = add_member conn ~user:member ~community:cid in
      let* () = add_role conn ~user:modu ~community:cid "mod" in
      let* () = add_role conn ~user:legacy ~community:cid "legacy_mod" in
      let* () = add_top_mod conn ~user:othermod ~community:other in
      let* () = add_top_mod conn ~user:removed ~community:cid in
      let* () = remove_moderator conn ~user:removed ~community:cid in
      let sees label uid ~admin_session =
        let pipeline =
          app_pipeline ~session_user_id:uid ~session_admin:admin_session ~url ()
        in
        let* response, body = get_queue ~slug:"phhr-authz-home" pipeline in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        let frag = Html_assert.panel_fragment body in
        Html_assert.must frag "Project home requests";
        Html_assert.must frag "Pfin Fixture Project";
        Lwt.return_unit
      in
      let denied label uid ~admin_session ~slug =
        let pipeline =
          app_pipeline ~session_user_id:uid ~session_admin:admin_session ~url ()
        in
        let* response, body = get_queue ~slug pipeline in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        Html_assert.must body "This page does not exist.";
        Html_assert.must_not body "Pfin Fixture Project";
        Html_assert.must_not body "phhr authz note";
        Lwt.return_unit
      in
      let* () = sees "top mod" top1 ~admin_session:false in
      let* () = sees "second top mod" top2 ~admin_session:false in
      let* () = sees "durable admin" admin ~admin_session:false in
      let* () = denied "ordinary member" member ~admin_session:false ~slug:"phhr-authz-home" in
      let* () = denied "non-member" stranger ~admin_session:false ~slug:"phhr-authz-home" in
      let* () = denied "mod role" modu ~admin_session:false ~slug:"phhr-authz-home" in
      let* () = denied "legacy_mod role" legacy ~admin_session:false ~slug:"phhr-authz-home" in
      let* () = denied "other-community mod" othermod ~admin_session:false ~slug:"phhr-authz-home" in
      let* () = denied "removed top mod" removed ~admin_session:false ~slug:"phhr-authz-home" in
      (* Session-only admin claim without the durable flag. *)
      let* () = denied "session-only admin" sonly ~admin_session:true ~slug:"phhr-authz-home" in
      (* Missing community collapses identically. *)
      denied "missing community" top1 ~admin_session:false ~slug:"phhr-authz-absent")

(* === GET states === *)

let get_states_case =
  db_case_lifecycle_relaxed "GET queue: empty, populated, note escaped, verification copy, \
           ineligible warning, closed rows absent, no ids" (fun ~url conn ->
      let* owner = insert_user conn "phhr_sowner" in
      let* reviewer = insert_user conn "phhr_smod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000010L ~slug:"phhr-state"
      in
      let* other_owner = insert_user conn "phhr_sowner2" in
      let* _, _ =
        make_project conn ~user:other_owner ~ext_id:946000011L
          ~slug:"phhr-state-b"
      in
      let* cid = insert_community ~name:"Phhr State Home" conn "phhr-state-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      (* Empty queue. *)
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "empty" response;
      Alcotest.(check bool) "noindex" true (Html_assert.contains body "noindex");
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "No pending project requests.";
      Alcotest.(check int) "no forms when empty" 0 (Html_assert.occurrences frag "<form");
      (* Two pending requests; a private note on the first. *)
      let* rid =
        request_pending "pending a" conn ~user:owner ~slug:"phhr-state"
          ~community:cid ~note:"<b>phhr-secret-note</b>" ()
      in
      let* _rid2 =
        request_pending "pending b" conn ~user:other_owner
          ~slug:"phhr-state-b" ~community:cid ()
      in
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "populated" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "action='/c/phhr-state-home/projects/phhr-state/accept'";
      Html_assert.must frag "action='/c/phhr-state-home/projects/phhr-state/reject'";
      Html_assert.must frag "action='/c/phhr-state-home/projects/phhr-state-b/accept'";
      Html_assert.must frag "Accept as community home";
      Html_assert.must frag "Reject request";
      (* The private note is escaped, never raw markup. *)
      Html_assert.must frag "phhr-secret-note";
      Html_assert.must frag "&lt;b&gt;phhr-secret-note&lt;/b&gt;";
      Html_assert.must_not frag "<b>phhr-secret-note</b>";
      Html_assert.must frag "phrv-verification'>Verified<";
      (* No internal ids leak. *)
      Html_assert.must_not frag (Int64.to_string rid);
      Html_assert.must_not frag (Int64.to_string project);
      Html_assert.must_not frag (string_of_int cid);
      (* Stale then revoked: acceptance suppressed, rejection stays. *)
      let* () = exec conn "stale" Home_request_fixture.q_set_verification (project, "stale") in
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "stale" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "Verification stale";
      Html_assert.must_not frag
        "action='/c/phhr-state-home/projects/phhr-state/accept'";
      Html_assert.must frag "action='/c/phhr-state-home/projects/phhr-state/reject'";
      Html_assert.must frag "Acceptance is unavailable for this request.";
      let* () = exec conn "revoked" Home_request_fixture.q_set_verification (project, "revoked") in
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "revoked" response;
      Html_assert.must (Html_assert.panel_fragment body) "Verification revoked";
      let* () = exec conn "reverify" Home_request_fixture.q_set_verification (project, "verified") in
      (* Ineligible community: page-level warning; every accept suppressed;
         reject stays. *)
      let* () = exec conn "make private" Community_fixture.q_make_private cid in
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "ineligible" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "This community cannot currently accept project-home requests.";
      Html_assert.must_not frag
        "action='/c/phhr-state-home/projects/phhr-state/accept'";
      Html_assert.must frag "action='/c/phhr-state-home/projects/phhr-state/reject'";
      Html_assert.must_not frag "private";
      let* () = exec conn "restore" Home_review_fixture.q_make_eligible cid in
      (* Accepted and rejected rows drop out of the queue entirely. *)
      let* () = exec conn "accept fixture" Home_request_fixture.q_mark_accepted rid in
      let* response, body = get_queue ~slug:"phhr-state-home" pipeline in
      Http_fixture.check_page "after accept" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must_not frag "action='/c/phhr-state-home/projects/phhr-state/accept'";
      Html_assert.must_not frag "action='/c/phhr-state-home/projects/phhr-state/reject'";
      Html_assert.must_not frag "phhr-secret-note";
      Lwt.return_unit)

let credential_needles =
  [ ("access token", Project_fixture.pods_access_fixture)
  ; ("refresh token", Project_fixture.pods_refresh_fixture)
  ; ("private repository name", Github_fixture.gur_private_name)
  ; ("private repository description", Github_fixture.gur_private_description)
  ]

(* === POST accept: PRG, durable review, replay, privacy === *)

let accept_prg_case =
  db_case "POST accept: PRG, pending disappears, relation accepted, exact \
           reviewer, replay is 409, no side effects" (fun ~url conn ->
      let* owner = insert_user conn "phhr_aowner" in
      let* reviewer = insert_user conn "phhr_amod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000020L ~slug:"phhr-accept"
      in
      let* cid = insert_community ~name:"Phhr Accept Home" conn "phhr-accept-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-accept"
          ~community:cid ~note:"phhr accept note" ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _body = open_queue "queue" ~slug:"phhr-accept-home" pipeline in
      let target = accept_target ~slug:"phhr-accept-home" ~project:"phhr-accept" in
      let* response = do_post ~cookie ~target ~token pipeline in
      let* () =
        Http_fixture.check_redirect_lwt "accepted"
          "/c/phhr-accept-home/project-home-requests" response
      in
      let redirect_cookies =
        String.concat "|" (Dream.headers response "Set-Cookie")
      in
      (* Exactly one relation, now accepted with the exact reviewer and a
         coherent review timestamp; note and requester preserved. *)
      let* ( ((_, rc), (rtype, status)), ((req, rev), (note, _)) ) =
        find conn "relation row" Home_request_fixture.q_relation_row rid
      in
      Alcotest.(check int) "same community" cid rc;
      Alcotest.(check string) "relation home" "home" rtype;
      Alcotest.(check string) "status accepted" "accepted" status;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option int)) "reviewer stored" (Some reviewer) rev;
      Alcotest.(check (option string)) "note preserved" (Some "phhr accept note") note;
      let* present, ordered, _ = find conn "review times" Home_review_fixture.q_review_times rid in
      Alcotest.(check bool) "reviewed_at present" true present;
      Alcotest.(check bool) "reviewed_at >= created_at" true ordered;
      (* No membership, moderator, or stewardship change from acceptance. *)
      let* members = find conn "members" Home_request_fixture.q_count_members cid in
      Alcotest.(check int) "no new member" 0 members;
      let* mods = find conn "moderators" Home_request_fixture.q_count_moderators cid in
      Alcotest.(check int) "only the one reviewer mod" 1 mods;
      let* stewards = find conn "stewards" Home_request_fixture.q_count_stewards_for_project project in
      Alcotest.(check int) "stewardship unchanged" 1 stewards;
      (* Redirected GET: the reviewed request is gone. *)
      let* response, body = get_queue ~cookie ~slug:"phhr-accept-home" pipeline in
      Http_fixture.check_page "after PRG" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must_not frag "action='/c/phhr-accept-home/projects/phhr-accept/accept'";
      Html_assert.must_not body "phhr accept note";
      Html_assert.must_not frag (Int64.to_string rid);
      (* No credential fixture rides in the page or redirect cookies. *)
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool) (label ^ " absent from page") false
            (Html_assert.contains_nonempty ~needle body);
          Alcotest.(check bool) (label ^ " absent from cookies") false
            (Html_assert.contains_nonempty ~needle redirect_cookies))
        credential_needles;
      (* Replay with a fresh CSRF token: 409 authoritative queue, no second
         review, exactly one durable relation. *)
      let* fresh = Http_fixture.mint_token "replay mint" ~cookie pipeline in
      let* replay = do_post ~cookie ~target ~token:fresh pipeline in
      Alcotest.(check int) "replay 409" 409 (status_of replay);
      Alcotest.(check (option string)) "replay no-store" (Some "no-store")
        (Dream.header replay "Cache-Control");
      Alcotest.(check (option string)) "replay no redirect" None
        (Dream.header replay "Location");
      let* rbody = Dream.body replay in
      Html_assert.must rbody "That request is no longer pending.";
      let* status2 = status_of_relation conn rid in
      Alcotest.(check string) "still accepted" "accepted" status2;
      let* active = find conn "active" Home_request_fixture.q_count_active_for_project project in
      Alcotest.(check int) "one active row" 1 active;
      Lwt.return_unit)

(* === POST reject: PRG frees the slot === *)

let reject_prg_case =
  db_case "POST reject: PRG, relation rejected, slot freed, requester/note \
           and verification preserved" (fun ~url conn ->
      let* owner = insert_user conn "phhr_rowner" in
      let* reviewer = insert_user conn "phhr_rmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000030L ~slug:"phhr-reject"
      in
      let* cid = insert_community ~name:"Phhr Reject Home" conn "phhr-reject-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-reject"
          ~community:cid ~note:"phhr reject note" ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _body = open_queue "queue" ~slug:"phhr-reject-home" pipeline in
      let target = reject_target ~slug:"phhr-reject-home" ~project:"phhr-reject" in
      let* response = do_post ~cookie ~target ~token pipeline in
      let* () =
        Http_fixture.check_redirect_lwt "rejected"
          "/c/phhr-reject-home/project-home-requests" response
      in
      let* ( ((_, _), (_, status)), ((req, rev), (note, _)) ) =
        find conn "relation row" Home_request_fixture.q_relation_row rid
      in
      Alcotest.(check string) "status rejected" "rejected" status;
      Alcotest.(check (option int)) "requester preserved" (Some owner) req;
      Alcotest.(check (option int)) "reviewer stored" (Some reviewer) rev;
      Alcotest.(check (option string)) "note preserved" (Some "phhr reject note") note;
      (* The active-home slot is free again. *)
      let* active = find conn "active" Home_request_fixture.q_count_active_for_project project in
      Alcotest.(check int) "no active row" 0 active;
      (* Project verification unchanged by a rejection. *)
      let* psig = find conn "project sig" Home_request_fixture.q_project_sig project in
      Alcotest.(check bool) "still verified" true (Html_assert.contains psig "verified");
      (* Redirected GET no longer lists it. *)
      let* response, body = get_queue ~cookie ~slug:"phhr-reject-home" pipeline in
      Http_fixture.check_page "after PRG" response;
      Html_assert.must_not (Html_assert.panel_fragment body)
        "action='/c/phhr-reject-home/projects/phhr-reject/reject'";
      ignore rid;
      Lwt.return_unit)

(* === Accept stale/revoked project: 409 Project_unavailable === *)

let accept_stale_case =
  db_case "POST accept: project gone stale after GET is 409 \
           Project_unavailable; request stays; reject still works"
    (fun ~url conn ->
      let* owner = insert_user conn "phhr_stowner" in
      let* reviewer = insert_user conn "phhr_stmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000040L ~slug:"phhr-stale"
      in
      let* cid = insert_community ~name:"Phhr Stale Home" conn "phhr-stale-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-stale"
          ~community:cid ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _ = open_queue "queue" ~slug:"phhr-stale-home" pipeline in
      (* Verification drifts to stale between GET and POST. *)
      let* () = exec conn "stale" Home_request_fixture.q_set_verification (project, "stale") in
      let target = accept_target ~slug:"phhr-stale-home" ~project:"phhr-stale" in
      let* response = do_post ~cookie ~target ~token pipeline in
      Alcotest.(check int) "409" 409 (status_of response);
      Alcotest.(check (option string)) "no redirect" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      Html_assert.must body "That project is no longer available for acceptance.";
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "Verification stale";
      Html_assert.must_not frag "action='/c/phhr-stale-home/projects/phhr-stale/accept'";
      Html_assert.must frag "action='/c/phhr-stale-home/projects/phhr-stale/reject'";
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      (* Rejection of a stale project still succeeds. *)
      let* fresh = Http_fixture.mint_token "reject mint" ~cookie pipeline in
      let rtarget = reject_target ~slug:"phhr-stale-home" ~project:"phhr-stale" in
      let* response = do_post ~cookie ~target:rtarget ~token:fresh pipeline in
      let* () =
        Http_fixture.check_redirect_lwt "reject stale"
          "/c/phhr-stale-home/project-home-requests" response
      in
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "now rejected" "rejected" status;
      Lwt.return_unit)

(* === Target becomes ineligible: accept 409 Target_ineligible, reject ok === *)

let target_ineligible_case =
  db_case_lifecycle_relaxed "POST accept: target ineligible after GET is 409 Target_ineligible \
           with no reason leak; reject then succeeds" (fun ~url conn ->
      let* owner = insert_user conn "phhr_tiowner" in
      let* reviewer = insert_user conn "phhr_timod" in
      let* _, _ =
        make_project conn ~user:owner ~ext_id:946000050L ~slug:"phhr-ti"
      in
      let* cid = insert_community ~name:"Phhr TI Home" conn "phhr-ti-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-ti"
          ~community:cid ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _ = open_queue "queue" ~slug:"phhr-ti-home" pipeline in
      (* Community goes private (a valid but ineligible host) after GET. *)
      let* () = exec conn "private" Community_fixture.q_make_private cid in
      let target = accept_target ~slug:"phhr-ti-home" ~project:"phhr-ti" in
      let* response = do_post ~cookie ~target ~token pipeline in
      Alcotest.(check int) "409" 409 (status_of response);
      let* body = Dream.body response in
      Html_assert.must body "This community cannot currently accept that project.";
      (* The specific ineligibility reason is never named. *)
      Html_assert.must_not body "private";
      Html_assert.must_not body "draft";
      Html_assert.must_not body "legacy";
      let frag = Html_assert.panel_fragment body in
      Html_assert.must_not frag "action='/c/phhr-ti-home/projects/phhr-ti/accept'";
      Html_assert.must frag "action='/c/phhr-ti-home/projects/phhr-ti/reject'";
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      (* Reject stays available on an ineligible target. *)
      let* fresh = Http_fixture.mint_token "reject mint" ~cookie pipeline in
      let rtarget = reject_target ~slug:"phhr-ti-home" ~project:"phhr-ti" in
      let* response = do_post ~cookie ~target:rtarget ~token:fresh pipeline in
      let* () =
        Http_fixture.check_redirect_lwt "reject ineligible"
          "/c/phhr-ti-home/project-home-requests" response
      in
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "now rejected" "rejected" status;
      Lwt.return_unit)

(* === Concurrency by replay: accept-vs-reject, exactly one durable review === *)

let concurrency_case =
  db_case "POST accept then reject: one winner, loser is 409, exactly one \
           durable review" (fun ~url conn ->
      let* owner = insert_user conn "phhr_cowner" in
      let* reviewer = insert_user conn "phhr_cmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000060L ~slug:"phhr-conc"
      in
      let* cid = insert_community ~name:"Phhr Conc Home" conn "phhr-conc-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-conc"
          ~community:cid ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _ = open_queue "queue" ~slug:"phhr-conc-home" pipeline in
      (* Accept wins. *)
      let atarget = accept_target ~slug:"phhr-conc-home" ~project:"phhr-conc" in
      let* response = do_post ~cookie ~target:atarget ~token pipeline in
      let* () =
        Http_fixture.check_redirect_lwt "accept wins"
          "/c/phhr-conc-home/project-home-requests" response
      in
      (* A follow-up reject (fresh token) loses: no pending row remains. *)
      let* fresh = Http_fixture.mint_token "reject mint" ~cookie pipeline in
      let rtarget = reject_target ~slug:"phhr-conc-home" ~project:"phhr-conc" in
      let* response = do_post ~cookie ~target:rtarget ~token:fresh pipeline in
      Alcotest.(check int) "loser 409" 409 (status_of response);
      let* body = Dream.body response in
      Html_assert.must body "That request is no longer pending.";
      (* Exactly one relation, accepted, one active slot. *)
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "final accepted" "accepted" status;
      let* total = find conn "total" Home_request_fixture.q_count_for_project project in
      Alcotest.(check int) "no duplicate relation" 1 total;
      let* active = find conn "active" Home_request_fixture.q_count_active_for_project project in
      Alcotest.(check int) "one active" 1 active;
      Lwt.return_unit)

(* === Authorization lost between GET and POST === *)

let authz_loss_case =
  db_case "POST: top-mod removed or admin revoked between GET and POST is a \
           generic 404 with no review" (fun ~url conn ->
      let* owner = insert_user conn "phhr_lowner" in
      let* top = insert_user conn "phhr_ltop" in
      let* admin = insert_user conn "phhr_ladmin" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000070L ~slug:"phhr-loss"
      in
      let* cid = insert_community ~name:"Phhr Loss Home" conn "phhr-loss-home" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-loss"
          ~community:cid ()
      in
      let target = accept_target ~slug:"phhr-loss-home" ~project:"phhr-loss" in
      (* Top-mod downgraded to mod after opening the queue. *)
      let tpipe = app_pipeline ~session_user_id:top ~url () in
      let* cookie, token, _ = open_queue "top queue" ~slug:"phhr-loss-home" tpipe in
      let* () = set_role conn ~user:top ~community:cid "mod" in
      let* response = do_post ~cookie ~target ~token tpipe in
      Alcotest.(check int) "downgraded top mod 404" 404 (status_of response);
      Alcotest.(check (option string)) "no redirect" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      Html_assert.must body "This page does not exist.";
      Html_assert.must_not body "moderator";
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "still pending after downgrade" "pending" status;
      (* Durable admin revoked after opening the queue. *)
      let apipe = app_pipeline ~session_user_id:admin ~url () in
      let* cookie, token, _ = open_queue "admin queue" ~slug:"phhr-loss-home" apipe in
      let* () = set_admin conn ~user:admin false in
      let* response = do_post ~cookie ~target ~token apipe in
      Alcotest.(check int) "revoked admin 404" 404 (status_of response);
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "still pending after revoke" "pending" status;
      ignore project;
      Lwt.return_unit)

(* === POST security: origin precedes form; empty-form rule is durable === *)

let post_security_case =
  db_case "POST security: cross-origin rejected before the form; an \
           unexpected field never reaches the store" (fun ~url conn ->
      let* owner = insert_user conn "phhr_psowner" in
      let* reviewer = insert_user conn "phhr_psmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000080L ~slug:"phhr-sec"
      in
      let* cid = insert_community ~name:"Phhr Sec Home" conn "phhr-sec-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-sec"
          ~community:cid ()
      in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* cookie, token, _ = open_queue "queue" ~slug:"phhr-sec-home" pipeline in
      let target = accept_target ~slug:"phhr-sec-home" ~project:"phhr-sec" in
      (* Cross-origin is a 403 before any form parse; no review. *)
      let* response =
        do_post ~origin:(Some "https://evil.example") ~cookie ~target ~token
          pipeline
      in
      Alcotest.(check int) "cross-origin 403" 403 (status_of response);
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "no review after origin reject" "pending" status;
      (* An unexpected application field is a 400 that never reviews. *)
      let headers =
        [ ("Origin", "https://earde.com");
          ("Content-Type", "application/x-www-form-urlencoded");
          ("Cookie", cookie);
        ]
      in
      let* response =
        pipeline
          (Dream.request ~method_:`POST ~target ~headers
             (Http_fixture.form_body [ ("decision", "accept"); ("dream.csrf", token) ]))
      in
      Alcotest.(check int) "unexpected field 400" 400 (status_of response);
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "no review after 400" "pending" status;
      ignore project;
      Lwt.return_unit)

(* === Storage and inconsistency collapse to a payload-free 500 === *)

let get_storage_case =
  db_case "GET queue: a real database failure collapses to the generic 500"
    (fun ~url _conn ->
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=phhr_void"))
      in
      let pipeline = app_pipeline ~session_user_id:42 ~url:poisoned () in
      let* response, body = get_queue ~slug:"phhr-anything" pipeline in
      Alcotest.(check int) "500" 500 (status_of response);
      Alcotest.(check (option string)) "no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      Html_assert.must body "Something went wrong on our side.";
      List.iter (Html_assert.must_not body)
        [ "phhr_void"; "search_path"; "PostgreSQL"; "Caqti"; "relation" ];
      Lwt.return_unit)

let get_inconsistent_case =
  db_case_lifecycle_relaxed "GET queue: durable community corruption is one generic 500"
    (fun ~url conn ->
      let* reviewer = insert_user conn "phhr_icmod" in
      (* Published public network community with indexable <> discoverable:
         a leaking shape the read model reports as corruption. *)
      let* cid =
        insert_community ~indexable:true ~discoverable:false conn "phhr-ic"
      in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let pipeline = app_pipeline ~session_user_id:reviewer ~url () in
      let* response, body = get_queue ~slug:"phhr-ic" pipeline in
      Alcotest.(check int) "500" 500 (status_of response);
      Html_assert.must body "Something went wrong on our side.";
      List.iter (Html_assert.must_not body)
        [ "Inconsistent"; "indexable"; "discoverable"; "Caqti" ];
      Lwt.return_unit)

let post_storage_case =
  db_case "POST accept: a review-store database failure is the generic 500, \
           no partial review" (fun ~url conn ->
      let* owner = insert_user conn "phhr_pstowner" in
      let* reviewer = insert_user conn "phhr_pstmod" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:946000090L ~slug:"phhr-pst"
      in
      let* cid = insert_community ~name:"Phhr Pst Home" conn "phhr-pst-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_pending "pending" conn ~user:owner ~slug:"phhr-pst"
          ~community:cid ()
      in
      (* One poisoned pipeline throughout: /mint touches no SQL (it only
         mints a session-bound CSRF token), so the session cookie and
         token establish here, and the accept POST then fails only inside
         the review store's SQL — the handler collapses it payload-free. *)
      let poisoned =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=phhr_void"))
      in
      let poison_pipe = app_pipeline ~session_user_id:reviewer ~url:poisoned () in
      let* mint_response, mint_body = Http_fixture.do_get ~target:"/mint" poison_pipe in
      Alcotest.(check int) "mint 200" 200 (status_of mint_response);
      let cookie = Http_fixture.session_cookie "poison mint" mint_response in
      let token = mint_body in
      let target = accept_target ~slug:"phhr-pst-home" ~project:"phhr-pst" in
      let* response = do_post ~cookie ~target ~token poison_pipe in
      Alcotest.(check int) "500" 500 (status_of response);
      Alcotest.(check (option string)) "no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      let* body = Dream.body response in
      Html_assert.must body "Something went wrong on our side.";
      List.iter (Html_assert.must_not body)
        [ "phhr_void"; "search_path"; "Caqti"; "PostgreSQL" ];
      (* No partial review committed. *)
      let* status = status_of_relation conn rid in
      Alcotest.(check string) "still pending" "pending" status;
      ignore project;
      Lwt.return_unit)

(* === Suites === *)

let nav_suite = nav_cases

let get_gate_suite = get_gate_cases

let post_gate_suite = post_gate_cases

let csrf_suite = csrf_cases

let db_suite =
  [ get_authz_case; get_states_case; accept_prg_case; reject_prg_case;
    accept_stale_case; target_ineligible_case; concurrency_case;
    authz_loss_case; post_security_case; get_storage_case;
    get_inconsistent_case; post_storage_case ]

let suites =
    (* Moderator review handlers: the settings navigation link, DB-free
       access/origin/CSRF gates for the queue GET and both mutation POSTs
       (all rejections precede SQL), and the database-gated authorization,
       queue states, accept/reject PRG, drifted-project and ineligible-
       target 409s, replay/concurrency, authorization loss, POST security,
       and storage/inconsistency collapse. *)
  [ ("project_home_review_settings_nav", nav_suite)
  ; ("project_home_review_get_gates", get_gate_suite)
  ; ("project_home_review_post_gates", post_gate_suite)
  ; ("project_home_review_post_csrf", csrf_suite)
  ; ("project_home_review_handlers_db", db_suite)
  ]
