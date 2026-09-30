module Ob = Earde.Project_onboarding

(* === GITHUB PROJECT AND COMMUNITY-HOME ANALYTICS FUNNEL (handlers) ===
   The eight post-installation funnel events, driven through the real
   handlers and the real production stores over a real Dream pipeline
   (sql_pool + secret + memory sessions + the real router shape and route
   patterns main.ml registers), with the fake capture sink replacing the
   PostHog HTTP transport: no request can leave the process, and capture
   becomes synchronous, so "exactly one event" is an exact assertion rather
   than a race. The two GitHub-installation events are covered where their
   harnesses already live (Gh_start_handler, Gh_oauth_callback).

   The authenticated-mutation rate limiter main.ml wraps these POSTs in is
   deliberately NOT installed here: it is orthogonal to the capture
   boundary, has its own coverage, and would otherwise arbitrate the
   concurrency cases instead of the stores under test.

   Reserved so no suite shares fixtures: external installation ids
   962000001..962000999 (hence namespace ids 962100001..962100999, which
   also scope the permanent-project cleanup), anfn_% usernames, and anfn-%
   community and project slugs.

   Every assertion about a submitted or durable value is boolean, and
   failures report event NAMES only — no fixture byte and no captured body
   reaches test output. Database-gated (EARDE_TEST_DATABASE_URL, the same
   opt-in as Mod_scope). *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Fin = Earde.Project_finalization_store

module Phr = Earde.Project_home_relation

module Rq = Earde.Project_home_request_store

module Rvs = Earde.Project_home_review_store

module Psh = Earde.Project_setup_handlers

module Pc = Earde.Project_creation_handlers

module Prh = Earde.Project_home_request_handlers

module Hrv = Earde.Project_home_review_handlers

module Pvh = Earde.Project_home_provisioning_handlers

module Rmh = Earde.Project_home_removal_handlers

module Ncph = Earde.Network_community_publication_handlers

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let ok_loader = Http_fixture.ok_loader

let status_of = Http_fixture.status_of

let q_insert_moderator = Community_fixture.q_insert_moderator

(* Notifications and audit events must go before projects and communities
   (audit RESTRICT-protects both), then the shared dependency order of the
   sibling suites. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 962100001 AND 962100999)"
    ; "DELETE FROM notifications \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'anfn-%')"
    ; "DELETE FROM notifications \
       WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'anfn_%')"
    ; "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 962100001 AND 962100999)"
    ; "DELETE FROM project_home_audit_events \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'anfn-%')"
    ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 962100001 AND 962100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 962000001 AND 962000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'anfn-%'"
    ; "DELETE FROM users WHERE username LIKE 'anfn_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 962000001 AND 962000999"
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

(* === Fixtures: only through the real chain === *)

let make_project = Home_provisioning_fixture.make_project

let make_home_draft = Network_community_fixture.make_draft

let q_relation_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM community_projects WHERE project_id = $1 \
   ORDER BY id DESC LIMIT 1"

let q_relation_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_projects WHERE project_id = $1"

let q_project_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM open_source_projects WHERE forge_namespace_id = $1"

let q_project_id_of_slug =
  (Caqti_type.string ->! Caqti_type.int64)
  "SELECT id FROM open_source_projects WHERE slug = $1"

let q_community_state =
  (Caqti_type.string ->! Caqti_type.(t2 string string))
  "SELECT onboarding_state, visibility FROM communities WHERE slug = $1"

let q_community_count =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*) FROM communities WHERE slug = $1"

let pending_request conn ~user ~slug ~community ?(note = None) () =
  let relation =
    match Phr.create_pending ~request_note:note with
    | Ok relation -> relation
    | Error _ -> Alcotest.fail "pending relation fixture rejected"
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "pending request fixture failed for %s" slug

let accept_request conn ~reviewer ~project_slug ~community_slug =
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug
      ~target_community_slug:community_slug ~decision:Rvs.Accept
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ ->
      Alcotest.failf "accept fixture failed for %s" project_slug

(* === The real pipeline ===

   One shared two-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, so a fresh pool per request would exhaust
   Postgres max_connections; two connections are the minimum that lets the
   concurrency cases run two requests genuinely at once. Session identity
   is sticky: a request that already carries a session keeps its own user,
   so independent cookies stay independent under concurrency. *)
let shared_identity : int option ref = ref None

let shared_pipeline = ref None

let identity_middleware handler request =
  match Dream.session_field request "user_id" with
  | Some _ -> handler request
  | None -> (
      match !shared_identity with
      | None -> handler request
      | Some uid ->
          let* () =
            Dream.set_session_field request "user_id" (string_of_int uid)
          in
          let* () =
            Dream.set_session_field request "username"
              ("anfn_user_" ^ string_of_int uid)
          in
          handler request)

let mode = Ob.Public

let config () = ok_loader ()

let build_pipeline ~url =
  Dream.sql_pool ~size:2 url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions @@ identity_middleware
  @@ Dream.router
       [ Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.post "/projects/new/repositories" (fun req ->
             Psh.make_repository_selection_handler ~mode ~load_config:config
               req);
         Dream.post "/projects" (fun req ->
             Pc.make_project_creation_handler ~mode ~load_config:config req);
         Dream.post "/projects/:slug/request-home" (fun req ->
             Prh.make_project_home_request_handler ~mode ~load_config:config
               req);
         Dream.post "/projects/:slug/community-home" (fun req ->
             Pvh.make_project_home_provisioning_handler ~mode
               ~load_config:config req);
         Dream.post "/c/:slug/projects/:project_slug/accept" (fun req ->
             Hrv.make_project_home_accept_handler ~mode ~load_config:config
               req);
         Dream.post "/c/:slug/projects/:project_slug/reject" (fun req ->
             Hrv.make_project_home_reject_handler ~mode ~load_config:config
               req);
         Dream.post
           "/projects/:project_slug/community-home/:community_slug/remove"
           (fun req ->
             Rmh.make_project_side_home_removal_handler ~mode
               ~load_config:config req);
         Dream.post "/c/:community_slug/projects/:project_slug/remove-home"
           (fun req ->
             Rmh.make_community_side_home_removal_handler ~mode
               ~load_config:config req);
         Dream.post "/c/:slug/publish" (fun req ->
             Ncph.make_network_community_publication_handler ~mode
               ~load_config:config req)
       ]

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline = build_pipeline ~url in
      shared_pipeline := Some pipeline;
      pipeline

let do_get ?cookie ~url ~target () =
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = (pipeline_for ~url) (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* Every POST carries the plaintext consent cookie beside the session
   cookie unless the caller overrides it, exactly as a consenting browser
   would. *)
let do_post ?(consent = [ Analytics_fixture.an_consent_granted ]) ~url ~cookie ~target ~token
    ~body_fields () =
  let headers =
    [ ("Origin", "https://earde.com");
      ("Content-Type", "application/x-www-form-urlencoded");
      ( "Cookie",
        String.concat "; "
          (cookie :: List.map (fun (n, v) -> n ^ "=" ^ v) consent) )
    ]
  in
  let* response =
    (pipeline_for ~url)
      (Dream.request ~method_:`POST ~target ~headers
         (Http_fixture.form_body (("dream.csrf", token) :: body_fields)))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* One cookie-less GET that opens a fresh session for this user and
   returns its cookie plus a live CSRF token. *)
let open_session label ~url uid =
  shared_identity := Some uid;
  let* response, token = do_get ~url ~target:"/mint" () in
  Alcotest.(check int) (label ^ ": mint 200") 200 (status_of response);
  Lwt.return (Http_fixture.session_cookie label response, token)

let check_redirect label ~location response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some location)
    (Dream.header response "Location")

(* === Targets === *)

let request_home_target slug = Printf.sprintf "/projects/%s/request-home" slug

let provision_target slug = Printf.sprintf "/projects/%s/community-home" slug

let publish_target slug = Printf.sprintf "/c/%s/publish" slug

let accept_target ~community ~project =
  Printf.sprintf "/c/%s/projects/%s/accept" community project

let reject_target ~community ~project =
  Printf.sprintf "/c/%s/projects/%s/reject" community project

let project_side_remove ~project ~community =
  Printf.sprintf "/projects/%s/community-home/%s/remove" project community

let community_side_remove ~community ~project =
  Printf.sprintf "/c/%s/projects/%s/remove-home" community project

(* === Form field sets === *)

let request_fields ~community ?(note = "") () =
  [ ("target_community_id", string_of_int community);
    ("request_note", note)
  ]

let community_fields ?(name = "Anfn Community Home") ~slug
    ?(description = "") () =
  [ ("community_name", name); ("community_slug", slug);
    ("community_description", description)
  ]

let publish_fields ?(name = "Anfn Community Home") ~slug ?(description = "")
    ?(visibility = "public") () =
  community_fields ~name ~slug ~description ()
  @ [ ("publication_visibility", visibility) ]

(* === github_repositories_selected === *)

let selection_case =
  db_case
    "repository selection: each durably accepted set captures exactly one \
     github_repositories_selected carrying only its size"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_sel" in
      let* _inst, draft, _v, _acct =
        Project_fixture.make_draft conn ~user:uid ~ext_id:962000001L
          (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:962900001L "alpha";
              Project_fixture.repo ~account_id ~id:962900002L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* cookie, token = open_session "sel" ~url uid in
      let post label ?consent fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ?consent ~url ~cookie ~target:"/projects/new/repositories"
              ~token ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let draft_field = ("draft_id", Int64.to_string draft) in
      (* One repository. *)
      let* label, response, captured =
        post "one"
          [ draft_field; ("repository", Int64.to_string s1) ]
      in
      Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
      Analytics_fixture.check_single_capture label ~name:"github_repositories_selected"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid); ("repository_count", `Int 1) ]
        captured;
      (* Two repositories. *)
      let* label, response, captured =
        post "two"
          [ draft_field; ("repository", Int64.to_string s1);
            ("repository", Int64.to_string s2)
          ]
      in
      Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
      Analytics_fixture.check_single_capture label ~name:"github_repositories_selected"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid); ("repository_count", `Int 2) ]
        captured;
      (* A deliberately cleared selection is still a committed
         transition, and reports count 0. *)
      let* label, response, captured = post "cleared" [ draft_field ] in
      Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
      Analytics_fixture.check_single_capture label ~name:"github_repositories_selected"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid); ("repository_count", `Int 0) ]
        captured;
      (* Denied consent: identical business outcome, no event. *)
      let* label, response, captured =
        post "denied consent" ~consent:[ Analytics_fixture.an_consent_denied ]
          [ draft_field; ("repository", Int64.to_string s1) ]
      in
      Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      Lwt.return_unit)

let selection_failure_case =
  db_case
    "repository selection: an invalid form, a foreign draft, and a stale \
     selection capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_selfail" in
      let* other = insert_user conn "anfn_selother" in
      let* _inst, draft, _v, _acct =
        Project_fixture.make_draft conn ~user:uid ~ext_id:962000002L
          (fun account_id -> [ Project_fixture.repo ~account_id ~id:962900011L "alpha" ])
      in
      let* _inst, foreign_draft, _v, _acct =
        Project_fixture.make_draft conn ~user:other ~ext_id:962000003L
          (fun account_id -> [ Project_fixture.repo ~account_id ~id:962900012L "alpha" ])
      in
      let* cookie, token = open_session "selfail" ~url uid in
      let post label fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:"/projects/new/repositories" ~token
              ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let refused label fields =
        let* label, _response, captured = post label fields in
        Analytics_fixture.check_no_capture label captured;
        Lwt.return_unit
      in
      let* () =
        refused "unknown field"
          [ ("draft_id", Int64.to_string draft); ("anfn_unknown", "x") ]
      in
      let* () = refused "missing draft id" [ ("repository", "1") ] in
      let* () =
        refused "non-numeric draft id" [ ("draft_id", "not-a-number") ]
      in
      let* () =
        refused "foreign draft"
          [ ("draft_id", Int64.to_string foreign_draft) ]
      in
      let* () =
        refused "snapshot id from another draft"
          [ ("draft_id", Int64.to_string draft);
            ("repository", "962999999")
          ]
      in
      (* An origin failure is refused before the form is even parsed. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            let headers =
              [ ("Origin", "https://evil.example");
                ("Content-Type", "application/x-www-form-urlencoded");
                ("Cookie", cookie ^ "; " ^ fst Analytics_fixture.an_consent_granted ^ "=granted")
              ]
            in
            let* response =
              (pipeline_for ~url)
                (Dream.request ~method_:`POST
                   ~target:"/projects/new/repositories" ~headers
                   (Http_fixture.form_body
                      [ ("dream.csrf", token);
                        ("draft_id", Int64.to_string draft)
                      ]))
            in
            let* body = Dream.body response in
            Lwt.return (response, body))
      in
      Alcotest.(check int) "cross-origin 403" 403 (status_of response);
      Analytics_fixture.check_no_capture "cross-origin" captured;
      (* A missing CSRF token is refused the same way. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:"/projects/new/repositories"
              ~token:"not-a-token"
              ~body_fields:[ ("draft_id", Int64.to_string draft) ]
              ())
      in
      Alcotest.(check int) "bad CSRF 403" 403 (status_of response);
      Analytics_fixture.check_no_capture "bad CSRF" captured;
      Lwt.return_unit)

(* === github_project_created === *)

let creation_case =
  db_case
    "project creation: a committed finalization captures exactly one \
     github_project_created carrying the permanent id, kind and count"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_create" in
      let* _inst, draft, _v, _acct =
        Project_fixture.make_draft conn ~user:uid ~ext_id:962000011L
          (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:962900021L "alpha";
              Project_fixture.repo ~account_id ~id:962900022L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1; s2 ]
      in
      let* cookie, token = open_session "create" ~url uid in
      let fields =
        Http_fixture.identity_fields ~draft:(Int64.to_string draft)
          ~name:"Anfn Created Project" ~slug:"anfn-created"
          ~primary:(Int64.to_string s1) ()
      in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:"/projects" ~token
              ~body_fields:fields ())
      in
      check_redirect "created" ~location:"/projects/anfn-created/setup"
        response;
      let* project = find conn "project id" q_project_id_of_slug "anfn-created" in
      Analytics_fixture.check_single_capture "created" ~name:"github_project_created"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:
          [ ("user_id", `Int uid);
            ("project_id", `Intlit (Int64.to_string project));
            ("project_kind", `String "project");
            ("repository_count", `Int 2)
          ]
        captured;
      (* A replay finds the draft completed: no second project, no second
         event. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:"/projects" ~token
              ~body_fields:fields ())
      in
      Alcotest.(check int) "replay 303" 303 (status_of response);
      Analytics_fixture.check_no_capture "replay" captured;
      let* count = find conn "project count" q_project_count 962100011L in
      Alcotest.(check int) "exactly one permanent project" 1 count;
      Lwt.return_unit)

let creation_failure_case =
  db_case
    "project creation: an invalid form, a taken slug, and an already \
     claimed repository capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_createfail" in
      (* An existing project owns both the slug and a repository id. *)
      let* _inst, _existing =
        make_project conn ~user:uid ~ext_id:962000021L ~slug:"anfn-taken"
      in
      let* _inst, draft, _v, _acct =
        Project_fixture.make_draft conn ~user:uid ~ext_id:962000022L
          (fun account_id -> [ Project_fixture.repo ~account_id ~id:962900031L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = List.nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* cookie, token = open_session "createfail" ~url uid in
      let post label fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:"/projects" ~token
              ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      (* Structurally invalid: an unknown field never reaches the store. *)
      let* label, response, captured =
        post "unknown field"
          [ ("draft_id", Int64.to_string draft); ("anfn_unknown", "x") ]
      in
      Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      (* Domain-invalid identity: a reserved slug is a 422 re-render. *)
      let* label, response, captured =
        post "reserved slug"
          (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
             ~name:"Anfn Reserved" ~slug:"new"
             ~primary:(Int64.to_string s1) ())
      in
      Alcotest.(check int) (label ^ ": 422") 422 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      (* Slug already taken by the existing project: a 409 re-render. *)
      let* label, response, captured =
        post "slug conflict"
          (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
             ~name:"Anfn Conflict" ~slug:"anfn-taken"
             ~primary:(Int64.to_string s1) ())
      in
      Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* count = find conn "project count" q_project_count 962100022L in
      Alcotest.(check int) "no project from a refused creation" 0 count;
      Lwt.return_unit)

(* === project_home_request_submitted === *)

let request_case =
  db_case
    "home request: a committed pending request captures exactly one \
     project_home_request_submitted for the steward"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_req" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:962000031L ~slug:"anfn-req"
      in
      let* community = insert_community conn "anfn-req-home" in
      let* cookie, token = open_session "req" ~url uid in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:(request_home_target "anfn-req")
              ~token
              ~body_fields:
                (request_fields ~community
                   ~note:"anfn-note-SHOULD-NOT-BE-CAPTURED" ())
              ())
      in
      check_redirect "submitted" ~location:"/projects/anfn-req/request-home"
        response;
      Analytics_fixture.check_single_capture "submitted"
        ~name:"project_home_request_submitted"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      (* The private note never reached the payload. *)
      let serialized =
        String.concat "|" (List.map Yojson.Safe.to_string captured)
      in
      Alcotest.(check bool) "request note absent from the payload" false
        (Html_assert.contains serialized "anfn-note");
      Alcotest.(check bool) "community slug absent from the payload" false
        (Html_assert.contains serialized "anfn-req-home");
      (* A replay hits the one-active-home rule: 409, and no event. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:(request_home_target "anfn-req")
              ~token ~body_fields:(request_fields ~community ()) ())
      in
      Alcotest.(check int) "replay 409" 409 (status_of response);
      Analytics_fixture.check_no_capture "replay" captured;
      Lwt.return_unit)

let request_failure_case =
  db_case
    "home request: an invalid form, an ineligible target, and an \
     unstewarded project capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_reqfail" in
      let* outsider = insert_user conn "anfn_reqout" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:962000032L ~slug:"anfn-reqfail"
      in
      (* Eligibility is exactly "network community AND published AND
         public-or-unlisted", so a legacy community is ineligible without
         forcing a shape the scoped lifecycle CHECKs forbid. *)
      let* legacy_community =
        insert_community ~network:false conn "anfn-reqfail-legacy"
      in
      let* cookie, token = open_session "reqfail" ~url uid in
      let post ?(target = request_home_target "anfn-reqfail") label fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target ~token ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        post "unknown field" [ ("anfn_unknown", "x") ]
      in
      Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* label, response, captured =
        post "ineligible target"
          (request_fields ~community:legacy_community ())
      in
      Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      (* An unstewarded project is the generic 404. *)
      let* community = insert_community conn "anfn-reqfail-home" in
      let* out_cookie, out_token = open_session "outsider" ~url outsider in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie:out_cookie
              ~target:(request_home_target "anfn-reqfail") ~token:out_token
              ~body_fields:(request_fields ~community ()) ())
      in
      Alcotest.(check int) "unstewarded 404" 404 (status_of response);
      Analytics_fixture.check_no_capture "unstewarded" captured;
      Lwt.return_unit)

let request_concurrency_case =
  db_case
    "home request concurrency: two competing submissions leave one \
     relation and exactly one submitted event"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_reqrace" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:962000033L ~slug:"anfn-reqrace"
      in
      let* a = insert_community conn "anfn-reqrace-a" in
      let* b = insert_community conn "anfn-reqrace-b" in
      let* cookie, token = open_session "reqrace" ~url uid in
      let* (first, second), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            Lwt.both
              (do_post ~url ~cookie
                 ~target:(request_home_target "anfn-reqrace") ~token
                 ~body_fields:(request_fields ~community:a ())
                 ())
              (do_post ~url ~cookie
                 ~target:(request_home_target "anfn-reqrace") ~token
                 ~body_fields:(request_fields ~community:b ())
                 ()))
      in
      let statuses =
        List.sort compare
          [ status_of (fst first); status_of (fst second) ]
      in
      Alcotest.(check (list int)) "one winner, one conflict" [ 303; 409 ]
        statuses;
      Analytics_fixture.check_single_capture "race" ~name:"project_home_request_submitted"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      let* count = find conn "relations" q_relation_count project in
      Alcotest.(check int) "exactly one relation" 1 count;
      Lwt.return_unit)

(* === project_home_request_reviewed === *)

let review_case =
  db_case
    "review: accept and reject each capture exactly one \
     project_home_request_reviewed for the reviewing moderator"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_revsteward" in
      let* moderator = insert_user conn "anfn_revmod" in
      let* _inst, _p1 =
        make_project conn ~user:steward ~ext_id:962000041L
          ~slug:"anfn-accepted"
      in
      let* _inst, _p2 =
        make_project conn ~user:steward ~ext_id:962000042L
          ~slug:"anfn-rejected"
      in
      let* community = insert_community conn "anfn-review-home" in
      let* () =
        exec conn "top mod" q_insert_moderator
          (moderator, community, "top_mod")
      in
      let* () =
        pending_request conn ~user:steward ~slug:"anfn-accepted" ~community ()
      in
      let* () =
        pending_request conn ~user:steward ~slug:"anfn-rejected" ~community ()
      in
      let* cookie, token = open_session "review" ~url moderator in
      let review label ~target =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target ~token ~body_fields:[] ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        review "accept"
          ~target:
            (accept_target ~community:"anfn-review-home"
               ~project:"anfn-accepted")
      in
      check_redirect label
        ~location:"/c/anfn-review-home/project-home-requests" response;
      Analytics_fixture.check_single_capture label ~name:"project_home_request_reviewed"
        (* The actor is the REVIEWER, not the requesting steward. *)
        ~distinct_id:(Printf.sprintf "user:%d" moderator)
        ~props:
          [ ("user_id", `Int moderator); ("decision", `String "accepted") ]
        captured;
      let* label, response, captured =
        review "reject"
          ~target:
            (reject_target ~community:"anfn-review-home"
               ~project:"anfn-rejected")
      in
      check_redirect label
        ~location:"/c/anfn-review-home/project-home-requests" response;
      Analytics_fixture.check_single_capture label ~name:"project_home_request_reviewed"
        ~distinct_id:(Printf.sprintf "user:%d" moderator)
        ~props:
          [ ("user_id", `Int moderator); ("decision", `String "rejected") ]
        captured;
      (* Replays of both decisions find nothing pending. *)
      let* label, response, captured =
        review "accept replay"
          ~target:
            (accept_target ~community:"anfn-review-home"
               ~project:"anfn-accepted")
      in
      Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      Lwt.return_unit)

let review_failure_case =
  db_case
    "review: an unauthorized reviewer and a body-carrying form capture \
     nothing"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_revfsteward" in
      let* moderator = insert_user conn "anfn_revfmod" in
      let* ordinary = insert_user conn "anfn_revfuser" in
      let* _inst, _p =
        make_project conn ~user:steward ~ext_id:962000043L
          ~slug:"anfn-revfail"
      in
      let* community = insert_community conn "anfn-revfail-home" in
      let* () =
        exec conn "top mod" q_insert_moderator
          (moderator, community, "top_mod")
      in
      let* () =
        pending_request conn ~user:steward ~slug:"anfn-revfail" ~community ()
      in
      let target =
        accept_target ~community:"anfn-revfail-home" ~project:"anfn-revfail"
      in
      let* cookie, token = open_session "ordinary" ~url ordinary in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target ~token ~body_fields:[] ())
      in
      Alcotest.(check int) "unauthorized 404" 404 (status_of response);
      Analytics_fixture.check_no_capture "unauthorized" captured;
      (* A moderator submitting any application field is a generic 400
         that never reaches the store. *)
      let* mod_cookie, mod_token = open_session "moderator" ~url moderator in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie:mod_cookie ~target ~token:mod_token
              ~body_fields:[ ("decision", "accepted") ] ())
      in
      Alcotest.(check int) "field-carrying 400" 400 (status_of response);
      Analytics_fixture.check_no_capture "field-carrying" captured;
      Lwt.return_unit)

let review_concurrency_case =
  db_case
    "review concurrency: two reviewers of one pending request produce \
     exactly one reviewed event"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_revrsteward" in
      let* mod_a = insert_user conn "anfn_revrmoda" in
      let* mod_b = insert_user conn "anfn_revrmodb" in
      let* _inst, project =
        make_project conn ~user:steward ~ext_id:962000044L
          ~slug:"anfn-revrace"
      in
      let* community = insert_community conn "anfn-revrace-home" in
      let* () =
        exec conn "mod a" q_insert_moderator (mod_a, community, "top_mod")
      in
      let* () =
        exec conn "mod b" q_insert_moderator (mod_b, community, "top_mod")
      in
      let* () =
        pending_request conn ~user:steward ~slug:"anfn-revrace" ~community ()
      in
      let target =
        accept_target ~community:"anfn-revrace-home" ~project:"anfn-revrace"
      in
      let* cookie_a, token_a = open_session "mod a" ~url mod_a in
      let* cookie_b, token_b = open_session "mod b" ~url mod_b in
      let* (first, second), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            Lwt.both
              (do_post ~url ~cookie:cookie_a ~target ~token:token_a
                 ~body_fields:[] ())
              (do_post ~url ~cookie:cookie_b ~target ~token:token_b
                 ~body_fields:[] ()))
      in
      let statuses =
        List.sort compare
          [ status_of (fst first); status_of (fst second) ]
      in
      Alcotest.(check (list int)) "one winner, one conflict" [ 303; 409 ]
        statuses;
      Alcotest.(check int) "exactly one reviewed event" 1
        (List.length captured);
      Alcotest.(check (list string)) "and it is the reviewed event"
        [ "project_home_request_reviewed" ] (Analytics_fixture.an_event_names captured);
      let* status = find conn "status" q_relation_status project in
      Alcotest.(check string) "one accepted relation" "accepted" status;
      Lwt.return_unit)

(* === dedicated_home_provisioned === *)

let provisioning_case =
  db_case
    "provisioning: a committed dedicated home captures exactly one \
     dedicated_home_provisioned and never a publication event"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_prov" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:962000051L ~slug:"anfn-prov"
      in
      let* cookie, token = open_session "prov" ~url uid in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:(provision_target "anfn-prov")
              ~token
              ~body_fields:(community_fields ~slug:"anfn-prov-home" ())
              ())
      in
      check_redirect "provisioned"
        ~location:"/c/anfn-prov-home/settings" response;
      Analytics_fixture.check_single_capture "provisioned" ~name:"dedicated_home_provisioned"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      (* The created community is still a private draft: nothing published
         happened, and no publication event may exist. *)
      let* state, visibility =
        find conn "community state" q_community_state "anfn-prov-home"
      in
      Alcotest.(check string) "still a draft" "draft" state;
      Alcotest.(check string) "still private" "private" visibility;
      (* A replay finds an active home: the current-home GET, no event. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:(provision_target "anfn-prov")
              ~token
              ~body_fields:(community_fields ~slug:"anfn-prov-home2" ())
              ())
      in
      check_redirect "replay" ~location:"/projects/anfn-prov/request-home"
        response;
      Analytics_fixture.check_no_capture "replay" captured;
      let* count = find conn "second community" q_community_count
                     "anfn-prov-home2" in
      Alcotest.(check int) "no second community" 0 count;
      Lwt.return_unit)

let provisioning_failure_case =
  db_case
    "provisioning: an invalid form, a taken slug, and an unstewarded \
     project capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_provfail" in
      let* outsider = insert_user conn "anfn_provout" in
      let* _inst, _project =
        make_project conn ~user:uid ~ext_id:962000052L ~slug:"anfn-provfail"
      in
      let* _taken = insert_community conn "anfn-provfail-taken" in
      let* cookie, token = open_session "provfail" ~url uid in
      let post label fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie
              ~target:(provision_target "anfn-provfail") ~token
              ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        post "invalid slug"
          (community_fields ~slug:"Anfn Invalid Slug" ())
      in
      Alcotest.(check int) (label ^ ": 422") 422 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* label, response, captured =
        post "taken slug" (community_fields ~slug:"anfn-provfail-taken" ())
      in
      Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* out_cookie, out_token = open_session "outsider" ~url outsider in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie:out_cookie
              ~target:(provision_target "anfn-provfail") ~token:out_token
              ~body_fields:(community_fields ~slug:"anfn-provfail-out" ())
              ())
      in
      Alcotest.(check int) "unstewarded 404" 404 (status_of response);
      Analytics_fixture.check_no_capture "unstewarded" captured;
      Lwt.return_unit)

let provisioning_concurrency_case =
  db_case
    "provisioning concurrency: two attempts leave one home and exactly one \
     provisioned event"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_provrace" in
      let* _inst, project =
        make_project conn ~user:uid ~ext_id:962000053L ~slug:"anfn-provrace"
      in
      let* cookie, token = open_session "provrace" ~url uid in
      let* (first, second), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            Lwt.both
              (do_post ~url ~cookie
                 ~target:(provision_target "anfn-provrace") ~token
                 ~body_fields:(community_fields ~slug:"anfn-provrace-a" ())
                 ())
              (do_post ~url ~cookie
                 ~target:(provision_target "anfn-provrace") ~token
                 ~body_fields:(community_fields ~slug:"anfn-provrace-b" ())
                 ()))
      in
      ignore first;
      ignore second;
      Alcotest.(check int) "exactly one provisioned event" 1
        (List.length captured);
      Alcotest.(check (list string)) "and it is the provisioned event"
        [ "dedicated_home_provisioned" ] (Analytics_fixture.an_event_names captured);
      let* count = find conn "relations" q_relation_count project in
      Alcotest.(check int) "exactly one relation" 1 count;
      Lwt.return_unit)

(* === network_community_published === *)

let publication_case =
  db_case
    "publication: Public and Unlisted each capture exactly one \
     network_community_published carrying only the committed exposure"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_pub" in
      let* _p, _cid, _rid =
        make_home_draft conn ~user:uid ~ext_id:962000061L
          ~project_slug:"anfn-pub-a" ~slug:"anfn-pub-a-home"
      in
      let* _p, _cid, _rid =
        make_home_draft conn ~user:uid ~ext_id:962000062L
          ~project_slug:"anfn-pub-b" ~slug:"anfn-pub-b-home"
      in
      let* cookie, token = open_session "pub" ~url uid in
      let publish label ~slug ~visibility =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target:(publish_target slug) ~token
              ~body_fields:(publish_fields ~slug ~visibility ())
              ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        publish "public" ~slug:"anfn-pub-a-home" ~visibility:"public"
      in
      check_redirect label ~location:"/c/anfn-pub-a-home" response;
      Analytics_fixture.check_single_capture label ~name:"network_community_published"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:
          [ ("user_id", `Int uid);
            ("publication_visibility", `String "public")
          ]
        captured;
      let* label, response, captured =
        publish "unlisted" ~slug:"anfn-pub-b-home" ~visibility:"unlisted"
      in
      check_redirect label ~location:"/c/anfn-pub-b-home" response;
      Analytics_fixture.check_single_capture label ~name:"network_community_published"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:
          [ ("user_id", `Int uid);
            ("publication_visibility", `String "unlisted")
          ]
        captured;
      (* A replayed publication is the generic 404, with no event. *)
      let* label, response, captured =
        publish "replay" ~slug:"anfn-pub-a-home" ~visibility:"public"
      in
      Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      Lwt.return_unit)

let publication_failure_case =
  db_case
    "publication: an invalid form, a taken slug, and an unauthorized \
     publisher capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_pubfail" in
      let* outsider = insert_user conn "anfn_pubout" in
      let* _p, _cid, _rid =
        make_home_draft conn ~user:uid ~ext_id:962000063L
          ~project_slug:"anfn-pubfail" ~slug:"anfn-pubfail-home"
      in
      let* _taken = insert_community conn "anfn-pubfail-taken" in
      let* cookie, token = open_session "pubfail" ~url uid in
      let post label ?(cookie = cookie) ?(token = token) fields =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie
              ~target:(publish_target "anfn-pubfail-home") ~token
              ~body_fields:fields ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        post "invalid visibility"
          (publish_fields ~slug:"anfn-pubfail-home" ~visibility:"private" ())
      in
      Alcotest.(check int) (label ^ ": 422") 422 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* label, response, captured =
        post "taken slug"
          (publish_fields ~slug:"anfn-pubfail-taken" ())
      in
      Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* out_cookie, out_token = open_session "outsider" ~url outsider in
      let* label, response, captured =
        post "unauthorized" ~cookie:out_cookie ~token:out_token
          (publish_fields ~slug:"anfn-pubfail-home" ())
      in
      Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
      Analytics_fixture.check_no_capture label captured;
      let* state, _visibility =
        find conn "community state" q_community_state "anfn-pubfail-home"
      in
      Alcotest.(check string) "still a draft" "draft" state;
      Lwt.return_unit)

let publication_concurrency_case =
  db_case
    "publication concurrency: two attempts publish once and capture \
     exactly one published event"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_pubrace" in
      let* _p, _cid, _rid =
        make_home_draft conn ~user:uid ~ext_id:962000064L
          ~project_slug:"anfn-pubrace" ~slug:"anfn-pubrace-home"
      in
      let* cookie, token = open_session "pubrace" ~url uid in
      let fields = publish_fields ~slug:"anfn-pubrace-home" () in
      let* (first, second), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            Lwt.both
              (do_post ~url ~cookie
                 ~target:(publish_target "anfn-pubrace-home") ~token
                 ~body_fields:fields ())
              (do_post ~url ~cookie
                 ~target:(publish_target "anfn-pubrace-home") ~token
                 ~body_fields:fields ()))
      in
      let statuses =
        List.sort compare
          [ status_of (fst first); status_of (fst second) ]
      in
      Alcotest.(check (list int)) "one winner, one generic 404" [ 303; 404 ]
        statuses;
      Alcotest.(check int) "exactly one published event" 1
        (List.length captured);
      Alcotest.(check (list string)) "and it is the published event"
        [ "network_community_published" ] (Analytics_fixture.an_event_names captured);
      Lwt.return_unit)

(* === project_home_removed === *)

let removal_case =
  db_case
    "removal: each surface captures exactly one project_home_removed \
     carrying its route surface and no authorization source"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_rmsteward" in
      let* moderator = insert_user conn "anfn_rmmod" in
      let seed ~ext_id ~project_slug ~community_slug =
        let* _inst, _project =
          make_project conn ~user:steward ~ext_id ~slug:project_slug
        in
        let* community = insert_community conn community_slug in
        let* () =
          exec conn "top mod" q_insert_moderator
            (moderator, community, "top_mod")
        in
        let* () =
          pending_request conn ~user:steward ~slug:project_slug ~community ()
        in
        accept_request conn ~reviewer:moderator ~project_slug
          ~community_slug
      in
      let* () =
        seed ~ext_id:962000071L ~project_slug:"anfn-rm-a"
          ~community_slug:"anfn-rm-a-home"
      in
      let* () =
        seed ~ext_id:962000072L ~project_slug:"anfn-rm-b"
          ~community_slug:"anfn-rm-b-home"
      in
      let* cookie, token = open_session "removal" ~url steward in
      let remove label ~target =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target ~token ~body_fields:[] ())
        |> Lwt.map (fun ((response, _body), captured) ->
               (label, response, captured))
      in
      let* label, response, captured =
        remove "project surface"
          ~target:
            (project_side_remove ~project:"anfn-rm-a"
               ~community:"anfn-rm-a-home")
      in
      check_redirect label ~location:"/projects/anfn-rm-a/request-home"
        response;
      Analytics_fixture.check_single_capture label ~name:"project_home_removed"
        ~distinct_id:(Printf.sprintf "user:%d" steward)
        ~props:
          [ ("user_id", `Int steward);
            ("removal_surface", `String "project")
          ]
        captured;
      let* label, response, captured =
        remove "community surface"
          ~target:
            (community_side_remove ~community:"anfn-rm-b-home"
               ~project:"anfn-rm-b")
      in
      check_redirect label
        ~location:"/c/anfn-rm-b-home/settings?panel=projects" response;
      Analytics_fixture.check_single_capture label ~name:"project_home_removed"
        ~distinct_id:(Printf.sprintf "user:%d" steward)
        ~props:
          [ ("user_id", `Int steward);
            ("removal_surface", `String "community")
          ]
        captured;
      (* A replay reaches the same destination — deliberately — with no
         second event. *)
      let* label, response, captured =
        remove "replay"
          ~target:
            (project_side_remove ~project:"anfn-rm-a"
               ~community:"anfn-rm-a-home")
      in
      check_redirect label ~location:"/projects/anfn-rm-a/request-home"
        response;
      Analytics_fixture.check_no_capture label captured;
      Lwt.return_unit)

let removal_failure_case =
  db_case
    "removal: a protected unpublished draft home and an unauthorized \
     actor capture nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "anfn_rmfail" in
      let* outsider = insert_user conn "anfn_rmout" in
      let* project, _cid, _rid =
        make_home_draft conn ~user:uid ~ext_id:962000073L
          ~project_slug:"anfn-rmfail" ~slug:"anfn-rmfail-home"
      in
      let* cookie, token = open_session "rmfail" ~url uid in
      let target =
        project_side_remove ~project:"anfn-rmfail"
          ~community:"anfn-rmfail-home"
      in
      (* The provisioned home of an unpublished setup draft is protected:
         the redirect is the same as a success, and there is deliberately
         no event. *)
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie ~target ~token ~body_fields:[] ())
      in
      check_redirect "protected draft"
        ~location:"/projects/anfn-rmfail/request-home" response;
      Analytics_fixture.check_no_capture "protected draft" captured;
      let* status = find conn "status" q_relation_status project in
      Alcotest.(check string) "home still accepted" "accepted" status;
      let* out_cookie, out_token = open_session "outsider" ~url outsider in
      let* (response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie:out_cookie ~target ~token:out_token
              ~body_fields:[] ())
      in
      Alcotest.(check int) "unauthorized 404" 404 (status_of response);
      Analytics_fixture.check_no_capture "unauthorized" captured;
      Lwt.return_unit)

let removal_concurrency_case =
  db_case
    "removal concurrency: two removers on both surfaces produce exactly \
     one removal event"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_rmrsteward" in
      let* moderator = insert_user conn "anfn_rmrmod" in
      let* _inst, project =
        make_project conn ~user:steward ~ext_id:962000074L
          ~slug:"anfn-rmrace"
      in
      let* community = insert_community conn "anfn-rmrace-home" in
      let* () =
        exec conn "top mod" q_insert_moderator
          (moderator, community, "top_mod")
      in
      let* () =
        pending_request conn ~user:steward ~slug:"anfn-rmrace" ~community ()
      in
      let* () =
        accept_request conn ~reviewer:moderator ~project_slug:"anfn-rmrace"
          ~community_slug:"anfn-rmrace-home"
      in
      let* steward_cookie, steward_token =
        open_session "steward" ~url steward
      in
      let* mod_cookie, mod_token = open_session "moderator" ~url moderator in
      let* (first, second), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            Lwt.both
              (do_post ~url ~cookie:steward_cookie
                 ~target:
                   (project_side_remove ~project:"anfn-rmrace"
                      ~community:"anfn-rmrace-home")
                 ~token:steward_token ~body_fields:[] ())
              (do_post ~url ~cookie:mod_cookie
                 ~target:
                   (community_side_remove ~community:"anfn-rmrace-home"
                      ~project:"anfn-rmrace")
                 ~token:mod_token ~body_fields:[] ()))
      in
      (* Both surfaces answer 303 whether they removed or found nothing to
         remove — that collapse is the point of the design. *)
      Alcotest.(check (list int)) "both redirect" [ 303; 303 ]
        [ status_of (fst first); status_of (fst second) ];
      Alcotest.(check int) "exactly one removal event" 1
        (List.length captured);
      Alcotest.(check (list string)) "and it is the removal event"
        [ "project_home_removed" ] (Analytics_fixture.an_event_names captured);
      let* status = find conn "status" q_relation_status project in
      Alcotest.(check string) "relation removed once" "removed" status;
      Lwt.return_unit)

(* === Privacy sweep === *)

let privacy_case =
  db_case
    "privacy: no email, GitHub identifier, repository name, slug, note, or \
     description reaches an event name, a property, a distinct id, or a \
     captured body"
    (fun ~url conn ->
      let* steward = insert_user conn "anfn_privsteward" in
      let* moderator = insert_user conn "anfn_privmod" in
      let repo_name = "anfnPrivateRepoFixture" in
      let note = "anfn-NOTE-fixture-9f2b" in
      let description = "anfn-DESCRIPTION-fixture-4c7d" in
      let* _inst, draft, _v, _acct =
        Project_fixture.make_draft ~login:"anfn-owner-fixture" conn ~user:steward
          ~ext_id:962000081L
          (fun account_id ->
            [ (* the listing fixture substitutes raw JSON per field, so a
                 description travels as a JSON string literal *)
              Project_fixture.repo ~owner_login:"anfn-owner-fixture" ~account_id
                ~description:(Printf.sprintf "%S" description)
                ~id:962900081L repo_name
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = List.nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed" conn ~user:steward ~draft ~primary:s1
          [ s1 ]
      in
      let* cookie, token = open_session "privacy" ~url steward in
      let collected = ref [] in
      let run label target fields =
        let* (response, _body), captured =
          Analytics_fixture.with_sink_lwt (fun () ->
              do_post ~url ~cookie ~target ~token ~body_fields:fields ())
        in
        collected := !collected @ captured;
        Lwt.return (label, response)
      in
      let* _ =
        run "select" "/projects/new/repositories"
          [ ("draft_id", Int64.to_string draft);
            ("repository", Int64.to_string s1)
          ]
      in
      let* _ =
        run "create" "/projects"
          (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
             ~name:"Anfn Privacy Project" ~slug:"anfn-privacy"
             ~description ~website:"https://anfn.example/privacy"
             ~primary:(Int64.to_string s1) ())
      in
      let* community = insert_community conn "anfn-privacy-home" in
      let* () =
        exec conn "top mod" q_insert_moderator
          (moderator, community, "top_mod")
      in
      let* _ =
        run "request"
          (request_home_target "anfn-privacy")
          (request_fields ~community ~note ())
      in
      let* mod_cookie, mod_token = open_session "moderator" ~url moderator in
      let* (_response, _body), captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            do_post ~url ~cookie:mod_cookie
              ~target:
                (accept_target ~community:"anfn-privacy-home"
                   ~project:"anfn-privacy")
              ~token:mod_token ~body_fields:[] ())
      in
      collected := !collected @ captured;
      let payloads = !collected in
      Alcotest.(check int) "four funnel events" 4 (List.length payloads);
      Alcotest.(check (list string)) "the expected four"
        [ "github_repositories_selected"; "github_project_created";
          "project_home_request_submitted"; "project_home_request_reviewed"
        ]
        (Analytics_fixture.an_event_names payloads);
      (* Distinct ids are only the two intended user identities. *)
      let distinct_ids =
        List.filter_map
          (fun p ->
            match Analytics_fixture.payload_member "distinct_id" p with
            | Some (`String s) -> Some s
            | _ -> None)
          payloads
      in
      List.iter
        (fun id ->
          Alcotest.(check bool) "distinct id is user:<id>" true
            (id = Printf.sprintf "user:%d" steward
            || id = Printf.sprintf "user:%d" moderator))
        distinct_ids;
      let serialized =
        String.concat "|" (List.map Yojson.Safe.to_string payloads)
      in
      List.iter
        (fun (what, needle) ->
          Alcotest.(check bool)
            ("no " ^ what ^ " anywhere in the captured payloads")
            false
            (Html_assert.contains serialized needle))
        [ ("email", "anfn_privsteward@test.invalid");
          ("username", "anfn_privsteward");
          ("moderator username", "anfn_privmod");
          ("github login", "anfn-owner-fixture");
          ("repository name", repo_name);
          ("repository full name", "anfn-owner-fixture/" ^ repo_name);
          ("repository url", "https://github.com/anfn-owner-fixture");
          ("installation id", "962000081");
          ("account id", "962100081");
          ("github repository id", "962900081");
          ("project slug", "anfn-privacy");
          ("community slug", "anfn-privacy-home");
          ("request note", note);
          ("description", description);
          ("website", "anfn.example")
        ];
      (* Small serial ids (draft id, snapshot id) are deliberately NOT
         probed by substring: they can legitimately appear inside an
         exported project_id or user_id, which would make the assertion
         meaningless. Their absence is pinned instead by the exact closed
         property allowlist every other case asserts. *)
      Lwt.return_unit)

let suite =
  [ selection_case; selection_failure_case; creation_case;
    creation_failure_case; request_case; request_failure_case;
    request_concurrency_case; review_case; review_failure_case;
    review_concurrency_case; provisioning_case; provisioning_failure_case;
    provisioning_concurrency_case; publication_case;
    publication_failure_case; publication_concurrency_case; removal_case;
    removal_failure_case; removal_concurrency_case; privacy_case
  ]

let suites =
    (* GitHub project and community-home analytics funnel: the eight
       post-installation events driven through the real handlers and the
       real production stores over a real Dream pipeline, with the fake
       capture sink replacing the PostHog transport. Exactly one event per
       committed transition, none on any refused, replayed, conflicted, or
       losing-concurrent path, and a privacy sweep over the captured
       payloads. The two installation events live with their own harnesses
       in github_start_handler_db and github_oauth_callback_db.
       Database-gated. *)
  [ ("analytics_funnel_handlers_db", suite)
  ]
