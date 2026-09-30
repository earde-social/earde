module An = Earde.Analytics
module AnT = Earde.Analytics.For_testing

let ( let* ) = Lwt.bind

(* --- Deployment environments: closed parsing, activation rules, envelope,
   and the credential/project preflight (local stub only, never PostHog). --- *)

(* Drives the pure validator with a COMPLETE dummy configuration by default,
   so each case flips exactly the value under test. Dummy credentials only. *)
let venv ?(enabled = "true") ?environment ?allow_development
    ?(project_token = "phc_test_token") ?(api_host = "https://eu.i.posthog.com")
    ?(ui_host = "https://eu.posthog.com") ?(project_id = "42")
    ?(personal_api_key = "phx_test_dummy") ?public_origin () =
  AnT.validate_environment_configuration ~enabled ?environment
    ?allow_development ~project_token ~api_host ~ui_host ~project_id
    ~personal_api_key ?public_origin ()

let check_env name ~expect_enabled ?expect_environment result =
  Analytics_fixture.an_case name (fun () ->
      let enabled, environment, _diags = result in
      Alcotest.(check bool) (name ^ " enabled") expect_enabled enabled;
      match expect_environment with
      | None -> ()
      | Some expected ->
          Alcotest.(check (option string))
            (name ^ " environment") expected environment)

(* Preflight stub plumbing (Api_stub, same as the deletion-client tests): the
   two documented private metadata endpoints, dummy values only. *)
let preflight_org = "0196aaaa-bbbb-cccc-dddd-eeeeffff0001"
let preflight_token = "phc_preflight_dummy_token"

let preflight_orgs_body ids =
  Printf.sprintf {|{"count": %d, "next": null, "results": [%s]}|}
    (List.length ids)
    (String.concat ", "
       (List.map (fun id -> Printf.sprintf {|{"id": %S, "name": "o"}|} id) ids))

let preflight_project_body ~id ~token =
  Printf.sprintf {|{"id": %d, "name": "p", "api_token": %S}|} id token

let preflight_handler ?(orgs_status = 200) ?orgs_override
    ?(project_status = 200) ?project_override ()
    (req : Posthog_persons_stub.req) =
  if req.Posthog_persons_stub.path = "/api/organizations/" then
    ( orgs_status,
      match orgs_override with
      | Some body -> body
      | None -> preflight_orgs_body [ preflight_org ] )
  else if
    req.Posthog_persons_stub.path
    = Printf.sprintf "/api/organizations/%s/projects/42/" preflight_org
  then
    ( project_status,
      match project_override with
      | Some body -> body
      | None -> preflight_project_body ~id:42 ~token:preflight_token )
  else (404, "{}")

(* Runs the real preflight against a local stub and returns
   (result, requests the stub saw). *)
let run_preflight ?(environment = An.Staging) handler =
  let ( let* ) = Lwt.bind in
  Lwt_main.run
    (let* base_url, seen, stop = Posthog_persons_stub.start handler in
     AnT.use_preflight_test_configuration ~environment ~ui_host:base_url
       ~project_id:"42" ~personal_api_key:Posthog_persons_stub.deletion_test_key
       ~project_token:preflight_token ();
     Lwt.finalize
       (fun () ->
         let* result = An.Preflight.run () in
         Lwt.return (result, !seen))
       (fun () ->
         AnT.clear_configuration_override ();
         stop ();
         Lwt.return_unit))

let preflight_class = function
  | Ok _ -> "verified"
  | Error (failure_class, _) -> failure_class

(* No request may leave the documented private metadata surface — above all,
   nothing may reach an event-ingestion path. *)
let check_preflight_requests_safe requests =
  List.iter
    (fun (req : Posthog_persons_stub.req) ->
      Alcotest.(check bool)
        ("request stays on /api/organizations: " ^ req.Posthog_persons_stub.path)
        true
        (String.length req.Posthog_persons_stub.path >= 19
        && String.sub req.Posthog_persons_stub.path 0 19 = "/api/organizations/"
        );
      Alcotest.(check bool)
        "no ingestion path" false
        (Html_assert.contains req.Posthog_persons_stub.path "/i/v0/e"
        || Html_assert.contains req.Posthog_persons_stub.path "/capture"
        || Html_assert.contains req.Posthog_persons_stub.path "/batch"))
    requests

let check_preflight name ?environment handler expected_class =
  Analytics_fixture.an_case name (fun () ->
      let result, requests = run_preflight ?environment handler in
      Alcotest.(check string) name expected_class (preflight_class result);
      check_preflight_requests_safe requests;
      (* Neither credential may appear in any output, verified or failed. *)
      let rendered =
        match result with
        | Ok r ->
            String.concat "\n"
              (r.An.Preflight.report_project_id
             :: r.An.Preflight.report_token_fingerprint
             :: r.An.Preflight.report_notes)
        | Error (failure_class, detail) -> failure_class ^ "\n" ^ detail
      in
      Alcotest.(check bool)
        "output never contains the project token" false
        (Html_assert.contains rendered preflight_token);
      Alcotest.(check bool)
        "output never contains the personal key" false
        (Html_assert.contains rendered Posthog_persons_stub.deletion_test_key))

let suites =
  (* Closed deployment-environment parsing: exact values only; unknown,
       missing, blank and differently-cased values fail closed; disabled
       analytics needs no environment at all. *)
  [
    ( "analytics_environment_parsing",
      [
        check_env "production parses exactly" ~expect_enabled:true
          ~expect_environment:(Some "production")
          (venv ~environment:"production" ~public_origin:"https://earde.com" ());
        check_env "staging parses exactly" ~expect_enabled:true
          ~expect_environment:(Some "staging")
          (venv ~environment:"staging"
             ~public_origin:"https://staging.example.com" ());
        check_env "development parses with explicit opt-in" ~expect_enabled:true
          ~expect_environment:(Some "development")
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"http://localhost:8080" ());
        check_env "missing environment fails closed" ~expect_enabled:false
          ~expect_environment:None
          (venv ~public_origin:"https://earde.com" ());
        check_env "blank environment fails closed" ~expect_enabled:false
          ~expect_environment:None
          (venv ~environment:"   " ~public_origin:"https://earde.com" ());
        check_env "cased Production fails closed" ~expect_enabled:false
          (venv ~environment:"Production" ~public_origin:"https://earde.com" ());
        check_env "cased STAGING fails closed" ~expect_enabled:false
          (venv ~environment:"STAGING"
             ~public_origin:"https://staging.example.com" ());
        check_env "abbreviation prod fails closed" ~expect_enabled:false
          (venv ~environment:"prod" ~public_origin:"https://earde.com" ());
        check_env "arbitrary value fails closed" ~expect_enabled:false
          (venv ~environment:"qa" ~public_origin:"https://earde.com" ());
        Analytics_fixture.an_case "missing environment produces a diagnostic"
          (fun () ->
            let _, _, diags = venv ~public_origin:"https://earde.com" () in
            Alcotest.(check int) "one diagnostic" 1 (List.length diags));
        Analytics_fixture.an_case
          "disabled analytics permits an absent environment" (fun () ->
            let enabled, environment, diags =
              AnT.validate_environment_configuration ()
            in
            Alcotest.(check bool) "disabled" false enabled;
            Alcotest.(check (option string)) "no environment" None environment;
            Alcotest.(check int) "no diagnostics" 0 (List.length diags));
        check_env "POSTHOG_ENABLED must be exactly true" ~expect_enabled:false
          (venv ~enabled:"TRUE" ~environment:"production"
             ~public_origin:"https://earde.com" ());
        check_env "development without opt-in stays disabled"
          ~expect_enabled:false
          (venv ~environment:"development"
             ~public_origin:"http://localhost:8080" ());
        Analytics_fixture.an_case "development without opt-in explains itself"
          (fun () ->
            let _, _, diags =
              venv ~environment:"development"
                ~public_origin:"http://localhost:8080" ()
            in
            Alcotest.(check int) "one diagnostic" 1 (List.length diags));
        check_env "opt-in must be exactly true (not TRUE)" ~expect_enabled:false
          (venv ~environment:"development" ~allow_development:"TRUE"
             ~public_origin:"http://localhost:8080" ());
        check_env "opt-in must be exactly true (not 1)" ~expect_enabled:false
          (venv ~environment:"development" ~allow_development:"1"
             ~public_origin:"http://localhost:8080" ());
        Analytics_fixture.an_case
          "explicit development enablement prints a diagnostic" (fun () ->
            let enabled, _, diags =
              venv ~environment:"development" ~allow_development:"true"
                ~public_origin:"http://localhost:8080" ()
            in
            Alcotest.(check bool) "enabled" true enabled;
            Alcotest.(check int) "one notice" 1 (List.length diags));
      ] )
    (* Origin/environment binding: production is bound exactly to
       https://earde.com; staging to any OTHER https origin; the
       development opt-in to loopback only. *);
    ( "analytics_environment_origin",
      [
        check_env "production accepts exactly https://earde.com"
          ~expect_enabled:true
          (venv ~environment:"production" ~public_origin:"https://earde.com" ());
        check_env "production rejects http" ~expect_enabled:false
          (venv ~environment:"production" ~public_origin:"http://earde.com" ());
        check_env "production rejects www" ~expect_enabled:false
          (venv ~environment:"production" ~public_origin:"https://www.earde.com"
             ());
        check_env "production rejects a staging origin" ~expect_enabled:false
          (venv ~environment:"production"
             ~public_origin:"https://staging.earde.com" ());
        check_env "production rejects localhost" ~expect_enabled:false
          (venv ~environment:"production" ~public_origin:"http://localhost:8080"
             ());
        check_env "production rejects a trailing slash" ~expect_enabled:false
          (venv ~environment:"production" ~public_origin:"https://earde.com/" ());
        check_env "staging accepts a non-production https origin"
          ~expect_enabled:true
          (venv ~environment:"staging"
             ~public_origin:"https://staging.example.com" ());
        check_env "staging rejects the production origin" ~expect_enabled:false
          (venv ~environment:"staging" ~public_origin:"https://earde.com" ());
        check_env "staging rejects the www production origin"
          ~expect_enabled:false
          (venv ~environment:"staging" ~public_origin:"https://www.earde.com" ());
        check_env "staging rejects http" ~expect_enabled:false
          (venv ~environment:"staging"
             ~public_origin:"http://staging.example.com" ());
        check_env "development accepts http://localhost with port"
          ~expect_enabled:true
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"http://localhost:8080" ());
        check_env "development accepts https 127.0.0.1" ~expect_enabled:true
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"https://127.0.0.1:8443" ());
        check_env "development accepts ::1" ~expect_enabled:true
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"http://[::1]:8080" ());
        check_env "development rejects the production origin"
          ~expect_enabled:false
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"https://earde.com" ());
        check_env "development rejects arbitrary remote origins"
          ~expect_enabled:false
          (venv ~environment:"development" ~allow_development:"true"
             ~public_origin:"https://remote.example.com" ());
      ] )
    (* Complete-configuration requirements and the public browser config:
       incomplete token/host/project/key combinations disable analytics;
       the browser sees only the public token, ingest host and normalized
       environment — never the personal key or project id. *);
    ( "analytics_environment_config",
      [
        check_env "missing token disables" ~expect_enabled:false
          (AnT.validate_environment_configuration ~enabled:"true"
             ~environment:"production" ~api_host:"https://eu.i.posthog.com"
             ~ui_host:"https://eu.posthog.com" ~project_id:"42"
             ~personal_api_key:"phx_test_dummy"
             ~public_origin:"https://earde.com" ());
        check_env "missing project id disables" ~expect_enabled:false
          (AnT.validate_environment_configuration ~enabled:"true"
             ~environment:"production" ~project_token:"phc_test_token"
             ~api_host:"https://eu.i.posthog.com"
             ~ui_host:"https://eu.posthog.com"
             ~personal_api_key:"phx_test_dummy"
             ~public_origin:"https://earde.com" ());
        check_env "zero project id disables" ~expect_enabled:false
          (venv ~environment:"production" ~project_id:"0"
             ~public_origin:"https://earde.com" ());
        check_env "negative project id disables" ~expect_enabled:false
          (venv ~environment:"production" ~project_id:"-3"
             ~public_origin:"https://earde.com" ());
        check_env "non-numeric project id disables" ~expect_enabled:false
          (venv ~environment:"production" ~project_id:"abc"
             ~public_origin:"https://earde.com" ());
        check_env "missing personal key disables" ~expect_enabled:false
          (AnT.validate_environment_configuration ~enabled:"true"
             ~environment:"production" ~project_token:"phc_test_token"
             ~api_host:"https://eu.i.posthog.com"
             ~ui_host:"https://eu.posthog.com" ~project_id:"42"
             ~public_origin:"https://earde.com" ());
        check_env "http api host disables" ~expect_enabled:false
          (venv ~environment:"production" ~api_host:"http://eu.i.posthog.com"
             ~public_origin:"https://earde.com" ());
        check_env "http ui host disables" ~expect_enabled:false
          (venv ~environment:"production" ~ui_host:"http://eu.posthog.com"
             ~public_origin:"https://earde.com" ());
        Analytics_fixture.an_case
          "staging requires the complete configuration too" (fun () ->
            let enabled, _, _ =
              AnT.validate_environment_configuration ~enabled:"true"
                ~environment:"staging" ~project_token:"phc_test_token"
                ~api_host:"https://eu.i.posthog.com"
                ~ui_host:"https://eu.posthog.com"
                ~public_origin:"https://staging.example.com" ()
            in
            Alcotest.(check bool) "disabled without id+key" false enabled);
        Analytics_fixture.an_case
          "browser config carries only public values + environment" (fun () ->
            Analytics_fixture.install_production_config ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                match An.browser_config () with
                | None -> Alcotest.fail "expected browser config"
                | Some cfg ->
                    Alcotest.(check string)
                      "token" "phc_test_token" cfg.An.browser_token;
                    Alcotest.(check string)
                      "ingest host" "https://eu.i.posthog.com"
                      cfg.An.browser_api_host;
                    Alcotest.(check string)
                      "normalized environment" "production"
                      cfg.An.browser_deployment_environment));
        Analytics_fixture.an_case
          "invalid configuration yields no browser config" (fun () ->
            AnT.install_validated_configuration ~enabled:"true"
              ~environment:"production" ~project_token:"phc_test_token"
              ~public_origin:"https://earde.com" ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                Alcotest.(check bool)
                  "no browser config" true
                  (An.browser_config () = None)));
        Analytics_fixture.an_case
          "launch document renders the environment attr and no secret"
          (fun () ->
            Analytics_fixture.install_production_config ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html = Analytics_fixture.launch_doc () in
                Alcotest.(check bool)
                  "environment attr" true
                  (Html_assert.contains html
                     "data-ph-deployment-environment='production'");
                Alcotest.(check bool)
                  "no personal key" false
                  (Html_assert.contains html "phx_secret_test_value");
                Alcotest.(check bool)
                  "no project id" false
                  (Html_assert.contains html "654321")));
      ] )
    (* Credential/project preflight against a LOCAL stub: verified only
       when the configured project id exists and its live api_token equals
       the configured token; every failure is a bounded class; no request
       ever reaches an ingestion path; no credential is ever printed. *);
    ( "analytics_preflight",
      [
        check_preflight "matching project and token verifies"
          (preflight_handler ()) "verified";
        Analytics_fixture.an_case "verified report carries the safe fields"
          (fun () ->
            let result, requests = run_preflight (preflight_handler ()) in
            (match result with
            | Ok r ->
                Alcotest.(check string)
                  "project id" "42" r.An.Preflight.report_project_id;
                Alcotest.(check bool)
                  "staging environment" true
                  (r.An.Preflight.report_environment = An.Staging);
                Alcotest.(check int)
                  "short fingerprint" 12
                  (String.length r.An.Preflight.report_token_fingerprint);
                Alcotest.(check bool)
                  "fingerprint is not the token" false
                  (Html_assert.contains preflight_token
                     r.An.Preflight.report_token_fingerprint)
            | Error (c, d) -> Alcotest.failf "expected Ok, got %s: %s" c d);
            Alcotest.(check int)
              "exactly two metadata requests" 2 (List.length requests);
            match requests with
            | [ orgs; project ] ->
                Alcotest.(check string)
                  "orgs listing first" "/api/organizations/"
                  orgs.Posthog_persons_stub.path;
                Alcotest.(check string)
                  "documented project retrieve"
                  (Printf.sprintf "/api/organizations/%s/projects/42/"
                     preflight_org)
                  project.Posthog_persons_stub.path
            | _ -> Alcotest.fail "unexpected request sequence");
        check_preflight "wrong token fails"
          (preflight_handler
             ~project_override:
               (preflight_project_body ~id:42 ~token:"phc_other_token")
             ())
          "token_mismatch";
        check_preflight "missing project fails"
          (preflight_handler ~project_status:404 ())
          "project_not_found";
        check_preflight "unauthorized key fails"
          (preflight_handler ~orgs_status:401 ())
          "unauthorized";
        check_preflight "missing scope fails"
          (preflight_handler ~orgs_status:403 ())
          "missing_scope";
        check_preflight "malformed organization listing fails"
          (preflight_handler ~orgs_override:"not json" ())
          "malformed_response";
        check_preflight "malformed project metadata fails"
          (preflight_handler ~project_override:{|{"unexpected": true}|} ())
          "malformed_response";
        check_preflight "different project id in the response fails"
          (preflight_handler
             ~project_override:
               (preflight_project_body ~id:99 ~token:preflight_token)
             ())
          "project_id_mismatch";
        check_preflight "server error on project retrieve fails"
          (preflight_handler ~project_status:500 ())
          "http_500";
        check_preflight "redirect behavior fails safely"
          (preflight_handler ~orgs_status:302 ())
          "unexpected_redirect";
        check_preflight "redirect on project retrieve fails safely"
          (preflight_handler ~project_status:301 ())
          "unexpected_redirect";
        Analytics_fixture.an_case "unreachable host fails as network_failure"
          (fun () ->
            AnT.use_preflight_test_configuration ~environment:An.Staging
              ~ui_host:"http://127.0.0.1:9" ~project_id:"42"
              ~personal_api_key:Posthog_persons_stub.deletion_test_key
              ~project_token:preflight_token ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                match Lwt_main.run (An.Preflight.run ()) with
                | Error ("network_failure", detail) ->
                    Alcotest.(check bool)
                      "no credential in detail" false
                      (Html_assert.contains detail preflight_token
                      || Html_assert.contains detail
                           Posthog_persons_stub.deletion_test_key)
                | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                | Ok _ -> Alcotest.fail "must not verify"));
        Analytics_fixture.an_case
          "invalid configuration fails before any request" (fun () ->
            AnT.use_disabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                match Lwt_main.run (An.Preflight.run ()) with
                | Error ("configuration_invalid", _) -> ()
                | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                | Ok _ -> Alcotest.fail "must not verify"));
        Analytics_fixture.an_case "mismatched cloud regions fail statically"
          (fun () ->
            (* eu ingest host against the us private host: fails before any
               network request is attempted. *)
            AnT.install_validated_configuration ~enabled:"true"
              ~environment:"staging" ~project_token:"phc_test_token"
              ~api_host:"https://eu.i.posthog.com"
              ~ui_host:"https://us.posthog.com" ~project_id:"42"
              ~personal_api_key:"phx_test_dummy"
              ~public_origin:"https://staging.example.com" ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                match Lwt_main.run (An.Preflight.run ()) with
                | Error ("region_mismatch", _) -> ()
                | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                | Ok _ -> Alcotest.fail "must not verify"));
      ] );
  ]
