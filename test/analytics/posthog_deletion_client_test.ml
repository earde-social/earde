module An = Earde.Analytics
module AnT = Earde.Analytics.For_testing

let ( let* ) = Lwt.bind
let other_uuid = "99999999-8888-7777-6666-555555555555"

(* Configures the deletion client against the stub, runs [f], restores. *)
let with_persons_stub handler f =
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* base_url, seen, stop = Posthog_persons_stub.start handler in
     AnT.use_deletion_test_configuration ~ui_host:base_url
       ~project_id:(Some "42")
       ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
     Lwt.finalize
       (fun () -> f ~seen)
       (fun () ->
         AnT.clear_configuration_override ();
         stop ();
         Lwt.return_unit))

(* Reference stub behavior: bearer-authenticated lookup of user:314 returns
   [persons]; DELETE of stub_uuid returns [delete_status]. *)
let persons_handler ?(lookup_status = 200)
    ?(persons =
      [ Posthog_persons_stub.person_json Posthog_persons_stub.stub_uuid ])
    ?lookup_body_override ?(delete_status = 204) ()
    (req : Posthog_persons_stub.req) =
  if req.auth <> Some ("Bearer " ^ Posthog_persons_stub.deletion_test_key) then
    (401, {|{"type":"authentication_error"}|})
  else if req.meth = "GET" && req.path = "/api/projects/42/persons/" then
    match lookup_body_override with
    | Some body -> (lookup_status, body)
    | None ->
        if List.assoc_opt "distinct_id" req.query = Some [ "user:314" ] then
          (lookup_status, Posthog_persons_stub.results_body persons)
        else (200, Posthog_persons_stub.results_body [])
  else if
    req.meth = "DELETE"
    && req.path
       = "/api/projects/42/persons/" ^ Posthog_persons_stub.stub_uuid ^ "/"
  then (delete_status, "")
  else (404, {|{"detail":"not found"}|})

let attempt_result = Alcotest.(result unit string)

let check_attempt name handler expected ~expect_delete =
  Analytics_fixture.an_case name (fun () ->
      with_persons_stub handler (fun ~seen ->
          let ( let* ) = Lwt.bind in
          let* r =
            Earde.Posthog_deletion.attempt_person_deletion
              ~distinct_id:"user:314"
          in
          Alcotest.(check attempt_result) name expected r;
          let deletes =
            List.filter
              (fun (q : Posthog_persons_stub.req) -> q.meth = "DELETE")
              !seen
          in
          Alcotest.(check int)
            (name ^ ": DELETE requests")
            (if expect_delete then 1 else 0)
            (List.length deletes);
          Lwt.return_unit))

let suites =
  [
    ( "posthog_deletion_api",
      [
        check_attempt "exact match: delete accepted -> completed"
          (persons_handler ()) (Ok ()) ~expect_delete:true;
        Analytics_fixture.an_case
          "delete carries delete_events=true for the looked-up uuid" (fun () ->
            with_persons_stub (persons_handler ()) (fun ~seen ->
                let ( let* ) = Lwt.bind in
                let* r =
                  Earde.Posthog_deletion.attempt_person_deletion
                    ~distinct_id:"user:314"
                in
                Alcotest.(check attempt_result) "completed" (Ok ()) r;
                (match !seen with
                | [ lookup; delete ] ->
                    Alcotest.(check string)
                      "lookup meth" "GET" lookup.Posthog_persons_stub.meth;
                    Alcotest.(check (option (list string)))
                      "exact URL-encoded distinct id" (Some [ "user:314" ])
                      (List.assoc_opt "distinct_id"
                         lookup.Posthog_persons_stub.query);
                    Alcotest.(check string)
                      "delete path"
                      ("/api/projects/42/persons/"
                     ^ Posthog_persons_stub.stub_uuid ^ "/")
                      delete.Posthog_persons_stub.path;
                    Alcotest.(check (option (list string)))
                      "delete_events=true" (Some [ "true" ])
                      (List.assoc_opt "delete_events"
                         delete.Posthog_persons_stub.query)
                | l ->
                    Alcotest.failf "expected lookup+delete, saw %d"
                      (List.length l));
                Lwt.return_unit));
        check_attempt "absent person -> completed without DELETE"
          (persons_handler ~persons:[] ())
          (Ok ()) ~expect_delete:false;
        check_attempt "ambiguous lookup -> pending, nobody deleted"
          (persons_handler
             ~persons:
               [
                 Posthog_persons_stub.person_json Posthog_persons_stub.stub_uuid;
                 Posthog_persons_stub.person_json other_uuid;
               ]
             ())
          (Error "ambiguous_person_match") ~expect_delete:false;
        check_attempt "malformed lookup JSON -> pending"
          (persons_handler ~lookup_body_override:"not json at all" ())
          (Error "malformed_lookup_response") ~expect_delete:false;
        check_attempt "lookup 401 -> pending with safe class"
          (persons_handler ~lookup_status:401
             ~lookup_body_override:{|{"type":"authentication_error"}|} ())
          (Error "lookup_http_401") ~expect_delete:false;
        check_attempt "lookup 500 -> pending with safe class"
          (persons_handler ~lookup_status:500
             ~lookup_body_override:{|{"detail":"boom"}|} ())
          (Error "lookup_http_500") ~expect_delete:false;
        check_attempt "delete 403 -> pending with safe class"
          (persons_handler ~delete_status:403 ())
          (Error "delete_http_403") ~expect_delete:true;
        Analytics_fixture.an_case
          "unreachable host -> network_error, never a raw exception" (fun () ->
            AnT.use_deletion_test_configuration ~ui_host:"http://127.0.0.1:9"
              ~project_id:(Some "42")
              ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let r =
                  Lwt_main.run
                    (Earde.Posthog_deletion.attempt_person_deletion
                       ~distinct_id:"user:314")
                in
                Alcotest.(check attempt_result)
                  "network error class" (Error "network_error") r));
        Analytics_fixture.an_case "missing configuration -> safe pending marker"
          (fun () ->
            AnT.use_deletion_test_configuration ~ui_host:"http://earde.test"
              ~project_id:None
              ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let r =
                  Lwt_main.run
                    (Earde.Posthog_deletion.attempt_person_deletion
                       ~distinct_id:"user:314")
                in
                Alcotest.(check attempt_result)
                  "missing configuration" (Error "missing_configuration") r));
        Analytics_fixture.an_case
          "account_deleted metric is personless by construction" (fun () ->
            let captured =
              Analytics_fixture.with_sink ~enabled:true (fun () ->
                  Lwt_main.run
                    (An.capture_account_deleted_sequenced
                       (Analytics_fixture.consent_request
                          (Some "earde_analytics_consent=granted"))))
            in
            match captured with
            | [ p ] ->
                Alcotest.(check string)
                  "event" "account_deleted"
                  (Analytics_fixture.event_of p);
                Alcotest.(check string)
                  "constant non-user distinct id"
                  An.account_deletion_distinct_id
                  (Analytics_fixture.distinct_of p);
                Alcotest.(check (slist string compare))
                  "person processing disabled, plus only the envelope"
                  [ "$process_person_profile"; "deployment_environment" ]
                  (Analytics_fixture.prop_keys p);
                (match
                   List.assoc_opt "$process_person_profile"
                     (Analytics_fixture.payload_props p)
                 with
                | Some (`Bool false) -> ()
                | _ -> Alcotest.fail "$process_person_profile must be false");
                let raw = Yojson.Safe.to_string p in
                Alcotest.(check bool)
                  "no user identity anywhere" false
                  (Html_assert.contains raw "user:");
                Alcotest.(check bool)
                  "no person $set" false
                  (Html_assert.contains raw "$set");
                Alcotest.(check bool)
                  "no community group" false
                  (Html_assert.contains raw "$groups")
            | l -> Alcotest.failf "expected 1 metric, got %d" (List.length l));
        Analytics_fixture.an_case
          "denied/missing consent emits no deletion metric" (fun () ->
            let denied =
              Analytics_fixture.with_sink ~enabled:true (fun () ->
                  Lwt_main.run
                    (An.capture_account_deleted_sequenced
                       (Analytics_fixture.consent_request
                          (Some "earde_analytics_consent=denied"))))
            in
            Alcotest.(check int) "denied emits none" 0 (List.length denied);
            let missing =
              Analytics_fixture.with_sink ~enabled:true (fun () ->
                  Lwt_main.run
                    (An.capture_account_deleted_sequenced
                       (Analytics_fixture.consent_request None)))
            in
            Alcotest.(check int) "missing emits none" 0 (List.length missing));
      ] );
  ]
