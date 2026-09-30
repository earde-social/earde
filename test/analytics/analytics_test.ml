module An = Earde.Analytics
module AnT = Earde.Analytics.For_testing
let ( let* ) = Lwt.bind

(* PostHog analytics module (lib/analytics.ml): pure payload builders, exact
   consent-cookie parsing, and the consent gate exercised through the test
   capture sink. No network, no DB, no real PostHog project — the sink
   replaces the HTTP transport entirely. *)

let consent_str = function
  | `Granted -> "granted"
  | `Denied -> "denied"
  | `Unknown -> "unknown"

let check_consent name expected header =
  Analytics_fixture.an_case name (fun () ->
      Alcotest.(check string)
        name expected
        (consent_str (AnT.consent_of_cookie_header header)))

let check_distinct name expected id =
  Analytics_fixture.an_case name (fun () ->
      Alcotest.(check string) name expected (An.distinct_id_of_user_id id))

let an_person =
  { An.username = "alice";
    signup_date = "2026-01-01T00:00:00Z"; is_admin = false }

let an_login = An.Account_logged_in { user_id = 7; person = an_person }

(* The exact closed $set object an_person must serialize to — email-free by
   contract: the stable identity is user:<id> and email never reaches
   PostHog. *)
let an_person_set : Yojson.Safe.t =
  `Assoc
    [ ("username", `String "alice")
    ; ("signup_date", `String "2026-01-01T00:00:00Z")
    ; ("is_admin", `Bool false)
    ]

let check_gate name expected_count cookie =
  Analytics_fixture.an_case name (fun () ->
      let captured =
        Analytics_fixture.with_sink ~enabled:true (fun () ->
            An.capture_if_consented (Analytics_fixture.consent_request cookie)
              ~distinct_id:"user:7" an_login)
      in
      Alcotest.(check int) name expected_count (List.length captured))

(* One instance of every event constructor, used for payload/allowlist tests. *)
let an_all_events =
  [
    ("account_signed_up", An.Account_signed_up { user_id = 1; person = an_person });
    ("account_logged_in", An.Account_logged_in { user_id = 1; person = an_person });
    ( "community_joined",
      An.Community_joined
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          community_visibility = "public" } );
    ("community_left", An.Community_left { user_id = 1; community_id = 7 });
    ( "chat_message_sent",
      An.Chat_message_sent
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          channel_id = 3; channel_slug = Some "general"; message_id = 91L;
          content_length = 42; response_mode = An.Response_json } );
    ( "forum_thread_created",
      An.Forum_thread_created
        { user_id = 1; community_id = 7; section_id = Some 2; post_id = 10;
          content_length = 100; has_link = true; has_mention = false } );
    ( "forum_comment_created",
      An.Forum_comment_created
        { user_id = 1; community_id = 7; post_id = 10; comment_id = 55;
          parent_comment_id = None; content_length = 9; has_mention = true } );
    ( "conversation_promoted",
      An.Conversation_promoted
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          channel_id = 3; channel_slug = Some "general"; section_id = None;
          post_id = 11; message_id = 91L; promoted_message_count = 4;
          promoted_participant_count = Some 2 } );
    ("account_deleted", An.Account_deleted);
    (* GitHub-anchored project and community-home funnels. Every one of
       these is deliberately identifier-poor; the shared allowlist and
       $set/$groups checks below therefore apply to them unchanged. *)
    ("github_app_install_started", An.Github_app_install_started { user_id = 1 });
    ("github_app_installed", An.Github_app_installed { user_id = 1 });
    ( "github_repositories_selected",
      An.Github_repositories_selected { user_id = 1; repository_count = 3 } );
    ( "github_project_created",
      An.Github_project_created
        { user_id = 1; project_id = 4242L;
          project_kind = Earde.Project_identity.Project; repository_count = 3 } );
    ("dedicated_home_provisioned", An.Dedicated_home_provisioned { user_id = 1 });
    ( "network_community_published",
      An.Network_community_published
        { user_id = 1; publication_visibility = An.Published_public } );
    ( "project_home_request_submitted",
      An.Project_home_request_submitted { user_id = 1 } );
    ( "project_home_request_reviewed",
      An.Project_home_request_reviewed
        { user_id = 1; decision = An.Review_accepted } );
    ( "project_home_removed",
      An.Project_home_removed
        { user_id = 1; removal_surface = An.Removal_project_route } );
  ]

(* The exact stable PostHog event name of every funnel event, paired with its
   exact closed property allowlist. One table drives the name assertions, the
   key assertions, and the "nothing else was added" sweep, so a constructor
   cannot gain a property without this list changing. *)
let an_funnel_contract =
  [ ("github_app_install_started", [ "user_id" ]);
    ("github_app_installed", [ "user_id" ]);
    ("github_repositories_selected", [ "user_id"; "repository_count" ]);
    ( "github_project_created",
      [ "user_id"; "project_id"; "project_kind"; "repository_count" ] );
    ("dedicated_home_provisioned", [ "user_id" ]);
    ("network_community_published", [ "user_id"; "publication_visibility" ]);
    ("project_home_request_submitted", [ "user_id" ]);
    ("project_home_request_reviewed", [ "user_id"; "decision" ]);
    ("project_home_removed", [ "user_id"; "removal_surface" ]);
  ]

(* Test payloads are built for the Development environment; the envelope
   value is asserted separately in the analytics_envelope suite. *)
let an_payload event =
  AnT.event_payload ~api_key:"phc_test" ~environment:An.Development
    ~distinct_id:"user:1" event

let check_keys name event expected =
  Analytics_fixture.an_case name (fun () ->
      Alcotest.(check (slist string compare))
        name expected
        (Analytics_fixture.prop_keys (an_payload event)))

let check_event_name name expected event =
  Analytics_fixture.an_case name (fun () ->
      match Analytics_fixture.payload_member "event" (an_payload event) with
      | Some (`String n) -> Alcotest.(check string) name expected n
      | _ -> Alcotest.fail "payload has no event name")

let check_group name expected event =
  Analytics_fixture.an_case name (fun () ->
      Alcotest.(check (option string))
        name expected
        (Analytics_fixture.an_group_key (an_payload event)))

(* Property readers for the funnel assertions: [None] means the property is
   absent OR carries another JSON shape, which is exactly what an omission
   assertion needs. *)
let an_string_prop payload name =
  match List.assoc_opt name (Analytics_fixture.payload_props payload) with
  | Some (`String s) -> Some s
  | _ -> None

let an_int_prop payload name =
  match List.assoc_opt name (Analytics_fixture.payload_props payload) with
  | Some (`Int n) -> Some n
  | _ -> None

(* project_id is an int64, encoded as `Intlit so a bigint never loses
   precision on the wire. *)
let an_intlit_prop payload name =
  match List.assoc_opt name (Analytics_fixture.payload_props payload) with
  | Some (`Intlit s) -> Some s
  | _ -> None

let check_string_prop label event name expected =
  Analytics_fixture.an_case label (fun () ->
      Alcotest.(check (option string))
        label expected
        (an_string_prop (an_payload event) name))

let read_analytics_js () =
  (* dune test runs in test/, dune exec from the project root. *)
  let path =
    if Sys.file_exists "../static/js/analytics.js" then
      "../static/js/analytics.js"
    else "static/js/analytics.js"
  in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let validate_result_str = function
  | Ok `Granted -> "granted"
  | Ok `Denied -> "denied"
  | Error (`Bad_request _) -> "bad_request"
  | Error (`Forbidden _) -> "forbidden"

(* The dummy origin baked into For_testing.use_enabled_test_configuration. *)
let test_origin = "http://earde.test"

let check_validate name expected ~content_type ~origin ~sec_fetch_site body =
  Analytics_fixture.an_case name (fun () ->
      AnT.use_enabled_test_configuration ();
      Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
          Alcotest.(check string) name expected
            (validate_result_str
               (An.validate_consent_request ~content_type ~origin
                  ~sec_fetch_site ~body))))

let consent_good_headers =
  [ ("Content-Type", "application/json")
  ; ("Origin", test_origin)
  ; ("Sec-Fetch-Site", "same-origin")
  ]

(* Runs the real handler on a mock request (deliberately WITHOUT session or
   sql middleware: a first-time visitor has neither) and returns
   (status, set-cookie header, sync payloads observed by the sink). *)
let run_consent ?(headers = consent_good_headers) body =
  let payloads = ref [] in
  AnT.use_enabled_test_configuration ();
  AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
  Fun.protect
    ~finally:(fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ())
    (fun () ->
      let request =
        Dream.request ~method_:`POST ~target:"/analytics/consent" ~headers body
      in
      let response =
        Lwt_main.run (Earde.Handlers.analytics_consent_handler request)
      in
      ( Dream.status_to_int (Dream.status response),
        Dream.header response "Set-Cookie",
        List.rev !payloads ))

let check_consent_reject name expected_status ?headers body =
  Analytics_fixture.an_case name (fun () ->
      let status, cookie, payloads = run_consent ?headers body in
      Alcotest.(check int) (name ^ " status") expected_status status;
      Alcotest.(check (option string)) (name ^ " no cookie") None cookie;
      Alcotest.(check int) (name ^ " no sync") 0 (List.length payloads))

(* Gated DB case: a real user row + sql_pool + memory sessions, so a granted
   authenticated request performs exactly one closed person-property sync. *)
let consent_sync_db_case =
  Db_fixture.returning_ids_db_case "granted consent syncs person props once (authed)"
    (fun _conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find Db_fixture.q_insert_user "step3ret_author" in
      let* author = Db_fixture.or_fail "author" author in
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some u -> u
        | None -> Alcotest.fail "gate env var vanished"
      in
      let payloads = ref [] in
      AnT.use_enabled_test_configuration ();
      AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
      Lwt.finalize
        (fun () ->
          let pipeline =
            Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
            let* () =
              Dream.set_session_field req "user_id" (string_of_int author)
            in
            Earde.Handlers.analytics_consent_handler req
          in
          let request =
            Dream.request ~method_:`POST ~target:"/analytics/consent"
              ~headers:consent_good_headers {|{"state":"granted"}|}
          in
          let* response = pipeline request in
          Alcotest.(check int) "status" 204
            (Dream.status_to_int (Dream.status response));
          (match !payloads with
           | [ payload ] ->
               (match Analytics_fixture.payload_member "event" payload with
                | Some (`String e) ->
                    Alcotest.(check string) "event" "$identify" e
                | _ -> Alcotest.fail "sync payload has no event");
               (match Analytics_fixture.payload_member "distinct_id" payload with
                | Some (`String d) ->
                    Alcotest.(check string) "distinct id"
                      ("user:" ^ string_of_int author)
                      d
                | _ -> Alcotest.fail "sync payload has no distinct_id");
               (match List.assoc_opt "$set" (Analytics_fixture.payload_props payload) with
                | Some (`Assoc set) ->
                    Alcotest.(check (option string)) "username"
                      (Some "step3ret_author")
                      (match List.assoc_opt "username" set with
                       | Some (`String u) -> Some u
                       | _ -> None);
                    Alcotest.(check bool) "email absent" false
                      (List.mem_assoc "email" set);
                    Alcotest.(check (slist string compare)) "sync $set keys"
                      [ "username"; "signup_date"; "is_admin" ]
                      (List.map fst set)
                | _ -> Alcotest.fail "sync payload has no $set")
           | l ->
               Alcotest.failf "expected exactly 1 sync, got %d" (List.length l));
          Lwt.return_unit)
        (fun () ->
          AnT.clear_capture_sink ();
          AnT.clear_configuration_override ();
          Lwt.return_unit))

(* --- Step-6: consent-gated $groupidentify API + domain-event wiring ------- *)

let an_group ?created_at () : An.community_group =
  { An.community_id = 7; community_slug = Some "ocaml";
    community_name = Some "OCaml"; community_visibility = "public";
    created_at }

(* The §13 private shape: numeric id and closed visibility only — no
   readable identifiers. *)
let an_private_group : An.community_group =
  { An.community_id = 9; community_slug = None; community_name = None;
    community_visibility = "private"; created_at = None }

let run_group_identify ~enabled cookie =
  Analytics_fixture.with_sink ~enabled (fun () ->
      An.identify_community_if_consented (Analytics_fixture.consent_request cookie)
        ~distinct_id:"user:7" (an_group ()))

let check_group_identify_gate name expected ~enabled cookie =
  Analytics_fixture.an_case name (fun () ->
      Alcotest.(check int)
        name expected
        (List.length (run_group_identify ~enabled cookie)))

let suites =
    (* PostHog distinct-ID scheme: exactly user:<database_id>. *)
  [ ( "analytics_distinct_id"
    , [ check_distinct "user 42" "user:42" 42
      ; check_distinct "user 7" "user:7" 7
      ] )
    (* Exact consent-cookie parsing: only the exact values granted/denied
       count; everything else (missing, malformed, wrong case, extra text)
       is unknown and must produce no capture. *)
  ; ( "analytics_consent_parse"
    , [ check_consent "granted" "granted"
          (Some "earde_analytics_consent=granted")
      ; check_consent "denied" "denied" (Some "earde_analytics_consent=denied")
      ; check_consent "missing header" "unknown" None
      ; check_consent "empty header" "unknown" (Some "")
      ; check_consent "other cookies only" "unknown" (Some "session=abc; theme=dark")
      ; check_consent "granted among other cookies" "granted"
          (Some "session=abc; earde_analytics_consent=granted; theme=dark")
      ; check_consent "wrong case" "unknown" (Some "earde_analytics_consent=Granted")
      ; check_consent "trailing junk in value" "unknown"
          (Some "earde_analytics_consent=granted-ish")
      ; check_consent "space inside value" "unknown"
          (Some "earde_analytics_consent= granted")
      ; check_consent "name prefix mismatch" "unknown"
          (Some "xearde_analytics_consent=granted")
      ; check_consent "no equals sign" "unknown" (Some "earde_analytics_consent")
      ] )
    (* Consent gate end-to-end through the sink: one capture for granted,
       none otherwise. *)
  ; ( "analytics_consent_gate"
    , [ check_gate "granted captures once" 1
          (Some "earde_analytics_consent=granted")
      ; check_gate "denied captures nothing" 0
          (Some "earde_analytics_consent=denied")
      ; check_gate "missing cookie captures nothing" 0 None
      ; check_gate "malformed value captures nothing" 0
          (Some "earde_analytics_consent=yes")
      ; Analytics_fixture.an_case "granted payload carries event name and distinct id" (fun () ->
            let captured =
              Analytics_fixture.with_sink ~enabled:true (fun () ->
                  An.capture_if_consented
                    (Analytics_fixture.consent_request (Some "earde_analytics_consent=granted"))
                    ~distinct_id:"user:7" an_login)
            in
            match captured with
            | [ payload ] ->
                Alcotest.(check (option string)) "event"
                  (Some "account_logged_in")
                  (match Analytics_fixture.payload_member "event" payload with
                   | Some (`String s) -> Some s
                   | _ -> None);
                Alcotest.(check (option string)) "distinct_id"
                  (Some "user:7")
                  (match Analytics_fixture.payload_member "distinct_id" payload with
                   | Some (`String s) -> Some s
                   | _ -> None)
            | l -> Alcotest.failf "expected 1 capture, got %d" (List.length l))
      ] )
    (* Per-constructor payloads: event names and the exact property key
       sets of the closed allowlist; optional fields omitted when None. *)
  ; ( "analytics_event_payloads"
    , List.map
        (fun (name, event) -> check_event_name ("name " ^ name) name event)
        an_all_events
      @ [ check_keys "account_signed_up keys (incl. $set)"
            (List.assoc "account_signed_up" an_all_events)
            [ "user_id"; "$set"; "deployment_environment" ]
        ; check_keys "account_logged_in keys (incl. $set)"
            (List.assoc "account_logged_in" an_all_events)
            [ "user_id"; "$set"; "deployment_environment" ]
        ; check_keys "community_joined keys"
            (List.assoc "community_joined" an_all_events)
            [ "user_id"; "community_id"; "community_slug";
              "community_visibility"; "$groups"; "deployment_environment" ]
        ; check_keys "community_left keys"
            (List.assoc "community_left" an_all_events)
            [ "user_id"; "community_id"; "$groups"; "deployment_environment" ]
        ; check_keys "chat_message_sent keys"
            (List.assoc "chat_message_sent" an_all_events)
            [ "user_id"; "community_id"; "community_slug"; "channel_id";
              "channel_slug"; "message_id"; "content_length";
              "response_mode"; "$groups"; "deployment_environment" ]
        ; check_keys "forum_thread_created keys"
            (List.assoc "forum_thread_created" an_all_events)
            [ "user_id"; "community_id"; "section_id"; "post_id";
              "content_length"; "has_link"; "has_mention"; "$groups";
              "deployment_environment" ]
        ; check_keys "forum_comment_created keys (no parent -> omitted)"
            (List.assoc "forum_comment_created" an_all_events)
            [ "user_id"; "community_id"; "post_id"; "comment_id";
              "content_length"; "has_mention"; "$groups";
              "deployment_environment" ]
        ; check_keys "conversation_promoted keys (no section -> omitted)"
            (List.assoc "conversation_promoted" an_all_events)
            [ "user_id"; "community_id"; "community_slug"; "channel_id";
              "channel_slug"; "post_id"; "message_id";
              "promoted_message_count"; "promoted_participant_count";
              "$groups"; "deployment_environment" ]
        ; check_keys "account_deleted keys (personless: no user_id)"
            An.Account_deleted
            [ "$process_person_profile"; "deployment_environment" ]
        ; Analytics_fixture.an_case "community_joined full payload" (fun () ->
              let expected : Yojson.Safe.t =
                `Assoc
                  [ ("api_key", `String "phc_test")
                  ; ("event", `String "community_joined")
                  ; ("distinct_id", `String "user:1")
                  ; ( "properties"
                    , `Assoc
                        [ ("user_id", `Int 1)
                        ; ("community_id", `Int 7)
                        ; ("community_slug", `String "ocaml")
                        ; ("community_visibility", `String "public")
                        ; ( "$groups"
                          , `Assoc [ ("community", `String "community:7") ] )
                        ; ("deployment_environment", `String "development")
                        ] )
                  ]
              in
              Alcotest.check Analytics_fixture.yojson "full payload" expected
                (an_payload (List.assoc "community_joined" an_all_events)))
        ] )
    (* Hard rule of §5.1/§4.3: no bodies, titles, or tokens anywhere;
       person properties never as ordinary top-level event properties —
       only inside $set, and $set only on the two identity events. *)
  ; ( "analytics_property_allowlist"
    , [ Analytics_fixture.an_case "no forbidden ordinary property on any event" (fun () ->
            let forbidden =
              [ "email"; "username"; "signup_date"; "is_admin"; "content";
                "body"; "title"; "query"; "token" ]
            in
            List.iter
              (fun (name, event) ->
                let keys = Analytics_fixture.prop_keys (an_payload event) in
                List.iter
                  (fun bad ->
                    if List.mem bad keys then
                      Alcotest.failf "%s carries forbidden property %s" name
                        bad)
                  forbidden)
              an_all_events)
      ; Analytics_fixture.an_case "$set only on account_signed_up and account_logged_in" (fun () ->
            List.iter
              (fun (name, event) ->
                let has_set =
                  List.mem_assoc "$set" (Analytics_fixture.payload_props (an_payload event))
                in
                let expected =
                  name = "account_signed_up" || name = "account_logged_in"
                in
                if has_set <> expected then
                  Alcotest.failf "%s: unexpected $set presence (%b)" name
                    has_set)
              an_all_events)
      ; Analytics_fixture.an_case "account_signed_up $set is exactly the closed person record"
          (fun () ->
            match
              List.assoc_opt "$set"
                (Analytics_fixture.payload_props
                   (an_payload (List.assoc "account_signed_up" an_all_events)))
            with
            | Some set -> Alcotest.check Analytics_fixture.yojson "signup $set" an_person_set set
            | None -> Alcotest.fail "account_signed_up has no $set")
      ; Analytics_fixture.an_case "account_logged_in $set is exactly the closed person record"
          (fun () ->
            match
              List.assoc_opt "$set"
                (Analytics_fixture.payload_props
                   (an_payload (List.assoc "account_logged_in" an_all_events)))
            with
            | Some set -> Alcotest.check Analytics_fixture.yojson "login $set" an_person_set set
            | None -> Alcotest.fail "account_logged_in has no $set")
        (* Pivot taxonomy invariants: the canonical closed name set (no
           pre-rename name, no not-yet-implemented GitHub onboarding
           event) and the email-free person contract on every $set-bearing
           payload, including the consent-transition sync. *)
      ; Analytics_fixture.an_case "server taxonomy is exactly the canonical closed set"
          (fun () ->
            Alcotest.(check (slist string compare))
              "emitted event names"
              [ "account_signed_up"; "account_logged_in"; "community_joined";
                "community_left"; "chat_message_sent";
                "forum_thread_created"; "forum_comment_created";
                "conversation_promoted"; "account_deleted";
                (* GitHub project and community-home funnels. The four
                   deliberately-not-in-this-slice names —
                   github_onboarding_failed,
                   github_repository_already_connected,
                   project_home_choice_viewed, bring_community_started —
                   and github_repositories_loaded (no single non-repeating
                   production boundary exists) must stay absent. *)
                "github_app_install_started"; "github_app_installed";
                "github_repositories_selected"; "github_project_created";
                "dedicated_home_provisioned"; "network_community_published";
                "project_home_request_submitted";
                "project_home_request_reviewed"; "project_home_removed" ]
              (List.map
                 (fun (_, event) ->
                   match Analytics_fixture.payload_member "event" (an_payload event) with
                   | Some (`String name) -> name
                   | _ -> "<missing>")
                 an_all_events))
      ; Analytics_fixture.an_case "no obsolete pre-pivot event name is ever emitted" (fun () ->
            let obsolete =
              [ "signup_confirmed"; "login_succeeded"; "post_created";
                "comment_created"; "thread_promoted";
                (* Explicitly out of scope for this slice; naming one of
                   them would create a production event nothing measures. *)
                "github_onboarding_failed";
                "github_repository_already_connected";
                "project_home_choice_viewed"; "bring_community_started";
                "github_repositories_loaded";
                (* The provisioning event must never be misnamed as a
                   publication: a provisioned home is still private. *)
                "community_published" ]
            in
            List.iter
              (fun (label, event) ->
                match Analytics_fixture.payload_member "event" (an_payload event) with
                | Some (`String name) ->
                    if List.mem name obsolete then
                      Alcotest.failf "%s emits obsolete name %s" label name
                | _ -> Alcotest.failf "%s: payload has no event name" label)
              an_all_events)
      ; Analytics_fixture.an_case "person $set never contains email" (fun () ->
            let check_set label payload =
              match List.assoc_opt "$set" (Analytics_fixture.payload_props payload) with
              | Some (`Assoc set) ->
                  if List.mem_assoc "email" set then
                    Alcotest.failf "%s: $set contains email" label
              | Some _ -> Alcotest.failf "%s: $set is not an object" label
              | None -> Alcotest.failf "%s: no $set" label
            in
            check_set "account_signed_up"
              (an_payload (List.assoc "account_signed_up" an_all_events));
            check_set "account_logged_in"
              (an_payload (List.assoc "account_logged_in" an_all_events));
            check_set "consent sync"
              (AnT.person_sync_payload ~api_key:"phc_test" ~environment:An.Development
                 ~distinct_id:"user:1" an_person))
      ] )
    (* $groups.community rides on community-scoped events only (§5.3). *)
  ; ( "analytics_groups"
    , [ check_group "joined has group" (Some "community:7")
          (List.assoc "community_joined" an_all_events)
      ; check_group "left has group" (Some "community:7")
          (List.assoc "community_left" an_all_events)
      ; check_group "chat has group" (Some "community:7")
          (List.assoc "chat_message_sent" an_all_events)
      ; check_group "post has group" (Some "community:7")
          (List.assoc "forum_thread_created" an_all_events)
      ; check_group "comment has group" (Some "community:7")
          (List.assoc "forum_comment_created" an_all_events)
      ; check_group "promoted has group" (Some "community:7")
          (List.assoc "conversation_promoted" an_all_events)
      ; check_group "signup has no group" None
          (List.assoc "account_signed_up" an_all_events)
      ; check_group "login has no group" None
          (List.assoc "account_logged_in" an_all_events)
      ; check_group "deletion has no group" None An.Account_deleted
      ] )
    (* ---- GitHub project / community-home funnel event contract ----

       Exact stable event names, exact closed property allowlists, exact
       closed value spellings, the centrally injected environment envelope,
       and the omission rules for out-of-range counts and non-positive ids.
       Everything here is pure: no request, no consent, no transport. *)
  ; ( "analytics_funnel_contract"
    , List.map
        (fun (name, _) ->
          check_event_name ("stable name " ^ name) name
            (List.assoc name an_all_events))
        an_funnel_contract
      @ List.map
          (fun (name, keys) ->
            check_keys
              ("closed property allowlist " ^ name)
              (List.assoc name an_all_events)
              (* deployment_environment is added by the shared envelope,
                 never by a constructor, so it belongs to every event. *)
              (keys @ [ "deployment_environment" ]))
          an_funnel_contract
      @ [ Analytics_fixture.an_case "no funnel event carries a group: the community group \
                   key needs a numeric id no success boundary has, and no \
                   second group type exists" (fun () ->
              List.iter
                (fun (name, _) ->
                  let payload = an_payload (List.assoc name an_all_events) in
                  Alcotest.(check (option string))
                    (name ^ ": no community group")
                    None (Analytics_fixture.an_group_key payload);
                  Alcotest.(check bool)
                    (name ^ ": no $groups key at all")
                    false
                    (List.mem_assoc "$groups" (Analytics_fixture.payload_props payload)))
                an_funnel_contract)
        ; Analytics_fixture.an_case "every funnel event is attributed to user:<id> and \
                   carries the matching user_id property" (fun () ->
              List.iter
                (fun (name, _) ->
                  let event = List.assoc name an_all_events in
                  let payload =
                    AnT.event_payload ~api_key:"phc_test"
                      ~environment:An.Development
                      ~distinct_id:(An.distinct_id_of_user_id 77)
                      event
                  in
                  Alcotest.(check (option Analytics_fixture.yojson))
                    (name ^ ": distinct id")
                    (Some (`String "user:77"))
                    (Analytics_fixture.payload_member "distinct_id" payload);
                  Alcotest.(check (option int))
                    (name ^ ": user_id property")
                    (Some 1) (an_int_prop payload "user_id"))
                an_funnel_contract)
          (* The envelope is injected centrally, once, by the shared
             payload builder. No constructor can supply, duplicate, or
             override it — the closed variants have no field for it. *)
        ; Analytics_fixture.an_case "deployment_environment is centrally injected on every \
                   funnel event, exactly once, with the caller's \
                   environment" (fun () ->
              List.iter
                (fun (name, _) ->
                  let event = List.assoc name an_all_events in
                  List.iter
                    (fun (env, spelling) ->
                      let payload =
                        AnT.event_payload ~api_key:"phc_test"
                          ~environment:env ~distinct_id:"user:1" event
                      in
                      Alcotest.(check (option string))
                        (name ^ ": " ^ spelling)
                        (Some spelling)
                        (an_string_prop payload "deployment_environment");
                      Alcotest.(check int)
                        (name ^ ": exactly one envelope key")
                        1
                        (List.length
                           (List.filter
                              (fun (k, _) -> k = "deployment_environment")
                              (Analytics_fixture.payload_props payload))))
                    [ (An.Production, "production");
                      (An.Staging, "staging");
                      (An.Development, "development")
                    ])
                an_funnel_contract)
        ; Analytics_fixture.an_case "the funnel property allowlist is closed: the union of \
                   every funnel event's keys is exactly the documented set"
            (fun () ->
              let union =
                List.sort_uniq compare
                  (List.concat_map
                     (fun (name, _) ->
                       Analytics_fixture.prop_keys (an_payload (List.assoc name an_all_events)))
                     an_funnel_contract)
              in
              Alcotest.(check (slist string compare))
                "closed union"
                [ "user_id"; "repository_count"; "project_id"; "project_kind";
                  "publication_visibility"; "decision"; "removal_surface";
                  "deployment_environment"
                ]
                union)
          (* Closed value spellings. A third spelling is unrepresentable —
             these types have exactly two constructors each. *)
        ; check_string_prop "review decision accepted"
            (An.Project_home_request_reviewed
               { user_id = 1; decision = An.Review_accepted })
            "decision" (Some "accepted")
        ; check_string_prop "review decision rejected"
            (An.Project_home_request_reviewed
               { user_id = 1; decision = An.Review_rejected })
            "decision" (Some "rejected")
        ; check_string_prop "publication visibility public"
            (An.Network_community_published
               { user_id = 1; publication_visibility = An.Published_public })
            "publication_visibility" (Some "public")
        ; check_string_prop "publication visibility unlisted"
            (An.Network_community_published
               { user_id = 1; publication_visibility = An.Published_unlisted })
            "publication_visibility" (Some "unlisted")
        ; check_string_prop "removal surface project route"
            (An.Project_home_removed
               { user_id = 1; removal_surface = An.Removal_project_route })
            "removal_surface" (Some "project")
        ; check_string_prop "removal surface community route"
            (An.Project_home_removed
               { user_id = 1; removal_surface = An.Removal_community_route })
            "removal_surface" (Some "community")
        ; Analytics_fixture.an_case "project_kind uses the exact closed Project_identity \
                   database spellings, with no stringified unknown"
            (fun () ->
              List.iter
                (fun (kind, spelling) ->
                  Alcotest.(check (option string))
                    spelling (Some spelling)
                    (an_string_prop
                       (an_payload
                          (An.Github_project_created
                             { user_id = 1; project_id = 1L;
                               project_kind = kind; repository_count = 1 }))
                       "project_kind"))
                [ (Earde.Project_identity.Project, "project");
                  (Earde.Project_identity.Organization, "organization");
                  (Earde.Project_identity.Ecosystem, "ecosystem");
                  (Earde.Project_identity.Foundation, "foundation");
                  (Earde.Project_identity.Working_group, "working_group");
                  (Earde.Project_identity.Other, "other")
                ])
        ; Analytics_fixture.an_case "project_id is an exact bigint literal, and a \
                   non-positive id is omitted rather than exported"
            (fun () ->
              let payload_for id =
                an_payload
                  (An.Github_project_created
                     { user_id = 1; project_id = id;
                       project_kind = Earde.Project_identity.Project;
                       repository_count = 1 })
              in
              Alcotest.(check (option string))
                "large id survives as a literal"
                (Some "9223372036854775807")
                (an_intlit_prop (payload_for Int64.max_int) "project_id");
              List.iter
                (fun (label, id) ->
                  Alcotest.(check bool)
                    (label ^ ": omitted")
                    false
                    (List.mem_assoc "project_id"
                       (Analytics_fixture.payload_props (payload_for id))))
                [ ("zero", 0L); ("negative", -1L); ("min_int", Int64.min_int) ])
        ; Analytics_fixture.an_case "repository_count keeps the whole legitimate domain range \
                   and omits anything outside it" (fun () ->
              let count_of n =
                an_int_prop
                  (an_payload
                     (An.Github_repositories_selected
                        { user_id = 1; repository_count = n }))
                  "repository_count"
              in
              (* Zero is a real committed transition: a deliberately
                 cleared selection. *)
              Alcotest.(check (option int)) "zero kept" (Some 0) (count_of 0);
              Alcotest.(check (option int)) "one kept" (Some 1) (count_of 1);
              Alcotest.(check (option int))
                "snapshot maximum kept" (Some 2000) (count_of 2000);
              List.iter
                (fun (label, n) ->
                  Alcotest.(check (option int))
                    (label ^ ": omitted")
                    None (count_of n))
                [ ("negative", -1); ("above the snapshot bound", 2001);
                  ("absurd", max_int)
                ];
              (* The same rule on the creation event. *)
              Alcotest.(check bool)
                "creation event omits an out-of-range count" false
                (List.mem_assoc "repository_count"
                   (Analytics_fixture.payload_props
                      (an_payload
                         (An.Github_project_created
                            { user_id = 1; project_id = 5L;
                              project_kind = Earde.Project_identity.Project;
                              repository_count = -3 })))))
        ; Analytics_fixture.an_case "github_project_created full payload" (fun () ->
              let expected : Yojson.Safe.t =
                `Assoc
                  [ ("api_key", `String "phc_test")
                  ; ("event", `String "github_project_created")
                  ; ("distinct_id", `String "user:1")
                  ; ( "properties"
                    , `Assoc
                        [ ("user_id", `Int 1)
                        ; ("project_id", `Intlit "4242")
                        ; ("project_kind", `String "project")
                        ; ("repository_count", `Int 3)
                        ; ("deployment_environment", `String "development")
                        ] )
                  ]
              in
              Alcotest.check Analytics_fixture.yojson "full payload" expected
                (an_payload (List.assoc "github_project_created" an_all_events)))
        ; Analytics_fixture.an_case "project_home_request_reviewed full payload" (fun () ->
              let expected : Yojson.Safe.t =
                `Assoc
                  [ ("api_key", `String "phc_test")
                  ; ("event", `String "project_home_request_reviewed")
                  ; ("distinct_id", `String "user:1")
                  ; ( "properties"
                    , `Assoc
                        [ ("user_id", `Int 1)
                        ; ("decision", `String "accepted")
                        ; ("deployment_environment", `String "development")
                        ] )
                  ]
              in
              Alcotest.check Analytics_fixture.yojson "full payload" expected
                (an_payload
                   (List.assoc "project_home_request_reviewed" an_all_events)))
          (* Nothing credential- or identity-shaped may appear anywhere in
             a funnel payload, whatever the fixture values are. *)
        ; Analytics_fixture.an_case "no funnel payload can carry an identity, credential, or \
                   free-text property name" (fun () ->
              let forbidden =
                [ "email"; "username"; "login"; "github_login"; "namespace";
                  "installation_id"; "github_installation_id"; "account_id";
                  "repository"; "repository_name"; "repository_full_name";
                  "repositories"; "repository_id"; "html_url"; "url";
                  "project_name"; "project_slug"; "slug"; "community_id";
                  "community_name"; "community_slug"; "note"; "request_note";
                  "review_note"; "description"; "state"; "code"; "verifier";
                  "code_verifier"; "token"; "error"; "authorization_source";
                  "session"
                ]
              in
              List.iter
                (fun (name, _) ->
                  let keys =
                    Analytics_fixture.prop_keys (an_payload (List.assoc name an_all_events))
                  in
                  List.iter
                    (fun bad ->
                      if List.mem bad keys then
                        Alcotest.failf "%s carries forbidden property %s"
                          name bad)
                    forbidden)
                an_funnel_contract)
        ] )
    (* Consent and configuration for the funnel events: the identical gate
       the existing events use, driven through the fake sink. No network. *)
  ; ( "analytics_funnel_consent"
    , [ Analytics_fixture.an_case "granted consent on an enabled configuration captures \
                 exactly one payload per funnel event" (fun () ->
            List.iter
              (fun (name, _) ->
                let event = List.assoc name an_all_events in
                let captured =
                  Analytics_fixture.with_sink ~enabled:true (fun () ->
                      An.capture_if_consented
                        (Analytics_fixture.consent_request
                           (Some "earde_analytics_consent=granted"))
                        ~distinct_id:"user:1" event)
                in
                Alcotest.(check int) (name ^ ": one capture") 1
                  (List.length captured);
                match captured with
                | [ payload ] ->
                    Alcotest.(check (option Analytics_fixture.yojson))
                      (name ^ ": stable event name")
                      (Some (`String name))
                      (Analytics_fixture.payload_member "event" payload)
                | _ -> Alcotest.fail "expected exactly one payload")
              an_funnel_contract)
      ; Analytics_fixture.an_case "denied, missing, and malformed consent capture nothing"
          (fun () ->
            List.iter
              (fun (name, _) ->
                let event = List.assoc name an_all_events in
                List.iter
                  (fun (label, cookie) ->
                    let captured =
                      Analytics_fixture.with_sink ~enabled:true (fun () ->
                          An.capture_if_consented (Analytics_fixture.consent_request cookie)
                            ~distinct_id:"user:1" event)
                    in
                    Alcotest.(check int)
                      (name ^ ": " ^ label ^ " captures nothing")
                      0 (List.length captured))
                  [ ("denied", Some "earde_analytics_consent=denied");
                    ("missing", None);
                    ("no consent cookie", Some "session=abc; theme=dark");
                    ("malformed value",
                     Some "earde_analytics_consent=granted-ish");
                    ("wrong case", Some "earde_analytics_consent=Granted")
                  ])
              an_funnel_contract)
      ; Analytics_fixture.an_case "disabled analytics captures nothing even with granted \
                 consent" (fun () ->
            List.iter
              (fun (name, _) ->
                let captured =
                  Analytics_fixture.with_sink ~enabled:false (fun () ->
                      An.capture_if_consented
                        (Analytics_fixture.consent_request
                           (Some "earde_analytics_consent=granted"))
                        ~distinct_id:"user:1"
                        (List.assoc name an_all_events))
                in
                Alcotest.(check int) (name ^ ": no capture") 0
                  (List.length captured))
              an_funnel_contract)
      ; Analytics_fixture.an_case "a transport failure is swallowed: the caller sees unit and \
                 no exception escapes" (fun () ->
            (* The sink stands in for the HTTP transport, and dispatch
               swallows sink failures exactly like network failures. A
               handler calling capture can therefore never have its
               response changed by PostHog. *)
            AnT.use_enabled_test_configuration ();
            AnT.set_capture_sink (fun _ -> failwith "posthog is down");
            Fun.protect
              ~finally:(fun () ->
                AnT.clear_capture_sink ();
                AnT.clear_configuration_override ())
              (fun () ->
                List.iter
                  (fun (name, _) ->
                    match
                      An.capture_if_consented
                        (Analytics_fixture.consent_request
                           (Some "earde_analytics_consent=granted"))
                        ~distinct_id:"user:1"
                        (List.assoc name an_all_events)
                    with
                    | () -> ()
                    | exception e ->
                        Alcotest.failf "%s: capture raised %s" name
                          (Printexc.to_string e))
                  an_funnel_contract))
      ] )
    (* Consent-transition sync: the dedicated $identify payload with the
       same closed $set object as the identity events. *)
  ; ( "analytics_person_sync"
    , [ Analytics_fixture.an_case "sync payload is a dedicated $identify" (fun () ->
            let expected : Yojson.Safe.t =
              `Assoc
                [ ("api_key", `String "phc_test")
                ; ("event", `String "$identify")
                ; ("distinct_id", `String "user:9")
                ; ( "properties"
                  , `Assoc
                      [ ("$set", an_person_set)
                      ; ("deployment_environment", `String "development")
                      ] )
                ]
            in
            let actual =
              AnT.person_sync_payload ~api_key:"phc_test" ~environment:An.Development
                ~distinct_id:"user:9" an_person
            in
            Alcotest.check Analytics_fixture.yojson "sync payload" expected actual)
      ; Analytics_fixture.an_case "sync_person_after_consent_grant emits exactly one $identify"
          (fun () ->
            let captured =
              Analytics_fixture.with_sink ~enabled:true (fun () ->
                  An.sync_person_after_consent_grant ~distinct_id:"user:9"
                    an_person)
            in
            match captured with
            | [ payload ] ->
                Alcotest.(check (option string)) "event" (Some "$identify")
                  (match Analytics_fixture.payload_member "event" payload with
                   | Some (`String s) -> Some s
                   | _ -> None)
            | l -> Alcotest.failf "expected 1 capture, got %d" (List.length l))
      ] )
    (* $groupidentify: community group type/key plus only the five
       allowlisted group properties; created_at omitted when absent. *)
  ; ( "analytics_group_identify"
    , [ Analytics_fixture.an_case "full payload with created_at" (fun () ->
            let expected : Yojson.Safe.t =
              `Assoc
                [ ("api_key", `String "phc_test")
                ; ("event", `String "$groupidentify")
                ; ("distinct_id", `String "user:1")
                ; ( "properties"
                  , `Assoc
                      [ ("$group_type", `String "community")
                      ; ("$group_key", `String "community:7")
                      ; ( "$group_set"
                        , `Assoc
                            [ ("community_id", `Int 7)
                            ; ("community_slug", `String "ocaml")
                            ; ("community_name", `String "OCaml")
                            ; ("community_visibility", `String "public")
                            ; ("created_at", `String "2026-01-01T00:00:00Z")
                            ] )
                      ; ("deployment_environment", `String "development")
                      ] )
                ]
            in
            let actual =
              AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                ~distinct_id:"user:1"
                { An.community_id = 7; community_slug = Some "ocaml";
                  community_name = Some "OCaml";
                  community_visibility = "public";
                  created_at = Some "2026-01-01T00:00:00Z" }
            in
            Alcotest.check Analytics_fixture.yojson "group identify payload" expected actual)
      ; Analytics_fixture.an_case "created_at omitted when absent" (fun () ->
            let actual =
              AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                ~distinct_id:"user:1"
                { An.community_id = 7; community_slug = Some "ocaml";
                  community_name = Some "OCaml";
                  community_visibility = "public"; created_at = None }
            in
            let set_keys =
              match List.assoc_opt "$group_set" (Analytics_fixture.payload_props actual) with
              | Some (`Assoc l) -> List.map fst l
              | _ -> []
            in
            Alcotest.(check (slist string compare))
              "group set keys"
              [ "community_id"; "community_slug"; "community_name";
                "community_visibility" ]
              set_keys)
        (* §13: fully private communities. Group identity stays
           community:<id>; readable identifiers never appear. *)
      ; Analytics_fixture.an_case "private $groupidentify has no name or slug" (fun () ->
            let actual =
              AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                ~distinct_id:"user:1" an_private_group
            in
            Alcotest.(check (option string)) "group key stays numeric"
              (Some "community:9") (Analytics_fixture.group_key_prop_of actual);
            let set = Analytics_fixture.group_set_of actual in
            Alcotest.(check (slist string compare))
              "private group set keys"
              [ "community_id"; "community_visibility" ]
              (List.map fst set);
            Alcotest.(check bool) "no community_name" false
              (List.mem_assoc "community_name" set);
            Alcotest.(check bool) "no community_slug" false
              (List.mem_assoc "community_slug" set))
      ; Analytics_fixture.an_case "private domain events omit slugs, keep ids/counts/group"
          (fun () ->
            let chat =
              an_payload
                (An.Chat_message_sent
                   { user_id = 1; community_id = 9; community_slug = None;
                     channel_id = 3; channel_slug = None; message_id = 91L;
                     content_length = 42; response_mode = An.Response_json })
            in
            Alcotest.(check (slist string compare))
              "private chat keys"
              [ "user_id"; "community_id"; "channel_id"; "message_id";
                "content_length"; "response_mode"; "$groups";
                "deployment_environment" ]
              (Analytics_fixture.prop_keys chat);
            Alcotest.(check (option string)) "private chat group"
              (Some "community:9") (Analytics_fixture.an_group_key chat);
            let joined =
              an_payload
                (An.Community_joined
                   { user_id = 1; community_id = 9; community_slug = None;
                     community_visibility = "private" })
            in
            Alcotest.(check (slist string compare))
              "private join keys"
              [ "user_id"; "community_id"; "community_visibility";
                "$groups"; "deployment_environment" ]
              (Analytics_fixture.prop_keys joined);
            let promoted =
              an_payload
                (An.Conversation_promoted
                   { user_id = 1; community_id = 9; community_slug = None;
                     channel_id = 3; channel_slug = None; section_id = None;
                     post_id = 11; message_id = 91L;
                     promoted_message_count = 4;
                     promoted_participant_count = Some 2 })
            in
            Alcotest.(check (slist string compare))
              "private promoted keys"
              [ "user_id"; "community_id"; "channel_id"; "post_id";
                "message_id"; "promoted_message_count";
                "promoted_participant_count"; "$groups";
                "deployment_environment" ]
              (Analytics_fixture.prop_keys promoted))
      ] )
    (* Disabled configuration and transport failures: never a capture, never
       an exception into the caller. *)
  ; ( "analytics_disabled_and_failures"
    , [ Analytics_fixture.an_case "disabled config captures nothing even when granted"
          (fun () ->
            let captured =
              Analytics_fixture.with_sink ~enabled:false (fun () ->
                  An.capture_if_consented
                    (Analytics_fixture.consent_request (Some "earde_analytics_consent=granted"))
                    ~distinct_id:"user:7" an_login;
                  An.sync_person_after_consent_grant ~distinct_id:"user:7"
                    { An.username = "a"; signup_date = "";
                      is_admin = false })
            in
            Alcotest.(check int) "no captures" 0 (List.length captured))
      ; Analytics_fixture.an_case "raising sink does not escape into the caller" (fun () ->
            AnT.use_enabled_test_configuration ();
            AnT.set_capture_sink (fun _ -> failwith "sink boom");
            Fun.protect
              ~finally:(fun () ->
                AnT.clear_capture_sink ();
                AnT.clear_configuration_override ())
              (fun () ->
                An.capture_if_consented
                  (Analytics_fixture.consent_request (Some "earde_analytics_consent=granted"))
                  ~distinct_id:"user:7" an_login;
                An.sync_person_after_consent_grant ~distinct_id:"user:7"
                  { An.username = "a"; signup_date = "";
                    is_admin = false });
            Alcotest.(check bool) "no exception escaped" true true)
      ; Analytics_fixture.an_case "test config report exposes presence booleans only" (fun () ->
            AnT.use_enabled_test_configuration ();
            Fun.protect
              ~finally:(fun () -> AnT.clear_configuration_override ())
              (fun () ->
                let report = AnT.config_report () in
                Alcotest.(check (option bool)) "enabled" (Some true)
                  (List.assoc_opt "POSTHOG_ENABLED" report);
                Alcotest.(check (option bool)) "token set" (Some true)
                  (List.assoc_opt "POSTHOG_PROJECT_TOKEN" report);
                Alcotest.(check (option bool)) "personal key unset"
                  (Some false)
                  (List.assoc_opt "POSTHOG_PERSONAL_API_KEY" report);
                Alcotest.(check (option bool)) "project id unset" (Some false)
                  (List.assoc_opt "POSTHOG_PROJECT_ID" report)))
      ] )
    (* §9 request validation matrix: JSON-only, exactly-one-field body,
       exact Origin, same-origin/same-site Sec-Fetch-Site. *)
  ; ( "analytics_consent_validate"
    , [ check_validate "granted ok" "granted"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
      ; check_validate "denied ok" "denied"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-site") {|{"state":"denied"}|}
      ; check_validate "json with charset ok" "granted"
          ~content_type:(Some "application/json; charset=utf-8")
          ~origin:(Some test_origin) ~sec_fetch_site:(Some "same-origin")
          {|{"state":"granted"}|}
      ; check_validate "form content-type rejected" "bad_request"
          ~content_type:(Some "application/x-www-form-urlencoded")
          ~origin:(Some test_origin) ~sec_fetch_site:(Some "same-origin")
          "state=granted"
      ; check_validate "missing content-type rejected" "bad_request"
          ~content_type:None ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
      ; check_validate "malformed json rejected" "bad_request"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") "{state:"
      ; check_validate "additional field rejected" "bad_request"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin")
          {|{"state":"granted","extra":1}|}
      ; check_validate "invalid state rejected" "bad_request"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") {|{"state":"yes"}|}
      ; check_validate "missing state rejected" "bad_request"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") {|{}|}
      ; check_validate "non-object body rejected" "bad_request"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "same-origin") {|"granted"|}
      ; check_validate "wrong origin rejected" "forbidden"
          ~content_type:(Some "application/json")
          ~origin:(Some "https://evil.example") ~sec_fetch_site:(Some "same-origin")
          {|{"state":"granted"}|}
      ; check_validate "missing origin rejected" "forbidden"
          ~content_type:(Some "application/json") ~origin:None
          ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
      ; check_validate "cross-site fetch rejected" "forbidden"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:(Some "cross-site") {|{"state":"granted"}|}
      ; check_validate "missing sec-fetch-site rejected" "forbidden"
          ~content_type:(Some "application/json") ~origin:(Some test_origin)
          ~sec_fetch_site:None {|{"state":"granted"}|}
      ] )
    (* The real handler on mock requests: cookie contract, controlled JSON
       errors, no session required, failures isolated. *)
  ; ( "analytics_consent_endpoint"
    , [ Analytics_fixture.an_case "granted: 204 + exact cookie, no session needed, no sync"
          (fun () ->
            let status, cookie, payloads =
              run_consent {|{"state":"granted"}|}
            in
            Alcotest.(check int) "status" 204 status;
            let cookie = Option.value ~default:"" cookie in
            Alcotest.(check bool) "value" true
              (Html_assert.contains cookie "earde_analytics_consent=granted");
            Alcotest.(check bool) "path" true (Html_assert.contains cookie "Path=/");
            Alcotest.(check bool) "max-age" true
              (Html_assert.contains cookie "Max-Age=15552000");
            Alcotest.(check bool) "samesite lax" true
              (Html_assert.contains cookie "SameSite=Lax");
            Alcotest.(check bool) "no httponly" false
              (Html_assert.contains cookie "HttpOnly");
            Alcotest.(check bool) "no secure on http origin" false
              (Html_assert.contains cookie "Secure");
            Alcotest.(check int) "anonymous grant syncs nothing" 0
              (List.length payloads))
      ; Analytics_fixture.an_case "denied: 204 + denied cookie, no sync" (fun () ->
            let status, cookie, payloads = run_consent {|{"state":"denied"}|} in
            Alcotest.(check int) "status" 204 status;
            Alcotest.(check bool) "value" true
              (Html_assert.contains (Option.value ~default:"" cookie)
                 "earde_analytics_consent=denied");
            Alcotest.(check int) "no sync" 0 (List.length payloads))
      ; Analytics_fixture.an_case "production https: exact cookie name, no __Host- prefix"
          (fun () ->
            (* Dream infers a __Host- prefix for a Secure + Path=/ cookie
               unless ~prefix:None is passed explicitly; the http-origin
               cases above never set Secure, so only this validated
               production (https) configuration can catch the regression. *)
            Analytics_fixture.install_production_config ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let request =
                  Dream.request ~method_:`POST ~target:"/analytics/consent"
                    ~headers:
                      [ ("Content-Type", "application/json")
                      ; ("Origin", "https://earde.com")
                      ; ("Sec-Fetch-Site", "same-origin")
                      ]
                    {|{"state":"granted"}|}
                in
                let response =
                  Lwt_main.run
                    (Earde.Handlers.analytics_consent_handler request)
                in
                Alcotest.(check int) "status" 204
                  (Dream.status_to_int (Dream.status response));
                let cookie =
                  Option.value ~default:""
                    (Dream.header response "Set-Cookie")
                in
                Alcotest.(check bool) "exact name=value" true
                  (Html_assert.contains cookie "earde_analytics_consent=granted");
                Alcotest.(check bool) "no __Host- prefix" false
                  (Html_assert.contains cookie "__Host-earde_analytics_consent");
                Alcotest.(check bool) "secure" true (Html_assert.contains cookie "Secure");
                Alcotest.(check bool) "path" true (Html_assert.contains cookie "Path=/");
                Alcotest.(check bool) "samesite lax" true
                  (Html_assert.contains cookie "SameSite=Lax");
                Alcotest.(check string) "parser recognizes returned cookie"
                  "granted"
                  (consent_str
                     (AnT.consent_of_cookie_header (Some cookie)))))
      ; check_consent_reject "form body -> 400" 400
          ~headers:
            [ ("Content-Type", "application/x-www-form-urlencoded")
            ; ("Origin", test_origin)
            ; ("Sec-Fetch-Site", "same-origin")
            ]
          "state=granted"
      ; check_consent_reject "extra field -> 400" 400
          {|{"state":"granted","x":1}|}
      ; check_consent_reject "invalid state -> 400" 400 {|{"state":"maybe"}|}
      ; check_consent_reject "malformed json -> 400" 400 "{"
      ; check_consent_reject "bad origin -> 403" 403
          ~headers:
            [ ("Content-Type", "application/json")
            ; ("Origin", "https://evil.example")
            ; ("Sec-Fetch-Site", "same-origin")
            ]
          {|{"state":"granted"}|}
      ; check_consent_reject "no origin metadata -> 403" 403
          ~headers:[ ("Content-Type", "application/json") ]
          {|{"state":"granted"}|}
      ; Analytics_fixture.an_case "unsupported method -> controlled 405 JSON" (fun () ->
            let response =
              Lwt_main.run
                (Earde.Handlers.analytics_consent_method_not_allowed
                   (Dream.request ~method_:`GET ~target:"/analytics/consent"
                      ""))
            in
            Alcotest.(check int) "status" 405
              (Dream.status_to_int (Dream.status response));
            Alcotest.(check (option string)) "allow" (Some "POST")
              (Dream.header response "Allow"))
      ; Analytics_fixture.an_case "person-lookup failure never blocks the consent response"
          (fun () ->
            (* Session present but no sql pool: the lookup raises, is
               swallowed, and the cookie is still set. *)
            let payloads = ref [] in
            AnT.use_enabled_test_configuration ();
            AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
            Fun.protect
              ~finally:(fun () ->
                AnT.clear_capture_sink ();
                AnT.clear_configuration_override ())
              (fun () ->
                let pipeline =
                  Dream.memory_sessions (fun req ->
                      Lwt.bind
                        (Dream.set_session_field req "user_id" "12345")
                        (fun () ->
                          Earde.Handlers.analytics_consent_handler req))
                in
                let response =
                  Lwt_main.run
                    (pipeline
                       (Dream.request ~method_:`POST
                          ~target:"/analytics/consent"
                          ~headers:consent_good_headers
                          {|{"state":"granted"}|}))
                in
                Alcotest.(check int) "status" 204
                  (Dream.status_to_int (Dream.status response));
                Alcotest.(check bool) "cookie still set" true
                  (Html_assert.contains
                     (Option.value ~default:""
                        (Dream.header response "Set-Cookie"))
                     "earde_analytics_consent=granted");
                Alcotest.(check int) "no sync happened" 0
                  (List.length !payloads)))
      ] )
    (* Launch-document emission: banner + strictly public config when
       enabled; nothing at all when disabled; never a server-only secret.
       Rendered through a live wrapper — the shared [analytics_assets] is the
       single source of these attributes for every launch document. *)
  ; ( "analytics_launch_document"
    , [ Analytics_fixture.an_case "enabled: banner, script, public attrs only" (fun () ->
            AnT.use_enabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html = Analytics_fixture.launch_doc () in
                Alcotest.(check bool) "banner" true
                  (Html_assert.contains html "id='analytics-consent'");
                Alcotest.(check bool) "script" true
                  (Html_assert.contains html "/static/js/analytics.js");
                Alcotest.(check bool) "token attr" true
                  (Html_assert.contains html "data-ph-token='phc_test_token'");
                Alcotest.(check bool) "api host attr" true
                  (Html_assert.contains html "data-ph-api-host='https://eu.i.posthog.com'");
                Alcotest.(check bool) "banner ships hidden" true
                  (Html_assert.contains html "id='analytics-consent' hidden");
                Alcotest.(check bool) "no personal key marker" false
                  (Html_assert.contains html "phx_");
                Alcotest.(check bool) "no project id leak" false
                  (Html_assert.contains html "229260")))
      ; Analytics_fixture.an_case "disabled: no banner, no script, no config" (fun () ->
            AnT.use_disabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html = Analytics_fixture.launch_doc () in
                Alcotest.(check bool) "no banner" false
                  (Html_assert.contains html "analytics-consent");
                Alcotest.(check bool) "no script" false
                  (Html_assert.contains html "/static/js/analytics.js");
                Alcotest.(check bool) "no token attr" false
                  (Html_assert.contains html "data-ph-token")))
        (* §13 private-community marker: derived from the authoritative
           visibility passed with the community id, value "true" only. *)
      ; Analytics_fixture.an_case "private community page carries the private marker" (fun () ->
            AnT.use_enabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html =
                  Analytics_fixture.launch_doc ~analytics_community:(9, Earde.Db.Community_private) ()
                in
                Alcotest.(check bool) "private marker" true
                  (Html_assert.contains html "data-analytics-private-community='true'");
                Alcotest.(check bool) "group attr stays numeric" true
                  (Html_assert.contains html "data-analytics-group='community:9'")))
      ; Analytics_fixture.an_case "public community page has no private marker" (fun () ->
            AnT.use_enabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html =
                  Analytics_fixture.launch_doc ~analytics_community:(7, Earde.Db.Community_public) ()
                in
                Alcotest.(check bool) "no private marker" false
                  (Html_assert.contains html "data-analytics-private-community");
                Alcotest.(check bool) "group attr present" true
                  (Html_assert.contains html "data-analytics-group='community:7'")))
      ; Analytics_fixture.an_case "global page has neither group nor private marker" (fun () ->
            AnT.use_enabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html = Analytics_fixture.launch_doc () in
                Alcotest.(check bool) "no group attr" false
                  (Html_assert.contains html "data-analytics-group");
                Alcotest.(check bool) "no private marker" false
                  (Html_assert.contains html "data-analytics-private-community")))
      ] )
    (* §2.3 URL rule, reference implementation. *)
  ; ( "analytics_url_sanitizer"
    , [ Analytics_fixture.an_case "token query stripped" (fun () ->
            Alcotest.(check string) "confirm-email" "/confirm-email"
              (An.sanitize_url_for_analytics "/confirm-email?token=abc"))
      ; Analytics_fixture.an_case "search query stripped" (fun () ->
            Alcotest.(check string) "search" "/search"
              (An.sanitize_url_for_analytics "/search?q=x&page=2"))
      ; Analytics_fixture.an_case "fragment stripped" (fun () ->
            Alcotest.(check string) "fragment" "https://earde.com/p/1"
              (An.sanitize_url_for_analytics "https://earde.com/p/1#frag"))
      ; Analytics_fixture.an_case "query and fragment stripped" (fun () ->
            Alcotest.(check string) "both" "https://earde.com/feed"
              (An.sanitize_url_for_analytics "https://earde.com/feed?a=1#b"))
      ; Analytics_fixture.an_case "clean url unchanged" (fun () ->
            Alcotest.(check string) "clean" "https://earde.com/feed"
              (An.sanitize_url_for_analytics "https://earde.com/feed"))
      ] )
    (* Replay masking config coverage: every §6 selector/setting must be in
       the shipped analytics.js, plus the document-title protections. *)
  ; ( "analytics_replay_masking"
    , [ Analytics_fixture.an_case "analytics.js covers all §6 masking targets" (fun () ->
            let js = read_analytics_js () in
            List.iter
              (fun needle ->
                if not (Html_assert.contains js needle) then
                  Alcotest.failf "analytics.js is missing %S" needle)
              [ ".ph-mask"; ".cs-msg-text"; ".cs-msg-author"; "#chat-typing"
              ; ".cs-presence-name"; ".ft-title"; ".ft-preview"; ".th-title"
              ; ".th-body"; ".sr-row-title"; ".sr-row-excerpt"; ".ctext"
              ; "comment-content-"; ".account-notif-msg"; ".cm-table-reason"
              ; ".admin-cell-muted"; ".account-bio"; "maskAllInputs: true"
              ; "recordHeaders: false"; "recordBody: false"
              ; "capture_pageview: false"; "capture_pageleave: true"
              ])
      ; Analytics_fixture.an_case "document title is masked in replay and stripped from events"
          (fun () ->
            let js = read_analytics_js () in
            (* "title" element selector in the mask list (verified against
               the rrweb source posthog-js bundles: text-node masking checks
               the parent element, only STYLE/SCRIPT excluded). *)
            Alcotest.(check bool) "title selector present" true
              (Html_assert.contains js "\"title\",");
            (* $title removed from every outbound event... *)
            Alcotest.(check bool) "$title deletion present" true
              (Html_assert.contains js "delete props.$title");
            (* ...and never replaced by document.title or any other
               user-controlled value; no search-query handling either. *)
            Alcotest.(check bool) "no document.title reference" false
              (Html_assert.contains js "document.title");
            Alcotest.(check bool) "no query-string handling" false
              (Html_assert.contains js "location.search"))
      ] )
    (* §13 privacy hardening: autocapture is structural-only, exception
       autocapture is off, and fully private community documents never
       initialize the SDK. Static contract checks against the shipped
       analytics.js (same technique as the replay-masking coverage). *)
  ; ( "analytics_autocapture_protection"
    , [ Analytics_fixture.an_case "autocapture text masking and scrubbers present" (fun () ->
            let js = read_analytics_js () in
            List.iter
              (fun needle ->
                if not (Html_assert.contains js needle) then
                  Alcotest.failf "analytics.js is missing %S" needle)
              [ (* documented SDK option: element text never captured *)
                "mask_all_text: true"
                (* defense-in-depth scrubbers in the sanitize hook *)
              ; "delete props.$el_text"
              ; "sanitizeElement"
              ; "sanitizeElementsChain"
              ; "$elements_chain"
              ; "attr__title"
              ; "attr__aria-label"
              ; "attr__value"
              ; "attr__href"
              ; "attr__src"
              ; "attr__action"
              ])
      ; Analytics_fixture.an_case "chain scrubber removes text and URL query/fragment"
          (fun () ->
            let js = read_analytics_js () in
            (* The regexes that make nested element text and full URLs
               unrepresentable in $elements_chain payloads. *)
            Alcotest.(check bool) "text= scrub regex" true
              (Html_assert.contains js {|text="[^"]*"|});
            Alcotest.(check bool) "url attr scrub regex" true
              (Html_assert.contains js {|(?:attr__)?(?:href|src|action)="|});
            Alcotest.(check bool) "query/fragment stripper" true
              (Html_assert.contains js {|split("?")[0].split("#")[0]|}))
      ; Analytics_fixture.an_case "manual closed events keep their own schemas" (fun () ->
            let js = read_analytics_js () in
            (* search_performed: exactly the existing closed properties. *)
            Alcotest.(check bool) "search_performed present" true
              (Html_assert.contains js "search_performed");
            List.iter
              (fun needle ->
                if not (Html_assert.contains js needle) then
                  Alcotest.failf "search_performed lost %S" needle)
              [ "result_count: resultCount"; "active_tab: tab"; "page: page" ])
      ; Analytics_fixture.an_case "automatic exception capture is disabled" (fun () ->
            let js = read_analytics_js () in
            Alcotest.(check bool) "capture_exceptions: false" true
              (Html_assert.contains js "capture_exceptions: false");
            Alcotest.(check bool) "no capture_exceptions: true" false
              (Html_assert.contains js "capture_exceptions: true");
            Alcotest.(check bool) "no window.onerror forwarding" false
              (Html_assert.contains js "window.onerror");
            Alcotest.(check bool) "no unhandledrejection forwarding" false
              (Html_assert.contains js "unhandledrejection");
            Alcotest.(check bool) "no raw error-message property" false
              (Html_assert.contains js "error_message"))
      ; Analytics_fixture.an_case "private documents never reach the SDK" (fun () ->
            let js = read_analytics_js () in
            Alcotest.(check bool) "reads the private marker" true
              (Html_assert.contains js "data-analytics-private-community");
            (* The single centralized gate: initAnalytics resolves inert
               before loadSdk on private documents, so pageview, group,
               search_performed and the SDK download are all unreachable. *)
            Alcotest.(check bool) "privateCommunity gate" true
              (Html_assert.contains js "if (privateCommunity)");
            Alcotest.(check bool) "gate precedes SDK load" true
              (match
                 ( Html_assert.index_of js "if (privateCommunity)",
                   Html_assert.index_of js "initPromise = loadSdk()" )
               with
              | Some gate, Some load -> gate < load
              | _ -> false))
      ] )
    (* Document-title leakage: the search term must never enter <title>. *)
  ; ( "analytics_title_leakage"
    , [ Analytics_fixture.an_case "/search?q=secret renders a generic document title" (fun () ->
            let rendered = ref "" in
            let (_ : Dream.response) =
              Lwt_main.run
                (Dream.memory_sessions
                   (fun req ->
                     rendered :=
                       Earde.Pages.search_results_page ~admin_usernames:[]
                         [] 1 "all" "secret" [] [] [] [] req;
                     Dream.html !rendered)
                   (Dream.request ~method_:`GET
                      ~target:"/search?q=secret" ""))
            in
            let html = !rendered in
            let title_tag =
              (* Extract exactly the <title>…</title> element. *)
              let start = ref 0 and stop = ref 0 in
              String.iteri
                (fun i _ ->
                  if
                    i + 7 <= String.length html
                    && String.sub html i 7 = "<title>"
                  then start := i
                  else if
                    i + 8 <= String.length html
                    && String.sub html i 8 = "</title>"
                  then if !stop = 0 then stop := i)
                html;
              if !stop > !start then String.sub html !start (!stop - !start)
              else Alcotest.fail "no <title> found"
            in
            Alcotest.(check bool) "title has no query" false
              (Html_assert.contains title_tag "secret");
            Alcotest.(check bool) "title is the generic label" true
              (Html_assert.contains title_tag "Search");
            (* The visible UI still echoes the query (input value, masked by
               maskAllInputs) — only the title had to change. *)
            Alcotest.(check bool) "page body still echoes query in input" true
              (Html_assert.contains html "value='secret'"))
      ] )
    (* Shipped analytics.js: reconciliation contract + strict ordering. *)
  ; ( "analytics_js_reconciliation"
    , [ Analytics_fixture.an_case "identity → group → pageview ordering" (fun () ->
            let js = read_analytics_js () in
            let pos needle =
              match Html_assert.index_of js needle with
              | Some i -> i
              | None -> Alcotest.failf "analytics.js is missing %S" needle
            in
            let identity_call = pos "reconcileIdentity();" in
            let group_call = pos "reconcileGroup();" in
            let pageview = pos "pageviewSent = true" in
            Alcotest.(check bool) "identify before group" true
              (identity_call < group_call);
            Alcotest.(check bool) "group before pageview" true
              (group_call < pageview))
      ; Analytics_fixture.an_case "identity contract guards" (fun () ->
            let js = read_analytics_js () in
            (* identify only when the persisted id differs, key only *)
            Alcotest.(check bool) "differs guard" true
              (Html_assert.contains js "current !== identityAttr");
            Alcotest.(check bool) "identify carries no properties" true
              (Html_assert.contains js "window.posthog.identify(identityAttr);");
            (* reset only a user:-prefixed id; anonymous ids preserved *)
            Alcotest.(check bool) "reset guard" true
              (Html_assert.contains js
                 "else if (typeof current === \"string\" && current.indexOf(\"user:\") === 0)");
            Alcotest.(check bool) "reset present" true
              (Html_assert.contains js "window.posthog.reset();"))
      ; Analytics_fixture.an_case "group contract guards" (fun () ->
            let js = read_analytics_js () in
            (* key only — exactly two arguments, no properties object *)
            Alcotest.(check bool) "group key only" true
              (Html_assert.contains js "window.posthog.group(\"community\", groupAttr);");
            Alcotest.(check bool) "global pages clear sticky group" true
              (Html_assert.contains js "window.posthog.resetGroups();");
            Alcotest.(check bool) "reads the layout attributes" true
              (Html_assert.contains js "data-analytics-user"
              && Html_assert.contains js "data-analytics-group"))
      ; Analytics_fixture.an_case "no duplicate init or pageview" (fun () ->
            let js = read_analytics_js () in
            Alcotest.(check bool) "shared init promise" true
              (Html_assert.contains js "if (initPromise) return initPromise;");
            Alcotest.(check bool) "single pageview guard" true
              (Html_assert.contains js "if (!pageviewSent)");
            Alcotest.(check int) "exactly one $pageview capture" 1
              (Html_assert.count_sub js "posthog.capture(\"$pageview\""))
      ] )
    (* Shipped analytics.js: search_performed capture contract. *)
  ; ( "analytics_js_search_event"
    , [ Analytics_fixture.an_case "search_performed follows the manual $pageview, once"
          (fun () ->
            let js = read_analytics_js () in
            let pos needle =
              match Html_assert.index_of js needle with
              | Some i -> i
              | None -> Alcotest.failf "analytics.js is missing %S" needle
            in
            Alcotest.(check int) "exactly one capture call" 1
              (Html_assert.count_sub js "posthog.capture(\"search_performed\"");
            Alcotest.(check int) "exactly one call site" 1
              (Html_assert.count_sub js "captureSearchPerformed();");
            Alcotest.(check bool) "pageview precedes search event" true
              (pos "pageviewSent = true" < pos "captureSearchPerformed();");
            (* the single call site lives inside the consent-gated init
               chain (initAnalytics only runs after granted consent and a
               successful SDK load), before the next top-level function *)
            Alcotest.(check bool) "inside initAnalytics" true
              (pos "function initAnalytics"
                 < pos "captureSearchPerformed();"
              && pos "captureSearchPerformed();"
                 < pos "function clearPosthogPersistence"))
      ; Analytics_fixture.an_case "once per document load, even across repeated init"
          (fun () ->
            let js = read_analytics_js () in
            Alcotest.(check bool) "module-owned guard" true
              (Html_assert.contains js "var searchPerformedSent = false;");
            Alcotest.(check bool) "re-entry returns" true
              (Html_assert.contains js "if (searchPerformedSent) return;");
            Alcotest.(check bool) "flag set before capture" true
              (Html_assert.contains js "searchPerformedSent = true;"))
      ; Analytics_fixture.an_case "strict allowlist: closed tabs, integer count, positive page"
          (fun () ->
            let js = read_analytics_js () in
            Alcotest.(check bool) "exact tab set" true
              (Html_assert.contains js
                 "[\"posts\", \"communities\", \"comments\", \"people\"]");
            Alcotest.(check bool) "tab membership required" true
              (Html_assert.contains js "SEARCH_TABS.indexOf(tab) === -1) return;");
            Alcotest.(check bool) "integer-only parse" true
              (Html_assert.contains js "/^[0-9]+$/.test");
            Alcotest.(check bool) "positive page required" true
              (Html_assert.contains js "if (page < 1) return;"))
      ; Analytics_fixture.an_case "captures a fresh closed object, never the raw dataset"
          (fun () ->
            let js = read_analytics_js () in
            (* reads exactly the three closed attributes... *)
            Alcotest.(check int) "three attribute reads" 3
              (Html_assert.count_sub js "getAttribute(\"data-analytics-search-");
            (* ...and never passes an attribute bag through *)
            Alcotest.(check bool) "no dataset access" false
              (Html_assert.contains js ".dataset");
            Alcotest.(check bool) "closed property object" true
              (Html_assert.contains js "result_count: resultCount,");
            (* no query-named property can exist in the file *)
            Alcotest.(check bool) "no query property" false
              (Html_assert.contains js "query:"))
      ] )
  ; ( "analytics_group_identify_api"
    , [ check_group_identify_gate "granted emits one" 1 ~enabled:true
          (Some "earde_analytics_consent=granted")
      ; check_group_identify_gate "denied emits none" 0 ~enabled:true
          (Some "earde_analytics_consent=denied")
      ; check_group_identify_gate "missing cookie emits none" 0 ~enabled:true
          None
      ; check_group_identify_gate "malformed value emits none" 0
          ~enabled:true
          (Some "earde_analytics_consent=maybe")
      ; check_group_identify_gate "disabled analytics emits none" 0
          ~enabled:false
          (Some "earde_analytics_consent=granted")
      ; Analytics_fixture.an_case "granted payload is a closed $groupidentify" (fun () ->
            match
              run_group_identify ~enabled:true
                (Some "earde_analytics_consent=granted")
            with
            | [ p ] ->
                Alcotest.(check string) "event" "$groupidentify" (Analytics_fixture.event_of p);
                Alcotest.(check string) "caller-supplied user distinct id"
                  "user:7" (Analytics_fixture.distinct_of p);
                Alcotest.(check (slist string compare))
                  "props keys"
                  [ "$group_type"; "$group_key"; "$group_set";
                    "deployment_environment" ]
                  (Analytics_fixture.prop_keys p);
                Alcotest.(check (option string)) "group key"
                  (Some "community:7") (Analytics_fixture.group_key_prop_of p);
                Alcotest.(check (slist string compare))
                  "closed group set"
                  [ "community_id"; "community_slug"; "community_name";
                    "community_visibility" ]
                  (List.map fst (Analytics_fixture.group_set_of p))
            | l -> Alcotest.failf "expected 1 payload, got %d" (List.length l))
      ; Analytics_fixture.an_case "raising sink never escapes to the caller" (fun () ->
            AnT.use_enabled_test_configuration ();
            AnT.set_capture_sink (fun _ -> failwith "sink boom");
            Fun.protect
              ~finally:(fun () ->
                AnT.clear_capture_sink ();
                AnT.clear_configuration_override ())
              (fun () ->
                An.identify_community_if_consented
                  (Analytics_fixture.consent_request (Some "earde_analytics_consent=granted"))
                  ~distinct_id:"user:7" (an_group ());
                An.capture_if_consented
                  (Analytics_fixture.consent_request (Some "earde_analytics_consent=granted"))
                  ~distinct_id:"user:7" an_login))
      ] )
    (* The common event envelope: deployment_environment appears exactly
       once on every server payload, follows the closed value, and the
       browser adds it centrally in the sanitizer. *)
  ; ( "analytics_envelope"
    , [ Analytics_fixture.an_case "every domain event carries the envelope exactly once"
          (fun () ->
            List.iter
              (fun (name, event) ->
                let occurrences =
                  List.length
                    (List.filter
                       (fun k -> k = "deployment_environment")
                       (Analytics_fixture.prop_keys (an_payload event)))
                in
                if occurrences <> 1 then
                  Alcotest.failf "%s carries the envelope %d times" name
                    occurrences)
              an_all_events)
      ; Analytics_fixture.an_case "envelope value follows the closed environment" (fun () ->
            List.iter
              (fun (environment, expected) ->
                let payload =
                  AnT.event_payload ~api_key:"phc_test" ~environment
                    ~distinct_id:"user:1" an_login
                in
                Alcotest.(check (option string)) expected (Some expected)
                  (match
                     List.assoc_opt "deployment_environment"
                       (Analytics_fixture.payload_props payload)
                   with
                  | Some (`String v) -> Some v
                  | _ -> None))
              [ (An.Production, "production"); (An.Staging, "staging");
                (An.Development, "development") ])
      ; Analytics_fixture.an_case "$groupidentify carries the envelope exactly once" (fun () ->
            let payload =
              AnT.group_identify_payload ~api_key:"phc_test"
                ~environment:An.Production ~distinct_id:"user:1"
                (an_group ())
            in
            Alcotest.(check int) "once" 1
              (List.length
                 (List.filter
                    (fun k -> k = "deployment_environment")
                    (Analytics_fixture.prop_keys payload)));
            Alcotest.(check bool) "not inside $group_set" false
              (List.mem_assoc "deployment_environment"
                 (Analytics_fixture.group_set_of payload)))
      ; Analytics_fixture.an_case "person sync carries the envelope exactly once" (fun () ->
            let payload =
              AnT.person_sync_payload ~api_key:"phc_test"
                ~environment:An.Staging ~distinct_id:"user:1" an_person
            in
            Alcotest.(check int) "once" 1
              (List.length
                 (List.filter
                    (fun k -> k = "deployment_environment")
                    (Analytics_fixture.prop_keys payload)));
            (match List.assoc_opt "$set" (Analytics_fixture.payload_props payload) with
            | Some (`Assoc set) ->
                Alcotest.(check bool) "not inside $set" false
                  (List.mem_assoc "deployment_environment" set)
            | _ -> Alcotest.fail "no $set"))
      ; Analytics_fixture.an_case "browser sanitizer adds the closed value centrally" (fun () ->
            let js = read_analytics_js () in
            (* the exact closed set, validated before any SDK work *)
            Alcotest.(check bool) "closed set" true
              (Html_assert.contains js
                 "[\"production\", \"staging\", \"development\"]");
            Alcotest.(check bool) "invalid environment bails out" true
              (Html_assert.contains js
                 "DEPLOYMENT_ENVIRONMENTS.indexOf(deploymentEnvironment) \
                  === -1) return;");
            (* one central assignment, inside sanitizeProperties *)
            Alcotest.(check int) "single central assignment" 1
              (Html_assert.count_sub js "props.deployment_environment =");
            (match
               ( Html_assert.index_of js "function sanitizeProperties",
                 Html_assert.index_of js "props.deployment_environment =",
                 Html_assert.index_of js "MASK_TEXT_SELECTOR" )
             with
            | Some sanitize, Some assign, Some after ->
                Alcotest.(check bool) "assignment inside the sanitizer" true
                  (sanitize < assign && assign < after)
            | _ -> Alcotest.fail "sanitizer markers missing");
            (* never added at capture call sites *)
            Alcotest.(check int) "no per-capture property" 0
              (Html_assert.count_sub js "deployment_environment:");
            (* the gate precedes SDK loading, so an invalid environment can
               produce no PostHog request at all *)
            match
              (Html_assert.index_of js "DEPLOYMENT_ENVIRONMENTS.indexOf",
               Html_assert.index_of js "function loadSdk")
            with
            | Some gate, Some load ->
                Alcotest.(check bool) "gate precedes SDK load" true
                  (gate < load)
            | _ -> Alcotest.fail "gate markers missing")
      ] )
  ; ( "analytics_consent_db", [ consent_sync_db_case ] )
  ]
