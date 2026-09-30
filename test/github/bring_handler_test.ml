module Ob = Earde.Project_onboarding

(* /bring handler (Github_onboarding_handlers.make_bring_handler) — DB-free:
   the factory takes only the closed mode and needs no configuration loader,
   credentials, database, or GitHub access, so the full request → HTML path
   runs offline. Session middleware is always installed (the shared layout
   assumes it, exactly like production); anonymous is an empty session. Raw
   feedback-query values are asserted absent from the page. *)

let ( let* ) = Lwt.bind

let case = Bring_fixture.bring_case

let success_copy = "GitHub installation connected successfully."

let failure_copy = "We couldn't complete the GitHub connection."

let checked_body label response =
  Bring_fixture.check_page label response;
  Bring_fixture.body_of response

let check_banner label body expected =
  let expect_success, expect_failure =
    match expected with
    | `Success -> (true, false)
    | `Failure -> (false, true)
    | `None -> (false, false)
  in
  Alcotest.(check bool) (label ^ ": success banner") expect_success
    (Html_assert.contains body success_copy);
  Alcotest.(check bool) (label ^ ": failure banner") expect_failure
    (Html_assert.contains body failure_copy)

let check_no_form label body =
  Alcotest.(check bool) (label ^ ": no form") false (Html_assert.contains body "<form");
  Alcotest.(check bool) (label ^ ": no start action") false
    (Html_assert.contains body Bring_fixture.start_action)

(* Strict duplicate-aware feedback interpretation: only the two exact
   lowercase callback values, exactly once, render a banner. *)
let feedback_matrix =
  [ ("missing query", "/bring", `None)
  ; ("connected", "/bring?github=connected", `Success)
  ; ("failed", "/bring?github=failed", `Failure)
  ; ("blank value", "/bring?github=", `None)
  ; ("bare key", "/bring?github", `None)
  ; ("unknown value", "/bring?github=done", `None)
  ; ("uppercase value", "/bring?github=Connected", `None)
  ; ("all-caps value", "/bring?github=FAILED", `None)
  ; ("duplicate connected", "/bring?github=connected&github=connected", `None)
  ; ("connected and failed", "/bring?github=connected&github=failed", `None)
  ; ("failed and connected", "/bring?github=failed&github=connected", `None)
  ; ( "unrelated parameters ignored"
    , "/bring?utm_source=x&github=connected&ref=y"
    , `Success )
  ]

let feedback_cases =
  List.map
    (fun (label, target, expected) ->
      case ("feedback: " ^ label) (fun () ->
          let body =
            checked_body label (Bring_fixture.run ~session:Bring_fixture.member ~target ())
          in
          check_banner label body expected))
    feedback_matrix

let raw_value_case =
  case "feedback: raw query values never reach the page" (fun () ->
      List.iter
        (fun (target, marker) ->
          let body = Bring_fixture.body_of (Bring_fixture.run ~session:Bring_fixture.member ~target ()) in
          Alcotest.(check bool) ("marker absent: " ^ marker) false
            (Html_assert.contains body marker))
        [ ("/bring?github=zqxmarker1", "zqxmarker1")
        ; ("/bring?github=%3Cscript%3Ezqxmarker2%3C%2Fscript%3E", "zqxmarker2")
        ; ("/bring?github=connectedzqxmarker3", "zqxmarker3")
        ; ("/bring?github=connected&github=zqxmarker4", "zqxmarker4")
        ])

let no_empty_banner_case =
  case "feedback: no banner means no alert element at all" (fun () ->
      (* Ready is the only state whose action panel is not an alert, so
         any auth-alert here could only be a leftover feedback shell. *)
      let body = Bring_fixture.body_of (Bring_fixture.run ~session:Bring_fixture.member ~mode:Ob.Public ()) in
      Alcotest.(check bool) "no alert element" false
        (Html_assert.contains body "auth-alert"))

let off_case =
  case "off: unavailable copy, no action, no false login prompt" (fun () ->
      List.iter
        (fun (label, session) ->
          let body =
            checked_body label (Bring_fixture.run ~session ~mode:Ob.Off ())
          in
          Alcotest.(check bool) (label ^ ": unavailable copy") true
            (Html_assert.contains body "Project onboarding is currently unavailable");
          check_no_form label body;
          (* Authenticated viewers must not be told to log in. *)
          Alcotest.(check bool) (label ^ ": no login copy") false
            (Html_assert.contains body Bring_fixture.login_copy);
          Alcotest.(check bool) (label ^ ": no login link") false
            (Html_assert.contains body "href='/login'"))
        [ ("member", Bring_fixture.member); ("admin", Bring_fixture.admin) ])

let off_feedback_case =
  case "off: callback feedback shows without enabling the action" (fun () ->
      List.iter
        (fun (label, target, expected) ->
          let body =
            checked_body label (Bring_fixture.run ~session:Bring_fixture.member ~mode:Ob.Off ~target ())
          in
          check_banner label body expected;
          check_no_form label body)
        [ ("connected", "/bring?github=connected", `Success)
        ; ("failed", "/bring?github=failed", `Failure)
        ])

let anonymous_case =
  case "anonymous: login link, no form, no hidden data" (fun () ->
      List.iter
        (fun (label, mode) ->
          let body = checked_body label (Bring_fixture.run ~mode ()) in
          Alcotest.(check bool) (label ^ ": login copy") true
            (Html_assert.contains body Bring_fixture.login_copy);
          Alcotest.(check bool) (label ^ ": login link") true
            (Html_assert.contains body "href='/login'");
          check_no_form label body;
          Alcotest.(check bool) (label ^ ": no hidden input") false
            (Html_assert.contains body "type='hidden'"))
        [ ("public", Ob.Public); ("admins", Ob.Admins) ])

let anonymous_feedback_case =
  case "anonymous: feedback does not bypass authentication" (fun () ->
      let body =
        checked_body "anonymous connected"
          (Bring_fixture.run ~mode:Ob.Public ~target:"/bring?github=connected" ())
      in
      check_banner "anonymous connected" body `Success;
      Alcotest.(check bool) "still the login state" true
        (Html_assert.contains body Bring_fixture.login_copy);
      check_no_form "anonymous connected" body)

let admins_non_admin_case =
  case "admins: authenticated non-admin sees the rollout state" (fun () ->
      List.iter
        (fun (label, session) ->
          let body =
            checked_body label (Bring_fixture.run ~session ~mode:Ob.Admins ())
          in
          Alcotest.(check bool) (label ^ ": rollout copy") true
            (Html_assert.contains body "limited to the early-access rollout");
          check_no_form label body;
          Alcotest.(check bool) (label ^ ": no login copy") false
            (Html_assert.contains body Bring_fixture.login_copy))
        [ ("member", Bring_fixture.member)
        ; ("explicit false", Bring_fixture.member @ [ ("is_admin", "false") ])
        ])

let admins_admin_case =
  case "admins: authenticated admin gets the start form" (fun () ->
      let body = checked_body "admin" (Bring_fixture.run ~session:Bring_fixture.admin ~mode:Ob.Admins ()) in
      Alcotest.(check bool) "start form" true (Html_assert.contains body Bring_fixture.start_action);
      Alcotest.(check bool) "button copy" true (Html_assert.contains body Bring_fixture.button_copy))

let invalid_session_case =
  case "session: invalid user_id reads as anonymous" (fun () ->
      List.iter
        (fun raw ->
          let label = "user_id " ^ (if raw = "" then "<empty>" else raw) in
          let body =
            checked_body label
              (Bring_fixture.run ~session:[ ("user_id", raw) ] ~mode:Ob.Public ())
          in
          Alcotest.(check bool) (label ^ ": login copy") true
            (Html_assert.contains body Bring_fixture.login_copy);
          check_no_form label body)
        [ "not-a-number"; ""; "0"; "-3" ])

let admin_flag_without_user_case =
  case "session: is_admin alone is not authentication" (fun () ->
      List.iter
        (fun (label, session) ->
          let body =
            checked_body label (Bring_fixture.run ~session ~mode:Ob.Admins ())
          in
          Alcotest.(check bool) (label ^ ": login copy") true
            (Html_assert.contains body Bring_fixture.login_copy);
          check_no_form label body)
        [ ("no user_id", [ ("is_admin", "true") ])
        ; ("zero user_id", [ ("user_id", "0"); ("is_admin", "true") ])
        ; ("negative user_id", [ ("user_id", "-1"); ("is_admin", "true") ])
        ])

let public_ready_case =
  case "public: authenticated user gets exactly one clean POST form"
    (fun () ->
      let body = checked_body "ready" (Bring_fixture.run ~session:Bring_fixture.member ()) in
      Alcotest.(check int) "exactly one form" 1
        (Bring_fixture.count_occurrences body "<form");
      Alcotest.(check bool) "method is POST" true
        (Html_assert.contains body "<form method='POST'");
      Alcotest.(check bool) "exact action" true (Html_assert.contains body Bring_fixture.start_action);
      Alcotest.(check bool) "button copy" true (Html_assert.contains body Bring_fixture.button_copy);
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("no " ^ needle) false
            (Html_assert.contains body needle))
        [ "type='hidden'"; "name='user_id'"; "name='state'"
        ; "name='redirect'"; "client_id"; "installation_id" ])

let retry_case =
  case "ready: failure banner and start form appear together" (fun () ->
      let body =
        checked_body "retry"
          (Bring_fixture.run ~session:Bring_fixture.member ~target:"/bring?github=failed" ())
      in
      check_banner "retry" body `Failure;
      Alcotest.(check bool) "start form" true (Html_assert.contains body Bring_fixture.start_action);
      let body =
        checked_body "again"
          (Bring_fixture.run ~session:Bring_fixture.member ~target:"/bring?github=connected" ())
      in
      check_banner "again" body `Success;
      Alcotest.(check bool) "start form" true (Html_assert.contains body Bring_fixture.start_action))

let copy_case =
  case "copy: no endorsement claims, no members-without-GitHub marketing"
    (fun () ->
      List.iter
        (fun (label, session, mode) ->
          let body =
            String.lowercase_ascii (Bring_fixture.body_of (Bring_fixture.run ~session ~mode ()))
          in
          List.iter
            (fun phrase ->
              Alcotest.(check bool) (label ^ " lacks " ^ phrase) false
                (Html_assert.contains body phrase))
            [ "official community"; "official home"; "github-approved"
            ; "github-endorsed"
            (* Retired: the page neither markets the absence of GitHub for
               ordinary members nor claims GitHub is required. *)
            ; "do not need a github account"; "never need github"
            ; "without github"; "no github required"; "lwt to ocaml"
            ; "github is required" ])
        Bring_fixture.all_states)

let headers_case =
  case "headers: every state answers 200 no-store no-referrer" (fun () ->
      List.iter
        (fun (label, session, mode) ->
          Bring_fixture.check_page label (Bring_fixture.run ~session ~mode ());
          Bring_fixture.check_page (label ^ " + feedback")
            (Bring_fixture.run ~session ~mode ~target:"/bring?github=failed" ()))
        Bring_fixture.all_states)

let suite =
  feedback_cases
  @ [ raw_value_case; no_empty_banner_case; off_case; off_feedback_case;
      anonymous_case; anonymous_feedback_case; admins_non_admin_case;
      admins_admin_case; invalid_session_case;
      admin_flag_without_user_case; public_ready_case; retry_case;
      copy_case; headers_case ]

let suites =
    (* /bring handler: DB-free request → HTML coverage of feedback-query
       parsing, session-derived access states, copy constraints, and the
       no-store/no-referrer headers (see Gh_bring). *)
  [ ( "bring_handler", suite )
  ]
