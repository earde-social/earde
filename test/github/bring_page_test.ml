module Gp = Earde.Github_onboarding_pages

(* /bring page renderer — pure (no server, no session middleware): the closed
   access state and callback feedback are plain arguments and the layout
   renders without a request. Assertions are substring checks on stable
   copy/markup; access derivation, feedback-query parsing, and response
   headers are covered on the handler in Gh_bring below. *)

let render_bring ?user ?(feedback = None) access =
  Gp.bring_page ?user ~access ~feedback ()

let bring_accesses =
  [ ("disabled", Gp.Onboarding_disabled)
  ; ("login-required", Gp.Login_required)
  ; ("rollout-limited", Gp.Rollout_limited)
  ; ("ready", Gp.Ready)
  ]

let bring_feedbacks =
  [ ("no feedback", None)
  ; ("connected", Some Gp.Connected)
  ; ("failed", Some Gp.Failed)
  ]

(* Shared invariants: every access × feedback state keeps the maintainer
   flow explanation and the factual verification language, stays noindex,
   never emits a forbidden endorsement claim (checked case-insensitively) or
   a link to the admin-only legacy route — and carries none of the retired
   members-don't-need-GitHub marketing or the named Lwt/OCaml example. *)
let bring_shared_case (access_name, access) (feedback_name, feedback) =
  Bring_fixture.bring_case
    (Printf.sprintf "%s, %s" access_name feedback_name)
    (fun () ->
      let html = render_bring ~feedback access in
      let lower = String.lowercase_ascii html in
      let must s = Alcotest.(check bool) ("contains: " ^ s) true (Html_assert.contains html s) in
      let must_not_ci s =
        Alcotest.(check bool) ("must not contain: " ^ s) false
          (Html_assert.contains lower (String.lowercase_ascii s))
      in
      must "verify project maintainers and their public repositories";
      must "does not automatically grant moderation rights";
      must "Project connected through GitHub";
      must "Verified through GitHub";
      must "Reads public-repository metadata only";
      must "no source-code or write access";
      must "noindex";
      must_not_ci "official community";
      must_not_ci "official home";
      must_not_ci "GitHub-approved";
      must_not_ci "GitHub-endorsed";
      must_not_ci "/new-community";
      must_not_ci "/projects/new";
      (* Retired copy: the page no longer markets the absence of GitHub for
         ordinary members (and never claims GitHub is required either), and
         the two-option explainer stands without the named example. *)
      must_not_ci "do not need a GitHub account";
      must_not_ci "never need GitHub";
      must_not_ci "without GitHub";
      must_not_ci "No GitHub required";
      must_not_ci "Lwt to OCaml";
      must_not_ci "GitHub is required")

let bring_shared_cases =
  List.concat_map
    (fun access -> List.map (bring_shared_case access) bring_feedbacks)
    bring_accesses

let suites =
    (* /bring renderer: shared copy invariants across every access ×
       feedback combination (see bring_shared_case). *)
  [ ( "bring_page_shared", bring_shared_cases )
    (* State-specific rendering: each closed access state renders its own
       controlled panel, and only Ready carries the start form. *)
  ; ( "bring_page_states"
    , [ Bring_fixture.bring_case "disabled: unavailable state, no start action" (fun () ->
            let html = render_bring Gp.Onboarding_disabled in
            Alcotest.(check bool) "unavailable state" true
              (Html_assert.contains html "Project onboarding is currently unavailable");
            Alcotest.(check bool) "no form" false (Html_assert.contains html "<form");
            Alcotest.(check bool) "no start action" false
              (Html_assert.contains html "/integrations/github/install/start"))
      ; Bring_fixture.bring_case "login-required: plain /login link, no form" (fun () ->
            let html = render_bring Gp.Login_required in
            Alcotest.(check bool) "account-required copy" true
              (Html_assert.contains html "An Earde account is required");
            Alcotest.(check bool) "login link" true
              (Html_assert.contains html "href='/login'");
            Alcotest.(check bool) "no return-url parameter" false
              (Html_assert.contains html "/login?");
            Alcotest.(check bool) "no form" false (Html_assert.contains html "<form"))
      ; Bring_fixture.bring_case "rollout-limited: early-access copy only" (fun () ->
            let html = render_bring ~user:"alice" Gp.Rollout_limited in
            Alcotest.(check bool) "rollout copy" true
              (Html_assert.contains html "limited to the early-access rollout");
            Alcotest.(check bool) "no form" false (Html_assert.contains html "<form");
            (* Nothing may read as a GitHub-side permission problem or
               name a configuration flag. *)
            Alcotest.(check bool) "no GitHub-permission claim" false
              (Html_assert.contains html "GitHub permission");
            Alcotest.(check bool) "no flag name" false
              (Html_assert.contains html "EARDE_GITHUB_ONBOARDING_ENABLED"))
      ; Bring_fixture.bring_case "ready: one parameter-free POST form" (fun () ->
            let html = render_bring ~user:"alice" Gp.Ready in
            Alcotest.(check bool) "POST form" true
              (Html_assert.contains html "<form method='POST' \
                              action='/integrations/github/install/start'>");
            Alcotest.(check bool) "button copy" true
              (Html_assert.contains html "Connect a GitHub project");
            Alcotest.(check bool) "no hidden input" false
              (Html_assert.contains html "type='hidden'"))
      ; Bring_fixture.bring_case "feedback: banners are the required copy" (fun () ->
            let connected =
              render_bring ~feedback:(Some Gp.Connected) Gp.Ready in
            Alcotest.(check bool) "success copy" true
              (Html_assert.contains connected
                 "GitHub installation connected successfully.");
            let failed = render_bring ~feedback:(Some Gp.Failed) Gp.Ready in
            Alcotest.(check bool) "failure copy" true
              (Html_assert.contains failed
                 "We couldn't complete the GitHub connection. Please try \
                  again.");
            Alcotest.(check bool) "failure styled as error" true
              (Html_assert.contains failed "auth-alert--error"))
      ; Bring_fixture.bring_case "feedback: none renders no alert element" (fun () ->
            (* Ready's action panel is the only one that is not an alert,
               so any auth-alert here would be an empty feedback shell. *)
            let html = render_bring ~user:"alice" Gp.Ready in
            Alcotest.(check bool) "no alert element" false
              (Html_assert.contains html "auth-alert"))
      ] )
    (* Viewer-aware navigation: anonymous gets real login/signup links;
       authenticated viewers are never offered signup. *)
  ; ( "bring_page_nav"
    , [ Bring_fixture.bring_case "anonymous: login and signup links" (fun () ->
            let html = render_bring Gp.Login_required in
            Alcotest.(check bool) "login link" true
              (Html_assert.contains html "href='/login'");
            Alcotest.(check bool) "signup link" true
              (Html_assert.contains html "href='/signup'"))
      ; Bring_fixture.bring_case "authenticated: no signup, feed link stays" (fun () ->
            let html = render_bring ~user:"alice" Gp.Ready in
            Alcotest.(check bool) "no signup anywhere" false
              (Html_assert.contains html "/signup");
            Alcotest.(check bool) "no login link" false
              (Html_assert.contains html "href='/login'");
            Alcotest.(check bool) "feed link" true
              (Html_assert.contains html "href='/feed'"))
      ] )
  ]
