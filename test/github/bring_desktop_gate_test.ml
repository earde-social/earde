(* /bring under the canonical desktop-only gate. The project-onboarding funnel
   this page opens (GitHub App installation → repository selection → project
   setup) is desktop-only and every downstream document already ships the gate;
   /bring was the one entry point that did not, so a phone visitor could walk
   the whole landing → /bring → signup → onboarding path into unsupported
   surfaces.

   The gate is the existing mechanism, unchanged: SSR panel + mobile-gate.css +
   a per-route [body.<page_class> > .app] hide rule under the one 800px
   breakpoint. Viewport, never user agent — so these cases test the actual
   contract (the document always carries the panel; the CSS decides which of
   the two is visible) rather than inventing a fake UA test. Because the gate
   is width-driven and the document is identical for every viewer, session
   state cannot bypass it: the anonymous, member, admin, rollout-limited and
   post-callback returns are all asserted to carry the same canonical panel.

   DB-free: the /bring handler needs no configuration, credentials, database or
   GitHub access, so the full request → HTML path runs offline. *)

let case = Bring_fixture.bring_case

(* dune test runs in test/, dune exec from the project root. *)
let root = if Sys.file_exists "../bin/main.ml" then ".." else "."

let read path =
  let ic = open_in_bin (Filename.concat root path) in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let gate_link = "<link rel='stylesheet' href='/static/css/mobile-gate.css'>"

let gate_open = "<div class='mobile-gate' role='dialog' aria-label='Desktop only'>"

let gate_end = "Mobile support is coming later.</p></div></div>"

(* The panel as it actually ships, sliced out of a rendered document, so
   nothing here restates the canonical copy: if Components' one definition
   changes, /bring follows it or these cases fail. *)
let panel_of label html =
  match Html_assert.index_of html gate_open with
  | None -> Alcotest.failf "%s: no desktop-only gate panel" label
  | Some i -> (
      let rest = String.sub html i (String.length html - i) in
      match Html_assert.index_of rest gate_end with
      | None -> Alcotest.failf "%s: unterminated desktop-only gate panel" label
      | Some j -> String.sub rest 0 (j + String.length gate_end))

(* The reference rendering: the /feed application document, gated since the
   mechanism was introduced and untouched by this change. *)
let canonical_panel () =
  panel_of "launch_app_page"
    (Earde.Page_shell.launch_app_page ~page_class:"launch-feed" ~title:"T"
       ~content:"B" ())

(* Every /bring document must carry exactly one canonical gate, whatever the
   viewer, mode or callback feedback. *)
let check_gated label body =
  Alcotest.(check bool)
    (label ^ ": launch-bring document")
    true
    (Html_assert.contains body "<body class='launch-bring'>");
  Alcotest.(check int)
    (label ^ ": exactly one mobile-gate.css link")
    1 (Html_assert.count_sub body gate_link);
  Alcotest.(check int)
    (label ^ ": exactly one gate panel")
    1 (Html_assert.count_sub body gate_open);
  Alcotest.(check string)
    (label ^ ": canonical panel, not a second desktop-only page")
    (canonical_panel ()) (panel_of label body)

(* 1./2. Anonymous and authenticated viewers alike: the mobile visitor gets
   the canonical desktop-only experience, and the response itself is the
   unchanged 200 / no-store / shared-referrer-policy page. *)
let anonymous_case =
  case "anonymous /bring carries the canonical desktop-only gate" (fun () ->
      let response = Bring_fixture.run () in
      Bring_fixture.check_page "anonymous" response;
      check_gated "anonymous" (Bring_fixture.body_of response))

let authenticated_case =
  case "authenticated /bring carries the same desktop-only gate" (fun () ->
      List.iter
        (fun (label, session) ->
          let response = Bring_fixture.run ~session () in
          Bring_fixture.check_page label response;
          check_gated label (Bring_fixture.body_of response))
        [ ("member", Bring_fixture.member); ("admin", Bring_fixture.admin) ])

(* 5. The login/signup return path cannot expose the funnel: the access state
   lives in the session and the callback feedback in the query string, and
   neither reaches the gate. Every user-facing state — including the
   ?github=connected return a viewer lands on straight after authenticating
   at GitHub — renders the identical panel. *)
let every_state_case =
  case "every access state and callback return stays gated" (fun () ->
      List.iter
        (fun (state, session, mode) ->
          List.iter
            (fun target ->
              let label = state ^ " " ^ target in
              let response = Bring_fixture.run ~session ~mode ~target () in
              Bring_fixture.check_page label response;
              check_gated label (Bring_fixture.body_of response))
            [ "/bring"; "/bring?github=connected"; "/bring?github=failed" ])
        Bring_fixture.all_states)

(* 6. The panel a phone actually sees says nothing about the viewer or the
   funnel: no installation or onboarding state, no repository or project
   metadata, no ids, and no action that could re-enter the flow. *)
let no_state_in_gate_case =
  case "the gate panel exposes no onboarding or project state" (fun () ->
      let panel =
        String.lowercase_ascii
          (panel_of "ready member"
             (Bring_fixture.body_of
                (Bring_fixture.run ~session:Bring_fixture.member
                   ~target:"/bring?github=connected" ())))
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("gate panel leaks " ^ needle)
            false (Html_assert.contains panel needle))
        [ "github"; "install"; "repositor"; "project"; "onboard"
        ; "/integrations"; "user_id"; "alice"; "42"; "auth-alert"
        ; "<form"; " id='"; "href=" ])

(* The panel is the .app column's sibling, not its descendant, so the
   [> .app] hide rule takes the whole onboarding UI with it and leaves the
   gate standing. The exact bytes between the footer and the panel are the
   .app close and nothing else. *)
let placement_case =
  case "the gate sits outside the hidden .app column" (fun () ->
      let body = Bring_fixture.body_of (Bring_fixture.run ~session:Bring_fixture.member ()) in
      match (Html_assert.index_of body "</footer>", Html_assert.index_of body gate_open) with
      | Some f, Some g ->
          let f = f + String.length "</footer>" in
          Alcotest.(check bool) "gate follows the app column" true (g > f);
          Alcotest.(check string) "only the .app close in between"
            "\n</div>\n" (String.sub body f (g - f))
      | _ -> Alcotest.fail "/bring lost its footer or its gate")

(* 3./4. Desktop is untouched: strip the two fragments the gate adds and the
   document is byte-identical to the one this wrapper rendered before the
   option existed — same chrome, same onboarding column, same everything. *)
let remove_once label needle haystack =
  match Html_assert.index_of haystack needle with
  | None -> Alcotest.failf "%s: nothing to remove" label
  | Some i ->
      String.sub haystack 0 i
      ^ String.sub haystack
          (i + String.length needle)
          (String.length haystack - i - String.length needle)

let entry_doc ?(desktop_only = false) () =
  Earde.Page_shell.launch_entry_page ~noindex:true ~desktop_only
    ~topbar:(Earde.Page_shell.Entry_viewer (Some "alice"))
    ~page_class:"launch-bring" ~title:"T" ~content:"B" ()

let desktop_unchanged_case =
  case "the gate is purely additive: desktop bytes are unchanged" (fun () ->
      let gated = entry_doc ~desktop_only:true () in
      let stripped =
        gated
        |> remove_once "gate link" (gate_link ^ "\n")
        |> remove_once "gate panel" (panel_of "gated" gated ^ "\n")
      in
      Alcotest.(check string) "identical to the ungated document"
        (entry_doc ()) stripped)

(* And the page a desktop viewer reads is still the onboarding page. *)
let desktop_content_case =
  case "desktop /bring still renders the onboarding page" (fun () ->
      let anon = Bring_fixture.body_of (Bring_fixture.run ()) in
      Alcotest.(check bool) "anonymous: hero" true
        (Html_assert.contains anon "Bring your open-source community");
      Alcotest.(check bool) "anonymous: both home options" true
        (Html_assert.contains anon "Create a community home"
        && Html_assert.contains anon "Connect to an existing community");
      Alcotest.(check bool) "anonymous: account-required panel" true
        (Html_assert.contains anon Bring_fixture.login_copy);
      let member =
        Bring_fixture.body_of (Bring_fixture.run ~session:Bring_fixture.member ())
      in
      Alcotest.(check int) "member: the one start form survives" 1
        (Html_assert.count_sub member Bring_fixture.start_action);
      Alcotest.(check bool) "member: full button label" true
        (Html_assert.contains member Bring_fixture.button_copy))

(* The gating itself: one authoritative breakpoint, one rule, reusing the
   existing per-route pattern. *)
let css_contract_case =
  case "one 800px hide rule for launch-bring, no second breakpoint"
    (fun () ->
      let earde = read "static/css/earde.css" in
      let rule =
        "@media (max-width: 800px) {\n\
        \  body.launch-bring > .app { display: none !important; }\n\
         }"
      in
      Alcotest.(check int) "exactly one launch-bring gate rule" 1
        (Html_assert.count_sub earde rule);
      Alcotest.(check int) "launch-bring is hidden nowhere else" 1
        (Html_assert.count_sub earde "body.launch-bring > .app");
      (* The panel's own stylesheet keeps the single breakpoint definition:
         invisible above it, the whole viewport below it. *)
      let gate_css = read "static/css/mobile-gate.css" in
      Alcotest.(check bool) "panel hidden above the breakpoint" true
        (Html_assert.contains gate_css ".mobile-gate { display: none; }");
      Alcotest.(check int) "one breakpoint in mobile-gate.css" 1
        (Html_assert.count_sub gate_css "@media");
      Alcotest.(check bool) "and it is the shared 800px one" true
        (Html_assert.contains gate_css "@media (max-width: 800px)"))

let check_gated_document label html =
  Alcotest.(check int)
    (label ^ ": one mobile-gate.css link")
    1 (Html_assert.count_sub html gate_link);
  Alcotest.(check string)
    (label ^ ": canonical panel")
    (canonical_panel ()) (panel_of label html)

let gate_test_community : Earde.Community_types.community =
  { id = 1; slug = "ocaml"; name = "OCaml"; description = None; rules = None
  ; avatar_url = None; banner_url = None; allow_downvotes = true
  ; sections_enabled = true; visibility = Earde.Community_types.Community_public
  ; indexable = true; is_network_community = false
  ; onboarding_state = Earde.Community_types.Community_published; discoverable = true }

(* 7. The canonical implementation elsewhere is untouched: still one
   definition, still shipped by the same application wrappers, and still
   absent from the auth, message and legal documents that must stay usable
   on a phone. *)
let elsewhere_unchanged_case =
  case "the canonical gate elsewhere is unchanged" (fun () ->
      Alcotest.(check int) "one panel definition" 1
        (Source_census.count_everywhere "let mobile_desktop_gate");
      Alcotest.(check int) "one stylesheet-link definition" 1
        (Source_census.count_everywhere "let mobile_gate_css_link");
      let community = gate_test_community in
      List.iter
        (fun (label, html) -> check_gated_document label html)
        [ ( "launch_app_page"
          , Earde.Page_shell.launch_app_page ~user:"alice"
              ~page_class:"launch-feed" ~title:"T" ~content:"B" () )
        ; ( "launch_onboarding_page"
          , Earde.Page_shell.launch_onboarding_page ~user:"alice"
              ~page_class:"launch-project-new" ~title:"T" ~content:"B" () )
        ; ( "launch_community_page"
          , Earde.Community_shell.launch_community_page ~user:"alice" ~community
              ~sidebar:"S" ~page_class:"launch-community-overview"
              ~title:"T" ~content:"B" () )
        ];
      List.iter
        (fun (label, html) ->
          Alcotest.(check int)
            (label ^ ": still ungated")
            0 (Html_assert.count_sub html "mobile-gate"))
        [ ( "launch_entry_page (/privacy)"
          , Earde.Page_shell.launch_entry_page ~page_class:"launch-privacy"
              ~title:"T" ~content:"B" () )
        ; ( "launch_auth_page"
          , Earde.Page_shell.launch_auth_page ~page_class:"launch-login"
              ~title:"T" ~content:"B" () )
        ; ( "launch_message_page"
          , Earde.Page_shell.launch_message_page ~title:"T" ~content:"B" () )
        ])

let suite =
  [ anonymous_case; authenticated_case; every_state_case;
    no_state_in_gate_case; placement_case; desktop_unchanged_case;
    desktop_content_case; css_contract_case; elsewhere_unchanged_case ]

let suites =
    (* /bring is gated by the canonical desktop-only mechanism: a mobile
       visitor gets the shared "Desktop only for now" panel instead of an
       entry point into the desktop-only project-onboarding funnel, in every
       access state and regardless of authentication (see
       Bring_desktop_gate). *)
  [ ( "bring_desktop_gate", suite )
  ]
