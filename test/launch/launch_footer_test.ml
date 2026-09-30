(* The global legal footer (Components.launch_footer): one slim strip as the
   .app column's last child on every chrome-bearing launch wrapper, carrying
   exactly one Privacy link and one Analytics-preferences link to the
   /privacy section that hosts the working consent controls. Rendered
   byte-identically for anonymous and authenticated viewers, form-free, and
   OUTSIDE <main> so the create-shell/cm-main → </main> fragment slices and
   the chat surface's .cs-main anatomy stay untouched. The message sheet
   keeps its documented no-chrome contract and renders none. *)

let fc_case name f = Alcotest.test_case name `Quick f

let footer_open = "<footer class='launch-footer'>"

let privacy_link = "<a href='/privacy'>Privacy</a>"

let preferences_link =
  "<a href='/privacy#cookies-analytics'>Analytics preferences</a>"

(* Every chrome-bearing wrapper, both viewer arms, including the /bring
   entry-viewer arms the CTA suite's list omits. *)
let footer_documents () =
  let community = Launch_fixture.nav_test_community in
  [ ( "launch_entry_page (default chrome)"
    , Earde.Page_shell.launch_entry_page ~page_class:"launch-privacy"
        ~title:"T" ~content:"B" () )
  ; ( "launch_entry_page (viewer, member)"
    , Earde.Page_shell.launch_entry_page
        ~topbar:(Earde.Page_shell.Entry_viewer (Some "alice"))
        ~page_class:"launch-bring" ~title:"T" ~content:"B" () )
  ; ( "launch_entry_page (viewer, anonymous)"
    , Earde.Page_shell.launch_entry_page
        ~topbar:(Earde.Page_shell.Entry_viewer None)
        ~page_class:"launch-bring" ~title:"T" ~content:"B" () )
  ; ( "launch_auth_page (login)"
    , Earde.Page_shell.launch_auth_page ~page_class:"launch-login"
        ~title:"T" ~content:"B" () )
  ; ( "launch_app_page (member)"
    , Earde.Page_shell.launch_app_page ~user:"alice"
        ~page_class:"launch-feed" ~title:"T" ~content:"B" () )
  ; ( "launch_app_page (anonymous)"
    , Earde.Page_shell.launch_app_page ~page_class:"launch-feed" ~title:"T"
        ~content:"B" () )
  ; ( "launch_onboarding_page (member)"
    , Earde.Page_shell.launch_onboarding_page ~user:"alice"
        ~page_class:"launch-project-new" ~title:"T" ~content:"B" () )
  ; ( "launch_community_page (member)"
    , Earde.Community_shell.launch_community_page ~user:"alice" ~community
        ~sidebar:"S" ~page_class:"launch-community-overview" ~title:"T"
        ~content:"B" () )
  ; ( "launch_community_page (anonymous)"
    , Earde.Community_shell.launch_community_page ~community ~sidebar:"S"
        ~page_class:"launch-community-overview" ~title:"T" ~content:"B" () )
  ; ( "launch_community_surface_page (member)"
    , Earde.Community_shell.launch_community_surface_page ~user:"alice"
        ~community ~sidebar:"S" ~page_class:"launch-community-channel"
        ~title:"T" ~main_el:"<main class='cs-main'>B</main>" () )
  ]

let presence_case =
  fc_case "every chrome wrapper renders one footer with both links"
    (fun () ->
      List.iter
        (fun (label, html) ->
          Alcotest.(check int)
            (label ^ ": exactly one footer")
            1 (Html_assert.count_sub html footer_open);
          Alcotest.(check int)
            (label ^ ": exactly one Privacy link")
            1 (Html_assert.count_sub html privacy_link);
          Alcotest.(check int)
            (label ^ ": exactly one Analytics-preferences link")
            1 (Html_assert.count_sub html preferences_link))
        (footer_documents ()))

let outside_main_case =
  fc_case "footer sits below the shell, never inside <main>" (fun () ->
      List.iter
        (fun (label, html) ->
          (* The strip closes immediately before the .app close — outside
             every </main>-terminated fragment slice. *)
          Alcotest.(check bool)
            (label ^ ": footer directly precedes the app close")
            true
            (Html_assert.contains html "</footer>\n</div>");
          Alcotest.(check bool)
            (label ^ ": no footer inside main")
            false
            (Html_assert.contains html (footer_open ^ "</main>")
            || Html_assert.contains html ("<main" ^ footer_open)))
        (footer_documents ()))

let inert_case =
  fc_case "footer is links-only: no nested interactive controls" (fun () ->
      let footer = Earde.Page_shell.launch_footer in
      Alcotest.(check bool) "no form" false (Html_assert.contains footer "<form");
      Alcotest.(check bool) "no button" false (Html_assert.contains footer "<button");
      Alcotest.(check bool) "no input" false (Html_assert.contains footer "<input");
      Alcotest.(check bool) "no script" false (Html_assert.contains footer "<script");
      Alcotest.(check int) "exactly two links" 2 (Html_assert.count_sub footer "<a ");
      Alcotest.(check bool) "privacy link" true
        (Html_assert.contains footer privacy_link);
      Alcotest.(check bool) "preferences target is exact" true
        (Html_assert.contains footer preferences_link))

let message_sheet_case =
  fc_case "the chrome-free message sheet renders no footer" (fun () ->
      let html =
        Earde.Page_shell.launch_message_page ~title:"T" ~content:"B" ()
      in
      Alcotest.(check int) "no footer" 0 (Html_assert.count_sub html footer_open);
      Alcotest.(check int) "no preferences link" 0
        (Html_assert.count_sub html preferences_link))

let suite =
  [ presence_case; outside_main_case; inert_case; message_sheet_case ]

let suites =
  [ ("launch_footer", suite)
  ]
