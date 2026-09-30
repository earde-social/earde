(* The persistent top-right launch-topbar action (Components.launch_connect_cta).
   One shared value with the visible label "Connect a project", rendered
   byte-identically for anonymous and authenticated viewers on every launch
   topbar — the old anonymous "Bring a project" variant is retired — so the
   assertions here are both rendered-document and source-census: no wrapper
   may grow a private copy or a second wording.

   The second half pins the /bring exception: the page whose route the CTA
   targets suppresses the self-linking topbar action, keeps its own
   full-label primary action byte-identical, and carries the viewer's auth
   controls instead; the chrome-free message document gains nothing. Pure
   renderers plus a file scan (and the real /bring handler): no database, no
   server. *)

let cc_case name f = Alcotest.test_case name `Quick f

let community = Launch_fixture.nav_test_community

let cta_label = "<span>Connect a project</span></a>"

let github_path = "<path d='M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59"

(* Every live launch document whose top bar carries the persistent action:
   BOTH viewer arms of each app-chrome wrapper, the auth documents'
   deterministic anonymous topbar, and the viewer-independent entry chrome
   (/privacy — /bring opts out below). *)
let cta_documents () =
  [ ( "launch_entry_page (default chrome)"
    , Earde.Page_shell.launch_entry_page ~page_class:"launch-privacy"
        ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_app_page (member)"
    , Earde.Page_shell.launch_app_page ~user:"alice"
        ~page_class:"launch-feed" ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_app_page (anonymous)"
    , Earde.Page_shell.launch_app_page ~page_class:"launch-feed" ~title:"T"
        ~content:(Earde.Html.static "B") () )
  ; ( "launch_onboarding_page (member)"
    , Earde.Page_shell.launch_onboarding_page ~user:"alice"
        ~page_class:"launch-project-new" ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_onboarding_page (anonymous)"
    , Earde.Page_shell.launch_onboarding_page
        ~page_class:"launch-project-new" ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_community_page (member)"
    , Earde.Community_shell.launch_community_page ~user:"alice" ~community
        ~sidebar:(Earde.Html.static "S") ~page_class:"launch-community-overview" ~title:"T"
        ~content:(Earde.Html.static "B") () )
  ; ( "launch_community_page (anonymous)"
    , Earde.Community_shell.launch_community_page ~community ~sidebar:(Earde.Html.static "S")
        ~page_class:"launch-community-overview" ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_community_surface_page (member)"
    , Earde.Community_shell.launch_community_surface_page ~user:"alice" ~community
        ~sidebar:(Earde.Html.static "S") ~page_class:"launch-community-channel" ~title:"T"
        ~main_el:(Earde.Html.static "<main class='cs-main'>B</main>") () )
  ; ( "launch_auth_page (login)"
    , Earde.Page_shell.launch_auth_page ~page_class:"launch-login"
        ~title:"T" ~content:(Earde.Html.static "B") () )
  ; ( "launch_auth_page (signup)"
    , Earde.Page_shell.launch_auth_page ~page_class:"launch-signup"
        ~title:"T" ~content:(Earde.Html.static "B") () )
  ]

(* The shape of the control itself, on every wrapper and viewer arm that
   renders it. /feed, /search, /notifications, /u/:name, /settings and
   /admin are all launch_app_page; the community surfaces are
   launch_community_doc; so covering the wrappers covers the routes. *)
let shape_case =
  cc_case "every launch topbar renders one Connect-a-project action"
    (fun () ->
      List.iter
        (fun (label, html) ->
          Alcotest.(check int)
            (label ^ ": exactly one CTA")
            1 (Html_assert.count_sub html Launch_fixture.cta_open);
          Alcotest.(check bool)
            (label ^ ": mark then label")
            true
            (Html_assert.contains html (Launch_fixture.cta_open ^ "<svg"));
          Alcotest.(check bool)
            (label ^ ": visible label closes the element")
            true
            (Html_assert.contains html cta_label);
          (* exactly one local GitHub mark in the topbar chrome *)
          Alcotest.(check int)
            (label ^ ": one GitHub mark")
            1 (Html_assert.count_sub html github_path);
          Alcotest.(check bool)
            (label ^ ": mark is 15px and decorative")
            true
            (Html_assert.contains html
               "<svg width='15' height='15' viewBox='0 0 16 16' \
                fill='currentColor' aria-hidden='true'>");
          (* no plus glyph, no ochre outline, no retired anonymous wording *)
          List.iter
            (fun needle ->
              Alcotest.(check bool)
                (label ^ ": no " ^ needle)
                false (Html_assert.contains html needle))
            [ "&#65291; Connect"; "btn--outline-ochre"; "Bring a project" ];
          (* still a same-tab GET link to /bring: no form, no new window, no
             query string, no script hook *)
          List.iter
            (fun needle ->
              Alcotest.(check bool)
                (label ^ ": CTA keeps no " ^ needle)
                false
                (Html_assert.contains Launch_fixture.cta_open needle))
            [ "target="; "onclick"; "?"; "method"; "http" ])
        (cta_documents ()))

(* 10. the mark is inlined local markup — no request leaves the origin for
   it, and no icon font or sprite is introduced. *)
(* The rendered element, sliced out of a real document so the test never
   needs the helper itself in Components' public surface. *)
let cta_element html =
  let rec find_open from =
    if from + String.length Launch_fixture.cta_open > String.length html then
      Alcotest.fail "no launch Connect CTA in document"
    else if String.sub html from (String.length Launch_fixture.cta_open) = Launch_fixture.cta_open then from
    else find_open (from + 1)
  in
  let i = find_open 0 in
  let close = "</a>" in
  let rec find_close from =
    if from + String.length close > String.length html then
      Alcotest.fail "launch Connect CTA is unclosed"
    else if String.sub html from (String.length close) = close then
      from + String.length close
    else find_close (from + 1)
  in
  let j = find_close i in
  String.sub html i (j - i)

let no_external_asset_case =
  cc_case "the GitHub mark is inlined, not fetched" (fun () ->
      let cta =
        cta_element
          (Earde.Page_shell.launch_app_page ~user:"alice"
             ~page_class:"launch-feed" ~title:"T" ~content:(Earde.Html.static "B") ())
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("CTA references " ^ needle)
            false (Html_assert.contains cta needle))
        [ "http"; "//"; "<img"; "url("; "@font-face"; "github.com"
        ; "githubusercontent" ];
      Alcotest.(check bool) "mark is an inline svg" true
        (Html_assert.contains cta github_path))

(* Anonymous and authenticated viewers render the byte-identical control;
   only the surrounding auth controls differ. *)
let identical_across_viewers_case =
  cc_case "anonymous and member topbars share the byte-identical CTA"
    (fun () ->
      let anon =
        Earde.Page_shell.launch_app_page ~page_class:"launch-feed"
          ~title:"T" ~content:(Earde.Html.static "B") ()
      in
      let member =
        Earde.Page_shell.launch_app_page ~user:"alice"
          ~page_class:"launch-feed" ~title:"T" ~content:(Earde.Html.static "B") ()
      in
      Alcotest.(check string) "same rendered element"
        (cta_element anon) (cta_element member);
      (* the surrounding clusters stay viewer-appropriate *)
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("anonymous keeps " ^ needle) true
            (Html_assert.contains anon needle))
        [ "<a class='btn btn--secondary btn--auth' href='/login'>Log \
           in</a>"
        ; "<a class='btn btn--primary btn--auth' href='/signup'>Sign \
           up</a>" ];
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("member keeps " ^ needle) true
            (Html_assert.contains member needle))
        [ "class='bell'"; "userchip" ];
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("member drops " ^ needle) false
            (Html_assert.contains member needle))
        [ "btn--auth' href='/login'"; "btn--auth' href='/signup'" ])

(* Documents without an application topbar gain nothing. *)
let chromeless_case =
  cc_case "the message document keeps no application Connect action"
    (fun () ->
      let html =
        Earde.Page_shell.launch_message_page ~title:"T" ~content:(Earde.Html.static "B") ()
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("launch_message_page: no " ^ needle)
            false (Html_assert.contains html needle))
        [ "btn--connect-github"; github_path; "&#65291; Connect"
        ; "Bring a project" ])

(* 6./7. /bring's own primary action is untouched, byte-for-byte: element,
   method, action, classes, 20px mark and full label. Rendered through the
   real handler, so this is the page a browser gets. *)
let bring_start_form =
  "<form method='POST' action='/integrations/github/install/start'><button \
   type='submit' class='btn btn--dark'><svg width='20' height='20' \
   viewBox='0 0 16 16' fill='currentColor' aria-hidden='true'><path d='M8 \
   0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 \
   7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
   1.08.58 1.23.82.72 1.21 1.87.87 \
   2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
   0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.6 \
   7.6 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 \
   2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 \
   3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 \
   .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z'/></svg> \
   Connect a GitHub project</button></form>"

let bring_untouched_case =
  cc_case
    "/bring suppresses the self-linking topbar CTA and keeps its \
     byte-identical full-label primary action"
    (fun () ->
      let body =
        Bring_fixture.body_of (Bring_fixture.run ~session:Bring_fixture.member ())
      in
      Alcotest.(check int) "start form appears exactly once" 1
        (Html_assert.count_sub body bring_start_form);
      Alcotest.(check bool) "full label intact" true
        (Html_assert.contains body "Connect a GitHub project");
      (* The compact topbar action would self-link on this page, so it is
         absent — the page's one GitHub mark is the start button's, and the
         chrome still adds no form. *)
      Alcotest.(check int) "no topbar CTA" 0 (Html_assert.count_sub body Launch_fixture.cta_open);
      Alcotest.(check int) "no compact CTA class" 0
        (Html_assert.count_sub body "btn--connect-github");
      Alcotest.(check int) "one GitHub mark in total" 1
        (Html_assert.count_sub body github_path);
      Alcotest.(check int) "still exactly one form" 1
        (Html_assert.count_sub body "<form");
      (* The member topbar carries the viewer's controls instead: bell and
         plain user-chip link, never the logout <details> menu (that menu
         carries a POST form the form-free states forbid). *)
      Alcotest.(check bool) "member bell" true
        (Html_assert.contains body "class='bell'");
      Alcotest.(check bool) "member chip" true
        (Html_assert.contains body "<a class='userchip' href='/u/alice'>");
      Alcotest.(check bool) "no logout menu" false
        (Html_assert.contains body "launch-user__menu");
      (* Anonymous /bring keeps the auth controls in the topbar and still
         renders no form and no compact CTA. *)
      let anon = Bring_fixture.body_of (Bring_fixture.run ()) in
      Alcotest.(check int) "anonymous: no topbar CTA" 0
        (Html_assert.count_sub anon Launch_fixture.cta_open);
      Alcotest.(check bool) "anonymous: Log in control" true
        (Html_assert.contains anon
           "<a class='btn btn--secondary btn--auth' href='/login'>Log \
            in</a>");
      Alcotest.(check bool) "anonymous: Sign up control" true
        (Html_assert.contains anon
           "<a class='btn btn--primary btn--auth' href='/signup'>Sign \
            up</a>");
      Alcotest.(check int) "anonymous: no form" 0 (Html_assert.count_sub anon "<form"))

(* One definition, five call sites (the shared anonymous cluster plus the
   entry/app/onboarding/community member arms), no survivors: the control
   cannot drift between wrappers, and both the retired plus glyph and the
   retired anonymous "Bring a project" wording are gone from production. *)
let single_definition_case =
  cc_case "one shared CTA definition feeds every launch topbar" (fun () ->
      let shells = Source_census.launch_shells in
      Alcotest.(check int) "one definition" 1
        (Source_census.count_everywhere "let launch_connect_cta");
      Alcotest.(check int) "definition plus five call sites" 6
        (Source_census.count_everywhere "launch_connect_cta");
      Alcotest.(check int) "one shared anonymous cluster" 1
        (Source_census.count_everywhere "let topbar_anon_actions");
      Alcotest.(check int) "four topbar action clusters" 4
        (Html_assert.count_sub shells "<div class='topbar__actions'>");
      Source_census.absent_everywhere "retired plus glyph" "&#65291; Connect";
      Source_census.absent_everywhere "retired anonymous CTA copy"
        ">Bring a project<";
      (* The generic ochre outline button survives for its own callers, but
         no launch topbar uses it any more. *)
      Alcotest.(check int) "the launch shells drop btn--outline-ochre" 0
        (Html_assert.count_sub shells "btn--outline-ochre");
      (* CSS isolation: the modifier exists and is scoped under the top bar,
         and the responsive hide rule followed the class rename. *)
      let css = Source_census.read "static/css/earde.css" in
      Alcotest.(check bool) "modifier is topbar-scoped" true
        (Html_assert.contains css ".topbar__actions .btn--connect-github {");
      Alcotest.(check int) "modifier is never styled unscoped" 0
        (Html_assert.count_sub css "\n.btn--connect-github");
      Alcotest.(check bool) "narrow viewports still hide it" true
        (Html_assert.contains css ".topbar__actions .btn--connect-github { display: none; }"))

let suite =
  [ shape_case; no_external_asset_case; identical_across_viewers_case
  ; chromeless_case; bring_untouched_case; single_definition_case ]

let suites =
    (* The persistent top-right launch-topbar action: one shared compact
       GitHub-mark "Connect" link on every application topbar, no plus glyph
       left, anonymous and chromeless documents unchanged, and /bring's own
       full-label primary action still byte-identical. Pure renders plus a
       source/CSS census — DB-free. *)
  [ ("launch_connect_cta", suite)
  ]
