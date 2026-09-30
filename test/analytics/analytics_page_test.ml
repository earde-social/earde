module AnT = Earde.Analytics.For_testing

(* Renders a live launch document through real session middleware, optionally
   with a session user_id value, under the enabled test analytics config.
   [launch_app_page] is the wrapper that threads both an optional request and an
   optional (id, visibility) analytics pair, so it exercises the shared
   [analytics_assets] contract on a document a real route actually serves. *)
let render_launch_doc ?session_user ?analytics_community () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           Lwt.bind
             (match session_user with
             | Some v -> Dream.set_session_field req "user_id" v
             | None -> Lwt.return_unit)
             (fun () ->
               rendered :=
                 Earde.Components.launch_app_page ~request:req
                   ?analytics_community ~page_class:"launch-feed" ~title:"T"
                   ~content:"<p>body</p>" ();
               Dream.html ""))
         (Dream.request ~method_:`GET ~target:"/feed" ""))
  in
  !rendered

let analytics_channel : Earde.Channel_store.channel =
  { id = 1; community_id = 9; slug = "general"; name = "general"; topic = None
  ; position = 0; is_archived = false; created_at = "2026-01-01 00:00:00"
  ; indexable = true }

let with_enabled_config f =
  AnT.use_enabled_test_configuration ();
  Fun.protect ~finally:AnT.clear_configuration_override f

let test_community ~id ~visibility : Earde.Community_types.community =
  { Earde.Community_types.id; slug = "testc"; name = "Test Community"; description = None;
    rules = None; avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = true; visibility; indexable = true;
    is_network_community = false; onboarding_state = Earde.Community_types.Community_published;
    discoverable = true }

(* Renders the search results page through real session middleware (the page
   embeds a CSRF tag). Analytics configuration is whatever the caller
   installed. *)
let render_search ?(page = 1) ?(tab = "posts") ?(communities = [])
    ?(users = []) ?(posts = []) ?(comments = []) query =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered :=
             Earde.Pages.search_results_page ~admin_usernames:[] [] page tab
               query communities users posts comments req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/search" ""))
  in
  !rendered

(* The full opening tag of the search analytics container, so leak assertions
   can look at exactly the markup that feeds analytics.js — the page itself
   legitimately echoes the query elsewhere (input value, pager hrefs). *)
let sr_analytics_tag html =
  match Html_assert.index_of html "<div id='sr-analytics'" with
  | None -> None
  | Some i -> (
      match String.index_from_opt html i '>' with
      | None -> None
      | Some j -> Some (String.sub html i (j - i + 1)))

let suites =
    (* §4.2 identity attribute: exactly user:<id> on authenticated pages,
       nothing anywhere else, and no person data in analytics attributes. *)
  [ ( "analytics_identity_attrs"
    , [ Analytics_fixture.an_case "authenticated launch document emits exactly user:42" (fun () ->
            with_enabled_config (fun () ->
                let html = render_launch_doc ~session_user:"42" () in
                Alcotest.(check (option string)) "identity attr"
                  (Some "user:42")
                  (Html_assert.attr_value html "data-analytics-user")))
      ; Analytics_fixture.an_case "anonymous launch document emits no identity attribute" (fun () ->
            with_enabled_config (fun () ->
                let html = render_launch_doc () in
                Alcotest.(check (option string)) "no identity" None
                  (Html_assert.attr_value html "data-analytics-user")))
      ; Analytics_fixture.an_case "malformed session value emits no identity" (fun () ->
            with_enabled_config (fun () ->
                let html = render_launch_doc ~session_user:"not-a-number" () in
                Alcotest.(check (option string)) "no identity" None
                  (Html_assert.attr_value html "data-analytics-user")))
      ; Analytics_fixture.an_case "disabled analytics emits neither identity nor group"
          (fun () ->
            AnT.use_disabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html =
                  render_launch_doc ~session_user:"42" ~analytics_community:(7, Earde.Community_types.Community_public)
                    ()
                in
                Alcotest.(check bool) "no identity attr" false
                  (Html_assert.contains html "data-analytics-user");
                Alcotest.(check bool) "no group attr" false
                  (Html_assert.contains html "data-analytics-group")))
      ; Analytics_fixture.an_case "only the two analytics attributes exist; no person data"
          (fun () ->
            with_enabled_config (fun () ->
                let html =
                  render_launch_doc ~session_user:"42" ~analytics_community:(7, Earde.Community_types.Community_public)
                    ()
                in
                (* exactly one identity and one group attribute (the other
                   data-analytics-* hits are the banner's own accept /
                   refuse / error control hooks, which carry no values) *)
                Alcotest.(check int) "one identity attr" 1
                  (Html_assert.count_sub html "data-analytics-user='");
                Alcotest.(check int) "one group attr" 1
                  (Html_assert.count_sub html "data-analytics-group='");
                Alcotest.(check bool) "no username attr" false
                  (Html_assert.contains html "data-analytics-username");
                Alcotest.(check bool) "no email anywhere in analytics root"
                  false
                  (Html_assert.contains html "data-analytics-email")))
      ] )
    (* §5.3 group attribute: exactly community:<id> on community-scoped
       pages, actively absent on global pages, across all wrappers. *)
  ; ( "analytics_group_attrs"
    , [ Analytics_fixture.an_case "community-bound launch document emits exactly community:7" (fun () ->
            with_enabled_config (fun () ->
                let html = render_launch_doc ~analytics_community:(7, Earde.Community_types.Community_public) () in
                Alcotest.(check (option string)) "group attr"
                  (Some "community:7")
                  (Html_assert.attr_value html "data-analytics-group")))
      ; Analytics_fixture.an_case "global launch document emits no group attribute" (fun () ->
            with_enabled_config (fun () ->
                let html = render_launch_doc () in
                Alcotest.(check (option string)) "no group" None
                  (Html_assert.attr_value html "data-analytics-group")))
      ; Analytics_fixture.an_case "launch community document (public) carries the group key"
          (fun () ->
            with_enabled_config (fun () ->
                let community =
                  test_community ~id:9 ~visibility:Earde.Community_types.Community_public
                in
                let html =
                  Earde.Components.launch_community_page ~community
                    ~sidebar:"SIDE"
                    ~page_class:"launch-community-overview" ~title:"T"
                    ~content:"MAIN" ()
                in
                Alcotest.(check (option string)) "group attr"
                  (Some "community:9")
                  (Html_assert.attr_value html "data-analytics-group");
                Alcotest.(check bool) "public: no replay block" false
                  (Html_assert.contains html "ph-no-capture")))
      ; Analytics_fixture.an_case
          "launch community document (private) keeps group key + \
           ph-no-capture on .shell"
          (fun () ->
            with_enabled_config (fun () ->
                let community =
                  test_community ~id:9 ~visibility:Earde.Community_types.Community_private
                in
                let html =
                  Earde.Components.launch_community_page ~community
                    ~sidebar:"SIDE"
                    ~page_class:"launch-community-overview" ~title:"T"
                    ~content:"MAIN" ()
                in
                Alcotest.(check (option string)) "group attr"
                  (Some "community:9")
                  (Html_assert.attr_value html "data-analytics-group");
                (* The replay guard rides on the existing .shell element, so
                   no wrapper div is interposed anywhere in the document. *)
                Alcotest.(check bool) "content replay-blocked" true
                  (Html_assert.contains html "class='shell ph-no-capture'");
                Alcotest.(check bool) "no wrapper div" false
                  (Html_assert.contains html "<div class='ph-no-capture'>");
                (* only the group attribute — no identity (no request) and
                   no visibility/name leak *)
                Alcotest.(check int) "one group attr" 1
                  (Html_assert.count_sub html "data-analytics-group='");
                Alcotest.(check int) "no identity attr" 0
                  (Html_assert.count_sub html "data-analytics-user='");
                Alcotest.(check bool) "no visibility leak" false
                  (Html_assert.contains html "data-analytics-visibility")))
        (* The live private-community chat/thread/section documents build
           their own <main class='cs-main …'> and must carry the replay class
           ON that element: a wrapper div detaches the chat
           head/scroller/composer from the .cs-main flex column and collapses
           the message pane. *)
      ; Analytics_fixture.an_case "private channel document marks cs-main itself" (fun () ->
            with_enabled_config (fun () ->
                let community =
                  test_community ~id:9 ~visibility:Earde.Community_types.Community_private
                in
                let html =
                  Http_fixture.with_session_request ~target:"/c/testc/ch/general"
                    (fun req ->
                      Earde.Pages.community_channel_shell_page ~user:"alice"
                        ~is_member:true ~rail_communities:[ community ]
                        ~channels:[ analytics_channel ] ~sections:[]
                        ~channel:analytics_channel ~messages:[] ~community req)
                in
                Alcotest.(check bool) "cs-main carries the class" true
                  (Html_assert.contains html "<main class='cs-main ph-no-capture'");
                Alcotest.(check bool) "no wrapper div inside cs-main" false
                  (Html_assert.contains html "<div class='ph-no-capture'>");
                Alcotest.(check (option string)) "group attr"
                  (Some "community:9")
                  (Html_assert.attr_value html "data-analytics-group")))
      ; Analytics_fixture.an_case "community-bound launch documents pass the key through"
          (fun () ->
            with_enabled_config (fun () ->
                let check_attr label expected html =
                  Alcotest.(check (option string)) label expected
                    (Html_assert.attr_value html "data-analytics-group")
                in
                (* launch_app_page carries the pair explicitly for global
                   surfaces whose content is community-bound (post creation
                   and its join gate). *)
                check_attr "launch_app_page (new-post / join gate)"
                  (Some "community:8")
                  (Analytics_fixture.launch_doc ~analytics_community:(8, Earde.Community_types.Community_public) ());
                (* The community documents derive it from the record. *)
                check_attr "launch_community_page" (Some "community:5")
                  (Earde.Components.launch_community_page
                     ~community:
                       (test_community ~id:5
                          ~visibility:Earde.Community_types.Community_public)
                     ~sidebar:"SIDE"
                     ~page_class:"launch-community-overview" ~title:"T"
                     ~content:"B" ());
                check_attr "launch_community_surface_page" (Some "community:6")
                  (Earde.Components.launch_community_surface_page
                     ~community:
                       (test_community ~id:6
                          ~visibility:Earde.Community_types.Community_public)
                     ~sidebar:"SIDE" ~page_class:"launch-community-channel"
                     ~title:"T" ~main_el:"<main class='cs-main'>B</main>" ())))
      ; Analytics_fixture.an_case "global launch documents emit no group" (fun () ->
            with_enabled_config (fun () ->
                List.iter
                  (fun (label, html) ->
                    Alcotest.(check (option string)) label None
                      (Html_assert.attr_value html "data-analytics-group"))
                  [ ("launch_app_page", Analytics_fixture.launch_doc ())
                  ; ( "launch_entry_page",
                      Earde.Components.launch_entry_page
                        ~page_class:"launch-bring" ~title:"T" ~content:"B" () )
                  ; ( "launch_auth_page",
                      Earde.Components.launch_auth_page
                        ~page_class:"launch-login" ~title:"T" ~content:"B" () )
                  ; ( "launch_message_page",
                      Earde.Components.launch_message_page ~title:"T"
                        ~content:"B" () )
                  ; ( "launch_onboarding_page",
                      Earde.Components.launch_onboarding_page
                        ~page_class:"launch-project-new" ~title:"T"
                        ~content:"B" () )
                  ]))
      ] )
    (* §2.4 search metadata container: closed values only, emitted only for
       an executed non-empty search, never carrying the query. *)
  ; ( "analytics_search_metadata"
    , [ Analytics_fixture.an_case "executed search emits the closed container" (fun () ->
            with_enabled_config (fun () ->
                let html =
                  render_search ~page:3 ~tab:"communities"
                    ~communities:
                      [ test_community ~id:1
                          ~visibility:Earde.Community_types.Community_public
                      ; test_community ~id:2
                          ~visibility:Earde.Community_types.Community_public
                      ]
                    "ocaml"
                in
                Alcotest.(check (option string)) "tab" (Some "communities")
                  (Html_assert.attr_value html "data-analytics-search-tab");
                Alcotest.(check (option string)) "count is rows rendered"
                  (Some "2")
                  (Html_assert.attr_value html "data-analytics-search-result-count");
                Alcotest.(check (option string)) "page" (Some "3")
                  (Html_assert.attr_value html "data-analytics-search-page");
                (* exactly the three closed attributes, nothing else *)
                Alcotest.(check int) "three attributes" 3
                  (Html_assert.count_sub html "data-analytics-search-")))
      ; Analytics_fixture.an_case "empty search emits no search analytics metadata" (fun () ->
            with_enabled_config (fun () ->
                let html = render_search "" in
                Alcotest.(check bool) "no container" false
                  (Html_assert.contains html "sr-analytics");
                Alcotest.(check bool) "no attributes" false
                  (Html_assert.contains html "data-analytics-search-")))
      ; Analytics_fixture.an_case "disabled analytics emits no container" (fun () ->
            AnT.use_disabled_test_configuration ();
            Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                let html = render_search "ocaml" in
                Alcotest.(check bool) "no container" false
                  (Html_assert.contains html "sr-analytics")))
      ; Analytics_fixture.an_case "every supported tab value is emitted verbatim" (fun () ->
            with_enabled_config (fun () ->
                List.iter
                  (fun tab ->
                    Alcotest.(check (option string)) tab (Some tab)
                      (Html_assert.attr_value (render_search ~tab "x")
                         "data-analytics-search-tab"))
                  [ "posts"; "communities"; "comments"; "people" ]))
      ; Analytics_fixture.an_case "arbitrary tab values are normalized, never echoed" (fun () ->
            with_enabled_config (fun () ->
                let html = render_search ~tab:"weird-tab" "x" in
                (* the renderer's catch-all shows the Threads tab, so the
                   authoritative reported tab is posts *)
                Alcotest.(check (option string)) "normalized" (Some "posts")
                  (Html_assert.attr_value html "data-analytics-search-tab");
                match sr_analytics_tag html with
                | None -> Alcotest.fail "container missing"
                | Some tag ->
                    Alcotest.(check bool) "raw tab not in container" false
                      (Html_assert.contains tag "weird-tab")))
      ; Analytics_fixture.an_case "the query never appears in the analytics container"
          (fun () ->
            with_enabled_config (fun () ->
                let html = render_search ~tab:"posts" "sekret-term" in
                (* the page legitimately echoes the query (input value,
                   pager hrefs — masked/stripped by other layers); the
                   analytics container must not *)
                Alcotest.(check bool) "page echoes query" true
                  (Html_assert.contains html "sekret-term");
                match sr_analytics_tag html with
                | None -> Alcotest.fail "container missing"
                | Some tag ->
                    Alcotest.(check bool) "no query in container" false
                      (Html_assert.contains tag "sekret-term")))
      ; Analytics_fixture.an_case "non-positive page is clamped to the effective page 1"
          (fun () ->
            with_enabled_config (fun () ->
                Alcotest.(check (option string)) "page 0 -> 1" (Some "1")
                  (Html_assert.attr_value (render_search ~page:0 "x")
                     "data-analytics-search-page")))
      ; Analytics_fixture.an_case "result_count is authoritative for the active tab" (fun () ->
            with_enabled_config (fun () ->
                (* a community row exists, but the active tab is people →
                   the count reflects the rendered people rows: 0 *)
                let html =
                  render_search ~tab:"people"
                    ~communities:
                      [ test_community ~id:1
                          ~visibility:Earde.Community_types.Community_public
                      ]
                    "x"
                in
                Alcotest.(check (option string)) "count 0" (Some "0")
                  (Html_assert.attr_value html "data-analytics-search-result-count")))
      ] )
  ]
