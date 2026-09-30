(* Ordinary navigation entry points — pure renderers (no server, no session
   middleware). Generic community creation is admin-only and reachable only by
   typing /new-community manually; normal navigation (topbar, rail, sidebars,
   creation-flow fallbacks) must advertise the /bring onboarding entry point
   instead — including for global admins. *)
let nav_case name f = Alcotest.test_case name `Quick f

let nav_check html =
  let must s = Alcotest.(check bool) ("contains: " ^ s) true (Html_assert.contains html s) in
  let must_not s =
    Alcotest.(check bool) ("must not contain: " ^ s) false (Html_assert.contains html s)
  in
  (must, must_not)

(* The launch chrome under test is the real one every app route renders: the
   54px top bar and the dark icon rail of Components.launch_app_page. *)
let nav_launch_doc ?user ?rail_communities ?session () =
  match session with
  | None ->
      Earde.Page_shell.launch_app_page ?user ?rail_communities
        ~page_class:"launch-feed" ~title:"Feed" ~content:"" ()
  | Some fields ->
      let rendered = ref "" in
      let (_ : Dream.response) =
        Lwt_main.run
          (Dream.memory_sessions
             (fun req ->
               Lwt.bind
                 (Lwt_list.iter_s
                    (fun (k, v) -> Dream.set_session_field req k v)
                    fields)
                 (fun () ->
                   rendered :=
                     Earde.Page_shell.launch_app_page ~request:req ?user
                       ?rail_communities ~page_class:"launch-feed"
                       ~title:"Feed" ~content:"" ();
                   Dream.html ""))
             (Dream.request ~method_:`GET ~target:"/feed" ""))
      in
      !rendered

let nav_entry_cases =
  [ nav_case "logged-in launch top bar advertises /bring" (fun () ->
        let html = nav_launch_doc ~user:"alice" () in
        let must, must_not = nav_check html in
        must "href='/bring'";
        must "Connect a project";
        must_not "Start community";
        must_not "/new-community")
  ; nav_case "logged-in launch top bar keeps unrelated actions" (fun () ->
        let html = nav_launch_doc ~user:"alice" () in
        let must, must_not = nav_check html in
        must "href='/notifications'";
        (* The bell is unconditional; the badge is not. This document is
           rendered with no request, so there is no count and therefore no
           badge element and no "0" anywhere. *)
        must "class='bell'";
        must_not "notif-badge";
        must "href='/settings'";
        must "Log out")
  ; nav_case "admin launch top bar offers /bring, not the legacy route"
      (fun () ->
        let html =
          nav_launch_doc ~user:"root"
            ~session:[ ("user_id", "1"); ("is_admin", "true") ] ()
        in
        let must, must_not = nav_check html in
        must "href='/admin'";
        must "href='/bring'";
        must_not "/new-community")
  ; nav_case "anonymous launch top bar offers login/signup and /bring"
      (fun () ->
        let html = nav_launch_doc () in
        let must, must_not = nav_check html in
        must "href='/login'";
        must "href='/signup'";
        (* Unlike the pre-launch anonymous navbar, the launch chrome invites
           anonymous visitors into the onboarding entry point too. *)
        must "href='/bring'";
        must_not "/new-community")
  ; nav_case "empty rail still offers the onboarding entry point" (fun () ->
        let html = nav_launch_doc ~user:"alice" ~rail_communities:[] () in
        let must, must_not = nav_check html in
        must "rail__item--add' href='/bring'";
        must "Connect a project";
        must_not "/new-community")
  ; nav_case "populated rail keeps real community tiles only" (fun () ->
        let html =
          nav_launch_doc ~user:"alice" ~rail_communities:[ Launch_fixture.nav_test_community ] ()
        in
        let must, must_not = nav_check html in
        must "href='/c/ocaml/ch/general'";
        must "rail__item--add' href='/bring'";
        must_not "/new-community")
  ; nav_case "launch rail add-tile targets /bring" (fun () ->
        let html = nav_launch_doc ~user:"alice" () in
        let must, must_not = nav_check html in
        must "rail__item--add' href='/bring'";
        must_not "/new-community")
  ; nav_case "choose-community fallback connects a project" (fun () ->
        let html =
          Earde.Post_pages.choose_community_page ~user:"alice" [ Launch_fixture.nav_test_community ] in
        let must, must_not = nav_check html in
        must "href='/bring'";
        must "Connect a project";
        must "Post here";
        must_not "/new-community";
        must_not "Start a community")
  ; nav_case "admin creation form renderer retained" (fun () ->
        (* Compile-time retention check: the admin-only legacy form (and its
           request-taking signature) must not be removed by this de-linking.
           Pass 19 added the optional launch rail parameter; the renderer and
           its request-taking shape remain. *)
        ignore
          (Earde.Community_settings_pages.new_community_form
            : ?user:string ->
              ?rail_communities:Earde.Community_types.community list ->
              Dream.request ->
              string))
  ]

let suites =
    (* Ordinary navigation must advertise /bring, never the admin-only
       /new-community flow (see nav_entry_cases). *)
  [ ( "app_nav_entry_points", nav_entry_cases )
  ]
