(* ===================== Show HN polish pass ===================================
   The first visual/copy polish pass: the repo-wide GitHub-copy census, the
   neutralized auth-page copy, the consolidated /feed right aside, and the
   shared community-settings shell (header band + grouped internal settings
   navigation + one active item) across every settings/management surface.
   Pure renderers plus the discovered-source census: no database. *)

let case name f = Alcotest.test_case name `Quick f

module Shell = Earde.Community_settings_shell

let must html s =
  Alcotest.(check bool) ("contains: " ^ s) true (Html_assert.contains html s)

let must_not html s =
  Alcotest.(check bool) ("must not contain: " ^ s) false (Html_assert.contains html s)

let index_of html needle from =
  let nl = String.length needle in
  let hl = String.length html in
  let rec loop i =
    if i + nl > hl then None
    else if String.sub html i nl = needle then Some i
    else loop (i + 1)
  in
  loop from

(* CSRF-carrying renderers need a live secret + sessions pipeline. *)
let render_with_request ~target f =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured := Some (f req);
    Dream.html ""
  in
  ignore (Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

(* --- 1. repo-wide GitHub-copy census ---------------------------------- *)

(* The retired members-don't-need-GitHub marketing (and the named Lwt/OCaml
   example, and the retired anonymous topbar wording) must not survive in
   any production source — renderer, component, or copy-documenting
   comment. Nothing here asserts that GitHub is required. *)
let github_copy_census_case =
  case "no GitHub-absence marketing survives in production sources"
    (fun () ->
      List.iter
        (Source_census.absent_everywhere "GitHub copy census")
        [ "No GitHub required"
        ; "never need GitHub"
        ; "join and chat without GitHub"
        ; "do not need a GitHub account"
        ; "GitHub is only needed"
        ; "Lwt to OCaml"
        ; ">Bring a project<"
        ; "GitHub is required" ])

(* --- 2. auth pages ------------------------------------------------------ *)

let render_login () =
  render_with_request ~target:"/login" (fun req -> Earde.Auth_pages.login_form req)

let render_signup () =
  render_with_request ~target:"/signup" (fun req ->
      Earde.Auth_pages.signup_form req)

let auth_copy_case =
  case "auth pages carry neutral copy and the shared topbar CTA" (fun () ->
      let login = render_login () in
      (* The old members-don't-need-GitHub notice became the
         maintainer-oriented action. *)
      must login "Maintaining an open-source project?";
      must login "<a href='/bring'>Connect it through GitHub</a>";
      must_not login "No GitHub required";
      must_not login "GitHub is only needed";
      must_not login "Members read, join and chat";
      must login Launch_fixture.cta_open;
      let signup = render_signup () in
      must signup
        "<p class='auth__sub'>One account for every community on Earde.</p>";
      must_not signup "No GitHub required";
      (* The maintainer pointer that was already neutral stays. *)
      must signup "connect a project through GitHub";
      must signup Launch_fixture.cta_open;
      (* One CTA per document; no second wording. *)
      Alcotest.(check int) "login: one CTA" 1
        (Html_assert.count_sub login Launch_fixture.cta_open);
      Alcotest.(check int) "signup: one CTA" 1
        (Html_assert.count_sub signup Launch_fixture.cta_open);
      must_not login "Bring a project";
      must_not signup "Bring a project")

(* --- 3. /feed right aside ---------------------------------------------- *)

let render_feed ?user ~is_logged_in ?(rail = []) () =
  render_with_request ~target:"/feed" (fun req ->
      Earde.Public_pages.feed_page ?user ~scope:"all" ~sort_mode:"hot" ~is_logged_in
        ~admin_usernames:[] ~rail_communities:rail ~user_votes:[]
        ~current_page:1 [] req)

let aside_slice html =
  match index_of html "<aside class='aside'" 0 with
  | None -> Alcotest.fail "no aside in feed document"
  | Some s -> (
      match index_of html "</aside>" s with
      | None -> Alcotest.fail "unterminated aside"
      | Some e -> String.sub html s (e - s))

let feed_aside_contract label aside =
  Alcotest.(check int)
    (label ^ ": one project-acquisition heading")
    1
    (Html_assert.count_sub aside "Bring your project");
  Alcotest.(check int)
    (label ^ ": one Connect-a-project CTA")
    1
    (Html_assert.count_sub aside ">Connect a project</a>");
  Alcotest.(check int)
    (label ^ ": exactly one /bring target")
    1
    (Html_assert.count_sub aside "href='/bring'");
  must_not aside "Start a pilot community";
  must_not aside "Bring your community";
  (* Unrelated aside content stays. *)
  must aside "Earde is early"

let feed_aside_case =
  case "feed aside keeps exactly one project-acquisition block" (fun () ->
      let anon = render_feed ~is_logged_in:false () in
      feed_aside_contract "anonymous" (aside_slice anon);
      let member =
        render_feed ~user:"alice" ~is_logged_in:true
          ~rail:[ Launch_fixture.nav_test_community ] ()
      in
      let member_aside = aside_slice member in
      feed_aside_contract "member" member_aside;
      (* The block is identical for both viewer states; only the real
         Following data differs. *)
      Alcotest.(check bool) "member aside keeps Following" true
        (Html_assert.contains member_aside "Following");
      must member_aside
        "<p class='aside__text'>Create a dedicated community home for your \
         open-source project, or connect it to an existing community.</p>")

(* --- 4. settings shell: the grouped nav (pure) -------------------------- *)

let all_items : (Shell.item * string) list =
  [ (Shell.Profile, "/settings?panel=profile")
  ; (Shell.Visibility, "/settings?panel=visibility")
  ; (Shell.Connected_projects, "/settings?panel=projects")
  ; (Shell.Home_requests, "/project-home-requests")
  ; (Shell.Connections, "/settings/connections")
  ; (Shell.Shared_threads, "/settings/shared-threads")
  ; (Shell.Channels, "/settings?panel=channels")
  ; (Shell.Members, "/settings?panel=members")
  ; (Shell.Manage_moderators, "/manage-mods")
  ; (Shell.Moderation, "/settings?panel=moderation")
  ; (Shell.Bans, "/settings?panel=bans")
  ]

let nav_grouping_case =
  case "grouped nav: complete top-mod set, ordered groups, one active"
    (fun () ->
      let nav =
        Shell.nav ~slug:"polish" ~active:Shell.Visibility
          ~can_complete_setup:false ~network_manager:true ()
      in
      (* Group headings in canonical order. *)
      let pos needle =
        match index_of nav needle 0 with
        | Some i -> i
        | None -> Alcotest.failf "nav lacks %s" needle
      in
      let community = pos ">Community</p>" in
      let network = pos ">Network</p>" in
      let structure = pos ">Structure</p>" in
      let people = pos ">People</p>" in
      Alcotest.(check bool) "Community < Network" true (community < network);
      Alcotest.(check bool) "Network < Structure" true (network < structure);
      Alcotest.(check bool) "Structure < People" true (structure < people);
      (* Every entry present exactly once, server-built from the slug. *)
      List.iter
        (fun (_, suffix) ->
          Alcotest.(check int)
            ("one entry for " ^ suffix)
            1
            (Html_assert.count_sub nav ("href='/c/polish" ^ suffix ^ "'")))
        all_items;
      (* Exactly one active item, whichever item is active. *)
      List.iter
        (fun (item, suffix) ->
          let nav =
            Shell.nav ~slug:"polish" ~active:item ~can_complete_setup:false
              ~network_manager:true ()
          in
          Alcotest.(check int)
            ("one active for " ^ suffix)
            1
            (Html_assert.count_sub nav "cm-index-link--active");
          Alcotest.(check bool)
            ("active is " ^ suffix)
            true
            (Html_assert.contains nav
               ("cm-index-link--active' href='/c/polish" ^ suffix ^ "'")))
        all_items;
      (* Danger styling stays confined to Bans. *)
      Alcotest.(check int) "one danger link" 1
        (Html_assert.count_sub nav "cm-index-link--danger");
      Alcotest.(check bool) "danger is Bans" true
        (Html_assert.contains nav
           "cm-index-link--danger' href='/c/polish/settings?panel=bans'"))

let nav_role_subset_case =
  case "grouped nav: regular moderators get the coherent subset" (fun () ->
      let nav =
        Shell.nav ~slug:"polish" ~active:Shell.Moderation
          ~can_complete_setup:false ~network_manager:false ()
      in
      (* No Network group, no entry the viewer categorically cannot open. *)
      must_not nav ">Network</p>";
      must_not nav "/project-home-requests";
      must_not nav "/settings/connections";
      must_not nav "/settings/shared-threads";
      must_not nav "?panel=projects";
      must_not nav "/manage-mods";
      must_not nav "/setup'";
      (* The rest of the groups survive intact. *)
      List.iter (must nav)
        [ ">Community</p>"; ">Structure</p>"; ">People</p>";
          "?panel=profile"; "?panel=visibility"; "?panel=channels";
          "?panel=members"; "?panel=moderation"; "?panel=bans" ];
      Alcotest.(check int) "one active" 1
        (Html_assert.count_sub nav "cm-index-link--active"))

let nav_setup_case =
  case "grouped nav: the setup link renders only for eligible drafts"
    (fun () ->
      let without =
        Shell.nav ~slug:"polish" ~active:Shell.Profile
          ~can_complete_setup:false ~network_manager:true ()
      in
      must_not without "Complete setup and publish";
      let with_setup =
        Shell.nav ~slug:"polish" ~active:Shell.Profile
          ~can_complete_setup:true ~network_manager:true ()
      in
      Alcotest.(check int) "one setup link" 1
        (Html_assert.count_sub with_setup "href='/c/polish/setup'");
      must with_setup ">Complete setup and publish</a>")

(* --- 5. settings surfaces: the shared DOM contract ---------------------- *)

(* One community rail, one community sidebar with Settings active, one
   internal settings navigation with exactly one active item, the shared
   header band, and no duplicate nav. *)
let assert_shell_contract label ~slug ~active_href html =
  Alcotest.(check int)
    (label ^ ": one settings wrap")
    1
    (Html_assert.count_sub html "<div class='cm-wrap cm-wrap--settings'>");
  Alcotest.(check int)
    (label ^ ": one settings index")
    1
    (Html_assert.count_sub html "<nav class='cm-index'>");
  Alcotest.(check int)
    (label ^ ": one index title")
    1
    (Html_assert.count_sub html "<div class='cm-index-title'>Settings</div>");
  Alcotest.(check int)
    (label ^ ": one active internal item")
    1
    (Html_assert.count_sub html "cm-index-link--active");
  Alcotest.(check bool)
    (label ^ ": the active item is " ^ active_href)
    true
    (Html_assert.contains html ("cm-index-link--active' href='" ^ active_href ^ "'"));
  Alcotest.(check int)
    (label ^ ": one header band")
    1
    (Html_assert.count_sub html "<div class='cm-head'>");
  Alcotest.(check bool)
    (label ^ ": header names the community")
    true
    (Html_assert.contains html ("/c/" ^ slug ^ " <span class='accent'>settings</span>"))

let polish_community : Earde.Community_types.community =
  { Launch_fixture.nav_test_community with id = 777; slug = "polish"; name = "Polish" }

let render_settings ?(target = "/c/polish/settings") ~is_admin ~is_top_mod ()
    =
  render_with_request ~target (fun req ->
      Earde.Community_settings_pages.community_settings_page ~is_admin ~is_top_mod
        ~open_reports_count:0 ~community:polish_community ~mods:[]
        ~banned_users:[] ~members:[] ~sections:[] ~channels:[] req)

let settings_surface_case =
  case "settings hub: base and query panels keep the shared contract"
    (fun () ->
      let base = render_settings ~is_admin:false ~is_top_mod:true () in
      assert_shell_contract "base" ~slug:"polish"
        ~active_href:"/c/polish/settings?panel=visibility" base;
      (* The outer sidebar marks Settings as the active community item. *)
      Alcotest.(check bool) "sidebar Settings active" true
        (Html_assert.contains base
           "navitem--active' href='/c/polish/settings'");
      let members =
        render_settings ~target:"/c/polish/settings?panel=members"
          ~is_admin:false ~is_top_mod:true ()
      in
      assert_shell_contract "panel=members" ~slug:"polish"
        ~active_href:"/c/polish/settings?panel=members" members)

(* The internal settings index alone (the outer community sidebar keeps
   its own, always-rendered "Network" group heading for Moderation log). *)
let index_slice html =
  match index_of html "<nav class='cm-index'>" 0 with
  | None -> Alcotest.fail "no settings index in document"
  | Some s -> (
      match index_of html "</nav>" s with
      | None -> Alcotest.fail "unterminated settings index"
      | Some e -> String.sub html s (e - s))

let settings_role_case =
  case "settings hub: regular moderators keep the coherent subset"
    (fun () ->
      let regular =
        index_slice (render_settings ~is_admin:false ~is_top_mod:false ())
      in
      must_not regular ">Network</p>";
      must_not regular "/c/polish/project-home-requests";
      must_not regular "/c/polish/settings/connections";
      must_not regular "/c/polish/settings/shared-threads";
      must_not regular "/c/polish/manage-mods";
      let top =
        index_slice (render_settings ~is_admin:false ~is_top_mod:true ())
      in
      List.iter (must top)
        [ ">Network</p>"; "/c/polish/project-home-requests";
          "/c/polish/settings/connections";
          "/c/polish/settings/shared-threads"; "/c/polish/manage-mods" ])

let manage_mods_case =
  case "manage-mods renders inside the shell with Manage moderators active"
    (fun () ->
      let html =
        render_with_request ~target:"/c/polish/manage-mods" (fun req ->
            Earde.Moderation_pages.manage_mods_page ~is_admin:false
              ~current_user_role:(Some "top_mod") ~channels:[] ~sections:[]
              ~community:polish_community ~mods:[] req)
      in
      assert_shell_contract "manage-mods" ~slug:"polish"
        ~active_href:"/c/polish/manage-mods" html;
      (* The roster panels and the add form survive inside the panel
         column. *)
      must html "Top Mods";
      must html "action='/c/polish/manage-mods/add'";
      Alcotest.(check bool) "sidebar Settings active" true
        (Html_assert.contains html "navitem--active' href='/c/polish/settings'"))

let reports_case =
  case "reports queue renders inside the shell with Moderation active"
    (fun () ->
      let html =
        render_with_request ~target:"/c/polish/reports" (fun req ->
            Earde.Moderation_pages.reports_queue_page ~is_admin:false ~is_top_mod:true
              ~channels:[] ~sections:[] ~community:polish_community
              ~status:Earde.Report_store.Report_open ~reports:[] ~previews:[] req)
      in
      assert_shell_contract "reports" ~slug:"polish"
        ~active_href:"/c/polish/settings?panel=moderation" html;
      (* The queue's own status tabs stay inside the panel. *)
      must html "cm-nav-link cm-nav-link--active";
      must html "Reports queue")

(* The dedicated management routes: fabricated shell tuples, exactly like
   the sibling review-page suite. *)
let mgmt_shell =
  ( polish_community,
    [ polish_community ],
    "<aside class='sidebar' aria-label='Polish community'><a class='navitem \
     navitem--pad navitem--active' href='/c/polish/settings'>Settings</a></aside>"
  )

let connections_case =
  case "connections management renders inside the shell, Connections active"
    (fun () ->
      let state : Earde.Community_connections_pages.state =
        { community = { name = "Polish"; slug = "polish"; eligible = true };
          accepted = [];
          incoming = [];
          outgoing = []
        }
      in
      let html =
        Earde.Community_connections_pages.management_page ~shell:mgmt_shell
          ~state ~feedback:None ()
      in
      assert_shell_contract "connections" ~slug:"polish"
        ~active_href:"/c/polish/settings/connections" html;
      must html "community-connections";
      must_not html "launch-review-context")

let shared_threads_case =
  case "shared-threads management renders inside the shell, Shared threads \
        active; the share page keeps its own document"
    (fun () ->
      let state : Earde.Shared_thread_placement_pages.management_state =
        { community_name = "Polish";
          community_slug = "polish";
          community_eligible = true;
          sections_enabled = false;
          section_options = [];
          incoming = [];
          outgoing = [];
          shared_into = [];
          shared_from = []
        }
      in
      let html =
        Earde.Shared_thread_placement_pages.management_page
          ~shell:mgmt_shell ~state ~notice:None ~feedback:None ()
      in
      assert_shell_contract "shared threads" ~slug:"polish"
        ~active_href:"/c/polish/settings/shared-threads" html;
      must html "community-shared-threads";
      must_not html "launch-review-context";
      (* The per-thread Share page is a member workflow, not a settings
         surface: no settings shell, context block intact. *)
      let share_state : Earde.Shared_thread_placement_pages.share_state =
        { share_thread_title = "T";
          share_origin_name = "Polish";
          share_origin_slug = "polish";
          share_thread_path = "/c/polish/t/1";
          share_candidates = [];
          share_placements = [];
          share_manage_connections = false
        }
      in
      let share =
        Earde.Shared_thread_placement_pages.share_page ~shell:mgmt_shell
          ~state:share_state ~notice:None ~feedback:None ()
      in
      must_not share "cm-wrap--settings";
      must_not share "<nav class='cm-index'>";
      must share "launch-review-context")

let suite =
  [ github_copy_census_case; auth_copy_case; feed_aside_case;
    nav_grouping_case; nav_role_subset_case; nav_setup_case;
    settings_surface_case; settings_role_case; manage_mods_case;
    reports_case; connections_case; shared_threads_case ]

let suites =
  [ ("show_hn_polish", suite)
  ]
