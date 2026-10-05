(* === Legacy-asset census (final cleanup pass) =============================
   A static/regression guard over the PRODUCTION sources and a set of live
   rendered documents, proving the removals of this pass stay removed:

   - GET /earde-hq-dashboard has no route registration, handler or renderer,
     and the router has no catch-all that could answer it — so the path falls
     to Dream's normal unknown-route response;
   - none of the deleted chrome wrappers has a production caller;
   - no production source or rendered document references a deleted
     stylesheet, the Tailwind CDN, or Google Fonts;
   - every launch document loads exactly the two approved local stylesheets;
   - the redirects, routes and shared renderers that had to survive are all
     still registered and still render on launch chrome.

   Source-census only — no database, no server, no shell commands. The scanned
   set is every lib/*.ml{,i} plus bin/main.ml, declared as dune deps; the route
   table is lib/app_routes.ml. Comments
   count: a stale note naming a deleted file is as much a lie as a live link,
   so the OCaml sources are scanned verbatim. *)

let lc_case name f = Alcotest.test_case name `Quick f

(* The route table lives in lib/app_routes.ml; bin/main.ml only mounts it. *)
let main_ml = List.assoc "lib/app_routes.ml" Source_census.production_sources
let count_in haystack needle = Html_assert.count_sub haystack needle

(* Registrations are compared as token sequences: the formatter decides where
   a long route line breaks, and that must not decide whether it exists. *)
let squash s =
  String.split_on_char '\n' s
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (( <> ) "")
  |> String.concat " "

let main_ml_tokens = squash main_ml

(* --- 1. no production route for the retired KPI dashboard -------------- *)
let hq_route_case =
  lc_case "no /earde-hq-dashboard route, handler or renderer" (fun () ->
      (* The route table is the authority: no registration of any method. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            (Printf.sprintf "route table free of %S" needle)
            false
            (Html_assert.contains main_ml needle))
        [ "\"/earde-hq-dashboard\""; "hq_dashboard" ];
      (* Handler and renderer are gone from every production source. The
         page-view exclusion filter in handlers.ml deliberately still names
         the retired path (unchanged analytics behaviour for 404 traffic),
         so the symbol census is what proves the surface is gone. *)
      Source_census.absent_everywhere "hq handler" "hq_dashboard_handler";
      Source_census.absent_everywhere "hq renderer" "hq_dashboard_page";
      Source_census.absent_everywhere "kpi query" "get_kpi_dashboard";
      Source_census.absent_everywhere "dau/mau query" "get_dau_mau_ratio")

(* --- 2. no production caller of any deleted wrapper -------------------- *)
let deleted_wrappers =
  [
    "Components.layout";
    "Components.auth_page";
    "Components.create_page";
    "Components.admin_page";
    "Components.account_page";
    "Components.community_manage_page";
    "Components.community_home_page";
    "Components.search_page";
    "Components.global_shell";
    "Components.community_shell";
    "Components.feed_shell";
    "Components.render_app_topbar";
    "Components.left_sidebar";
    "Components.community_card";
    "Pages.index";
    "Handlers.home_handler";
  ]

let dead_wrapper_case =
  lc_case "no production caller of any deleted wrapper" (fun () ->
      List.iter
        (fun n -> Source_census.absent_everywhere "deleted wrapper" n)
        deleted_wrappers;
      (* and no definition survives either *)
      List.iter
        (fun n -> Source_census.absent_everywhere "deleted definition" n)
        [
          "let layout ";
          "let auth_page ";
          "let create_page ";
          "let admin_page ";
          "let account_page ";
          "let community_manage_page ";
          "let community_home_page ";
          "let search_page ";
          "let global_shell ";
          "let community_shell ";
          "let feed_shell ";
          "let render_app_topbar ";
          "let left_sidebar ";
          "let community_card ";
          "let home_handler ";
          "type rail_active";
          "type nav_item";
          "type nav_group";
        ])

(* --- 3./4./5. deleted stylesheets, Tailwind CDN, Google Fonts ---------- *)
let deleted_stylesheets =
  [
    "shell.css";
    "create.css";
    "admin.css";
    "auth.css";
    "account.css";
    "community-manage.css";
    "community-home.css";
    "search.css";
  ]

let deleted_css_case =
  lc_case "no production reference to a deleted stylesheet" (fun () ->
      List.iter
        (fun f ->
          Source_census.absent_everywhere "deleted stylesheet" f;
          Source_census.absent_everywhere "deleted stylesheet path"
            ("/static/css/" ^ f))
        deleted_stylesheets)

let approved_stylesheets = [ "earde.css"; "mobile-gate.css" ]

let surviving_css_case =
  lc_case "only the two approved stylesheets are served" (fun () ->
      (* Every <link rel='stylesheet'> the production sources emit points at
         an approved local path. *)
      let sources =
        List.concat_map
          (fun (_, body) ->
            let marker = "href='/static/css/" in
            let ml = String.length marker in
            let rec loop i acc =
              match
                Html_assert.index_of
                  (String.sub body i (String.length body - i))
                  marker
              with
              | None -> acc
              | Some j ->
                  let start = i + j + ml in
                  let stop =
                    let rec f k = if body.[k] = '\'' then k else f (k + 1) in
                    f start
                  in
                  loop stop (String.sub body start (stop - start) :: acc)
            in
            loop 0 [])
          Source_census.production_sources
      in
      Alcotest.(check bool)
        "at least one stylesheet is emitted" true (sources <> []);
      List.iter
        (fun f ->
          if not (List.mem f approved_stylesheets) then
            Alcotest.failf "unapproved stylesheet reference: %S" f)
        sources;
      (* Both survivors are actually still used. *)
      List.iter
        (fun f ->
          Alcotest.(check bool)
            (Printf.sprintf "%s is still linked" f)
            true (List.mem f sources))
        approved_stylesheets)

let external_assets_case =
  lc_case "no Tailwind CDN and no Google Fonts in production" (fun () ->
      List.iter
        (fun n -> Source_census.absent_everywhere "external asset" n)
        [
          "cdn.tailwindcss.com";
          "tailwindcss";
          "fonts.googleapis.com";
          "fonts.gstatic.com";
        ];
      (* No shipped stylesheet, entrypoint or partial, pulls them in either. *)
      List.iter
        (fun path ->
          let css = Source_census.read path in
          List.iter
            (fun n ->
              if Html_assert.contains css n then
                Alcotest.failf "%s imports %S" path n)
            [
              "fonts.googleapis.com";
              "fonts.gstatic.com";
              "cdn.tailwindcss.com";
              "@import url('http";
              "@import url(\"http";
              "@import url(\"//";
            ])
        ("static/css/mobile-gate.css" :: Css_census.entrypoint
       :: Css_census.partials))

(* earde.css is the one linked stylesheet; its partials load through
   @import. Every CSS file on disk is either linked or imported exactly
   once, so a partial cannot be orphaned or loaded twice. *)
let partials_case =
  lc_case "earde.css imports every stylesheet partial exactly once" (fun () ->
      let rec css_files dir =
        Sys.readdir (Filename.concat Source_census.root dir)
        |> Array.to_list |> List.sort compare
        |> List.concat_map (fun f ->
            let path = dir ^ "/" ^ f in
            if Sys.is_directory (Filename.concat Source_census.root path) then
              css_files path
            else if Filename.check_suffix f ".css" then [ path ]
            else [])
      in
      let on_disk =
        List.filter
          (fun p ->
            not
              (List.mem p
                 [ Css_census.entrypoint; "static/css/mobile-gate.css" ]))
          (css_files "static/css")
      in
      Alcotest.(check bool)
        "the stylesheet is split into partials" true
        (List.length Css_census.partials > 10);
      Alcotest.(check (list string))
        "imported partials = partials on disk" on_disk
        (List.sort compare Css_census.partials);
      Alcotest.(check int)
        "no partial imported twice"
        (List.length Css_census.partials)
        (List.length (List.sort_uniq compare Css_census.partials)))

(* --- 6./13. rendered documents: approved local CSS only ---------------- *)
let census_community ~visibility : Earde.Community_types.community =
  {
    id = 3;
    slug = "census";
    name = "Census";
    description = None;
    rules = None;
    avatar_url = None;
    banner_url = None;
    allow_downvotes = true;
    sections_enabled = true;
    visibility;
    indexable = true;
    is_network_community = false;
    onboarding_state = Earde.Community_types.Community_published;
    discoverable = true;
  }

(* One document per live wrapper family. *)
let launch_documents () =
  let community =
    census_community ~visibility:Earde.Community_types.Community_public
  in
  [
    ( "launch_entry_page",
      Earde.Page_shell.launch_entry_page ~page_class:"launch-bring" ~title:"T"
        ~content:(Earde.Html.static "B") () );
    ( "launch_auth_page",
      Earde.Page_shell.launch_auth_page ~page_class:"launch-login" ~title:"T"
        ~content:(Earde.Html.static "B") () );
    ( "launch_message_page",
      Earde.Page_shell.launch_message_page ~title:"T"
        ~content:(Earde.Html.static "B") () );
    ( "launch_app_page (anonymous)",
      Earde.Page_shell.launch_app_page ~page_class:"launch-feed" ~title:"T"
        ~content:(Earde.Html.static "B") () );
    ( "launch_app_page (member)",
      Earde.Page_shell.launch_app_page ~user:"alice" ~page_class:"launch-feed"
        ~title:"T" ~content:(Earde.Html.static "B") () );
    ( "launch_onboarding_page",
      Earde.Page_shell.launch_onboarding_page ~page_class:"launch-project-new"
        ~title:"T" ~content:(Earde.Html.static "B") () );
    ( "launch_community_page",
      Earde.Community_shell.launch_community_page ~community
        ~sidebar:(Earde.Html.static "S") ~page_class:"launch-community-overview"
        ~title:"T" ~content:(Earde.Html.static "B") () );
    ( "launch_community_surface_page",
      Earde.Community_shell.launch_community_surface_page ~community
        ~sidebar:(Earde.Html.static "S") ~page_class:"launch-community-channel"
        ~title:"T"
        ~main_el:(Earde.Html.static "<main class='cs-main'>B</main>")
        () );
  ]

let rendered_css_case =
  lc_case "every launch document loads approved local CSS only" (fun () ->
      List.iter
        (fun (label, html) ->
          (* no off-origin asset of any kind *)
          List.iter
            (fun needle ->
              if Html_assert.contains html needle then
                Alcotest.failf "%s references %S" label needle)
            [
              "cdn.tailwindcss.com";
              "fonts.googleapis.com";
              "fonts.gstatic.com";
              "href='http";
              "href=\"http";
              "src='http";
              "src=\"http";
              "@import";
            ];
          List.iter
            (fun sheet ->
              if Html_assert.contains html sheet then
                Alcotest.failf "%s still loads %s" label sheet)
            deleted_stylesheets;
          Alcotest.(check bool)
            (label ^ " loads earde.css")
            true
            (Html_assert.contains html
               "<link rel='stylesheet' href='/static/css/earde.css'>"))
        (launch_documents ()))

(* --- 7./8./9./10. routes that had to survive --------------------------- *)
let surviving_routes_case =
  lc_case "surviving route registrations are intact" (fun () ->
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            (Printf.sprintf "route table registers %s" needle)
            true
            (Html_assert.contains main_ml_tokens needle))
        (* 7. the root and legacy /all redirects, 8. the feed, 9. canonical
           /p/:id, 10. /admin with its global ban/unban actions, and the routes
           explicitly retained by the product decision. *)
        [
          "Dream.get \"/\" (fun request -> Dream.redirect request \"/feed\")";
          "Dream.get \"/all\" (fun request -> Dream.redirect request \"/feed\")";
          "Dream.get \"/feed\" Public_handlers.feed_handler";
          "Dream.get \"/p/:id\" Post_handlers.view_post_handler";
          "Dream.get \"/admin\" Admin_handlers.admin_dashboard_handler";
          "Dream.post \"/admin/ban/user/:id\" Admin_handlers.ban_user_handler";
          "Dream.post \"/admin/unban/user/:id\" \
           Admin_handlers.unban_user_global_handler";
          "Dream.get \"/_debug/state\" Admin_handlers.debug_state_handler";
          "Dream.get \"/new-community\" Community_handlers.new_community_page";
        ])

(* --- 14. a removed route reaches the normal unknown-route response ----- *)
let unknown_route_case =
  lc_case "no catch-all can answer a removed route" (fun () ->
      (* Dream answers an unmatched target with its own 404. The only way the
         retired path could still be served is a wildcard route, so the
         router must carry exactly one — the static file mount. *)
      Alcotest.(check int) "wildcard routes" 1 (count_in main_ml "/**");
      Alcotest.(check bool)
        "the wildcard is the static mount" true
        (Html_assert.contains main_ml
           "Dream.get \"/static/**\" (Dream.static \"static\")");
      (* Dream.any is used once, for the analytics-consent 405 arm. *)
      Alcotest.(check int) "Dream.any routes" 1 (count_in main_ml "Dream.any");
      Alcotest.(check bool)
        "and it is path-specific" true
        (Html_assert.contains main_ml "Dream.any \"/analytics/consent\""))

(* --- 10b. /admin carries no KPI-dashboard link ------------------------- *)
let admin_no_kpi_case =
  lc_case "/admin offers no KPI dashboard and no analytics vendor link"
    (fun () ->
      let html =
        Http_fixture.with_session_request ~target:"/admin" (fun req ->
            Earde.Admin_pages.admin_dashboard_page ~user:"root"
              ~signups_enabled:true ~turnstile:`Configured
              ~brevo_configured:true ~recent_users:[] ~pending:[]
              ~banned_users:[] req)
      in
      List.iter
        (fun needle ->
          if Html_assert.contains html needle then
            Alcotest.failf "/admin still offers %S" needle)
        [
          "earde-hq-dashboard";
          "hq-dashboard";
          "Mission Control";
          "posthog.com";
          "PostHog";
        ];
      (* still the real admin surface *)
      Alcotest.(check bool)
        "renders the launch admin document" true
        (Html_assert.contains html "<body class='launch-global-admin'>"))

(* --- 11. the shared message document survives -------------------------- *)
let msg_page_case =
  lc_case "shared msg_page is intact on the launch message document" (fun () ->
      (* signature retention *)
      ignore
        (Earde.Site_pages.msg_page
          : ?user:string ->
            ?auth:bool ->
            title:string ->
            message:string ->
            alert_type:string ->
            return_url:string ->
            Dream.request ->
            string);
      let render ?auth () =
        Earde.Site_pages.msg_page ?auth ~title:"T" ~message:"M"
          ~alert_type:"success" ~return_url:"/login"
          (Dream.request ~method_:`GET ~target:"/login" "")
      in
      let plain = render () in
      Alcotest.(check bool)
        "neutral launch message document" true
        (Html_assert.contains plain "<body class='launch-message-page'>");
      (* the anti-enumeration equality: both arms stay byte-identical *)
      Alcotest.(check string)
        "~auth arms are byte-identical" plain (render ~auth:true ());
      Alcotest.(check string)
        "~auth:false is the same document" plain (render ~auth:false ()))

(* --- 12. exactly one behavior-script definition ------------------------ *)
let single_behavior_script_case =
  lc_case "launch helpers keep exactly one behavior-script definition"
    (fun () ->
      let components = Source_census.launch_shells in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check int)
            (Printf.sprintf "one %s" label)
            1
            (count_in components needle))
        [
          ("behavior script", "let launch_behavior_script");
          ("share snippet", "let launch_share_snippet");
          ("share script", "let launch_share_script");
          ("mobile gate panel", "let mobile_desktop_gate");
          ("analytics assets", "let analytics_assets");
        ];
      (* The share helper is one snippet reused by both scripts, so
         copyPostLink can never fork. *)
      Alcotest.(check int)
        "one copyPostLink definition" 1
        (count_in components "function copyPostLink");
      (* No count fetch anywhere in the component library: the badge is
         server-rendered. The four document builders that carry a
         member-capable top bar (entry under Entry_viewer, app,
         onboarding, community) each call the one shared renderer, so no
         builder can hard-code a badge (or a zero) of its own. *)
      Alcotest.(check int)
        "no unread-notifs fetch call" 0
        (count_in components "unread-notifs");
      Alcotest.(check int)
        "no hard-coded badge markup" 0
        (count_in components "bell__count");
      Alcotest.(check int)
        "four shared badge render calls" 4
        (count_in components "Notification_badge.badge_html"))

let suite =
  [
    hq_route_case;
    dead_wrapper_case;
    deleted_css_case;
    surviving_css_case;
    external_assets_case;
    partials_case;
    rendered_css_case;
    surviving_routes_case;
    unknown_route_case;
    admin_no_kpi_case;
    msg_page_case;
    single_behavior_script_case;
  ]

let suites =
  (* Final legacy-asset census: the retired KPI dashboard route family,
       the deleted chrome wrappers, the deleted per-page stylesheets, the
       Tailwind CDN and Google Fonts are all absent from every production
       source and every rendered launch document; the redirects, routes,
       shared message document and single behavior-script definition that
       had to survive are all still there. Static source census plus pure
       renders — DB-free. *)
  [ ("legacy_asset_census", suite) ]
