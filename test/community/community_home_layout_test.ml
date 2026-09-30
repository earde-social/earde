(* The /c/:slug information architecture. Live chat and durable forum
   structure are the two complementary ways to take part, so they sit as
   equal siblings inside one community-spaces composition; the two connection
   cards sit in one network group beneath it; governance stays in the
   secondary column.

   The grouping is built in the composition, not by CSS ordering, which is
   what this pins: which wrapper each panel lands in, in which order, and
   that a wrapper is never emitted around nothing. Renders the real page
   under a secret + sessions pipeline (it emits a framework CSRF field); no
   SQL is touched. *)

let contains haystack needle = Html_assert.occurs haystack ~needle

let at html needle =
  match Community_page_fixture.index_of html needle with
  | Some i -> i
  | None -> Alcotest.failf "the document has no %S" needle

let section ?description ~name ~slug () : Earde.Db.community_section =
  { section_id = 1; community_id = 7711; name; slug; description;
    position = 0; default_sort = "hot"; is_introduction_section = false;
    indexable = true }

let channel ~slug () : Earde.Db.channel =
  { id = 1; community_id = 7711; slug; name = slug; topic = None;
    position = 0; is_archived = false; created_at = "2026-07-31 10:00:00";
    indexable = true }

let post ~title () : Earde.Db.post =
  { id = 501; title; url = None; content = Some "Body."; community_id = 7711;
    user_id = 9; username = "mapmaker"; community_slug = "cmia";
    created_at = "2026-07-31 10:00:00"; score = 3; comment_count = 1;
    allow_downvotes = true; image_url = None; section_name = None;
    section_slug = None; community_sections_enabled = true;
    author_local_karma = 0; author_local_post_count = 1;
    author_local_comment_count = 0; author_first_active_at = None }

let render ?(projects = 0) ?(communities = 0) ?(top_mod = false)
    ?(channels = []) ?(sections = []) ?(recent_posts = []) () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.community_overview_page
           ~connected_projects_count:projects
           ~connected_communities_count:communities ~is_member:false
           ~is_current_user_mod:false ~is_current_user_top_mod:top_mod
           ~mod_usernames:[ "mapmaker" ] ~orphaned:(0, None)
           ~rail_communities:[] ~channels ~recent_posts Community_page_fixture.community
           (List.map (fun s -> (s, 0, None)) sections)
           req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/c/cmia" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "overview renderer did not run"

let one_channel = [ channel ~slug:"general" () ]

let one_section =
  [ section ~name:"Field notes" ~slug:"field-notes"
      ~description:"Observations from the ground." () ]

(* The panel label. "Knowledge sections" is the pre-reorganisation copy and
   must be gone from this page — the kicker uppercases it, so the document
   carries the sentence case and renders FORUM SECTIONS. *)
let label_case =
  Alcotest.test_case "the sections panel is labelled Forum sections" `Quick
    (fun () ->
      let html = render ~channels:one_channel ~sections:one_section () in
      Alcotest.(check int)
        "exactly one Forum sections kicker" 1
        (Html_assert.count_sub html "<span class='kicker'>Forum sections</span>");
      if contains html "Knowledge sections" then
        Alcotest.fail "the overview still says Knowledge sections")

(* Community spaces leads the main column, channels before sections. *)
let spaces_case =
  Alcotest.test_case
    "live channels and forum sections are siblings in one community-spaces \
     group" `Quick (fun () ->
      let html = render ~channels:one_channel ~sections:one_section () in
      Alcotest.(check int) "one spaces group" 1
        (Html_assert.count_sub html "<div class='launch-spaces'>");
      let spaces = at html "<div class='launch-spaces'>" in
      let channels = at html "<span class='kicker'>Live channels</span>" in
      let sections = at html "<span class='kicker'>Forum sections</span>" in
      Alcotest.(check bool) "channels lead the group" true
        (spaces < channels && channels < sections);
      (* And the group leads the main column. *)
      Alcotest.(check bool) "spaces open the main stack" true
        (at html "<div class='stack'>" < spaces))

(* One surface takes the column on its own; no wrapper is emitted around a
   single panel, and none around nothing. *)
let lone_surface_case =
  Alcotest.test_case "a lone space surface needs no group wrapper" `Quick
    (fun () ->
      (* Only forum sections: the channels panel is absent entirely. *)
      let html = render ~sections:one_section () in
      Alcotest.(check int) "no wrapper around one panel" 0
        (Html_assert.count_sub html "<div class='launch-spaces'>");
      Alcotest.(check int) "sections still render" 1
        (Html_assert.count_sub html "<span class='kicker'>Forum sections</span>");
      if contains html "<span class='kicker'>Live channels</span>" then
        Alcotest.fail "an empty channels panel was rendered";
      (* Only channels: the sections panel keeps its existing empty state,
         so both surfaces are present and the group is emitted. *)
      let html = render ~channels:one_channel () in
      Alcotest.(check int) "both surfaces present" 1
        (Html_assert.count_sub html "<div class='launch-spaces'>");
      if not (contains html "No sections yet.") then
        Alcotest.fail "the sections empty state was lost")

(* Recent knowledge follows community spaces and closes the main column. *)
let recent_case =
  Alcotest.test_case "recent knowledge follows community spaces in the main \
                      column" `Quick (fun () ->
      let html =
        render ~channels:one_channel ~sections:one_section
          ~recent_posts:
            [ { Earde.Db.fi_post = post ~title:"Contour intervals" ();
                fi_shared = None } ] ()
      in
      let recent = at html "<span class='kicker'>Recent durable knowledge</span>" in
      Alcotest.(check bool) "spaces first" true
        (at html "<div class='launch-spaces'>" < recent);
      Alcotest.(check bool) "still in the main column" true
        (recent < at html "<div class='stack stack--sm'>"))

(* The full connection lists left the home for /c/:slug/network. Nothing of
   either fragment may render here — not the sections, not the headings, not
   the group wrapper that used to hold them. *)
let no_lists_case =
  Alcotest.test_case "no connection list renders on the home" `Quick
    (fun () ->
      let html =
        render ~channels:one_channel ~sections:one_section ~projects:3
          ~communities:2 ()
      in
      List.iter
        (fun needle ->
          if contains html needle then
            Alcotest.failf "the home still renders %S" needle)
        [ "ccp-section"; "ccc-section"; "<h2 class='ccp-title'>"
        ; "<h2 class='ccc-title'>"; "launch-network"
        ; "Open-source projects that use this community as their Earde home."
        ; "Communities this one is mutually connected with." ])

(* The compact entry point: two stable rows with their counts, both leading
   to the public Network page, at the foot of the secondary column. *)
let network_panel_case =
  Alcotest.test_case "the network panel closes the secondary column with two \
                      counted links" `Quick (fun () ->
      let html =
        render ~channels:one_channel ~sections:one_section ~projects:1
          ~communities:3 ()
      in
      Alcotest.(check int) "one network panel" 1
        (Html_assert.count_sub html "<span class='kicker'>Network</span>");
      let network = at html "<span class='kicker'>Network</span>" in
      Alcotest.(check bool) "in the secondary column" true
        (at html "<div class='stack stack--sm'>" < network);
      Alcotest.(check bool) "after the moderation log" true
        (at html "<span class='kicker'>Moderation log</span>" < network);
      List.iter
        (fun (label, count, anchor) ->
          let row =
            Printf.sprintf
              "<a class='project-row launch-netrow' \
               href='/c/cmia/network#%s'><span \
               class='launch-net-label'>%s</span><span \
               class='launch-net-count'>%d</span><span \
               class='launch-net-go' aria-hidden='true'>&rarr;</span></a>"
              anchor label count
          in
          if not (contains html row) then
            Alcotest.failf "the network panel has no %s row" label)
        [ ("Connected projects", 1, "projects")
        ; ("Connected communities", 3, "communities") ];
      (* Projects lead, communities follow. *)
      Alcotest.(check bool) "projects row first" true
        (at html "network#projects" < at html "network#communities"))

(* A zero count is a rendered row, not a hidden one: both are permanent
   destinations, so the panel's shape never depends on the data. *)
let zero_counts_case =
  Alcotest.test_case "zero counts still render both navigation rows" `Quick
    (fun () ->
      let html = render ~channels:one_channel ~sections:one_section () in
      Alcotest.(check int) "network panel present" 1
        (Html_assert.count_sub html "<span class='kicker'>Network</span>");
      Alcotest.(check int) "two rows" 2
        (Html_assert.count_sub html "<a class='project-row launch-netrow'");
      Alcotest.(check int) "both at zero" 2
        (Html_assert.count_sub html "<span class='launch-net-count'>0</span>"))

(* The connect shortcut is the sidebar's existing top-mod-or-admin gate and
   points at the existing flow — no management control travels with it. *)
let connect_cta_case =
  Alcotest.test_case "only a top mod is offered the connect shortcut" `Quick
    (fun () ->
      let ordinary = render ~channels:one_channel ~sections:one_section () in
      if contains ordinary "settings/connections/new" then
        Alcotest.fail "an ordinary viewer was offered the connect flow";
      let top =
        render ~channels:one_channel ~sections:one_section ~top_mod:true ()
      in
      Alcotest.(check int) "one shortcut" 1
        (Html_assert.count_sub top "href='/c/cmia/settings/connections/new'");
      if not (contains top ">Connect a community &rarr;</a>") then
        Alcotest.fail "the connect shortcut has no label";
      (* And nothing else from the management surface came with it. *)
      List.iter
        (fun needle ->
          if contains top needle then
            Alcotest.failf "the home network panel renders %S" needle)
        [ "Incoming requests"; "Outgoing requests"; "ccn-form"
        ; "connections/request"; "/accept"; "/reject" ])

(* Governance stays in the secondary column, subordinate to participation. *)
let governance_case =
  Alcotest.test_case
    "moderators and the moderation log stay in the secondary column" `Quick
    (fun () ->
      let html = render ~channels:one_channel ~sections:one_section () in
      let secondary = at html "<div class='stack stack--sm'>" in
      Alcotest.(check bool) "spaces are in the main column" true
        (at html "<div class='launch-spaces'>" < secondary);
      Alcotest.(check bool) "moderators are secondary" true
        (secondary < at html "<span class='kicker'>Moderators</span>");
      Alcotest.(check bool) "moderation log is secondary" true
        (secondary < at html "<span class='kicker'>Moderation log</span>");
      (* Live channels left the secondary column for good. *)
      Alcotest.(check bool) "channels are no longer secondary" true
        (at html "<span class='kicker'>Live channels</span>" < secondary))

let suite =
  [ label_case; spaces_case; lone_surface_case; recent_case; no_lists_case
  ; network_panel_case; zero_counts_case; connect_cta_case
  ; governance_case ]

let suites =
  [ ("community_home_ia", suite)
  ]
