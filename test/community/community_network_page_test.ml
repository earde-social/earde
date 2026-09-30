(* The public Network page (GET /c/:slug/network) — pure composition over the
   two fragments the community page already used. What this pins is the
   composition itself: both blocks always present and in order, each behind
   its own anchor, a quiet empty state instead of a missing block, the
   authorized shortcut and nothing else management-shaped, and no lifecycle,
   note, or actor vocabulary anywhere. The fragments' own suites judge the
   fragments; the read models' suites judge visibility. *)

let contains haystack needle = Html_assert.occurs haystack ~needle
let index_of = Community_page_fixture.index_of

let at html needle =
  match index_of html needle with
  | Some i -> i
  | None -> Alcotest.failf "the document has no %S" needle

let community = Community_page_fixture.community

let render ?(projects_section = "") ?(communities_section = "")
    ?(can_connect = false) () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Community_network_pages.community_network_page ~community
           ~sidebar:(Earde.Html.static "<aside class='sidebar'></aside>")
             (* The fixtures stand for fragments the real section renderers
              produced, so they enter as already-rendered markup. *)
           ~projects_section:(Earde.Html.trusted projects_section)
           ~communities_section:(Earde.Html.trusted communities_section)
           ~can_connect req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/c/cmia/network" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "network renderer did not run"

let projects =
  "<section class='ccp-section'><h2 class='ccp-title'>Connected \
   projects</h2><ul class='ccp-projects'><li>Atlas</li></ul></section>"

let communities =
  "<section class='ccc-section'><h2 class='ccc-title'>Connected \
   communities</h2><ul \
   class='ccc-communities'><li>Neighbour</li></ul></section>"

let heading_case =
  Alcotest.test_case "the page is titled Network and says what it holds" `Quick
    (fun () ->
      let html = render ~projects_section:projects () in
      if not (contains html "<h1 class='chead__title'>Network</h1>") then
        Alcotest.fail "no Network title";
      if
        not
          (contains html "Projects and communities connected to this community.")
      then Alcotest.fail "no supporting copy";
      if not (contains html "/c/cmia") then Alcotest.fail "no community context")

let both_lists_case =
  Alcotest.test_case
    "both complete lists render, projects first, each behind its own anchor"
    `Quick (fun () ->
      let html =
        render ~projects_section:projects ~communities_section:communities ()
      in
      (* Spliced verbatim: the fragments are pinned by their own suites. *)
      if not (contains html projects) then
        Alcotest.fail "the projects fragment was altered";
      if not (contains html communities) then
        Alcotest.fail "the communities fragment was altered";
      let p = at html "<div class='cnet-block' id='projects'>" in
      let c = at html "<div class='cnet-block' id='communities'>" in
      Alcotest.(check bool) "projects lead" true (p < c);
      Alcotest.(check bool)
        "each block holds its own fragment" true
        (p < at html "ccp-section" && at html "ccp-section" < c))

let empty_states_case =
  Alcotest.test_case "an empty side keeps its block and says so quietly" `Quick
    (fun () ->
      let html = render ~communities_section:communities () in
      (* The projects block is framed and headed, with one quiet line. *)
      if not (contains html "<h2 class='ccp-title'>Connected projects</h2>")
      then Alcotest.fail "the empty projects block lost its heading";
      if not (contains html "No connected projects yet.") then
        Alcotest.fail "no quiet empty copy";
      if contains html "<ul class='ccp-projects'>" then
        Alcotest.fail "an empty projects list was rendered";
      (* And the populated side is untouched. *)
      if not (contains html communities) then
        Alcotest.fail "the populated block was disturbed";
      (* Both empty: the page still renders, with both blocks. *)
      let bare = render () in
      Alcotest.(check int)
        "two blocks" 2
        (Html_assert.count_sub bare "<div class='cnet-block'");
      if not (contains bare "No connected communities yet.") then
        Alcotest.fail "no quiet empty copy for communities")

let cta_case =
  Alcotest.test_case
    "the connect shortcut renders only for an authorized viewer" `Quick
    (fun () ->
      let public = render ~projects_section:projects () in
      if contains public "settings/connections" then
        Alcotest.fail "an ordinary visitor was offered the management flow";
      let authorized = render ~projects_section:projects ~can_connect:true () in
      Alcotest.(check int)
        "one shortcut" 1
        (Html_assert.count_sub authorized
           "href='/c/cmia/settings/connections/new'");
      if not (contains authorized ">Connect a community</a>") then
        Alcotest.fail "the shortcut has no label")

(* Nothing the management surface knows may appear here, for either viewer. *)
let vocabulary_case =
  Alcotest.test_case
    "no lifecycle, note, actor, or management control can appear" `Quick
    (fun () ->
      List.iter
        (fun can_connect ->
          let html =
            render ~projects_section:projects ~communities_section:communities
              ~can_connect ()
          in
          List.iter
            (fun needle ->
              if contains html needle then
                Alcotest.failf "the network page renders %S (can_connect=%b)"
                  needle can_connect)
            [
              "pending";
              "Pending";
              "rejected";
              "Rejected";
              "removed";
              "Removed";
              "Requested by";
              "Reviewed by";
              "requester";
              "reviewer";
              "request note";
              "ccn-form";
              "ccn-btn";
              "connections/request";
              "/accept";
              "/reject";
            ])
        [ false; true ])

let suite =
  [
    heading_case; both_lists_case; empty_states_case; cta_case; vocabulary_case;
  ]

let suites = [ ("community_network_page", suite) ]
