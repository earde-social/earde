(* Every community-shell launch surface is <body class='launch-X'> over the
   SAME chrome: the dark rail, the community sidebar, the top bar's user
   menu, and the desktop-only gate. None of that has a global rule —
   each rule reaches only the page classes it lists (routes/chrome.css) — so a
   scope that ships only its feature fragment renders all of the shared chrome
   unstyled.

   That is precisely what shipped for launch-community-connections: the
   management page had no main-column width or gutter (so it began against
   the sidebar and ran to the viewport edge), no head band (so the context
   band's two spans collapsed into one malformed "slug/c/slug" token), no
   user menu and no mobile gate.

   The page classes are read out of the production sources rather than
   listed, so a new community-shell surface cannot escape the check. Source
   and stylesheet census only — no database, no server. *)

let contains haystack needle = Html_assert.occurs haystack ~needle
let css = Css_census.stylesheet

(* Page classes stamped on a document built by the community-shell doc
   builders. The literal always sits within a few lines of the call, so the
   window below is generous rather than exact. *)
let community_shell_classes =
  let builders =
    [
      "Community_shell.launch_community_page";
      "Community_shell.launch_community_surface_page";
    ]
  in
  let class_after source start =
    let marker = "~page_class:\"" in
    (* Search a bounded window, so an unrelated later call cannot be
       mistaken for this one's argument. *)
    let window_end = min (String.length source) (start + 400) in
    let rec scan i =
      if i + String.length marker > window_end then None
      else if String.sub source i (String.length marker) = marker then
        let v = i + String.length marker in
        match String.index_from_opt source v '"' with
        | Some j -> Some (String.sub source v (j - v))
        | None -> None
      else scan (i + 1)
    in
    scan start
  in
  let found = ref [] in
  List.iter
    (fun (_, source) ->
      List.iter
        (fun builder ->
          let bl = String.length builder in
          let rec scan i =
            if i + bl > String.length source then ()
            else if String.sub source i bl = builder then (
              (match class_after source i with
              | Some c when not (List.mem c !found) -> found := c :: !found
              | _ -> ());
              scan (i + bl))
            else scan (i + 1)
          in
          scan 0)
        builders)
    Source_census.production_sources;
  List.sort compare !found

let census_case =
  Alcotest.test_case
    "the community-shell page classes are discovered from the sources" `Quick
    (fun () ->
      (* A sanity floor: if the scan silently stopped matching, the rule
         case below would pass vacuously. *)
      Alcotest.(check bool)
        "several community-shell surfaces found" true
        (List.length community_shell_classes >= 10);
      List.iter
        (fun expected ->
          Alcotest.(check bool)
            (expected ^ " is a community-shell surface")
            true
            (List.mem expected community_shell_classes))
        [
          "launch-community-connections";
          "launch-community-network";
          "launch-community-overview";
          "launch-community-settings";
          "launch-project-home-review";
        ])

(* The shared half of a community-shell scope. Each of these is chrome the
   document always emits and that only a rule listing this scope can style;
   a leading :is(...) counts once per page class it lists. *)
let required page_class =
  [
    ("top-bar user menu", Printf.sprintf ".%s .launch-user__menu" page_class);
    ("rail avatar", Printf.sprintf ".%s .launch-rail__img" page_class);
    ("sidebar avatar", Printf.sprintf ".%s .launch-avatar-img" page_class);
    ("sidebar identity", Printf.sprintf ".%s .launch-side-id" page_class);
    ("desktop-only gate", Printf.sprintf "body.%s > .app" page_class);
  ]

let shared_rules_case =
  Alcotest.test_case
    "every community-shell page class scopes the whole shared chrome" `Quick
    (fun () ->
      List.iter
        (fun page_class ->
          List.iter
            (fun (label, needle) ->
              if not (Css_census.has_selector_prefix needle) then
                Alcotest.failf "no %s rule reaches %s (expected %S)" label
                  page_class needle)
            (required page_class))
        community_shell_classes)

(* The badge is server-rendered and absent at zero, so the per-scope
   reveal rule it used to need must be gone everywhere — a leftover copy
   would be dead CSS pointing at a class the markup no longer carries. *)
let no_badge_reveal_case =
  Alcotest.test_case "no scope still carries a badge reveal rule" `Quick
    (fun () ->
      List.iter
        (fun needle ->
          if contains css needle then
            Alcotest.failf "the stylesheet still contains %S" needle)
        [ "#notif-badge"; "bell__count hidden" ])

(* The public connected-communities block is a list, not a tile grid. It
   borrowed the connected-projects panel's two-up track list, where each
   cell is a multi-line project; a connected community is one line, so an
   odd count left an orphan cell and the block read as tiles. Every scope
   that renders the block must keep one community per full-width row, with
   no count-dependent rule propping it up — including the Network page the
   complete lists moved to. *)
let connected_communities_list_case =
  Alcotest.test_case
    "the public connected-communities block is a single-column list in every \
     scope that renders it"
    `Quick (fun () ->
      let scopes =
        [
          "launch-community-overview";
          "launch-flat-community";
          "launch-community-network";
        ]
      in
      let flex_column =
        "list-style: none; margin: 0; padding: 0; display: flex; \
         flex-direction: column; gap: var(--bw);"
      in
      List.iter
        (fun scope ->
          let list_rule = Printf.sprintf ".%s .ccc-communities" scope in
          if not (Css_census.has_selector list_rule) then
            Alcotest.failf "no %s rule" list_rule;
          if
            contains css
              "grid-template-columns: 1fr 1fr;\n  background: var(--line)"
          then Alcotest.failf "a two-column track rule is still shipped";
          if
            Css_census.has_selector_prefix
              (Printf.sprintf ".%s .ccc-community:last-child" scope)
          then Alcotest.failf "%s still has an odd-count span rule" scope;
          (* And the rows really are a plain column, once per scope. *)
          Alcotest.(check int)
            (scope ^ ": one flex column")
            1
            (List.length
               (List.filter
                  (fun (r : Css_census.rule) ->
                    List.mem list_rule r.selectors
                    && Html_assert.contains r.body flex_column)
                  Css_census.rules)))
        scopes)

let suite =
  [
    census_case;
    shared_rules_case;
    no_badge_reveal_case;
    connected_communities_list_case;
  ]

let suites = [ ("launch_scope_css", suite) ]
