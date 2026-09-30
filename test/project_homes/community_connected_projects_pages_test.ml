(* === Community connected-projects section (Community_connected_projects_pages) ===
   The pure fragment composed into the existing community page: section copy,
   verification and kind vocabulary, link safety, defensive degradation, and
   the absence of officiality language, identifiers, and private workflow
   data. DB-free — the renderer depends on no connection, no read model, and
   no session. Privacy assertions are boolean, so no fixture byte reaches test
   output on failure. *)

module Ccp = Earde.Community_connected_projects_pages
module Pi = Earde.Project_identity

let case = Case.quick

let repo ?(primary = false) ?(archived = false) ?url full_name =
  ({
     full_name;
     html_url =
       (match url with
       | Some u -> u
       | None -> "https://github.com/" ^ full_name);
     is_primary = primary;
     is_archived = archived;
   }
    : Ccp.repository)

let project ?(kind = Pi.Project) ?(name = "Ccpp Project")
    ?(slug = "ccpp-project") ?(login = "ccpp-owner")
    ?(verification = Ccp.Verified) ?website ?repositories () =
  ({
     name;
     slug;
     kind;
     namespace_login = login;
     verification;
     website_url = website;
     repositories =
       (match repositories with
       | Some r -> r
       | None -> [ repo ~primary:true "ccpp-owner/alpha" ]);
   }
    : Ccp.project)

let render projects = Ccp.connected_projects_section ~projects

let one ?kind ?name ?slug ?login ?verification ?website ?repositories () =
  render
    [ project ?kind ?name ?slug ?login ?verification ?website ?repositories () ]

(* Wording that would misrepresent an accepted home as an endorsement. *)
let forbidden_officiality =
  [
    "Official project";
    "Official community";
    "Official home";
    "GitHub-approved";
    "GitHub-endorsed";
    "Official";
  ]

let check_no_officiality label html =
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no " ^ needle)
        false
        (Html_assert.contains html needle))
    forbidden_officiality

(* Inertness is asserted structurally, not by substring: hostile input is
   deliberately rendered as escaped TEXT, so a fixture that contains the
   bytes "javascript:" or "onerror=" may legitimately appear in the output.
   What must never happen is that any of it becomes markup. Every '<' in the
   output therefore has to open one of the renderer's own tags — which
   leaves no way to introduce a script, an iframe, an inline style, or an
   event handler — and no attribute may carry a script-bearing URL. *)
let allowed_tags =
  [
    "section";
    "/section";
    "div";
    "/div";
    "h2";
    "/h2";
    "h3";
    "/h3";
    "p";
    "/p";
    "ul";
    "/ul";
    "li";
    "/li";
    "a";
    "/a";
    "span";
    "/span";
  ]

let check_only_safe_tags label html =
  let n = String.length html in
  let rec go i =
    if i >= n then ()
    else if html.[i] <> '<' then go (i + 1)
    else begin
      let rest = String.sub html (i + 1) (n - i - 1) in
      let ok =
        List.exists
          (fun tag ->
            let t = String.length tag in
            String.length rest >= t
            && String.sub rest 0 t = tag
            && (String.length rest = t
               ||
               let c = rest.[t] in
               c = ' ' || c = '>'))
          allowed_tags
      in
      Alcotest.(check bool)
        (label ^ ": every '<' opens a renderer-authored tag")
        true ok;
      go (i + 1)
    end
  in
  go 0

let check_no_script_urls label html =
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no " ^ needle)
        false
        (Html_assert.contains html needle))
    [
      "href='javascript:";
      "href=\"javascript:";
      "href='data:";
      "href='vbscript:";
      "href='#'";
      "src='javascript:";
      "style=";
    ]

let check_inert label html =
  check_only_safe_tags label html;
  check_no_script_urls label html

let empty_cases =
  [
    case "connected projects: an empty list renders the empty fragment"
      (fun () ->
        Alcotest.(check string)
          "empty fragment" ""
          (Earde.Html.to_string (render []));
        (* No placeholder is shown to ordinary visitors. *)
        Alcotest.(check bool)
          "no empty-state copy" false
          (Html_assert.contains
             (Earde.Html.to_string (render []))
             "No connected projects"));
  ]

let content_cases =
  [
    case
      "connected projects: section copy, identity, kind, namespace and \
       verification all render" (fun () ->
        let html =
          one ~name:"Ccpp Alpha" ~slug:"ccpp-alpha" ~login:"ccpp-owner" ()
        in
        Alcotest.(check bool)
          "heading" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "Connected projects");
        Alcotest.(check bool)
          "supporting copy" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "Open-source projects that use this community as their Earde home.");
        Alcotest.(check bool)
          "project name" true
          (Html_assert.contains (Earde.Html.to_string html) "Ccpp Alpha");
        Alcotest.(check bool)
          "kind copy" true
          (Html_assert.contains (Earde.Html.to_string html) ">Project<");
        Alcotest.(check bool)
          "namespace login" true
          (Html_assert.contains (Earde.Html.to_string html) "ccpp-owner");
        Alcotest.(check bool)
          "verification copy" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "Verified through GitHub");
        Alcotest.(check bool)
          "safe provenance copy" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "Project connected through GitHub");
        check_no_officiality "content" (Earde.Html.to_string html);
        check_inert "content" (Earde.Html.to_string html));
    case
      "connected projects: stale and revoked carry their exact copy and stay \
       visible" (fun () ->
        let stale = one ~verification:Ccp.Stale () in
        Alcotest.(check bool)
          "stale copy" true
          (Html_assert.contains
             (Earde.Html.to_string stale)
             "Verification stale");
        Alcotest.(check bool)
          "stale is not verified copy" false
          (Html_assert.contains
             (Earde.Html.to_string stale)
             "Verified through GitHub");
        let revoked = one ~verification:Ccp.Revoked () in
        Alcotest.(check bool)
          "revoked copy" true
          (Html_assert.contains
             (Earde.Html.to_string revoked)
             "Verification revoked");
        Alcotest.(check bool)
          "revoked still rendered" true
          (Html_assert.contains (Earde.Html.to_string revoked) "Ccpp Project");
        check_no_officiality "stale" (Earde.Html.to_string stale);
        check_no_officiality "revoked" (Earde.Html.to_string revoked));
    case
      "connected projects: every project kind uses the current product \
       vocabulary" (fun () ->
        List.iter
          (fun (kind, copy) ->
            let html = one ~kind () in
            Alcotest.(check bool)
              ("kind copy " ^ copy) true
              (Html_assert.contains
                 (Earde.Html.to_string html)
                 (">" ^ copy ^ "<")))
          [
            (Pi.Project, "Project");
            (Pi.Organization, "Organization");
            (Pi.Ecosystem, "Ecosystem");
            (Pi.Foundation, "Foundation");
            (Pi.Working_group, "Working group");
            (Pi.Other, "Other");
          ]);
    case
      "connected projects: repositories keep supplied order and carry Primary \
       and Archived markers" (fun () ->
        let html =
          one
            ~repositories:
              [
                repo ~primary:true "ccpp-owner/alpha";
                repo ~archived:true "ccpp-owner/beta";
                repo "ccpp-owner/gamma";
              ]
            ()
        in
        let index needle =
          let rec go i =
            if
              i + String.length needle
              > String.length (Earde.Html.to_string html)
            then -1
            else if
              String.sub (Earde.Html.to_string html) i (String.length needle)
              = needle
            then i
            else go (i + 1)
          in
          go 0
        in
        Alcotest.(check bool)
          "alpha before beta" true
          (index "ccpp-owner/alpha" < index "ccpp-owner/beta");
        Alcotest.(check bool)
          "beta before gamma" true
          (index "ccpp-owner/beta" < index "ccpp-owner/gamma");
        Alcotest.(check bool)
          "primary marker" true
          (Html_assert.contains (Earde.Html.to_string html) ">Primary<");
        Alcotest.(check bool)
          "archived marker" true
          (Html_assert.contains (Earde.Html.to_string html) ">Archived<");
        Alcotest.(check int)
          "exactly one primary marker" 1
          (Html_assert.occurrences (Earde.Html.to_string html) ">Primary<");
        Alcotest.(check int)
          "exactly one archived marker" 1
          (Html_assert.occurrences (Earde.Html.to_string html) ">Archived<"));
    case "connected projects: multiple projects preserve the supplied order"
      (fun () ->
        let html =
          render
            [
              project ~name:"Ccpp One" ~slug:"ccpp-one" ();
              project ~name:"Ccpp Two" ~slug:"ccpp-two" ();
            ]
        in
        Alcotest.(check int)
          "two project entries" 2
          (Html_assert.occurrences
             (Earde.Html.to_string html)
             "<li class='ccp-project'>");
        let idx needle =
          let rec go i =
            if
              i + String.length needle
              > String.length (Earde.Html.to_string html)
            then -1
            else if
              String.sub (Earde.Html.to_string html) i (String.length needle)
              = needle
            then i
            else go (i + 1)
          in
          go 0
        in
        Alcotest.(check bool)
          "supplied order kept" true
          (idx "Ccpp One" < idx "Ccpp Two"));
  ]

let link_cases =
  [
    case
      "connected projects: a safe website is linked; an unsafe one degrades to \
       inert escaped text" (fun () ->
        let safe = one ~website:"https://ccpp.example/home" () in
        Alcotest.(check bool)
          "website linked" true
          (Html_assert.contains
             (Earde.Html.to_string safe)
             "href='https://ccpp.example/home'");
        (* Each unsafe scheme renders its text but never an href; the
           repository list is dropped so the only possible href would be the
           website's. *)
        List.iter
          (fun bad ->
            let html = one ~website:bad ~repositories:[] () in
            Alcotest.(check bool)
              ("no href for " ^ bad) false
              (Html_assert.contains (Earde.Html.to_string html) "href=");
            check_inert ("unsafe website " ^ bad) (Earde.Html.to_string html))
          [
            "javascript:alert(1)";
            "data:text/html,x";
            "/relative/path";
            "//evil.example/x";
            "ftp://ccpp.example/x";
            "";
          ]);
    case
      "connected projects: a repository links only at its canonical HTTPS \
       GitHub URL" (fun () ->
        let good = one ~repositories:[ repo "ccpp-owner/alpha" ] () in
        Alcotest.(check bool)
          "canonical repository linked" true
          (Html_assert.contains
             (Earde.Html.to_string good)
             "href='https://github.com/ccpp-owner/alpha'");
        List.iter
          (fun bad ->
            let html =
              one ~repositories:[ repo ~url:bad "ccpp-owner/alpha" ] ()
            in
            Alcotest.(check bool)
              ("no href for " ^ bad) false
              (Html_assert.contains (Earde.Html.to_string html) "href=");
            (* The label still renders, inert and escaped. *)
            Alcotest.(check bool)
              "full name still shown" true
              (Html_assert.contains
                 (Earde.Html.to_string html)
                 "ccpp-owner/alpha");
            check_inert
              ("bad repository url " ^ bad)
              (Earde.Html.to_string html))
          [
            "https://evil.example/ccpp-owner/alpha";
            "https://github.com.evil.example/ccpp-owner/alpha";
            "http://github.com/ccpp-owner/alpha";
            "https://github.com/ccpp-owner/other";
            "javascript:alert(1)";
            "";
          ]);
    case
      "connected projects: the project name is never a link and the owner-only \
       setup route is never advertised" (fun () ->
        let html = one ~slug:"ccpp-alpha" () in
        Alcotest.(check bool)
          "no setup link" false
          (Html_assert.contains
             (Earde.Html.to_string html)
             "/projects/ccpp-alpha/setup");
        Alcotest.(check bool)
          "no projects route at all" false
          (Html_assert.contains (Earde.Html.to_string html) "/projects/");
        Alcotest.(check bool)
          "no request-home link" false
          (Html_assert.contains (Earde.Html.to_string html) "request-home");
        (* The name renders as a heading, never wrapped in an anchor. *)
        Alcotest.(check bool)
          "name is a heading" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "<h3 class='ccp-name'>Ccpp Project</h3>"));
    case
      "connected projects: an empty repository list still renders the project \
       identity" (fun () ->
        let html = one ~repositories:[] () in
        Alcotest.(check bool)
          "identity present" true
          (Html_assert.contains (Earde.Html.to_string html) "Ccpp Project");
        Alcotest.(check bool)
          "no repository list" false
          (Html_assert.contains (Earde.Html.to_string html) "ccp-repos");
        Alcotest.(check bool)
          "no repository link" false
          (Html_assert.contains (Earde.Html.to_string html) "href="));
  ]

let defensive_cases =
  [
    case
      "connected projects: a blank project name degrades to a generic safe \
       label" (fun () ->
        List.iter
          (fun blank ->
            let html = one ~name:blank () in
            Alcotest.(check bool)
              "generic label" true
              (Html_assert.contains
                 (Earde.Html.to_string html)
                 "Open-source project");
            Alcotest.(check bool)
              "no empty heading" false
              (Html_assert.contains
                 (Earde.Html.to_string html)
                 "<h3 class='ccp-name'></h3>"))
          [ ""; "   "; "\t\n" ]);
    case
      "connected projects: a duplicated project slug leaves at most one \
       link-carrying group" (fun () ->
        let html =
          render
            [
              project ~name:"Ccpp One" ~slug:"ccpp-dup"
                ~website:"https://one.example/" ();
              project ~name:"Ccpp Two" ~slug:"ccpp-dup"
                ~website:"https://two.example/" ();
            ]
        in
        Alcotest.(check int)
          "both groups render" 2
          (Html_assert.occurrences
             (Earde.Html.to_string html)
             "<li class='ccp-project'>");
        Alcotest.(check bool)
          "first keeps its website link" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "href='https://one.example/'");
        Alcotest.(check bool)
          "duplicate carries no website link" false
          (Html_assert.contains
             (Earde.Html.to_string html)
             "href='https://two.example/'");
        Alcotest.(check bool)
          "duplicate still shows its website text" true
          (Html_assert.contains
             (Earde.Html.to_string html)
             "https://two.example/");
        (* Exactly one repository link survives across the duplicate pair. *)
        Alcotest.(check int)
          "one repository link" 1
          (Html_assert.occurrences
             (Earde.Html.to_string html)
             "href='https://github.com/ccpp-owner/alpha'"));
    case "connected projects: an invalid project slug carries no links"
      (fun () ->
        List.iter
          (fun bad ->
            let html = one ~slug:bad ~website:"https://ccpp.example/" () in
            Alcotest.(check bool)
              "no links at all" false
              (Html_assert.contains (Earde.Html.to_string html) "href="))
          [
            "";
            "Ccpp-Alpha";
            "ccpp alpha";
            "-ccpp";
            "ccpp-";
            "ccpp/alpha";
            "ccpp\x01";
          ]);
    case
      "connected projects: a duplicated repository full name leaves at most \
       one linked item" (fun () ->
        let html =
          one
            ~repositories:
              [ repo ~primary:true "ccpp-owner/alpha"; repo "ccpp-owner/alpha" ]
            ()
        in
        Alcotest.(check int)
          "both rows render" 2
          (Html_assert.occurrences
             (Earde.Html.to_string html)
             "<li class='ccp-repo'>");
        Alcotest.(check int)
          "only one linked" 1
          (Html_assert.occurrences
             (Earde.Html.to_string html)
             "href='https://github.com/ccpp-owner/alpha'"));
    case
      "connected projects: a malformed namespace login is escaped, never \
       executed" (fun () ->
        let html = one ~login:"<script>alert(1)</script>" () in
        Alcotest.(check bool)
          "escaped" true
          (Html_assert.contains (Earde.Html.to_string html) "&lt;script&gt;");
        check_inert "malformed login" (Earde.Html.to_string html));
  ]

let escaping_cases =
  [
    case "connected projects: every displayed field is HTML-escaped" (fun () ->
        let hostile = "<img src=x onerror=alert(1)>\"'&" in
        let html =
          one ~name:hostile ~login:hostile
            ~website:("https://ccpp.example/?q=" ^ hostile)
            ~repositories:
              [ repo ~url:"https://github.com/a/b" (hostile ^ "/x") ]
            ()
        in
        Alcotest.(check bool)
          "no raw img tag" false
          (Html_assert.contains (Earde.Html.to_string html) "<img ");
        Alcotest.(check bool)
          "escaped lt" true
          (Html_assert.contains (Earde.Html.to_string html) "&lt;img");
        Alcotest.(check bool)
          "escaped amp" true
          (Html_assert.contains (Earde.Html.to_string html) "&amp;");
        check_inert "hostile fields" (Earde.Html.to_string html));
    case
      "connected projects: no identifier, provenance or private workflow data \
       can appear" (fun () ->
        (* The page model carries none of these, so the assertion is that
           nothing resembling them is synthesized by the renderer. *)
        let html =
          render
            [
              project ~name:"Ccpp One" ~slug:"ccpp-one" ();
              project ~name:"Ccpp Two" ~slug:"ccpp-two"
                ~verification:Ccp.Revoked ();
            ]
        in
        List.iter
          (fun needle ->
            Alcotest.(check bool)
              ("no " ^ needle) false
              (Html_assert.contains (Earde.Html.to_string html) needle))
          [
            "Requested by";
            "Reviewed by";
            "request note";
            "Private";
            "relation";
            "installation";
            "member";
            "star";
            "karma";
            "moderator";
            "Accept";
            "Reject";
          ];
        check_no_officiality "identifiers" (Earde.Html.to_string html);
        check_inert "identifiers" (Earde.Html.to_string html));
  ]

let suites =
  (* Connected-projects section renderer: section copy, verification and
       kind vocabulary, link safety, defensive degradation, escaping, and
       the absence of officiality language and private workflow data.
       DB-free. *)
  [
    ("community_connected_projects_section_empty", empty_cases);
    ("community_connected_projects_section_content", content_cases);
    ("community_connected_projects_section_links", link_cases);
    ("community_connected_projects_section_defensive", defensive_cases);
    ("community_connected_projects_section_escaping", escaping_cases);
  ]
