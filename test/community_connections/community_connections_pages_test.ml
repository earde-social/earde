(* Community connections, slice 2 — the pure eligibility rule and the
   management/search/confirmation renderers. DB-free. *)

module Cc = Earde.Community_connections

module P = Earde.Community_connections_pages

let contains haystack needle = Html_assert.occurs haystack ~needle

let count_sub haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  if nl = 0 || hl < nl then 0
  else
    let rec go i n =
      if i > hl - nl then n
      else if String.sub haystack i nl = needle then go (i + nl) (n + 1)
      else go (i + 1) n
    in
    go 0 0

(* === the one eligibility rule === *)

let eligibility_case =
  Alcotest.test_case
    "eligibility: public + published + discoverable, and nothing else"
    `Quick (fun () ->
      let check label ~visibility ~onboarding_state ~discoverable expected =
        Alcotest.(check bool) label expected
          (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
      in
      let vis = [ (Earde.Db.Community_public, "public")
                ; (Earde.Db.Community_private, "private") ] in
      let states = [ (Earde.Db.Community_published, "published")
                   ; (Earde.Db.Community_draft, "draft") ] in
      List.iter
        (fun (visibility, vname) ->
          List.iter
            (fun (onboarding_state, sname) ->
              List.iter
                (fun discoverable ->
                  let expected =
                    visibility = Earde.Db.Community_public
                    && onboarding_state = Earde.Db.Community_published
                    && discoverable
                  in
                  check
                    (Printf.sprintf "%s/%s/discoverable=%b" vname sname
                       discoverable)
                    ~visibility ~onboarding_state ~discoverable expected)
                [ true; false ])
            states)
        vis;
      (* Exactly one combination qualifies out of the eight. *)
      let qualifying =
        List.concat_map
          (fun (visibility, _) ->
            List.concat_map
              (fun (onboarding_state, _) ->
                List.filter
                  (fun discoverable ->
                    Cc.connection_eligible ~visibility ~onboarding_state
                      ~discoverable)
                  [ true; false ])
              states)
          vis
      in
      Alcotest.(check int) "one qualifying combination" 1
        (List.length qualifying))

(* === page fixtures === *)

let community ?(eligible = true) () =
  { P.name = "Ccon Source"; slug = "ccon-source"; eligible }

let counterpart ?(name = "OCaml") ?(slug = "ocaml") () =
  { P.counterpart_name = name; counterpart_slug = slug }

let state ?(eligible = true) ?(accepted = []) ?(incoming = []) ?(outgoing = [])
    () =
  { P.community = community ~eligible (); accepted; incoming; outgoing }

let render_management ?(eligible = true) ?accepted ?incoming ?outgoing
    ?feedback () =
  P.management_page ~state:(state ~eligible ?accepted ?incoming ?outgoing ())
    ~feedback ()

(* === management page === *)

let sections_case =
  Alcotest.test_case "management: the three sections always render, with \
                      symmetric connected-community copy" `Quick (fun () ->
      let html =
        render_management
          ~accepted:
            [ { P.accepted_id = "7"; accepted_with = counterpart () } ]
          ~incoming:
            [ { P.pending_id = "8";
                pending_with = counterpart ~name:"Reason" ~slug:"reason" ();
                pending_note = Some "we run a joint reading group" } ]
          ~outgoing:
            [ { P.pending_id = "9";
                pending_with = counterpart ~name:"Gleam" ~slug:"gleam" ();
                pending_note = None } ]
          ()
      in
      List.iter
        (fun needle -> Alcotest.(check bool) ("has " ^ needle) true
            (contains html needle))
        [ ">Connected communities</h2>"; ">Incoming requests</h2>"
        ; ">Outgoing requests</h2>"; ">Connect a community</h2>"
        ; ">OCaml</a>"; "/c/ocaml"; ">Reason</a>"; ">Gleam</a>"
        ; "we run a joint reading group"; "Waiting for a reply" ];
      (* Exact action paths, community-scoped and id-addressed. *)
      List.iter
        (fun needle -> Alcotest.(check bool) ("action " ^ needle) true
            (contains html needle))
        [ "action='/c/ccon-source/settings/connections/7/remove'"
        ; "action='/c/ccon-source/settings/connections/8/accept'"
        ; "action='/c/ccon-source/settings/connections/8/reject'" ];
      (* An outgoing request is a state, not a control. *)
      Alcotest.(check int) "no outgoing action" 0
        (count_sub html "connections/9/");
      Alcotest.(check int) "three forms in total" 3
        (count_sub html "<form method='POST'"))

let empty_sections_case =
  Alcotest.test_case "management: every empty section says so plainly"
    `Quick (fun () ->
      let html = render_management () in
      List.iter
        (fun needle -> Alcotest.(check bool) ("has " ^ needle) true
            (contains html needle))
        [ "No connected communities yet."; "No incoming requests."
        ; "No outgoing requests." ];
      Alcotest.(check int) "no forms at all" 0
        (count_sub html "<form method='POST'"))

(* The action a moderator comes here to take leads; the three lists are the
   record of what it produced. The order is fixed, so an empty page and a
   busy one read the same way. *)
let section_order_case =
  Alcotest.test_case
    "management: Connect a community leads, and its CTA is the panel's \
     primary action" `Quick (fun () ->
      let index_of haystack needle =
        let hl = String.length haystack and nl = String.length needle in
        let rec go i =
          if i > hl - nl then None
          else if String.sub haystack i nl = needle then Some i
          else go (i + 1)
        in
        go 0
      in
      let check_order label html =
        let positions =
          List.map
            (fun heading ->
              match index_of html heading with
              | Some i -> i
              | None -> Alcotest.failf "%s: %s missing" label heading)
            [ ">Connect a community</h2>"; ">Connected communities</h2>"
            ; ">Incoming requests</h2>"; ">Outgoing requests</h2>" ]
        in
        Alcotest.(check (list int))
          (label ^ ": sections render in the required order")
          (List.sort compare positions) positions
      in
      check_order "empty" (render_management ());
      check_order "populated"
        (render_management
           ~accepted:
             [ { P.accepted_id = "7"; accepted_with = counterpart () } ]
           ~incoming:
             [ { P.pending_id = "8"; pending_with = counterpart ();
                 pending_note = None } ]
           ~outgoing:
             [ { P.pending_id = "9"; pending_with = counterpart ();
                 pending_note = None } ]
           ());
      (* The CTA is the shared button control, an ordinary link, with its
         label and destination unchanged. *)
      let html = render_management () in
      Alcotest.(check bool) "CTA on the shared primary button" true
        (contains html
           "<a class='btn btn--primary' \
            href='/c/ccon-source/settings/connections/new'>Find a \
            community</a>");
      (* One control. The phrase also opens the explanatory copy above it,
         so the anchor is what gets counted. *)
      Alcotest.(check int) "exactly one CTA control" 1
        (count_sub html "class='btn btn--primary'");
      (* Nothing outside the anchor became clickable, and no script. *)
      Alcotest.(check int) "no onclick anywhere" 0 (count_sub html "onclick");
      Alcotest.(check int) "no script" 0 (count_sub html "<script"))

let note_escaping_case =
  Alcotest.test_case "management: the private note is escaped, labelled, and \
                      confined to pending rows" `Quick (fun () ->
      let hostile = "<script>alert('x')</script> & \"quoted\"" in
      let html =
        render_management
          ~accepted:
            [ { P.accepted_id = "1"; accepted_with = counterpart () } ]
          ~incoming:
            [ { P.pending_id = "2"; pending_with = counterpart ();
                pending_note = Some hostile } ]
          ()
      in
      Alcotest.(check bool) "no raw script tag" false
        (contains html "<script>alert");
      Alcotest.(check bool) "escaped instead" true
        (contains html "&lt;script&gt;alert");
      Alcotest.(check bool) "escaped ampersand" true (contains html "&amp;");
      Alcotest.(check bool) "labelled private" true
        (contains html "Private note");
      (* One note block for one pending row; the accepted row carries
         none. *)
      Alcotest.(check int) "exactly one note block" 1
        (count_sub html "class='ccn-note'"))

let no_people_case =
  Alcotest.test_case "management: no usernames, no relationship labels, no \
                      approval copy" `Quick (fun () ->
      let html =
        render_management
          ~accepted:
            [ { P.accepted_id = "1"; accepted_with = counterpart () } ]
          ~incoming:
            [ { P.pending_id = "2"; pending_with = counterpart ();
                pending_note = Some "note" } ]
          ~outgoing:
            [ { P.pending_id = "3"; pending_with = counterpart ();
                pending_note = None } ]
          ()
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("absent: " ^ needle) false
            (contains html needle))
        [ "Accepted by"; "Requested by"; "Reviewed by"; "Removed by"
        ; "Approved by"; "accepted by"; "requested by"
          (* no relationship vocabulary of any kind *)
        ; "relationship"; "Relationship"; "partner"; "Partner"; "parent"
        ; "child"; "Tier"; "kind=" ])

let ineligible_case =
  Alcotest.test_case "management: an ineligible community keeps reject and \
                      remove, loses accept and connect" `Quick (fun () ->
      let html =
        render_management ~eligible:false
          ~accepted:
            [ { P.accepted_id = "1"; accepted_with = counterpart () } ]
          ~incoming:
            [ { P.pending_id = "2"; pending_with = counterpart ();
                pending_note = None } ]
          ()
      in
      Alcotest.(check bool) "generic notice" true
        (contains html "cannot create or accept new connections right now");
      Alcotest.(check bool) "remove survives" true
        (contains html "connections/1/remove");
      Alcotest.(check bool) "reject survives" true
        (contains html "connections/2/reject");
      Alcotest.(check bool) "accept suppressed" false
        (contains html "connections/2/accept");
      Alcotest.(check bool) "accept explained, not disabled" true
        (contains html "Accepting is unavailable");
      Alcotest.(check bool) "search entry closed" false
        (contains html "href='/c/ccon-source/settings/connections/new'");
      (* The notice never names which of the three facts failed. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("no reason: " ^ needle) false
            (contains html needle))
        [ "private"; "Private"; "draft"; "Draft"; "undiscoverable"
        ; "not discoverable" ])

let hostile_ids_case =
  Alcotest.test_case "management: a non-numeric id or unaddressable slug \
                      renders no action" `Quick (fun () ->
      let html =
        render_management
          ~accepted:
            [ { P.accepted_id = "1/../9"; accepted_with = counterpart () }
            ; { P.accepted_id = ""; accepted_with = counterpart () } ]
          ~incoming:
            [ { P.pending_id = "2 3"; pending_with = counterpart ();
                pending_note = None } ]
          ()
      in
      Alcotest.(check int) "no forms" 0 (count_sub html "<form method='POST'");
      Alcotest.(check bool) "no traversal in any path" false
        (contains html "..");
      (* A counterpart with a non-addressable slug still renders as text. *)
      let html2 =
        render_management
          ~accepted:
            [ { P.accepted_id = "5";
                accepted_with = counterpart ~slug:"bad/slug" () } ]
          ()
      in
      Alcotest.(check bool) "identity still shown" true
        (contains html2 "OCaml");
      Alcotest.(check bool) "but not linked" false
        (contains html2 "href='/c/bad/slug'"))

let feedback_case =
  Alcotest.test_case "management: every feedback outcome is stable and \
                      non-revealing" `Quick (fun () ->
      List.iter
        (fun (feedback, needle) ->
          let html = render_management ~feedback ~eligible:true () in
          Alcotest.(check bool) ("copy: " ^ needle) true
            (contains html needle);
          List.iter
            (fun leak ->
              Alcotest.(check bool) ("no leak " ^ leak) false
                (contains html leak))
            [ "SQL"; "Caqti"; "constraint"; "community_connections"
            ; "user_id"; "top_mod" ])
        [ (P.Already_connected, "already connected")
        ; (P.Review_unavailable, "no longer pending")
        ; (P.Removal_unavailable, "no longer active")
        ; (P.Target_unavailable, "not available to connect")
        ; (P.Source_ineligible, "cannot create or accept new connections")
        ; (P.Note_invalid, "2,000 characters")
        ; (P.Stale_form, "open too long")
        ; (P.Action_failed, "couldn't complete that action") ])

(* === search and confirmation === *)

let search_states_case =
  Alcotest.test_case "search: an unsearched page and an empty result are \
                      different copy, and neither states a count" `Quick
    (fun () ->
      let blank =
        P.target_search_page ~community:(community ()) ~query:"" ~results:[]
          ~searched:false ~feedback:None ()
      in
      Alcotest.(check bool) "prompt" true
        (contains blank "Enter a name or address to search.");
      let empty =
        P.target_search_page ~community:(community ()) ~query:"zzz"
          ~results:[] ~searched:true ~feedback:None ()
      in
      Alcotest.(check bool) "no match" true
        (contains empty "No communities matched.");
      let found =
        P.target_search_page ~community:(community ()) ~query:"oc"
          ~results:
            [ { P.target_name = "OCaml"; target_slug = "ocaml" }
            ; { P.target_name = "Reason"; target_slug = "reason" } ]
          ~searched:true ~feedback:None ()
      in
      (* Results link into step two; no note field lives on a result row,
         and no total, count, or "showing N of M" is rendered. *)
      Alcotest.(check bool) "continue link" true
        (contains found "target=ocaml");
      Alcotest.(check int) "no textarea in results" 0
        (count_sub found "<textarea");
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("no count: " ^ needle) false
            (contains found needle))
        [ "results)"; "2 communities"; "of 2"; "Showing"; "showing" ];
      (* The eligibility rule is stated as a rule, never as a verdict on a
         specific withheld community. *)
      Alcotest.(check bool) "rule stated once" true
        (contains found "public, published, discoverable"))

let search_escaping_case =
  Alcotest.test_case "search: the query and every result identity are escaped"
    `Quick (fun () ->
      let html =
        P.target_search_page ~community:(community ())
          ~query:"\"><script>alert(1)</script>"
          ~results:
            [ { P.target_name = "<b>Bold</b>"; target_slug = "bold" } ]
          ~searched:true ~feedback:None ()
      in
      Alcotest.(check bool) "no raw script" false
        (contains html "<script>alert");
      Alcotest.(check bool) "no raw bold" false (contains html "<b>Bold");
      Alcotest.(check bool) "escaped name" true (contains html "&lt;b&gt;Bold"))

let confirm_case =
  Alcotest.test_case "confirm: one target, one optional note, one submit"
    `Quick (fun () ->
      let html =
        P.confirm_page ~community:(community ())
          ~target:{ P.target_name = "OCaml"; target_slug = "ocaml" }
          ~note:"draft <text>" ~feedback:None ()
      in
      Alcotest.(check bool) "posts to the request route" true
        (contains html
           "action='/c/ccon-source/settings/connections/request'");
      Alcotest.(check int) "one hidden target" 1
        (count_sub html "<input type='hidden' name='target'");
      Alcotest.(check bool) "the exact target slug" true
        (contains html "value='ocaml'");
      Alcotest.(check int) "exactly one textarea" 1
        (count_sub html "<textarea");
      Alcotest.(check bool) "named note" true (contains html "name='note'");
      Alcotest.(check bool) "capped at the domain limit" true
        (contains html "maxlength='2000'");
      Alcotest.(check bool) "note redisplayed, escaped" true
        (contains html "draft &lt;text&gt;");
      Alcotest.(check bool) "labelled private" true
        (contains html "Private note (optional)");
      (* An unaddressable target never becomes a form. *)
      let bad =
        P.confirm_page ~community:(community ())
          ~target:{ P.target_name = "Bad"; target_slug = "a/b" }
          ~note:"" ~feedback:None ()
      in
      Alcotest.(check int) "no form for an unaddressable target" 0
        (count_sub bad "<form method='POST'"))

let csrf_field_case =
  Alcotest.test_case "every mutation form carries the framework CSRF field"
    `Quick (fun () ->
      let captured = ref None in
      let pipeline =
        Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
        @@ fun req ->
        captured :=
          Some
            ( P.management_page ~request:req
                ~state:
                  (state
                     ~accepted:
                       [ { P.accepted_id = "1";
                           accepted_with = counterpart () } ]
                     ~incoming:
                       [ { P.pending_id = "2";
                           pending_with = counterpart ();
                           pending_note = None } ]
                     ())
                ~feedback:None (),
              P.confirm_page ~request:req ~community:(community ())
                ~target:{ P.target_name = "OCaml"; target_slug = "ocaml" }
                ~note:"" ~feedback:None () );
        Dream.html ""
      in
      ignore
        (Lwt_main.run
           (pipeline
              (Dream.request ~method_:`GET
                 ~target:"/c/ccon-source/settings/connections" "")));
      match !captured with
      | None -> Alcotest.fail "renderer did not run"
      | Some (management, confirm) ->
          (* One token per form: three forms on the management page, one on
             the confirmation page. *)
          Alcotest.(check int) "management tokens" 3
            (count_sub management "name=\"dream.csrf\"");
          Alcotest.(check int) "confirm token" 1
            (count_sub confirm "name=\"dream.csrf\""))

let noindex_case =
  Alcotest.test_case "every connections document is noindex" `Quick
    (fun () ->
      List.iter
        (fun html ->
          Alcotest.(check bool) "noindex" true (contains html "noindex"))
        [ render_management ()
        ; P.target_search_page ~community:(community ()) ~query:""
            ~results:[] ~searched:false ~feedback:None ()
        ; P.confirm_page ~community:(community ())
            ~target:{ P.target_name = "OCaml"; target_slug = "ocaml" }
            ~note:"" ~feedback:None () ])

let suite =
  [ eligibility_case; sections_case; empty_sections_case; section_order_case
  ; note_escaping_case
  ; no_people_case; ineligible_case; hostile_ids_case; feedback_case
  ; search_states_case; search_escaping_case; confirm_case; csrf_field_case
  ; noindex_case ]

let suites =
  [ ("community_connections_ui", suite)
  ]
