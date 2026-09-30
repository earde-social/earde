(* ===================================================================== *)
(* === Shared threads HTTP slice (Slice 2): pure page suites =========== *)
(* ===================================================================== *)

(* DB-free rendering checks for the two shared-threads surfaces: the pure
   page module is called directly with fabricated view models (no request,
   so no CSRF field and no live forms are required), and every assertion is
   a substring over the returned document. The HTTP module below covers the
   same surfaces end-to-end over real rows. *)

module Pg = Earde.Shared_thread_placement_pages

let contains haystack needle = Html_assert.occurs haystack ~needle
let case name f = Alcotest.test_case name `Quick f

let must body needle =
  if not (contains body needle) then Alcotest.failf "missing fragment %S" needle

let must_not body needle =
  if contains body needle then Alcotest.failf "forbidden fragment %S" needle

let candidate ?(name = "Sth Ui Dest") ?(slug = "sth-ui-dest") () =
  { Pg.candidate_name = name; candidate_slug = slug }

let placement ?(id = "7") ?(name = "Sth Ui Dest") ?(slug = "sth-ui-dest")
    ?(pending = true) ?(withdraw = false) ?(remove = false) ?note () =
  {
    Pg.share_placement_id = id;
    share_destination_name = name;
    share_destination_slug = slug;
    share_pending = pending;
    share_can_withdraw = withdraw;
    share_can_remove = remove;
    share_note = note;
  }

let share_state ?(candidates = []) ?(placements = [])
    ?(manage_connections = false) () =
  {
    Pg.share_thread_title = "Sth Ui Thread";
    share_origin_name = "Sth Ui Origin";
    share_origin_slug = "sth-ui-origin";
    share_thread_path = "/c/sth-ui-origin/t/12-sth-ui-thread";
    share_candidates = candidates;
    share_placements = placements;
    share_manage_connections = manage_connections;
  }

let pending_entry ?(id = "9") ?(title = "Sth Ui Incoming")
    ?(cname = "Sth Ui Other") ?(cslug = "sth-ui-other") ?note () =
  {
    Pg.pending_id = id;
    pending_title = title;
    pending_thread_path = "/c/" ^ cslug ^ "/t/31-x";
    pending_counterpart_name = cname;
    pending_counterpart_slug = cslug;
    pending_note = note;
    pending_requested_at = "2026-08-03 10:00:00";
  }

let accepted_entry ?(id = "11") ?(title = "Sth Ui Accepted")
    ?(cname = "Sth Ui Other") ?(cslug = "sth-ui-other") ?section () =
  {
    Pg.accepted_id = id;
    accepted_title = title;
    accepted_thread_path = "/c/" ^ cslug ^ "/t/32-y";
    accepted_counterpart_name = cname;
    accepted_counterpart_slug = cslug;
    accepted_section = section;
    accepted_at = "2026-08-03 11:00:00";
  }

let management_state ?(eligible = true) ?(sections_enabled = false)
    ?(section_options = []) ?(incoming = []) ?(outgoing = [])
    ?(shared_into = []) ?(shared_from = []) () =
  {
    Pg.community_name = "Sth Ui Home";
    community_slug = "sth-ui-home";
    community_eligible = eligible;
    sections_enabled;
    section_options;
    incoming;
    outgoing;
    shared_into;
    shared_from;
  }

let share ?(state = share_state ()) ?notice ?feedback () =
  Pg.share_page ~state ~notice ~feedback ()

let manage ?(state = management_state ()) ?notice ?feedback () =
  Pg.management_page ~state ~notice ~feedback ()

(* The rendered create-shell fragment, so chrome (which legitimately
   carries scripts elsewhere) never hides a violation inside the panel —
   and page-level assertions cannot pass off chrome content as ours. *)
let fragment html =
  match Html_assert.index_from html "<div class='create-shell'>" 0 with
  | None -> Alcotest.fail "create shell missing from page"
  | Some s -> (
      match Html_assert.index_from html "</main>" s with
      | None -> String.sub html s (String.length html - s)
      | Some e -> String.sub html s (e - s))

let sections_order_case =
  case "management page: the four sections in the required order" (fun () ->
      let body = fragment (manage ()) in
      let idx label =
        match Html_assert.index_from body label 0 with
        | Some i -> i
        | None -> Alcotest.failf "section %S missing" label
      in
      let incoming = idx ">Incoming requests</h2>" in
      let outgoing = idx ">Outgoing requests</h2>" in
      let into = idx ">Shared into this community</h2>" in
      let from = idx ">Shared from this community</h2>" in
      Alcotest.(check bool) "incoming before outgoing" true (incoming < outgoing);
      Alcotest.(check bool) "outgoing before shared-into" true (outgoing < into);
      Alcotest.(check bool) "shared-into before shared-from" true (into < from);
      (* And all four empty states, since the state was empty. *)
      must body "No incoming requests.";
      must body "No outgoing requests.";
      must body "Nothing is shared into this community.";
      must body "Nothing is shared from this community.")

let no_script_case =
  case "both pages emit no JavaScript and no inline handlers" (fun () ->
      let sectioned =
        management_state ~sections_enabled:true
          ~section_options:[ { Pg.section_id = "3"; section_name = "General" } ]
          ~incoming:[ pending_entry ~note:"a note" () ]
          ~outgoing:[ pending_entry ~id:"10" () ]
          ~shared_into:[ accepted_entry ~section:"General" () ]
          ~shared_from:[ accepted_entry ~id:"12" () ]
          ()
      in
      let busy_share =
        share
          ~state:
            (share_state
               ~candidates:[ candidate () ]
               ~placements:
                 [
                   placement ~withdraw:true ();
                   placement ~id:"8" ~pending:false ~remove:true ();
                 ]
               ~manage_connections:true ())
          ()
      in
      List.iter
        (fun body ->
          let body = fragment body in
          must_not body "<script";
          must_not body "onclick";
          must_not body "javascript:")
        [ busy_share; manage ~state:sectioned () ])

let share_form_case =
  case "share page: destination select, note field, and the action" (fun () ->
      let body =
        fragment
          (share
             ~state:
               (share_state
                  ~candidates:
                    [
                      candidate ~name:"Alpha" ~slug:"sth-ui-alpha" ();
                      candidate ~name:"beta" ~slug:"sth-ui-beta" ();
                    ]
                  ())
             ())
      in
      must body "action='/c/sth-ui-origin/t/12-sth-ui-thread/share'";
      must body "name='destination'";
      must body "<option value='sth-ui-alpha'>Alpha</option>";
      must body "<option value='sth-ui-beta'>beta</option>";
      must body "name='note'";
      must body "Request sharing";
      must body "maxlength='2000'";
      (* The one canonical discussion, never a copy. *)
      must body "The discussion stays in one place";
      must_not body "copy";
      must_not body "mirror")

let share_empty_case =
  case "share page: quiet empty state, no select, gated connections link"
    (fun () ->
      let plain = fragment (share ()) in
      must plain "No connected community can receive this thread right now.";
      must_not plain "<select";
      must_not plain "Request sharing";
      must_not plain "/settings/connections";
      let manager =
        fragment (share ~state:(share_state ~manage_connections:true ()) ())
      in
      must manager "href='/c/sth-ui-origin/settings/connections'")

let share_controls_case =
  case "share page: status copy and per-row control gates" (fun () ->
      let body =
        fragment
          (share
             ~state:
               (share_state
                  ~placements:
                    [
                      placement ~id:"7" ~withdraw:false ();
                      placement ~id:"8" ~withdraw:true ();
                      placement ~id:"9" ~pending:false ~remove:false ();
                      placement ~id:"10" ~pending:false ~remove:true ();
                    ]
                  ())
             ())
      in
      must body "Awaiting approval";
      must body ">Shared<";
      must body "/c/sth-ui-origin/settings/shared-threads/8/withdraw";
      must_not body "/c/sth-ui-origin/settings/shared-threads/7/withdraw";
      must body "/c/sth-ui-origin/settings/shared-threads/10/remove";
      must_not body "/c/sth-ui-origin/settings/shared-threads/9/remove";
      must body "Withdraw request";
      must body "Stop sharing";
      (* Never internal vocabulary or ids as debug text. *)
      must_not body "pending<";
      must_not body "placement";
      must_not body "transition")

let note_case =
  case
    "both pages: the private note renders only when supplied, labelled and \
     escaped" (fun () ->
      let marker = "SthUiNote <b>bold</b> & 'quoted'" in
      let with_note =
        fragment
          (share
             ~state:(share_state ~placements:[ placement ~note:marker () ] ())
             ())
      in
      must with_note "Private note";
      must with_note "SthUiNote &lt;b&gt;bold&lt;/b&gt; &amp; &#39;quoted&#39;";
      must_not with_note "<b>bold</b>";
      let without =
        fragment (share ~state:(share_state ~placements:[ placement () ] ()) ())
      in
      must_not without "Private note";
      let mgmt =
        fragment
          (manage
             ~state:
               (management_state ~incoming:[ pending_entry ~note:marker () ] ())
             ())
      in
      must mgmt "SthUiNote &lt;b&gt;bold&lt;/b&gt; &amp; &#39;quoted&#39;")

let escaping_case =
  case "hostile titles and names stay inert on both pages" (fun () ->
      let title = "Sth <script>alert(1)</script> title" in
      let name = "Sth <img src=x onerror=alert(2)> name" in
      let body =
        fragment
          (manage
             ~state:
               (management_state
                  ~incoming:[ pending_entry ~title ~cname:name () ]
                  ())
             ())
      in
      must body "Sth &lt;script&gt;alert(1)&lt;/script&gt; title";
      must body "Sth &lt;img src=x onerror=alert(2)&gt; name";
      must_not body "<script>alert(1)</script>";
      must_not body "<img src=x")

let selector_case =
  case
    "management page: one section selector per accept form, flat pages none, \
     ineligible pages an unavailable notice" (fun () ->
      let sectioned =
        fragment
          (manage
             ~state:
               (management_state ~sections_enabled:true
                  ~section_options:
                    [
                      { Pg.section_id = "3"; section_name = "General" };
                      { Pg.section_id = "4"; section_name = "Help" };
                    ]
                  ~incoming:[ pending_entry () ]
                  ())
             ())
      in
      must sectioned "name='section'";
      must sectioned "<option value='3'>General</option>";
      must sectioned "<option value='4'>Help</option>";
      must sectioned "Choose a section";
      let flat =
        fragment
          (manage
             ~state:(management_state ~incoming:[ pending_entry () ] ())
             ())
      in
      must flat "Accept";
      must_not flat "name='section'";
      let ineligible =
        fragment
          (manage
             ~state:
               (management_state ~eligible:false
                  ~incoming:[ pending_entry () ]
                  ())
             ())
      in
      must ineligible "cannot accept newly shared threads right now";
      must_not ineligible ">Accept</button>";
      (* Rejection stays available on an ineligible community. *)
      must ineligible ">Reject</button>")

let section_label_case =
  case "accepted rows: section name or the flat label" (fun () ->
      let body =
        fragment
          (manage
             ~state:
               (management_state
                  ~shared_into:[ accepted_entry ~section:"General" () ]
                  ~shared_from:[ accepted_entry ~id:"12" () ]
                  ())
             ())
      in
      must body "Section: General";
      must body "Uncategorized")

let notice_case =
  case "each closed notice renders its copy; feedback renders alongside"
    (fun () ->
      List.iter
        (fun (notice, needle) -> must (fragment (manage ~notice ())) needle)
        [
          (Pg.Request_accepted, "now shared into this community");
          (Pg.Request_rejected, "The request was declined");
          (Pg.Request_withdrawn, "sharing request was withdrawn");
          (Pg.Placement_removed, "no longer shared");
        ];
      must (fragment (share ~notice:Pg.Request_sent ())) "Sharing request sent";
      must
        (fragment (share ~feedback:Pg.Already_shared ()))
        "already shared with that community";
      must (fragment (manage ~feedback:Pg.Stale_form ())) "open too long")

let degraded_case =
  case
    "a non-addressable slug or a non-decimal id drops the actionable form, \
     never the row" (fun () ->
      let body =
        fragment
          (share
             ~state:
               (share_state
                  ~placements:
                    [
                      placement ~id:"7'; DROP" ~withdraw:true ();
                      placement ~id:"8" ~slug:"evil/../slug" ~withdraw:true ();
                    ]
                  ())
             ())
      in
      must body "Awaiting approval";
      must_not body "7'; DROP/withdraw";
      must_not body "evil/../slug/withdraw")

let suite =
  [
    sections_order_case;
    no_script_case;
    share_form_case;
    share_empty_case;
    share_controls_case;
    note_case;
    escaping_case;
    selector_case;
    section_label_case;
    notice_case;
    degraded_case;
  ]

let suites =
  (* Shared threads, slice 2 (HTTP workflow): the pure page renders are
       DB-free; the Share and management surfaces, the five mutations'
       authorization, subject binding, and CSRF behavior, the notification
       rendering with its current-access gating, and the read-side
       boundary all run over the real routed pipeline against the gated
       database. *)
  [ ("shared_thread_pages_ui", suite) ]
