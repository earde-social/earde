module Phrp = Earde.Project_home_review_pages

(* ===== Moderator review page (Project_home_review_pages) =====
   Pure rendering over handler-supplied view models; DB-free. No GitHub,
   installation, repository external id, or credential fixtures exist
   anywhere here. Fragment-scoped assertions pin only what the feature
   templates introduce inside the shared create-shell layout. *)

let phrp_case name f = Alcotest.test_case name `Quick f

let phrp_render ?user ?(feedback = None) state =
  Phrp.project_home_review_page ?user ~state ~feedback ()

let phrp_frag ?user ?feedback state =
  Html_assert.panel_fragment (phrp_render ?user ?feedback state)

let phrp_content_cases =
  [
    phrp_case "empty queue renders copy and no forms" (fun () ->
        let frag = phrp_frag (Home_review_fixture.phrp_state ~requests:[] ()) in
        Html_assert.must frag "Project home requests";
        Html_assert.must frag "Review projects requesting this community";
        Html_assert.must frag "No pending project requests.";
        Alcotest.(check int) "no forms" 0 (Html_assert.occurrences frag "<form"));
    phrp_case
      "verified request: identity, kind, namespace, verification, factual copy"
      (fun () ->
        let frag = phrp_frag (Home_review_fixture.phrp_state ()) in
        Html_assert.must frag "Widget Kit";
        Html_assert.must frag "phrv-project-kind'>Project<";
        Html_assert.must frag "octo-org";
        Html_assert.must frag "phrv-verification'>Verified<";
        Html_assert.must frag "Project connected through GitHub";
        Html_assert.must_not frag "Official";
        Html_assert.must_not frag "GitHub-approved";
        Html_assert.must_not frag "GitHub-endorsed";
        Html_assert.must_not frag "member";
        Html_assert.must_not frag "karma";
        Html_assert.must_not frag "relation_id";
        Html_assert.must_not frag "project_id";
        Html_assert.must_not frag "community_id");
    phrp_case "stale and revoked verification copy stays distinct" (fun () ->
        let stale =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [
                   Home_review_fixture.phrp_request ~verification:Phrp.Stale ();
                 ]
               ())
        in
        Html_assert.must stale "Verification stale";
        let revoked =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [
                   Home_review_fixture.phrp_request ~verification:Phrp.Revoked
                     ();
                 ]
               ())
        in
        Html_assert.must revoked "Verification revoked");
    phrp_case "public repository links and primary/archived markers, in order"
      (fun () ->
        let repos =
          [
            Home_review_fixture.phrp_repo ~full_name:"octo-org/core"
              ~url:"https://github.com/octo-org/core" ~primary:true ();
            Home_review_fixture.phrp_repo ~full_name:"octo-org/docs"
              ~url:"https://github.com/octo-org/docs" ~archived:true ();
          ]
        in
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [ Home_review_fixture.phrp_request ~repositories:repos () ]
               ())
        in
        Html_assert.must frag "href='https://github.com/octo-org/core'";
        Html_assert.must frag "octo-org/core";
        Html_assert.must frag "octo-org/docs";
        Html_assert.must frag ">Primary<";
        Html_assert.must frag ">Archived<";
        Html_assert.order frag "octo-org/core" "octo-org/docs");
    phrp_case "deleted requester copy; present requester escaped" (fun () ->
        let deleted =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:[ Home_review_fixture.phrp_request () ]
               ())
        in
        Html_assert.must deleted "Requested by Deleted user";
        let named =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [ Home_review_fixture.phrp_request ~requester:"a<lice" () ]
               ())
        in
        Html_assert.must named "Requested by a&lt;lice";
        Html_assert.must_not named "a<lice");
    phrp_case "private note is labelled, escaped, never markup" (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [
                   Home_review_fixture.phrp_request
                     ~note:"<script>alert('x')</script> & <b>b</b>" ();
                 ]
               ())
        in
        Html_assert.must frag "Private request note";
        Html_assert.must frag "visible only to";
        Html_assert.must frag
          "&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt; &amp; \
           &lt;b&gt;b&lt;/b&gt;";
        Html_assert.must_not frag "<script>alert";
        Html_assert.must_not frag "<b>b</b>";
        let none =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:[ Home_review_fixture.phrp_request () ]
               ())
        in
        Html_assert.must_not none "Private request note");
    phrp_case
      "no scripts, inline styles, handlers, javascript urls, or credential \
       material" (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:[ Home_review_fixture.phrp_request ~note:"hi" () ]
               ())
        in
        Html_assert.must_not frag "<script";
        Html_assert.must_not frag "style=";
        Html_assert.must_not frag "onclick";
        Html_assert.must_not frag "onsubmit";
        Html_assert.must_not frag "javascript:";
        Html_assert.must_not frag "token";
        Html_assert.must_not frag "installation";
        Html_assert.must_not frag "secret");
  ]

let phrp_form_cases =
  [
    phrp_case
      "exact accept and reject actions, nameless submit, zero application \
       fields" (fun () ->
        let frag = phrp_frag (Home_review_fixture.phrp_state ()) in
        Alcotest.(check int)
          "two forms" 2
          (Html_assert.occurrences frag "<form");
        Html_assert.must frag "action='/c/alpine/projects/widget-kit/accept'";
        Html_assert.must frag "action='/c/alpine/projects/widget-kit/reject'";
        Html_assert.must frag ">Accept as community home<";
        Html_assert.must frag ">Reject request<";
        (* No application field and no framework field without a request. *)
        Alcotest.(check int)
          "no name attributes" 0
          (Html_assert.occurrences frag "name=");
        Html_assert.must_not frag "type='hidden'";
        Html_assert.must_not frag "value=";
        Html_assert.must_not frag "disabled");
    phrp_case "accept only for Verified + Eligible; rejection always present"
      (fun () ->
        let no_accept label state =
          let frag = phrp_frag state in
          Alcotest.(check int)
            (label ^ ": one form only")
            1
            (Html_assert.occurrences frag "<form");
          Html_assert.must_not frag "/accept'";
          Html_assert.must frag "/reject'";
          Html_assert.must frag "Acceptance is unavailable for this request."
        in
        no_accept "stale"
          (Home_review_fixture.phrp_state
             ~requests:
               [ Home_review_fixture.phrp_request ~verification:Phrp.Stale () ]
             ());
        no_accept "revoked"
          (Home_review_fixture.phrp_state
             ~requests:
               [
                 Home_review_fixture.phrp_request ~verification:Phrp.Revoked ();
               ]
             ());
        no_accept "ineligible community"
          (Home_review_fixture.phrp_state
             ~community:
               (Home_review_fixture.phrp_community
                  ~eligibility:Phrp.Currently_ineligible ())
             ());
        let ok = phrp_frag (Home_review_fixture.phrp_state ()) in
        Html_assert.must ok "/accept'";
        Html_assert.must_not ok "Acceptance is unavailable");
    phrp_case "ineligible community: page notice and every accept suppressed"
      (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~community:
                 (Home_review_fixture.phrp_community
                    ~eligibility:Phrp.Currently_ineligible ())
               ~requests:
                 [
                   Home_review_fixture.phrp_request ~slug:"one" ();
                   Home_review_fixture.phrp_request ~slug:"two" ();
                 ]
               ())
        in
        Html_assert.must frag
          "This community cannot currently accept project-home requests.";
        Alcotest.(check int)
          "no accept actions" 0
          (Html_assert.occurrences frag "/accept'");
        Alcotest.(check int)
          "two reject actions" 2
          (Html_assert.occurrences frag "/reject'"));
    phrp_case "empty repository list suppresses acceptance, keeps rejection"
      (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [ Home_review_fixture.phrp_request ~repositories:[] () ]
               ())
        in
        Html_assert.must_not frag "/accept'";
        Html_assert.must frag "/reject'";
        Html_assert.must frag "Acceptance is unavailable for this request.";
        Html_assert.must frag "No repositories.");
  ]

let phrp_defensive_cases =
  [
    phrp_case "invalid community slug suppresses every form" (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~community:
                 (Home_review_fixture.phrp_community ~slug:"bad slug" ())
               ())
        in
        Alcotest.(check int) "no forms" 0 (Html_assert.occurrences frag "<form");
        (* Identity still renders — only the actions drop. *)
        Html_assert.must frag "Widget Kit");
    phrp_case "invalid project slug suppresses only that request's forms"
      (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [
                   Home_review_fixture.phrp_request ~name:"Bad One"
                     ~slug:"Bad--Slug" ();
                   Home_review_fixture.phrp_request ~name:"Good One"
                     ~slug:"good-one" ();
                 ]
               ())
        in
        Html_assert.must frag "Bad One";
        Html_assert.must frag "Good One";
        Html_assert.must frag "/projects/good-one/accept'";
        Html_assert.must_not frag "Bad--Slug";
        Alcotest.(check int)
          "only the good request is actionable" 2
          (Html_assert.occurrences frag "<form"));
    phrp_case "duplicate project slug leaves at most one actionable group"
      (fun () ->
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [
                   Home_review_fixture.phrp_request ~name:"First" ~slug:"dup" ();
                   Home_review_fixture.phrp_request ~name:"Second" ~slug:"dup"
                     ();
                 ]
               ())
        in
        Html_assert.must frag "First";
        Html_assert.must frag "Second";
        Alcotest.(check int)
          "one accept action" 1
          (Html_assert.occurrences frag "/accept'");
        Alcotest.(check int)
          "one reject action" 1
          (Html_assert.occurrences frag "/reject'"));
    phrp_case "malformed repository URL degrades to inert text" (fun () ->
        let repos =
          [
            Home_review_fixture.phrp_repo ~full_name:"octo-org/js"
              ~url:"javascript:alert(1)" ~primary:true ();
          ]
        in
        let frag =
          phrp_frag
            (Home_review_fixture.phrp_state
               ~requests:
                 [ Home_review_fixture.phrp_request ~repositories:repos () ]
               ())
        in
        Html_assert.must frag "octo-org/js";
        Html_assert.must_not frag "javascript:";
        Html_assert.must_not frag "phrv-repo-link");
  ]

let phrp_feedback_cases =
  [
    phrp_case "each feedback variant renders its generic copy" (fun () ->
        List.iter
          (fun (fb, needle) ->
            let frag =
              phrp_frag ~feedback:(Some fb) (Home_review_fixture.phrp_state ())
            in
            Html_assert.must frag "phrv-alert";
            Html_assert.must frag needle)
          [
            (Phrp.Stale_form, "This page had been open too long");
            (Phrp.Review_unavailable, "no longer pending");
            (Phrp.Project_unavailable, "no longer available for acceptance");
            (Phrp.Target_ineligible, "cannot currently accept that project");
            (Phrp.Review_failed, "couldn't review the request");
          ]);
    phrp_case "no feedback renders no alert; feedback is cosmetic" (fun () ->
        Html_assert.must_not
          (phrp_frag (Home_review_fixture.phrp_state ()))
          "phrv-alert";
        let with_fb =
          phrp_frag ~feedback:(Some Phrp.Review_failed)
            (Home_review_fixture.phrp_state ())
        in
        Alcotest.(check int)
          "forms unchanged by feedback" 2
          (Html_assert.occurrences with_fb "<form"));
  ]

(* --- Page rendering with a live request: the framework CSRF field. --- *)

let phrp_csrf_render state =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some (Phrp.project_home_review_page ~request:req ~state ~feedback:None ());
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/c/alpine/review" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

let phrp_csrf_cases =
  [
    phrp_case
      "with request: one framework field per form, hidden, no application field"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (phrp_csrf_render (Home_review_fixture.phrp_state ()))
        in
        Alcotest.(check int)
          "two framework fields" 2
          (Html_assert.occurrences frag "name=\"dream.csrf\"");
        Alcotest.(check int)
          "two hidden framework fields" 2
          (Html_assert.occurrences frag "type=\"hidden\"");
        Alcotest.(check int)
          "no application hidden field" 0
          (Html_assert.occurrences frag "type='hidden'");
        (* The framework fields are the only name= attributes. *)
        Alcotest.(check int)
          "only framework name attrs" 2
          (Html_assert.occurrences frag "name="));
    phrp_case "form-free states emit no framework field even with a request"
      (fun () ->
        Html_assert.must_not
          (Html_assert.panel_fragment
             (phrp_csrf_render (Home_review_fixture.phrp_state ~requests:[] ())))
          "dream.csrf";
        Html_assert.must_not
          (Html_assert.panel_fragment
             (phrp_csrf_render
                (Home_review_fixture.phrp_state
                   ~community:
                     (Home_review_fixture.phrp_community ~slug:"bad slug" ())
                   ())))
          "dream.csrf");
    phrp_case "pure rendering without a request stays CSRF-free" (fun () ->
        Html_assert.must_not
          (phrp_frag (Home_review_fixture.phrp_state ()))
          "dream.csrf");
  ]

let suites =
  (* Moderator review page: request rendering, accept/reject form
       contracts, defensive degradation, feedback, and framework CSRF.
       DB-free. *)
  [
    ("project_home_review_page_content", phrp_content_cases);
    ("project_home_review_page_forms", phrp_form_cases);
    ("project_home_review_page_defensive", phrp_defensive_cases);
    ("project_home_review_page_feedback", phrp_feedback_cases);
    ("project_home_review_page_csrf", phrp_csrf_cases);
  ]
