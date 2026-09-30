module Phcp = Earde.Project_home_choice_pages

(* ===== Home choice page (Project_home_choice_pages) =====
   Same fragment-scoped technique as the other create-flow page tests:
   assertions pin only what the feature templates introduce inside the
   shared layout. DB-free; no GitHub, installation, repository, or
   credential fixtures exist anywhere in these view models. *)

let phcp_case name f = Alcotest.test_case name `Quick f

let phcp_project ?(name = "Widget Kit") ?(slug = "widget-kit")
    ?(login = "octo-org") () : Phcp.project =
  { Phcp.name; slug; namespace_login = login }

let phcp_community ?(id = 31) ?(name = "Alpine Devs") ?(slug = "alpine")
    ?description ?(visibility = Phcp.Public) () : Phcp.community =
  { Phcp.id; name; slug; description; visibility }

let phcp_c1 =
  phcp_community ~id:31 ~name:"Alpine Devs" ~slug:"alpine"
    ~description:"Shared alpine tooling talk" ()

let phcp_c2 =
  phcp_community ~id:32 ~name:"Beta Builders" ~slug:"beta"
    ~visibility:Phcp.Unlisted ()

let phcp_unavailable =
  phcp_community ~id:33 ~name:"Gamma Sunset" ~slug:"gamma"
    ~visibility:Phcp.Currently_unavailable ()

let phcp_choose ?project ?(communities = [ phcp_c1; phcp_c2 ])
    ?(note = "") () =
  Phcp.Choose_existing
    { project = (match project with Some p -> p | None -> phcp_project ());
      communities; request_note = note }

let phcp_render ?user ?(feedback = None) state =
  Phcp.project_home_choice_page ?user ~state ~feedback ()

let phcp_frag ?user ?feedback state =
  Html_assert.panel_fragment (phcp_render ?user ?feedback state)

let phcp_form_cases =
  [ phcp_case "exact form method, action, and application field set"
      (fun () ->
        let frag = phcp_frag (phcp_choose ()) in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
        Html_assert.must frag
          "<form method='POST' action='/projects/widget-kit/request-home' \
           class='create-form phc-request-form'>";
        Alcotest.(check int) "two radios" 2
          (Html_assert.occurrences frag "name='target_community_id'");
        Alcotest.(check int) "one note field" 1
          (Html_assert.occurrences frag "name='request_note'");
        (* Exactly the two application fields: no other name= attribute
           exists in the fragment. *)
        Alcotest.(check int) "exact application field set" 3
          (Html_assert.occurrences frag "name='");
        Html_assert.must frag "<textarea name='request_note' maxlength='2000'";
        (* The one nameless submit control, byte-exact. *)
        Html_assert.must frag
          "<button type='submit' class='create-btn \
           create-btn--block'>Send home request</button>";
        (* No hidden project slug or identifiers of any kind. *)
        Html_assert.must_not frag "type='hidden'";
        Html_assert.must_not frag "name='project_slug'";
        Html_assert.must_not frag "name='slug'";
        Html_assert.must_not frag "name='user_id'";
        Html_assert.must_not frag "name='relation_id'";
        Html_assert.must_not frag "installation";
        Html_assert.must_not frag "return_url")
  ; phcp_case "radio options in supplied order, never preselected"
      (fun () ->
        let frag = phcp_frag (phcp_choose ()) in
        Html_assert.order frag "Alpine Devs" "Beta Builders";
        Html_assert.order frag "value='31'" "value='32'";
        Html_assert.must_not frag "checked")
  ; phcp_case "request note is escaped, never rendered as markup" (fun () ->
        let frag =
          phcp_frag
            (phcp_choose ~note:"<script>alert('x')</script> & <b>bold</b>" ())
        in
        Html_assert.must frag
          "&lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt; &amp; \
           &lt;b&gt;bold&lt;/b&gt;";
        Html_assert.must_not frag "<script>alert";
        Html_assert.must_not frag "<b>bold</b>")
  ; phcp_case "community identity, visibility labels, no invented metrics"
      (fun () ->
        let frag = phcp_frag (phcp_choose ()) in
        Html_assert.must frag "href='/c/alpine'";
        Html_assert.must frag "href='/c/beta'";
        Html_assert.must frag "phc-community-visibility'>Public<";
        Html_assert.must frag "phc-community-visibility'>Unlisted<";
        Html_assert.must frag "Shared alpine tooling talk";
        Html_assert.must_not frag "member";
        Html_assert.must_not frag "activity";
        Html_assert.must_not frag "karma";
        Html_assert.must_not frag "verified badge")
  ; phcp_case "copy: factual verification, moderator review, no officiality"
      (fun () ->
        let frag = phcp_frag (phcp_choose ()) in
        Html_assert.must frag "Connect to an existing community";
        Html_assert.must frag "Request an eligible Earde community";
        Html_assert.must frag "Project connected through GitHub";
        Html_assert.must frag "moderators must review";
        Html_assert.must frag "grants no community moderation rights";
        Html_assert.must frag "visible only to";
        Html_assert.must_not frag "Official";
        Html_assert.must_not frag "GitHub-approved";
        Html_assert.must_not frag "GitHub-endorsed")
  ; phcp_case "no GitHub, installation, repository, or credential material"
      (fun () ->
        let frag = phcp_frag (phcp_choose ()) in
        Html_assert.must_not frag "github.com";
        Html_assert.must_not frag "repository";
        Html_assert.must_not frag "installation";
        Html_assert.must_not frag "token";
        Html_assert.must_not frag "octo-org")
  ; phcp_case "no inline styles, scripts, or handlers in the fragment"
      (fun () ->
        List.iter
          (fun state ->
            let frag = phcp_frag state in
            Html_assert.must_not frag "<script";
            Html_assert.must_not frag "style='";
            Html_assert.must_not frag "onclick";
            Html_assert.must_not frag "javascript:";
            Html_assert.must_not frag "http-equiv")
          [ phcp_choose ()
          ; Phcp.No_eligible_communities (phcp_project ())
          ; Phcp.Active_relation
              { project = phcp_project ();
                relation = Phcp.Pending_request phcp_c1 }
          ; Phcp.Active_relation
              { project = phcp_project ();
                relation =
                  Phcp.Accepted_home
                    { community = phcp_c1; removal_allowed = true } }
          ; Phcp.Active_relation
              { project = phcp_project ();
                relation =
                  Phcp.Accepted_home
                    { community = phcp_c1; removal_allowed = false } }
          ])
  ]

let phcp_state_cases =
  [ phcp_case "no eligible communities: explanation and setup link, no form"
      (fun () ->
        let frag =
          phcp_frag (Phcp.No_eligible_communities (phcp_project ()))
        in
        Html_assert.must frag "No eligible published network community";
        Html_assert.must frag "href='/projects/widget-kit/setup'";
        Html_assert.must frag "Back to project setup";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "target_community_id";
        Html_assert.must_not frag "Send home request")
  ; phcp_case "pending relation: status copy and target, no request controls"
      (fun () ->
        let frag =
          phcp_frag
            (Phcp.Active_relation
               { project = phcp_project ();
                 relation = Phcp.Pending_request phcp_c1 })
        in
        Html_assert.must frag "Home request pending";
        Html_assert.must frag "Alpine Devs";
        Html_assert.must frag "href='/c/alpine'";
        Html_assert.must frag "accept or reject";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "<textarea";
        Html_assert.must_not frag "target_community_id";
        Html_assert.must_not frag "Send home request";
        Html_assert.must_not frag "phc-note")
  ; phcp_case "accepted relation: connected copy, no moderation implication"
      (fun () ->
        let frag =
          phcp_frag
            (Phcp.Active_relation
               { project = phcp_project ();
                 relation =
                   Phcp.Accepted_home
                     { community = phcp_c1; removal_allowed = true } })
        in
        Html_assert.must frag "Community home connected";
        Html_assert.must frag "href='/c/alpine'";
        Html_assert.must frag "moderation stays with its moderators";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "target_community_id";
        Html_assert.must_not frag "Send home request";
        Html_assert.must_not frag "Official")
  ; phcp_case
      "unavailable active target: generic label only, relation still shown"
      (fun () ->
        List.iter
          (fun (relation, status_copy) ->
            let frag =
              phcp_frag
                (Phcp.Active_relation
                   { project = phcp_project (); relation })
            in
            Html_assert.must frag status_copy;
            Html_assert.must frag "Gamma Sunset";
            Html_assert.must frag "href='/c/gamma'";
            Html_assert.must frag "Currently unavailable";
            (* The generic label never becomes a reason or a false
               publication state. *)
            Html_assert.must_not frag "Unlisted";
            Html_assert.must_not frag "Private";
            Html_assert.must_not frag "Draft";
            Html_assert.must_not frag "Legacy";
            Html_assert.must_not frag "indexable";
            Html_assert.must_not frag "discoverable";
            Html_assert.must_not frag "onboarding";
            Html_assert.must_not frag "network community";
            (* Still no request controls and no target id. *)
            Html_assert.must_not frag "<form";
            Html_assert.must_not frag "target_community_id";
            Html_assert.must_not frag "Send home request";
            Html_assert.must_not frag "value='33'";
            Html_assert.must_not frag "'33'")
          [ (Phcp.Pending_request phcp_unavailable, "Home request pending")
          ; ( Phcp.Accepted_home
                { community = phcp_unavailable; removal_allowed = true },
              "Community home connected" )
          ])
  ]

let phcp_defensive_cases =
  [ phcp_case "invalid project slug: no form, no project-derived links"
      (fun () ->
        List.iter
          (fun slug ->
            let frag =
              phcp_frag
                (phcp_choose ~project:(phcp_project ~slug ()) ())
            in
            Html_assert.must frag "Connect to an existing community";
            Html_assert.must_not frag "<form";
            Html_assert.must_not frag "request-home";
            Html_assert.must_not frag "/projects/")
          [ ""; "Widget Kit"; "-widget"; "widget-"; "wid--get";
            String.make 81 'a' ])
  ; phcp_case "invalid project slug: no setup link either" (fun () ->
        let frag =
          phcp_frag
            (Phcp.No_eligible_communities (phcp_project ~slug:"Bad Slug" ()))
        in
        Html_assert.must frag "No eligible published network community";
        Html_assert.must_not frag "href='/projects";
        Html_assert.must_not frag "Bad Slug")
  ; phcp_case "non-positive community ids never become radio inputs"
      (fun () ->
        let corrupt_zero = phcp_community ~id:0 ~name:"Zero Corrupt" () in
        let corrupt_neg = phcp_community ~id:(-3) ~name:"Neg Corrupt" () in
        let frag =
          phcp_frag
            (phcp_choose ~communities:[ corrupt_zero; corrupt_neg ] ())
        in
        (* With no valid option left there is no request form at all, so
           nothing renders that could carry a corrupt id. *)
        Html_assert.must frag "Connect to an existing community";
        Html_assert.must_not frag "type='radio'";
        Html_assert.must_not frag "value='0'";
        Html_assert.must_not frag "value='-3'";
        Html_assert.must_not frag "<form")
  ; phcp_case "a valid option keeps the form; corrupt rows stay inert"
      (fun () ->
        let corrupt = phcp_community ~id:0 ~name:"Zero Corrupt" () in
        let frag =
          phcp_frag (phcp_choose ~communities:[ phcp_c1; corrupt ] ())
        in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
        Alcotest.(check int) "one radio" 1 (Html_assert.occurrences frag "type='radio'");
        Html_assert.must frag "Zero Corrupt")
  ; phcp_case "currently-unavailable community is never selectable"
      (fun () ->
        (* Alone: nothing actionable remains, so no form at all. *)
        let alone =
          phcp_frag (phcp_choose ~communities:[ phcp_unavailable ] ())
        in
        Html_assert.must_not alone "<form";
        Html_assert.must_not alone "type='radio'";
        Html_assert.must_not alone "value='33'";
        (* Beside a valid option: the form renders with exactly one radio,
           and the unavailable row stays inert with the generic label. *)
        let mixed =
          phcp_frag
            (phcp_choose ~communities:[ phcp_c1; phcp_unavailable ] ())
        in
        Alcotest.(check int) "one radio" 1 (Html_assert.occurrences mixed "type='radio'");
        Html_assert.must mixed "Gamma Sunset";
        Html_assert.must mixed "Currently unavailable";
        Html_assert.must_not mixed "value='33'")
  ; phcp_case "invalid community slug renders no actionable link" (fun () ->
        let broken =
          phcp_community ~id:41 ~name:"Broken Slug" ~slug:"has space" ()
        in
        let frag = phcp_frag (phcp_choose ~communities:[ broken ] ()) in
        Html_assert.must frag "Broken Slug";
        Html_assert.must_not frag "<a href='/c/";
        (* The identity still renders as inert text. *)
        Html_assert.must frag "/c/has space")
  ; phcp_case "duplicate community ids render one actionable option"
      (fun () ->
        let duplicate = { phcp_c1 with Phcp.name = "Alpine Mirror" } in
        let frag =
          phcp_frag (phcp_choose ~communities:[ phcp_c1; duplicate ] ())
        in
        Alcotest.(check int) "one radio" 1 (Html_assert.occurrences frag "type='radio'");
        Alcotest.(check int) "one value" 1 (Html_assert.occurrences frag "value='31'"))
  ; phcp_case "empty community list renders no form" (fun () ->
        let frag = phcp_frag (phcp_choose ~communities:[] ()) in
        Html_assert.must frag "Connect to an existing community";
        Html_assert.must_not frag "<form")
  ]

let phcp_feedback_cases =
  [ phcp_case "each feedback variant renders its generic copy" (fun () ->
        List.iter
          (fun (feedback, marker) ->
            let frag = phcp_frag ~feedback:(Some feedback) (phcp_choose ()) in
            Html_assert.must frag "phc-alert";
            Html_assert.must frag marker)
          [ (Phcp.Stale_form, "This page had been open too long")
          ; (Phcp.Request_form_invalid, "Review the form and try again")
          ; (Phcp.Community_unavailable,
             "no longer available for project requests")
          ; (Phcp.Active_home_exists,
             "already has a pending request or community home")
          ; (Phcp.Request_failed, "Try again.")
          ])
  ; phcp_case "no feedback renders no alert container" (fun () ->
        Html_assert.must_not (phcp_frag (phcp_choose ())) "phc-alert")
  ; phcp_case "feedback is cosmetic: an active relation still shows no form"
      (fun () ->
        let frag =
          phcp_frag ~feedback:(Some Phcp.Active_home_exists)
            (Phcp.Active_relation
               { project = phcp_project ();
                 relation = Phcp.Pending_request phcp_c1 })
        in
        Html_assert.must frag "phc-alert";
        Html_assert.must_not frag "<form")
  ]

(* --- Page rendering with a live request: the framework CSRF field. --- *)

let phcp_csrf_render state =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Phcp.project_home_choice_page ~request:req ~state ~feedback:None ());
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline
          (Dream.request ~method_:`GET ~target:"/projects/widget-kit/home"
             "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

let phcp_csrf_cases =
  [ phcp_case "choose with request: one framework field inside the form"
      (fun () ->
        let frag = Html_assert.panel_fragment (phcp_csrf_render (phcp_choose ())) in
        Alcotest.(check int) "one framework field" 1
          (Html_assert.occurrences frag "name=\"dream.csrf\"");
        Alcotest.(check int) "framework field is hidden" 1
          (Html_assert.occurrences frag "type=\"hidden\"");
        (* Still zero application-owned hidden fields. *)
        Alcotest.(check int) "no application hidden field" 0
          (Html_assert.occurrences frag "type='hidden'");
        match
          ( Html_assert.index_from frag "<form" 0,
            Html_assert.index_from frag "name=\"dream.csrf\"" 0,
            Html_assert.index_from frag "</form>" 0 )
        with
        | Some f, Some c, Some e ->
            Alcotest.(check bool) "CSRF inside the form" true (f < c && c < e)
        | _ -> Alcotest.fail "form or CSRF field missing")
  ; phcp_case "form-free states emit no framework field even with a request"
      (fun () ->
        List.iter
          (fun state ->
            Html_assert.must_not (Html_assert.panel_fragment (phcp_csrf_render state)) "dream.csrf")
          [ Phcp.No_eligible_communities (phcp_project ())
          ; Phcp.Active_relation
              { project = phcp_project ();
                relation = Phcp.Pending_request phcp_c1 }
          ; phcp_choose ~communities:[] ()
          ])
    (* The accepted state is the one active-relation state that is no longer
       form-free: it carries the steward removal control, whose single
       framework field sits inside that one form. It still carries no
       application-owned hidden field. *)
  ; phcp_case "accepted state: one framework field inside the removal form"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (phcp_csrf_render
               (Phcp.Active_relation
                  { project = phcp_project ();
                    relation =
                      Phcp.Accepted_home
                        { community = phcp_c1; removal_allowed = true } }))
        in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
        Alcotest.(check int) "one framework field" 1
          (Html_assert.occurrences frag "name=\"dream.csrf\"");
        Alcotest.(check int) "no application hidden field" 0
          (Html_assert.occurrences frag "type='hidden'");
        Html_assert.must frag
          "action='/projects/widget-kit/community-home/alpine/remove'";
        match
          ( Html_assert.index_from frag "<form" 0,
            Html_assert.index_from frag "name=\"dream.csrf\"" 0,
            Html_assert.index_from frag "</form>" 0 )
        with
        | Some f, Some c, Some e ->
            Alcotest.(check bool) "CSRF inside the form" true (f < c && c < e)
        | _ -> Alcotest.fail "form or CSRF field missing")
  ; phcp_case "pure rendering without a request stays CSRF-free" (fun () ->
        Html_assert.must_not (phcp_frag (phcp_choose ())) "dream.csrf")
  ]

let suites =
    (* Home choice page: the one request form, active-relation states,
       defensive degradation, feedback, and framework CSRF. *)
  [ ("project_home_choice_page_form", phcp_form_cases)
  ; ("project_home_choice_page_states", phcp_state_cases)
  ; ("project_home_choice_page_defensive", phcp_defensive_cases)
  ; ("project_home_choice_page_feedback", phcp_feedback_cases)
  ; ("project_home_choice_page_csrf", phcp_csrf_cases)
  ]
