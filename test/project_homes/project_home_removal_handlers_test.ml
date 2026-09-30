module Ob = Earde.Project_onboarding
module Phr = Earde.Project_home_relation

(* ===== Accepted project-home removal HTTP integration
   (Project_home_removal_pages + Project_home_removal_handlers) =====
   The two removal surfaces end to end: the pure fragments, their
   composition into the steward home-choice page and the community settings
   surface, the shared rollout/authentication gates and POST security of
   both routes, and the database-gated removal flow. The database-gated
   cases use the opt-in EARDE_TEST_DATABASE_URL gate with their own reserved
   external-installation-id range 948000001..948000999 (hence account ids
   948100001..948100999, which also scope the permanent-project cleanup),
   phrh_% usernames, and phrh-% community slugs so no suite shares fixtures.
   Accepted relations are built only through production paths (the real
   request store plus the real review store), except the provisioned shape,
   which no store writes yet and is inserted in its exact production row
   form. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rp = Earde.Project_home_removal_pages

module Hd = Earde.Project_home_removal_handlers

module Cp = Earde.Project_home_choice_pages

module Rq = Earde.Project_home_request_store

module Rvs = Earde.Project_home_review_store

module Fin = Earde.Project_finalization_store

let case = Case.quick

let counting_loader = Http_fixture.counting_loader

let ok_loader = Http_fixture.ok_loader

let status_of = Http_fixture.status_of

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let make_project_side ~mode ~load_config =
  Hd.make_project_side_home_removal_handler ~mode ~load_config

let make_community_side ~mode ~load_config =
  Hd.make_community_side_home_removal_handler ~mode ~load_config

let project_side_pattern =
  "/projects/:project_slug/community-home/:community_slug/remove"

let community_side_pattern =
  "/c/:community_slug/projects/:project_slug/remove-home"

let project_side_target ~project ~community =
  Printf.sprintf "/projects/%s/community-home/%s/remove" project community

let community_side_target ~community ~project =
  Printf.sprintf "/c/%s/projects/%s/remove-home" community project

let request_home_target slug = Printf.sprintf "/projects/%s/request-home" slug

let settings_projects_target slug =
  Printf.sprintf "/c/%s/settings?panel=projects" slug

(* The association-only warning both surfaces must carry, word for word. *)
let warning_copy =
  "This removes the association only. It does not delete the project, \
   community, or community content."

(* ================= DB-free: the pure fragments ================= *)

(* A live request under a secret + sessions pipeline, so the framework
   CSRF field can be emitted; no SQL is touched. *)
let with_request render =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured := Some (render req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target:"/" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

let project_form ?request ?(removal_allowed = true) ~project ~community () =
  Rp.project_side_removal_form ?request ~removal_allowed
    ~project_slug:project ~community_slug:community ()

let live_project_form ~project ~community =
  with_request (fun req -> project_form ~request:req ~project ~community ())

(* The protected counterpart: an unpublished dedicated-community setup
   draft, whose provisioned home the removal store refuses to detach. *)
let live_protected_project_form ~project ~community =
  with_request (fun req ->
      project_form ~request:req ~removal_allowed:false ~project ~community ())

let section ?request ?(removal_allowed = true) ~community ~projects () =
  Rp.community_side_management_section ?request ~removal_allowed
    ~community_slug:community ~projects ()

let live_section ~community ~projects =
  with_request (fun req -> section ~request:req ~community ~projects ())

let live_protected_section ~community ~projects =
  with_request (fun req ->
      section ~request:req ~removal_allowed:false ~community ~projects ())

let project ?(name = "Phrh Project") ?(slug = "phrh-alpha")
    ?(namespace = "phrh-owner") ?(verification = Rp.Verified) () =
  ({ name; slug; namespace_login = namespace; verification }
    : Rp.connected_project)

(* Structural markers: a form's method and action, and the absence of any
   application-owned input. *)
let check_form_shape label html ~action ~count =
  Alcotest.(check int) (label ^ ": form count") count (Html_assert.occurrences html "<form");
  Alcotest.(check int)
    (label ^ ": POST method")
    count
    (Html_assert.occurrences html "method='POST'");
  Alcotest.(check int)
    (label ^ ": exact action")
    count
    (Html_assert.occurrences html (Printf.sprintf "action='%s'" action))

(* Beyond Dream's own hidden CSRF field, no input, select, or textarea may
   exist, and the submit control carries no name. *)
let check_zero_application_fields label html =
  Alcotest.(check int)
    (label ^ ": only the framework CSRF input")
    (Html_assert.occurrences html "name=\"dream.csrf\"")
    (Html_assert.occurrences html "<input");
  Alcotest.(check bool) (label ^ ": no select") false (Html_assert.contains html "<select");
  Alcotest.(check bool)
    (label ^ ": no textarea") false (Html_assert.contains html "<textarea");
  Alcotest.(check bool)
    (label ^ ": nameless submit") false
    (Html_assert.contains html "<button type='submit' name")

let check_inert label html =
  List.iter
    (fun needle ->
      Alcotest.(check bool) (label ^ ": no " ^ needle) false
        (Html_assert.contains html needle))
    [ "<script"; "javascript:"; "onclick"; "onsubmit"; "style="; "http-equiv" ]

let project_form_cases =
  [ case "removal fragment: the project-side form is one POST to the exact \
          removal route, with a CSRF field and zero application fields"
      (fun () ->
        let html = live_project_form ~project:"phrh-alpha" ~community:"phrh-home" in
        check_form_shape "project side" (Earde.Html.to_string html)
          ~action:"/projects/phrh-alpha/community-home/phrh-home/remove"
          ~count:1;
        Alcotest.(check bool) "framework CSRF field" true
          (Html_assert.contains (Earde.Html.to_string html) "name=\"dream.csrf\"");
        check_zero_application_fields "project side" (Earde.Html.to_string html);
        check_inert "project side" (Earde.Html.to_string html);
        Alcotest.(check bool) "heading" true
          (Html_assert.contains (Earde.Html.to_string html) "Remove community home");
        Alcotest.(check bool) "association-only warning" true
          (Html_assert.contains (Earde.Html.to_string html) warning_copy);
        Alcotest.(check bool) "submit copy" true (Html_assert.contains (Earde.Html.to_string html) "Remove home"))
  ; case "removal fragment: the project-side form never claims to delete \
          anything else" (fun () ->
        let html = live_project_form ~project:"phrh-alpha" ~community:"phrh-home" in
        List.iter
          (fun needle ->
            Alcotest.(check bool) ("no " ^ needle) false (Html_assert.contains (Earde.Html.to_string html) needle))
          [ "Delete project"; "Delete community"; "moderation"; "membership";
            "repositories"; "verification"; "Official" ])
  ; case "removal fragment: without a live request the project-side form is \
          not rendered, but the copy is" (fun () ->
        let html = project_form ~project:"phrh-alpha" ~community:"phrh-home" () in
        Alcotest.(check int) "no form" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check bool) "heading survives" true
          (Html_assert.contains (Earde.Html.to_string html) "Remove community home");
        Alcotest.(check bool) "warning survives" true
          (Html_assert.contains (Earde.Html.to_string html) warning_copy))
  ; case "removal fragment: a malformed project or community slug \
          suppresses the project-side form and leaks no route value"
      (fun () ->
        let bad_project =
          [ ""; "Phrh-Alpha"; "phrh_alpha"; "-phrh"; "phrh-"; "phrh/alpha";
            "phrh alpha"; String.make 81 'a' ]
        in
        List.iter
          (fun slug ->
            let html = live_project_form ~project:slug ~community:"phrh-home" in
            Alcotest.(check int) ("no form for project " ^ slug) 0
              (Html_assert.occurrences (Earde.Html.to_string html) "<form");
            Alcotest.(check bool) ("no action for project " ^ slug) false
              (Html_assert.contains (Earde.Html.to_string html) "action="))
          bad_project;
        List.iter
          (fun slug ->
            let html = live_project_form ~project:"phrh-alpha" ~community:slug in
            Alcotest.(check int) "no form for bad community" 0
              (Html_assert.occurrences (Earde.Html.to_string html) "<form");
            Alcotest.(check bool) "no action for bad community" false
              (Html_assert.contains (Earde.Html.to_string html) "action="))
          [ ""; "phrh/home"; "phrh home"; "phrh\thome"; "phrh\127home" ])
  ]

let section_cases =
  [ case "removal fragment: the settings section renders one exact POST per \
          accepted project, with zero application fields" (fun () ->
        let html =
          live_section ~community:"phrh-home"
            ~projects:
              [ project ~slug:"phrh-alpha" ~name:"Alpha" ();
                project ~slug:"phrh-beta" ~name:"Beta" () ]
        in
        Alcotest.(check int) "two forms" 2 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check int) "alpha action" 1
          (Html_assert.occurrences (Earde.Html.to_string html)
             "action='/c/phrh-home/projects/phrh-alpha/remove-home'");
        Alcotest.(check int) "beta action" 1
          (Html_assert.occurrences (Earde.Html.to_string html)
             "action='/c/phrh-home/projects/phrh-beta/remove-home'");
        Alcotest.(check int) "POST only" 2 (Html_assert.occurrences (Earde.Html.to_string html) "method='POST'");
        check_zero_application_fields "section" (Earde.Html.to_string html);
        check_inert "section" (Earde.Html.to_string html);
        Alcotest.(check bool) "heading" true
          (Html_assert.contains (Earde.Html.to_string html) "Connected projects");
        Alcotest.(check bool) "association-only warning" true
          (Html_assert.contains (Earde.Html.to_string html) warning_copy);
        Alcotest.(check int) "submit copy per project" 2
          (Html_assert.occurrences (Earde.Html.to_string html) ">Remove home<"))
  ; case "removal fragment: identity is public project identity only — \
          never workflow provenance or an internal id" (fun () ->
        let html =
          live_section ~community:"phrh-home"
            ~projects:
              [ project ~slug:"phrh-alpha" ~name:"Alpha"
                  ~namespace:"phrh-owner" () ]
        in
        Alcotest.(check bool) "name" true (Html_assert.contains (Earde.Html.to_string html) "Alpha");
        Alcotest.(check bool) "slug" true (Html_assert.contains (Earde.Html.to_string html) "phrh-alpha");
        Alcotest.(check bool) "namespace" true (Html_assert.contains (Earde.Html.to_string html) "phrh-owner");
        List.iter
          (fun needle ->
            Alcotest.(check bool) ("no " ^ needle) false (Html_assert.contains (Earde.Html.to_string html) needle))
          [ "Requested by"; "Reviewed"; "request_note"; "relation_id";
            "Accepted at"; "requester"; "reviewer" ])
  ; case "removal fragment: the three verification labels are exact and \
          never block removal" (fun () ->
        List.iter
          (fun (verification, copy) ->
            let html =
              live_section ~community:"phrh-home"
                ~projects:[ project ~verification () ]
            in
            Alcotest.(check bool) ("label " ^ copy) true (Html_assert.contains (Earde.Html.to_string html) copy);
            Alcotest.(check int) ("form still present for " ^ copy) 1
              (Html_assert.occurrences (Earde.Html.to_string html) "<form"))
          [ (Rp.Verified, "Verified through GitHub");
            (Rp.Stale, "Verification stale");
            (Rp.Revoked, "Verification revoked") ])
  ; case "removal fragment: no accepted projects renders restrained \
          settings copy and no form" (fun () ->
        let html = live_section ~community:"phrh-home" ~projects:[] in
        Alcotest.(check int) "no form" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check bool) "section still present" true
          (Html_assert.contains (Earde.Html.to_string html) "Connected projects");
        Alcotest.(check bool) "restrained copy" true
          (Html_assert.contains (Earde.Html.to_string html) "No connected projects.");
        (* Without a project there is nothing to warn about. *)
        Alcotest.(check bool) "no warning" false (Html_assert.contains (Earde.Html.to_string html) warning_copy))
  ]

let defensive_cases =
  [ case "removal fragment: a duplicate project slug leaves at most one \
          actionable row" (fun () ->
        let html =
          live_section ~community:"phrh-home"
            ~projects:
              [ project ~slug:"phrh-dup" ~name:"First" ();
                project ~slug:"phrh-dup" ~name:"Second" ();
                project ~slug:"phrh-dup" ~name:"Third" () ]
        in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check int) "one action" 1
          (Html_assert.occurrences (Earde.Html.to_string html)
             "action='/c/phrh-home/projects/phrh-dup/remove-home'");
        (* Every identity stays visible; only the duplicates lose their
           control. *)
        List.iter
          (fun n ->
            Alcotest.(check bool) ("identity " ^ n) true (Html_assert.contains (Earde.Html.to_string html) n))
          [ "First"; "Second"; "Third" ])
  ; case "removal fragment: a malformed project slug renders its identity \
          inert while its siblings stay actionable" (fun () ->
        let html =
          live_section ~community:"phrh-home"
            ~projects:
              [ project ~slug:"Phrh-Bad" ~name:"Bad" ();
                project ~slug:"phrh-good" ~name:"Good" () ]
        in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check int) "good action" 1
          (Html_assert.occurrences (Earde.Html.to_string html)
             "action='/c/phrh-home/projects/phrh-good/remove-home'");
        Alcotest.(check bool) "no action for the malformed slug" false
          (Html_assert.contains (Earde.Html.to_string html) "Phrh-Bad/remove-home");
        Alcotest.(check bool) "malformed identity still escaped-visible" true
          (Html_assert.contains (Earde.Html.to_string html) "Phrh-Bad"))
  ; case "removal fragment: an unaddressable community slug drops every \
          form on the section" (fun () ->
        List.iter
          (fun slug ->
            let html =
              live_section ~community:slug
                ~projects:
                  [ project ~slug:"phrh-alpha" (); project ~slug:"phrh-beta" () ]
            in
            Alcotest.(check int) "no form" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
            Alcotest.(check bool) "no action attribute" false
              (Html_assert.contains (Earde.Html.to_string html) "action="))
          [ ""; "phrh/home"; "phrh home"; "phrh\127home" ])
  ; case "removal fragment: a blank project name degrades to a generic safe \
          label" (fun () ->
        List.iter
          (fun blank ->
            let html =
              live_section ~community:"phrh-home"
                ~projects:[ project ~name:blank () ]
            in
            Alcotest.(check bool) "generic label" true
              (Html_assert.contains (Earde.Html.to_string html) "Open-source project");
            Alcotest.(check int) "form still rendered" 1
              (Html_assert.occurrences (Earde.Html.to_string html) "<form"))
          [ ""; "   "; "\t\n" ])
  ; case "removal fragment: caller text is escaped, never markup"
      (fun () ->
        let html =
          live_section ~community:"phrh-home"
            ~projects:
              [ project ~name:"<script>phrh_x()</script>"
                  ~namespace:"<b>phrh-ns</b>" () ]
        in
        Alcotest.(check bool) "escaped name" true
          (Html_assert.contains (Earde.Html.to_string html) "&lt;script&gt;phrh_x()&lt;/script&gt;");
        Alcotest.(check bool) "raw name absent" false
          (Html_assert.contains (Earde.Html.to_string html) "<script>phrh_x()</script>");
        Alcotest.(check bool) "escaped namespace" true
          (Html_assert.contains (Earde.Html.to_string html) "&lt;b&gt;phrh-ns&lt;/b&gt;");
        Alcotest.(check bool) "raw namespace absent" false
          (Html_assert.contains (Earde.Html.to_string html) "<b>phrh-ns</b>"))
  ]

let csrf_field_cases =
  [ case "removal fragment: without a live request no submittable form \
          exists on either surface" (fun () ->
        let p = project_form ~project:"phrh-alpha" ~community:"phrh-home" () in
        let s =
          section ~community:"phrh-home" ~projects:[ project () ] ()
        in
        Alcotest.(check int) "project side: no form" 0 (Html_assert.occurrences (Earde.Html.to_string p) "<form");
        Alcotest.(check int) "section: no form" 0 (Html_assert.occurrences (Earde.Html.to_string s) "<form");
        Alcotest.(check bool) "project side: no CSRF" false
          (Html_assert.contains (Earde.Html.to_string p) "dream.csrf");
        Alcotest.(check bool) "section: no CSRF" false
          (Html_assert.contains (Earde.Html.to_string s) "dream.csrf");
        (* The identity a moderator needs still renders. *)
        Alcotest.(check bool) "section identity survives" true
          (Html_assert.contains (Earde.Html.to_string s) "Phrh Project"))
  ; case "removal fragment: with a live request each actionable form \
          carries exactly one framework CSRF field" (fun () ->
        let s =
          live_section ~community:"phrh-home"
            ~projects:[ project ~slug:"phrh-a" (); project ~slug:"phrh-b" () ]
        in
        Alcotest.(check int) "two CSRF fields" 2
          (Html_assert.occurrences (Earde.Html.to_string s) "name=\"dream.csrf\""))
  ]

(* ============ DB-free: the protected-draft fragments ============

   The unpublished dedicated-community setup draft, whose provisioned home
   the removal store refuses to detach. Both fragments must render the
   structural reason and no control at all — and, crucially, no action path
   either, since an emitted action is the only thing a forged submission
   would need to lift. Assertions are structural counts and exact action
   strings, never whole-page comparisons, so the random CSRF bytes Dream
   mints on every render cannot make them flaky. *)

let draft_copy =
  "This project home is part of an unpublished community setup draft. \
   Complete setup and publish the community before detaching it."

let protected_cases =
  [ case "removal fragment: a protected draft renders no project-side form, \
          no action, and the draft-integrity reason" (fun () ->
        let html =
          live_protected_project_form ~project:"phrh-alpha"
            ~community:"phrh-home"
        in
        Alcotest.(check int) "no form" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check int) "no input of any kind" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<input");
        Alcotest.(check bool) "no action attribute" false
          (Html_assert.contains (Earde.Html.to_string html) "action=");
        (* The action path itself never appears, in any form. *)
        Alcotest.(check bool) "no removal route emitted" false
          (Html_assert.contains (Earde.Html.to_string html) "community-home");
        Alcotest.(check bool) "no CSRF field" false
          (Html_assert.contains (Earde.Html.to_string html) "dream.csrf");
        Alcotest.(check bool) "no submit control" false
          (Html_assert.contains (Earde.Html.to_string html) "Remove home");
        Alcotest.(check bool) "heading drops the action" false
          (Html_assert.contains (Earde.Html.to_string html) "Remove community home");
        Alcotest.(check bool) "neutral heading" true
          (Html_assert.contains (Earde.Html.to_string html) "Community home");
        Alcotest.(check bool) "draft-integrity copy" true
          (Html_assert.contains (Earde.Html.to_string html) draft_copy);
        check_inert "protected project side" (Earde.Html.to_string html))
  ; case "removal fragment: the protected project-side copy promises no \
          outcome and offers no destructive alternative" (fun () ->
        let html =
          live_protected_project_form ~project:"phrh-alpha"
            ~community:"phrh-home"
        in
        List.iter
          (fun needle ->
            Alcotest.(check bool) ("no " ^ needle) false
              (Html_assert.contains (Earde.Html.to_string html) needle))
          [ "Delete"; "Abandon"; "Discard"; "will be published";
            "guaranteed"; "private"; "draft community"; "top_mod";
            "administrator"; "steward" ];
        (* The association-only warning describes a control that is not
           offered here, so it must not appear. *)
        Alcotest.(check bool) "no association-only warning" false
          (Html_assert.contains (Earde.Html.to_string html) warning_copy))
  ; case "removal fragment: a protected draft keeps every connected \
          project's identity in settings but makes every row inert"
      (fun () ->
        let html =
          live_protected_section ~community:"phrh-home"
            ~projects:
              [ project ~slug:"phrh-a" ~name:"Phrh Alpha" ();
                project ~slug:"phrh-b" ~name:"Phrh Beta" () ]
        in
        Alcotest.(check int) "no form on any row" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check bool) "no action attribute" false
          (Html_assert.contains (Earde.Html.to_string html) "action=");
        Alcotest.(check bool) "no removal route emitted" false
          (Html_assert.contains (Earde.Html.to_string html) "remove-home");
        Alcotest.(check bool) "no CSRF field" false
          (Html_assert.contains (Earde.Html.to_string html) "dream.csrf");
        Alcotest.(check bool) "no submit control" false
          (Html_assert.contains (Earde.Html.to_string html) "Remove home");
        (* Identity survives for every project: a moderator still sees
           which projects the draft is connected to. *)
        Alcotest.(check bool) "first identity" true
          (Html_assert.contains (Earde.Html.to_string html) "Phrh Alpha");
        Alcotest.(check bool) "second identity" true
          (Html_assert.contains (Earde.Html.to_string html) "Phrh Beta");
        Alcotest.(check bool) "section title" true
          (Html_assert.contains (Earde.Html.to_string html) "Connected projects");
        Alcotest.(check bool) "draft-integrity copy" true
          (Html_assert.contains (Earde.Html.to_string html) draft_copy);
        Alcotest.(check bool) "association-only warning replaced" false
          (Html_assert.contains (Earde.Html.to_string html) warning_copy);
        check_inert "protected section" (Earde.Html.to_string html))
  ; case "removal fragment: the protected section still renders its \
          restrained empty state" (fun () ->
        let html = live_protected_section ~community:"phrh-home" ~projects:[] in
        Alcotest.(check int) "no form" 0 (Html_assert.occurrences (Earde.Html.to_string html) "<form");
        Alcotest.(check bool) "empty-state copy" true
          (Html_assert.contains (Earde.Html.to_string html) "No connected projects.");
        Alcotest.(check bool) "section still present" true
          (Html_assert.contains (Earde.Html.to_string html) "Connected projects"))
  ; case "removal fragment: removal_allowed:true is byte-for-byte the \
          existing behaviour on both surfaces" (fun () ->
        (* The protection parameter must be the only difference: with it
           set, both fragments are exactly what they were before. *)
        let p =
          live_project_form ~project:"phrh-alpha" ~community:"phrh-home"
        in
        check_form_shape "unprotected project side" (Earde.Html.to_string p)
          ~action:"/projects/phrh-alpha/community-home/phrh-home/remove"
          ~count:1;
        Alcotest.(check bool) "no draft copy" false (Html_assert.contains (Earde.Html.to_string p) draft_copy);
        let s =
          live_section ~community:"phrh-home"
            ~projects:[ project ~slug:"phrh-a" () ]
        in
        check_form_shape "unprotected section" (Earde.Html.to_string s)
          ~action:"/c/phrh-home/projects/phrh-a/remove-home" ~count:1;
        Alcotest.(check bool) "no draft copy in section" false
          (Html_assert.contains (Earde.Html.to_string s) draft_copy))
  ]

(* ============ DB-free: the steward home-choice page ============ *)

let cp_community ?(slug = "phrh-home") ?(visibility = Cp.Public) () =
  ({ id = 77; name = "Phrh Home"; slug; description = None; visibility }
    : Cp.community)

let cp_project ?(slug = "phrh-alpha") () =
  ({ name = "Phrh Project"; slug; namespace_login = "phrh-owner" }
    : Cp.project)

let choice_page state =
  with_request (fun req ->
      Cp.project_home_choice_page ~request:req ~state ~feedback:None ())

let accepted_state ?project_slug ?community_slug ?(removal_allowed = true) ()
    =
  Cp.Active_relation
    { project = cp_project ?slug:project_slug ();
      relation =
        Cp.Accepted_home
          { community = cp_community ?slug:community_slug ();
            removal_allowed } }

let removal_action = "/projects/phrh-alpha/community-home/phrh-home/remove"

let choice_page_cases =
  [ case "request-home page: only the accepted state carries the removal \
          form" (fun () ->
        let accepted = choice_page (accepted_state ()) in
        Alcotest.(check bool) "accepted: removal form" true
          (Html_assert.contains accepted (Printf.sprintf "action='%s'" removal_action));
        Alcotest.(check bool) "accepted: heading" true
          (Html_assert.contains accepted "Remove community home");
        Alcotest.(check bool) "accepted: warning" true
          (Html_assert.contains accepted warning_copy);
        let pending =
          choice_page
            (Cp.Active_relation
               { project = cp_project ();
                 relation = Cp.Pending_request (cp_community ()) })
        in
        Alcotest.(check bool) "pending: no removal form" false
          (Html_assert.contains pending "community-home");
        Alcotest.(check bool) "pending: no removal heading" false
          (Html_assert.contains pending "Remove community home");
        let chooser =
          choice_page
            (Cp.Choose_existing
               { project = cp_project ();
                 communities = [ cp_community () ];
                 request_note = "" })
        in
        Alcotest.(check bool) "chooser: no removal form" false
          (Html_assert.contains chooser "community-home");
        let none = choice_page (Cp.No_eligible_communities (cp_project ())) in
        Alcotest.(check bool) "no-communities: no removal form" false
          (Html_assert.contains none "community-home"))
  ; case "request-home page: the accepted removal form carries no hidden id \
          or slug field" (fun () ->
        let frag = Html_assert.panel_fragment (choice_page (accepted_state ())) in
        Alcotest.(check int) "one form on the accepted page" 1
          (Html_assert.occurrences frag "<form");
        let form = Html_assert.form_region frag in
        (* Both identities ride the route path structurally, which is why
           the form needs no field of its own. *)
        Alcotest.(check bool) "both slugs ride the form action" true
          (Html_assert.contains form (Printf.sprintf "action='%s'" removal_action));
        (* The form's only input is the framework CSRF field, carrying the
           framework's own attributes and nothing more. A hidden community
           id, project slug, or community slug would surface here either as
           a second input or as an extra attribute on this one. *)
        (match Html_assert.input_tags form with
        | [ tag ] ->
            Alcotest.(check bool) "the one input is the framework CSRF field"
              true (Html_assert.is_csrf_input tag)
        | tags ->
            Alcotest.failf "expected exactly one input, got %d"
              (List.length tags));
        (* The pages render their own attributes single-quoted and the
           framework renders double-quoted, so an application-owned field
           of any kind would show up in these two counts. *)
        Alcotest.(check int) "no application-owned named field" 0
          (Html_assert.occurrences form "name='");
        Alcotest.(check int) "no application-owned hidden field" 0
          (Html_assert.occurrences form "type='hidden'");
        (* Exactly one named field on the whole form, and it is the
           framework's. *)
        Alcotest.(check int) "one named field" 1 (Html_assert.occurrences form "name=\"");
        Alcotest.(check bool) "and it is the CSRF field" true
          (Html_assert.contains form "name=\"dream.csrf\"");
        (* Nothing outside that form carries a field either. *)
        Alcotest.(check int) "no input elsewhere on the page" 1
          (Html_assert.occurrences frag "<input"))
  ; case "request-home page: a malformed project or community slug \
          suppresses the removal form on the accepted page" (fun () ->
        let bad_project =
          choice_page (accepted_state ~project_slug:"Phrh-Bad" ())
        in
        Alcotest.(check bool) "no form for a bad project slug" false
          (Html_assert.contains bad_project "community-home");
        let bad_community =
          choice_page (accepted_state ~community_slug:"phrh/home" ())
        in
        Alcotest.(check bool) "no form for a bad community slug" false
          (Html_assert.contains bad_community "community-home");
        (* The accepted copy itself still renders in both. *)
        List.iter
          (fun html ->
            Alcotest.(check bool) "accepted copy survives" true
              (Html_assert.contains html "Community home connected"))
          [ bad_project; bad_community ])
  ; case "request-home page: a target that later became unavailable stays \
          removable" (fun () ->
        let html =
          choice_page
            (Cp.Active_relation
               { project = cp_project ();
                 relation =
                   Cp.Accepted_home
                     { community =
                         cp_community ~visibility:Cp.Currently_unavailable ();
                       removal_allowed = true } })
        in
        Alcotest.(check bool) "still actionable" true
          (Html_assert.contains html (Printf.sprintf "action='%s'" removal_action));
        Alcotest.(check bool) "generic availability marker" true
          (Html_assert.contains html "Currently unavailable"))
  ; case "request-home page: the existing chooser behaviour and copy are \
          unchanged" (fun () ->
        let html =
          choice_page
            (Cp.Choose_existing
               { project = cp_project ();
                 communities = [ cp_community () ];
                 request_note = "" })
        in
        let frag = Html_assert.panel_fragment html in
        Alcotest.(check bool) "request form action" true
          (Html_assert.contains frag "action='/projects/phrh-alpha/request-home'");
        Alcotest.(check bool) "request submit copy" true
          (Html_assert.contains frag "Send home request");
        Alcotest.(check bool) "radio input" true
          (Html_assert.contains frag "name='target_community_id'");
        Alcotest.(check bool) "note field" true
          (Html_assert.contains frag "name='request_note'");
        Alcotest.(check bool) "verification copy" true
          (Html_assert.contains frag "Project connected through GitHub"))
  ]

(* ========== DB-free: shared gates, origin, CSRF (both routes) ========== *)

(* Route pattern + target + factory, parametrized over both handlers so
   each gets identical coverage. *)
let routes =
  [ ("project side", make_project_side, project_side_pattern,
     project_side_target ~project:"phrh-any" ~community:"phrh-home")
  ; ("community side", make_community_side, community_side_pattern,
     community_side_target ~community:"phrh-home" ~project:"phrh-any")
  ]

let post_run ~make ~target ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target
    (make ~mode ~load_config)

let routed_post ~make ~pattern ~target ?session ?(headers = []) ~load_config ()
    =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target
    (Dream.router
       [ Dream.post pattern (fun req -> make ~mode:Ob.Public ~load_config req) ])

let gate_cases =
  List.concat_map
    (fun (label, make, pattern, target) ->
      [ case (label ^ " off: clean /bring redirect, loader untouched")
          (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            let response =
              Http_fixture.gate_response "off"
                (post_run ~make ~target ~session:Http_fixture.admin_session ~mode:Ob.Off
                   ~load_config:loader ())
            in
            Http_fixture.check_clean_redirect "off" "/bring" response;
            Alcotest.(check int) "loader never called" 0 !calls)
      ; case
          (label ^ ": anonymous and invalid sessions to /login, loader \
           untouched") (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            Http_fixture.check_clean_redirect "anonymous" "/login"
              (Http_fixture.gate_response "anonymous"
                 (post_run ~make ~target ~mode:Ob.Public ~load_config:loader ()));
            List.iter
              (fun raw ->
                Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
                  (Http_fixture.gate_response ("user_id " ^ raw)
                     (post_run ~make ~target
                        ~session:[ ("user_id", raw) ]
                        ~mode:Ob.Public ~load_config:loader ())))
              [ "not-a-number"; ""; "0"; "-3"; "  42" ];
            (* A session admin claim alone is not an identity. *)
            Http_fixture.check_clean_redirect "is_admin only" "/login"
              (Http_fixture.gate_response "is_admin only"
                 (post_run ~make ~target
                    ~session:[ ("is_admin", "true") ]
                    ~mode:Ob.Admins ~load_config:loader ()));
            Alcotest.(check int) "loader never called" 0 !calls)
      ; case
          (label ^ " admins mode: non-admin to /bring before route or \
           configuration") (fun () ->
            let loader, calls = counting_loader (ok_loader ()) in
            Http_fixture.check_clean_redirect "non-admin" "/bring"
              (Http_fixture.gate_response "non-admin"
                 (post_run ~make ~target ~session:Http_fixture.logged_in ~mode:Ob.Admins
                    ~load_config:loader ()));
            Alcotest.(check int) "loader never called" 0 !calls;
            (* Past the gates, the routerless harness has no route
               parameters: the defensive 404 answers before the
               configuration load, proving both that the gates passed and
               that no SQL ran (no sql_pool is installed). *)
            let continues label' session mode =
              let loader, calls = counting_loader (ok_loader ()) in
              let response =
                Http_fixture.gate_response label'
                  (post_run ~make ~target ~session ~mode ~load_config:loader ())
              in
              Alcotest.(check int) (label' ^ ": defensive 404") 404
                (status_of response);
              Alcotest.(check int) (label' ^ ": loader untouched") 0 !calls
            in
            continues "admin continues" Http_fixture.admin_session Ob.Admins;
            continues "public user continues" Http_fixture.logged_in Ob.Public)
      ; case (label ^ ": configuration error is a generic 503, no form")
          (fun () ->
            let loader, calls =
              counting_loader (Github_fixture.gac_of_values ~origin:None ())
            in
            let response =
              Http_fixture.gate_response "config failure"
                (routed_post ~make ~pattern ~target ~session:Http_fixture.logged_in
                   ~headers:
                     [ ("Origin", "https://earde.com");
                       ("Content-Type", "application/x-www-form-urlencoded");
                     ]
                   ~load_config:loader ())
            in
            Alcotest.(check int) "503" 503 (status_of response);
            Alcotest.(check int) "loader called once" 1 !calls;
            Alcotest.(check (option string)) "no-store" (Some "no-store")
              (Dream.header response "Cache-Control");
            let body = Lwt_main.run (Dream.body response) in
            List.iter
              (fun needle ->
                Alcotest.(check bool) ("no leak " ^ needle) false
                  (Html_assert.contains body needle))
              [ "EARDE_PUBLIC_ORIGIN"; "Missing"; "Invalid"; "public_origin" ])
      ; case (label ^ " origin gate: exact policy, before any form parse")
          (fun () ->
            let origin_run label' ?sec_fetch_site origin =
              let headers =
                (match origin with Some o -> [ ("Origin", o) ] | None -> [])
                @
                match sec_fetch_site with
                | Some v -> [ ("Sec-Fetch-Site", v) ]
                | None -> []
              in
              routed_post ~make ~pattern ~target ~session:Http_fixture.logged_in
                ~headers ~load_config:(fun () -> ok_loader ()) ()
              |> Http_fixture.gate_response label'
            in
            let rejected label' ?sec_fetch_site origin =
              Alcotest.(check int) (label' ^ ": 403") 403
                (status_of (origin_run label' ?sec_fetch_site origin))
            in
            (* Past the origin gate, the missing content type is the next
               rejection (400) — reaching it proves origin accepted, and
               that the form was not parsed before the check. *)
            let accepted label' ?sec_fetch_site origin =
              Alcotest.(check int) (label' ^ ": passes origin") 400
                (status_of (origin_run label' ?sec_fetch_site origin))
            in
            accepted "exact origin" (Some "https://earde.com");
            accepted "explicit default port" (Some "https://earde.com:443");
            accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
            rejected "cross-origin" (Some "https://evil.example");
            rejected "same-site subdomain" (Some "https://www.earde.com");
            rejected "wrong scheme" (Some "http://earde.com");
            rejected "null" (Some "null");
            rejected "mismatch beats metadata" ~sec_fetch_site:"same-origin"
              (Some "https://evil.example");
            rejected "no signals" None;
            rejected "cross-site" ~sec_fetch_site:"cross-site" None;
            rejected "same-site metadata" ~sec_fetch_site:"same-site" None;
            let leak = origin_run "reflection" (Some "https://evil.example") in
            Alcotest.(check bool) "origin not reflected" false
              (Html_assert.contains (Lwt_main.run (Dream.body leak)) "evil.example"))
      ])
    routes

let csrf_pipeline ~make ~pattern () =
  Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n" ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ ->
      Dream.router
        [ Dream.post pattern (fun r ->
              make ~mode:Ob.Public ~load_config:(fun () -> ok_loader ()) r) ]
        req

let csrf_post ~target ?cookie ?(content_type = true) pipeline fields =
  let headers =
    [ ("Origin", "https://earde.com") ]
    @ (if content_type then
         [ ("Content-Type", "application/x-www-form-urlencoded") ]
       else [])
    @ match cookie with Some c -> [ ("Cookie", c) ] | None -> []
  in
  match
    Lwt_main.run
      (pipeline
         (Dream.request ~method_:`POST ~target ~headers
            (Http_fixture.form_body fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  Alcotest.(check int) (label ^ ": status") expected
    (status_of (Http_fixture.gate_response label result))

let csrf_cases =
  List.map
    (fun (label, make, pattern, target) ->
      case
        (label ^ " CSRF: Dream verification gates every submission; only a \
         zero-field verified form reaches the store")
        (fun () ->
          let pipeline = csrf_pipeline ~make ~pattern () in
          let cookie, fresh, expired = Http_fixture.mint_tokens "mint" pipeline in
          let post = csrf_post ~target in
          csrf_rejected "missing token" 403 (post ~cookie pipeline []);
          csrf_rejected "invalid token" 403
            (post ~cookie pipeline [ ("dream.csrf", "not-a-token") ]);
          csrf_rejected "expired token" 403
            (post ~cookie pipeline [ ("dream.csrf", expired) ]);
          csrf_rejected "duplicate tokens" 403
            (post ~cookie pipeline
               [ ("dream.csrf", fresh); ("dream.csrf", fresh) ]);
          csrf_rejected "wrong session" 403
            (post pipeline [ ("dream.csrf", fresh) ]);
          csrf_rejected "wrong content type" 400
            (post ~cookie ~content_type:false pipeline
               [ ("dream.csrf", fresh) ]);
          (* Any application field at all is a generic 400 that never
             reaches the store — the route itself expresses removal. *)
          List.iter
            (fun (name, value) ->
              csrf_rejected ("unexpected field " ^ name) 400
                (post ~cookie pipeline
                   [ (name, value); ("dream.csrf", fresh) ]))
            [ ("relation_id", "1"); ("project_slug", "phrh-alpha");
              ("community_slug", "phrh-home"); ("decision", "remove");
              ("confirm", "yes"); ("return_url", "/"); ("unknown", "x") ];
          csrf_rejected "duplicate application field" 400
            (post ~cookie pipeline
               [ ("confirm", "yes"); ("confirm", "yes");
                 ("dream.csrf", fresh) ]);
          (* A verified token with no application field reaches the DB
             boundary: CSRF passed, Dream stripped its own field, and the
             store call is the next thing to run. *)
          Http_fixture.check_db_boundary "empty verified form continues"
            (post ~cookie pipeline [ ("dream.csrf", fresh) ])))
    routes

(* ================= Database-gated integration ================= *)

(* Distinctive credential-shaped fixtures. None may appear in any page,
   redirect, header, or cookie this feature produces. *)
let private_note = "phrh private note gho_PHRH_ACCESS_TOKEN_SECRET"

let refresh_marker = "ghr_PHRH_REFRESH_TOKEN"

let pkce_marker = "PHRH_PKCE_VERIFIER_VALUE"

let state_marker = "PHRH_OAUTH_STATE_VALUE"

let secret_marker = "PHRH_CLIENT_SECRET_VALUE"

let credential_markers =
  [ ("access token / private note", private_note);
    ("refresh token", refresh_marker);
    ("PKCE verifier", pkce_marker);
    ("OAuth state", state_marker);
    ("client secret", secret_marker);
    ("external installation id", "948000001");
    ("external account id", "948100001")
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 948100001 AND 948100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 948100001 AND 948100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 948000001 AND 948000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phrh-%'"
    ; "DELETE FROM users WHERE username LIKE 'phrh_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 948000001 AND 948000999"
    ]

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM community_projects WHERE id = $1"

let q_relation_id =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
  "SELECT id FROM community_projects \
   WHERE project_id = $1 AND community_id = $2"

let q_count_relations =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_projects WHERE project_id = $1"

let q_count_removed =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_projects \
   WHERE project_id = $1 AND status = 'removed'"

let q_project_exists =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM open_source_projects WHERE id = $1"

let q_steward_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_stewards WHERE project_id = $1"

let q_repo_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_repositories WHERE project_id = $1"

let q_community_sig =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT slug || '|' || name || '|' || visibility::text || '|' || \
          onboarding_state::text || '|' || is_network_community::text \
   FROM communities WHERE id = $1"

let q_moderator_sig =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(user_id::text || ':' || role, ',' \
            ORDER BY user_id), '<none>') \
   FROM community_moderators WHERE community_id = $1"

let q_member_count =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_members WHERE community_id = $1"

let q_set_verification = Connected_projects_fixture.q_set_verification

let q_corrupt_login = Connected_projects_fixture.q_corrupt_login

let q_hide_relations = Connected_projects_fixture.q_hide_relations

let q_show_relations = Connected_projects_fixture.q_show_relations

let q_restore_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE open_source_projects SET forge_namespace_login = 'pfin-owner' \
   WHERE id = $1"

let q_remove_steward = Home_request_fixture.q_delete_steward

(* A second steward for an existing project, reusing the project's own
   installation provenance so the NOT NULL reference stays coherent. *)
let q_add_steward =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "INSERT INTO project_stewards \
     (project_id, user_id, github_installation_record_id, role) \
   SELECT $1, $2, ps.github_installation_record_id, 'steward' \
   FROM project_stewards ps WHERE ps.project_id = $1 LIMIT 1"

let q_mark_removed = Home_request_fixture.q_mark_removed

(* An automatically provisioned accepted home: no requester, no reviewer,
   no note — exactly the shape the production status CHECK admits. No
   store writes this yet, so the row is built directly. *)
let q_provision_accepted =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, reviewed_at) \
   VALUES ($1, $2, 'home', 'accepted', NOW()) RETURNING id"

(* Test-only failure injection for the handler-level storage failure: an
   AFTER UPDATE trigger scoped to one reserved note value, installed only
   once the accepted fixture exists so the review store's own update is
   untouched. Installed and dropped inside that case alone under
   Lwt.finalize; production migrations are untouched. *)
let poison_note = "phrh poison marker"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION phrh_fail_update_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''phrh fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER phrh_fail_update
   AFTER UPDATE ON community_projects
   FOR EACH ROW WHEN (NEW.request_note = 'phrh poison marker')
   EXECUTE FUNCTION phrh_fail_update_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS phrh_fail_update ON community_projects"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS phrh_fail_update_fn()"

(* Durable corruption the removal store must reject as Inconsistent_data.
   An off-enum relation status is blocked by the production CHECK, so the
   probe uses a contradictory community publication pair instead — a
   genuinely representable durable inconsistency that needs no weakening
   of production constraints. *)
let q_mix_flags = Home_review_fixture.q_mix_flags

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url conn)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun ~url conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () -> Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f ~url conn))

(* === the real production pipeline shape ===

   One shared single-connection sql_pool for the whole suite: nothing ever
   closes a Dream.sql_pool, and this suite issues many requests across many
   identities, so a fresh pool per request exhausts Postgres
   max_connections. The session identity is swapped per request through a
   ref instead; cases run sequentially. Everything else is the real
   production shape — secret, memory sessions, and the real router paths
   bound to the real handlers, including the sibling accept/reject routes
   so the two removal patterns are proven to coexist with them. is_admin
   enters the session only when asked: durable admin authority is the
   users.is_admin column, never the session flag. *)
let shared_identity : (int * bool) option ref = ref None

let shared_pipeline = ref None

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline =
        Dream.sql_pool ~size:1 url @@ Dream.set_secret Github_fixture.cookie_secret
        @@ Dream.memory_sessions
        @@ (fun handler request ->
             match !shared_identity with
             | None -> handler request
             | Some (uid, is_admin) ->
                 let* () =
                   Dream.set_session_field request "user_id"
                     (string_of_int uid)
                 in
                 let* () =
                   if is_admin then
                     Dream.set_session_field request "is_admin" "true"
                   else Lwt.return_unit
                 in
                 handler request)
        @@ Dream.router
             [ Dream.get "/mint" (fun req ->
                   Dream.respond (Dream.csrf_token req));
               Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
               Dream.get "/c/:slug/network"
                 Earde.Community_handlers.community_network_handler;
               Dream.get "/c/:slug/settings"
                 Earde.Community_settings_handlers.community_settings_handler;
               Dream.get "/projects/:slug/request-home" (fun req ->
                   Earde.Project_home_request_handlers
                   .make_project_home_choice_handler ~mode:Ob.Public req);
               Dream.post "/c/:slug/projects/:project_slug/accept" (fun req ->
                   Earde.Project_home_review_handlers
                   .make_project_home_accept_handler ~mode:Ob.Public
                     ~load_config:(fun () -> ok_loader ())
                     req);
               Dream.post project_side_pattern (fun req ->
                   make_project_side ~mode:Ob.Public
                     ~load_config:(fun () -> ok_loader ())
                     req);
               Dream.post community_side_pattern (fun req ->
                   make_community_side ~mode:Ob.Public
                     ~load_config:(fun () -> ok_loader ())
                     req);
             ]
      in
      shared_pipeline := Some pipeline;
      pipeline

let as_user ?(admin_session = false) uid =
  shared_identity := Some (uid, admin_session)

let as_anonymous () = shared_identity := None

let do_get ?cookie ~url ~target () =
  let pipeline = pipeline_for ~url in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response =
    pipeline (Dream.request ~method_:`GET ~target ~headers "")
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

let do_post ?(origin = Some "https://earde.com") ~url ~cookie ~target ~token ()
    =
  let pipeline = pipeline_for ~url in
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [ ("Content-Type", "application/x-www-form-urlencoded");
        ("Cookie", cookie);
      ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target ~headers
       (Http_fixture.form_body [ ("dream.csrf", token) ]))

(* One GET that opens a surface: the page, its session cookie, and a CSRF
   token for the follow-up POST (tokens are session-bound, so one serves
   either route). *)
let open_surface label ~url ~target () =
  let* response, body = do_get ~url ~target () in
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  let cookie = Http_fixture.session_cookie label response in
  let token = Http_fixture.csrf_of_page label body in
  Lwt.return (cookie, token, body)

let check_clean_redirect_lwt label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string)) (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string)) (label ^ ": no-referrer")
    (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

let check_generic_404 label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool) (label ^ ": generic copy") true
    (Html_assert.contains body "This page does not exist.")

let check_generic_500 label response body =
  Alcotest.(check int) (label ^ ": 500") 500 (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  List.iter
    (fun needle ->
      Alcotest.(check bool) (label ^ ": no SQL detail " ^ needle) false
        (Html_assert.contains body needle))
    [ "community_projects"; "open_source_projects"; "Caqti"; "PostgreSQL";
      "SELECT"; "relation \"" ]

(* Nothing outside intentionally public project/community identity may
   appear in a body, a redirect Location, or a Set-Cookie header. *)
let check_no_credentials label response body =
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool) (label ^ ": body free of " ^ what) false
        (Html_assert.contains body needle);
      Alcotest.(check bool) (label ^ ": headers free of " ^ what) false
        (Html_assert.contains headers needle))
    credential_markers

(* === fixtures === *)

let make_project ?name ?(repos = [ "alpha" ]) conn ~user ~ext_id ~slug =
  let base = Int64.add ext_id 400000L in
  let* _inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        List.mapi
          (fun i n ->
            Project_fixture.repo ~account_id ~id:(Int64.add base (Int64.of_int i)) n)
          repos)
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let primary = List.nth ids 0 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary ids
  in
  let identity = Project_fixture.identity_exn ?name ~slug ~selected:ids ~primary () in
  let* created =
    Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity
  in
  Lwt.return (Fin.project_id created)

let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" Community_fixture.q_insert_moderator
    (user, community, "top_mod")

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, role)

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

let request_pending label conn ~user ~slug ~community ?(note = private_note) ()
    =
  let relation =
    Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:(Some note))
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: request fixture failed" label

let review label conn ~reviewer ~slug ~community_slug decision =
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community_slug ~decision
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: review fixture failed" label

(* An accepted home produced exactly as production produces one. *)
let accepted_home label conn ~owner ~reviewer ~slug ~cid ~community_slug ?note
    () =
  let* () = request_pending label conn ~user:owner ~slug ~community:cid ?note () in
  review label conn ~reviewer ~slug ~community_slug Rvs.Accept

let status_of_relation conn id = find conn "status" q_status id

let relation_id conn ~project ~community =
  find conn "relation id" q_relation_id (project, community)

let check_status label conn id expected =
  let* status = status_of_relation conn id in
  Alcotest.(check string) (label ^ ": relation status") expected status;
  Lwt.return_unit

(* Everything removal must leave untouched: the project row, its stewards
   and repositories, the community identity, its moderator roster, and its
   membership. *)
let durable_signature conn ~project ~community =
  let* projects = find conn "project exists" q_project_exists project in
  let* stewards = find conn "stewards" q_steward_count project in
  let* repos = find conn "repos" q_repo_count project in
  let* csig = find conn "community" q_community_sig community in
  let* msig = find conn "moderators" q_moderator_sig community in
  let* members = find conn "members" q_member_count community in
  Lwt.return
    (Printf.sprintf "%d|%d|%d|%s|%s|%d" projects stewards repos csig msig
       members)

let check_untouched label conn ~project ~community before =
  let* after = durable_signature conn ~project ~community in
  Alcotest.(check string)
    (label ^ ": project, community, stewardship, roles and membership \
      untouched")
    before after;
  Lwt.return_unit

(* === project-side success === *)

let project_side_success_case =
  db_case "project side: a steward's removal is a PRG to the permanent \
           request-home route, the relation becomes removed, and nothing \
           else changes" (fun ~url conn ->
      let* owner = insert_user conn "phrh_owner" in
      let* reviewer = insert_user conn "phrh_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000001L ~slug:"phrh-alpha"
          ~name:"Phrh Alpha"
      in
      let* cid =
        insert_community ~name:"Phrh Home" conn "phrh-home"
      in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer ~slug:"phrh-alpha" ~cid
          ~community_slug:"phrh-home" ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      let* before = durable_signature conn ~project ~community:cid in
      as_user owner;
      (* The accepted page carries the removal form and its CSRF token. *)
      let* cookie, token, body =
        open_surface "accepted page" ~url
          ~target:(request_home_target "phrh-alpha") ()
      in
      Alcotest.(check bool) "removal form present" true
        (Html_assert.contains body
           "action='/projects/phrh-alpha/community-home/phrh-home/remove'");
      Alcotest.(check bool) "warning present" true (Html_assert.contains body warning_copy);
      (* The private note never reaches the steward page. *)
      Alcotest.(check bool) "no private note" false (Html_assert.contains body private_note);
      let* response =
        do_post ~url ~cookie
          ~target:(project_side_target ~project:"phrh-alpha" ~community:"phrh-home")
          ~token ()
      in
      (* No query, no fragment, no result token. *)
      let* () =
        check_clean_redirect_lwt "removal" "/projects/phrh-alpha/request-home"
          response
      in
      check_no_credentials "removal redirect" response "";
      let* () = check_status "after removal" conn rid "removed" in
      (* The destination now shows the current no-active-home state. *)
      let* after_response, after_body =
        do_get ~cookie ~url ~target:(request_home_target "phrh-alpha") ()
      in
      Alcotest.(check int) "current state 200" 200 (status_of after_response);
      Alcotest.(check bool) "no removal form remains" false
        (Html_assert.contains after_body "community-home");
      Alcotest.(check bool) "chooser is back" true
        (Html_assert.contains after_body "action='/projects/phrh-alpha/request-home'");
      Alcotest.(check bool) "chooser lists the community" true
        (Html_assert.contains after_body "name='target_community_id'");
      check_untouched "project side" conn ~project ~community:cid before)

(* === community-side success === *)

let community_side_success_case =
  db_case "community side: a top moderator's removal is a PRG to the \
           canonical settings route and the project leaves both the \
           management section and the public page" (fun ~url conn ->
      let* owner = insert_user conn "phrh_cowner" in
      let* reviewer = insert_user conn "phrh_cmod" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000010L ~slug:"phrh-cproj"
          ~name:"Phrh Cproj"
      in
      let* cid = insert_community ~name:"Phrh Chome" conn "phrh-chome" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer ~slug:"phrh-cproj" ~cid
          ~community_slug:"phrh-chome" ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      let* before = durable_signature conn ~project ~community:cid in
      as_user reviewer;
      let* cookie, token, body =
        open_surface "settings" ~url
          ~target:(settings_projects_target "phrh-chome") ()
      in
      Alcotest.(check bool) "management section" true
        (Html_assert.contains body "Connected projects");
      Alcotest.(check bool) "removal form present" true
        (Html_assert.contains body
           "action='/c/phrh-chome/projects/phrh-cproj/remove-home'");
      Alcotest.(check bool) "project identity" true (Html_assert.contains body "Phrh Cproj");
      Alcotest.(check bool) "verification label" true
        (Html_assert.contains body "Verified through GitHub");
      (* No workflow provenance on the settings surface. *)
      Alcotest.(check bool) "no private note" false (Html_assert.contains body private_note);
      let* response =
        do_post ~url ~cookie
          ~target:
            (community_side_target ~community:"phrh-chome" ~project:"phrh-cproj")
          ~token ()
      in
      let* () =
        check_clean_redirect_lwt "removal"
          "/c/phrh-chome/settings?panel=projects" response
      in
      check_no_credentials "removal redirect" response "";
      let* () = check_status "after removal" conn rid "removed" in
      let* _, after_body =
        do_get ~cookie ~url ~target:(settings_projects_target "phrh-chome") ()
      in
      Alcotest.(check bool) "project gone from management" false
        (Html_assert.contains after_body "phrh-cproj/remove-home");
      Alcotest.(check bool) "empty-state copy" true
        (Html_assert.contains after_body "No connected projects.");
      (* The public Network page — where the list lives — also drops it, and
         the community home's count falls back to zero. *)
      let* public_response, public_body =
        do_get ~cookie ~url ~target:"/c/phrh-chome/network" ()
      in
      Alcotest.(check int) "network page 200" 200 (status_of public_response);
      Alcotest.(check bool) "no project list" false
        (Html_assert.contains public_body "<ul class='ccp-projects'>");
      Alcotest.(check bool) "quiet empty state instead" true
        (Html_assert.contains public_body "No connected projects yet.");
      Alcotest.(check bool) "project absent from the public page" false
        (Html_assert.contains public_body "Phrh Cproj");
      let* _, home_body = do_get ~cookie ~url ~target:"/c/phrh-chome" () in
      Alcotest.(check bool) "home count back to zero" true
        (Html_assert.contains home_body
           "<span class='launch-net-label'>Connected projects</span><span \
            class='launch-net-count'>0</span>");
      check_untouched "community side" conn ~project ~community:cid before)

(* === settings-surface authorization and content === *)

let settings_surface_case =
  db_case "community settings: only the top-mod/admin surface gains the \
           connected-project controls, and only accepted relations appear"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_sowner" in
      let* top = insert_user conn "phrh_stop" in
      let* admin = insert_user conn "phrh_sadmin" in
      let* modu = insert_user conn "phrh_smod" in
      let* legacy = insert_user conn "phrh_slegacy" in
      let* _accepted_project =
        make_project conn ~user:owner ~ext_id:948000020L ~slug:"phrh-sacc"
          ~name:"Phrh Accepted"
      in
      let* _pending_project =
        make_project conn ~user:owner ~ext_id:948000021L ~slug:"phrh-spend"
          ~name:"Phrh Pending"
      in
      let* _rejected_project =
        make_project conn ~user:owner ~ext_id:948000022L ~slug:"phrh-srej"
          ~name:"Phrh Rejected"
      in
      let* prov_project =
        make_project conn ~user:owner ~ext_id:948000023L ~slug:"phrh-sprov"
          ~name:"Phrh Provisioned"
      in
      let* removed_project =
        make_project conn ~user:owner ~ext_id:948000024L ~slug:"phrh-srem"
          ~name:"Phrh Removed"
      in
      let* cid = insert_community ~name:"Phrh Shome" conn "phrh-shome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* () = add_role conn ~user:modu ~community:cid "mod" in
      let* () = add_role conn ~user:legacy ~community:cid "legacy_mod" in
      (* One of each relation shape. *)
      let* () =
        accepted_home "accepted" conn ~owner ~reviewer:top ~slug:"phrh-sacc"
          ~cid ~community_slug:"phrh-shome" ()
      in
      let* () =
        request_pending "pending" conn ~user:owner ~slug:"phrh-spend"
          ~community:cid ()
      in
      let* () =
        request_pending "rejected" conn ~user:owner ~slug:"phrh-srej"
          ~community:cid ()
      in
      let* () =
        review "rejected" conn ~reviewer:top ~slug:"phrh-srej"
          ~community_slug:"phrh-shome" Rvs.Reject
      in
      let* _prov_rid =
        find conn "provision" q_provision_accepted (prov_project, cid)
      in
      let* () =
        accepted_home "to remove" conn ~owner ~reviewer:top ~slug:"phrh-srem"
          ~cid ~community_slug:"phrh-shome" ()
      in
      let* removed_rid =
        relation_id conn ~project:removed_project ~community:cid
      in
      let* () = exec conn "mark removed" q_mark_removed removed_rid in
      let visit label uid ~admin_session =
        as_user ~admin_session uid;
        let* response, body =
          do_get ~url ~target:(settings_projects_target "phrh-shome") ()
        in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        Lwt.return body
      in
      (* Top moderator and durable admin both get the panel. *)
      let* top_body = visit "top mod" top ~admin_session:false in
      Alcotest.(check bool) "top: nav entry" true
        (Html_assert.contains top_body "href='/c/phrh-shome/settings?panel=projects'");
      Alcotest.(check bool) "top: accepted reviewed home" true
        (Html_assert.contains top_body "phrh-sacc/remove-home");
      Alcotest.(check bool) "top: accepted provisioned home" true
        (Html_assert.contains top_body "phrh-sprov/remove-home");
      List.iter
        (fun slug ->
          Alcotest.(check bool) ("top: no control for " ^ slug) false
            (Html_assert.contains top_body (slug ^ "/remove-home")))
        [ "phrh-spend"; "phrh-srej"; "phrh-srem" ];
      Alcotest.(check bool) "top: no private note" false
        (Html_assert.contains top_body private_note);
      (* The existing settings surface is intact. *)
      Alcotest.(check bool) "top: settings still renders" true
        (Html_assert.contains top_body "Visibility &amp; discovery");
      Alcotest.(check bool) "top: queue link still there" true
        (Html_assert.contains top_body "href='/c/phrh-shome/project-home-requests'");
      (* The session is_admin flag is the settings surface's existing
         convention; durable authority is still the store's business. *)
      let* admin_body = visit "durable admin" admin ~admin_session:true in
      Alcotest.(check bool) "admin: management controls" true
        (Html_assert.contains admin_body "phrh-sacc/remove-home");
      (* Regular and legacy mods reach settings but never the panel. *)
      let* mod_body = visit "mod" modu ~admin_session:false in
      Alcotest.(check bool) "mod: no nav entry" false
        (Html_assert.contains mod_body "panel=projects");
      Alcotest.(check bool) "mod: no controls" false
        (Html_assert.contains mod_body "remove-home");
      Alcotest.(check bool) "mod: no section heading" false
        (Html_assert.contains mod_body "Connected projects");
      let* legacy_body = visit "legacy_mod" legacy ~admin_session:false in
      Alcotest.(check bool) "legacy: no controls" false
        (Html_assert.contains legacy_body "remove-home");
      (* An ordinary member and an unrelated user do not reach settings at
         all — the pre-existing gate is unchanged. *)
      let* member = insert_user conn "phrh_smember" in
      let* () = add_member conn ~user:member ~community:cid in
      let denied label uid =
        as_user uid;
        let* response, body =
          do_get ~url ~target:(settings_projects_target "phrh-shome") ()
        in
        Alcotest.(check int) (label ^ ": 403") 403 (status_of response);
        Alcotest.(check bool) (label ^ ": no controls") false
          (Html_assert.contains body "remove-home");
        Lwt.return_unit
      in
      let* () = denied "ordinary member" member in
      let* stranger = insert_user conn "phrh_sstranger" in
      let* () = denied "unrelated user" stranger in
      (* The public surfaces never carry removal controls. The list itself
         lives on the community's Network page now; the home only counts
         it. *)
      as_anonymous ();
      let* _, public_body = do_get ~url ~target:"/c/phrh-shome" () in
      Alcotest.(check bool) "public page: no controls" false
        (Html_assert.contains public_body "remove-home");
      let* _, network_body = do_get ~url ~target:"/c/phrh-shome/network" () in
      Alcotest.(check bool) "network page: no controls" false
        (Html_assert.contains network_body "remove-home");
      Alcotest.(check bool) "network page: still lists the projects" true
        (Html_assert.contains network_body "Phrh Accepted");
      Lwt.return_unit)

let settings_labels_case =
  db_case "community settings: drifted verification is labelled and stays \
           removable; an empty community gets restrained copy"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_lowner" in
      let* top = insert_user conn "phrh_ltop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000030L ~slug:"phrh-lproj"
          ~name:"Phrh Lproj"
      in
      let* cid = insert_community ~name:"Phrh Lhome" conn "phrh-lhome" in
      let* empty = insert_community ~name:"Phrh Empty" conn "phrh-lempty" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = add_top_mod conn ~user:top ~community:empty in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer:top ~slug:"phrh-lproj"
          ~cid ~community_slug:"phrh-lhome" ()
      in
      as_user top;
      let* _, empty_body =
        do_get ~url ~target:(settings_projects_target "phrh-lempty") ()
      in
      Alcotest.(check bool) "empty: restrained copy" true
        (Html_assert.contains empty_body "No connected projects.");
      Alcotest.(check bool) "empty: no form" false
        (Html_assert.contains empty_body "remove-home");
      let check_label label expected =
        let* _, body =
          do_get ~url ~target:(settings_projects_target "phrh-lhome") ()
        in
        Alcotest.(check bool) (label ^ ": label") true (Html_assert.contains body expected);
        Alcotest.(check bool) (label ^ ": still removable") true
          (Html_assert.contains body "phrh-lproj/remove-home");
        Lwt.return_unit
      in
      let* () = check_label "verified" "Verified through GitHub" in
      let* () = exec conn "stale" q_set_verification (project, "stale") in
      let* () = check_label "stale" "Verification stale" in
      let* () = exec conn "revoked" q_set_verification (project, "revoked") in
      check_label "revoked" "Verification revoked")

let settings_failure_case =
  db_case "community settings: a corrupt or unreadable connected-project \
           read is one generic non-cacheable 500, never a partial settings \
           page" (fun ~url conn ->
      let* owner = insert_user conn "phrh_fowner" in
      let* top = insert_user conn "phrh_ftop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000040L ~slug:"phrh-fproj"
          ~name:"Phrh Fproj"
      in
      let* cid = insert_community ~name:"Phrh Fhome" conn "phrh-fhome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer:top ~slug:"phrh-fproj"
          ~cid ~community_slug:"phrh-fhome" ()
      in
      as_user top;
      let* ok_response, ok_body =
        do_get ~url ~target:(settings_projects_target "phrh-fhome") ()
      in
      Alcotest.(check int) "before corruption 200" 200 (status_of ok_response);
      Alcotest.(check bool) "controls present" true
        (Html_assert.contains ok_body "phrh-fproj/remove-home");
      (* Durable corruption inside a connected project. *)
      let* () = exec conn "corrupt login" q_corrupt_login project in
      let* response, body =
        do_get ~url ~target:(settings_projects_target "phrh-fhome") ()
      in
      check_generic_500 "corrupted" response body;
      Alcotest.(check bool) "no partial settings page" false
        (Html_assert.contains body "Visibility &amp; discovery");
      check_no_credentials "corrupted" response body;
      (* A genuine storage failure in the same read. *)
      let* () = exec conn "restore login" q_restore_login project in
      let* () = exec conn "hide relations" q_hide_relations () in
      Lwt.finalize
        (fun () ->
          let* response, body =
            do_get ~url ~target:(settings_projects_target "phrh-fhome") ()
          in
          check_generic_500 "storage failure" response body;
          Lwt.return_unit)
        (fun () -> exec conn "restore relations" q_show_relations ()))

(* === authority variants and cross-surface authorization === *)

let authority_case =
  db_case "removal authority: every durable source may use either route, \
           and a multiply authorized actor still creates one removal"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_aowner" in
      let* second = insert_user conn "phrh_asecond" in
      let* top = insert_user conn "phrh_atop" in
      let* admin = insert_user conn "phrh_aadmin" in
      let* both = insert_user conn "phrh_aboth" in
      let* cid = insert_community ~name:"Phrh Ahome" conn "phrh-ahome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* () = add_top_mod conn ~user:both ~community:cid in
      let* () = set_admin conn ~user:both true in
      (* One project per actor, each with its own accepted home. *)
      let setup ~ext_id ~slug =
        let* project =
          make_project conn ~user:owner ~ext_id ~slug ~name:"Phrh Auth"
        in
        let* () =
          accepted_home slug conn ~owner ~reviewer:top ~slug ~cid
            ~community_slug:"phrh-ahome" ()
        in
        let* rid = relation_id conn ~project ~community:cid in
        Lwt.return (project, rid)
      in
      let* _p1, r1 = setup ~ext_id:948000050L ~slug:"phrh-a1" in
      let* p2, r2 = setup ~ext_id:948000051L ~slug:"phrh-a2" in
      let* _p3, r3 = setup ~ext_id:948000052L ~slug:"phrh-a3" in
      let* _p4, r4 = setup ~ext_id:948000053L ~slug:"phrh-a4" in
      let* _p5, r5 = setup ~ext_id:948000054L ~slug:"phrh-a5" in
      let* () = exec conn "second steward" q_add_steward (p2, second) in
      (* Removal by an actor over a given route, minting the CSRF token
         from the neutral /mint endpoint so the removal never depends on
         being able to render the emitting surface. *)
      let remove_via label uid ~target =
        as_user uid;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* response =
          do_post ~url ~cookie ~target ~token ()
        in
        Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
        Lwt.return_unit
      in
      (* Project steward and a second steward, on the project-shaped
         route. *)
      let* () =
        remove_via "steward" owner
          ~target:(project_side_target ~project:"phrh-a1" ~community:"phrh-ahome")
      in
      let* () = check_status "steward" conn r1 "removed" in
      let* () =
        remove_via "second steward" second
          ~target:(project_side_target ~project:"phrh-a2" ~community:"phrh-ahome")
      in
      let* () = check_status "second steward" conn r2 "removed" in
      (* Target top moderator and durable admin, on the community-shaped
         route. *)
      let* () =
        remove_via "top mod" top
          ~target:
            (community_side_target ~community:"phrh-ahome" ~project:"phrh-a3")
      in
      let* () = check_status "top mod" conn r3 "removed" in
      let* () =
        remove_via "durable admin" admin
          ~target:
            (community_side_target ~community:"phrh-ahome" ~project:"phrh-a4")
      in
      let* () = check_status "durable admin" conn r4 "removed" in
      (* A multiply authorized actor performs exactly one removal. *)
      let* () =
        remove_via "top mod and admin" both
          ~target:
            (community_side_target ~community:"phrh-ahome" ~project:"phrh-a5")
      in
      let* () = check_status "multiply authorized" conn r5 "removed" in
      let* removed = find conn "removed count" q_count_removed _p5 in
      Alcotest.(check int) "exactly one removed row" 1 removed;
      let* total = find conn "relation count" q_count_relations _p5 in
      Alcotest.(check int) "no new relation row" 1 total;
      Lwt.return_unit)

let cross_surface_case =
  db_case "removal routes: the route shape adds no second policy — a top \
           moderator may use the project-shaped route and a steward the \
           community-shaped one" (fun ~url conn ->
      let* owner = insert_user conn "phrh_xowner" in
      let* top = insert_user conn "phrh_xtop" in
      let* cid = insert_community ~name:"Phrh Xhome" conn "phrh-xhome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let setup ~ext_id ~slug =
        let* project =
          make_project conn ~user:owner ~ext_id ~slug ~name:"Phrh Cross"
        in
        let* () =
          accepted_home slug conn ~owner ~reviewer:top ~slug ~cid
            ~community_slug:"phrh-xhome" ()
        in
        let* rid = relation_id conn ~project ~community:cid in
        Lwt.return rid
      in
      let* r1 = setup ~ext_id:948000060L ~slug:"phrh-x1" in
      let* r2 = setup ~ext_id:948000061L ~slug:"phrh-x2" in
      let remove_via label uid ~target ~expected =
        as_user uid;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* response = do_post ~url ~cookie ~target ~token () in
        check_clean_redirect_lwt label expected response
      in
      (* A moderator on the project-shaped route. *)
      let* () =
        remove_via "top mod, project route" top
          ~target:(project_side_target ~project:"phrh-x1" ~community:"phrh-xhome")
          ~expected:"/projects/phrh-x1/request-home"
      in
      let* () = check_status "top mod via project route" conn r1 "removed" in
      (* A steward on the community-shaped route. *)
      let* () =
        remove_via "steward, community route" owner
          ~target:
            (community_side_target ~community:"phrh-xhome" ~project:"phrh-x2")
          ~expected:"/c/phrh-xhome/settings?panel=projects"
      in
      check_status "steward via community route" conn r2 "removed")

(* === unauthorized and missing === *)

let unauthorized_case =
  db_case "removal: every unauthorized identity and every missing or \
           mismatched target is the same generic 404, on both routes"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_uowner" in
      let* creator = insert_user conn "phrh_ucreator" in
      let* exsteward = insert_user conn "phrh_uex" in
      let* member = insert_user conn "phrh_umember" in
      let* modu = insert_user conn "phrh_umod" in
      let* legacy = insert_user conn "phrh_ulegacy" in
      let* othermod = insert_user conn "phrh_uother" in
      let* sonly = insert_user conn "phrh_usonly" in
      let* top = insert_user conn "phrh_utop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000070L ~slug:"phrh-uproj"
          ~name:"Phrh Uproj"
      in
      let* cid = insert_community ~name:"Phrh Uhome" conn "phrh-uhome" in
      let* other = insert_community ~name:"Phrh Uother" conn "phrh-uother" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = add_top_mod conn ~user:othermod ~community:other in
      let* () = add_member conn ~user:member ~community:cid in
      let* () = add_role conn ~user:modu ~community:cid "mod" in
      let* () = add_role conn ~user:legacy ~community:cid "legacy_mod" in
      let* () = exec conn "ex-steward" q_add_steward (project, exsteward) in
      let* () = exec conn "drop ex-steward" q_remove_steward (project, exsteward) in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer:top ~slug:"phrh-uproj"
          ~cid ~community_slug:"phrh-uhome" ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      let* before = durable_signature conn ~project ~community:cid in
      let denied label uid ~admin_session ~target =
        as_user ~admin_session uid;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* response = do_post ~url ~cookie ~target ~token () in
        let* body = Dream.body response in
        check_generic_404 label response body;
        check_no_credentials label response body;
        Lwt.return_unit
      in
      let both label uid ~admin_session =
        let* () =
          denied (label ^ " (project route)") uid ~admin_session
            ~target:
              (project_side_target ~project:"phrh-uproj" ~community:"phrh-uhome")
        in
        denied (label ^ " (community route)") uid ~admin_session
          ~target:
            (community_side_target ~community:"phrh-uhome" ~project:"phrh-uproj")
      in
      let* () = both "project creator without stewardship" creator ~admin_session:false in
      let* () = both "removed steward" exsteward ~admin_session:false in
      let* () = both "ordinary member" member ~admin_session:false in
      let* () = both "mod role" modu ~admin_session:false in
      let* () = both "legacy_mod role" legacy ~admin_session:false in
      let* () = both "other-community moderator" othermod ~admin_session:false in
      (* A session admin claim with no durable users.is_admin backing. *)
      let* () = both "session-only admin" sonly ~admin_session:true in
      (* The relation survived every attempt. *)
      let* () = check_status "still accepted" conn rid "accepted" in
      (* Missing project, missing community, and a wrong pair collapse
         identically for an actor who is otherwise fully authorized. *)
      let* () =
        denied "missing project" owner ~admin_session:false
          ~target:
            (project_side_target ~project:"phrh-absent" ~community:"phrh-uhome")
      in
      let* () =
        denied "missing community" owner ~admin_session:false
          ~target:
            (project_side_target ~project:"phrh-uproj" ~community:"phrh-absent")
      in
      (* A wrong project/community pair is the generic 404 for anyone
         without authority over it, exactly like every other unavailable
         state. *)
      let* () =
        denied "wrong pair, unauthorized actor" member ~admin_session:false
          ~target:
            (project_side_target ~project:"phrh-uproj" ~community:"phrh-uother")
      in
      (* A malformed slug is the same generic 404, never reflected. *)
      let* () =
        denied "malformed project slug" owner ~admin_session:false
          ~target:
            (project_side_target ~project:"Phrh-Bad" ~community:"phrh-uhome")
      in
      (* For an actor who does hold authority over the pair, a wrong pair
         reaches the relation lookup and finds nothing — the store's
         Removal_unavailable, which the handler answers with the same safe
         303 current-state destination as success. No relation is touched
         and nothing distinguishes it from a replay. *)
      as_user owner;
      let* mint_response, mint = do_get ~url ~target:"/mint" () in
      let cookie = Http_fixture.session_cookie "authorized wrong pair" mint_response in
      let* response =
        do_post ~url ~cookie
          ~target:
            (project_side_target ~project:"phrh-uproj" ~community:"phrh-uother")
          ~token:mint ()
      in
      let* () =
        check_clean_redirect_lwt "authorized wrong pair"
          "/projects/phrh-uproj/request-home" response
      in
      let* () = check_status "still accepted after probing" conn rid "accepted" in
      check_untouched "unauthorized" conn ~project ~community:cid before)

(* === replay and concurrency === *)

let replay_case =
  db_case "removal: a replayed or stale form is the same 303 current-state \
           destination, with exactly one durable removal" (fun ~url conn ->
      let* owner = insert_user conn "phrh_rowner" in
      let* top = insert_user conn "phrh_rtop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000080L ~slug:"phrh-rproj"
          ~name:"Phrh Rproj"
      in
      let* cid = insert_community ~name:"Phrh Rhome" conn "phrh-rhome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer:top ~slug:"phrh-rproj"
          ~cid ~community_slug:"phrh-rhome" ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      as_user owner;
      let* cookie, token, _ =
        open_surface "accepted page" ~url
          ~target:(request_home_target "phrh-rproj") ()
      in
      let target =
        project_side_target ~project:"phrh-rproj" ~community:"phrh-rhome"
      in
      let* first = do_post ~url ~cookie ~target ~token () in
      let* () =
        check_clean_redirect_lwt "first" "/projects/phrh-rproj/request-home"
          first
      in
      (* The same stale form replayed: same destination, no error oracle,
         no second mutation. *)
      let* second = do_post ~url ~cookie ~target ~token () in
      let* () =
        check_clean_redirect_lwt "replay" "/projects/phrh-rproj/request-home"
          second
      in
      let* () = check_status "still removed" conn rid "removed" in
      let* removed = find conn "removed count" q_count_removed project in
      Alcotest.(check int) "exactly one removal" 1 removed;
      let* total = find conn "relation count" q_count_relations project in
      Alcotest.(check int) "no new relation row" 1 total;
      (* A replay from the other surface behaves identically. *)
      as_user top;
      let* response, mint = do_get ~url ~target:"/mint" () in
      let cookie = Http_fixture.session_cookie "top mint" response in
      let* third =
        do_post ~url ~cookie
          ~target:
            (community_side_target ~community:"phrh-rhome" ~project:"phrh-rproj")
          ~token:mint ()
      in
      let* () =
        check_clean_redirect_lwt "cross-surface replay"
          "/c/phrh-rhome/settings?panel=projects" third
      in
      let* removed = find conn "removed count" q_count_removed project in
      Alcotest.(check int) "still exactly one removal" 1 removed;
      Lwt.return_unit)

let concurrency_case =
  db_case "removal: two concurrent removals commit exactly one; the loser \
           receives the same 303 destination" (fun ~url conn ->
      let* owner = insert_user conn "phrh_kowner" in
      let* top = insert_user conn "phrh_ktop" in
      let* admin = insert_user conn "phrh_kadmin" in
      let* cid = insert_community ~name:"Phrh Khome" conn "phrh-khome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = set_admin conn ~user:admin true in
      (* A second connection holds the winning removal's transaction open
         while the HTTP loser is launched, so the outcome is deterministic
         without sleeps. *)
      let url_of () =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some u -> u
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let race label ~ext_id ~slug ~loser ~target ~expected =
        let* project =
          make_project conn ~user:owner ~ext_id ~slug ~name:"Phrh Race"
        in
        let* () =
          accepted_home slug conn ~owner ~reviewer:top ~slug ~cid
            ~community_slug:"phrh-khome" ()
        in
        let* rid = relation_id conn ~project ~community:cid in
        as_user loser;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* conn2 = Caqti_lwt_unix.connect (Uri.of_string (url_of ())) in
        let* conn2 = or_fail "second connect" conn2 in
        let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
        Lwt.finalize
          (fun () ->
            let* r = C2.start () in
            let* () = or_fail "second begin" r in
            (* The winner removes on the second connection, holding its
               locks. *)
            let* winner =
              Earde.Project_home_removal_store.remove conn2
                ~actor_user_id:owner ~project_slug:slug
                ~community_slug:"phrh-khome"
            in
            (match winner with
             | Ok _ -> ()
             | Error _ -> Alcotest.failf "%s: winner failed" label);
            let loser_promise = do_post ~url ~cookie ~target ~token () in
            let* r = C2.commit () in
            let* () = or_fail "second commit" r in
            let* response = loser_promise in
            let* () = check_clean_redirect_lwt label expected response in
            let* () = check_status label conn rid "removed" in
            let* removed = find conn "removed count" q_count_removed project in
            Alcotest.(check int) (label ^ ": exactly one removal") 1 removed;
            Lwt.return_unit)
          (fun () -> C2.disconnect ())
      in
      (* Two project-side removals. *)
      let* () =
        race "project vs project" ~ext_id:948000090L ~slug:"phrh-k1"
          ~loser:owner
          ~target:
            (project_side_target ~project:"phrh-k1" ~community:"phrh-khome")
          ~expected:"/projects/phrh-k1/request-home"
      in
      (* Project-side winner versus community-side loser. *)
      let* () =
        race "project vs community" ~ext_id:948000091L ~slug:"phrh-k2"
          ~loser:top
          ~target:
            (community_side_target ~community:"phrh-khome" ~project:"phrh-k2")
          ~expected:"/c/phrh-khome/settings?panel=projects"
      in
      (* Top moderator versus durable admin. *)
      race "top mod vs admin" ~ext_id:948000092L ~slug:"phrh-k3" ~loser:admin
        ~target:
          (community_side_target ~community:"phrh-khome" ~project:"phrh-k3")
        ~expected:"/c/phrh-khome/settings?panel=projects")

(* === lifecycle drift === *)

let drift_case =
  db_case_lifecycle_relaxed "removal: both surfaces still remove across project verification \
           drift and community lifecycle drift" (fun ~url conn ->
      let* owner = insert_user conn "phrh_downer" in
      let* top = insert_user conn "phrh_dtop" in
      let setup ~ext_id ~slug ~cid ~community_slug =
        let* project =
          make_project conn ~user:owner ~ext_id ~slug ~name:"Phrh Drift"
        in
        let* () =
          accepted_home slug conn ~owner ~reviewer:top ~slug ~cid
            ~community_slug ()
        in
        let* rid = relation_id conn ~project ~community:cid in
        Lwt.return (project, rid)
      in
      let remove_via label uid ~target ~expected =
        as_user uid;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* response = do_post ~url ~cookie ~target ~token () in
        check_clean_redirect_lwt label expected response
      in
      (* A stale and a revoked project, one per surface. *)
      let* cid = insert_community ~name:"Phrh Dhome" conn "phrh-dhome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* p1, r1 =
        setup ~ext_id:948000100L ~slug:"phrh-d1" ~cid
          ~community_slug:"phrh-dhome"
      in
      let* p2, r2 =
        setup ~ext_id:948000101L ~slug:"phrh-d2" ~cid
          ~community_slug:"phrh-dhome"
      in
      let* () = exec conn "stale" q_set_verification (p1, "stale") in
      let* () = exec conn "revoked" q_set_verification (p2, "revoked") in
      let* () =
        remove_via "stale project" owner
          ~target:(project_side_target ~project:"phrh-d1" ~community:"phrh-dhome")
          ~expected:"/projects/phrh-d1/request-home"
      in
      let* () = check_status "stale project removed" conn r1 "removed" in
      let* () =
        remove_via "revoked project" top
          ~target:
            (community_side_target ~community:"phrh-dhome" ~project:"phrh-d2")
          ~expected:"/c/phrh-dhome/settings?panel=projects"
      in
      let* () = check_status "revoked project removed" conn r2 "removed" in
      (* A private, a draft, and a legacy community. *)
      let drifted label ~ext_id ~slug ~community_slug ~drift ~uid ~target
          ~expected =
        let* cid' = insert_community ~name:label conn community_slug in
        let* () = add_top_mod conn ~user:top ~community:cid' in
        let* _p, rid = setup ~ext_id ~slug ~cid:cid' ~community_slug in
        let* () = drift cid' in
        let* () = remove_via label uid ~target ~expected in
        check_status label conn rid "removed"
      in
      let* () =
        drifted "private community" ~ext_id:948000110L ~slug:"phrh-d3"
          ~community_slug:"phrh-dpriv"
          ~drift:(fun c -> exec conn "private" Community_fixture.q_make_private c)
          ~uid:owner
          ~target:(project_side_target ~project:"phrh-d3" ~community:"phrh-dpriv")
          ~expected:"/projects/phrh-d3/request-home"
      in
      (* Drift into the exact unpublished network setup draft is the one
         lifecycle that is not ordinary drift. A dedicated draft's home is
         auto-provisioned with no requester, reviewer or note, so a
         moderator-reviewed accepted home under that lifecycle is
         contradictory provenance: the store answers Inconsistent_data and
         the handler turns that into its existing generic 500, never a
         redirect that names a lifecycle. Nothing is mutated. *)
      let* () =
        let* cid' = insert_community ~name:"Phrh Ddraft" conn "phrh-ddraft" in
        let* () = add_top_mod conn ~user:top ~community:cid' in
        let* _p, rid =
          setup ~ext_id:948000111L ~slug:"phrh-d4" ~cid:cid'
            ~community_slug:"phrh-ddraft"
        in
        let* () = exec conn "draft" Community_fixture.q_make_draft_state cid' in
        as_user top;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie "draft community" response in
        let* response =
          do_post ~url ~cookie
            ~target:
              (community_side_target ~community:"phrh-ddraft"
                 ~project:"phrh-d4")
            ~token ()
        in
        let* body = Dream.body response in
        check_generic_500 "draft community" response body;
        check_status "draft community: relation untouched" conn rid
          "accepted"
      in
      drifted "legacy community" ~ext_id:948000112L ~slug:"phrh-d5"
        ~community_slug:"phrh-dlegacy"
        ~drift:(fun c -> exec conn "legacy" Community_fixture.q_make_legacy c)
        ~uid:owner
        ~target:
          (project_side_target ~project:"phrh-d5" ~community:"phrh-dlegacy")
        ~expected:"/projects/phrh-d5/request-home")

(* === durable failures === *)

let store_failure_case =
  db_case_lifecycle_relaxed "removal: a store storage failure or durable inconsistency is one \
           generic non-cacheable 500 with no partial mutation"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_gowner" in
      let* top = insert_user conn "phrh_gtop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000120L ~slug:"phrh-gproj"
          ~name:"Phrh Gproj"
      in
      let* other =
        make_project conn ~user:owner ~ext_id:948000121L ~slug:"phrh-gother"
          ~name:"Phrh Gother"
      in
      let* cid = insert_community ~name:"Phrh Ghome" conn "phrh-ghome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      (* The poisoned relation carries the reserved note the trigger keys
         on; the trigger is installed only after the review store already
         accepted it. *)
      let* () =
        accepted_home "poisoned" conn ~owner ~reviewer:top ~slug:"phrh-gproj"
          ~cid ~community_slug:"phrh-ghome" ~note:poison_note ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      let* () =
        accepted_home "corrupt" conn ~owner ~reviewer:top ~slug:"phrh-gother"
          ~cid ~community_slug:"phrh-ghome" ()
      in
      let* other_rid = relation_id conn ~project:other ~community:cid in
      let post label ~target =
        as_user owner;
        let* response, token = do_get ~url ~target:"/mint" () in
        let cookie = Http_fixture.session_cookie label response in
        let* response = do_post ~url ~cookie ~target ~token () in
        let* body = Dream.body response in
        check_generic_500 label response body;
        check_no_credentials label response body;
        Lwt.return_unit
      in
      let* () = exec conn "create fn" q_create_fail_fn () in
      let* () =
        Lwt.finalize
        (fun () ->
          let* () = exec conn "create trigger" q_create_fail_trigger () in
          let* () =
            post "storage failure"
              ~target:
                (project_side_target ~project:"phrh-gproj"
                   ~community:"phrh-ghome")
          in
          (* Complete rollback: the relation is still accepted. *)
          check_status "rolled back" conn rid "accepted")
        (fun () ->
          let* () = exec conn "drop trigger" q_drop_fail_trigger () in
          exec conn "drop fn" q_drop_fail_fn ())
      in
      (* Durable inconsistency: contradictory publication flags on the
         locked community. *)
      let* () = exec conn "mix flags" q_mix_flags cid in
      let* () =
        post "inconsistent data"
          ~target:
            (community_side_target ~community:"phrh-ghome"
               ~project:"phrh-gother")
      in
      (* Nothing was mutated on the way to the generic 500. *)
      let* () = check_status "still accepted" conn other_rid "accepted" in
      exec conn "restore eligible" Home_review_fixture.q_make_eligible cid)

(* === privacy sweep === *)

let privacy_case =
  db_case "removal: no credential-shaped fixture, private note, or internal \
           id appears in any form, redirect, error, or cookie"
    (fun ~url conn ->
      let* owner = insert_user conn "phrh_powner" in
      let* top = insert_user conn "phrh_ptop" in
      let* project =
        make_project conn ~user:owner ~ext_id:948000130L ~slug:"phrh-pproj"
          ~name:"Phrh Pproj"
      in
      let* cid = insert_community ~name:"Phrh Phome" conn "phrh-phome" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () =
        accepted_home "fixture" conn ~owner ~reviewer:top ~slug:"phrh-pproj"
          ~cid ~community_slug:"phrh-phome" ()
      in
      let* rid = relation_id conn ~project ~community:cid in
      (* Internal identifiers are swept out of everything the browser can
         keep: the redirect Location and every response header (cookies
         included). Response bodies are swept for the credential-shaped
         fixtures and the private workflow vocabulary instead — a short
         serial id would false-positive inside an opaque CSRF token. *)
      let internal_ids =
        [ ("relation id", Int64.to_string rid);
          ("project id", Int64.to_string project);
          ("community id", string_of_int cid);
          ("steward id", string_of_int owner);
          ("moderator id", string_of_int top)
        ]
      in
      let header_text response =
        String.concat "\n"
          (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
      in
      let sweep_headers label response =
        let headers = header_text response in
        List.iter
          (fun (what, needle) ->
            Alcotest.(check bool) (label ^ ": headers free of " ^ what) false
              (Html_assert.contains headers needle))
          internal_ids;
        List.iter
          (fun (what, needle) ->
            Alcotest.(check bool) (label ^ ": headers free of " ^ what) false
              (Html_assert.contains headers needle))
          credential_markers
      in
      let sweep_body label body =
        List.iter
          (fun (what, needle) ->
            Alcotest.(check bool) (label ^ ": body free of " ^ what) false
              (Html_assert.contains body needle))
          credential_markers;
        List.iter
          (fun needle ->
            Alcotest.(check bool) (label ^ ": body free of " ^ needle) false
              (Html_assert.contains body needle))
          [ "requested_by"; "reviewed_by"; "request_note"; "relation_id";
            "top_mod"; "is_admin"; "Requested by"; "Reviewed by" ]
      in
      (* The steward form. *)
      as_user owner;
      let* page_response, page_body =
        do_get ~url ~target:(request_home_target "phrh-pproj") ()
      in
      Alcotest.(check int) "steward page 200" 200 (status_of page_response);
      sweep_body "steward page" page_body;
      sweep_headers "steward page" page_response;
      let cookie = Http_fixture.session_cookie "steward page" page_response in
      let token = Http_fixture.csrf_of_page "steward page" page_body in
      (* The settings management section. *)
      as_user top;
      let* settings_response, settings_body =
        do_get ~url ~target:(settings_projects_target "phrh-phome") ()
      in
      Alcotest.(check int) "settings 200" 200 (status_of settings_response);
      sweep_body "settings section" settings_body;
      sweep_headers "settings section" settings_response;
      (* The redirect itself: an exact server-built Location, nothing else. *)
      as_user owner;
      let* response =
        do_post ~url ~cookie
          ~target:
            (project_side_target ~project:"phrh-pproj" ~community:"phrh-phome")
          ~token ()
      in
      let* redirect_body = Dream.body response in
      Alcotest.(check (option string)) "exact Location"
        (Some "/projects/phrh-pproj/request-home")
        (Dream.header response "Location");
      Alcotest.(check string) "empty redirect body" "" redirect_body;
      sweep_headers "removal redirect" response;
      (* And an error path. *)
      let* stranger = insert_user conn "phrh_pstranger" in
      as_user stranger;
      let* mint_response, mint = do_get ~url ~target:"/mint" () in
      let cookie = Http_fixture.session_cookie "stranger mint" mint_response in
      let* response =
        do_post ~url ~cookie
          ~target:
            (project_side_target ~project:"phrh-pproj" ~community:"phrh-phome")
          ~token:mint ()
      in
      let* body = Dream.body response in
      check_generic_404 "stranger" response body;
      sweep_body "stranger 404" body;
      sweep_headers "stranger 404" response;
      Lwt.return_unit)

let page_suite =
  project_form_cases @ section_cases @ defensive_cases @ csrf_field_cases
  @ protected_cases

let db_suite =
  [ project_side_success_case; community_side_success_case;
    settings_surface_case; settings_labels_case; settings_failure_case;
    authority_case; cross_surface_case; unauthorized_case; replay_case;
    concurrency_case; drift_case; store_failure_case; privacy_case ]

let suites =
    (* Accepted project-home removal HTTP integration: the pure removal
       fragments (exact actions, zero application fields, CSRF only with a
       live request, defensive degradation, verification copy, escaping),
       their composition into the steward home-choice page, the shared
       rollout/authentication gates, origin and CSRF contracts of both
       POST routes, and the database-gated flow — both surfaces' success
       and redirect, settings authorization and content, authority
       variants, cross-surface authorization, unauthorized/missing
       collapse, replay and concurrency, lifecycle drift, durable failure,
       and the privacy sweep. *)
  [ ("project_home_removal_pages", page_suite)
  ; ("project_home_removal_choice_page", choice_page_cases)
  ; ("project_home_removal_post_gates", gate_cases)
  ; ("project_home_removal_post_csrf", csrf_cases)
  ; ("project_home_removal_handlers_db", db_suite)
  ]
