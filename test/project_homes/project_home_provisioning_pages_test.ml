module Pi = Earde.Project_identity
module Phvp = Earde.Project_home_provisioning_pages

(* === Dedicated-home creation page (Project_home_provisioning_pages) ===
   Pure rendering: the exact form contract, the setup-draft and publication
   copy, defensive degradation, escaping, and the absence of officiality
   language, hidden identifiers, and script. DB-free. *)

let phvp_case = Case.quick

let phvp_project ?(name = "Phvp Project") ?(slug = "phvp-project") ?description
    ?(kind = Pi.Project) ?(login = "phvp-owner") () : Phvp.project =
  { Phvp.name; slug; description; kind; namespace_login = login }

let phvp_values ?(name = "") ?(slug = "") ?(description = "") () :
    Phvp.form_values =
  { Phvp.community_name = name;
    community_slug = slug;
    community_description = description
  }

(* Assertions run over the feature fragment, not the whole page: the shared
   shell owns its own forms and chrome, and this suite is about what the
   provisioning surface itself renders. *)
let phvp_render ?request ?(project = phvp_project ())
    ?(values = phvp_values ()) ?feedback () =
  Html_assert.panel_fragment
    (Phvp.project_home_provisioning_page ?request ~project ~values ~feedback ())

(* A live request under a secret + sessions pipeline, so the framework CSRF
   field can be emitted; no SQL is touched. *)
let phvp_live ?project ?values ?feedback () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured := Some (phvp_render ~request:req ?project ?values ?feedback ());
    Dream.html ""
  in
  ignore (Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target:"/" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "provisioning renderer did not run"

let phvp_action = "action='/projects/phvp-project/community-home'"

let phvp_copy_cases =
  [ phvp_case "provisioning page: heading, setup-draft copy, and factual \
               verification wording" (fun () ->
        let html = phvp_render () in
        Html_assert.must html "Create a community home";
        Html_assert.must html
          "Create a private setup draft for this project, then configure and \
           publish it as Public or Unlisted.";
        Html_assert.must html "Project connected through GitHub";
        Html_assert.must html "private setup draft";
        Html_assert.must html "It is not public yet.";
        Html_assert.must html "Only authorized setup users can reach it before \
                      publication.";
        Html_assert.must html "initial top moderator";
        Html_assert.must html "Publication is a separate, later step")
  ; phvp_case "provisioning page: publication offers Public or Unlisted and \
               never a fully private published community" (fun () ->
        let html = phvp_render () in
        Html_assert.must html "you choose Public or Unlisted";
        Html_assert.must html "keeps a public home, with private rooms available";
        List.iter (Html_assert.must_not html)
          [ "fully private"; "Fully private"; "Private community";
            "publish as Private"; "Public, Unlisted, or Private"
          ])
  ; phvp_case "provisioning page: no officiality or endorsement language, and \
               no claim that a draft already exists" (fun () ->
        let html = phvp_render ~project:(phvp_project ~description:"A project." ()) () in
        List.iter (Html_assert.must_not html)
          [ "Official"; "official"; "GitHub-approved"; "GitHub-endorsed";
            "endorsed"; "approved by GitHub"
          ];
        List.iter (Html_assert.must_not html)
          [ "Your draft community"; "The draft community is";
            "已"; "already been created"; "has been created"
          ])
  ; phvp_case "provisioning page: the project identity renders as escaped \
               text only" (fun () ->
        let html =
          phvp_render
            ~project:
              (phvp_project ~name:"Phvp Alpha" ~kind:Pi.Ecosystem
                 ~login:"phvp-org" ~description:"**not** <b>markup</b>" ())
            ()
        in
        Html_assert.must html "Phvp Alpha";
        Html_assert.must html "Ecosystem";
        Html_assert.must html "phvp-org";
        Html_assert.must html "**not** &lt;b&gt;markup&lt;/b&gt;";
        Html_assert.must_not html "<b>markup</b>")
  ]

let phvp_form_cases =
  [ phvp_case "provisioning form: exactly one POST form to the exact future \
               route" (fun () ->
        let html = phvp_live () in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences html "<form");
        Alcotest.(check int) "POST method" 1 (Html_assert.occurrences html "method='POST'");
        Alcotest.(check int) "exact action" 1 (Html_assert.occurrences html phvp_action);
        Html_assert.must html "Create private draft";
        (* Nameless submit control. *)
        Html_assert.must_not html "<button type='submit' name")
  ; phvp_case "provisioning form: exactly the three application fields, and \
               no hidden field of our own" (fun () ->
        let html = phvp_live () in
        List.iter
          (fun field ->
            Alcotest.(check int)
              ("one " ^ field)
              1
              (Html_assert.occurrences html (Printf.sprintf "name='%s'" field)))
          [ "community_name"; "community_slug"; "community_description" ];
        Alcotest.(check int) "two text inputs plus the framework CSRF field" 3
          (Html_assert.occurrences html "<input");
        Alcotest.(check int) "one textarea" 1 (Html_assert.occurrences html "<textarea");
        Alcotest.(check int) "no select" 0 (Html_assert.occurrences html "<select");
        (* The only hidden input is Dream's own, in its own quoting style. *)
        Alcotest.(check int) "no hidden field of ours" 0
          (Html_assert.occurrences html "type='hidden'");
        Alcotest.(check int) "exactly one framework hidden field" 1
          (Html_assert.occurrences html "type=\"hidden\"");
        List.iter (Html_assert.must_not html)
          [ "name='project_slug'"; "name='project_id'"; "name='user_id'";
            "name='installation_id'"; "name='repository_id'";
            "name='return_url'"; "name='visibility'"; "name='publish'";
            "name='active_home'"
          ])
  ; phvp_case "provisioning form: the framework CSRF field appears only with a \
               live request" (fun () ->
        let pure = phvp_render () in
        Alcotest.(check int) "pure render has no CSRF field" 0
          (Html_assert.occurrences pure "dream.csrf");
        Alcotest.(check int) "pure render still has the form" 1
          (Html_assert.occurrences pure "<form");
        let live = phvp_live () in
        Alcotest.(check int) "live render has one CSRF field" 1
          (Html_assert.occurrences live "name=\"dream.csrf\""))
  ; phvp_case "provisioning form: suggested values populate the controls and \
               stay escaped" (fun () ->
        let html =
          phvp_render
            ~values:
              (phvp_values ~name:"Phvp Suggested" ~slug:"phvp-suggested"
                 ~description:"Suggested body" ())
            ()
        in
        Html_assert.must html "value='Phvp Suggested'";
        Html_assert.must html "value='phvp-suggested'";
        Html_assert.must html ">Suggested body</textarea>";
        let hostile =
          phvp_render
            ~values:
              (phvp_values ~name:"' onfocus='alert(1)"
                 ~slug:"'><script>alert(1)</script>"
                 ~description:"</textarea><script>alert(1)</script>" ())
            ()
        in
        Html_assert.must_not hostile "<script";
        Html_assert.must_not hostile "onfocus='alert";
        Html_assert.must hostile "&#39;")
  ; phvp_case "provisioning form: an empty suggested slug renders an empty \
               control rather than a repaired value" (fun () ->
        let html =
          phvp_render ~values:(phvp_values ~name:"Phvp" ~slug:"" ()) ()
        in
        Html_assert.must html "name='community_slug' maxlength='80' value=''")
  ]

let phvp_defensive_cases =
  [ phvp_case "provisioning page: an invalid project slug suppresses the form \
               and every project-derived link" (fun () ->
        List.iter
          (fun slug ->
            let html = phvp_live ~project:(phvp_project ~slug ()) () in
            Alcotest.(check int) ("no form for " ^ String.escaped slug) 0
              (Html_assert.occurrences html "<form");
            Alcotest.(check int) ("no action for " ^ String.escaped slug) 0
              (Html_assert.occurrences html "action=");
            Alcotest.(check int) ("no project link for " ^ String.escaped slug) 0
              (Html_assert.occurrences html "href='/projects/");
            (* The copy survives; only the actions disappear. *)
            Html_assert.must html "Create a community home";
            Html_assert.must html "Project connected through GitHub")
          [ ""; "Phvp-Project"; "phvp_project"; "-phvp"; "phvp-";
            "phvp/project"; "phvp project"; "phvp--project";
            String.make 81 'a'
          ])
  ; phvp_case "provisioning page: a canonical slug yields the back link too"
      (fun () ->
        let html = phvp_render () in
        Html_assert.must html "href='/projects/phvp-project/setup'";
        Html_assert.must html "Back to project setup")
  ; phvp_case "provisioning page: no identifier of any kind is rendered"
      (fun () ->
        let html =
          phvp_live
            ~project:(phvp_project ~description:"desc" ())
            ~values:(phvp_values ~name:"Phvp" ~slug:"phvp-project" ())
            ()
        in
        List.iter (Html_assert.must_not html)
          [ "project_id"; "draft_id"; "installation_id"; "steward_id";
            "relation_id"; "community_id"; "forge_namespace_id"
          ])
  ; phvp_case "provisioning page: no script, inline style, handler, or refresh"
      (fun () ->
        let html = phvp_live ~feedback:Phvp.Provisioning_failed () in
        List.iter (Html_assert.must_not html)
          [ "<script"; "javascript:"; "onclick"; "onsubmit"; "onload";
            "onerror"; "style='"; "style=\""; "http-equiv"; "window.location";
            "location.href"
          ])
  ; phvp_case "provisioning page: no credential-shaped fixture can appear"
      (fun () ->
        let html =
          phvp_live ~project:(phvp_project ~login:"phvp-owner" ()) ()
        in
        List.iter (Html_assert.must_not html)
          [ "gho_"; "ghr_"; "ghs_"; "client_secret"; "code_verifier";
            "access_token"
          ])
  ]

let phvp_feedback_cases =
  [ phvp_case "provisioning page: no feedback renders no alert" (fun () ->
        let html = phvp_render () in
        Alcotest.(check int) "no alert" 0 (Html_assert.occurrences html "phv-alert"))
  ; phvp_case "provisioning page: every feedback variant renders exactly one \
               generic alert that echoes no submitted value" (fun () ->
        let variants =
          [ ("Stale_form", Phvp.Stale_form);
            ("Invalid_form", Phvp.Invalid_form);
            ("Invalid_community_name", Phvp.Invalid_community_name);
            ("Invalid_community_slug", Phvp.Invalid_community_slug);
            ("Invalid_community_description", Phvp.Invalid_community_description);
            ("Community_slug_unavailable", Phvp.Community_slug_unavailable);
            ("Active_home_exists", Phvp.Active_home_exists);
            ("Provisioning_failed", Phvp.Provisioning_failed)
          ]
        in
        let seen = ref [] in
        List.iter
          (fun (label, feedback) ->
            let html =
              phvp_render
                ~values:
                  (phvp_values ~name:"SECRETNAME" ~slug:"SECRETSLUG"
                     ~description:"SECRETBODY" ())
                ~feedback ()
            in
            Alcotest.(check int) (label ^ ": one alert") 1
              (Html_assert.occurrences html "phv-alert");
            (* The alert text itself never repeats what was submitted. *)
            let alert_start =
              match Html_assert.index_from html "phv-alert" 0 with
              | Some i -> i
              | None -> Alcotest.failf "%s: alert missing" label
            in
            let alert_end =
              match Html_assert.index_from html "</div>" alert_start with
              | Some i -> i
              | None -> Alcotest.failf "%s: unterminated alert" label
            in
            let alert =
              String.sub html alert_start (alert_end - alert_start)
            in
            List.iter
              (fun secret ->
                Alcotest.(check bool)
                  (label ^ ": alert free of " ^ secret)
                  false (Html_assert.contains alert secret))
              [ "SECRETNAME"; "SECRETSLUG"; "SECRETBODY" ];
            (* Distinct copy per variant: no two variants share a message. *)
            Alcotest.(check bool)
              (label ^ ": distinct copy") false
              (List.mem alert !seen);
            seen := alert :: !seen;
            (* The form is still offered so the steward can correct. *)
            Alcotest.(check int) (label ^ ": form survives") 1
              (Html_assert.occurrences html "<form"))
          variants)
  ]

let phvp_suite =
  phvp_copy_cases @ phvp_form_cases @ phvp_defensive_cases
  @ phvp_feedback_cases

let suites =
    (* The creation page itself: setup-draft and Public/Unlisted copy, the
       exact one-form contract with three application fields and no hidden
       identifier, CSRF only with a live request, defensive degradation on
       a malformed slug, escaping, and every feedback variant. DB-free. *)
  [ ("project_home_provisioning_page", phvp_suite)
  ]
