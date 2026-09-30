module Pi = Earde.Project_identity
module Ncpp = Earde.Network_community_publication_pages

(* === Network-community publication page (Network_community_publication_pages) ===
   Pure rendering of the review-and-publish surface: heading and draft copy,
   the two publication choices and the absence of a private one, the exact
   one-form contract, defensive degradation, escaping, and every feedback
   variant. DB-free. *)

let ncpp_case = Case.quick

let ncpp_project ?(name = "Ncpp Project") ?(slug = "ncpp-project")
    ?(login = "ncpp-owner") ?(kind = Pi.Project) () : Ncpp.project =
  { Ncpp.name; slug; namespace_login = login; kind }

let ncpp_community ?(name = "Ncpp Home") ?(slug = "ncpp-home") ?description () :
    Ncpp.community =
  { Ncpp.name; slug; description }

let ncpp_values ?(name = "Ncpp Home") ?(slug = "ncpp-home") ?(description = "")
    ?(visibility = "public") () : Ncpp.form_values =
  {
    Ncpp.community_name = name;
    community_slug = slug;
    community_description = description;
    publication_visibility = visibility;
  }

(* Assertions run over the feature fragment, not the whole page: the shared
   shell owns its own forms and chrome. *)
let ncpp_render ?request ?(community = ncpp_community ())
    ?(project = ncpp_project ()) ?(values = ncpp_values ()) ?feedback () =
  Html_assert.panel_fragment
    (Ncpp.network_community_publication_page ?request ~community ~project
       ~values ~feedback ())

let ncpp_whole ?request ?(community = ncpp_community ())
    ?(project = ncpp_project ()) ?(values = ncpp_values ()) ?feedback () =
  Ncpp.network_community_publication_page ?request ~community ~project ~values
    ~feedback ()

(* A live request under a secret + sessions pipeline, so the framework CSRF
   field can be emitted; no SQL is touched. *)
let ncpp_live ?community ?project ?values ?feedback () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some (ncpp_render ~request:req ?community ?project ?values ?feedback ());
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/c/ncpp-home/setup" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "publication renderer did not run"

let ncpp_action = "action='/c/ncpp-home/publish'"

let ncpp_copy_cases =
  [
    ncpp_case "publication page: heading and the supporting review copy"
      (fun () ->
        let html = ncpp_render () in
        Html_assert.must html "Complete setup and publish";
        Html_assert.must html
          "Review the community identity and choose how people can discover it.");
    ncpp_case
      "publication page: the draft state and the separateness of publication \
       are stated plainly" (fun () ->
        let html = ncpp_render () in
        Html_assert.must html "still a private setup draft";
        Html_assert.must html "Only people authorized for setup can reach it.";
        Html_assert.must html "Publishing is a separate, explicit action.";
        Html_assert.must html "Nothing is published until you submit this form.");
    ncpp_case
      "publication page: Public and Unlisted are each described exactly, and a \
       fully private publication is offered nowhere" (fun () ->
        let html = ncpp_render () in
        Html_assert.must html
          "reachable by anyone, listed in Earde&#39;s own discovery surfaces, \
           and open to search-engine indexing";
        Html_assert.must html
          "reachable by anyone with the direct URL, but kept out of \
           Earde&#39;s discovery surfaces and marked <code>noindex</code>";
        Html_assert.must html
          "It cannot be published as a fully private community.";
        List.iter
          (Html_assert.must_not html)
          [
            "value='private'";
            "value=\"private\"";
            ">Private<";
            "publish as Private";
            "Public, Unlisted, or Private";
            "fully private community.</li></ul><li>";
          ]);
    ncpp_case
      "publication page: private rooms remain possible under either choice, \
       and publication grants no new permission" (fun () ->
        let html = ncpp_render () in
        Html_assert.must html
          "Private rooms and restricted sections can still exist inside it \
           under either choice";
        Html_assert.must html
          "Publishing does not change who moderates or administers anything.";
        Html_assert.must html
          "Project stewards gain no permission beyond the durable roles they \
           already hold.");
    ncpp_case
      "publication page: the connected project reads factually, with no \
       officiality or endorsement language" (fun () ->
        let html =
          ncpp_render
            ~project:
              (ncpp_project ~name:"Ncpp Alpha" ~kind:Pi.Ecosystem
                 ~login:"ncpp-org" ())
            ()
        in
        Html_assert.must html "Ncpp Alpha";
        Html_assert.must html "Ecosystem";
        Html_assert.must html "ncpp-org";
        Html_assert.must html "Connected through GitHub.";
        List.iter
          (Html_assert.must_not html)
          [
            "Official";
            "official";
            "GitHub-approved";
            "GitHub-endorsed";
            "endorsed";
            "approved by GitHub";
            "sponsored";
          ]);
    ncpp_case
      "publication page: every project kind has its own non-endorsing label"
      (fun () ->
        List.iter
          (fun (kind, label) ->
            let html = ncpp_render ~project:(ncpp_project ~kind ()) () in
            Html_assert.must html label)
          [
            (Pi.Project, "Project");
            (Pi.Organization, "GitHub organization");
            (Pi.Ecosystem, "Ecosystem");
            (Pi.Foundation, "Foundation");
            (Pi.Working_group, "Working group");
            (Pi.Other, "Other open-source initiative");
          ]);
  ]

let ncpp_form_cases =
  [
    ncpp_case
      "publication page: exactly one form, with the exact method and \
       structurally built action" (fun () ->
        let html = ncpp_render () in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences html "<form");
        Alcotest.(check int)
          "one closing form" 1
          (Html_assert.occurrences html "</form>");
        Html_assert.must html "method='POST'";
        Html_assert.must html ncpp_action;
        (* The action is derived from the community slug alone: a different
           draft slug moves it, nothing else does. *)
        Html_assert.must
          (ncpp_render ~community:(ncpp_community ~slug:"ncpp-other" ()) ())
          "action='/c/ncpp-other/publish'");
    ncpp_case
      "publication page: exactly the four application controls, and no hidden \
       identifier, current slug, return URL, or lifecycle flag" (fun () ->
        let html = ncpp_render () in
        let region = Html_assert.form_region html in
        List.iter (Html_assert.must region)
          [
            "name='community_name'";
            "name='community_slug'";
            "name='community_description'";
            "name='publication_visibility'";
          ];
        Alcotest.(check int)
          "one name control" 1
          (Html_assert.occurrences region "name='community_name'");
        Alcotest.(check int)
          "one slug control" 1
          (Html_assert.occurrences region "name='community_slug'");
        Alcotest.(check int)
          "one description control" 1
          (Html_assert.occurrences region "name='community_description'");
        (* Pure rendering emits no framework field, so the only inputs are
           the application controls: two radios plus two text inputs. *)
        Alcotest.(check int)
          "four inputs" 4
          (List.length (Html_assert.input_tags region));
        List.iter
          (Html_assert.must_not region)
          [
            "type='hidden'";
            "type=\"hidden\"";
            "name='community_id'";
            "name='project_id'";
            "name='relation_id'";
            "name='actor_id'";
            "name='user_id'";
            "name='current_slug'";
            "name='return_url'";
            "name='redirect_to'";
            "name='onboarding_state'";
            "name='indexable'";
            "name='discoverable'";
            "name='visibility'";
          ]);
    ncpp_case
      "publication page: exactly two publication radio values, and a nameless \
       submit with no confirmation script" (fun () ->
        let region = Html_assert.form_region (ncpp_render ()) in
        Alcotest.(check int)
          "two radios" 2
          (Html_assert.occurrences region "type='radio'");
        Alcotest.(check int)
          "two publication controls" 2
          (Html_assert.occurrences region "name='publication_visibility'");
        Alcotest.(check int)
          "one public value" 1
          (Html_assert.occurrences region "value='public'");
        Alcotest.(check int)
          "one unlisted value" 1
          (Html_assert.occurrences region "value='unlisted'");
        Html_assert.must region "<button type='submit'";
        Html_assert.must region ">Publish community</button>";
        List.iter
          (Html_assert.must_not region)
          [ "<button name"; "onsubmit"; "confirmModal"; "onclick" ]);
    ncpp_case
      "publication page: public is the default only when no valid explicit \
       choice is present" (fun () ->
        (* No explicit choice: public preselected. *)
        let default =
          Html_assert.form_region
            (ncpp_render ~values:(ncpp_values ~visibility:"" ()) ())
        in
        Html_assert.must default "value='public' checked";
        Html_assert.must_not default "value='unlisted' checked";
        (* An explicit unlisted choice survives the round trip. *)
        let unlisted =
          Html_assert.form_region
            (ncpp_render ~values:(ncpp_values ~visibility:"unlisted" ()) ())
        in
        Html_assert.must unlisted "value='unlisted' checked";
        Html_assert.must_not unlisted "value='public' checked";
        (* An explicit public choice stays public. *)
        let public =
          Html_assert.form_region
            (ncpp_render ~values:(ncpp_values ~visibility:"public" ()) ())
        in
        Html_assert.must public "value='public' checked");
    ncpp_case
      "publication page: an invalid submitted publication value selects \
       neither option rather than defaulting to the more exposed one" (fun () ->
        List.iter
          (fun value ->
            let region =
              Html_assert.form_region
                (ncpp_render ~values:(ncpp_values ~visibility:value ()) ())
            in
            Alcotest.(check int)
              ("no preselection for " ^ String.escaped value)
              0
              (Html_assert.occurrences region "checked"))
          [ "private"; "Public"; " public"; "unlisted "; "PRIVATE"; "listed" ];
        (* The empty value is "no choice yet", not a rejected one, and is
             the single documented case that falls back to public. *)
        let empty =
          Html_assert.form_region
            (ncpp_render ~values:(ncpp_values ~visibility:"" ()) ())
        in
        Alcotest.(check int)
          "empty value defaults to public" 1
          (Html_assert.occurrences empty "value='public' checked"));
    ncpp_case
      "publication page: the framework CSRF field appears only with a live \
       request, and only inside the form" (fun () ->
        let pure = ncpp_render () in
        Html_assert.must_not pure "dream.csrf";
        let live = ncpp_live () in
        let region = Html_assert.form_region live in
        let tags =
          List.filter Html_assert.is_csrf_input (Html_assert.input_tags region)
        in
        Alcotest.(check int) "exactly one framework field" 1 (List.length tags);
        (* Removing it leaves exactly the four application controls. *)
        Alcotest.(check int)
          "no other framework field in the page" 1
          (Html_assert.occurrences live Html_assert.csrf_input_prefix));
    ncpp_case
      "publication page: the controls prefill from the supplied values, with \
       an absent description as an empty textarea" (fun () ->
        let html =
          ncpp_render
            ~values:
              (ncpp_values ~name:"Ncpp Current" ~slug:"ncpp-current"
                 ~description:"Current body." ())
            ()
        in
        Html_assert.must html "value='Ncpp Current'";
        Html_assert.must html "value='ncpp-current'";
        Html_assert.must html "Current body.";
        let blank = ncpp_render ~values:(ncpp_values ~description:"" ()) () in
        Html_assert.must blank
          "<textarea name='community_description' maxlength='2000' rows='6'>";
        Html_assert.must_not blank ">None<");
    ncpp_case "publication page: control limits mirror the server-side policy"
      (fun () ->
        let region = Html_assert.form_region (ncpp_render ()) in
        Html_assert.must region "maxlength='120'";
        Html_assert.must region "maxlength='80'";
        Html_assert.must region "maxlength='2000'");
  ]

let ncpp_defensive_cases =
  [
    ncpp_case
      "publication page: every caller-controlled value is escaped, and a \
       description is never treated as markup" (fun () ->
        let html =
          ncpp_render
            ~community:
              (ncpp_community ~name:"<b>Name</b>"
                 ~description:"**bold** <i>x</i>" ())
            ~project:
              (ncpp_project ~name:"<script>alert(1)</script>" ~login:"o'wner\"x"
                 ())
            ~values:
              (ncpp_values ~name:"a'\"<>&" ~slug:"b'\"<>&"
                 ~description:"**bold** <i>x</i>" ())
            ()
        in
        List.iter
          (Html_assert.must_not html)
          [ "<script>alert(1)</script>"; "<b>Name</b>"; "<i>x</i>" ];
        Html_assert.must html "&lt;script&gt;alert(1)&lt;/script&gt;";
        Html_assert.must html "**bold** &lt;i&gt;x&lt;/i&gt;");
    ncpp_case
      "publication page: a draft slug outside the canonical network grammar \
       suppresses the whole form rather than emitting an unusable action"
      (fun () ->
        List.iter
          (fun slug ->
            let html = ncpp_render ~community:(ncpp_community ~slug ()) () in
            Alcotest.(check int)
              ("no form for " ^ String.escaped slug)
              0
              (Html_assert.occurrences html "<form");
            Html_assert.must_not html "/publish";
            (* The explanatory copy still renders — the page never raises. *)
            Html_assert.must html "Complete setup and publish")
          [
            "";
            " ";
            "Ncpp-Home";
            "ncpp home";
            "ncpp/home";
            "ncpp-";
            "-ncpp";
            "ncpp--home";
            "ncpp_home";
            "ncpp.home";
            "ncpp\x01home";
            String.make 81 'a';
          ]);
    ncpp_case
      "publication page: no project-derived link exists under any project \
       slug, malformed or not" (fun () ->
        List.iter
          (fun slug ->
            let html = ncpp_render ~project:(ncpp_project ~slug ()) () in
            List.iter
              (Html_assert.must_not html)
              [
                "href='/projects/";
                "href=\"/projects/";
                "/community-home";
                "/request-home";
              ])
          [ "ncpp-project"; ""; "Ncpp Project"; "ncpp/project"; "ncpp--x" ]);
    ncpp_case
      "publication page: no script, inline style, event handler, or refresh \
       redirect" (fun () ->
        let html = ncpp_whole () in
        let feature = ncpp_render () in
        List.iter
          (Html_assert.must_not feature)
          [
            "<script";
            "javascript:";
            "style='";
            "style=\"";
            "onclick";
            "onsubmit";
            "onload";
            "onerror";
            "http-equiv";
          ];
        (* noindex is a page-level property, so it is asserted on the whole
           document rather than the fragment. *)
        Html_assert.must html "content='noindex'");
    ncpp_case
      "publication page: no identifier of any kind is renderable — the models \
       carry none" (fun () ->
        let html = Html_assert.without_csrf_inputs (ncpp_live ()) in
        List.iter
          (Html_assert.must_not html)
          [
            "community_id";
            "project_id";
            "relation_id";
            "steward_id";
            "installation_id";
            "membership_id";
          ]);
    ncpp_case
      "publication page: credential-shaped fixtures never reach the rendered \
       page" (fun () ->
        let html =
          Html_assert.without_csrf_inputs
            (ncpp_live
               ~project:
                 (ncpp_project ~name:"gho_NCPP_ACCESS_TOKEN"
                    ~login:"NCPP_CLIENT_SECRET" ())
               ())
        in
        (* The project identity is rendered, so the assertion is that the
           page adds nothing of its own: only the two supplied strings may
           appear, and no token-shaped value the page could have invented. *)
        List.iter
          (Html_assert.must_not html)
          [
            "ghr_";
            "ghu_";
            "code=";
            "state=";
            "code_verifier";
            "client_secret=";
            "access_token";
          ]);
  ]

let ncpp_feedback_cases =
  [
    ncpp_case "publication page: no feedback renders no alert" (fun () ->
        Html_assert.must_not (ncpp_render ()) "ncp-alert");
    ncpp_case
      "publication page: every feedback variant renders its own generic \
       message and never a submitted value" (fun () ->
        List.iter
          (fun (feedback, needle) ->
            let html =
              ncpp_render
                ~values:
                  (ncpp_values ~name:"NcppSecretName" ~slug:"ncpp-secret-slug"
                     ~visibility:"private" ())
                ~feedback ()
            in
            Html_assert.must html "ncp-alert";
            Html_assert.must html needle;
            (* The message itself never quotes the rejected value or the
               rule that failed. *)
            List.iter
              (Html_assert.must_not html)
              [
                "communities_network";
                "CHECK";
                "constraint";
                "regex";
                "char_length";
              ])
          [
            ( Ncpp.Invalid_form,
              "We couldn't read that submission. Review the form and try again."
            );
            ( Ncpp.Stale_form,
              "This page had been open too long, so the form could no longer \
               be submitted. Nothing was changed. Review it and submit again."
            );
            (Ncpp.Invalid_community_name, "Enter a community name we can use.");
            ( Ncpp.Invalid_community_slug,
              "Enter a community address using lowercase letters, numbers, and \
               single hyphens." );
            ( Ncpp.Invalid_community_description,
              "That description can't be used. Edit it and try again." );
            ( Ncpp.Invalid_publication_visibility,
              "Choose whether the community should be public or unlisted." );
            ( Ncpp.Community_slug_unavailable,
              "That community address is already taken. Choose another one." );
            ( Ncpp.Draft_unavailable,
              "This community is no longer waiting to be published." );
            ( Ncpp.Publication_failed,
              "We couldn't publish the community. Try again." );
          ]);
    ncpp_case
      "publication page: feedback never suppresses the form, and the rejected \
       values still round-trip escaped" (fun () ->
        let html =
          ncpp_render
            ~values:
              (ncpp_values ~name:"<x>" ~slug:"Bad Slug" ~visibility:"private" ())
            ~feedback:Ncpp.Invalid_community_slug ()
        in
        Alcotest.(check int)
          "form still present" 1
          (Html_assert.occurrences html "<form");
        Html_assert.must html "value='&lt;x&gt;'";
        Html_assert.must html "value='Bad Slug'";
        Html_assert.must_not html "checked");
  ]

let suites =
  (* The setup/publication page: heading and draft copy, the two
       publication choices and the absence of a private one, the exact
       one-form contract with four application fields and no hidden
       identifier, defensive degradation on a malformed draft slug,
       escaping, and every feedback variant. DB-free. *)
  [
    ("network_community_publication_page_copy", ncpp_copy_cases);
    ("network_community_publication_page_form", ncpp_form_cases);
    ("network_community_publication_page_defensive", ncpp_defensive_cases);
    ("network_community_publication_page_feedback", ncpp_feedback_cases);
  ]
