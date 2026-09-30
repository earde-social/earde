module Psp = Earde.Project_setup_pages

let psp_no_draft_cases =
  [
    Project_setup_fixture.psp_case
      "/bring link, no form, no chooser, no hidden draft input" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps Psp.No_available_drafts)
        in
        Html_assert.must frag "Create a project";
        Html_assert.must frag "href='/bring'";
        Html_assert.must frag "Connect a GitHub project";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "type='hidden'";
        Html_assert.must_not frag "name='draft_id'";
        Html_assert.must_not frag "ps-draft-list");
    Project_setup_fixture.psp_case "no feedback renders no alert container"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps Psp.No_available_drafts)
        in
        Html_assert.must_not frag "ps-alert");
  ]

let psp_chooser_cases =
  [
    Project_setup_fixture.psp_case
      "options in supplied order with type copy and counts" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps ~user:"alice"
               (Psp.Choose_draft
                  [
                    Project_setup_fixture.ps_chooser_a;
                    Project_setup_fixture.ps_chooser_b;
                  ]))
        in
        Html_assert.order frag "alpha-dev" "beta-org";
        Html_assert.must frag "Personal account";
        Html_assert.must frag "Organization";
        Html_assert.must frag "1 repository<";
        Html_assert.must frag "12 repositories, 4 selected";
        Html_assert.must_not frag "0 selected";
        Html_assert.must_not frag "1 repositories");
    Project_setup_fixture.psp_case
      "structured /projects/new?draft= links, nothing automatic" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Choose_draft
                  [
                    Project_setup_fixture.ps_chooser_a;
                    Project_setup_fixture.ps_chooser_b;
                  ]))
        in
        Html_assert.must frag "href='/projects/new?draft=5'";
        Html_assert.must frag "href='/projects/new?draft=9'";
        (* No form and no checked control: nothing is selected for the
           user, and only the permitted draft id appears — no installation,
           account, or user identifiers. *)
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "checked";
        Html_assert.must_not frag "installation";
        Html_assert.must_not frag "connected_by";
        Html_assert.must_not frag "account_id");
    Project_setup_fixture.psp_case
      "empty chooser falls back to the safe empty state" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps (Psp.Choose_draft []))
        in
        Html_assert.must frag "href='/bring'";
        Html_assert.must_not frag "ps-draft-list";
        Html_assert.must_not frag "<form");
    Project_setup_fixture.psp_case
      "non-positive draft ids never become actionable links" (fun () ->
        let corrupt_zero =
          Project_setup_fixture.ps_draft ~id:0L ~login:"zero-corrupt" ()
        in
        let corrupt_neg =
          Project_setup_fixture.ps_draft ~id:(-3L) ~login:"neg-corrupt" ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Choose_draft [ corrupt_zero; corrupt_neg ]))
        in
        Html_assert.must frag "zero-corrupt";
        Html_assert.must frag "neg-corrupt";
        Html_assert.must_not frag "draft=0";
        Html_assert.must_not frag "draft=-3";
        Html_assert.must_not frag "<a ");
    Project_setup_fixture.psp_case
      "negative counts are clamped, never rendered as metrics" (fun () ->
        let corrupt =
          Project_setup_fixture.ps_draft ~id:5L ~login:"count-corrupt"
            ~repos:(-4) ~selected:(-2) ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps (Psp.Choose_draft [ corrupt ]))
        in
        Html_assert.must_not frag "-4";
        Html_assert.must_not frag "-2";
        Html_assert.must frag "0 repositories");
  ]

let psp_configure_cases =
  [
    Project_setup_fixture.psp_case "exactly one POST form with the exact action"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
        Html_assert.must frag
          "<form method='POST' action='/projects/new/repositories'";
        Html_assert.must frag "octo-org";
        Html_assert.must frag "Organization");
    Project_setup_fixture.psp_case "hidden field set is exactly draft_id"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Alcotest.(check int)
          "one hidden input" 1
          (Html_assert.occurrences frag "type='hidden'");
        Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
        (* Only draft_id plus the three repository checkboxes carry a form
           name — the submit button and everything else stay nameless. *)
        Alcotest.(check int)
          "named fields" 4
          (Html_assert.occurrences frag " name='");
        Html_assert.must_not frag "name='primary'";
        Html_assert.must_not frag "primary_repository";
        Html_assert.must_not frag "github_repository_id";
        Html_assert.must_not frag "name='user_id'";
        Html_assert.must_not frag "installation_id";
        Html_assert.must_not frag "name='state'";
        Html_assert.must_not frag "token");
    Project_setup_fixture.psp_case
      "one checkbox per repository valued by snapshot id" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Alcotest.(check int)
          "three checkboxes" 3
          (Html_assert.occurrences frag "type='checkbox'");
        Html_assert.must frag "name='repository' value='101' checked";
        Html_assert.must frag "name='repository' value='102'";
        Html_assert.must frag "name='repository' value='103'";
        Alcotest.(check int)
          "only the selected row is checked" 1
          (Html_assert.occurrences frag " checked"));
    Project_setup_fixture.psp_case
      "nameless submit button with the required copy" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Html_assert.must frag "Save repository selection";
        Html_assert.must_not frag "<button type='submit' name=";
        Html_assert.must_not frag "button name=");
    Project_setup_fixture.psp_case
      "repository order, description omission, archived marker" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Html_assert.order frag "octo-org/first" "octo-org/second";
        Html_assert.order frag "octo-org/second" "octo-org/legacy";
        Alcotest.(check int)
          "one description" 1
          (Html_assert.occurrences frag "ps-repo-desc");
        Html_assert.must frag "A second tool";
        Alcotest.(check int)
          "one archived marker" 1
          (Html_assert.occurrences frag "ps-repo-archived");
        Html_assert.must frag "Archived";
        (* Archived repositories stay selectable. *)
        Alcotest.(check int)
          "still three checkboxes" 3
          (Html_assert.occurrences frag "type='checkbox'"));
    Project_setup_fixture.psp_case
      "slash-containing default branch is text, never a URL" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Html_assert.must frag "<code class='ps-repo-branch'>release/2.x</code>";
        Html_assert.must_not frag "/release/2.x";
        Html_assert.must_not frag "/tree/";
        Html_assert.must frag "href='https://github.com/octo-org/second'");
    Project_setup_fixture.psp_case
      "canonical repository links rendered for every row" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
        in
        Html_assert.must frag "href='https://github.com/octo-org/first'";
        Html_assert.must frag "href='https://github.com/octo-org/legacy'");
    Project_setup_fixture.psp_case
      "empty repository list renders no actionable form" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories
                  (Project_setup_fixture.ps_config ~repos:[] ())))
        in
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "type='checkbox'";
        Html_assert.must_not frag "value='11'";
        Html_assert.must frag "href='/bring'");
    Project_setup_fixture.psp_case
      "non-positive draft id renders no form and leaks no id" (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories
                  (Project_setup_fixture.ps_config
                     ~draft:(Project_setup_fixture.ps_draft ~id:0L ())
                     ())))
        in
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "value='0'";
        Html_assert.must_not frag "name='draft_id'");
    Project_setup_fixture.psp_case
      "non-positive snapshot id renders no form control" (fun () ->
        let corrupt =
          Project_setup_fixture.ps_repo ~id:0L ~full_name:"octo-org/corrupt" ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories
                  (Project_setup_fixture.ps_config
                     ~draft:Project_setup_fixture.ps_cfg_draft
                     ~repos:[ Project_setup_fixture.ps_r1; corrupt ]
                     ())))
        in
        Alcotest.(check int)
          "one checkbox" 1
          (Html_assert.occurrences frag "type='checkbox'");
        Html_assert.must_not frag "value='0'";
        Html_assert.must frag "octo-org/corrupt");
  ]

let psp_escaping_cases =
  [
    Project_setup_fixture.psp_case "chooser login is escaped, not raw HTML"
      (fun () ->
        let hostile =
          Project_setup_fixture.ps_draft ~id:21L ~login:"x<b>&\"y'" ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps (Psp.Choose_draft [ hostile ]))
        in
        Html_assert.must_not frag "<b>";
        Html_assert.must frag "x&lt;b&gt;&amp;&quot;y&#39;");
    Project_setup_fixture.psp_case "repository display strings are escaped"
      (fun () ->
        let hostile =
          Project_setup_fixture.ps_repo ~id:31L
            ~full_name:"evil<script>alert(1)</script>"
            ~url:"https://github.com/octo-org/ok'><img src=x onerror=alert(1)>"
            ~description:"a & b <i>italic</i>" ~branch:"feat/<img src=x>" ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories
                  (Project_setup_fixture.ps_config
                     ~draft:Project_setup_fixture.ps_cfg_draft
                     ~repos:[ hostile ] ())))
        in
        Html_assert.must_not frag "<script";
        Html_assert.must frag "evil&lt;script&gt;";
        Html_assert.must_not frag "<img";
        Html_assert.must_not frag "<i>";
        (* The hostile URL cannot break out of the href attribute. *)
        Html_assert.must_not frag "'><img";
        Html_assert.must frag "a &amp; b &lt;i&gt;italic&lt;/i&gt;";
        Html_assert.must frag "feat/&lt;img src=x&gt;");
    Project_setup_fixture.psp_case
      "non-http repository URL collapses to an inert href" (fun () ->
        let hostile =
          Project_setup_fixture.ps_repo ~id:32L ~full_name:"octo-org/js"
            ~url:"javascript:alert(1)" ()
        in
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               (Psp.Configure_repositories
                  (Project_setup_fixture.ps_config
                     ~draft:Project_setup_fixture.ps_cfg_draft
                     ~repos:[ hostile ] ())))
        in
        Html_assert.must_not frag "javascript:";
        Html_assert.must frag "href='#'");
  ]

let psp_feedback_check name feedback ~copy ~class_ =
  Project_setup_fixture.psp_case name (fun () ->
      let frag =
        Html_assert.panel_fragment
          (Project_setup_fixture.render_ps ~feedback:(Some feedback)
             (Psp.Configure_repositories Project_setup_fixture.ps_cfg))
      in
      Html_assert.must frag copy;
      Html_assert.must frag class_;
      (* Cosmetic only: the same single form with the same hidden field
         renders regardless of feedback. *)
      Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
      Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
      (* Alert copy is generic: no id, count, or submitted value ever
         reaches it. *)
      let alert = Project_setup_fixture.ps_alert_fragment frag in
      Alcotest.(check bool)
        "no digits in alert copy" false
        (String.exists (fun c -> c >= '0' && c <= '9') alert))

let psp_feedback_cases =
  [
    psp_feedback_check "saved: success alert" Psp.Selection_saved
      ~copy:"Repository selection saved." ~class_:"ps-alert--success";
    psp_feedback_check "stale: generic refresh warning" Psp.Selection_stale
      ~copy:
        "The GitHub repository list changed. Review the current repositories \
         and save again."
      ~class_:"ps-alert--error";
    psp_feedback_check "invalid: generic error" Psp.Selection_invalid
      ~copy:
        "We couldn't save that repository selection. Review it and try again."
      ~class_:"ps-alert--error";
    psp_feedback_check "unavailable: indistinguishable copy"
      Psp.Draft_unavailable ~copy:"That project setup is no longer available."
      ~class_:"ps-alert--error";
    psp_feedback_check "required: generic empty-selection copy"
      Psp.Repository_selection_required
      ~copy:"Select at least one public repository before continuing."
      ~class_:"ps-alert--error";
    Project_setup_fixture.psp_case
      "no feedback renders no alert container in any state" (fun () ->
        List.iter
          (fun state ->
            let frag =
              Html_assert.panel_fragment (Project_setup_fixture.render_ps state)
            in
            Html_assert.must_not frag "ps-alert")
          [
            Psp.No_available_drafts;
            Psp.Choose_draft [ Project_setup_fixture.ps_chooser_a ];
            Psp.Configure_repositories Project_setup_fixture.ps_cfg;
          ]);
    Project_setup_fixture.psp_case "feedback never changes which state renders"
      (fun () ->
        let frag =
          Html_assert.panel_fragment
            (Project_setup_fixture.render_ps
               ~feedback:(Some Psp.Draft_unavailable) Psp.No_available_drafts)
        in
        Html_assert.must frag "That project setup is no longer available.";
        Html_assert.must frag "href='/bring'";
        Html_assert.must_not frag "<form");
  ]

(* Copy and future-design constraints across every state × feedback pair:
   factual verification language, no endorsement claims, and none of the
   style/script/redirect constructs the feature templates must not
   introduce. Scoped to the feature fragment: the shared layout's own
   scripts are out of scope here. *)
let psp_copy_states =
  [
    ("no drafts", Psp.No_available_drafts);
    ( "chooser",
      Psp.Choose_draft
        [
          Project_setup_fixture.ps_chooser_a; Project_setup_fixture.ps_chooser_b;
        ] );
    ("configure", Psp.Configure_repositories Project_setup_fixture.ps_cfg);
    ("empty chooser", Psp.Choose_draft []);
    ( "corrupt configure",
      Psp.Configure_repositories (Project_setup_fixture.ps_config ~repos:[] ())
    );
  ]

let psp_copy_cases =
  List.concat_map
    (fun state ->
      List.map
        (Project_setup_fixture.psp_copy_case state)
        Project_setup_fixture.psp_copy_feedbacks)
    psp_copy_states

let suites =
  (* /projects/new renderer: one suite per page state, plus escaping,
       feedback, and copy/future-design constraints. All assertions scope
       to the feature fragment inside the shared create-flow layout. *)
  [
    ("project_setup_page_no_drafts", psp_no_draft_cases);
    ("project_setup_page_chooser", psp_chooser_cases);
    ("project_setup_page_configure", psp_configure_cases);
    ("project_setup_page_escaping", psp_escaping_cases);
    ("project_setup_page_feedback", psp_feedback_cases);
    ("project_setup_page_copy", psp_copy_cases);
  ]
