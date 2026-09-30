module Psp = Earde.Project_setup_pages
module Pi = Earde.Project_identity

(* ===== Identity page state (Configure_identity) =====
   Same fragment-scoped technique as the repository-selection page tests:
   assertions pin only what the feature templates introduce inside the
   shared create-flow layout. *)

let pip_case = Project_setup_fixture.psp_case

let pip_repo ?(id = 501L) ?(full_name = "octo-org/widgets")
    ?(archived = false) () : Psp.identity_repository =
  { Psp.snapshot_id = id; full_name; is_archived = archived }

let pip_values ?(kind = Pi.Organization) ?(name = "Widgets")
    ?(slug = "widgets") ?(description = "") ?(website = "") ?primary () :
    Psp.identity_values =
  { Psp.kind; name; slug; description; website_url = website;
    primary_snapshot_id = primary }

let pip_config ?draft ?(repos = [ pip_repo () ]) ?values () :
    Psp.identity_configuration =
  { Psp.draft = (match draft with Some d -> d | None -> Project_setup_fixture.ps_cfg_draft);
    selected_repositories = repos;
    values = (match values with Some v -> v | None -> pip_values ()) }

let pip_r1 = pip_repo ~id:101L ~full_name:"octo-org/first" ()

let pip_r2 = pip_repo ~id:102L ~full_name:"octo-org/second" ~archived:true ()

let pip_r3 = pip_repo ~id:103L ~full_name:"octo-org/legacy" ()

let pip_cfg =
  pip_config
    ~repos:[ pip_r1; pip_r2; pip_r3 ]
    ~values:
      (pip_values ~kind:Pi.Ecosystem ~name:"Widgets" ~slug:"widgets"
         ~description:"A toolkit" ~website:"https://example.com"
         ~primary:102L ())
    ()

let pip_render ?feedback state = Html_assert.panel_fragment (Project_setup_fixture.render_ps ?feedback state)

(* One named <select> element in isolation, so selected-option assertions
   cannot leak across the kind and primary controls. *)
let pip_select frag select_name =
  let marker = Printf.sprintf "<select name='%s'>" select_name in
  match Html_assert.index_from frag marker 0 with
  | None -> Alcotest.fail "expected select element"
  | Some s -> (
      match Html_assert.index_from frag "</select>" s with
      | None -> Alcotest.fail "unterminated select element"
      | Some e -> String.sub frag s (e - s))

let pip_structure_cases =
  [ pip_case "separate project-details step with one POST /projects form"
      (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must frag "Project details";
        Html_assert.must frag
          "Describe the verified project before choosing its community home.";
        Html_assert.must frag "Verified through GitHub";
        Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
        Html_assert.must frag "<form method='POST' action='/projects'";
        Html_assert.must_not frag "action='/projects/new/repositories'")
  ; pip_case "application field set is exactly the seven identity fields"
      (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Alcotest.(check int) "one hidden input" 1
          (Html_assert.occurrences frag "type='hidden'");
        Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
        Alcotest.(check int) "named fields" 7 (Html_assert.occurrences frag " name='");
        List.iter
          (fun field -> Html_assert.must frag (Printf.sprintf "name='%s'" field))
          [ "draft_id"; "kind"; "name"; "slug"; "description"; "website_url";
            "primary_snapshot_id" ];
        Html_assert.must_not frag "installation_id";
        Html_assert.must_not frag "account_id";
        Html_assert.must_not frag "name='user_id'";
        Html_assert.must_not frag "github_repository_id";
        Html_assert.must_not frag "name='state'";
        Html_assert.must_not frag "token";
        Html_assert.must_not frag "redirect")
  ; pip_case "nameless submit button with the required copy" (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must frag "Create project</button>";
        Html_assert.must_not frag "<button type='submit' name=";
        Html_assert.must_not frag "button name=")
  ; pip_case "no community-home fields or chooser in the identity state"
      (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must_not frag "name='community";
        Html_assert.must_not frag "community_id";
        Html_assert.must_not frag "community_slug";
        Html_assert.must_not frag "Choose which GitHub project setup")
  ]

let pip_kind_cases =
  [ pip_case "exactly the six canonical kind values, no aliases" (fun () ->
        let sel =
          pip_select (pip_render (Psp.Configure_identity pip_cfg)) "kind"
        in
        Alcotest.(check int) "six options" 6 (Html_assert.occurrences sel "<option");
        List.iter
          (fun value ->
            Alcotest.(check int)
              ("one option " ^ value)
              1
              (Html_assert.occurrences sel (Printf.sprintf "<option value='%s'" value)))
          [ "project"; "organization"; "ecosystem"; "foundation";
            "working_group"; "other" ];
        Html_assert.must_not sel "value='working-group'";
        Html_assert.must_not sel "value='github_organization'";
        Html_assert.must_not sel "value='initiative'";
        Html_assert.must sel "A project";
        Html_assert.must sel "A GitHub organization";
        Html_assert.must sel "An ecosystem";
        Html_assert.must sel "A foundation";
        Html_assert.must sel "A working group";
        Html_assert.must sel "Another open-source initiative")
  ; pip_case "exactly the represented kind is selected" (fun () ->
        let sel =
          pip_select (pip_render (Psp.Configure_identity pip_cfg)) "kind"
        in
        Html_assert.must sel "<option value='ecosystem' selected>";
        Alcotest.(check int) "one selected kind" 1 (Html_assert.occurrences sel " selected"))
  ; pip_case "every kind value renders as the selected one" (fun () ->
        List.iter
          (fun kind ->
            let config =
              pip_config ~values:(pip_values ~kind ()) ()
            in
            let sel =
              pip_select (pip_render (Psp.Configure_identity config)) "kind"
            in
            Html_assert.must sel
              (Printf.sprintf "<option value='%s' selected>"
                 (Pi.string_of_kind kind));
            Alcotest.(check int) "one selected kind" 1
              (Html_assert.occurrences sel " selected"))
          Project_setup_fixture.pi_all_kinds)
  ]

let pip_value_cases =
  [ pip_case "values populate their fields exactly" (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must frag
          "<input type='text' name='name' maxlength='120' value='Widgets'>";
        Html_assert.must frag
          "<input type='text' name='slug' maxlength='80' value='widgets'>";
        Html_assert.must frag
          "<textarea name='description' maxlength='2000' rows='6'>A \
           toolkit</textarea>";
        Html_assert.must frag
          "<input type='url' name='website_url' maxlength='2048' \
           value='https://example.com'>";
        (* The website is an editable value, never an actionable link. *)
        Html_assert.must_not frag "href='https://example.com'")
  ; pip_case "hostile values are escaped everywhere" (fun () ->
        let hostile_values =
          pip_values ~name:"x<b>&\"y'" ~slug:"sl<u>g"
            ~description:"a & b <i>italic</i>"
            ~website:"https://example.com/'><img src=x onerror=alert(1)>" ()
        in
        let hostile_repo =
          pip_repo ~id:77L ~full_name:"evil<script>alert(1)</script>" ()
        in
        let frag =
          pip_render
            (Psp.Configure_identity
               (pip_config ~repos:[ hostile_repo ] ~values:hostile_values ()))
        in
        Html_assert.must_not frag "<b>";
        Html_assert.must frag "x&lt;b&gt;&amp;&quot;y&#39;";
        Html_assert.must_not frag "<u>";
        Html_assert.must frag "sl&lt;u&gt;g";
        Html_assert.must_not frag "<i>";
        Html_assert.must frag "a &amp; b &lt;i&gt;italic&lt;/i&gt;";
        (* The repository name is escaped both in the summary and in its
           option label. *)
        Html_assert.must_not frag "<script";
        Alcotest.(check int) "escaped twice" 2
          (Html_assert.occurrences frag "evil&lt;script&gt;");
        (* The hostile URL cannot break out of the value attribute. *)
        Html_assert.must_not frag "'><img";
        Html_assert.must_not frag "<img")
  ]

let pip_primary_cases =
  [ pip_case "blank option plus one option per repository, supplied order"
      (fun () ->
        let sel =
          pip_select
            (pip_render (Psp.Configure_identity pip_cfg))
            "primary_snapshot_id"
        in
        Alcotest.(check int) "four options" 4 (Html_assert.occurrences sel "<option");
        Html_assert.must sel "<option value=''>No primary repository</option>";
        Html_assert.must sel "<option value='101'>octo-org/first</option>";
        Html_assert.must sel
          "<option value='102' selected>octo-org/second (Archived)</option>";
        Html_assert.must sel "<option value='103'>octo-org/legacy</option>";
        Html_assert.order sel "value='101'" "value='102'";
        Html_assert.order sel "value='102'" "value='103'";
        Alcotest.(check int) "one selected option" 1 (Html_assert.occurrences sel " selected"))
  ; pip_case "no primary renders the blank option selected" (fun () ->
        let config = pip_config ~repos:[ pip_r1; pip_r2; pip_r3 ] () in
        let sel =
          pip_select
            (pip_render (Psp.Configure_identity config))
            "primary_snapshot_id"
        in
        Html_assert.must sel "<option value='' selected>No primary repository</option>";
        Alcotest.(check int) "one selected option" 1 (Html_assert.occurrences sel " selected"))
  ; pip_case "archived marker in summary and option label" (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Alcotest.(check int) "one summary archived marker" 1
          (Html_assert.occurrences frag "ps-repo-archived");
        Html_assert.must frag "octo-org/second (Archived)";
        (* Archived repositories stay offered, never disabled. *)
        Html_assert.must_not frag "disabled")
  ]

let pip_separation_cases =
  [ pip_case "no repository checkboxes or hidden snapshot ids" (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must_not frag "type='checkbox'";
        Html_assert.must_not frag "name='repository'";
        Alcotest.(check int) "single hidden field" 1
          (Html_assert.occurrences frag "type='hidden'"))
  ; pip_case "summary lists selected repositories in supplied order"
      (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.order frag "octo-org/first" "octo-org/second";
        Html_assert.order frag "octo-org/second" "octo-org/legacy")
  ; pip_case "structural link back to the repository-selection step"
      (fun () ->
        let frag = pip_render (Psp.Configure_identity pip_cfg) in
        Html_assert.must frag "href='/projects/new?draft=11'";
        Html_assert.must frag "Change repository selection";
        (* No snapshot id rides in the back link: the draft id is the only
           query parameter, and no query key carries a snapshot id. *)
        Html_assert.must_not frag "draft=11&";
        Html_assert.must_not frag "snapshot_id=")
  ]

let pip_corrupt_cases =
  [ pip_case "zero selected repositories renders no identity form" (fun () ->
        let frag =
          pip_render (Psp.Configure_identity (pip_config ~repos:[] ()))
        in
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "type='hidden'";
        Html_assert.must frag "Select at least one repository";
        Html_assert.must frag "href='/projects/new?draft=11'";
        Html_assert.must frag "Change repository selection")
  ; pip_case "non-positive draft id renders no form and leaks no id"
      (fun () ->
        List.iter
          (fun id ->
            let frag =
              pip_render
                (Psp.Configure_identity
                   (pip_config ~draft:(Project_setup_fixture.ps_draft ~id ()) ()))
            in
            Html_assert.must_not frag "<form";
            Html_assert.must_not frag "name='draft_id'";
            Html_assert.must_not frag (Printf.sprintf "draft=%Ld" id);
            Html_assert.must_not frag (Printf.sprintf "value='%Ld'" id))
          [ 0L; -7L ])
  ; pip_case "non-positive snapshot ids never become option values" (fun () ->
        let corrupt_zero = pip_repo ~id:0L ~full_name:"octo-org/czero" () in
        let corrupt_neg =
          pip_repo ~id:(-424242L) ~full_name:"octo-org/cneg" ()
        in
        let frag =
          pip_render
            (Psp.Configure_identity
               (pip_config ~repos:[ pip_r1; corrupt_zero; corrupt_neg ] ()))
        in
        let sel = pip_select frag "primary_snapshot_id" in
        Alcotest.(check int) "blank plus the one valid option" 2
          (Html_assert.occurrences sel "<option");
        Html_assert.must_not frag "value='0'";
        Html_assert.must_not frag "value='-424242'";
        (* Corrupt rows may stay visible as non-actionable information. *)
        Html_assert.must frag "octo-org/czero")
  ; pip_case "all-corrupt repositories render no identity form" (fun () ->
        let frag =
          pip_render
            (Psp.Configure_identity
               (pip_config ~repos:[ pip_repo ~id:0L () ] ()))
        in
        Html_assert.must_not frag "<form";
        Html_assert.must frag "Select at least one repository")
  ; pip_case "unknown selected primary falls back to the blank option"
      (fun () ->
        let config =
          pip_config
            ~repos:[ pip_r1; pip_r3 ]
            ~values:(pip_values ~primary:424242L ())
            ()
        in
        let frag = pip_render (Psp.Configure_identity config) in
        let sel = pip_select frag "primary_snapshot_id" in
        Html_assert.must sel "<option value='' selected>";
        Html_assert.must_not frag "value='424242'";
        Alcotest.(check int) "one selected option" 1 (Html_assert.occurrences sel " selected"))
  ; pip_case "corrupt primary pointing at a corrupt row also falls back"
      (fun () ->
        let config =
          pip_config
            ~repos:[ pip_r1; pip_repo ~id:0L () ]
            ~values:(pip_values ~primary:0L ())
            ()
        in
        let sel =
          pip_select
            (pip_render (Psp.Configure_identity config))
            "primary_snapshot_id"
        in
        Html_assert.must sel "<option value='' selected>";
        Html_assert.must_not sel "value='0'")
  ; pip_case "Configure_identity never raises on malformed view models"
      (fun () ->
        List.iter
          (fun state -> ignore (pip_render state : string))
          [ Psp.Configure_identity (pip_config ~repos:[] ())
          ; Psp.Configure_identity
              (pip_config ~draft:(Project_setup_fixture.ps_draft ~id:(-1L) ()) ~repos:[] ())
          ; Psp.Configure_identity
              (pip_config
                 ~repos:[ pip_repo ~id:0L (); pip_repo ~id:(-2L) () ]
                 ~values:(pip_values ~primary:(-9L) ())
                 ())
          ])
  ]

(* Every identity feedback variant: exact copy, error styling, and generic
   contents — no fixture value, id, or conflicting repository ever reaches
   the alert — while the form contract stays untouched. *)
let pip_feedback_check name feedback ~copy =
  pip_case name (fun () ->
      let frag =
        pip_render ~feedback:(Some feedback) (Psp.Configure_identity pip_cfg)
      in
      Html_assert.must frag copy;
      Html_assert.must frag "ps-alert--error";
      Alcotest.(check int) "one form" 1 (Html_assert.occurrences frag "<form");
      Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
      Alcotest.(check int) "named fields" 7 (Html_assert.occurrences frag " name='");
      let alert = Project_setup_fixture.ps_alert_fragment frag in
      List.iter
        (fun fixture ->
          Alcotest.(check bool) "no fixture value in alert" false
            (Html_assert.contains alert fixture))
        [ "Widgets"; "widgets"; "octo-org"; "https://example.com"; "102";
          "draft" ])

let pip_feedback_cases =
  [ pip_feedback_check "form invalid" Psp.Identity_form_invalid
      ~copy:
        "We couldn't read those project details. Review the form and try \
         again."
  ; pip_feedback_check "name invalid" Psp.Identity_name_invalid
      ~copy:"Enter a valid project name."
  ; pip_feedback_check "slug invalid" Psp.Identity_slug_invalid
      ~copy:
        "Enter a valid project slug using lowercase letters, numbers and \
         single hyphens."
  ; pip_feedback_check "slug reserved" Psp.Identity_slug_reserved
      ~copy:"That project slug is reserved. Choose another one."
  ; pip_feedback_check "description invalid" Psp.Identity_description_invalid
      ~copy:"Enter a valid description of at most 2,000 characters."
  ; pip_feedback_check "website invalid" Psp.Identity_website_invalid
      ~copy:"Enter a valid HTTP or HTTPS website address."
  ; pip_feedback_check "primary invalid" Psp.Identity_primary_invalid
      ~copy:
        "Choose a primary repository from the currently selected \
         repositories."
  ; pip_feedback_check "primary required" Psp.Identity_primary_required
      ~copy:"A project must have a primary repository."
  ; pip_feedback_check "namespace mismatch" Psp.Identity_namespace_mismatch
      ~copy:
        "A GitHub organization project must be connected through a GitHub \
         organization."
  ; pip_feedback_check "slug unavailable" Psp.Identity_slug_unavailable
      ~copy:"That project slug is already in use."
  ; pip_feedback_check "repository already connected"
      Psp.Identity_repository_already_connected
      ~copy:
        "One or more selected repositories are already connected to \
         another Earde project."
  ; pip_feedback_check "creation failed" Psp.Identity_creation_failed
      ~copy:"We couldn't create the project. Try again."
  ; pip_case "no feedback renders no alert container" (fun () ->
        Html_assert.must_not (pip_render (Psp.Configure_identity pip_cfg)) "ps-alert")
  ; pip_case "identity feedback never changes which state renders" (fun () ->
        let frag =
          pip_render
            ~feedback:(Some Psp.Identity_creation_failed)
            (Psp.Configure_identity (pip_config ~repos:[] ()))
        in
        Html_assert.must frag "We couldn't create the project. Try again.";
        Html_assert.must_not frag "<form")
  ]

(* Copy and future-design constraints over the identity states, reusing the
   established matrix checker: factual verification language, no
   endorsement claims, and no style/script/redirect constructs inside the
   feature fragment. *)
let pip_copy_cases =
  let states =
    [ ("identity", Psp.Configure_identity pip_cfg)
    ; ( "identity empty repos"
      , Psp.Configure_identity (pip_config ~repos:[] ()) )
    ; ( "identity corrupt draft"
      , Psp.Configure_identity (pip_config ~draft:(Project_setup_fixture.ps_draft ~id:0L ()) ()) )
    ]
  in
  let feedbacks =
    Project_setup_fixture.psp_copy_feedbacks
    @ [ ("identity form invalid", Some Psp.Identity_form_invalid)
      ; ("identity slug unavailable", Some Psp.Identity_slug_unavailable)
      ; ( "identity repository connected"
        , Some Psp.Identity_repository_already_connected )
      ; ("identity creation failed", Some Psp.Identity_creation_failed)
      ]
  in
  List.concat_map
    (fun state -> List.map (Project_setup_fixture.psp_copy_case state) feedbacks)
    states

let pip_csrf_cases =
  [ pip_case "with request: one framework field inside the identity form"
      (fun () ->
        let frag = Html_assert.panel_fragment (Project_setup_fixture.psc_render (Psp.Configure_identity pip_cfg)) in
        Alcotest.(check int) "one framework field" 1
          (Html_assert.occurrences frag "name=\"dream.csrf\"");
        Alcotest.(check int) "framework field is hidden" 1
          (Html_assert.occurrences frag "type=\"hidden\"");
        Alcotest.(check int) "one application hidden field" 1
          (Html_assert.occurrences frag "type='hidden'");
        Html_assert.must frag "<input type='hidden' name='draft_id' value='11'>";
        match
          ( Html_assert.index_from frag "<form" 0,
            Html_assert.index_from frag "name=\"dream.csrf\"" 0,
            Html_assert.index_from frag "</form>" 0 )
        with
        | Some f, Some c, Some e ->
            Alcotest.(check bool) "CSRF inside the form" true (f < c && c < e)
        | _ -> Alcotest.fail "form or CSRF field missing")
  ; pip_case "pure rendering without a request stays CSRF-free" (fun () ->
        Html_assert.must_not (pip_render (Psp.Configure_identity pip_cfg)) "dream.csrf")
  ; pip_case "form-free identity states emit no framework field" (fun () ->
        List.iter
          (fun state ->
            Html_assert.must_not (Html_assert.panel_fragment (Project_setup_fixture.psc_render state)) "dream.csrf")
          [ Psp.Configure_identity (pip_config ~repos:[] ())
          ; Psp.Configure_identity
              (pip_config ~draft:(Project_setup_fixture.ps_draft ~id:0L ()) ())
          ])
  ]

let suites =
    (* Configure_identity page state: separate project-details step, exact
       application field set, kind/primary option contracts, escaping,
       defensive degradation, identity feedback, copy constraints, and
       framework CSRF only with a live request. *)
  [ ("project_identity_page_structure", pip_structure_cases)
  ; ("project_identity_page_kind", pip_kind_cases)
  ; ("project_identity_page_values", pip_value_cases)
  ; ("project_identity_page_primary", pip_primary_cases)
  ; ("project_identity_page_separation", pip_separation_cases)
  ; ("project_identity_page_corrupt", pip_corrupt_cases)
  ; ("project_identity_page_feedback", pip_feedback_cases)
  ; ("project_identity_page_copy", pip_copy_cases)
  ; ("project_identity_page_csrf", pip_csrf_cases)
  ]
