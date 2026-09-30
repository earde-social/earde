(* Project setup page fixtures shared by the setup and identity cases. *)

module Psp = Earde.Project_setup_pages
module Pi = Earde.Project_identity

let render_ps ?user ?(feedback = None) state =
  Psp.project_setup_page ?user ~state ~feedback ()

let psp_case name f = Alcotest.test_case name `Quick f

let ps_draft ?(id = 11L) ?(login = "octo-org") ?(atype = Psp.Organization)
    ?(repos = 3) ?(selected = 0) () : Psp.draft_option =
  { Psp.draft_id = id; account_login = login; account_type = atype;
    repository_count = repos; selected_repository_count = selected }

let ps_repo ?(id = 501L) ?(full_name = "octo-org/widgets")
    ?(url = "https://github.com/octo-org/widgets") ?description
    ?(branch = "main") ?(archived = false) ?(selected = false) () :
    Psp.repository_option =
  { Psp.snapshot_id = id; full_name; html_url = url; description;
    default_branch = branch; is_archived = archived; is_selected = selected }

let ps_config ?draft ?(repos = [ ps_repo () ]) () : Psp.configuration =
  { Psp.draft = (match draft with Some d -> d | None -> ps_draft ());
    repositories = repos }

let ps_chooser_a =
  ps_draft ~id:5L ~login:"alpha-dev" ~atype:Psp.Personal ~repos:1 ~selected:0
    ()

let ps_chooser_b =
  ps_draft ~id:9L ~login:"beta-org" ~atype:Psp.Organization ~repos:12
    ~selected:4 ()

let ps_cfg_draft = ps_draft ~id:11L ~login:"octo-org" ()

let ps_r1 =
  ps_repo ~id:101L ~full_name:"octo-org/first"
    ~url:"https://github.com/octo-org/first" ~selected:true ()

let ps_r2 =
  ps_repo ~id:102L ~full_name:"octo-org/second"
    ~url:"https://github.com/octo-org/second" ~description:"A second tool"
    ~branch:"release/2.x" ()

let ps_r3 =
  ps_repo ~id:103L ~full_name:"octo-org/legacy"
    ~url:"https://github.com/octo-org/legacy" ~archived:true ()

let ps_cfg = ps_config ~draft:ps_cfg_draft ~repos:[ ps_r1; ps_r2; ps_r3 ] ()

(* Alert element contents in isolation: copy only, no fixture data. *)
let ps_alert_fragment frag =
  match Html_assert.index_from frag "<div class='ps-alert" 0 with
  | None -> Alcotest.fail "expected an alert element"
  | Some s -> (
      match Html_assert.index_from frag "</div>" s with
      | None -> Alcotest.fail "unterminated alert element"
      | Some e -> String.sub frag s (e - s))

let psp_copy_feedbacks =
  [ ("no feedback", None)
  ; ("saved", Some Psp.Selection_saved)
  ; ("stale", Some Psp.Selection_stale)
  ; ("invalid", Some Psp.Selection_invalid)
  ; ("unavailable", Some Psp.Draft_unavailable)
  ; ("required", Some Psp.Repository_selection_required)
  ]

let psp_copy_case (state_name, state) (feedback_name, feedback) =
  psp_case
    (Printf.sprintf "%s, %s" state_name feedback_name)
    (fun () ->
      let frag = Html_assert.panel_fragment (render_ps ~feedback state) in
      let lower = String.lowercase_ascii frag in
      let must_ci s =
        Alcotest.(check bool) ("contains: " ^ s) true
          (Html_assert.contains lower (String.lowercase_ascii s))
      in
      let must_not_ci s =
        Alcotest.(check bool) ("must not contain: " ^ s) false
          (Html_assert.contains lower (String.lowercase_ascii s))
      in
      must_ci "Create a project";
      must_ci "verified through GitHub";
      must_not_ci "official project";
      must_not_ci "official community";
      must_not_ci "official home";
      must_not_ci "GitHub-approved";
      must_not_ci "GitHub-endorsed";
      must_not_ci "<style";
      must_not_ci "style=";
      must_not_ci "<script";
      must_not_ci "<meta http-equiv";
      must_not_ci "window.location";
      must_not_ci "location.href")

let psc_render state =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some (Psp.project_setup_page ~request:req ~state ~feedback:None ());
    Dream.html ""
  in
  ignore
    (Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target:"/projects/new" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

let pi_all_kinds =
  [ Pi.Project
  ; Pi.Organization
  ; Pi.Ecosystem
  ; Pi.Foundation
  ; Pi.Working_group
  ; Pi.Other
  ]
