(* /projects/new — server-rendered page/view models for project setup. Pure
   rendering: the closed state and one-time feedback arrive as arguments (the
   later handler owns session, query, and database reads, and translates the
   draft read model into these view models), so every state renders without a
   server or database.

   Language rules: verification wording stays factual ("verified through
   GitHub") — never an "Official ..." or "GitHub-approved" claim, and nothing
   here claims a community exists or that repository synchronization is
   active. Every caller-controlled string is escaped at the template
   boundary.

   Defensive rendering: view models are expected valid, but malformed ones
   degrade instead of raising — and, critically, a non-positive draft or
   snapshot id is never emitted as a link target or form value, so corrupt
   data cannot become an actionable identifier. *)

type account_type =
  | Personal
  | Organization

type feedback =
  | Selection_saved
  | Selection_stale
  | Selection_invalid
  | Draft_unavailable
  | Repository_selection_required
  | Identity_form_invalid
  | Identity_name_invalid
  | Identity_slug_invalid
  | Identity_slug_reserved
  | Identity_description_invalid
  | Identity_website_invalid
  | Identity_primary_invalid
  | Identity_primary_required
  | Identity_namespace_mismatch
  | Identity_slug_unavailable
  | Identity_repository_already_connected
  | Identity_creation_failed

type draft_option = {
  draft_id : int64;
  account_login : string;
  account_type : account_type;
  repository_count : int;
  selected_repository_count : int;
}

type repository_option = {
  snapshot_id : int64;
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_archived : bool;
  is_selected : bool;
}

type configuration = {
  draft : draft_option;
  repositories : repository_option list;
}

type identity_repository = {
  snapshot_id : int64;
  full_name : string;
  is_archived : bool;
}

type identity_values = {
  kind : Project_identity.kind;
  name : string;
  slug : string;
  description : string;
  website_url : string;
  primary_snapshot_id : int64 option;
}

type identity_configuration = {
  draft : draft_option;
  selected_repositories : identity_repository list;
  values : identity_values;
}

type state =
  | No_available_drafts
  | Choose_draft of draft_option list
  | Configure_repositories of configuration
  | Configure_identity of identity_configuration

let esc = Components.html_escape

let account_type_copy = function
  | Personal -> "Personal account"
  | Organization -> "Organization"

let heading =
  "<div class='create-head'>\
   <h1 class='create-title'>Create a project</h1>\
   <p class='create-sub'>An Earde project is built from public repositories \
   verified through GitHub.</p>\
   </div>"

(* One-time feedback. Cosmetic only — it never changes which state renders —
   and None renders no alert element at all. The stale and unavailable copy
   is deliberately generic: which submitted ids went stale, and whether a
   draft was foreign, expired, terminal, or revoked, stay
   indistinguishable. *)
let feedback_html = function
  | None -> ""
  | Some Selection_saved ->
      "<div class='ps-alert ps-alert--success'>Repository selection \
       saved.</div>"
  | Some Selection_stale ->
      "<div class='ps-alert ps-alert--error'>The GitHub repository list \
       changed. Review the current repositories and save again.</div>"
  | Some Selection_invalid ->
      "<div class='ps-alert ps-alert--error'>We couldn't save that \
       repository selection. Review it and try again.</div>"
  | Some Draft_unavailable ->
      "<div class='ps-alert ps-alert--error'>That project setup is no \
       longer available.</div>"
  | Some Repository_selection_required ->
      "<div class='ps-alert ps-alert--error'>Select at least one public \
       repository before continuing.</div>"
  (* Identity/finalization feedback. All copy is generic by design: no
     submitted value, id, conflicting repository, constraint name, or raw
     error ever reaches an alert. *)
  | Some Identity_form_invalid ->
      "<div class='ps-alert ps-alert--error'>We couldn't read those project \
       details. Review the form and try again.</div>"
  | Some Identity_name_invalid ->
      "<div class='ps-alert ps-alert--error'>Enter a valid project \
       name.</div>"
  | Some Identity_slug_invalid ->
      "<div class='ps-alert ps-alert--error'>Enter a valid project slug \
       using lowercase letters, numbers and single hyphens.</div>"
  | Some Identity_slug_reserved ->
      "<div class='ps-alert ps-alert--error'>That project slug is reserved. \
       Choose another one.</div>"
  | Some Identity_description_invalid ->
      "<div class='ps-alert ps-alert--error'>Enter a valid description of \
       at most 2,000 characters.</div>"
  | Some Identity_website_invalid ->
      "<div class='ps-alert ps-alert--error'>Enter a valid HTTP or HTTPS \
       website address.</div>"
  | Some Identity_primary_invalid ->
      "<div class='ps-alert ps-alert--error'>Choose a primary repository \
       from the currently selected repositories.</div>"
  | Some Identity_primary_required ->
      "<div class='ps-alert ps-alert--error'>A project must have a primary \
       repository.</div>"
  | Some Identity_namespace_mismatch ->
      "<div class='ps-alert ps-alert--error'>A GitHub organization project \
       must be connected through a GitHub organization.</div>"
  | Some Identity_slug_unavailable ->
      "<div class='ps-alert ps-alert--error'>That project slug is already \
       in use.</div>"
  | Some Identity_repository_already_connected ->
      "<div class='ps-alert ps-alert--error'>One or more selected \
       repositories are already connected to another Earde project.</div>"
  | Some Identity_creation_failed ->
      "<div class='ps-alert ps-alert--error'>We couldn't create the \
       project. Try again.</div>"

(* Empty state: no chooser container, no form, no draft id — just the way
   into the onboarding flow. *)
let empty_state_html =
  "<p class='create-sub ps-empty'>There is no GitHub project setup in \
   progress for your account. Connect a project you maintain first; its \
   verified repositories will appear here.</p>\
   <div class='create-foot'><a href='/bring' class='create-link'>Connect a \
   GitHub project</a></div>"

(* Fallback for a corrupt configuration (empty snapshot, non-positive draft
   id): generic, form-free, and distinct from the Draft_unavailable feedback
   copy so the two never masquerade as each other in tests or logs. *)
let corrupt_state_html =
  "<p class='create-sub ps-corrupt'>This project setup can't be shown right \
   now. <a href='/bring' class='create-link'>Connect a GitHub \
   project</a></p>"

(* Structured query construction — the id is server-controlled, but building
   through Uri keeps caller text out of hand-assembled URLs by shape. *)
let draft_href id =
  Uri.to_string
    (Uri.make ~path:"/projects/new"
       ~query:[ ("draft", [ Int64.to_string id ]) ]
       ())

let draft_metrics option =
  (* Negative counts are corrupt, not metrics — clamp instead of render. *)
  let total = max 0 option.repository_count in
  let selected = max 0 option.selected_repository_count in
  let noun = if total = 1 then "repository" else "repositories" in
  if selected > 0 then Printf.sprintf "%d %s, %d selected" total noun selected
  else Printf.sprintf "%d %s" total noun

let draft_option_html option =
  let inner =
    Printf.sprintf
      "<span class='ps-draft-main'>\
       <span class='create-comm-name'>%s</span>\
       <span class='create-comm-slug'>%s</span>\
       <span class='ps-draft-counts'>%s</span>\
       </span>"
      (esc option.account_login)
      (account_type_copy option.account_type)
      (draft_metrics option)
  in
  if Int64.compare option.draft_id 0L > 0 then
    Printf.sprintf
      "<a href='%s' class='create-comm ps-draft-option'>%s<span \
       class='create-comm-go'>Choose repositories &rarr;</span></a>"
      (esc (draft_href option.draft_id))
      inner
  else
    (* A non-positive id can never name a real draft row; a link would hand
       out an actionable bogus identifier. *)
    Printf.sprintf
      "<div class='ps-draft-option ps-draft-option--unavailable'>%s</div>"
      inner

(* No option is ever selected automatically: a maintainer may legitimately
   hold one draft per installation, so every draft is offered equally. *)
let chooser_html options =
  Printf.sprintf
    "<p class='create-sub ps-chooser-intro'>Choose which GitHub project \
     setup to continue.</p>\
     <div class='create-list ps-draft-list'>%s</div>"
    (String.concat "\n" (List.map draft_option_html options))

let repository_details_html (repository : repository_option) =
  let archived =
    if repository.is_archived then
      "<span class='ps-repo-archived'>Archived</span>"
    else ""
  in
  let description =
    match repository.description with
    | Some text -> Printf.sprintf "<p class='ps-repo-desc'>%s</p>" (esc text)
    | None -> ""
  in
  (* default_branch may contain '/' — text only, never a URL segment. The
     html_url is the validated canonical GitHub URL, still gated and escaped
     normally (safe_url) like any other href. *)
  Printf.sprintf
    "%s%s<p class='ps-repo-meta'>Default branch <code \
     class='ps-repo-branch'>%s</code> &middot; <a href='%s' \
     class='create-link'>View on GitHub</a></p>"
    archived description
    (esc repository.default_branch)
    (Components.safe_url repository.html_url)

let repository_row_html (repository : repository_option) =
  if Int64.compare repository.snapshot_id 0L > 0 then
    Printf.sprintf
      "<li class='ps-repo'><label class='ps-repo-pick'><input \
       type='checkbox' name='repository' value='%Ld'%s><span \
       class='ps-repo-name'>%s</span></label>%s</li>"
      repository.snapshot_id
      (if repository.is_selected then " checked" else "")
      (esc repository.full_name)
      (repository_details_html repository)
  else
    (* A non-positive snapshot id gets no form control: the row stays
       visible, but nothing actionable carries the corrupt id. *)
    Printf.sprintf
      "<li class='ps-repo ps-repo--unavailable'><span \
       class='ps-repo-name'>%s</span>%s</li>"
      (esc repository.full_name)
      (repository_details_html repository)

(* The one selection form. Its only application-owned hidden field is
   draft_id: no user, installation, account, GitHub repository, state, token,
   or redirect field — the POST handler re-derives everything from the
   session and re-authorizes the draft. When a request is supplied, Dream's
   own CSRF hidden field (Dream.csrf_tag) is emitted additionally; it is
   framework-owned, carries no GitHub or draft-derived secret, and the form
   layer strips it before the strict application parser runs. Pure rendering
   calls (no request) omit it, keeping the page testable without a server.
   The submit button has no name, so an empty selection submits as draft_id
   alone. Archived repositories stay fully selectable: archived is
   information, not a policy this page enforces. *)
let configuration_html ?request (configuration : configuration) =
  if
    Int64.compare configuration.draft.draft_id 0L <= 0
    || configuration.repositories = []
  then corrupt_state_html
  else
    let csrf_field =
      match request with
      | None -> ""
      | Some request -> Dream.csrf_tag request
    in
    Printf.sprintf
      "<p class='create-sub ps-configure-intro'>Setting up \
       <strong>%s</strong> (%s). Choose which of these public repositories \
       belong to the Earde project.</p>\
       <form method='POST' action='/projects/new/repositories' \
       class='create-form ps-repo-form'>\
       %s<input type='hidden' name='draft_id' value='%Ld'>\
       <ul class='ps-repo-list'>%s</ul>\
       <div class='create-actions'><button type='submit' class='create-btn \
       create-btn--block'>Save repository selection</button></div>\
       </form>"
      (esc configuration.draft.account_login)
      (account_type_copy configuration.draft.account_type)
      csrf_field configuration.draft.draft_id
      (String.concat "\n"
         (List.map repository_row_html configuration.repositories))

(* --- Project-identity step ------------------------------------------------ *)

(* Canonical wire values only (string_of_kind); labels stay descriptive
   without endorsement language. *)
let identity_kind_labels =
  [ (Project_identity.Project, "A project")
  ; (Project_identity.Organization, "A GitHub organization")
  ; (Project_identity.Ecosystem, "An ecosystem")
  ; (Project_identity.Foundation, "A foundation")
  ; (Project_identity.Working_group, "A working group")
  ; (Project_identity.Other, "Another open-source initiative")
  ]

let identity_kind_option_html ~selected (kind, label) =
  Printf.sprintf "<option value='%s'%s>%s</option>"
    (Project_identity.string_of_kind kind)
    (if kind = selected then " selected" else "")
    label

(* Read-only summary row: display data only, never a form control. Corrupt
   (non-positive) snapshot ids are harmless here — nothing actionable
   carries an id. *)
let identity_summary_row_html (repository : identity_repository) =
  let archived =
    if repository.is_archived then
      " <span class='ps-repo-archived'>Archived</span>"
    else ""
  in
  Printf.sprintf "<li class='ps-id-repo'><span class='ps-repo-name'>%s</span>%s</li>"
    (esc repository.full_name)
    archived

let identity_primary_option_html ~selected (repository : identity_repository) =
  let chosen =
    match selected with
    | Some id when Int64.equal id repository.snapshot_id -> " selected"
    | Some _ | None -> ""
  in
  let archived = if repository.is_archived then " (Archived)" else "" in
  Printf.sprintf "<option value='%Ld'%s>%s%s</option>"
    repository.snapshot_id chosen
    (esc repository.full_name)
    archived

(* The identity step keeps repository selection and project identity
   visibly separate: a read-only selected-repository summary plus one POST
   /projects form whose only application-owned hidden field is draft_id. The
   selected set is server-owned — the later handler re-derives it from the
   owner-authorized draft read model, so no snapshot id ever rides in a
   hidden field. Values are prefills or a failed submission being
   re-rendered; both escape normally and are never rewritten here (no slug
   generation, no inferred primary — the domain deliberately never infers
   one, even from a single selected repository). *)
let identity_form_html ?request ~draft_id ~(values : identity_values)
    valid_repositories =
  let csrf_field =
    match request with
    | None -> ""
    | Some request -> Dream.csrf_tag request
  in
  (* A submitted primary that no longer names a rendered repository falls
     back to the blank option — an arbitrary actionable value is never
     emitted for it. *)
  let selected_primary =
    match values.primary_snapshot_id with
    | Some id
      when List.exists
             (fun (r : identity_repository) -> Int64.equal id r.snapshot_id)
             valid_repositories ->
        Some id
    | Some _ | None -> None
  in
  Printf.sprintf
    "<form method='POST' action='/projects' class='create-form ps-id-form'>\
     %s<input type='hidden' name='draft_id' value='%Ld'>\
     <label class='ps-id-field'>Project kind\
     <select name='kind'>%s</select></label>\
     <label class='ps-id-field'>Project name\
     <input type='text' name='name' maxlength='120' value='%s'></label>\
     <label class='ps-id-field'>Project slug\
     <input type='text' name='slug' maxlength='80' value='%s'></label>\
     <p class='ps-id-help'>The slug becomes the stable Earde identifier for \
     this project. Availability is confirmed when the project is \
     created.</p>\
     <label class='ps-id-field'>Description\
     <textarea name='description' maxlength='2000' rows='6'>%s</textarea>\
     </label>\
     <label class='ps-id-field'>Website\
     <input type='url' name='website_url' maxlength='2048' value='%s'>\
     </label>\
     <label class='ps-id-field'>Primary repository\
     <select name='primary_snapshot_id'>\
     <option value=''%s>No primary repository</option>%s</select></label>\
     <p class='ps-id-help'>A single project requires a primary repository. \
     Organization, ecosystem, foundation, working group and other setups \
     may leave it empty.</p>\
     <div class='create-actions'><button type='submit' class='create-btn \
     create-btn--block'>Create project</button></div>\
     </form>"
    csrf_field draft_id
    (String.concat ""
       (List.map
          (identity_kind_option_html ~selected:values.kind)
          identity_kind_labels))
    (esc values.name) (esc values.slug) (esc values.description)
    (esc values.website_url)
    (if selected_primary = None then " selected" else "")
    (String.concat ""
       (List.map
          (identity_primary_option_html ~selected:selected_primary)
          valid_repositories))

let identity_step_heading =
  "<h2 class='ps-id-title'>Project details</h2>\
   <p class='create-sub ps-id-intro'>Describe the verified project before \
   choosing its community home.</p>"

let configure_identity_html ?request (configuration : identity_configuration)
    =
  if Int64.compare configuration.draft.draft_id 0L <= 0 then
    (* A non-positive draft id can never name a real row; no form or link
       may carry it. *)
    corrupt_state_html
  else
    let back_link =
      Printf.sprintf
        "<a href='%s' class='create-link'>Change repository selection</a>"
        (esc (draft_href configuration.draft.draft_id))
    in
    let valid_repositories =
      List.filter
        (fun (r : identity_repository) -> Int64.compare r.snapshot_id 0L > 0)
        configuration.selected_repositories
    in
    if valid_repositories = [] then
      (* Nothing to build a project from: no identity form — only the way
         back to the repository-selection step. *)
      Printf.sprintf
        "%s<p class='create-sub ps-id-no-repos'>Select at least one \
         repository verified through GitHub before describing the \
         project.</p>\
         <div class='create-foot'>%s</div>"
        identity_step_heading back_link
    else
      Printf.sprintf
        "%s<div class='ps-id-summary'>\
         <p class='ps-id-verified'>Verified through GitHub</p>\
         <ul class='ps-id-repo-list'>%s</ul>\
         <p class='ps-id-change'>%s</p>\
         </div>%s"
        identity_step_heading
        (String.concat "\n"
           (List.map identity_summary_row_html
              configuration.selected_repositories))
        back_link
        (identity_form_html ?request
           ~draft_id:configuration.draft.draft_id
           ~values:configuration.values valid_repositories)

let state_html ?request = function
  | No_available_drafts | Choose_draft [] -> empty_state_html
  | Choose_draft options -> chooser_html options
  | Configure_repositories configuration ->
      configuration_html ?request configuration
  | Configure_identity configuration ->
      configure_identity_html ?request configuration

(* Launch onboarding stepper (Cartographic Civic, 04-ROUTES): the full
   approved five-step sequence, truthfully positioned. This pass renders only
   the two states this route really owns — the repository-selection step
   (GitHub active) and the project-details step (GitHub done, Project active);
   later steps are plain upcoming dots, never links, and nothing is marked
   done before its operation actually committed. Rendered by the launch
   wrapper BEFORE the immutable create-shell fragment, so the byte-exact
   fragment the test suites slice is untouched. Markup only — no form, no
   field, no script, no inline style. *)
let stepper_html state =
  let labels = [ "GitHub"; "Project"; "Home"; "Configure"; "Complete" ] in
  let active =
    match state with
    | Configure_identity _ -> 1
    | No_available_drafts | Choose_draft _ | Configure_repositories _ -> 0
  in
  let step index label =
    let dot_class, dot_text =
      if index < active then ("step__dot step__dot--done", "&#10003;")
      else if index = active then
        ("step__dot step__dot--active", string_of_int (index + 1))
      else ("step__dot", string_of_int (index + 1))
    in
    let label_class =
      if index = active then "step__label step__label--active"
      else "step__label"
    in
    Printf.sprintf
      "<li class='step'><span class='%s'>%s</span><span class='%s'>%s</span></li>"
      dot_class dot_text label_class label
  in
  Printf.sprintf "<ol class='steps' aria-label='Project onboarding steps'>%s</ol>"
    (String.concat "<li class='step__rule' aria-hidden='true'></li>"
       (List.mapi step labels))

let project_setup_page ?user ?request ~state ~feedback () =
  let body =
    Printf.sprintf
      "<div class='create-wrap project-setup'><div \
       class='create-panel'>%s%s%s</div></div>"
      heading (feedback_html feedback) (state_html ?request state)
  in
  (* noindex: a session-dependent setup surface — not for search indexes. *)
  Components.launch_onboarding_page ?user ?request ~noindex:true
    ~stepper:(stepper_html state) ~page_class:"launch-project-new"
    ~title:"Create a project" ~content:body ()
