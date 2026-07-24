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

type state =
  | No_available_drafts
  | Choose_draft of draft_option list
  | Configure_repositories of configuration

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

let repository_details_html repository =
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

let repository_row_html repository =
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

(* The one selection form. Its only hidden field is draft_id: no user,
   installation, account, GitHub repository, state, token, or redirect field
   — the POST handler re-derives everything from the session and
   re-authorizes the draft. The submit button has no name, so an empty
   selection submits as draft_id alone. Archived repositories stay fully
   selectable: archived is information, not a policy this page enforces. *)
let configuration_html configuration =
  if
    Int64.compare configuration.draft.draft_id 0L <= 0
    || configuration.repositories = []
  then corrupt_state_html
  else
    Printf.sprintf
      "<p class='create-sub ps-configure-intro'>Setting up \
       <strong>%s</strong> (%s). Choose which of these public repositories \
       belong to the Earde project.</p>\
       <form method='POST' action='/projects/new/repositories' \
       class='create-form ps-repo-form'>\
       <input type='hidden' name='draft_id' value='%Ld'>\
       <ul class='ps-repo-list'>%s</ul>\
       <div class='create-actions'><button type='submit' class='create-btn \
       create-btn--block'>Save repository selection</button></div>\
       </form>"
      (esc configuration.draft.account_login)
      (account_type_copy configuration.draft.account_type)
      configuration.draft.draft_id
      (String.concat "\n"
         (List.map repository_row_html configuration.repositories))

let state_html = function
  | No_available_drafts | Choose_draft [] -> empty_state_html
  | Choose_draft options -> chooser_html options
  | Configure_repositories configuration -> configuration_html configuration

let project_setup_page ?user ?request ~state ~feedback () =
  let body =
    Printf.sprintf
      "<div class='create-wrap project-setup'><div \
       class='create-panel'>%s%s%s</div></div>"
      heading (feedback_html feedback) (state_html state)
  in
  (* noindex: a session-dependent setup surface — not for search indexes. *)
  Components.create_page ?user ?request ~noindex:true ~title:"Create a project"
    ~body ()
