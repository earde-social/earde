(* /projects/:slug/community-home/new — server-rendered steward page for
   starting a dedicated community home. Pure rendering over handler-supplied
   view models: no Caqti, no read-model dependency, no session access.

   Language rules: verification wording stays factual ("Project connected
   through GitHub") — never an "Official ..." or "GitHub-approved" claim —
   nothing here says a draft community already exists, and the publication
   copy names only the two choices publication actually offers. Every
   caller-controlled string is escaped at the template boundary, and a
   project slug outside the canonical permanent shape suppresses the form and
   every project-derived link rather than producing an unusable action. *)

type project = {
  name : string;
  slug : string;
  description : string option;
  kind : Project_identity.kind;
  namespace_login : string;
}

type form_values = {
  community_name : string;
  community_slug : string;
  community_description : string;
}

type feedback =
  | Stale_form
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description
  | Community_slug_unavailable
  | Active_home_exists
  | Provisioning_failed

let esc = Components.html_escape

(* Descriptive kind labels without endorsement language, identical to the
   permanent setup page's vocabulary. *)
let kind_copy = function
  | Project_identity.Project -> "Project"
  | Project_identity.Organization -> "GitHub organization"
  | Project_identity.Ecosystem -> "Ecosystem"
  | Project_identity.Foundation -> "Foundation"
  | Project_identity.Working_group -> "Working group"
  | Project_identity.Other -> "Other open-source initiative"

(* The same canonical grammar the route and read model require. A project
   slug outside it never reaches an action attribute or an app link. *)
let valid_project_slug value =
  let length = String.length value in
  let is_alnum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  let rec check i =
    i >= length
    ||
    match value.[i] with
    | c when is_alnum c -> check (i + 1)
    | '-' -> i > 0 && is_alnum value.[i - 1] && check (i + 1)
    | _ -> false
  in
  length >= 1 && length <= 80 && is_alnum value.[length - 1] && check 0

(* One generic message per rejected outcome. None names a submitted value, a
   length, or which durable row lost a race. *)
let feedback_copy = function
  | Stale_form ->
      "This page had been open too long, so the form could no longer be \
       submitted. Nothing was created. Fill it in again and submit."
  | Invalid_form ->
      "We couldn't read that submission. Review the form and try again."
  | Invalid_community_name -> "Enter a community name we can use."
  | Invalid_community_slug ->
      "Enter a community address using lowercase letters, numbers, and \
       single hyphens."
  | Invalid_community_description ->
      "That description can't be used. Edit it and try again."
  | Community_slug_unavailable ->
      "That community address is already taken. Choose another one."
  | Active_home_exists ->
      "This project already has a pending request or community home."
  | Provisioning_failed -> "We couldn't create the community. Try again."

let feedback_html = function
  | None -> ""
  | Some feedback ->
      Printf.sprintf "<div class='phv-alert'><p>%s</p></div>"
        (feedback_copy feedback)

let heading_html =
  "<div class='create-head'>\
   <h1 class='create-title'>Create a community home</h1>\
   <p class='create-sub phv-intro'>Create a private setup draft for this \
   project, then configure and publish it as Public or Unlisted.</p>\
   <p class='phv-verified'>Project connected through GitHub</p>\
   </div>"

(* The project this home would belong to, as plain identity. The description
   is escaped text — never Markdown, never HTML. *)
let project_html (project : project) =
  let description =
    match project.description with
    | None -> ""
    | Some text ->
        Printf.sprintf "<p class='phv-project-desc'>%s</p>" (esc text)
  in
  Printf.sprintf
    "<section class='phv-project'>\
     <h2 class='phv-project-name'>%s</h2>\
     <p class='phv-project-meta'>%s &middot; %s</p>%s\
     </section>"
    (esc project.name)
    (kind_copy project.kind)
    (esc project.namespace_login)
    description

(* What creating the home does and does not do. Publication is named as a
   later explicit action with exactly the two choices it offers, so nobody
   expects a fully private published community. *)
let explanation_html =
  "<section class='phv-explain'>\
   <h2 class='phv-explain-title'>What happens next</h2>\
   <ul class='phv-explain-list'>\
   <li>The community is created as a private setup draft. It is not public \
   yet.</li>\
   <li>Only authorized setup users can reach it before publication.</li>\
   <li>You become the community&#39;s initial top moderator as this \
   project&#39;s steward.</li>\
   <li>Publication is a separate, later step you take yourself.</li>\
   <li>When you publish, you choose Public or Unlisted. A published network \
   community keeps a public home, with private rooms available inside \
   it.</li>\
   <li>The project is connected to the new community as its home at the same \
   moment the community is created.</li>\
   </ul>\
   </section>"

(* The one form. No application-owned hidden field exists: the route path
   supplies the project slug, and the future POST handler re-derives the user
   from the session and reauthorizes everything through the provisioning
   store. When a request is supplied, Dream's own CSRF hidden field is
   emitted additionally; pure rendering calls omit it, keeping the page
   testable without a server.

   Maximum lengths mirror the server-side policy exactly, so the control
   never invites a value the parser would reject. *)
let form_html ?request ~project_slug ~(values : form_values) () =
  let csrf_field =
    match request with
    | None -> ""
    | Some request -> Dream.csrf_tag request
  in
  Printf.sprintf
    "<form method='POST' action='/projects/%s/community-home' \
     class='create-form phv-form'>%s\
     <label class='phv-field'>Community name\
     <input type='text' name='community_name' maxlength='120' value='%s'>\
     </label>\
     <label class='phv-field'>Community address\
     <input type='text' name='community_slug' maxlength='80' value='%s'>\
     </label>\
     <p class='phv-hint'>The community will live at /c/&lt;address&gt;. Use \
     lowercase letters, numbers, and single hyphens.</p>\
     <label class='phv-field'>Community description\
     <textarea name='community_description' maxlength='2000' rows='6'>%s\
     </textarea></label>\
     <div class='create-actions'><button type='submit' class='create-btn \
     create-btn--block'>Create private draft</button></div>\
     </form>"
    (esc project_slug) csrf_field
    (esc values.community_name)
    (esc values.community_slug)
    (esc values.community_description)

let setup_link_html (project : project) =
  Printf.sprintf
    "<p class='phv-back'><a href='%s' class='create-link'>Back to project \
     setup</a></p>"
    (Components.safe_internal_path ("/projects/" ^ project.slug ^ "/setup"))

(* Without a canonical project slug there is no POST target to build and no
   project-derived link to offer; the page degrades to its copy alone rather
   than emitting an action that could not work. *)
let actions_html ?request ~(project : project) ~values () =
  if valid_project_slug project.slug then
    form_html ?request ~project_slug:project.slug ~values ()
    ^ setup_link_html project
  else ""

(* Launch onboarding stepper (Cartographic Civic, 04-ROUTES): the same
   five-step sequence the /projects/new wrapper renders, truthfully
   positioned for this branch — GitHub and Project are committed by the time
   this page can render at all, Home is the live decision (this route is one
   of its two branches), and the later steps stay plain upcoming dots, never
   links. Rendered by the launch wrapper BEFORE the immutable create-shell
   fragment, so the byte-exact fragment the test suites slice is untouched.
   Markup only — no form, no field, no script, no inline style. *)
let stepper_html =
  let labels = [ "GitHub"; "Project"; "Home"; "Configure"; "Complete" ] in
  let active = 2 in
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

let project_home_provisioning_page ?user ?request ~project ~values ~feedback ()
    =
  let body =
    Printf.sprintf
      "<div class='create-wrap project-home-provisioning'><div \
       class='create-panel'>%s%s%s%s%s</div></div>"
      (feedback_html feedback) heading_html (project_html project)
      explanation_html
      (actions_html ?request ~project ~values ())
  in
  (* noindex: a steward-only setup surface — not for search indexes. *)
  Page_shell.launch_onboarding_page ?user ?request ~noindex:true
    ~stepper:stepper_html ~page_class:"launch-project-community-home"
    ~title:"Create a community home" ~content:body ()
