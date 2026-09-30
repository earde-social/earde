open Html.Infix

(* /projects/:slug/setup — server-rendered permanent project-home setup
   page. Pure rendering over handler-supplied view models.

   Language rules: verification wording stays factual ("Project connected
   through GitHub") — never an "Official ..." or "GitHub-approved" claim,
   and nothing here claims a community home exists yet. Every
   caller-controlled string is escaped at the template boundary, and a URL
   that fails the shared HTTP(S) gate degrades to plain text rather than
   becoming an actionable link. *)

type account_type = Personal | Organization

type repository = {
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_primary : bool;
  is_archived : bool;
}

type project = {
  name : string;
  slug : string;
  description : string option;
  website_url : string option;
  kind : Project_identity.kind;
  namespace_login : string;
  namespace_type : account_type;
  repositories : repository list;
}

let account_type_copy = function
  | Personal -> Html.static "Personal account"
  | Organization -> Html.static "Organization"

(* Descriptive kind labels without endorsement language, matching the
   choices offered on the identity step. *)
let kind_copy = function
  | Project_identity.Project -> Html.static "Project"
  | Project_identity.Organization -> Html.static "GitHub organization"
  | Project_identity.Ecosystem -> Html.static "Ecosystem"
  | Project_identity.Foundation -> Html.static "Foundation"
  | Project_identity.Working_group -> Html.static "Working group"
  | Project_identity.Other -> Html.static "Other open-source initiative"

(* An href is emitted only when the shared gate passes the URL; a value
   that fails the gate degrades to the plain display text, so corrupt data
   can never become an actionable link. *)
let linked_or_text ~link_class url text_html =
  match Html.external_url_opt url with
  | None -> Html.template "<span class='phs-unlinked'>%s</span>" [ text_html ]
  | Some safe ->
      Html.template "<a href='%s' class='%s'>%s</a>"
        [ safe; Html.text link_class; text_html ]

let heading =
  Html.static
    "<div class='create-head'><h1 class='create-title'>Project created</h1><p \
     class='create-sub phs-verified'>Project connected through \
     GitHub</p></div>"

let summary_html project =
  let optional_rows =
    (match project.description with
      | None -> Html.empty
      | Some text ->
          Html.template
            "<dt>Description</dt><dd class='phs-description'>%s</dd>"
            [ Html.text text ])
    ++
    (* The website row exists only for a URL the shared HTTP(S) gate
       accepts: a corrupt scheme is dropped whole rather than rendered as
       text, so nothing URL-shaped and unvetted ever enters the page. *)
    match project.website_url with
    | None -> Html.empty
    | Some url -> (
        match Html.external_url_opt url with
        | None -> Html.empty
        | Some safe ->
            Html.template
              "<dt>Website</dt><dd class='phs-website'><a href='%s' \
               class='create-link'>%s</a></dd>"
              [ safe; Html.text url ])
  in
  Html.template
    "<section class='phs-summary'><h2 class='phs-name'>%s</h2><dl \
     class='phs-facts'><dt>Earde identifier</dt><dd><code \
     class='phs-slug'>%s</code></dd><dt>Kind</dt><dd \
     class='phs-kind'>%s</dd><dt>GitHub namespace</dt><dd \
     class='phs-namespace'>%s (%s)</dd>%s</dl></section>"
    [
      Html.text project.name;
      Html.text project.slug;
      kind_copy project.kind;
      Html.text project.namespace_login;
      account_type_copy project.namespace_type;
      optional_rows;
    ]

let repository_html repository =
  let markers =
    (if repository.is_primary then
       Html.static "<span class='phs-repo-primary'>Primary</span>"
     else Html.empty)
    ++
    if repository.is_archived then
      Html.static "<span class='phs-repo-archived'>Archived</span>"
    else Html.empty
  in
  let description =
    match repository.description with
    | Some text ->
        Html.template "<p class='phs-repo-desc'>%s</p>" [ Html.text text ]
    | None -> Html.empty
  in
  (* default_branch may contain '/' — text only, never a URL segment. *)
  Html.template
    "<li class='phs-repo'><span class='phs-repo-name'>%s</span>%s%s<p \
     class='phs-repo-meta'>Default branch <code \
     class='phs-repo-branch'>%s</code></p></li>"
    [
      linked_or_text ~link_class:"create-link phs-repo-link" repository.html_url
        (Html.text repository.full_name);
      markers;
      description;
      Html.text repository.default_branch;
    ]

let repositories_html repositories =
  Html.template
    "<section class='phs-repos'><h2 \
     class='phs-repos-title'>Repositories</h2><ul \
     class='phs-repo-list'>%s</ul></section>"
    [ (Html.join (Html.static "\n")) (List.map repository_html repositories) ]

(* The same canonical grammar the request-home route and read model
   require. A project slug outside it never becomes a navigation link. *)
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

(* The continuation: two real, visibly distinct navigation steps — connect
   to an existing community, or start a dedicated one. Both are plain links,
   never forms: each destination authorizes independently and refuses a
   project that already has an active home relation. Both are built only
   over a canonical slug, so corrupt data never becomes actionable. *)
let next_step_html project =
  let links =
    if valid_project_slug project.slug then
      Html.template
        "<p class='phs-next-connect'><a href='%s' class='create-link \
         phs-next-request-link'>Connect to an existing community</a></p><p \
         class='phs-next-create'><a href='%s' class='create-link \
         phs-next-create-link'>Create a community home</a></p>"
        [
          Html.internal_path ("/projects/" ^ project.slug ^ "/request-home");
          Html.internal_path
            ("/projects/" ^ project.slug ^ "/community-home/new");
        ]
    else Html.empty
  in
  Html.template
    "<section class='phs-next'><h2 class='phs-next-title'>Choose a community \
     home</h2><p class='create-sub phs-next-copy'>Connect this project to an \
     existing Earde community, or create a community home for it.</p>%s<p \
     class='create-sub phs-next-freshness'>Both need a GitHub verification \
     from the last 30 days. If yours is older, <a href='/bring'>connect the \
     project through GitHub again</a> to renew it.</p></section>"
    [ links ]

(* Launch onboarding stepper (Cartographic Civic, 04-ROUTES): the same
   five-step sequence the /projects/new wrapper renders, truthfully
   positioned for this destination — GitHub and Project are committed by the
   time this page can render at all, Home is the live decision, and the later
   steps stay plain upcoming dots, never links. Rendered by the launch
   wrapper BEFORE the immutable create-shell fragment, so the byte-exact
   fragment the test suites slice is untouched. Markup only — no form, no
   field, no script, no inline style. *)
let stepper_html =
  let labels =
    List.map Html.text [ "GitHub"; "Project"; "Home"; "Configure"; "Complete" ]
  in
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
    Html.template
      "<li class='step'><span class='%s'>%s</span><span \
       class='%s'>%s</span></li>"
      [ Html.text dot_class; Html.text dot_text; Html.text label_class; label ]
  in
  Html.template
    "<ol class='steps' aria-label='Project onboarding steps'>%s</ol>"
    [
      Html.join
        (Html.static "<li class='step__rule' aria-hidden='true'></li>")
        (List.mapi step labels);
    ]

let project_home_setup_page ?user ?request ~project () =
  let body =
    Html.template
      "<div class='create-wrap project-home-setup'><div \
       class='create-panel'>%s%s%s%s</div></div>"
      [
        heading;
        summary_html project;
        repositories_html project.repositories;
        next_step_html project;
      ]
  in
  (* noindex: a steward-only setup surface — not for search indexes. *)
  Page_shell.launch_onboarding_page ?user ?request ~noindex:true
    ~stepper:stepper_html ~page_class:"launch-project-home-setup"
    ~title:"Project created" ~content:body ()
