(* /projects/:slug/setup — server-rendered permanent project-home setup
   page. Pure rendering over handler-supplied view models.

   Language rules: verification wording stays factual ("Project connected
   through GitHub") — never an "Official ..." or "GitHub-approved" claim,
   and nothing here claims a community home exists yet. Every
   caller-controlled string is escaped at the template boundary, and a URL
   that fails the shared HTTP(S) gate degrades to plain text rather than
   becoming an actionable link. *)

type account_type =
  | Personal
  | Organization

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

let esc = Components.html_escape

let account_type_copy = function
  | Personal -> "Personal account"
  | Organization -> "Organization"

(* Descriptive kind labels without endorsement language, matching the
   choices offered on the identity step. *)
let kind_copy = function
  | Project_identity.Project -> "Project"
  | Project_identity.Organization -> "GitHub organization"
  | Project_identity.Ecosystem -> "Ecosystem"
  | Project_identity.Foundation -> "Foundation"
  | Project_identity.Working_group -> "Working group"
  | Project_identity.Other -> "Other open-source initiative"

(* An href is emitted only when the shared gate passes the URL; a value
   that fails the gate degrades to the plain display text, so corrupt data
   can never become an actionable link. *)
let linked_or_text ~link_class url text_html =
  let safe = Components.safe_url url in
  if String.equal safe "#" then
    Printf.sprintf "<span class='phs-unlinked'>%s</span>" text_html
  else Printf.sprintf "<a href='%s' class='%s'>%s</a>" safe link_class text_html

let heading =
  "<div class='create-head'>\
   <h1 class='create-title'>Project created</h1>\
   <p class='create-sub phs-verified'>Project connected through GitHub</p>\
   </div>"

let summary_html project =
  let optional_rows =
    (match project.description with
    | None -> ""
    | Some text ->
        Printf.sprintf
          "<dt>Description</dt><dd class='phs-description'>%s</dd>" (esc text))
    ^
    (* The website row exists only for a URL the shared HTTP(S) gate
       accepts: a corrupt scheme is dropped whole rather than rendered as
       text, so nothing URL-shaped and unvetted ever enters the page. *)
    match project.website_url with
    | None -> ""
    | Some url ->
        let safe = Components.safe_url url in
        if String.equal safe "#" then ""
        else
          Printf.sprintf
            "<dt>Website</dt><dd class='phs-website'><a href='%s' \
             class='create-link'>%s</a></dd>"
            safe (esc url)
  in
  Printf.sprintf
    "<section class='phs-summary'>\
     <h2 class='phs-name'>%s</h2>\
     <dl class='phs-facts'>\
     <dt>Earde identifier</dt><dd><code class='phs-slug'>%s</code></dd>\
     <dt>Kind</dt><dd class='phs-kind'>%s</dd>\
     <dt>GitHub namespace</dt><dd class='phs-namespace'>%s (%s)</dd>\
     %s</dl>\
     </section>"
    (esc project.name) (esc project.slug)
    (kind_copy project.kind)
    (esc project.namespace_login)
    (account_type_copy project.namespace_type)
    optional_rows

let repository_html repository =
  let markers =
    (if repository.is_primary then
       "<span class='phs-repo-primary'>Primary</span>"
     else "")
    ^
    if repository.is_archived then
      "<span class='phs-repo-archived'>Archived</span>"
    else ""
  in
  let description =
    match repository.description with
    | Some text -> Printf.sprintf "<p class='phs-repo-desc'>%s</p>" (esc text)
    | None -> ""
  in
  (* default_branch may contain '/' — text only, never a URL segment. *)
  Printf.sprintf
    "<li class='phs-repo'><span class='phs-repo-name'>%s</span>%s%s\
     <p class='phs-repo-meta'>Default branch <code \
     class='phs-repo-branch'>%s</code></p></li>"
    (linked_or_text ~link_class:"create-link phs-repo-link"
       repository.html_url
       (esc repository.full_name))
    markers description
    (esc repository.default_branch)

let repositories_html repositories =
  Printf.sprintf
    "<section class='phs-repos'>\
     <h2 class='phs-repos-title'>Repositories</h2>\
     <ul class='phs-repo-list'>%s</ul>\
     </section>"
    (String.concat "\n" (List.map repository_html repositories))

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

(* The continuation: connecting to an existing community is a real
   navigable step now, while dedicated-home creation remains future copy —
   no form, button, or disabled control pretends otherwise. The link is
   built only over a canonical slug, so corrupt data never becomes
   actionable. *)
let next_step_html project =
  let connect_link =
    if valid_project_slug project.slug then
      Printf.sprintf
        "<p class='phs-next-connect'><a href='%s' class='create-link \
         phs-next-request-link'>Connect to an existing community</a></p>"
        (Components.safe_internal_path
           ("/projects/" ^ project.slug ^ "/request-home"))
    else ""
  in
  Printf.sprintf
    "<section class='phs-next'>\
     <h2 class='phs-next-title'>Choose a community home</h2>\
     <p class='create-sub phs-next-copy'>Connect this project to an \
     existing Earde community, or create a community home for it.</p>\
     %s<p class='phs-next-note'>Create a community home: this option is \
     not available yet.</p>\
     </section>"
    connect_link

let project_home_setup_page ?user ?request ~project () =
  let body =
    Printf.sprintf
      "<div class='create-wrap project-home-setup'><div \
       class='create-panel'>%s%s%s%s</div></div>"
      heading (summary_html project)
      (repositories_html project.repositories)
      (next_step_html project)
  in
  (* noindex: a steward-only setup surface — not for search indexes. *)
  Components.create_page ?user ?request ~noindex:true ~title:"Project created"
    ~body ()
