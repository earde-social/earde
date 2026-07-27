(* The "Connected projects" section of the existing community page — a pure
   fragment over handler-supplied page models. No Caqti, no read-model
   dependency, no session access.

   Deliberately a fragment rather than a page: the community route already
   owns authorization, chrome, cache and indexing policy, and analytics
   behavior, and composing into it is the narrowest integration that keeps all
   of that in one place. Building a second complete page would have duplicated
   the very decisions that must not diverge.

   Wording stays factual — an accepted home is a GitHub-verified connection,
   never an "Official ..." or "GitHub-approved" claim — and no identifier is
   part of the model, so none can leak. Every caller-controlled string is
   escaped at the template boundary, and any URL that is not provably safe
   degrades to inert text rather than becoming an actionable link. Nothing
   here raises on a malformed model. *)

type verification =
  | Verified
  | Stale
  | Revoked

type repository = {
  full_name : string;
  html_url : string;
  is_primary : bool;
  is_archived : bool;
}

type project = {
  name : string;
  slug : string;
  kind : Project_identity.kind;
  namespace_login : string;
  verification : verification;
  website_url : string option;
  repositories : repository list;
}

let esc = Components.html_escape

(* The same canonical grammar the read model and the project-home routes
   require. Used only to decide whether a slug is coherent enough to count as
   this project's identity for duplicate detection — never to build a link,
   because no public project route exists. *)
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

(* Same http(s)-only test Components.safe_url applies, so a URL that is not a
   real link degrades to inert text instead of becoming a "#" anchor. *)
let http_url value =
  let lower = String.lowercase_ascii value in
  (String.length lower >= 7 && String.sub lower 0 7 = "http://")
  || (String.length lower >= 8 && String.sub lower 0 8 = "https://")

(* A repository link is offered only when the supplied URL is exactly the
   canonical HTTPS GitHub URL for that repository's own full name —
   reconstructed here, never compared as a prefix, so a lookalike host or a
   redirect target can never inherit the repository's label. *)
let valid_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let canonical_github_url ~full_name ~html_url =
  match String.index_opt full_name '/' with
  | None -> false
  | Some i ->
      let owner = String.sub full_name 0 i in
      let name =
        String.sub full_name (i + 1) (String.length full_name - i - 1)
      in
      valid_segment owner && valid_segment name
      && String.equal html_url
           (Uri.to_string
              (Uri.make ~scheme:"https" ~host:"github.com"
                 ~path:("/" ^ owner ^ "/" ^ name)
                 ()))

(* Nonblank display text: at least one byte outside ASCII whitespace. *)
let nonblank value =
  String.exists
    (fun c ->
      not
        (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

(* The current human-readable product vocabulary, identical to the moderator
   review surface so one project reads the same everywhere. *)
let kind_copy = function
  | Project_identity.Project -> "Project"
  | Project_identity.Organization -> "Organization"
  | Project_identity.Ecosystem -> "Ecosystem"
  | Project_identity.Foundation -> "Foundation"
  | Project_identity.Working_group -> "Working group"
  | Project_identity.Other -> "Other"

(* Factual verification wording only. A stale or revoked project keeps its
   place in the community's history and says plainly which state it is in;
   none of these is a badge of officiality or endorsement. *)
let verification_copy = function
  | Verified -> "Verified through GitHub"
  | Stale -> "Verification stale"
  | Revoked -> "Verification revoked"

let heading_html =
  "<h2 class='ccp-title'>Connected projects</h2>\
   <p class='ccp-sub'>Open-source projects that use this community as their \
   Earde home.</p>"

(* Markers ride after the identity, never replacing it, so an archived or
   primary repository stays fully labelled. *)
let repo_markers (r : repository) =
  let primary =
    if r.is_primary then " <span class='ccp-marker ccp-primary'>Primary</span>"
    else ""
  in
  let archived =
    if r.is_archived then
      " <span class='ccp-marker ccp-archived'>Archived</span>"
    else ""
  in
  primary ^ archived

(* The full name always renders (escaped); it becomes a link only when this
   project may carry links at all and the URL is the canonical GitHub target
   for that exact full name. *)
let repo_identity_html ~linkable (r : repository) =
  let text = esc r.full_name in
  if
    linkable
    && http_url r.html_url
    && canonical_github_url ~full_name:r.full_name ~html_url:r.html_url
  then
    Printf.sprintf "<a href='%s' class='ccp-repo-link'>%s</a>"
      (Components.safe_url r.html_url)
      text
  else Printf.sprintf "<span class='ccp-repo-name'>%s</span>" text

let repo_html ~linkable (r : repository) =
  Printf.sprintf "<li class='ccp-repo'>%s%s</li>"
    (repo_identity_html ~linkable r)
    (repo_markers r)

(* A repository full name repeated inside one project leaves only its first
   occurrence linked, so a duplicated row can never present two competing
   actionable targets under the same label. An empty list renders nothing at
   all — the project identity stands on its own. *)
let repositories_html ~linkable repos =
  let rec go seen = function
    | [] -> []
    | (r : repository) :: rest ->
        let first = not (List.mem r.full_name seen) in
        repo_html ~linkable:(linkable && first) r :: go (r.full_name :: seen) rest
  in
  match repos with
  | [] -> ""
  | repos ->
      Printf.sprintf "<ul class='ccp-repos'>%s</ul>"
        (String.concat "" (go [] repos))

(* The website is offered as a link only when it is a real http(s) target and
   this project may carry links; otherwise the stored value still renders, but
   inert and escaped. *)
let website_html ~linkable = function
  | None -> ""
  | Some url ->
      if linkable && http_url url then
        Printf.sprintf "<p class='ccp-website'><a href='%s' class='ccp-website-link'>%s</a></p>"
          (Components.safe_url url) (esc url)
      else Printf.sprintf "<p class='ccp-website'>%s</p>" (esc url)

(* The project name is never a link: no public permanent-project route exists,
   and the owner-only setup destination must not be advertised on a page
   ordinary visitors reach. A blank name degrades to a generic label rather
   than rendering an empty heading. *)
let project_name_html (p : project) =
  let label = if nonblank p.name then esc p.name else "Open-source project" in
  Printf.sprintf "<h3 class='ccp-name'>%s</h3>" label

let project_html ~linkable (p : project) =
  Printf.sprintf
    "<li class='ccp-project'>\
     <div class='ccp-identity'>%s\
     <p class='ccp-kind'>%s</p>\
     <p class='ccp-namespace'>%s</p>\
     <p class='ccp-verification'>%s</p>\
     <p class='ccp-connected'>Project connected through GitHub</p>%s\
     </div>%s</li>"
    (project_name_html p) (kind_copy p.kind)
    (esc p.namespace_login)
    (verification_copy p.verification)
    (website_html ~linkable p.website_url)
    (repositories_html ~linkable p.repositories)

(* Link-carrying is decided per project: the slug must be canonical and this
   the first occurrence of it, so a duplicated project slug leaves at most one
   group able to present actionable links. The supplied order is preserved
   exactly — nothing is re-sorted here. *)
let projects_html projects =
  let rec go seen = function
    | [] -> []
    | (p : project) :: rest ->
        let slug_ok = valid_project_slug p.slug in
        let linkable = slug_ok && not (List.mem p.slug seen) in
        let seen = if slug_ok then p.slug :: seen else seen in
        project_html ~linkable p :: go seen rest
  in
  String.concat "" (go [] projects)

let connected_projects_section ~projects =
  match projects with
  | [] -> ""
  | projects ->
      Printf.sprintf
        "<section class='ccp-section'>%s<ul class='ccp-projects'>%s</ul></section>"
        heading_html (projects_html projects)
