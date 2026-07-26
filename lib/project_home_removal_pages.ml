(* Removal controls for an accepted project-home relation, rendered on the
   two authorized surfaces: the steward-facing home-choice page and the
   moderator-facing community settings surface. Pure rendering over
   handler-supplied page models — no Caqti, no read model, no store, no
   session access.

   Language rules: removal detaches an association and nothing else, and
   the copy says exactly that. Nothing here implies that removing a home
   deletes the project, the community, its content, stewardship,
   moderation, membership, GitHub verification, or repositories, and
   nothing implies that a rendered form is itself permission — the
   transactional store reauthorizes every POST.

   Every caller-controlled string is escaped at the template boundary, and
   any value that cannot be proven canonical drops the form it would have
   carried rather than becoming a broken or guessable action. *)

type verification =
  | Verified
  | Stale
  | Revoked

type connected_project = {
  name : string;
  slug : string;
  namespace_login : string;
  verification : verification;
}

let esc = Components.html_escape

(* The same canonical permanent grammar the routes, the read models, and
   the removal store require. A project slug outside it never reaches an
   action attribute. *)
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

(* A community slug in an action path must be a single non-empty URL path
   segment; anything else drops every actionable form on the surface. *)
let valid_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Nonblank display text: at least one byte outside ASCII whitespace. *)
let nonblank value =
  String.exists
    (fun c ->
      not
        (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

(* All three states are removable, so the copy only names the state — it
   never suggests that verification drift blocks or forces detaching. *)
let verification_copy = function
  | Verified -> "Verified through GitHub"
  | Stale -> "Verification stale"
  | Revoked -> "Verification revoked"

(* The one warning both surfaces share, word for word: removal is an
   association-only operation. *)
let association_only_copy =
  "This removes the association only. It does not delete the project, \
   community, or community content."

let warning_html cls =
  Printf.sprintf "<p class='%s'>%s</p>" cls association_only_copy

(* Both slugs are validated before an action path is built, so escaping
   here is defense-in-depth over values already known canonical. *)
let project_side_action ~project_slug ~community_slug =
  esc
    (Printf.sprintf "/projects/%s/community-home/%s/remove" project_slug
       community_slug)

let community_side_action ~community_slug ~project_slug =
  esc
    (Printf.sprintf "/c/%s/projects/%s/remove-home" community_slug project_slug)

(* One form shape for both surfaces: the route path carries both
   identities, so no application field and no hidden identifier exists, and
   the submit control is nameless. Dream's framework CSRF field is emitted
   only when a live request is supplied; without one the form could not be
   submitted at all, so the caller renders none. *)
let removal_form ~request ~action ~cls =
  match request with
  | None -> ""
  | Some request ->
      Printf.sprintf
        "<form method='POST' action='%s' class='phrm-removal-form %s'>%s\
         <button type='submit' class='phrm-btn'>Remove home</button></form>"
        action cls (Dream.csrf_tag request)

(* --- Project side: the steward's control for the one accepted home --- *)

let project_side_removal_form ?request ~project_slug ~community_slug () =
  let form =
    (* Without both canonical slugs there is no POST target to build, so
       the section degrades to its copy alone rather than emitting an
       action that guesses at the missing identity. *)
    if valid_project_slug project_slug && valid_community_slug community_slug
    then
      removal_form ~request
        ~action:(project_side_action ~project_slug ~community_slug)
        ~cls:"phrm-project-side"
    else ""
  in
  Printf.sprintf
    "<div class='phrm-removal'>\
     <h2 class='phrm-removal-title'>Remove community home</h2>%s%s</div>"
    (warning_html "phrm-removal-copy") form

(* --- Community side: the settings management section --- *)

let project_identity_html (p : connected_project) =
  let label = if nonblank p.name then esc p.name else "Open-source project" in
  Printf.sprintf
    "<div class='phrm-identity'>\
     <h3 class='phrm-project-name'>%s</h3>\
     <p class='phrm-project-slug'>%s</p>\
     <p class='phrm-namespace'>%s</p>\
     <p class='phrm-verification'>%s</p></div>"
    label (esc p.slug) (esc p.namespace_login)
    (verification_copy p.verification)

(* Actionability is decided per project: the community slug must be
   addressable, the project slug canonical, and this the first occurrence
   of that slug — so a duplicated slug leaves at most one actionable row
   and a corrupt one leaves the identity visible but inert. *)
let project_row_html ~request ~community_slug ~actionable (p : connected_project)
    =
  let form =
    if actionable then
      removal_form ~request
        ~action:(community_side_action ~community_slug ~project_slug:p.slug)
        ~cls:"phrm-community-side"
    else ""
  in
  Printf.sprintf "<li class='phrm-project'>%s%s</li>"
    (project_identity_html p) form

let community_side_management_section ?request ~community_slug ~projects () =
  let addressable = valid_community_slug community_slug in
  let rendered =
    let rec render seen = function
      | [] -> []
      | (p : connected_project) :: rest ->
          let canonical = valid_project_slug p.slug in
          let actionable =
            addressable && canonical && not (List.mem p.slug seen)
          in
          let seen = if canonical then p.slug :: seen else seen in
          project_row_html ~request ~community_slug ~actionable p
          :: render seen rest
    in
    render [] projects
  in
  let body =
    match rendered with
    (* A settings panel is permanent navigation, so the empty state gets
       restrained copy rather than vanishing the way the public
       connected-projects fragment does. *)
    | [] -> "<p class='phrm-none'>No connected projects.</p>"
    | rows ->
        Printf.sprintf "%s<ul class='phrm-projects'>%s</ul>"
          (warning_html "phrm-section-copy")
          (String.concat "\n" rows)
  in
  Printf.sprintf
    "<section class='phrm-section'>\
     <h2 class='phrm-section-title'>Connected projects</h2>%s</section>"
    body
