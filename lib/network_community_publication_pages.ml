(* /c/:slug/setup — server-rendered setup-and-publication page for one
   provisioned network community. Pure rendering over handler-supplied view
   models: no Caqti, no read-model dependency, no session access.

   Language rules: the connected project is described factually ("Connected
   through GitHub": this page does not re-check how fresh the verification
   is) — never "Official ...", "GitHub-approved",
   or "GitHub-endorsed" — the two publication choices are named exactly as
   publication implements them, and nothing claims that publishing grants a
   project steward a permission they do not already durably hold. Every
   caller-controlled string is escaped at the template boundary, and a draft
   slug outside the canonical network grammar suppresses the form rather than
   producing an unusable action. See the .mli for the full contract. *)

type project = {
  name : string;
  slug : string;
  namespace_login : string;
  kind : Project_identity.kind;
}

type community = {
  name : string;
  slug : string;
  description : string option;
}

type form_values = {
  community_name : string;
  community_slug : string;
  community_description : string;
  publication_visibility : string;
}

type feedback =
  | Stale_form
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description
  | Invalid_publication_visibility
  | Community_slug_unavailable
  | Draft_unavailable
  | Publication_failed


(* Descriptive kind labels without endorsement language, identical to the
   provisioning page's vocabulary. *)
let kind_copy = function
  | Project_identity.Project -> Html.static "Project"
  | Project_identity.Organization -> Html.static "GitHub organization"
  | Project_identity.Ecosystem -> Html.static "Ecosystem"
  | Project_identity.Foundation -> Html.static "Foundation"
  | Project_identity.Working_group -> Html.static "Working group"
  | Project_identity.Other -> Html.static "Other open-source initiative"

(* The scoped network-community slug grammar the database itself enforces on
   every network row. A slug outside it never reaches an action attribute. *)
let canonical_network_slug value =
  let n = String.length value in
  let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  n >= 1 && n <= 80
  && value.[0] <> '-'
  && value.[n - 1] <> '-'
  &&
  let rec ok i =
    i >= n
    ||
    if is_slug_char value.[i] then ok (i + 1)
    else value.[i] = '-' && value.[i + 1] <> '-' && ok (i + 1)
  in
  ok 0

(* One generic message per rejected outcome. None names a submitted value, a
   length, or which durable row lost a race. *)
let feedback_copy = function
  | Stale_form ->
      Html.static "This page had been open too long, so the form could no longer be \
       submitted. Nothing was changed. Review it and submit again."
  | Invalid_form ->
      Html.static "We couldn't read that submission. Review the form and try again."
  | Invalid_community_name -> Html.static "Enter a community name we can use."
  | Invalid_community_slug ->
      Html.static "Enter a community address using lowercase letters, numbers, and \
       single hyphens."
  | Invalid_community_description ->
      Html.static "That description can't be used. Edit it and try again."
  | Invalid_publication_visibility ->
      Html.static "Choose whether the community should be public or unlisted."
  | Community_slug_unavailable ->
      Html.static "That community address is already taken. Choose another one."
  | Draft_unavailable ->
      Html.static "This community is no longer waiting to be published."
  | Publication_failed -> Html.static "We couldn't publish the community. Try again."

let feedback_html = function
  | None -> Html.empty
  | Some feedback ->
      (Html.template "<div class='ncp-alert'><p>%s</p></div>"
  [ (feedback_copy feedback) ])

let heading_html =
  (Html.static "<div class='create-head'>\
   <h1 class='create-title'>Complete setup and publish</h1>\
   <p class='create-sub ncp-intro'>Review the community identity and choose \
   how people can discover it.</p>\
   </div>")

(* The project this community is the home of, as plain identity. No link is
   emitted: no public project route exists, so a project-derived href could
   only ever be an unusable or misleading one. *)
let project_html (project : project) =
  (Html.template "<section class='ncp-project'>\
     <h2 class='ncp-project-name'>%s</h2>\
     <p class='ncp-project-meta'>%s &middot; %s</p>\
     <p class='ncp-project-note'>Connected through GitHub.</p>\
     </section>"
  [ (Html.text (project.name))
  ; (kind_copy project.kind)
  ; (Html.text (project.namespace_login)) ])

(* What state the community is in now, and what publishing does and does not
   do. Publication is named as the explicit action it is, with exactly the
   two choices it offers, so nobody expects a fully private published
   community — and nothing here suggests a new permission appears. *)
let explanation_html =
  (Html.static "<section class='ncp-explain'>\
   <h2 class='ncp-explain-title'>Where this community stands</h2>\
   <ul class='ncp-explain-list'>\
   <li>This community is still a private setup draft. Only people authorized \
   for setup can reach it.</li>\
   <li>Publishing is a separate, explicit action. Nothing is published until \
   you submit this form.</li>\
   <li>A network community keeps a public home. Private rooms and restricted \
   sections can still exist inside it under either choice below.</li>\
   <li>It cannot be published as a fully private community.</li>\
   <li>Publishing does not change who moderates or administers anything. \
   Project stewards gain no permission beyond the durable roles they already \
   hold.</li>\
   </ul>\
   </section>")

(* The two publication choices, described so the difference is unambiguous
   before the radio is clicked. *)
let choices_html =
  (Html.static "<section class='ncp-choices'>\
   <h2 class='ncp-choices-title'>How people find this community</h2>\
   <ul class='ncp-choices-list'>\
   <li><strong>Public</strong> &mdash; reachable by anyone, listed in \
   Earde&#39;s own discovery surfaces, and open to search-engine \
   indexing.</li>\
   <li><strong>Unlisted</strong> &mdash; reachable by anyone with the direct \
   URL, but kept out of Earde&#39;s discovery surfaces and marked \
   <code>noindex</code> for search engines.</li>\
   </ul>\
   </section>")

(* Preselection is deliberately conservative. A valid explicit choice wins.
   No choice at all — the empty value the first render carries — falls back
   to [public], the documented default. Anything else is a value the parser
   rejected, including "private": it preselects neither option rather than
   quietly moving the publisher onto the more exposed one. *)
let radio_html ~(values : form_values) =
  let checked value =
    match values.publication_visibility with
    | "public" | "" -> if String.equal value "public" then " checked" else ""
    | "unlisted" -> if String.equal value "unlisted" then " checked" else ""
    | _ -> ""
  in
  (Html.template "<fieldset class='ncp-visibility'>\
     <legend class='ncp-legend'>Publish as</legend>\
     <label class='ncp-radio'><input type='radio' \
     name='publication_visibility' value='public'%s> Public</label>\
     <label class='ncp-radio'><input type='radio' \
     name='publication_visibility' value='unlisted'%s> Unlisted</label>\
     </fieldset>"
  [ (Html.text (checked "public"))
  ; (Html.text (checked "unlisted")) ])

(* The one form. No application-owned hidden field exists: the route path
   supplies the community, and the future POST handler re-derives the
   publisher from the session and reauthorizes everything durably. When a
   request is supplied, Dream's own CSRF hidden field is emitted
   additionally; pure rendering calls omit it, keeping the page testable
   without a server.

   Maximum lengths mirror the server-side policy and the scoped database
   CHECKs exactly, so the control never invites a value the parser would
   reject. *)
let form_html ?request ~community_slug ~(values : form_values) () =
  let csrf_field =
    match request with None -> Html.empty | Some request -> Csrf_field.tag request
  in
  (Html.template "<form method='POST' action='/c/%s/publish' class='create-form \
     ncp-form'>%s\
     <label class='ncp-field'>Community name\
     <input type='text' name='community_name' maxlength='120' value='%s'>\
     </label>\
     <label class='ncp-field'>Community address\
     <input type='text' name='community_slug' maxlength='80' value='%s'>\
     </label>\
     <p class='ncp-hint'>The community lives at /c/&lt;address&gt;. Use \
     lowercase letters, numbers, and single hyphens.</p>\
     <label class='ncp-field'>Community description\
     <textarea name='community_description' maxlength='2000' rows='6'>%s\
     </textarea></label>%s\
     <div class='create-actions'><button type='submit' class='create-btn \
     create-btn--block'>Publish community</button></div>\
     </form>"
  [ (Html.text (community_slug))
  ; csrf_field
  ; (Html.text (values.community_name))
  ; (Html.text (values.community_slug))
  ; (Html.text (values.community_description))
  ; (radio_html ~values) ])

(* Without a canonical draft slug there is no POST target to build; the page
   degrades to its copy alone rather than emitting an action that could not
   work. *)
let actions_html ?request ~(community : community) ~values () =
  if canonical_network_slug community.slug then
    form_html ?request ~community_slug:community.slug ~values ()
  else Html.empty

(* Launch onboarding stepper (Cartographic Civic, 04-ROUTES): the same
   five-step sequence the earlier launch wrappers render, truthfully
   positioned for this route — GitHub, Project, and Home are committed by
   the time a provisioned draft can render this page at all, Configure is
   the live step (this page is where the draft is configured and
   published), and Complete stays a plain upcoming dot, never a link.
   Rendered by the launch wrapper BEFORE the immutable create-shell
   fragment, so the byte-exact fragment the test suites slice is untouched.
   Markup only — no form, no field, no script, no inline style. *)
let stepper_html =
  let labels = List.map Html.text [ "GitHub"; "Project"; "Home"; "Configure"; "Complete" ] in
  let active = 3 in
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
    (Html.template "<li class='step'><span class='%s'>%s</span><span class='%s'>%s</span></li>"
  [ (Html.text dot_class)
  ; (Html.text dot_text)
  ; (Html.text label_class)
  ; label ])
  in
  (Html.template "<ol class='steps' aria-label='Project onboarding steps'>%s</ol>"
  [ (Html.join (Html.static "<li class='step__rule' aria-hidden='true'></li>")
       (List.mapi step labels)) ])

let network_community_publication_page ?user ?request ~community ~project
    ~values ~feedback () =
  let body =
    (Html.template "<div class='create-wrap network-community-publication'><div \
       class='create-panel'>%s%s%s%s%s%s</div></div>"
  [ (feedback_html feedback)
  ; heading_html
  ; (project_html project)
  ; explanation_html
  ; choices_html
  ; (actions_html ?request ~community ~values ()) ])
  in
  (* noindex: an authorized-only setup surface — not for search indexes. *)
  Page_shell.launch_onboarding_page ?user ?request ~noindex:true
    ~stepper:stepper_html ~page_class:"launch-community-publication"
    ~title:"Complete setup and publish" ~content:body ()
