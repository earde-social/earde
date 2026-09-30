(* The existing-community home choice — server-rendered steward page for
   choosing an eligible community as the project's home, or displaying the
   current active relation. Pure rendering over handler-supplied view
   models: no Caqti, no read-model dependency, no session access.

   Language rules: verification wording stays factual ("Project connected
   through GitHub") — never an "Official ..." or "GitHub-approved" claim —
   and nothing implies that a request or an accepted home grants community
   moderation rights. Every caller-controlled string is escaped at the
   template boundary; anything that would carry an invalid slug or id
   degrades to plain text rather than becoming actionable. *)

type visibility =
  | Public
  | Unlisted
  | Currently_unavailable

type community = {
  id : int;
  name : string;
  slug : string;
  description : string option;
  visibility : visibility;
}

type project = {
  name : string;
  slug : string;
  namespace_login : string;
}

type active_relation =
  | Pending_request of community
  | Accepted_home of {
      community : community;
      removal_allowed : bool;
    }

type state =
  | Choose_existing of {
      project : project;
      communities : community list;
      request_note : string;
    }
  | No_eligible_communities of project
  | Active_relation of {
      project : project;
      relation : active_relation;
    }

type feedback =
  | Stale_form
  | Request_form_invalid
  | Community_unavailable
  | Active_home_exists
  | Request_failed


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

(* A community link target must be a single non-empty URL path segment;
   anything else renders as text only. *)
let valid_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* One generic label per presentation state. "Currently unavailable"
   deliberately explains nothing: whether the target went private, back to
   draft, or was never a network community is lifecycle detail this page
   must not reveal. *)
let visibility_copy = function
  | Public -> Html.static "Public"
  | Unlisted -> Html.static "Unlisted"
  | Currently_unavailable -> Html.static "Currently unavailable"

(* /c/<slug>, linked only when the slug is addressable; the identity text
   itself always renders (escaped) so a degraded row stays recognizable. *)
let community_identity_html (community : community) =
  let text = (Html.text (("/c/" ^ community.slug))) in
  if valid_community_slug community.slug then
    (Html.template "<a href='%s' class='create-link phc-community-slug'>%s</a>"
  [ (Html.internal_path (("/c/" ^ community.slug)))
  ; text ])
  else (Html.template "<span class='phc-community-slug'>%s</span>"
  [ text ])

let feedback_copy = function
  | Stale_form ->
      Html.static "This page had been open too long, so the form could no longer be \
       submitted. Nothing was changed. Review it and submit again."
  | Request_form_invalid ->
      Html.static "We couldn't read that request. Review the form and try again."
  | Community_unavailable ->
      Html.static "That community is no longer available for project requests."
  | Active_home_exists ->
      Html.static "This project already has a pending request or community home."
  | Request_failed -> Html.static "We couldn't send the request. Try again."

let feedback_html = function
  | None -> Html.empty
  | Some feedback ->
      (Html.template "<div class='phc-alert'><p>%s</p></div>"
  [ (feedback_copy feedback) ])

(* A chooser row may carry a form control only for a positive id on a
   community that is not Currently_unavailable — that state can only mean
   the view model drifted past the read model's contract (the eligible
   list never contains it), and an ineligible target must never become
   selectable. *)
let selectable (community : community) =
  community.id > 0 && community.visibility <> Currently_unavailable

(* One radio option per selectable community. Everything else gets no
   control — the row stays visible, but nothing actionable carries a
   corrupt id or an unavailable target — and nothing is ever
   preselected. *)
let community_option_html (community : community) =
  let description =
    match community.description with
    | Some text ->
        (Html.template "<p class='phc-community-desc'>%s</p>"
  [ (Html.text (text)) ])
    | None -> Html.empty
  in
  let identity =
    (Html.template "<span class='phc-community-name'>%s</span> %s \
       <span class='phc-community-visibility'>%s</span>%s"
  [ (Html.text (community.name))
  ; (community_identity_html community)
  ; (visibility_copy community.visibility)
  ; description ])
  in
  if selectable community then
    (Html.template "<li class='phc-community'><label>\
       <input type='radio' name='target_community_id' value='%s'> %s\
       </label></li>"
  [ Html.int (community.id)
  ; identity ])
  else
    (Html.template "<li class='phc-community phc-community--unavailable'>%s</li>"
  [ identity ])

(* Options that may carry a form control: positive ids, first occurrence
   wins — a duplicated id never renders two actionable inputs. *)
let dedupe_communities communities =
  let rec keep seen = function
    | [] -> []
    | (community : community) :: rest ->
        if community.id > 0 && List.mem community.id seen then
          keep seen rest
        else community :: keep (community.id :: seen) rest
  in
  keep [] communities

let heading_html =
  (Html.static "<div class='create-head'>\
   <h1 class='create-title'>Connect to an existing community</h1>\
   <p class='create-sub phc-intro'>Request an eligible Earde community to \
   become this project&#39;s home.</p>\
   <p class='phc-verified'>Project connected through GitHub</p>\
   </div>")

(* Review copy stays explicit about who decides and what verification does
   not grant. *)
let review_copy_html =
  (Html.static "<p class='phc-review-copy'>The target community&#39;s moderators must \
   review and accept this request before the community becomes the \
   project&#39;s home. Project verification grants no community moderation \
   rights.</p>")

let note_copy_html =
  (Html.static "<p class='phc-note-copy'>This note is visible only to this \
   project&#39;s stewards, the target community&#39;s moderators, and \
   administrators.</p>")

(* The one request form. No application-owned hidden field exists: the
   route path supplies the project slug, and the POST handler re-derives
   the user from the session and reauthorizes everything through the
   transactional request store. When a request is supplied, Dream's own
   CSRF hidden field is emitted additionally; pure rendering calls omit
   it, keeping the page testable without a server. *)
let request_form_html ?request ~project_slug ~request_note options_html =
  let csrf_field =
    match request with
    | None -> Html.empty
    | Some request -> Csrf_field.tag request
  in
  (Html.template "<form method='POST' action='/projects/%s/request-home' \
     class='create-form phc-request-form'>\
     %s<ul class='phc-community-list'>%s</ul>\
     <label class='phc-note-field'>Request note\
     <textarea name='request_note' maxlength='2000' rows='6'>%s</textarea>\
     </label>%s\
     <div class='create-actions'><button type='submit' class='create-btn \
     create-btn--block'>Send home request</button></div>\
     </form>"
  [ (Html.text (project_slug))
  ; csrf_field
  ; options_html
  ; (Html.text (request_note))
  ; note_copy_html ])

let choose_existing_html ?request ~(project : project) ~communities
    ~request_note () =
  let renderable = dedupe_communities communities in
  let actionable = List.exists selectable renderable in
  let form =
    (* Without a canonical project slug there is no POST target to build;
       without one valid option there is nothing to submit. Either way the
       page degrades to its copy alone. *)
    if valid_project_slug project.slug && actionable then
      request_form_html ?request ~project_slug:project.slug ~request_note
        ((Html.join (Html.static "\n")) (List.map community_option_html renderable))
    else Html.empty
  in
  (Html.template "%s<p class='phc-project'>Choosing a home for \
     <strong>%s</strong>.</p>%s%s"
  [ heading_html
  ; (Html.text (project.name))
  ; review_copy_html
  ; form ])

let setup_link_html (project : project) =
  if valid_project_slug project.slug then
    (Html.template "<p class='phc-back'><a href='%s' class='create-link'>Back to \
       project setup</a></p>"
  [ (Html.internal_path (("/projects/" ^ project.slug ^ "/setup"))) ])
  else Html.empty

let no_eligible_html (project : project) =
  (Html.template "<div class='create-head'>\
     <h1 class='create-title'>Connect to an existing community</h1>\
     <p class='create-sub phc-verified'>Project connected through \
     GitHub</p></div>\
     <p class='phc-project'>Choosing a home for <strong>%s</strong>.</p>\
     <p class='phc-none'>No eligible published network community is \
     currently available to request as this project&#39;s home.</p>%s"
  [ (Html.text (project.name))
  ; (setup_link_html project) ])

(* An unavailable active target keeps its name, identity, and relation
   status, gaining only the generic non-actionable marker — never the
   reason, and never a Public/Unlisted label it no longer earns. *)
let availability_html (community : community) =
  match community.visibility with
  | Currently_unavailable ->
      (Html.static " <span class='phc-community-availability'>Currently unavailable</span>")
  | Public | Unlisted -> Html.empty

(* Active-relation states render no request controls at all: the workflow
   continues with the target community's moderators. *)
let pending_html (community : community) =
  (Html.template "<div class='create-head'><h1 class='create-title'>Home request \
     pending</h1></div>\
     <p class='phc-pending-copy'>This project has a pending home request \
     to <strong class='phc-community-name'>%s</strong> %s%s. The target \
     community&#39;s moderators must accept or reject it.</p>"
  [ (Html.text (community.name))
  ; (community_identity_html community)
  ; (availability_html community) ])

(* The accepted state is the only one that carries a removal control, and
   the only place on this page a mutation form exists besides the chooser.
   Both slugs come from the canonical page model and are revalidated by the
   removal fragment itself, so a malformed one silently drops the form
   rather than building an unusable action. Removal is deliberately
   available while the target is Currently_unavailable: a home must stay
   separable exactly when the community's lifecycle has drifted.

   [removal_allowed] is the one exception, and it is not a drift state: an
   unpublished dedicated-community setup draft structurally requires its
   provisioned home, so the fragment renders its reason instead of a
   control. Presentation only — the removal store re-decides every POST. *)
let accepted_html ?request ~(project : project) ~removal_allowed
    (community : community) =
  (Html.template "<div class='create-head'><h1 class='create-title'>Community home \
     connected</h1></div>\
     <p class='phc-accepted-copy'><strong class='phc-community-name'>%s\
     </strong> %s%s is this project&#39;s community home. Community \
     moderation stays with its moderators.</p>%s"
  [ (Html.text (community.name))
  ; (community_identity_html community)
  ; (availability_html community)
  ; (Project_home_removal_pages.project_side_removal_form ?request
       ~removal_allowed ~project_slug:project.slug
       ~community_slug:community.slug ()) ])

(* Launch onboarding stepper (Cartographic Civic, 04-ROUTES): the same
   five-step sequence the /projects/new wrapper renders, truthfully
   positioned for this branch — GitHub and Project are committed by the time
   this page can render at all, Home is the live decision (this route is one
   of its two branches), and the later steps stay plain upcoming dots, never
   links. Rendered by the launch wrapper BEFORE the immutable create-shell
   fragment, so the byte-exact fragment the test suites slice is untouched.
   Markup only — no form, no field, no script, no inline style. *)
let stepper_html =
  let labels = List.map Html.text [ "GitHub"; "Project"; "Home"; "Configure"; "Complete" ] in
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
    (Html.template "<li class='step'><span class='%s'>%s</span><span class='%s'>%s</span></li>"
  [ (Html.text dot_class)
  ; (Html.text dot_text)
  ; (Html.text label_class)
  ; label ])
  in
  (Html.template "<ol class='steps' aria-label='Project onboarding steps'>%s</ol>"
  [ (Html.join (Html.static "<li class='step__rule' aria-hidden='true'></li>")
       (List.mapi step labels)) ])

let project_home_choice_page ?user ?request ~state ~feedback () =
  let body =
    match state with
    | Choose_existing { project; communities; request_note } ->
        choose_existing_html ?request ~project ~communities ~request_note ()
    | No_eligible_communities project -> no_eligible_html project
    | Active_relation { project = _; relation = Pending_request community }
      ->
        pending_html community
    | Active_relation
        { project; relation = Accepted_home { community; removal_allowed } } ->
        accepted_html ?request ~project ~removal_allowed community
  in
  let body =
    (Html.template "<div class='create-wrap project-home-choice'><div \
       class='create-panel'>%s%s</div></div>"
  [ (feedback_html feedback)
  ; body ])
  in
  (* noindex: a steward-only workflow surface — not for search indexes. *)
  Page_shell.launch_onboarding_page ?user ?request ~noindex:true
    ~stepper:stepper_html ~page_class:"launch-project-request-home"
    ~title:"Choose a community home" ~content:body ()
