(* The pending project-home review queue — server-rendered moderator page
   for accepting or rejecting requests to adopt this community as a
   project's Earde home. Pure rendering over handler-supplied view models:
   no Caqti, no read-model dependency, no session access.

   Language rules: verification wording stays factual ("Project connected
   through GitHub") — never an "Official ..." or "GitHub-approved" claim —
   and no relation, project, community, or user identifier is ever emitted.
   Every caller-controlled string is escaped at the template boundary;
   anything that would carry an invalid slug or URL degrades to plain text
   rather than becoming actionable. This is an authorized workflow surface,
   so full verification and eligibility state is shown to the moderator,
   unlike the public/steward-facing channels. *)

type project_verification =
  | Verified
  | Stale
  | Revoked

type host_eligibility =
  | Eligible
  | Currently_ineligible

type repository = {
  full_name : string;
  html_url : string;
  is_primary : bool;
  is_archived : bool;
}

type pending_request = {
  project_name : string;
  project_slug : string;
  project_kind : Project_identity.kind;
  namespace_login : string;
  verification : project_verification;
  repositories : repository list;
  requester_name : string option;
  request_note : string option;
}

type community = {
  name : string;
  slug : string;
  host_eligibility : host_eligibility;
}

type state = {
  community : community;
  requests : pending_request list;
}

type feedback =
  | Stale_form
  | Review_unavailable
  | Project_unavailable
  | Target_ineligible
  | Review_failed

(* Cartographic Civic shell data the handler loads only after the read model
   has authorized the reviewer: the durable community record (rail tile,
   analytics community context, private-community replay marker), the
   viewer's joined communities for the shared global rail, and the prebuilt
   shared knowledge sidebar. Absent (pure rendering, or a degraded shell
   load) the page falls back to the legacy create-shell document; the
   feature fragment between the create-shell marker and </main> is
   byte-identical either way. *)
type launch_shell = {
  community_record : Db.community;
  rail_communities : Db.community list;
  sidebar : string;
}

let esc = Components.html_escape

(* The same canonical grammar the route and read model require. A project
   slug outside it never reaches an action attribute. *)
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
   segment; anything else drops every actionable form on the page. *)
let valid_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Same http(s)-only test Components.safe_url applies, so a repository URL
   that is not a real link degrades to inert text instead of a "#" anchor. *)
let http_url value =
  let lower = String.lowercase_ascii value in
  (String.length lower >= 7 && String.sub lower 0 7 = "http://")
  || (String.length lower >= 8 && String.sub lower 0 8 = "https://")

let kind_copy = function
  | Project_identity.Project -> "Project"
  | Project_identity.Organization -> "Organization"
  | Project_identity.Ecosystem -> "Ecosystem"
  | Project_identity.Foundation -> "Foundation"
  | Project_identity.Working_group -> "Working group"
  | Project_identity.Other -> "Other"

(* Stale and revoked stay distinct: an authorized moderator needs to know
   why acceptance is unavailable, unlike the collapsed public channels. *)
let verification_copy = function
  | Verified -> "Verified"
  | Stale -> "Verification stale"
  | Revoked -> "Verification revoked"

let feedback_copy = function
  | Stale_form ->
      "This page had been open too long, so the action could no longer be \
       submitted. Nothing was changed. Try again."
  | Review_unavailable -> "That request is no longer pending."
  | Project_unavailable ->
      "That project is no longer available for acceptance."
  | Target_ineligible ->
      "This community cannot currently accept that project."
  | Review_failed -> "We couldn't review the request. Try again."

let feedback_html = function
  | None -> ""
  | Some feedback ->
      Printf.sprintf "<div class='phrv-alert'><p>%s</p></div>"
        (feedback_copy feedback)

let heading_html =
  "<div class='create-head'>\
   <h1 class='create-title'>Project home requests</h1>\
   <p class='create-sub phrv-intro'>Review projects requesting this \
   community as their Earde home.</p></div>"

(* One generic page-level notice; it never reveals whether the community is
   private, a draft, or legacy — only that acceptance is currently closed. *)
let ineligible_notice_html (community : community) =
  match community.host_eligibility with
  | Eligible -> ""
  | Currently_ineligible ->
      "<div class='phrv-ineligible'><p>This community cannot currently \
       accept project-home requests.</p></div>"

(* Markers ride after the identity, never replacing it, so an archived or
   primary repository stays fully labelled. *)
let repo_markers (r : repository) =
  let primary =
    if r.is_primary then
      " <span class='phrv-marker phrv-primary'>Primary</span>"
    else ""
  in
  let archived =
    if r.is_archived then
      " <span class='phrv-marker phrv-archived'>Archived</span>"
    else ""
  in
  primary ^ archived

(* The full name always renders (escaped); it only becomes a link when the
   stored URL is a real http(s) target. *)
let repo_identity_html (r : repository) =
  let text = esc r.full_name in
  if http_url r.html_url then
    Printf.sprintf "<a href='%s' class='phrv-repo-link'>%s</a>"
      (Components.safe_url r.html_url) text
  else Printf.sprintf "<span class='phrv-repo-name'>%s</span>" text

let repo_html (r : repository) =
  Printf.sprintf "<li class='phrv-repo'>%s%s</li>" (repo_identity_html r)
    (repo_markers r)

let repositories_html = function
  | [] -> "<p class='phrv-no-repos'>No repositories.</p>"
  | repos ->
      Printf.sprintf "<ul class='phrv-repos'>%s</ul>"
        (String.concat "\n" (List.map repo_html repos))

let requester_html = function
  | Some name ->
      Printf.sprintf "<p class='phrv-requester'>Requested by %s</p>"
        (esc name)
  | None -> "<p class='phrv-requester'>Requested by Deleted user</p>"

(* Private workflow text: labelled as private, HTML-escaped, and never
   parsed as Markdown or HTML. *)
let note_html = function
  | None -> ""
  | Some note ->
      Printf.sprintf
        "<div class='phrv-note'>\
         <p class='phrv-note-label'>Private request note — visible only to \
         this community&#39;s moderators and administrators.</p>\
         <p class='phrv-note-body'>%s</p></div>"
        (esc note)

let identity_html (req : pending_request) =
  Printf.sprintf
    "<div class='phrv-identity'>\
     <h2 class='phrv-project-name'>%s</h2>\
     <p class='phrv-project-kind'>%s</p>\
     <p class='phrv-namespace'>%s</p>\
     <p class='phrv-verification'>%s</p>\
     <p class='phrv-connected'>Project connected through GitHub</p>\
     </div>"
    (esc req.project_name) (kind_copy req.project_kind)
    (esc req.namespace_login)
    (verification_copy req.verification)

(* Both slugs are validated before an action path is built, so escaping here
   is defense-in-depth on values already known canonical. *)
let action_path ~community_slug ~project_slug ~verb =
  esc
    (Printf.sprintf "/c/%s/projects/%s/%s" community_slug project_slug verb)

(* One form: the route path carries both identities, so no application field
   and no hidden identifier exist; the submit control is nameless. Dream's
   framework CSRF field is emitted only when a live request is supplied. *)
let review_form ?request ~action ~cls ~label () =
  let csrf_field =
    match request with None -> "" | Some request -> Dream.csrf_tag request
  in
  Printf.sprintf
    "<form method='POST' action='%s' class='phrv-review-form %s'>%s\
     <button type='submit' class='phrv-btn'>%s</button></form>"
    action cls csrf_field label

(* Acceptance requires a verified project, an eligible community, and at
   least one repository; anything else leaves rejection available but shows
   only generic non-actionable copy — never a disabled accept button. *)
let accept_available ~(community : community) ~(req : pending_request) =
  req.verification = Verified
  && community.host_eligibility = Eligible
  && req.repositories <> []

let actions_html ?request ~(community : community) ~(req : pending_request) ()
    =
  let accept =
    if accept_available ~community ~req then
      review_form ?request
        ~action:
          (action_path ~community_slug:community.slug
             ~project_slug:req.project_slug ~verb:"accept")
        ~cls:"phrv-accept" ~label:"Accept as community home" ()
    else
      "<p class='phrv-accept-unavailable'>Acceptance is unavailable for this \
       request.</p>"
  in
  let reject =
    review_form ?request
      ~action:
        (action_path ~community_slug:community.slug
           ~project_slug:req.project_slug ~verb:"reject")
      ~cls:"phrv-reject" ~label:"Reject request" ()
  in
  Printf.sprintf "<div class='phrv-actions'>%s%s</div>" accept reject

let request_html ?request ~(community : community) ~actionable
    (req : pending_request) =
  Printf.sprintf "<li class='phrv-request'>%s%s%s%s%s</li>"
    (identity_html req)
    (repositories_html req.repositories)
    (requester_html req.requester_name)
    (note_html req.request_note)
    (if actionable then actions_html ?request ~community ~req () else "")

(* Actionability is decided per request: the community slug must be
   addressable, the project slug canonical, and this the first occurrence of
   that slug — so a duplicated project slug leaves at most one actionable
   group and never produces two forms posting to the same route. *)
let requests_html ?request ~(community : community) requests =
  let community_slug_ok = valid_community_slug community.slug in
  let rec go seen = function
    | [] -> []
    | (req : pending_request) :: rest ->
        let slug_ok = valid_project_slug req.project_slug in
        let actionable =
          community_slug_ok && slug_ok && not (List.mem req.project_slug seen)
        in
        let seen = if slug_ok then req.project_slug :: seen else seen in
        request_html ?request ~community ~actionable req :: go seen rest
  in
  String.concat "\n" (go [] requests)

let body_html ?request ~(state : state) () =
  let requests_section =
    match state.requests with
    | [] -> "<p class='phrv-empty'>No pending project requests.</p>"
    | requests ->
        Printf.sprintf "<ul class='phrv-request-list'>%s</ul>"
          (requests_html ?request ~community:state.community requests)
  in
  Printf.sprintf "%s%s%s" heading_html
    (ineligible_notice_html state.community)
    requests_section

let project_home_review_page ?user ?request ?shell ~state ~feedback () =
  let body =
    Printf.sprintf
      "<div class='create-wrap project-home-review'><div \
       class='create-panel'>%s%s</div></div>"
      (feedback_html feedback)
      (body_html ?request ~state ())
  in
  match shell with
  | None ->
      (* noindex: a moderator-only workflow surface — not for search
         indexes. *)
      (* Degraded document (pass 19): [shell] is None only when the durable
         community record could not be re-read for launch chrome AFTER the
         read model already authorized this reviewer (a mid-request deletion
         race or a storage failure in the decorative load). Without a
         trustworthy Db.community there is no honest community sidebar,
         rail tile, or analytics group — so the queue body renders inside
         the chrome-free launch message document instead: no fabricated
         community data, no extra queries, no behavior script, no
         notification fetch. The create-shell wrapper create_page used to
         emit is kept verbatim so the test-sliced feature fragment
         (create-shell → </main>) stays byte-identical; [user] only fed the
         legacy top bar, which this document intentionally has none of. *)
      Components.launch_message_page ?request ~noindex:true
        ~title:"Project home requests"
        ~content:(Printf.sprintf "<div class='create-shell'>%s</div>" body) ()
  | Some shell ->
      (* Cartographic Civic conversion: only the outer document changes.
         The queue panel renders inside the shared community-settings shell
         (header band + grouped settings navigation, Project home requests
         active — the viewer is top_mod-or-durable-admin by the read model's
         SQL, so the full permitted nav is honest); everything inside the
         panel is the exact body above. *)
      let content =
        Community_settings_shell.wrap
          ~slug:shell.community_record.Db.slug
          ~active:Community_settings_shell.Home_requests
          ~can_complete_setup:
            (Community_settings_shell.can_complete_setup
               ~community:shell.community_record ~authorized:true)
          ~network_manager:true ~panel:body ()
      in
      Components.launch_community_page ?user ?request ~noindex:true
        ~rail_communities:shell.rail_communities
        ~community:shell.community_record ~sidebar:shell.sidebar
        ~page_class:"launch-project-home-review"
        ~title:"Project home requests"
        ~content ()
