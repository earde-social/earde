(** Server-rendered moderator review page for the pending project-home
    queue — pure rendering over handler-supplied view models. No Caqti, no
    read-model dependency, no session access, no logging, no JavaScript; the
    page is complete before any script runs and is always rendered noindex.

    Semantics the templates enforce:

    - each renderable request carries at most two forms — a
      [POST /c/<community-slug>/projects/<project-slug>/reject] and, only
      when the project is [Verified] and the community is [Eligible] (and the
      request has at least one repository), a matching [.../accept]; both
      actions are built structurally from the canonical page-model slugs (the
      route path supplies both identities — no hidden relation, project,
      community, reviewer, requester, decision, or return-URL field exists);
    - each form's only control is a nameless submit button; there are no
      application fields, and Dream's framework CSRF field is emitted only
      when [request] is supplied, keeping pure rendering testable without a
      server;
    - acceptance is never a disabled button: when it is unavailable the page
      renders generic non-actionable copy and leaves rejection available;
    - malformed view models degrade instead of raising: an invalid community
      slug drops every actionable form, an invalid project slug drops that
      request's forms, duplicate project slugs leave at most one actionable
      group, an invalid repository URL renders as inert text, and an empty
      repository list suppresses acceptance while leaving rejection;
    - the private request note is HTML-escaped, labelled as private workflow
      information, and never rendered as Markdown or HTML;
    - copy stays factual ("Project connected through GitHub"): no
      "Official ..." or "GitHub-approved/-endorsed" claims, no member or
      activity counts, and no relation/project/community/user identifiers,
      timestamps, or GitHub external ids ever appear. *)

(** The requesting project's verification status. [Stale] and [Revoked] are
    shown in full (unlike public/steward channels) so an authorized
    moderator understands why acceptance is unavailable. *)
type project_verification =
  | Verified
  | Stale
  | Revoked

(** Whether the target community can currently accept a project-home
    request. [Currently_ineligible] renders one generic page-level notice
    and suppresses every accept form; the specific reason
    (private/draft/legacy) is never revealed. Rejection stays available. *)
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

(** Generic, ID-free outcome copy. Cosmetic only — feedback never alters
    authorization or which controls render. *)
type feedback =
  | Review_unavailable
  | Project_unavailable
  | Target_ineligible
  | Review_failed

val project_home_review_page :
  ?user:string ->
  ?request:Dream.request ->
  state:state ->
  feedback:feedback option ->
  unit ->
  string
