(** Server-rendered existing-community home choice page — pure rendering
    over handler-supplied view models. No Caqti, no read-model dependency,
    no session access, no logging, no JavaScript; the page is complete
    before any script runs and is always rendered noindex.

    Semantics the templates enforce:

    - exactly one [POST /projects/<project-slug>/request-home] form in
      {!Choose_existing}, and only there; its action is built structurally
      from the canonical page-model slug (the route supplies the slug — no
      hidden project, user, relation, installation, GitHub, or return-URL
      field exists);
    - the application fields are exactly [target_community_id] (one radio
      per valid community — positive id, not [Currently_unavailable] —
      never preselected) and [request_note] (escaped verbatim, never
      rendered as Markdown/HTML);
    - Dream's framework CSRF field is emitted only when [request] is
      supplied, keeping pure rendering testable without a server;
    - an active relation always suppresses the request form, and malformed
      view models degrade instead of raising: an invalid project slug
      drops every actionable form and project-derived link, a non-positive
      community id renders no radio input, an invalid community slug
      renders no community link, duplicate ids render at most one
      actionable option, and a state with no valid option renders no form;
    - copy stays factual ("Project connected through GitHub"): no
      "Official ..." or "GitHub-approved/-endorsed" claims, no member or
      activity counts, no fabricated badges, and nothing implying that
      verification or an accepted home grants community moderation
      rights. *)

(** [Public] and [Unlisted] carry their exact Earde meanings (the two
    published network shapes); [Currently_unavailable] is the generic
    presentation state for an active relation's target that later became
    ineligible — the page renders only "Currently unavailable", never the
    reason (private/draft/legacy), and never treats such a community as
    selectable. Presentation state, not authorization. *)
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
  | Accepted_home of community

type state =
  | Choose_existing of {
      project : project;
      communities : community list;
      request_note : string;
    }
      (** The choice form: eligible communities in the read model's
          deterministic order, plus the note prefill (a failed submission
          being re-rendered, or empty). *)
  | No_eligible_communities of project
      (** No form; explains the absence and links back to
          [/projects/<slug>/setup] when the slug is valid. *)
  | Active_relation of {
      project : project;
      relation : active_relation;
    }
      (** Pending or accepted home state; never renders request controls,
          and the private note is never shown here. *)

(** Generic, ID-free outcome copy. Cosmetic only — feedback never alters
    authorization or which controls render. *)
type feedback =
  | Request_form_invalid
  | Community_unavailable
  | Active_home_exists
  | Request_failed

val project_home_choice_page :
  ?user:string ->
  ?request:Dream.request ->
  state:state ->
  feedback:feedback option ->
  unit ->
  string
