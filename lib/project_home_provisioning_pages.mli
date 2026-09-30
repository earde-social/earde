(** [/projects/:slug/community-home/new] — server-rendered page and view models
    for the dedicated-community-home creation entry. Pure rendering: the
    complete view models arrive as arguments; this module never reads the
    environment, the session, the query string, or the database, and it
    deliberately depends on neither Caqti nor the read model — the handler
    translates read-model values into these view models. *)

type project = {
  name : string;
  slug : string;
  description : string option;
  kind : Project_identity.kind;
  namespace_login : string;
}
(** The permanent project as the creation page shows it. No project id, draft
    id, steward, installation, or account identifier exists here. *)

type form_values = {
  community_name : string;
  community_slug : string;
  community_description : string;
}
(** Exactly the three application fields the form submits, as raw strings to
    render back into the controls. The handler fills these from the read model's
    suggestions on a first view, and from the rejected submission when the
    future POST re-renders. *)

(** One generic message above the form. The three [Invalid_*] variants mirror
    {!Project_home_provisioning_form}'s semantic errors;
    {!Community_slug_unavailable}, {!Active_home_exists}, and
    {!Provisioning_failed} exist for the future POST integration and are dormant
    in the current GET-only wiring. {!Stale_form} is the framework CSRF refusal
    — a page held open past the token's lifetime, or served before a restart —
    and states only that nothing was created. No variant carries a payload, and
    no message repeats a submitted value. *)
type feedback =
  | Stale_form
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description
  | Community_slug_unavailable
  | Active_home_exists
  | Provisioning_failed

val project_home_provisioning_page :
  ?user:string ->
  ?request:Dream.request ->
  project:project ->
  values:form_values ->
  feedback:feedback option ->
  unit ->
  string
(** The complete "Create a community home" page in the shared create-flow layout
    (noindex; the handler additionally answers non-cacheable).

    Copy states that the community is created as a private setup draft reachable
    only by authorized setup users, that publication is a later explicit action
    offering Public or Unlisted (never a fully private published community),
    that a network community's home stays public with optional private rooms,
    and that the project steward becomes the community's initial top moderator.
    It renders the verification-safe phrase "Project connected through GitHub" —
    never an "Official ..." or "GitHub-approved/endorsed" claim — and never
    asserts that a draft already exists.

    Exactly one form is rendered, [method='POST'] to
    [/projects/<canonical-slug>/community-home] (a route this slice deliberately
    does not register), carrying exactly the three application fields
    [community_name], [community_slug], and [community_description] and a
    nameless submit control. There is no hidden field of any kind: the route
    path supplies the project slug, and the future POST handler re-derives the
    user from the session and reauthorizes everything through the provisioning
    store. When [request] is supplied, Dream's own hidden CSRF field is emitted
    additionally; pure rendering calls omit it, keeping the page testable
    without a server.

    Rendering is defensive and never raises: a page-model project slug outside
    the canonical permanent shape suppresses the form and every project-derived
    link entirely rather than emitting an unusable action, every
    caller-controlled string escapes at the template boundary, descriptions
    render as plain escaped text rather than Markdown or HTML, and the fragment
    emits no script, inline style, event handler, or client-side redirect. *)
