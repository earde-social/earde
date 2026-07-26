(** The server-rendered setup-and-publication page for one provisioned
    network community — GET [/c/:slug/setup]. Pure rendering over
    handler-supplied view models: no Caqti, no read-model dependency, no
    session access, no database, and no logging.

    The page never raises. Every caller-controlled string is escaped at the
    template boundary, no identifier of any kind is part of a model, and a
    draft slug outside the canonical network grammar suppresses the form
    rather than emitting an action that could not work. Nothing rendered is
    Markdown or HTML: a description is escaped text. The page carries no
    script, inline style, event handler, or refresh redirect, and is
    rendered noindex — it is a private setup surface. *)

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

(** The four application controls, as submitted-or-suggested strings. An
    absent description is the empty string, never the text ["None"]. *)
type form_values = {
  community_name : string;
  community_slug : string;
  community_description : string;
  publication_visibility : string;
}

(** One generic message per rejected outcome. The last three are dormant in
    this slice: the POST route they belong to is deliberately not registered
    yet, and no handler can produce them today. *)
type feedback =
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description
  | Invalid_publication_visibility
  | Community_slug_unavailable
  | Draft_unavailable
  | Publication_failed

val network_community_publication_page :
  ?user:string ->
  ?request:Dream.request ->
  community:community ->
  project:project ->
  values:form_values ->
  feedback:feedback option ->
  unit ->
  string
(** Renders the complete page.

    Exactly one form is emitted, with method [POST] and an action built
    structurally from [community.slug] as [/c/<slug>/publish] — the route
    that will exist once publication ships, and which this slice does not
    register. It carries exactly four application fields
    ([community_name], [community_slug], [community_description],
    [publication_visibility]) and no hidden identifier, current slug, return
    URL, lifecycle state, or indexability/discoverability flag: the route
    supplies the community, and the future handler re-derives the publisher
    from the session and reauthorizes everything durably.

    Publication renders as exactly two radio options, [public] and
    [unlisted]. There is no fully private option anywhere on the page — a
    network community has no such published shape. A
    [values.publication_visibility] of exactly ["public"] or ["unlisted"]
    preselects that option; the empty string — no choice yet — falls back to
    [public]; and any other value, including ["private"] and every
    case-variant or padded spelling the parser rejects, preselects neither
    option rather than moving the publisher onto the more exposed one.

    When [request] is supplied, Dream's own framework CSRF hidden field is
    emitted inside the form; pure rendering calls omit it, keeping the page
    testable without a server. The submit button is nameless and there is no
    JavaScript confirmation.

    If [community.slug] is not canonical under the network grammar
    ([^\[a-z0-9\]+(-\[a-z0-9\]+)*$] within 80 characters) the whole form is
    suppressed and the page degrades to its explanatory copy. No project
    link is ever emitted — no public project route exists — so a malformed
    project slug can only ever appear as escaped text, and does not here:
    the project is rendered by name, kind, and GitHub namespace only.

    Copy is factual throughout: the connected project is described as
    connected or verified through GitHub, never as official, GitHub-approved,
    or GitHub-endorsed, and publication is never described as granting a
    project steward any permission they do not already durably hold. *)
