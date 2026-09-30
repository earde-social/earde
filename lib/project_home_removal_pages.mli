(** The two removal controls for an accepted project-home relation: the
    steward-facing form on [/projects/:slug/request-home] and the
    moderator-facing "Connected projects" management section inside the
    existing community settings surface.

    Pure rendering over caller-supplied page models — no Caqti, no read
    model, no store, no session access, no SQL, no IO, no logging. Dream is
    used for one thing only: emitting the framework CSRF field when a live
    request is supplied, so pure rendering stays testable without a server.

    These are fragments, not pages: each is composed into an existing
    surface, which keeps sole ownership of chrome, authorization, cache
    policy, indexing, referrer policy, and analytics behaviour.

    This module performs no authorization. Rendering a form is never proof
    that the viewer may remove anything — the transactional
    {!Project_home_removal_store} independently reauthorizes every POST
    against the three durable sources. The page that emitted a form
    therefore grants nothing.

    Language rules: removal is an association-only operation and is
    described as exactly that. The copy never implies that removal deletes
    the project, the community, posts, comments, chat messages, channels,
    sections, stewardship, moderation, membership, GitHub verification, or
    repositories.

    No identifier of any kind is part of the models, so none can be
    emitted: no relation, project, community, or user id, no requester or
    reviewer, no request note, no timestamps, no acceptance provenance, and
    no GitHub installation/account/repository external ids. Canonical
    public project and community slugs appear in form action paths as the
    intentional route identities they are. Every caller-controlled string
    is escaped at the template boundary, and malformed models degrade to
    inert text instead of raising. *)

(** The project's current GitHub verification state, shown so a moderator
    can see plainly which state a connected project is in rather than
    having it silently dropped. Removal is available in all three. *)
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

val project_side_removal_form :
  ?request:Dream.request ->
  removal_allowed:bool ->
  project_slug:string -> community_slug:string -> unit -> Html.t
(** The steward-facing removal control for the project's one accepted home,
    for insertion into the accepted state of the home-choice page.

    [removal_allowed:false] — the caller's durable derivation that the home
    belongs to an unpublished dedicated-community setup draft, which the
    removal store refuses to detach — renders the section heading and one
    restrained sentence naming the structural reason and the ordinary way
    forward, and nothing else: no form, no action path built anywhere in
    that branch, no hidden field, no disabled control, no script, and no
    destructive alternative. The copy promises no publication outcome, and
    names no lifecycle column, authority, or provenance. No setup link is
    offered: project stewardship does not establish that the viewer may
    reach the community's setup surface, and this model carries nothing
    that would.

    [removal_allowed:true] renders exactly one [POST
    /projects/<project-slug>/community-home/<community-slug>/remove] form
    whose action is built structurally from the two supplied canonical
    slugs. The form carries zero application fields, no hidden id, slug, or
    return URL, and a nameless submit button — the route path is the whole
    request. Dream's framework CSRF field is emitted only when [request] is
    supplied.

    A project slug outside the canonical permanent grammar, or a community
    slug that is not a single addressable URL path segment, yields the
    heading and association-only copy with no form at all: an
    unbuildable action never becomes a broken or guessable one, and no
    route value leaks into a hidden field instead. Without [request] the
    form would be unsubmittable, so it is likewise not rendered. *)

val community_side_management_section :
  ?request:Dream.request ->
  removal_allowed:bool ->
  community_slug:string ->
  projects:connected_project list -> unit -> Html.t
(** The "Connected projects" management section for the existing community
    settings surface, listing each accepted connected project's public
    identity and its removal control.

    [removal_allowed:false] — the settings surface's own derivation that
    this community is an unpublished network setup draft, whose provisioned
    home the removal store refuses to detach — keeps every connected
    project's identity visible, so a moderator still sees which project the
    draft belongs to, but makes every row inert: no row builds an action
    path, and the section copy states the draft-integrity reason in place of
    the association-only warning, which would otherwise describe a control
    this surface does not offer. The section is still rendered, so the
    settings panel and its navigation entry stay in place alongside the
    existing "Complete setup and publish" link.

    [removal_allowed:true] renders, for each actionable project, one [POST
    /c/<community-slug>/projects/<project-slug>/remove-home] form, built
    structurally from the two canonical slugs, with zero application
    fields, no hidden identifier, and a nameless submit button. Dream's
    framework CSRF field is emitted only when [request] is supplied.

    [projects = []] renders the section with restrained settings copy
    ("No connected projects.") and no form — the section is a permanent
    settings panel, so unlike the public page fragment it does not vanish
    when a community has no accepted home.

    Defensive rendering, never an exception: an unaddressable community
    slug drops every form on the section; a project whose slug is outside
    the canonical grammar renders its identity with no form; a project slug
    repeated in the list leaves at most the first occurrence actionable; a
    blank project name falls back to a generic safe label; and a malformed
    namespace login renders escaped. The section contains no inline styles,
    scripts, event handlers, [javascript:] URLs, or refresh behaviour, and
    no requester, reviewer, note, timestamp, or internal identifier. *)
