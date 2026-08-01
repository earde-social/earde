(** The public Network page of a community (GET /c/:slug/network): the
    complete connected-projects and connected-communities lists, on the shared
    Cartographic Civic community shell.

    Pure rendering. No Caqti, no read-model dependency, no session access, no
    JavaScript — the page is complete before any script runs, and every link
    works with JavaScript off.

    Both lists are supplied already rendered, by the same fragment modules the
    community page uses, and are spliced verbatim: this page restates no
    visibility, eligibility, or ordering rule of its own, and cannot present a
    relation differently from the surface it was moved off. An empty fragment
    is replaced by that fragment module's quiet empty section, because the page
    names both destinations whether or not either holds anything.

    Nothing the authorized management surface knows is representable in either
    fragment model — no lifecycle status vocabulary, direction, request note,
    requester, reviewer, remover, or connection id — so none of it can reach
    this page. *)

val community_network_page :
  ?user:string ->
  ?noindex:bool ->
  ?rail_communities:Db.community list ->
  community:Db.community ->
  sidebar:string ->
  projects_section:string ->
  communities_section:string ->
  can_connect:bool ->
  Dream.request ->
  string
(** [projects_section] and [communities_section] are the pre-rendered public
    fragments ([""] when the community has nothing publicly connected of that
    kind, which renders the quiet empty section instead).

    [sidebar] is the shared community sidebar the caller built, and
    [rail_communities] the viewer's joined communities — both supplied only
    after the route's own view authorization, exactly like the sibling
    community routes. [noindex] follows the community's existing indexing
    policy; this page adds none of its own.

    [can_connect] renders the compact link to the existing
    [/c/:slug/settings/connections/new] flow. It is presentation only: the
    caller decides it with the same top-mod-or-admin reading the community
    sidebar already uses, and the linked surface reauthorizes every request in
    SQL regardless. No other management affordance exists on this page — no
    request, review, or removal control, and no pending, rejected, or removed
    relationship. *)
