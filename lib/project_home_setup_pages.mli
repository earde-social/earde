(** /projects/:slug/setup — server-rendered page/view models for the
    permanent project-home setup destination. Pure rendering: the complete
    view models arrive as arguments; this module never reads the
    environment, the session, the query string, or the database, and it
    deliberately does not depend on Caqti or the permanent read model — the
    handler translates read-model values into these view models. *)

type account_type =
  | Personal
  | Organization

(** One permanent repository as the setup page shows it. [html_url] is the
    previously validated canonical GitHub URL; it is still gated and
    escaped normally at render time. [default_branch] may contain ['/'] and
    is rendered strictly as text, never placed into a URL. No local or
    GitHub identifier crosses into this model. *)
type repository = {
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_primary : bool;
  is_archived : bool;
}

(** The permanent project as the setup page shows it: identity, verified
    namespace, and the permanent repository list in position order. No
    project id, draft id, steward, installation, or account identifier
    exists here. *)
type project = {
  name : string;
  slug : string;
  description : string option;
  website_url : string option;
  kind : Project_identity.kind;
  namespace_login : string;
  namespace_type : account_type;
  repositories : repository list;
}

val project_home_setup_page :
  ?user:string ->
  ?request:Dream.request ->
  project:project ->
  unit ->
  string
(** The complete "Project created" page in the shared create-flow layout
    (noindex; the handler additionally answers non-cacheable).

    Renders the verification-safe phrase "Project connected through
    GitHub" — never an "Official ..." or "GitHub-approved/endorsed" claim —
    followed by the project summary (name, canonical slug, kind, namespace
    login with its Personal account/Organization label, optional
    description, optional website as a safely escaped HTTP(S) link) and the
    permanent repository list in the supplied order with Primary and
    Archived markers.

    A semantic next-step section headed "Choose a community home" offers
    one plain navigation link — "Connect to an existing community", built
    structurally as [/projects/<canonical-slug>/request-home] and rendered
    only when the page-model slug already has the canonical permanent
    shape — while dedicated-home creation stays clearly non-actionable
    future copy. No working form, state-changing button, or
    disabled-but-actionable-looking control is rendered, no project id is
    exposed, and nothing claims the project already has a home community.

    Rendering is defensive: every string escapes normally, and a website or
    repository URL that fails the shared HTTP(S) gate is rendered as plain
    text, never as an actionable link. The feature fragment emits no inline
    style, no script, and no client-side redirect. [request] is layout
    context only (topbar session display); it adds no form and no CSRF
    field because the page owns no POST. *)
