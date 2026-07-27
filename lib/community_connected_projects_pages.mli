(** The "Connected projects" section of the existing community page: the
    open-source projects whose accepted home relation names that community.

    Pure rendering over handler-supplied page models — no Caqti, no read-model
    dependency, no session access, no SQL, no IO. This is a fragment, not a
    page: it is composed into the real community page by the existing
    renderers, so the community route keeps sole ownership of authorization,
    chrome, cache policy, indexing, referrer policy, and analytics behavior.

    Language rules: an accepted home is a factual, GitHub-verified connection
    and is described as one. The section never says "Official project",
    "Official community", "Official home", "GitHub-approved", or
    "GitHub-endorsed", and never implies that GitHub or Earde endorses the
    project, that its stewards moderate the community, or that the community
    controls the repository.

    No identifier of any kind is part of the model, so none can be emitted: no
    relation, project, or community id, no requester or reviewer, no request
    note, no timestamps, no repository or installation external ids, no member
    or activity counts, and no moderation roles. Every caller-controlled string
    is escaped at the template boundary, and anything carrying a URL that is
    not provably safe degrades to inert escaped text rather than becoming a
    link. Malformed page models never raise. *)

type verification =
  | Verified
  | Stale
  | Revoked

type repository = {
  full_name : string;
  html_url : string;
  is_primary : bool;
  is_archived : bool;
}

type project = {
  name : string;
  slug : string;
  kind : Project_identity.kind;
  namespace_login : string;
  verification : verification;
  website_url : string option;
  repositories : repository list;
}

val connected_projects_section : projects:project list -> string
(** The section fragment, ready for insertion into the current community page,
    with the supplied project order and each project's repository order
    preserved exactly.

    [projects = []] renders the empty fragment [""]. Ordinary visitors are
    never shown a "no connected projects" placeholder: the section simply does
    not exist until a community has at least one accepted home.

    Defensive rendering, never an exception: a blank project name falls back to
    a generic safe label; a project slug repeated in the list leaves only the
    first occurrence able to carry links; a website URL that is not a safe
    http(s) target renders as inert escaped text; a repository URL that is not
    the canonical HTTPS GitHub URL for its own full name renders as inert
    escaped text; a repository full name repeated within one project leaves
    only the first occurrence linked; and an empty repository list simply
    renders the project identity with no repository links.

    The project name is never a link: no public permanent-project route exists,
    and the owner-only [/projects/:slug/setup] destination is deliberately not
    linked from a page ordinary visitors can reach. The fragment contains no
    inline styles, scripts, event handlers, [javascript:] URLs, or refresh
    behavior. *)
