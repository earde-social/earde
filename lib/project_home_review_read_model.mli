(** Moderator-authorized read model behind the pending project-home review
    queue for one target community — the data for the future moderator-only
    review page. Read-only: this feature module owns its SQL, performs no
    network IO, no logging, takes no locks, and never writes.

    Authorization is decided entirely in SQL, using the exact durable model
    the transactional {!Project_home_review_store} enforces: the reviewer is
    a current [top_mod] of the target community, or a durable global
    administrator ([users.is_admin]). Session-shaped admin claims, project
    stewardship, ordinary membership, [mod]/[legacy_mod] roles, and
    moderator roles in another community are never consulted. Every
    unauthorized or missing-community outcome collapses to [Ok None], so
    probing a community slug cannot become an authorization oracle.

    The read takes no locks: an informational GET has nothing to serialize,
    and the transactional review store independently reauthorizes and locks
    every durable row on the later accept/reject POST. A pending request may
    legitimately outlive verification or host eligibility, so the queue
    lists stale and revoked projects and requests against a
    currently-ineligible community — the page suppresses acceptance for
    those, but rejection stays available.

    Nothing internal crosses: no relation id, project id, community id,
    requester or reviewer user id, timestamps, moderator-role rows,
    membership data, installation or GitHub external ids, repository
    external ids, serializers, or printers. The private request note is
    exposed only to this authorized moderator view. The closed error variant
    is payload-free — Caqti/PostgreSQL details (which can echo SQL
    parameters) are dropped, never returned or logged. *)

type community
(** The target community's public identity plus its current host
    eligibility. Abstract; the community id, creator, membership,
    moderation, and counts never cross. *)

type repository
(** One permanent project repository's public identity. Abstract; the
    GitHub external id, position, description, and default branch never
    cross. *)

type pending_request
(** One pending [home] request targeting the authorized community: the
    requesting project's public identity and repositories, its current
    verification status, the requester's public name (when still present),
    and the private request note. Abstract; no relation, project, requester,
    or reviewer id crosses. *)

type view
(** One authorized load: the target community and its pending requests in
    the deterministic queue order. *)

(** The requesting project's current verification status, exposed in full to
    this authorized workflow: [Stale] and [Revoked] are deliberately not
    collapsed, so a moderator understands why acceptance is unavailable. *)
type project_verification =
  | Verified
  | Stale
  | Revoked

(** Whether the target community can currently accept a project-home
    request. [Eligible] is exactly the durable eligible predicate the review
    store accepts on: a network community, [onboarding_state = 'published'],
    [visibility = 'public'], and [indexable = discoverable] (both listed or
    both unlisted). [Currently_ineligible] is any individually valid but
    presently ineligible lifecycle (legacy/non-network, draft, or a
    published network community since gone fully private); the specific
    reason is deliberately not exposed. A lifecycle that contradicts the
    durable model itself (unknown enum, [indexable <> discoverable] on a
    network community, or other corruption) is [Inconsistent_data], never
    [Currently_ineligible]. *)
type host_eligibility =
  | Eligible
  | Currently_ineligible

type error =
  | Invalid_user_id  (** [reviewer_user_id <= 0]; rejected before any SQL. *)
  | Invalid_community_slug
      (** The supplied route value is not a single non-empty URL path
          segment (no ASCII whitespace, controls, DEL, or '/'); rejected
          before any SQL. Route values are never trimmed or repaired. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: an unknown
          lifecycle enum, [indexable <> discoverable] or another
          contradictory flag combination on the target community, a
          non-positive/blank/non-addressable community identity, an
          off-enum project verification or kind, a malformed stored project
          slug or namespace login, a malformed present requester username, a
          non-canonical durable request note, a duplicate project group, a
          missing or malformed repository set (zero repositories,
          non-contiguous positions, duplicate full names, a non-canonical
          HTTPS GitHub URL, a blank full name or branch, or a primary-count
          violation). Payload-free on purpose; never a partial view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped,
          never returned or logged: they can echo SQL parameters. *)

val load_for_reviewer :
  (module Caqti_lwt.CONNECTION) ->
  reviewer_user_id:int ->
  community_slug:string ->
  (view option, error) result Lwt.t
(** The review queue for the community with this addressable slug, only when
    [reviewer_user_id] is a current [top_mod] of that community or a durable
    global administrator. Authorization happens inside the SQL — never
    loaded and checked afterward. [Ok None] deliberately collapses every
    non-viewable state: a nonexistent community, an unauthorized reviewer, a
    removed or downgraded top moderator, an ordinary member or non-member, a
    [mod]/[legacy_mod], a moderator of a different community, and a
    session-shaped admin without the durable [users.is_admin] flag.

    The queue is exactly the [home] relations targeting the community with
    [status = 'pending'] — accepted, rejected, removed, and
    auto-provisioned accepted homes never appear — ordered by
    [community_projects.created_at ASC, open_source_projects.slug ASC], with
    each request's repositories preserved in stored position order. This
    read takes no row locks. *)

val community : view -> community
val pending_requests : view -> pending_request list

val community_name : community -> string
val community_slug : community -> string

val community_host_eligibility : community -> host_eligibility
(** Whether the community can currently accept a project-home request. The
    queue is loaded regardless of eligibility so a moderator can always
    reject; the page suppresses acceptance when this is
    [Currently_ineligible]. *)

val project_name : pending_request -> string

val project_slug : pending_request -> string
(** The canonical persisted project slug — the value the accept/reject route
    path is built from. *)

val project_kind : pending_request -> Project_identity.kind
val project_namespace_login : pending_request -> string

val project_verification : pending_request -> project_verification
(** [Verified], [Stale], or [Revoked]. A request may be pending against a
    project that has since gone stale or revoked; the page shows the status
    and offers only rejection for the non-verified states. *)

val project_repositories : pending_request -> repository list
(** The project's permanent repositories in stored position order — at least
    one, at most 2,000, positions exactly [1..n], distinct full names, and
    the primary-count rule for the project kind, all revalidated before the
    request is returned. *)

val requester_name : pending_request -> string option
(** The requester's current public username, or [None] when the requesting
    account was deleted (the provenance column is [ON DELETE SET NULL]).
    [None] is not treated as corruption; a present but malformed username
    is. *)

val request_note : pending_request -> string option
(** The private request note, exposed only to this authorized moderator
    view, canonical (byte-identical to the value {!Project_home_relation}
    reconstructs). The page must HTML-escape it and never render it as
    Markdown. *)

val repository_full_name : repository -> string
val repository_html_url : repository -> string
val repository_is_primary : repository -> bool
val repository_is_archived : repository -> bool
