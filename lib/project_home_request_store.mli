(** Transactional creation of one pending request to use an existing eligible
    network community as a verified permanent project's home.

    One explicit transaction, in the shared project-first lock order that every
    later home-relation store (dedicated-community provisioning, moderator
    review, removal) must follow: the [open_source_projects] row is authorized
    and locked first, the target [communities] row second, and the
    [community_projects] insertion happens last. Authorization is steward-only
    and lives inside the SQL itself — never [created_by_user_id], installation
    provenance, or community membership. The target community needs no
    requester-side role: this store only submits a request; target-community
    authorization belongs to the future review store, and nothing here grants a
    role, creates a membership, or accepts anything — even when the requester
    happens to moderate the target community, the created relation is [pending].

    The partial unique active-home index arbitrates every concurrent active-home
    race through an [ON CONFLICT DO NOTHING] insertion — never a
    read-before-insert check. Historical rejected/removed rows fall outside the
    index predicate and never block a fresh request.

    Error privacy: every error is payload-free, every zero-row authorization or
    eligibility cause collapses into one variant ([Project_unavailable] /
    [Community_unavailable]), and no printer or serializer exists — the store
    cannot be used to probe projects, stewardship, communities, or existing
    relations. No tokens, OAuth or PKCE material, session bindings, or GitHub
    identifiers are read or written. Nothing is logged. *)

type created_request
(** Proof that the complete pending request committed. Carries only the local
    [community_projects.id] — no project or community row, no requester, no
    note. *)

val relation_id : created_request -> int64
(** The created relation's local row id — an internal identifier for later
    workflow wiring, not a bearer credential. *)

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_id
  | Invalid_relation
  | Project_unavailable
  | Community_unavailable
  | Active_home_exists
  | Inconsistent_data
  | Storage_error

val create :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  project_slug:string ->
  target_community_id:int ->
  relation:Project_home_relation.t ->
  (created_request, error) result Lwt.t
(** Creates exactly one [pending] home relation for the steward-authorized
    verified project named by the already-canonical [project_slug] and the
    eligible target community.

    Pure validation precedes any SQL: a non-positive [user_id] is
    [Invalid_user_id]; a [project_slug] outside the canonical permanent grammar
    (1–80 ASCII characters, [^[a-z0-9]+(-[a-z0-9]+)*$], no trimming,
    lowercasing, or repair) is [Invalid_project_slug]; a non-positive
    [target_community_id] is [Invalid_community_id]; a relation whose status is
    not {!Project_home_relation.Pending} is [Invalid_relation] — an accepted,
    rejected, or removed value is never silently transitioned. The request note
    is taken only through {!Project_home_relation.request_note}, already
    canonical.

    [Project_unavailable] covers every zero-row project cause alike: missing,
    another user's, creator without stewardship, stale, revoked.
    [Community_unavailable] likewise: missing, legacy, setup draft, unpublished,
    fully private, or otherwise ineligible — eligibility is exactly "network
    community AND published AND public-or-unlisted" in the current durable
    columns. [Active_home_exists] deliberately collapses an existing pending
    request, an existing accepted home, and any concurrently won race, without
    identifying the target, status, or winner. Malformed durable data
    ([Inconsistent_data]) and unexpected database failures ([Storage_error])
    roll back completely: on any failure no relation is created and project,
    community, and relation history are unchanged. *)
