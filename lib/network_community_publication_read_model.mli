(** Publisher-authorized read model behind the final setup surface of one
    provisioned network community — the data for GET [/c/:slug/setup].
    Read-only: this feature module owns its SQL, performs no network IO, no
    logging, takes no locks, and never writes.

    Authorization is decided entirely in SQL: the view is loaded only when the
    supplied Earde user is a current [top_mod] of this exact community or a
    durable global administrator ([users.is_admin]). A session [is_admin] claim
    is never consulted — it is not durable state and cannot authorize a
    lifecycle surface. An ordinary member, a [mod], a [legacy_mod], a moderator
    of another community, and a removed or downgraded top moderator all fail the
    same way as a stranger.

    The same statement also pins the lifecycle exactly: only a community that is
    still a private, non-indexable, non-discoverable network setup draft can be
    loaded. A legacy community and an already-published network community are as
    absent as one that never existed.

    Nothing internal crosses: no community, project, relation, membership, or
    moderator row id, no actor id, no relation provenance (requester, reviewer,
    note), no GitHub installation, account, or repository external id, no
    timestamps, no serializers, and no printers. The closed error variant is
    payload-free — Caqti/PostgreSQL details (which can echo SQL parameters) are
    dropped, never returned or logged.

    Concurrency: reading takes no locks and writes nothing, so the draft may be
    published, removed, or lose its home relation after the read, and the
    requested slug may be taken between this GET and the eventual POST. Nothing
    here queries slug availability: a GET-time answer would be a race the
    publisher could only lose later, and the future atomic publication
    transaction is authoritative for all of it. *)

type community
(** The draft community's current canonical identity. Deliberately abstract;
    only the accessors below are exposed. *)

type project
(** The public identity of the one project this community is the home of.
    Deliberately abstract. *)

type view
(** One authorized load: the draft, its connected project, and the durable
    source of the publisher's authorization. *)

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_community_slug
      (** The supplied route value is not a single addressable URL path segment
          (non-empty, and free of ASCII whitespace, control bytes, DEL, and
          ['/']) — the strongest community-addressability predicate in
          production, shared with the project-home request, review, and removal
          surfaces. Rejected before any SQL runs; route values are never
          trimmed, lowercased, or repaired. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: a non-positive id, a
          stored slug that differs from the supplied one or is not canonical
          under the scoped network grammar, a name or description outside the
          scoped network identity policy, a lifecycle combination the
          {!Network_communities} invariants forbid, a draft with no member or no
          [top_mod], a missing or duplicated General section or general channel,
          zero or more than one accepted home project, a contradictory active
          home relation on either side, an off-enum project kind or verification
          status, or a malformed project identity. Payload-free on purpose;
          never a partial view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped, never
          returned or logged: they can echo SQL parameters. *)

val load_for_publisher :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  community_slug:string ->
  (view option, error) result Lwt.t
(** The setup view for the one network-community setup draft with this exact
    slug, only when [user_id] is a current [top_mod] of it or a durable global
    administrator. Slug, the network marker, the complete draft lifecycle
    ([onboarding_state = 'draft'], [visibility = 'private'], not indexable, not
    discoverable), and the authorization source are decided together inside the
    SQL — never loaded and checked afterward.

    [Ok None] deliberately collapses every unavailable state — a nonexistent
    slug, a legacy community, an already-published network community, an
    ordinary member, a [mod], a [legacy_mod], a moderator of a different
    community, a removed or downgraded top moderator, a session-shaped admin
    with no durable [users.is_admin] row, and a stranger — so probing slugs
    cannot become an authorization or lifecycle oracle.

    Beyond the load, the complete draft is revalidated with a bounded number of
    further reads and no N+1: the community must have at least one current
    member and at least one current [top_mod], exactly one General forum section
    and one active general channel, and exactly one accepted [home] relation,
    which must also be the only active ([pending] or [accepted]) home relation
    on both the community side and the linked project's side. Any other shape —
    including a pending-only relation, a deleted project row, or a duplicated
    accepted relation — is {!Inconsistent_data}, never a silently narrowed view.

    Project verification is validated as a known closed value ([verified],
    [stale], or [revoked]) but deliberately does not gate the view. The project
    was verified when the community was provisioned and the home relation
    accepted; later token drift is a re-verification concern, not a reason to
    strand a private draft that can then never be published. This matches
    {!Project_home_removal_store} and the connected-projects surface, both of
    which are indifferent to verification drift on an already accepted home, and
    differs from acceptance itself, which does require [verified]. The
    verification value is not exposed. *)

val community : view -> community
val project : view -> project

val publisher_is_top_moderator : view -> bool
(** True when the publisher holds a current [top_mod] row in this community.
    Independent of {!publisher_is_durable_admin}: both may be true, and at least
    one always is. *)

val publisher_is_durable_admin : view -> bool
(** True when the publisher holds [users.is_admin] durably. Never derived from a
    session field. *)

val community_name : community -> string

val community_slug : community -> string
(** The canonical persisted slug, byte-identical to the accepted route value. *)

val community_description : community -> string option
val project_name : project -> string
val project_slug : project -> string

val project_namespace_login : project -> string
(** The verified GitHub namespace login as persisted at finalization time —
    display metadata, never an authorization source. *)

val project_kind : project -> Project_identity.kind
