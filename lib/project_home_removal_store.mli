(** Transactional removal of the exact accepted home relation between one
    permanent project and one Earde community.

    Removal is the only accepted → removed path. Either side of the
    relation may take it, with current durable authority: a current row in
    [project_stewards] for the locked project, a current ['top_mod'] row in
    [community_moderators] for the locked community, or the durable
    [users.is_admin] flag — never a session boolean, project creator or
    GitHub provenance, community membership, a lower moderator role, or a
    browser-supplied claim. All three sources are consulted and locked in
    one fixed order on every call, even after one of them already
    qualifies, so no caller and no future mutation path can observe a
    different authorization-row lock order.

    One explicit transaction, in the shared project-first lock order the
    request and review stores established, extended by the two extra
    authorization rows this store needs:
    [open_source_projects] → [communities] → [project_stewards] →
    [community_moderators] → [users] → the exact accepted
    [community_projects] row. Every row is taken [FOR UPDATE], so a
    concurrent steward deletion, moderator removal or downgrade, admin
    revocation, verification change, or lifecycle change serializes
    against the removal instead of racing it.

    Unlike the request and review stores, removal deliberately requires
    neither current project verification nor current community host
    eligibility. A project whose verification went stale or was revoked,
    and a community that has since gone private, unpublished, or
    non-network, must still be separable — otherwise a home would stay
    durably attached precisely when either side most wants it detached.
    Valid lifecycle drift is therefore not corruption here; only unknown
    enum values, mixed publication flags, and contradictory durable fields
    are.

    The lifecycle decision itself is the pure {!Project_home_relation}
    transition applied to the accepted value reconstructed from the locked
    row — the store never hand-rolls a second status machine, and the
    reconstruction validates the abstract status/note state only: the pure
    domain carries no requester/reviewer provenance and none is invented.

    Removal changes exactly three columns of exactly one row (status,
    [removed_at], [updated_at]). It deletes no project, community, or
    content; touches no membership, moderator role, or stewardship;
    creates no new relation; and records no actor — there is no durable
    [removed_by_user_id] column and [reviewed_by_user_id] is never
    overwritten with the remover. Actor provenance belongs to future
    audit/modlog infrastructure. The historical row stays behind, outside
    the one-active-home partial unique index, so the project's active slot
    is free for a later request or dedicated-home provisioning; the
    removed row itself is never reopened or reused.

    On every failure the transaction rolls back completely: the relation
    stays accepted with its requester, reviewer, note, and timestamps
    intact.

    Error privacy: every error is payload-free and every collapsible cause
    collapses ([Actor_unauthorized] covers all three absent authorities
    alike, [Removal_unavailable] every zero-row relation cause alike), so
    the store cannot be used to probe projects, communities, roles,
    stewardship, or relation history. No tokens, OAuth or PKCE material,
    session bindings, GitHub installation/account/repository identifiers,
    or private repository metadata are read or written. Nothing is
    logged. *)

type removed_home
(** Proof that the complete removal committed. Carries only the resulting
    closed relation status — no relation, project, community, actor,
    requester, reviewer, note, timestamp, or authorization source. *)

val resulting_status : removed_home -> Project_home_relation.status
(** Always {!Project_home_relation.Removed}. *)

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_slug
  | Project_unavailable
  | Community_unavailable
  | Actor_unauthorized
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

val remove :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  project_slug:string ->
  community_slug:string ->
  (removed_home, error) result Lwt.t
(** Removes the exact accepted home relation between the project named by
    [project_slug] and the community named by [community_slug], on the
    durable authority of [actor_user_id].

    Pure validation precedes any SQL: a non-positive [actor_user_id] is
    [Invalid_user_id]; a [project_slug] outside the canonical permanent
    grammar (1–80 ASCII characters, [^[a-z0-9]+(-[a-z0-9]+)*$], no
    trimming, lowercasing, or repair) is [Invalid_project_slug]; a
    [community_slug] outside the canonical addressable shape (one
    non-empty URL path segment — no ['/'], ASCII whitespace, controls, or
    DEL) is [Invalid_community_slug].

    [Project_unavailable] is a missing project — a stale or revoked
    project is deliberately not hidden here, because removal must stay
    available across the whole closed verification vocabulary.
    [Community_unavailable] is a missing community — a private,
    unpublished, or legacy community is likewise not hidden, because
    removal must stay available across valid lifecycle drift.
    [Actor_unauthorized] collapses every insufficient authority alike:
    ordinary user, non-member, project creator without stewardship,
    removed steward, steward of an unrelated project, community ['mod'] or
    ['legacy_mod'], moderator of another community, removed or downgraded
    top moderator, and an admin claim without durable [users.is_admin]
    backing. An actor who independently qualifies through more than one
    source still performs exactly one removal, and which source qualified
    is neither exposed nor persisted. [Removal_unavailable] collapses
    every zero-row relation cause alike: no relation, a pending request, a
    rejected history row, an already removed row, an accepted home
    targeting another community, and a concurrent removal that committed
    first. Malformed durable data ([Inconsistent_data]) and unexpected
    database failures ([Storage_error]) roll back completely: on any
    failure the accepted relation and every authorization row remain
    exactly as they were. *)
