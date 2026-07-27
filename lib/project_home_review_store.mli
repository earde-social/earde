(** Transactional moderator review — accept or reject — of the exact
    pending home request between one verified permanent project and one
    target Earde community.

    One explicit transaction, in the shared project-first lock order the
    request store established and every later home-relation store must
    follow: the [open_source_projects] row is locked and validated first,
    the target [communities] row second, the reviewer's durable
    authorization row third, and the exact pending [community_projects]
    row last. Authorization is community-side and decided inside the SQL
    itself: a current ['top_mod'] row of the target community, or the
    durable [users.is_admin] flag — never a session boolean, project
    stewardship, requester identity, or creator/GitHub provenance. The
    qualifying authorization row is locked ([FOR UPDATE]) so a concurrent
    role removal, downgrade, or admin revocation serializes through it.

    The lifecycle decision itself is the pure {!Project_home_relation}
    transition applied to the pending value reconstructed from the locked
    row — the store never hand-rolls a second status machine. Acceptance
    additionally requires a currently verified project and a target
    community currently eligible to host projects (published, fully public
    network community in one of the two legal publication-flag shapes).
    Rejection requires neither: it stays available while the project is
    verified, stale, or revoked, and on a valid but currently ineligible
    target, so moderators can close a request after the project lost
    verification or the community went private, unpublished, or
    non-network — without that distinction, a stale pending relation
    would permanently occupy the project's one-active-home slot. A
    successful rejection neither restores nor modifies project
    verification or any other project metadata. The accepted relation
    keeps the same active-home index slot its pending row already held;
    rejecting frees the slot immediately for a later request or
    dedicated-home provisioning once the project is verified again.

    On every failure the transaction rolls back completely: the relation
    stays pending, requester and note are preserved, and no role,
    membership, stewardship, or project/community metadata changes.

    Error privacy: every error is payload-free and every collapsible cause
    collapses ([Project_unavailable], [Community_unavailable],
    [Reviewer_unauthorized], [Review_unavailable] each cover all of their
    zero-row causes alike), so the store cannot be used to probe projects,
    communities, roles, or relation history. No tokens, OAuth or PKCE
    material, session bindings, or GitHub identifiers are read or written.
    Nothing is logged. *)

type decision =
  | Accept
  | Reject

type reviewed_request
(** Proof that the complete review committed. Carries only the resulting
    closed relation status — no relation, project, community, requester,
    reviewer, note, or timestamp. *)

val resulting_status : reviewed_request -> Project_home_relation.status
(** {!Project_home_relation.Accepted} for {!Accept},
    {!Project_home_relation.Rejected} for {!Reject}. *)

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_slug
  | Project_unavailable
  | Community_unavailable
  | Reviewer_unauthorized
  | Review_unavailable
  | Target_ineligible
  | Inconsistent_data
  | Storage_error

val review :
  (module Caqti_lwt.CONNECTION) ->
  reviewer_user_id:int ->
  project_slug:string ->
  target_community_slug:string ->
  decision:decision ->
  (reviewed_request, error) result Lwt.t
(** Applies [decision] to the exact pending home request between the
    verified project named by [project_slug] and the community named by
    [target_community_slug], recording [reviewer_user_id] as the durable
    reviewer with coherent review timestamps.

    Pure validation precedes any SQL: a non-positive [reviewer_user_id]
    is [Invalid_user_id]; a [project_slug] outside the canonical
    permanent grammar (1–80 ASCII characters,
    [^[a-z0-9]+(-[a-z0-9]+)*$], no trimming, lowercasing, or repair) is
    [Invalid_project_slug]; a [target_community_slug] outside the
    canonical addressable shape (one non-empty URL path segment — no
    ['/'], ASCII whitespace, controls, or DEL) is
    [Invalid_community_slug].

    [Project_unavailable] covers a missing project for either decision,
    and — for {!Accept} only, decided after reviewer authorization and
    the pending-row lock — a stale or revoked project, without
    distinguishing the two; {!Reject} remains available across the whole
    closed verification vocabulary (verified, stale, revoked) so a
    pending request can always be closed. [Community_unavailable] is a
    missing community — a valid but currently ineligible community is not
    hidden here, because moderators must still be able to reject its
    pending requests. [Reviewer_unauthorized] collapses every insufficient
    authority alike: ordinary user, non-member, lower or removed
    moderator role, moderator of another community, or an admin claim
    without durable [users.is_admin] backing — stewardship, requester
    identity, and creator provenance grant nothing, while a requester who
    independently holds the qualifying community authority may review.
    [Review_unavailable] collapses every zero-row relation cause alike:
    no pending request, a request targeting another community, already
    accepted or rejected, removed, or a concurrent review that committed
    first. [Target_ineligible] is returned only for {!Accept} against a
    valid community that is not currently an eligible host. Malformed
    durable data ([Inconsistent_data]) and unexpected database failures
    ([Storage_error]) roll back completely: on any failure the relation
    remains pending and unchanged. *)
