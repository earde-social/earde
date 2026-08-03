(** Transactional writes for the shared-thread placement lifecycle:
    request, accept, reject, withdraw, remove.

    One canonical discussion stays one [posts] row. These functions never
    create, copy, edit, tombstone, or delete a post, a comment, or an origin
    section; they only manage the destination-local placement rows beside
    it, so removing a placement can never touch the canonical thread,
    another placement, or the comment tree.

    Each call is one explicit transaction. Every successful mutation writes
    exactly one {!Shared_thread_placement_audit} event and its
    {!Shared_thread_notifications} rows on that same transaction, so a
    committed placement change without its event — or an event without its
    change, or a notification without either — cannot exist. Every failure
    rolls back whole: no partial row, no orphan event, no notification, no
    timestamp left behind. A stale or losing concurrent transition fails its
    guard before reaching either, so it creates neither.

    {b The origin is never caller input.} The origin community of every
    placement is [posts.community_id] — first discovered from the post (on
    {!request}) or from the placement row (elsewhere), then re-verified
    against the locked post row and the locked placement row. That column is
    immutable in the application, so any mismatch is [Inconsistent_data],
    never a race. This first slice also restricts requesting to the origin
    side by construction: the required connection is always between the
    post's own community and the destination.

    {b Notification recipients} are computed here, at mutation time, from
    current durable rows under the held locks — never from handler input, a
    session claim, or an earlier snapshot. A {!request} notifies the
    destination community's exact ['top_mod']s in destination context; a
    {!review} notifies the original requester and the thread author (both,
    deduplicated — the author must learn the outcome even when an origin top
    moderator submitted the request) in origin context; a {!withdraw}
    notifies the destination's ['top_mod']s in destination context; a
    {!remove} notifies origin ['top_mod']s, the requester, and the author in
    origin context plus destination ['top_mod']s in destination context.
    The acting user is always excluded, the set is deduplicated (a user
    reachable through both sides is notified once, in origin context), zero
    recipients is a legitimate outcome, and ['mod'] / ['legacy_mod'] /
    [users.is_admin] are never recipients. No email is sent and no external
    call is made inside the transaction.

    {b Lock order.} There is exactly one order and every function follows a
    prefix of it whole: the two [communities] rows in ascending id order
    ([FOR SHARE]); the pair's accepted [community_connections] row
    ([FOR SHARE]) when creating or accepting; the canonical [posts] row
    ([FOR SHARE]) when its content or author is read; the chosen
    destination [community_sections] row ([FOR SHARE]) when accepting into a
    section; then the exact [shared_thread_placements] row ([FOR UPDATE]).
    [FOR SHARE] — rather than the weaker [FOR KEY SHARE] — because the
    eligibility decision reads visibility, onboarding state, and
    discoverability (and, on the destination, the section mode), and only
    [FOR SHARE] conflicts with the row lock a concurrent visibility,
    publication, tombstoning, or section-deletion write takes. [FOR SHARE]
    locks are mutually compatible, so placement transactions sharing a
    community, connection, post, or section never wait on each other, and
    the placement row is the only [FOR UPDATE]. The community-then-
    connection prefix is the same relative order the connections store
    takes, so the two stores cannot form an ordering cycle either; no
    existing statement locks a section or community after a post, so the
    post-then-section extension introduces none.

    Every function begins with one {b unlocked} discovery read — the post's
    origin on {!request}, the placement's subjects elsewhere — purely to
    decide the lock order. That read is never the race-safety mechanism:
    the guarded [UPDATE] re-verifies status and subject under the row lock,
    and the locked rows' subjects are additionally checked against the
    discovered ones — those columns are immutable, so a difference is
    durable corruption, not a race.

    {b Race safety} is the partial unique index over
    [(post_id, destination_community_id) WHERE status IN
     ('pending','accepted')] and the status-guarded [UPDATE ... WHERE],
    never a read-before-write check. Two concurrent requests for the same
    thread and destination resolve to exactly one active placement; the
    loser gets [Active_placement_exists] and writes nothing. Concurrent
    review/withdraw/remove of one row commit exactly once; every loser
    fails its guard. A uniqueness conflict never surfaces raw SQL.

    {b Authorization is not implemented here.} These functions take the
    acting user only as provenance and the acting community only as the
    subject boundary the SQL verifies. Nothing checks moderator roles,
    membership, authorship, [users.is_admin], or community visibility — the
    handler slice owns the finalized policy (author-while-member or origin
    top mod or durable admin may request; destination top mod or durable
    admin reviews; requester or origin top mod or durable admin withdraws;
    either side's top mod or durable admin removes), and no value returned
    here grants anything. The subject boundaries below are exactly the ones
    that policy needs: review binds to the destination, withdrawal to the
    origin, removal to either side.

    {b Eligibility and the standing connection} are the durable policy this
    module does enforce, and only where creating a destination placement is
    at stake. {!request} and an accepting {!review} require, under the held
    locks: an accepted [community_connections] row between the post's
    origin community and the destination (read through the same
    LEAST/GREATEST identity the connections store writes, never a second
    copy of its policy), both communities satisfying
    {!Community_connections.connection_eligible}, a non-tombstoned
    canonical post ({!Shared_thread_placements.post_content_tombstoned} —
    the same closed rule the comment gate uses), and — when accepting — a
    coherent section choice: a sectioned destination must name one of its
    own live sections (locked against concurrent deletion), a sectionless
    destination must accept with no section. A rejecting {!review},
    {!withdraw}, and {!remove} deliberately require none of that: stale
    work must stay closeable after a disconnect, an eligibility change, a
    tombstoning, or a section deletion.

    Error privacy: every error is payload-free, every collapsible cause
    collapses into one variant, and no printer or serializer exists — the
    store cannot be used to probe communities, posts, connections,
    placements, or history. Which of missing/private/draft/undiscoverable
    an unavailable subject was never crosses. Nothing is logged. *)

type decision =
  | Accept of int option
      (** The destination reviewer's section choice: [Some section_id] into
          one of the destination's own sections, [None] into a flat
          (sections-disabled) destination's surface. The store validates
          the choice against the destination's current section mode. *)
  | Reject

type created_placement
(** Proof that the complete pending request committed. Carries only the
    local [shared_thread_placements.id]. *)

val created_placement_id : created_placement -> int64
(** The created placement's local row id — an internal identifier for later
    workflow wiring, not a bearer credential. *)

type reviewed_placement
(** Proof that the complete review committed. Subjects cross back so the
    caller can address either side without a second read. *)

val reviewed_placement_id : reviewed_placement -> int64

val reviewed_post_id : reviewed_placement -> int

val reviewed_origin_community_id : reviewed_placement -> int

val reviewed_destination_community_id : reviewed_placement -> int

val reviewed_status : reviewed_placement -> Shared_thread_placements.status
(** {!Shared_thread_placements.Accepted} or
    {!Shared_thread_placements.Rejected}, matching the decision. *)

val reviewed_destination_section_id : reviewed_placement -> int option
(** The accepted destination section, [None] on a rejection or a flat
    acceptance. *)

type withdrawn_placement
(** Proof that the complete withdrawal committed. *)

val withdrawn_placement_id : withdrawn_placement -> int64

val withdrawn_post_id : withdrawn_placement -> int

val withdrawn_origin_community_id : withdrawn_placement -> int

val withdrawn_destination_community_id : withdrawn_placement -> int

type removed_placement
(** Proof that the complete removal committed. *)

val removed_placement_id : removed_placement -> int64

val removed_post_id : removed_placement -> int

val removed_origin_community_id : removed_placement -> int

val removed_destination_community_id : removed_placement -> int

type error =
  | Invalid_user_id
  | Invalid_placement_id
  | Invalid_post_id
  | Invalid_community_id
  | Invalid_request_note
  | Same_community
      (** The destination is the post's own origin community. *)
  | Community_unavailable
      (** A referenced community is missing, on either side alike, without
          saying which. *)
  | Post_unavailable
  | Post_tombstoned
      (** The canonical content carries a deletion tombstone; the thread is
          not shareable and not acceptable. *)
  | No_accepted_connection
      (** The origin and destination hold no accepted mutual connection —
          none ever, a still-pending request, or a rejected/removed
          history, without saying which. *)
  | Origin_ineligible
      (** The post's origin community may not currently take part in
          creating a placement. *)
  | Destination_ineligible
      (** The destination community may not currently take part in creating
          one. Callers rendering for the other side must collapse this into
          their generic "target unavailable" outcome: which of missing,
          private, draft, or undiscoverable it was must not cross. *)
  | Active_placement_exists
  | Invalid_destination_section
      (** The accept-time section choice is incoherent with the
          destination: a missing section, another community's section, a
          section supplied to a flat destination, or none supplied to a
          sectioned one — collapsed alike. *)
  | Review_unavailable
  | Withdrawal_unavailable
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

val request :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  post_id:int ->
  destination_community_id:int ->
  request_note:string option ->
  (created_placement, error) result Lwt.t
(** Creates exactly one [pending] placement of the canonical post into the
    destination community.

    The origin community is derived from the locked post row — a caller
    cannot substitute one. The raw note is canonicalized through
    {!Shared_thread_placements.create_pending} (blank collapses to absent;
    an invalid note is [Invalid_request_note]). Under the held locks the
    store then requires the pair's accepted connection
    ([No_accepted_connection]), a live non-tombstoned post
    ([Post_unavailable] / [Post_tombstoned]), and both communities
    currently eligible ([Origin_ineligible] first, then
    [Destination_ineligible]).

    [Active_placement_exists] deliberately collapses an existing pending
    request, an existing accepted placement, the same request arriving
    twice, and any concurrently committed winner — without identifying
    which. Historical rejected, removed, and withdrawn rows fall outside
    the uniqueness predicate: after any terminal state a fresh request
    succeeds as a new row. One post may hold placements in any number of
    {i distinct} destination communities at once. *)

val review :
  (module Caqti_lwt.CONNECTION) ->
  reviewer_user_id:int ->
  placement_id:int64 ->
  destination_community_id:int ->
  decision:decision ->
  (reviewed_placement, error) result Lwt.t
(** Accepts or rejects the exact still-pending placement [placement_id]
    whose destination is [destination_community_id].

    The destination is verified inside the SQL mutation boundary — both in
    the locking [SELECT] and again in the guarded [UPDATE] — so a caller
    cannot review a request addressed to another community even if the row
    id is guessed, and a concurrent transition cannot be overwritten. The
    transition itself is the pure {!Shared_thread_placements.apply} over
    the value reconstructed from the locked row; the store never hand-rolls
    a second status machine.

    [Review_unavailable] collapses every zero-row cause alike: no such
    placement, another community's request, an already accepted, rejected,
    withdrawn, or removed row, a vanished post (which cascades its
    placements away), and a concurrent transition that committed first.
    Nothing is written in any of those cases, and no audit event is
    appended.

    Accepting additionally requires, under the held locks: the pair's still
    accepted connection ([No_accepted_connection]); both communities still
    eligible ([Destination_ineligible] first — the reviewing community, the
    one its own moderators may hear about — then [Origin_ineligible]); a
    non-tombstoned canonical post ([Post_tombstoned]); and a coherent
    section choice ([Invalid_destination_section], which covers a missing
    section, another community's section, a section supplied to a flat
    destination, and none supplied to a sectioned one alike, and locks the
    chosen section against concurrent deletion). A [None] section on a
    sections-disabled destination accepts into its flat surface. Rejecting
    reaches none of those checks and stays available on a disconnected,
    ineligible, or tombstoned pair, so a request can always be closed. *)

val withdraw :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  placement_id:int64 ->
  origin_community_id:int ->
  (withdrawn_placement, error) result Lwt.t
(** Withdraws the exact still-pending placement [placement_id] whose origin
    is [origin_community_id] — the origin side taking its own request back
    before review.

    The origin is verified inside the SQL mutation boundary, both in the
    locking [SELECT] and again in the guarded [UPDATE]. Withdrawal is
    cleanup: it deliberately requires no eligibility, no connection, no
    membership, and no live content, so a stale request survives none of
    those changes. The resulting [withdrawn] row is terminal and
    structurally distinct from [removed] — it never carries a review or a
    removal — and frees the active slot, so a fresh request may follow as a
    new row.

    [Withdrawal_unavailable] collapses every zero-row cause alike: no such
    placement, another community's request, an already reviewed, withdrawn,
    or removed row, a vanished post, and a concurrent transition that
    committed first. Nothing is written in any of those cases. *)

val remove :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  placement_id:int64 ->
  acting_community_id:int ->
  (removed_placement, error) result Lwt.t
(** Removes the exact accepted placement [placement_id] on behalf of
    [acting_community_id].

    An accepted placement concerns both sides, so either may detach it: the
    SQL mutation boundary requires [acting_community_id] to be the
    placement's origin or destination — in both the locking [SELECT] and
    the guarded [UPDATE] — and records the acting user in
    [removed_by_user_id]. Removal is cleanup: it deliberately requires no
    eligibility, no connection, and no live content, and it survives
    destination-section deletion (the accepted section column keeps its
    [SET NULL] history). It never touches the canonical post, its comments,
    its origin section, or any other placement of the same post.

    [Removal_unavailable] collapses every zero-row cause alike: no such
    placement, a still-pending request, a rejected, withdrawn, or already
    removed row, a community outside the pair, a vanished post, and a
    concurrent removal that committed first. Nothing is written in any of
    those cases. The historical removed row stays behind, outside the
    active uniqueness predicate, so a later fresh request for the same
    thread and destination is possible. *)
