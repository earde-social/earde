(** The authorized per-thread "Share thread" view: who may share one canonical
    thread, which connected communities it could be shared with, and where its
    active placements stand. Read-only: this module owns its SQL, takes no
    locks, writes nothing, performs no network IO, and logs nothing.

    {b Authorization lives in the SQL here}, exactly as the sibling
    community-connections management read model does. The share surface loads
    only for:

    - the canonical author, while they are still a current member of the post's
      origin community, not globally banned, and not banned in that community —
      a historical author who left keeps no sharing authority;
    - an exact ['top_mod'] of the origin community;
    - a caller the session reports as a global administrator {i whose}
      [users.is_admin] flag is also still set durably — a session claim alone
      never suffices.

    Ordinary ['mod'] and ['legacy_mod'] rows, plain members, non-members,
    moderators of other communities, and every other caller produce [Ok None] —
    the same answer as a post that does not exist and as a tombstoned post, so
    this module cannot become an existence oracle. All three admitted roles can
    already read the origin community (membership, a moderator row, or global
    adminship), so an authorized view never crosses a read boundary the viewer
    lacks.

    Privacy: a pending placement's [request_note] is private workflow text. It
    crosses out of this module only when the viewer is its original requester or
    an origin-side manager (origin ['top_mod'] or durable admin); for every
    other authorized viewer — the canonical author who did not send the request
    — the note reads as absent. No requester, reviewer, or remover identity is
    ever returned. Errors are payload-free; Caqti/PostgreSQL details are
    dropped, never returned or logged. *)

type candidate
(** A destination community the thread could be requested into right now. *)

val candidate_id : candidate -> int
val candidate_name : candidate -> string
val candidate_slug : candidate -> string

type active_placement
(** One live (pending or accepted) placement of this thread. *)

val placement_id : active_placement -> int64
(** The placement's row id as an opaque record locator for an action path —
    never a grant of authority: the handler re-resolves and re-authorizes it
    server-side. *)

val placement_destination_name : active_placement -> string
val placement_destination_slug : active_placement -> string

val placement_pending : active_placement -> bool
(** [true] for a pending request, [false] for an accepted placement — the only
    two statuses this view ever returns. *)

val placement_requested_by_viewer : active_placement -> bool
(** Whether the viewing user is this placement's original requester — the fact
    the withdraw-control gate needs, without exposing who the requester is when
    it is someone else. *)

val placement_note : active_placement -> string option
(** The private request note, already gated: [Some] only on a pending placement
    whose viewer is its original requester or an origin-side manager. Everyone
    else — including the canonical author — reads [None]. *)

type share_view

val view_post_id : share_view -> int
val view_post_title : share_view -> string
val view_origin_community_id : share_view -> int
val view_origin_community_name : share_view -> string
val view_origin_community_slug : share_view -> string

val view_origin_manager : share_view -> bool
(** Whether the viewer manages the origin side (origin ['top_mod'] or durable
    admin) — the gate for the remove controls, for withdraw controls on requests
    the viewer did not send, and for the connections-management cross-link. *)

val view_candidates : share_view -> candidate list
(** Communities a fresh request could go to right now: connected to the origin
    by an accepted mutual connection, currently satisfying
    {!Community_connections.connection_eligible}, not the origin itself, and
    holding no active (pending or accepted) placement of this post. Rejected,
    removed, and withdrawn history does not exclude a community. One bounded
    query, ordered by lower-cased name then id; at most
    {!max_candidate_destinations} rows. The store still revalidates all of it on
    POST — this list decides nothing. *)

val view_placements : share_view -> active_placement list
(** This thread's pending and accepted placements, oldest first, at most
    {!max_active_placements} rows. Terminal history is deliberately absent. *)

type error =
  | Invalid_user_id
  | Invalid_post_id
  | Inconsistent_data
  | Storage_error

val max_candidate_destinations : int
(** The hard bound on {!view_candidates}. No total count is computed, so the
    number of communities excluded by eligibility, connection state, or an
    active placement cannot be inferred from the page. *)

val max_active_placements : int
(** The hard bound on {!view_placements}. *)

val viewer_may_share :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_global_admin:bool ->
  post_id:int ->
  (bool, error) result Lwt.t
(** The cheap entry-point probe for the canonical thread page: whether
    {!load_share_view} would answer [Ok (Some _)] for this viewer — the same SQL
    authorization and the same tombstone rule, without loading candidates or
    placements. [false] for every denial alike, including a missing or
    tombstoned post. The GET route must still call {!load_share_view}: this
    probe renders a link and grants nothing. *)

val resolve_destination :
  (module Caqti_lwt.CONNECTION) ->
  slug:string ->
  (int option, error) result Lwt.t
(** The community id behind one exact posted destination slug, or [Ok None] for
    absence and for a slug outside the addressable shape. Deliberately free of
    eligibility, connection, and placement conditions: the resolved id only
    identifies the record the store then fully revalidates, and the store's
    collapsed errors are what cross back to the caller — this lookup reveals
    nothing an answered request would not. *)

val connected_destinations :
  (module Caqti_lwt.CONNECTION) ->
  origin_community_id:int ->
  (candidate list, error) result Lwt.t
(** The composer-shaped candidate list: communities a brand-new thread in
    [origin_community_id] could be requested into right now — connected to the
    origin by an accepted mutual connection, currently satisfying
    {!Community_connections.connection_eligible}, and not the origin itself. The
    same SQL body, OCaml re-check, ordering, and {!max_candidate_destinations}
    bound as {!view_candidates}, minus the active-placement exclusion: the post
    does not exist yet, so there is nothing to exclude. This list decides
    nothing — the store revalidates everything under its own locks on POST. The
    caller must already have authorized the viewer for the origin community;
    this read adds no authorization of its own, and a non-positive id answers
    [Ok []]. *)

val load_share_view :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_global_admin:bool ->
  post_id:int ->
  (share_view option, error) result Lwt.t
(** The whole Share-thread view for one canonical post, or [Ok None] when the
    caller may not share it — missing post, tombstoned post
    ({!Shared_thread_placements.post_content_tombstoned}), unauthorized viewer,
    author-no-longer-member, banned author, ['mod'], ['legacy_mod'], and an
    unbacked admin claim all collapse into that one answer.

    [session_global_admin] is the caller's already-derived session state; it
    only ever {i enables} the durable [users.is_admin] check and can never
    substitute for it. The caller must still verify that the route's community
    slug equals {!view_origin_community_slug} — the origin is the post's own
    immutable community, never a route or form value. *)
