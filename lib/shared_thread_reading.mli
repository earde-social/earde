(** The shared-thread read side of the canonical thread route and the comment
    write path. Read-only: owns its SQL, takes no locks, writes nothing,
    performs no network IO, logs nothing; errors are payload-free.

    A canonical post can be read in exactly three context kinds, and this module
    is how the route tells them apart:

    - {b origin context}: the route slug equals the post's own immutable
      [community_slug] — the existing origin path, untouched by this module;
    - {b accepted destination context}: {!resolve_destination_context} answers
      [Ok (Some _)] and the caller's existing can_view_community decision admits
      the viewer to the destination;
    - {b unavailable}: everything else — no placement, an inactive
      (pending/rejected/removed/withdrawn) placement, a currently private
      origin, and a malformed slug all collapse into the same [Ok None], so none
      of them is observably distinct. *)

type destination_context = {
  destination_community_id : int;
      (** The destination community holding the accepted placement. The caller
          loads the full record by id and applies its ONE existing
          can_view_community rule — viewer access is deliberately not decided
          here, so no third divergent read rule can exist. *)
  destination_section : (string * string) option;
      (** The placement's own destination section as [(name, slug)] — the
          effective local section context. [None] for a sectionless (flat)
          destination and for a section since deleted (ON DELETE SET NULL): both
          render the destination's uncategorized/flat context. *)
  origin_community_name : string;
      (** The canonical origin community's display name, for the public "Shared
          from" provenance label. Only ever returned when the origin is
          currently public, so naming it is safe by construction. *)
}

type error = Storage_error

val resolve_destination_context :
  (module Caqti_lwt.CONNECTION) ->
  post_id:int ->
  destination_slug:string ->
  (destination_context option, error) result Lwt.t
(** Whether [destination_slug] is a currently readable accepted destination of
    the canonical post: an accepted placement bound to exactly that community,
    whose origin community is currently public. Connection state,
    discoverability, and onboarding eligibility are deliberately not consulted —
    they gate request and acceptance, never continued rendering. One bounded
    point query over the active-placement unique index. *)

val public_destinations_for_posts :
  (module Caqti_lwt.CONNECTION) ->
  post_ids:int list ->
  ((int * (string * string)) list, error) result Lwt.t
(** Origin-side provenance for a page of already-selected canonical posts:
    [(post_id, (destination_slug, destination_name))] rows for every placement
    that is CURRENTLY publicly renderable — accepted, origin community currently
    public (the destination-rendering rule), and destination community currently
    public (what an anonymous reader could open right now). Pending, rejected,
    withdrawn, and removed placements and private destinations produce no row;
    request notes and every actor identity never leave the database. One bounded
    query per page (ids CSV-joined — the [get_thread_sources_for_posts] idiom),
    never one per post; non-positive ids are dropped and an empty input answers
    [Ok []]. Deterministic order: post id, lower-cased destination name, then
    the unique slug. Callers enrich existing feed/thread view models AFTER their
    own queries — this read changes no row selection, ordering, or pagination
    anywhere. *)

val viewer_may_comment :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  post_id:int ->
  (bool, error) result Lwt.t
(** The single comment-participation capability, shared in meaning by the
    thread-page composer gate and the POST /comments authorization. [true] iff
    the post exists untombstoned, the user is neither globally banned nor banned
    in the canonical origin community, and the user currently holds at least one
    participation path: origin membership, or membership in an accepted
    destination whose placement is currently readable (origin public) and where
    they are not banned. Anonymous callers ([user_id <= 0]) and missing posts
    are [false], never errors. One bounded query — never one per placement. *)
