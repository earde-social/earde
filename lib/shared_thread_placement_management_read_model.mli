(** The authorized community-level "Shared threads" management view, plus the
    placement subject binding and the withdrawal grant the mutation handlers
    need. Read-only: this module owns its SQL, takes no locks, writes nothing,
    performs no network IO, and logs nothing.

    {b Authorization lives in the SQL here}, exactly as the sibling
    community-connections management read model does: the management view loads
    only for a current ['top_mod'] row of the named community, or for a caller
    the session reports as a global administrator {i whose} [users.is_admin]
    flag is also still set durably — a session claim alone never suffices.
    Ordinary ['mod'] and ['legacy_mod'] rows, members, non-members and
    moderators of other communities all produce [Ok None], the same answer as a
    community that does not exist, so this module cannot become an existence
    oracle.

    The management view deliberately loads for an {i ineligible} community too.
    A private, draft or undiscoverable community keeps its queues, can still
    reject a pending incoming request, withdraw its own, and remove an accepted
    placement; only accepting a new placement is closed to it, and
    {!view_community_eligible} is how the page knows which.

    Privacy: [request_note] is private workflow text and crosses only into this
    authorized view — the destination's managers read incoming notes, the
    origin's managers read outgoing ones, and it never reaches a public surface.
    No requester, reviewer, or remover identity is read or returned anywhere,
    matching the connections precedent that keeps actor identity off management
    pages. Errors are payload-free; Caqti/PostgreSQL details are dropped, never
    returned or logged.

    Every queue is bounded by {!max_rows_per_queue} with a deterministic order;
    there is deliberately no pagination in this slice — the sibling settings
    queues have none to reuse — and no terminal (rejected, withdrawn, removed)
    history. *)

type counterpart
(** The other community of a placement, as public identity only. *)

val counterpart_name : counterpart -> string
val counterpart_slug : counterpart -> string

type section_option
(** One of this community's own forum sections, offered to the accept forms.
    Loaded once for the whole page, never per row. *)

val section_option_id : section_option -> int
val section_option_name : section_option -> string

type pending_row
(** One pending request, incoming (counterpart = origin) or outgoing
    (counterpart = destination). *)

val pending_placement_id : pending_row -> int64
(** An opaque record locator for an action path — never a grant of authority:
    the handler re-resolves and re-authorizes it server-side. *)

val pending_post_id : pending_row -> int
val pending_post_title : pending_row -> string
val pending_counterpart : pending_row -> counterpart
val pending_note : pending_row -> string option
val pending_requested_at : pending_row -> string

type accepted_row
(** One accepted placement, shared into this community (counterpart = origin) or
    shared from it (counterpart = destination). *)

val accepted_placement_id : accepted_row -> int64
val accepted_post_id : accepted_row -> int
val accepted_post_title : accepted_row -> string
val accepted_counterpart : accepted_row -> counterpart

val accepted_section_name : accepted_row -> string option
(** The destination forum section the placement was accepted into; [None] for a
    flat acceptance and after the section was deleted. *)

val accepted_at : accepted_row -> string

type view

val view_community_id : view -> int
val view_community_name : view -> string
val view_community_slug : view -> string

val view_community_eligible : view -> bool
(** {!Community_connections.connection_eligible} for this community right now —
    while [false], accepting new placements is closed; everything else on the
    page stays available. *)

val view_sections_enabled : view -> bool

val view_section_options : view -> section_option list
(** This community's own sections, position order — empty when sections are
    disabled. *)

val view_incoming : view -> pending_row list
(** Pending requests addressed to this community, oldest first. *)

val view_outgoing : view -> pending_row list
(** Pending requests sent from this community's threads, oldest first. *)

val view_shared_into : view -> accepted_row list
(** Accepted placements of other communities' threads into this one, newest
    acceptance first. *)

val view_shared_from : view -> accepted_row list
(** Accepted placements of this community's threads elsewhere, newest acceptance
    first. *)

type subjects
(** One placement's durable subjects, loaded for route binding: the route
    community must match the side the mutation requires before the store is
    opened. Carries no note and no post title. *)

val subjects_post_id : subjects -> int
val subjects_origin_community_id : subjects -> int
val subjects_destination_community_id : subjects -> int

type withdrawal_grant
(** Proof that a withdrawal caller passed the withdrawal gate — the original
    requester, an origin ['top_mod'], or a durable admin — bound to the route
    community as the placement's origin. *)

val grant_community_id : withdrawal_grant -> int
val grant_community_slug : withdrawal_grant -> string
val grant_post_id : withdrawal_grant -> int

type error =
  | Invalid_user_id
  | Invalid_community_slug
  | Invalid_placement_id
  | Inconsistent_data
  | Storage_error

val max_rows_per_queue : int
(** The hard bound on each of the four queues. *)

val load_for_manager :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_global_admin:bool ->
  community_slug:string ->
  (view option, error) result Lwt.t
(** The whole management view for one community, or [Ok None] when the caller
    may not manage it — missing community, ordinary member, non-member, ['mod'],
    ['legacy_mod'], moderator of another community, and an unbacked admin claim
    all collapse into that one answer. *)

val load_placement_subjects :
  (module Caqti_lwt.CONNECTION) ->
  placement_id:int64 ->
  (subjects option, error) result Lwt.t
(** The placement's subjects in any status, or [Ok None] for absence. The caller
    decides nothing from this beyond route binding; a stale status is the
    store's to refuse. *)

val authorize_withdrawal :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_global_admin:bool ->
  community_slug:string ->
  placement_id:int64 ->
  (withdrawal_grant option, error) result Lwt.t
(** The withdrawal gate, decided in one SQL statement: the named community must
    exist and be the placement's origin, and the caller must be the placement's
    original requester, a current origin ['top_mod'], or a durable admin.
    Everything else — missing community, missing placement, another community's
    placement, an unauthorized caller, an unbacked admin claim — collapses into
    [Ok None]. Deliberately status-blind and free of eligibility, connection,
    membership, and tombstone conditions: withdrawal is cleanup and must survive
    all of those changing, and a stale status is the store's to refuse. *)
