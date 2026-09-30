(** The authorized community-connections management view, and the target search
    behind "Connect a community". Read-only: this module owns its SQL, takes no
    locks, writes nothing, performs no network IO, and logs nothing.

    {b Authorization lives in the SQL here}, exactly as the sibling project-home
    review read model does: the view loads only for a current ['top_mod'] row of
    the named community, or for a caller the session reports as a global
    administrator {i whose} [users.is_admin] flag is also still set durably — a
    session claim alone never suffices. Ordinary ['mod'] and ['legacy_mod']
    rows, members, non-members and moderators of other communities all produce
    [Ok None], the same answer as a community that does not exist, so this
    module cannot become an existence oracle.

    The management view deliberately loads for an {i ineligible} community too.
    A private, draft or undiscoverable community keeps its history, can still
    reject a pending request and can still remove an accepted connection; only
    creating new connections is closed to it, and {!view_community_eligible} is
    how the page knows which.

    Privacy: [request_note] is private workflow text between the two moderator
    teams and crosses only into this authorized view — never onto a public
    surface. No requester, reviewer, or remover user id or username is read or
    returned anywhere: the accepted state is symmetric and has no author. Errors
    are payload-free; Caqti/PostgreSQL details are dropped, never returned or
    logged. *)

type counterpart
(** The other community in a connection, as public identity only. *)

val counterpart_id : counterpart -> int
val counterpart_name : counterpart -> string
val counterpart_slug : counterpart -> string

type entry
(** One connection as this community sees it: its row id and the other side,
    plus the private note for the two pending shapes. *)

val entry_id : entry -> int64
val entry_counterpart : entry -> counterpart

val entry_note : entry -> string option
(** Always [None] for an accepted connection: the note belongs to the request
    workflow, and an accepted connection is symmetric with no author. *)

type view

val view_community_id : view -> int
val view_community_name : view -> string
val view_community_slug : view -> string

val view_community_eligible : view -> bool
(** {!Community_connections.connection_eligible} for this community right now —
    whether it may create or accept connections at all. *)

val view_accepted : view -> entry list
(** Accepted connections, from either side, ordered by counterpart name. *)

val view_incoming : view -> entry list
(** Pending requests addressed to this community, oldest first. *)

val view_outgoing : view -> entry list
(** Pending requests this community sent, oldest first. *)

type target
(** An eligible, connectable community offered by {!search_targets}. *)

val target_id : target -> int
val target_name : target -> string
val target_slug : target -> string

(** What an exact target slug resolves to for one searching community. The three
    cases are deliberately unequal in what they may reveal: [Already_active]
    names a state the asking community can already see on its own management
    page, while [Unavailable] collapses missing, private, draft, undiscoverable,
    and itself into one answer. *)
type resolution = Connectable of target | Already_active | Unavailable

type error =
  | Invalid_user_id
  | Invalid_community_id
  | Invalid_community_slug
  | Inconsistent_data
  | Storage_error

val max_search_results : int
(** The hard bound on {!search_targets}. No total count is ever computed, so the
    number of communities excluded by eligibility or by an existing connection
    cannot be inferred from the page. *)

val load_for_manager :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_global_admin:bool ->
  community_slug:string ->
  (view option, error) result Lwt.t
(** The whole management view for one community, or [Ok None] when the caller
    may not manage it — missing community, ordinary member, non-member, ['mod'],
    ['legacy_mod'], moderator of another community, and an unbacked admin claim
    all collapse into that one answer.

    [session_global_admin] is the caller's already-derived session state; it
    only ever {i enables} the durable [users.is_admin] check and can never
    substitute for it. A [community_slug] outside the addressable shape (one
    non-empty URL path segment) is [Invalid_community_slug], rejected before any
    SQL; nothing is trimmed, lowercased, or repaired.

    Every returned row is validated: positive ids, a coherent counterpart
    identity, a closed status, and a canonical note. Durable incoherence is
    [Inconsistent_data] for the whole call — never a partial list. *)

val search_targets :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  query:string ->
  (target list, error) result Lwt.t
(** Up to {!max_search_results} communities this community could send a request
    to, matched on name or slug.

    Excluded: the community itself; every community failing
    {!Community_connections.connection_eligible} (checked both in the SQL
    predicate, so the bound applies after exclusion, and again in OCaml through
    the pure predicate, which stays the authority); and every community already
    sharing a pending or accepted connection with this one, in either direction.
    Rejected and removed history does not exclude a community — a fresh request
    is legitimate.

    The query is matched case-insensitively as a substring. [%], [_] and
    backslash in the caller's text are escaped, so no user input can widen the
    pattern. A blank query, or one longer than a slug or name can be, returns
    the empty list without touching the database — the same answer as a query
    that matches nothing, so absence is never distinguishable from
    ineligibility. The caller must still authorize the surface; this function
    decides nothing about who may search. *)

val resolve_target :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  slug:string ->
  (resolution, error) result Lwt.t
(** Resolves one exact target slug for [community_id] — what the confirmation
    step and the request submission each need, rather than re-running a
    substring search over a value that is already exact.

    [Connectable] means the community exists, satisfies
    {!Community_connections.connection_eligible} (the same pure predicate, the
    same authority), is not [community_id] itself, and shares no pending or
    accepted connection with it. [Already_active] means it is otherwise
    connectable but the pair already has a live request or connection — a fact
    the asking community can already read off its own management page, so saying
    so reveals nothing new. [Unavailable] collapses everything else alike: no
    such community, a private, draft or undiscoverable one, and [community_id]
    itself. A slug outside the addressable shape is [Unavailable] too, before
    any SQL. *)
