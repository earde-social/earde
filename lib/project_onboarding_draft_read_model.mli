(** Owner-authorized read model over a user's project-onboarding drafts and
    their verified public-repository snapshots — the data that will later power
    [GET /projects/new]. Read-only: this feature module owns its SQL, performs
    no network IO, no logging, and never writes.

    Availability is decided entirely in SQL: a draft is usable only while it is
    [active] and unexpired ([expires_at > NOW()]) and its backing
    [github_installations] row is [active] with [revoked_at IS NULL], and it has
    at least one snapshot row. [connected_by_user_id] is installation provenance
    and is never consulted for filtering, authorization, or ownership — the
    draft's own [user_id] is the only owner.

    Nothing credential-shaped can pass through here: no tokens of any kind,
    authorization codes, PKCE values, client secrets, external installation ids,
    local installation record ids, provenance, timestamps, or private repository
    metadata are exposed, and the closed error variant is payload-free —
    Caqti/PostgreSQL details (which can echo SQL parameters) are deliberately
    dropped, never returned or logged. *)

type draft_summary
(** One usable draft as the chooser list needs it: identity, the display
    account, and snapshot selection totals. Deliberately abstract, with no
    serializer or printer. *)

type draft_view
(** One usable draft in full: its summary plus the complete repository snapshot,
    read in one statement so the two can never disagree. *)

type repository
(** One validated snapshot row. Deliberately abstract, with no serializer or
    printer; only the accessor fields below are retained. *)

val draft_id : draft_summary -> int64
(** The local draft row id. Not a bearer credential — every state-changing
    operation must re-authorize it against the current Earde user through its
    owning draft, exactly as {!load_available} does. *)

val account_login : draft_summary -> string
(** The CURRENT [github_installations.github_account_login] — display metadata
    that GitHub can rename, never derived from a repository snapshot and never a
    stable identity key. *)

val account_type : draft_summary -> Github_user_installations.account_type
(** Whether the installation targets a user or an organization, parsed strictly
    from the canonical database enum. *)

val repository_count : draft_summary -> int
(** Total snapshot rows; always within [1..2000]. *)

val selected_repository_count : draft_summary -> int
(** Rows currently marked selected; always within [0..repository_count]. *)

val has_primary_repository : draft_summary -> bool
(** Whether exactly one snapshot row is marked primary. A primary always implies
    at least one selected repository. *)

val summary : draft_view -> draft_summary

val repositories : draft_view -> repository list
(** The complete snapshot, ordered by [position ASC] with positions contiguous
    from 1. Never empty. *)

val snapshot_id : repository -> int64
(** The local [project_onboarding_draft_repositories.id] — deliberately exposed
    so future state-changing forms can name exact snapshot rows: a GitHub
    refresh deletes and recreates them, so a stale browser form references ids
    that no longer exist and can be rejected safely. Not a bearer credential —
    it must always be authorized through its owning draft and the current Earde
    user. *)

val position : repository -> int
(** Display order only, contiguous from 1 within one snapshot. Never an
    identifier — use {!snapshot_id}. *)

val github_repository_id : repository -> int64
val github_owner_id : repository -> int64

val owner_login : repository -> string
(** The owner login as snapshotted at verification time. May differ from
    {!account_login}, which is current; identity comparisons must use
    {!github_owner_id}. *)

val name : repository -> string

val full_name : repository -> string
(** Exactly [<owner_login>/<name>]. *)

val html_url : repository -> string
(** Exactly [https://github.com/<owner_login>/<name>], revalidated by structured
    reconstruction before being returned. *)

val description : repository -> string option

val default_branch : repository -> string
(** Snapshotted byte-for-byte; may contain ['/']. Any consumer that embeds it in
    a URL or command must encode it at that boundary. *)

val is_archived : repository -> bool
val is_selected : repository -> bool
val is_primary : repository -> bool

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_draft_id
      (** [draft_id <= 0]; rejected before any SQL runs. Compared as [int64],
          never converted through [int]. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant this module guarantees
          to callers — snapshot counts, contiguous positions, canonical full
          name or URL, byte-validity, selection or primary rules, duplicates, or
          an off-enum account type — including the zero-snapshot corruption of
          an otherwise available owned draft. Payload-free on purpose; nothing
          about the offending row is revealed. Never a partial view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped, never
          returned or logged: they can echo SQL parameters. *)

val list_available :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  (draft_summary list, error) result Lwt.t
(** Every currently usable draft owned by [user_id], ordered by
    [updated_at DESC, id DESC] — newest activity first, id as the deterministic
    tiebreak. No draft is ever chosen automatically: a user may legitimately
    hold one draft per installation (a personal account and several
    organizations), and the caller presents them all. [Ok []] when the user owns
    no usable draft; drafts hidden by any availability rule — expired, terminal,
    or backed by an inaccessible or revoked installation — and installations
    without a draft are simply absent. Each summary's counts are aggregated from
    the snapshot table in the same statement. *)

val load_available :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  draft_id:int64 ->
  (draft_view option, error) result Lwt.t
(** The one usable draft with this id, only when [user_id] owns it. The draft id
    and user id are authorized together inside the SQL — never loaded by id and
    checked afterward — and summary plus snapshot come from one joined
    statement, so a concurrent verified refresh can never interleave between
    them.

    [Ok None] deliberately collapses every unavailable state — nonexistent
    draft, another user's draft, expired, completed, cancelled, and inaccessible
    or revoked installation — so probing draft ids cannot become an ownership or
    state oracle. The single exception: a corrupted zero-snapshot draft that IS
    owned by [user_id] and otherwise available returns [Error Inconsistent_data]
    (never an empty view); any other user still receives [Ok None] for that same
    id. *)
