(** Transactional persistence of a user-owned project-onboarding draft and
    its complete verified public-repository snapshot. This feature module
    owns its SQL.

    The only GitHub-derived inputs are two abstract proof values — the
    verified installation identity from {!Github_user_installations} and the
    validated public repository set from
    {!Github_user_installation_repositories} — whose constructors already
    validated every field, so nothing is trimmed, normalized, or re-checked
    here. No token of any kind (access, refresh, installation), no
    authorization code, PKCE verifier, client secret, App private key, OAuth
    state, session binding, or private-repository metadata can reach this
    module, and the error variant is closed and payload-free —
    Caqti/PostgreSQL details (which can echo SQL parameters) are
    deliberately dropped, never returned or logged. *)

type draft
(** A successfully created or refreshed draft. Deliberately abstract, with
    no serializer or printer; the representation holds only the local
    [project_onboarding_drafts] primary key. The id is not a bearer
    credential — every future handler must authorize it against the current
    Earde user before acting on it. *)

val draft_id : draft -> int64
(** The local draft row id, for owner-authorized follow-up operations. *)

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Installation_unavailable
      (** No local [github_installations] row simultaneously matches the
          verified installation id, account id, and account type while
          being [active] with [revoked_at IS NULL]. Deliberately collapses
          every cause — missing, inaccessible, revoked, account-id
          mismatch, account-type mismatch — into one payload-free result,
          so callers cannot use the store to probe installation state. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure, at any step. The transaction is
          rolled back, leaving any previous draft and snapshot unchanged.
          Raw database errors are dropped, never returned or logged: they
          can echo SQL parameters (repository metadata). *)

val refresh_verified :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  installation:Github_user_installations.verified_installation ->
  repositories:Github_user_installation_repositories.repository_set ->
  (draft, error) result Lwt.t
(** Creates or refreshes the caller's active draft for the verified
    installation and replaces its complete repository snapshot, in one
    explicit transaction on the supplied connection:

    + the authoritative local [github_installations] row is located by the
      verified installation id, account id, and canonical account type,
      requiring [status = 'active'] and [revoked_at IS NULL] — never by
      account login (renamable) and never via [connected_by_user_id]
      (provenance, not ownership); the installation row is never modified;
    + one atomic insert-or-refresh (arbitrated by the partial unique index
      on active drafts — no read-before-insert) yields the active draft:
      a fresh insert, or an in-place refresh of the existing active row —
      including an expired-but-still-active one — that preserves [id],
      [created_at], and the NULL lifecycle timestamps while renewing
      [expires_at] and [updated_at]. Terminal ([completed]/[cancelled])
      drafts are never touched; when only those exist a new active draft
      is inserted;
    + the previous snapshot rows are deleted and the supplied set is
      inserted completely, in its existing list order, with positions
      contiguous from 1 through one prepared statement executed per row —
      metadata byte-for-byte, [is_selected] and [is_primary] reset to
      false (a refreshed verification is a new authoritative snapshot, so
      no previous selection state survives).

    + before commit, the caller's own [project_stewards] rows are renewed
      ([github_verified_at], and the proving installation record) for
      every project in the verified account whose unreleased repositories
      all appear in the new snapshot. Other users' rows are never written.
      The installation row ([FOR KEY SHARE]) and then those projects'
      rows ([FOR UPDATE], ascending id) are locked first and the renewal
      decided in a later statement, so it serializes with a concurrent
      claim release in [Project_finalization_store.finalize]: renewal
      never restores fresh evidence over claims a committed release took.

    [expires_at] is always the database transaction's [NOW() + 24 hours],
    and [verified_at] its [NOW()] — never the application clock, never
    caller-supplied.

    On any failure the whole transaction rolls back — the previous draft
    and snapshot remain unchanged and no partial replacement ever becomes
    visible. Concurrent calls for the same user and installation serialize
    on the draft row lock taken by the insert-or-refresh, so both may
    succeed with the same draft id and the surviving snapshot is one
    complete input set, never a mixture. [Lwt.Canceled] is never
    swallowed. This module performs no network IO and no logging. *)
