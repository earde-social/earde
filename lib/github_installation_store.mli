(** Persistence of a successfully verified GitHub App installation into
    [github_installations]. This feature module owns its SQL.

    The only GitHub-derived input is the abstract
    {!Github_user_installations.verified_installation} — proof that the
    installation was seen in an authenticated listing — whose constructor
    already validated the identity fields (positive ids, non-empty safe login,
    closed account type), so this module stores them exactly as verified,
    without trimming, normalizing, or repairing. No token, authorization code,
    verifier, or secret of any kind is accepted, read, or stored. The error
    variant is closed and payload-free — Caqti/PostgreSQL details (which can
    echo SQL parameters) are deliberately dropped, never returned or logged. *)

type error =
  | Invalid_connected_by_user_id
      (** [connected_by_user_id <= 0]; rejected before any SQL runs. *)
  | Installation_unavailable
      (** The installation id already exists but the row could not be
          re-activated: it is terminally revoked, or it carries a different
          GitHub account id or account type. Deliberately one payload-free
          constructor for both causes, and the existing row is untouched. A
          genuine reinstall arrives with a new GitHub installation id and
          creates a new row. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure, including the foreign-key failure for a
          positive but nonexistent Earde user. Raw database errors are dropped,
          never returned or logged. *)

val record_verified :
  (module Caqti_lwt.CONNECTION) ->
  connected_by_user_id:int ->
  Github_user_installations.verified_installation ->
  (unit, error) result Lwt.t
(** Records the verified installation in one atomic upsert — no
    read-before-write. A fresh installation id inserts an [active] row carrying
    the verified identity and [connected_by_user_id]; timestamps and
    [revoked_at] come from database defaults.

    When a row already exists for the same installation id with the same GitHub
    account id and account type (the account id, not the mutable login, is the
    identity) and is not revoked, the call is idempotent [Ok ()]: it refreshes
    [github_account_login] to the newly verified exact value, sets [status] back
    to [active] (recovering [inaccessible]), and touches [updated_at] — nothing
    else. In particular the original [connected_by_user_id] is preserved, even
    when a different Earde user reconnects and even when the column is already
    NULL because the original user was deleted: the installation belongs to the
    GitHub account, not to the caller.

    A revoked row is terminal and a row with a different account identity is
    never modified; both return [Error Installation_unavailable] with the
    existing row byte-for-byte intact.

    The UNIQUE constraint on the installation id plus the single-statement
    upsert make concurrent writes safe without application locks: identical
    concurrent writes both succeed and leave exactly one row, and conflicting
    identities for one installation id can never both persist or interleave. *)
