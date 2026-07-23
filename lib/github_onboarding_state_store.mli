(** Issuance and persistence of GitHub onboarding callback states, plus
    mid-flow attachment of the untrusted installation id from the GitHub App
    setup return (lookup/consumption is a separate slice). This feature
    module owns its SQL; nothing here belongs
    to the legacy [Db] macro-module. The database only ever receives the
    deterministic lookup hashes: the raw state lives solely in the returned
    abstract value, and the raw session binding never reaches this module at
    all, so a database leak cannot be replayed against the callback. The
    error variant is closed and payload-free — Caqti/PostgreSQL details
    (which can echo SQL parameters) are deliberately dropped, never returned
    or logged. *)

type issue_error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Storage_error
      (** Any database failure, including the operationally negligible
          unique collision between independently generated 256-bit states. *)

val ttl_seconds : int
(** Lifetime of an issued state: 900 seconds (15 minutes). The persisted
    [expires_at] is computed by Postgres from this constant ([NOW() + ttl]),
    never from the application process clock. *)

val issue :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  session_binding_hash:Github_onboarding_crypto.session_binding_hash ->
  flow:Github_onboarding.flow ->
  (Github_onboarding_crypto.state, issue_error) result Lwt.t
(** Generates a fresh state and inserts one [github_onboarding_states] row:
    the state's hash, [user_id], the supplied session-binding hash, the
    canonical flow string, and a Postgres-computed [expires_at];
    [pending_github_installation_id] and [consumed_at] start NULL. The raw
    state is returned only after the insert succeeds. Earlier unconsumed
    states for the same user/session/flow are left untouched — each open tab
    may hold its own individually single-use state. *)

type attach_error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_pending_installation_id
      (** [pending_github_installation_id <= 0]; rejected before any SQL
          runs. GitHub installation ids are positive [BIGINT]s. *)
  | State_unavailable
      (** No attachable row matched. Deliberately collapses every zero-row
          cause — state unknown, expired, already consumed, user mismatch,
          session-binding mismatch, flow mismatch, or a {e different}
          installation id already attached — so callers (and attackers
          driving the setup return) cannot use the store as a state-probing
          oracle. *)
  | Storage_error
      (** Any actual Caqti/PostgreSQL execution failure. Raw database errors
          are dropped, never returned or logged: they can echo SQL
          parameters (the hashes). *)

val attach_pending_installation :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  state:Github_onboarding_crypto.state ->
  session_binding_hash:Github_onboarding_crypto.session_binding_hash ->
  flow:Github_onboarding.flow ->
  pending_github_installation_id:int64 ->
  (unit, attach_error) result Lwt.t
(** Atomically attaches the untrusted installation id echoed back by the
    GitHub App setup return to the caller's still-live state row, in one
    UPDATE — no read-then-write. The row must match the supplied state's
    hash, [user_id], session-binding hash, and canonical flow string, be
    unconsumed and unexpired, and have either no pending installation id yet
    or exactly the supplied one (making an identical retry — e.g. a browser
    refresh — idempotent [Ok ()], while a different id never overwrites the
    first). Only [pending_github_installation_id] ever changes: the state is
    {e not} consumed here, no rows are created or deleted, and other states
    are untouched. Only hashes cross the SQL boundary — the raw state and
    raw session binding never do. *)

type consumed_state = {
  user_id : int;
  flow : Github_onboarding.flow;
  pending_github_installation_id : int64;
}
(** What a successful consumption yields, read back from the locked database
    row itself — never reconstructed from caller input. *)

type consume_error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | State_not_found
      (** No row carries the supplied state's hash. Nothing is created or
          mutated. *)
  | State_expired
      (** The row exists but [expires_at <= NOW()] by the database clock.
          Left untouched ([consumed_at] stays NULL) for cleanup/audit. *)
  | State_already_consumed
      (** The row was consumed earlier; its original [consumed_at] is
          preserved. Reported even when the row is also expired. *)
  | User_mismatch
      (** The state belongs to a different user. The state is burned. *)
  | Session_binding_mismatch
      (** The stored session-binding hash differs from the supplied one. The
          state is burned. *)
  | Flow_mismatch
      (** The stored flow differs from the requested one. The state is
          burned. Structurally present, but unreachable while the domain and
          schema CHECK admit only [project_onboarding]. *)
  | Missing_pending_installation
      (** The row is otherwise valid but no installation id was ever
          attached. The state is burned. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure, or corrupt stored data (an
          unparseable flow despite the CHECK, a non-positive pending id).
          Raw database errors are dropped, never returned or logged: they
          can echo SQL parameters (the hashes). *)

val consume :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  state:Github_onboarding_crypto.state ->
  session_binding_hash:Github_onboarding_crypto.session_binding_hash ->
  flow:Github_onboarding.flow ->
  (consumed_state, consume_error) result Lwt.t
(** Atomically consumes a state exactly once. Runs a transaction on the
    supplied connection that locks the row by state hash ([SELECT … FOR
    UPDATE]; [state_hash] is UNIQUE) and classifies under the lock, in
    order: already consumed, expired, user mismatch, session-binding
    mismatch, flow mismatch, missing pending installation, valid. Expiry and
    prior consumption are judged by the database clock, and dead states
    (missing, expired, replayed) are never written to.

    Burn-on-mismatch: the four mismatch errors set [consumed_at = NOW()] on
    the locked row and commit {e before} the error is returned — presenting
    a real state from the wrong user/session/context destroys it and forces
    onboarding to restart. Only [consumed_at] ever changes, for burns and
    valid consumption alike; identity and lifecycle columns are untouched.
    If the burn or its commit fails, the result is [Storage_error], not the
    mismatch.

    On success the returned record carries the locked row's stored user id,
    parsed flow, and positive pending installation id, and the state is
    permanently unavailable to every later consume or attach attempt. Only
    hashes cross the SQL boundary — the raw state and raw session binding
    never do. *)
