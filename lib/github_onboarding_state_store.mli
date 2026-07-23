(** Issuance and persistence of GitHub onboarding callback states — the
    issuing half of the state lifecycle only (lookup/consumption is a
    separate slice). This feature module owns its SQL; nothing here belongs
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
