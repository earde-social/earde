(** Transaction-local, append-only audit recording for the
    GitHub-project/community-home lifecycle.

    One durable row per successfully committed lifecycle transition, in
    the [project_home_audit_events] table: a closed action, the acting
    user, and the exact durable subject rows (project, community, home
    relation) as internal foreign keys plus a database timestamp —
    nothing else. No request note, submitted text, GitHub identifiers,
    tokens, OAuth/PKCE material, session values, authorization
    provenance, or diagnostics are ever written.

    {!insert} is deliberately transaction-local: it runs on the caller's
    open connection, after the enclosing store's business mutation has
    succeeded and returned validated ids, and before that store commits.
    It never opens, commits, or rolls back a transaction itself — the
    enclosing store owns the boundary, so a failed audit insertion rolls
    the whole business mutation back and a rolled-back mutation leaves no
    event behind.

    Error privacy: both errors are payload-free and no printer or
    serializer exists. The internal row ids stay between the stores and
    this module; they never reach handler or page APIs. *)

type action =
  | Home_requested
  | Home_accepted
  | Home_rejected
  | Home_removed
  | Dedicated_home_provisioned
  | Network_community_published

type error =
  | Inconsistent_data
  | Storage_error

val insert :
  (module Caqti_lwt.CONNECTION) ->
  action:action ->
  actor_user_id:int ->
  project_id:int64 ->
  community_id:int ->
  relation_id:int64 ->
  (unit, error) result Lwt.t
(** Appends exactly one audit event on the caller's open transaction.

    Every id must already be validated and locked by the enclosing
    store; a non-positive id here is [Inconsistent_data] — it can only
    mean the caller skipped its own validation. The returned row is
    revalidated (positive id, byte-identical action) before success.
    Any database failure — including a foreign key refused because a
    subject row vanished outside the locked protocol — is
    [Storage_error]. On either error the caller must roll back; this
    module never commits on its behalf. *)
