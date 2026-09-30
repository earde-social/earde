(** Transaction-local, durable in-app notification insertion for the
    project/community-home lifecycle.

    One notifications row per (recipient, kind, home relation): a closed kind,
    the recipient, the acting user, and the exact durable subject rows (project,
    community, home relation) as internal foreign keys — nothing else. No
    request note, decision text, authorization provenance, GitHub identifiers,
    tokens, OAuth/PKCE material, emails, usernames, or browser-supplied URLs are
    ever written; rendering derives entirely from current entity state at read
    time.

    Everything here is deliberately transaction-local: it runs on the caller's
    open connection, after the enclosing store's business mutation has succeeded
    and its audit event has been inserted, and before that store commits. This
    module never opens, commits, or rolls back a transaction itself — the
    enclosing store owns the boundary, so a failed notification insertion rolls
    the whole business mutation (and its audit event) back, and a rolled-back
    mutation leaves no notification behind.

    Error privacy: both errors are payload-free and no printer or serializer
    exists. The internal row ids stay between the stores and this module; they
    never reach handler or page APIs. *)

type kind = Home_requested | Home_accepted | Home_rejected | Home_removed
type error = Inconsistent_data | Storage_error

val community_top_moderator_ids :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  (int list, error) result Lwt.t
(** All current ['top_mod'] users of one community, resolved from durable rows
    on the caller's open transaction — never from handler or form input. Plain
    reads, no row locks: the enclosing store has already locked the community
    row, and these reads must never extend the shared lock protocol. Callers
    must resolve recipients only after their business locks are held and the
    transition has validated. A non-positive id in the result set is
    [Inconsistent_data]. *)

val project_steward_ids :
  (module Caqti_lwt.CONNECTION) ->
  project_id:int64 ->
  (int list, error) result Lwt.t
(** All current stewards of one project, under the same contract as
    {!community_top_moderator_ids}. *)

val insert_many :
  (module Caqti_lwt.CONNECTION) ->
  kind:kind ->
  actor_user_id:int ->
  project_id:int64 ->
  community_id:int ->
  relation_id:int64 ->
  recipient_user_ids:int list ->
  (unit, error) result Lwt.t
(** Inserts exactly one durable notification per distinct recipient on the
    caller's open transaction.

    Recipient ids are deduplicated internally and the actor is always excluded,
    so a mutation can never notify its own actor no matter what the caller
    resolved. An empty (or actor-only) recipient set is a successful no-op: zero
    recipients never fails a business transition. Every id must already be
    validated and locked by the enclosing store; a non-positive id here —
    including any recipient — is [Inconsistent_data], because it can only mean
    the caller skipped its own validation or resolution. Each inserted row is
    revalidated (positive id, byte-identical kind, exact recipient) before
    success. Any database failure — including a duplicate refused by the
    per-(recipient, kind, relation) unique index, which is impossible under the
    locked protocol — is [Storage_error]. On either error the caller must roll
    back; this module never commits on its behalf. *)
