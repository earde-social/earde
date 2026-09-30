(** Atomic finalization of one available project-onboarding draft into a
    permanent verified open-source project. This feature module owns its
    SQL.

    In one explicit PostgreSQL transaction the store authorizes and locks
    the caller's draft, locks and validates its verified installation,
    locks and revalidates the complete repository snapshot, and — using
    the currently selected snapshot rows as the authoritative repository
    set — creates the [open_source_projects] row, the initial
    [project_stewards] row, and the [project_repositories] copies, then
    marks the source draft [completed]. Any failure rolls back every
    change: the draft, its snapshot, and its saved selection remain
    exactly as they were, and no permanent row survives.

    Concurrency is arbitrated by the database, never by application
    mutexes: competing finalizations of the same draft serialize on the
    draft row lock (the same draft-first lock order as
    [Project_onboarding_draft_store.refresh_verified] and
    [Project_onboarding_draft_selection_store.replace], so no reversed-
    order deadlock is possible), while slug and global repository claims
    are arbitrated by the unique constraints via conflict-tolerant
    inserts — never by read-before-insert checks.

    No token of any kind, authorization code, PKCE verifier, client
    secret, OAuth state, session binding, or private-repository metadata
    can reach this module, and the error variant is closed and
    payload-free — Caqti/PostgreSQL details (which can echo SQL
    parameters) are deliberately dropped, never returned or logged. This
    module performs no network IO and no logging. *)

type created_project
(** The finalized project: only its permanent local id and canonical
    persisted slug. Deliberately abstract with no serializer or printer;
    no draft, installation, snapshot, user, or GitHub identifier travels
    out. The id is a database identifier, not a bearer credential. *)

val project_id : created_project -> int64
val project_slug : created_project -> string

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_draft_id  (** [draft_id <= 0]; rejected before any SQL runs. *)
  | Draft_unavailable
      (** No available draft is owned by the caller under that id: the
          draft is missing, another user's, expired, completed (including
          a replay after a successful finalization), or cancelled, or its
          installation is missing, inaccessible, or revoked. Deliberately
          collapses every cause into one payload-free result, so the
          store cannot be used to probe drafts or installations. *)
  | No_repositories_selected
      (** The draft is available and its snapshot valid, but no snapshot
          row is currently selected — the saved selection was cleared (or
          reset by a snapshot refresh) after the identity value was
          constructed. Nothing was created. *)
  | Selection_stale
      (** The identity's primary snapshot id does not name a currently
          selected row of this draft's snapshot — deleted by a refresh,
          unselected by a later replacement, foreign, or arbitrary; which
          is never revealed. Nothing was created. *)
  | Kind_namespace_mismatch
      (** The identity kind is [Organization] but the verified GitHub
          namespace is a personal account. Every other kind is accepted
          under either namespace type. Nothing was created. *)
  | Slug_unavailable
      (** The canonical slug is already taken by an existing project. The
          unique constraint arbitrates, so a concurrent finalization
          racing the same slug loses here deterministically. The whole
          transaction is rolled back. *)
  | Repository_already_connected
      (** At least one selected GitHub repository is already claimed by
          an existing permanent project that still has a steward with
          fresh GitHub evidence — which one is never revealed. (A claim
          without any fresh steward is released first; see
          docs/features/github-verification-lifecycle.md.) The unique
          index on unreleased claims arbitrates concurrent claims. The
          whole transaction is rolled back. *)
  | Inconsistent_data
      (** Durable state violates an invariant the write side promises:
          malformed installation identity, a snapshot failing structural
          validation (count outside 1..2000, non-contiguous positions,
          duplicate or non-positive ids, owner not the verified account,
          non-canonical names or URLs, forbidden bytes, unselected or
          plural primaries), a kind-[Project] identity without a primary,
          an active draft already referenced by a permanent project, or
          an expected single-row update that did not hit exactly one row.
          The transaction is rolled back; nothing partial survives. *)
  | Storage_error
      (** Any other Caqti/PostgreSQL failure, at any step. The
          transaction is rolled back. Raw database errors are dropped,
          never returned or logged. *)

val finalize :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  draft_id:int64 ->
  identity:Project_identity.t ->
  (created_project, error) result Lwt.t
(** Finalizes the caller's available draft under [identity], in one
    explicit transaction on the supplied connection:

    + the draft row is authorized and locked ([FOR UPDATE], draft row
      first — the shared lock order) in one statement requiring the
      supplied id AND owner together with availability —
      [status = 'active'] and [expires_at > NOW()]; ownership is
      [user_id] only, never [connected_by_user_id] (provenance, not
      ownership);
    + the draft's exact installation row is then locked and required to
      be [active] with [revoked_at IS NULL]; its durable identity
      (positive ids, structurally valid login, closed account type) is
      revalidated, and kind [Organization] additionally requires an
      organization namespace;
    + the complete snapshot is loaded and locked under the draft lock,
      ordered by position, and structurally revalidated as a whole before
      any permanent row is created;
    + the currently selected rows are the authoritative repository set —
      the identity deliberately retains no selected-id list, and a
      selection changed since identity construction is used as-is; an
      empty selection is [No_repositories_selected]. The identity's
      primary (required for kind [Project]) must name a currently
      selected row, otherwise [Selection_stale];
    + after verifying no permanent project already references this still-
      active draft, the project row is inserted with the identity fields,
      [forge = 'github'], the locked installation's account id/login/type
      as the namespace, [verification_status = 'verified'], and the
      caller as [created_by_user_id]; a slug conflict (arbitrated by the
      unique constraint, [ON CONFLICT DO NOTHING]) is [Slug_unavailable];
    + exactly one [project_stewards] row is inserted for the caller with
      the locked installation record id as its authorization proof and
      the draft's [verified_at] as its evidence time;
    + the selected rows are copied into [project_repositories] in their
      snapshot order, renumbered contiguously from 1, carrying only
      repository identity and display metadata (no owner ids, logins,
      selection flags, or local snapshot ids); exactly the row named by
      the identity's primary is copied primary. Before each copy, if the
      repository is claimed by a project with no steward holding fresh
      evidence, all of that project's claims are released; the projects
      holding any selected repository are first locked ([FOR UPDATE],
      ascending id), so a concurrent steward renewal either commits first
      and keeps the claims or finds them released. A remaining claim conflict
      (arbitrated by the unique index on unreleased claims) is
      [Repository_already_connected];
    + the locked draft becomes [status = 'completed'] with
      [completed_at] and [updated_at] set to
      [GREATEST(NOW(), created_at)] — clamped because the transaction's
      [NOW()] can predate a draft row refreshed by a concurrent
      transaction this one waited on. [user_id],
      [github_installation_record_id], [expires_at], and [created_at]
      never move, and the snapshot rows remain stored under the
      completed draft.

    Commit happens only after every step; on any error the whole
    transaction rolls back and the active draft, snapshot, and selection
    are preserved untouched. A replay after successful completion gets
    [Draft_unavailable] — finalization is deliberately not idempotent by
    draft id. [Lwt.Canceled] is never swallowed. *)
