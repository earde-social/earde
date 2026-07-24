(** Owner-authorized, transactional replacement of the repository selection
    on one available project-onboarding draft. This feature module owns its
    SQL; nothing here belongs to the legacy [Db] macro-module.

    The caller identifies repositories by LOCAL
    [project_onboarding_draft_repositories] row ids — never by GitHub
    repository id — so a GitHub re-verification that replaces the snapshot
    (new rows, new ids) automatically invalidates any stale browser
    submission. No token of any kind, authorization code, PKCE verifier,
    client secret, OAuth state, or private-repository metadata can reach
    this module, and the error variant is closed and payload-free —
    Caqti/PostgreSQL details (which can echo SQL parameters) are
    deliberately dropped, never returned or logged. *)

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_draft_id  (** [draft_id <= 0]; rejected before any SQL runs. *)
  | Invalid_selection
      (** The supplied selection is structurally invalid, before any SQL:
          a non-positive or duplicate selected id, more than 2000 selected
          ids, or a primary id that is non-positive or does not occur
          exactly once in the selected list. An empty selection with no
          primary is valid — project-kind policy (primary required, at
          least one repository) is enforced at project creation, not
          here. *)
  | Draft_unavailable
      (** No available draft is owned by the caller under that id: the
          draft is missing, another user's, expired, completed, or
          cancelled, or its installation is inaccessible or revoked.
          Deliberately collapses every cause into one payload-free result,
          so the store cannot be used to probe drafts or installations. *)
  | Selection_stale
      (** The caller was authorized for the draft, but at least one
          supplied snapshot id does not belong to its current snapshot —
          typically because a GitHub re-verification replaced the rows
          since the page was rendered. Where the id does exist (another
          draft, nowhere) is never revealed. No selection state was
          modified. *)
  | Inconsistent_data
      (** The stored snapshot violates a durable structural invariant
          (row count outside 1..2000, non-positive or duplicate ids,
          non-contiguous positions, more than one primary, an unselected
          primary), or an expected single-row update did not hit exactly
          one row. The transaction is rolled back and nothing partial is
          applied or returned. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure, at any step. The transaction is
          rolled back, leaving the previous selection unchanged. Raw
          database errors are dropped, never returned or logged. *)

val replace :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  draft_id:int64 ->
  selected_snapshot_ids:int64 list ->
  primary_snapshot_id:int64 option ->
  (unit, error) result Lwt.t
(** Replaces the complete selection state of the caller's available draft,
    in one explicit transaction on the supplied connection:

    + the draft row is authorized and locked ([FOR UPDATE] on the draft
      row alone) in one statement requiring the supplied id AND owner
      together with availability — [status = 'active'],
      [expires_at > NOW()], and a backing installation that is [active]
      with [revoked_at IS NULL]; ownership is [user_id] only, never
      [connected_by_user_id] (provenance, not ownership). This is the same
      lock, taken first, as [Project_onboarding_draft_store.refresh_verified],
      so selection updates serialize safely with snapshot refreshes;
    + the complete current snapshot is then loaded and locked under the
      draft lock and its structural invariants re-checked;
    + every supplied id must name a row of that exact snapshot, otherwise
      [Selection_stale] and nothing is modified;
    + selection state is replaced completely: a row ends selected exactly
      when its id occurs in [selected_snapshot_ids], exactly the row named
      by [primary_snapshot_id] (when supplied) ends primary, and every
      other row ends unselected and non-primary — an empty selection
      clears every flag. Repository identity and metadata never change;
      snapshot rows touched by the replacement get a fresh [updated_at]
      (their [created_at] is untouched);
    + the draft's [updated_at] becomes [GREATEST(NOW(), created_at)] —
      clamped because the transaction's [NOW()] can predate a draft row
      created by a concurrent transaction this one waited on. Nothing
      else on the draft moves: in particular [expires_at] is NOT renewed —
      the 24-hour verification lifetime is a security boundary only a new
      successful GitHub verification refreshes.

    On any failure the whole transaction rolls back — the previous
    complete selection remains visible and no mixture ever becomes
    durable. Two concurrent replacements serialize on the draft lock: both
    may succeed and the final state equals one complete submitted
    selection, never a blend. Racing a snapshot refresh yields one of the
    two serialized outcomes: the refresh rebuilds an unselected snapshot
    over a committed selection, or the replacement sees the new snapshot
    and reports [Selection_stale]. [Lwt.Canceled] is never swallowed. This
    module performs no network IO and no logging. *)
