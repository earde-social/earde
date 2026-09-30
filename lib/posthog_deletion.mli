(** Durable PostHog person-deletion worker (analytics spec §3.3).

    This module owns the private Persons-API HTTP client and the per-job attempt
    orchestration; job PERSISTENCE lives in [Posthog_deletion_job_store]. It
    deliberately sits outside [Analytics] (which stays pure event collection, no
    DB knowledge) and outside the stores (which perform no network IO) — callers
    wire the two together through the callbacks below, so no database connection
    is ever held across PostHog HTTP.

    Configuration comes from [Analytics.deletion_api_config] (POSTHOG_UI_HOST,
    POSTHOG_PROJECT_ID, POSTHOG_PERSONAL_API_KEY — server-only; the public
    project token is never used here). All diagnostics are bounded safe classes:
    never a response body, URL, credential, or personal data. *)

val default_batch_limit : int

val attempt_person_deletion : distinct_id:string -> (unit, string) result Lwt.t
(** One bounded HTTP attempt (no DB access, no retries inside the attempt):
    URL-encoded exact-distinct-id lookup on the project Persons API, then — for
    exactly one unambiguous match — deletion of the person and associated events
    ([delete_events=true]).

    [Ok ()] = the job may be marked completed: PostHog accepted the deletion, or
    the person is already absent. [Error class] = the job must remain pending;
    [class] is one of the bounded diagnostics (missing_configuration, timeout,
    network_error, lookup_http_<n>, malformed_lookup_response,
    ambiguous_person_match, delete_http_<n>). An ambiguous lookup never deletes
    anyone. *)

val group_cleanup_targets : string list
(** The closed set of group properties the §13 privacy scrub removes from a
    fully private community's PostHog group profile. Property KEYS only — values
    (names, slugs) never enter requests, jobs, or logs. *)

val attempt_group_cleanup : group_key:string -> (unit, string) result Lwt.t
(** One bounded, idempotent §13 scrub attempt against the private Groups API:
    read the group ([GET /groups/find/]), then delete whichever
    [group_cleanup_targets] are still present ([POST /groups/delete_property/],
    body [{"$unset": <key>}] — the documented removal mechanism;
    [$groupidentify] has no unset operation).

    [Ok ()] = the job may be marked completed: every target is gone, was never
    there, or the group does not exist in PostHog at all (404 on find).
    [Error class] = the job must remain pending; [class] is one of the bounded
    diagnostics (missing_configuration, timeout, network_error,
    group_lookup_http_<n>, malformed_group_response, group_delete_http_<n>).
    Never raises. *)

val process_claimed_job :
  mark_completed:(unit -> (unit, string) result Lwt.t) ->
  mark_failed:(string -> (unit, string) result Lwt.t) ->
  distinct_id:string ->
  [ `Completed | `Left_pending of string ] Lwt.t
(** Runs one ALREADY-CLAIMED job: performs the HTTP attempt, then persists the
    outcome through the provided callbacks (each is expected to run its own
    short DB operation). Never raises; marking failures are logged and
    swallowed. *)

val process_claimed_group_job :
  mark_completed:(unit -> (unit, string) result Lwt.t) ->
  mark_failed:(string -> (unit, string) result Lwt.t) ->
  group_key:string ->
  [ `Completed | `Left_pending of string ] Lwt.t
(** [process_claimed_job]'s exact counterpart for an already-claimed §13
    group-cleanup job: one [attempt_group_cleanup], outcome persisted through
    the callbacks. Never raises. *)

type batch_summary = { claimed : int; completed : int; left_pending : int }

val process_batch :
  claim:(unit -> ((int * string) list, string) result Lwt.t) ->
  mark_completed:(int -> (unit, string) result Lwt.t) ->
  mark_failed:(int -> string -> (unit, string) result Lwt.t) ->
  unit ->
  (batch_summary, string) result Lwt.t
(** Claims a bounded batch through [claim] and processes each job with exactly
    the same behavior as the immediate post-deletion attempt. Jobs are processed
    sequentially in the order [claim] returns them (oldest first). Used by the
    maintenance executable and by tests. *)

val process_group_batch :
  claim:(unit -> ((int * string) list, string) result Lwt.t) ->
  mark_completed:(int -> (unit, string) result Lwt.t) ->
  mark_failed:(int -> string -> (unit, string) result Lwt.t) ->
  unit ->
  (batch_summary, string) result Lwt.t
(** [process_batch] over §13 group-cleanup jobs ([(job_id, group_key)] rows from
    [Posthog_group_cleanup_job_store.claim_batch]) — the same bounded, durable
    retry mechanism, not a second system. *)
