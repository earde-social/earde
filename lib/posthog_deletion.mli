(** Durable PostHog person-deletion worker (analytics spec §3.3).

    This module owns the private Persons-API HTTP client and the per-job
    attempt orchestration; job PERSISTENCE lives in [Db.PosthogDeletionJobs].
    It deliberately sits outside [Analytics] (which stays pure event
    collection, no DB knowledge) and outside [Db] (which performs no network
    IO) — callers wire the two together through the callbacks below, so no
    database connection is ever held across PostHog HTTP.

    Configuration comes from [Analytics.deletion_api_config] (POSTHOG_UI_HOST,
    POSTHOG_PROJECT_ID, POSTHOG_PERSONAL_API_KEY — server-only; the public
    project token is never used here). All diagnostics are bounded safe
    classes: never a response body, URL, credential, or personal data. *)

val default_batch_limit : int

(** One bounded HTTP attempt (no DB access, no retries inside the attempt):
    URL-encoded exact-distinct-id lookup on the project Persons API, then —
    for exactly one unambiguous match — deletion of the person and associated
    events ([delete_events=true]).

    [Ok ()] = the job may be marked completed: PostHog accepted the deletion,
    or the person is already absent. [Error class] = the job must remain
    pending; [class] is one of the bounded diagnostics (missing_configuration,
    timeout, network_error, lookup_http_<n>, malformed_lookup_response,
    ambiguous_person_match, delete_http_<n>). An ambiguous lookup never
    deletes anyone. *)
val attempt_person_deletion : distinct_id:string -> (unit, string) result Lwt.t

(** Runs one ALREADY-CLAIMED job: performs the HTTP attempt, then persists the
    outcome through the provided callbacks (each is expected to run its own
    short DB operation). Never raises; marking failures are logged and
    swallowed. *)
val process_claimed_job :
  mark_completed:(unit -> (unit, string) result Lwt.t) ->
  mark_failed:(string -> (unit, string) result Lwt.t) ->
  distinct_id:string ->
  [ `Completed | `Left_pending of string ] Lwt.t

type batch_summary = { claimed : int; completed : int; left_pending : int }

(** Claims a bounded batch through [claim] and processes each job with exactly
    the same behavior as the immediate post-deletion attempt. Jobs are
    processed sequentially in the order [claim] returns them (oldest first).
    Used by the maintenance executable and by tests. *)
val process_batch :
  claim:(unit -> ((int * string) list, string) result Lwt.t) ->
  mark_completed:(int -> (unit, string) result Lwt.t) ->
  mark_failed:(int -> string -> (unit, string) result Lwt.t) ->
  unit ->
  (batch_summary, string) result Lwt.t
