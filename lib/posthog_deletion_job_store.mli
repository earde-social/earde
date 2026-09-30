(* Durable PostHog person-deletion jobs (analytics spec §3.3). Job state only —
   no function here performs network IO; the Persons-API client lives in
   Posthog_deletion. *)
val default_lease_minutes : int

val anonymize_and_enqueue :
  (module Caqti_lwt.CONNECTION) -> int -> (int * string, string) result Lwt.t
(** Account deletion's one durable transaction: lock the user row, apply exactly
    the [User_store.anonymize_user] rewrite, revoke every Dream session
    belonging to that user, insert-or-adopt the pending deletion job for the
    immutable ["user:<id>"], commit both or roll back both. Returns
    [(job_id, distinct_id)]. Duplicate/concurrent calls converge on the same
    job. Never performs HTTP. *)

val claim :
  (module Caqti_lwt.CONNECTION) ->
  ?lease_minutes:int ->
  int ->
  (string option, string) result Lwt.t
(** Atomically claim one pending job (attempts+1, lease timestamp set in the
    same statement). [None] = completed, or lease-held by another attempt. *)

val claim_batch :
  (module Caqti_lwt.CONNECTION) ->
  ?lease_minutes:int ->
  limit:int ->
  unit ->
  ((int * string) list, string) result Lwt.t
(** Claim up to [limit] oldest eligible pending jobs (SKIP LOCKED — safe under
    concurrent invocations), returned oldest-first as [(job_id, distinct_id)].
*)

val mark_completed :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val mark_failed :
  (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
(** Store a bounded safe diagnostic class on a still-pending job. Never pass
    response bodies, URLs, tokens, or personal data. *)

val get_by_distinct_id :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  ((int * string * int * string option) option, string) result Lwt.t
(** [(id, status, attempts, last_error)] for tests/inspection. *)
