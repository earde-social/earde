(** Durable §13 group-profile scrub jobs (privacy cleanup on public->private
    transitions). Same architecture as [PosthogDeletionJobs]: enqueue happens
    inside the authoritative transaction, claims are lease-based and bounded,
    and no function here performs network IO. [group_key] is always the
    immutable ["community:<id>"] — never a name or slug. *)
val default_lease_minutes : int

(** One transaction: apply the visibility UPDATE and, when the new value is
    [Community_private], insert-or-re-arm the pending cleanup job for the
    community's group key; commit both or roll back both. Returns the
    updated community ([None] = id matched nothing) and the job id ([None]
    on ->public transitions). Never performs HTTP. *)
val update_visibility_and_enqueue :
  (module Caqti_lwt.CONNECTION) -> int -> Community_types.community_visibility ->
  (Community_types.community option * int option, string) result Lwt.t

(** Atomically claim one pending job (attempts+1, lease timestamp set in the
    same statement). [None] = completed, or lease-held by another attempt. *)
val claim :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> int ->
  (string option, string) result Lwt.t

(** Claim up to [limit] oldest eligible pending jobs (SKIP LOCKED), returned
    oldest-first as [(job_id, group_key)]. *)
val claim_batch :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> limit:int -> unit ->
  ((int * string) list, string) result Lwt.t

val mark_completed :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

(** Store a bounded safe diagnostic class on a still-pending job. Never pass
    response bodies, URLs, tokens, names, or slugs. *)
val mark_failed :
  (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t

(** [(id, status, attempts, last_error)] for tests/inspection. *)
val get_by_group_key :
  (module Caqti_lwt.CONNECTION) -> string ->
  ((int * string * int * string option) option, string) result Lwt.t
