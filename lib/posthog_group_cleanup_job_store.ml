open Lwt.Infix

(* Durable §13 group-profile scrub jobs: when a community turns fully
   private, its previously sent human-readable PostHog group properties must
   be removed via the private Groups API. Same architecture as
   PosthogDeletionJobs — enqueue in the authoritative transaction, bounded
   lease-based claims, HTTP strictly outside the DB layer. *)
let default_lease_minutes = 15

(* Re-arming upsert: while pending, duplicate transitions converge on ONE
   job; a NEW public->private transition re-arms a completed job (fresh
   logical scrub request, counters and diagnostics reset). *)
let enqueue_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_group_cleanup_jobs (group_key) VALUES ($1)\n\
    \   ON CONFLICT (group_key) DO UPDATE\n\
    \     SET status = 'pending', attempts = 0, last_error = NULL,\n\
    \         last_attempt_at = NULL, completed_at = NULL\n\
    \   RETURNING id"

(* §13 atomic transition: the visibility UPDATE and — when the new value is
   private — the durable cleanup job commit together or roll back together,
   so a crash can never leave a private community without its pending
   scrub. No HTTP anywhere near this transaction. Returns the updated
   community (None = id matched nothing) and the enqueued job id (None on
   ->public transitions, which need no scrub). *)
let update_visibility_and_enqueue (module C : Caqti_lwt.CONNECTION) community_id
    visibility =
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () -> (
      Community_store.update_community_visibility
        (module C)
        community_id visibility
      >>= function
      | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
      | Ok None -> (
          C.commit () >>= function
          | Error e -> Lwt.return (Error (Caqti_error.show e))
          | Ok () -> Lwt.return (Ok (None, None)))
      | Ok (Some updated) -> (
          if visibility = Community_types.Community_private then
            C.find enqueue_query
              (Analytics.community_group_key
                 (updated : Community_types.community).id)
            >>= function
            | Error e ->
                C.rollback () >>= fun _ ->
                Lwt.return (Error (Caqti_error.show e))
            | Ok job_id -> (
                C.commit () >>= function
                | Error e -> Lwt.return (Error (Caqti_error.show e))
                | Ok () -> Lwt.return (Ok (Some updated, Some job_id)))
          else
            C.commit () >>= function
            | Error e -> Lwt.return (Error (Caqti_error.show e))
            | Ok () -> Lwt.return (Ok (Some updated, None))))

let claim_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "UPDATE posthog_group_cleanup_jobs\n\
    \   SET attempts = attempts + 1, last_attempt_at = NOW()\n\
    \   WHERE id = $1 AND status = 'pending'\n\
    \     AND (last_attempt_at IS NULL\n\
    \          OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))\n\
    \   RETURNING group_key"

let claim (module C : Caqti_lwt.CONNECTION)
    ?(lease_minutes = default_lease_minutes) job_id =
  C.find_opt claim_query (job_id, lease_minutes) >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Same canonical CTE claim as PosthogDeletionJobs.claim_batch (see that
   comment for the FOR UPDATE SKIP LOCKED rationale). *)
let claim_batch_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->* Caqti_type.(t2 int string))
    "WITH picked AS (\n\
    \     SELECT id FROM posthog_group_cleanup_jobs\n\
    \     WHERE status = 'pending'\n\
    \       AND (last_attempt_at IS NULL\n\
    \            OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))\n\
    \     ORDER BY created_at ASC, id ASC\n\
    \     LIMIT $1\n\
    \     FOR UPDATE SKIP LOCKED)\n\
    \   UPDATE posthog_group_cleanup_jobs j\n\
    \   SET attempts = j.attempts + 1, last_attempt_at = NOW()\n\
    \   FROM picked\n\
    \   WHERE j.id = picked.id\n\
    \   RETURNING j.id, j.group_key"

let claim_batch (module C : Caqti_lwt.CONNECTION)
    ?(lease_minutes = default_lease_minutes) ~limit () =
  C.collect_list claim_batch_query (limit, lease_minutes) >>= function
  | Ok rows ->
      Lwt.return (Ok (List.sort (fun (a, _) (b, _) -> compare a b) rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let mark_completed_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posthog_group_cleanup_jobs\n\
    \   SET status = 'completed', completed_at = NOW(), last_error = NULL\n\
    \   WHERE id = $1"

let mark_completed (module C : Caqti_lwt.CONNECTION) job_id =
  C.exec mark_completed_query job_id >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let mark_failed_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE posthog_group_cleanup_jobs\n\
    \   SET last_error = $2\n\
    \   WHERE id = $1 AND status = 'pending'"

let mark_failed (module C : Caqti_lwt.CONNECTION) job_id error =
  let error =
    if String.length error > 120 then String.sub error 0 120 else error
  in
  C.exec mark_failed_query (job_id, error) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Test/inspection lookup: (id, status, attempts, last_error). *)
let get_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t4 int string int (option string)))
    "SELECT id, status, attempts, last_error\n\
    \   FROM posthog_group_cleanup_jobs WHERE group_key = $1"

let get_by_group_key (module C : Caqti_lwt.CONNECTION) group_key =
  C.find_opt get_query group_key >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error err -> Lwt.return (Error (Caqti_error.show err))
