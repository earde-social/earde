open Lwt.Infix

(* Durable PostHog person-deletion jobs (analytics spec §3.3). Job state only —
   the Persons-API HTTP client lives in Posthog_deletion, and no function here
   ever performs network IO. *)
(* Retry lease: a claimed pending job is ineligible for this long, so an
   immediate attempt and a maintenance retry (or two concurrent retries)
   cannot process the same job at once; a crash after claiming simply makes
   the job eligible again after the lease. *)
let default_lease_minutes = 15

let lock_user_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
    "SELECT id FROM users WHERE id = $1 FOR UPDATE"

(* ON CONFLICT (distinct_id) DO UPDATE is a no-op rewrite that still RETURNS
   the existing row's id, so duplicate/concurrent deletions deterministically
   converge on ONE job instead of erroring or creating competitors. *)
let enqueue_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id) VALUES ($1)\n\
    \   ON CONFLICT (distinct_id) DO UPDATE SET distinct_id = \
     EXCLUDED.distinct_id\n\
    \   RETURNING id"

(* §3.3 atomic local deletion: lock the user row, apply exactly the
   anonymize_user rewrite, enqueue (or adopt) the durable deletion job, and
   commit both together. The FOR UPDATE lock serializes concurrent deletions
   of the same account. No HTTP happens anywhere near this transaction. *)
let anonymize_and_enqueue (module C : Caqti_lwt.CONNECTION) user_id =
  let distinct_id = Analytics.distinct_id_of_user_id user_id in
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () -> (
      C.find_opt lock_user_query user_id >>= function
      | Error e ->
          C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
      | Ok _locked_row -> (
          (* A missing row (already hard-deleted) keeps the old anonymize_user
          semantics — the UPDATE matches nothing — while the deletion job is
          still enqueued: PostHog may hold data regardless. *)
          User_store.anonymize_user (module C) user_id
          >>= function
          | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
          | Ok () -> (
              (* Session revocation joins the same transaction as the
             anonymization and the deletion job: there is no window in
             which the account is anonymized but another browser still
             authenticates as it, and a failure here rolls the whole
             deletion back rather than leaving it half-done. Only this
             user's rows are matched. *)
              Credential_store.delete_for_user (module C) user_id
              >>= function
              | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
              | Ok () -> (
                  C.find enqueue_query distinct_id >>= function
                  | Error e ->
                      C.rollback () >>= fun _ ->
                      Lwt.return (Error (Caqti_error.show e))
                  | Ok job_id -> (
                      C.commit () >>= function
                      | Error e -> Lwt.return (Error (Caqti_error.show e))
                      | Ok () -> Lwt.return (Ok (job_id, distinct_id)))))))

(* Atomic claim of one specific pending job (the immediate post-deletion
   attempt): attempts and the lease timestamp advance in the same statement.
   None = already completed, or claimed within the lease by someone else. *)
let claim_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "UPDATE posthog_person_deletion_jobs\n\
    \   SET attempts = attempts + 1, last_attempt_at = NOW()\n\
    \   WHERE id = $1 AND status = 'pending'\n\
    \     AND (last_attempt_at IS NULL\n\
    \          OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))\n\
    \   RETURNING distinct_id"

let claim (module C : Caqti_lwt.CONNECTION)
    ?(lease_minutes = default_lease_minutes) job_id =
  C.find_opt claim_query (job_id, lease_minutes) >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Maintenance batch claim: oldest eligible pending jobs first, bounded by
   LIMIT; FOR UPDATE SKIP LOCKED keeps concurrent invocations from blocking
   on (or double-claiming) the same rows. RETURNING order is unspecified, so
   the caller re-sorts by id (BIGSERIAL follows enqueue order). *)
(* Canonical job-queue claim: the picked set lives in a CTE (materialized —
   it contains FOR UPDATE), because a bare `WHERE id IN (SELECT ... LIMIT n
   FOR UPDATE SKIP LOCKED)` may re-evaluate the subquery during the outer
   scan and claim more than n rows. *)
let claim_batch_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->* Caqti_type.(t2 int string))
    "WITH picked AS (\n\
    \     SELECT id FROM posthog_person_deletion_jobs\n\
    \     WHERE status = 'pending'\n\
    \       AND (last_attempt_at IS NULL\n\
    \            OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))\n\
    \     ORDER BY created_at ASC, id ASC\n\
    \     LIMIT $1\n\
    \     FOR UPDATE SKIP LOCKED)\n\
    \   UPDATE posthog_person_deletion_jobs j\n\
    \   SET attempts = j.attempts + 1, last_attempt_at = NOW()\n\
    \   FROM picked\n\
    \   WHERE j.id = picked.id\n\
    \   RETURNING j.id, j.distinct_id"

let claim_batch (module C : Caqti_lwt.CONNECTION)
    ?(lease_minutes = default_lease_minutes) ~limit () =
  C.collect_list claim_batch_query (limit, lease_minutes) >>= function
  | Ok rows ->
      Lwt.return (Ok (List.sort (fun (a, _) (b, _) -> compare a b) rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let mark_completed_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs\n\
    \   SET status = 'completed', completed_at = NOW(), last_error = NULL\n\
    \   WHERE id = $1"

let mark_completed (module C : Caqti_lwt.CONNECTION) job_id =
  C.exec mark_completed_query job_id >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* attempts/last_attempt_at were already advanced by the claim; a failure
   only records the bounded safe diagnostic. Length-capped as a backstop —
   callers must already pass short error classes, never response bodies. *)
let mark_failed_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs\n\
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
    \   FROM posthog_person_deletion_jobs WHERE distinct_id = $1"

let get_by_distinct_id (module C : Caqti_lwt.CONNECTION) distinct_id =
  C.find_opt get_query distinct_id >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error err -> Lwt.return (Error (Caqti_error.show err))
