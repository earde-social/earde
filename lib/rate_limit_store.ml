open Lwt.Infix

(* DB-backed rate limit trades a synchronous Hashtbl lookup for a round-trip to
   Postgres; the ~1ms I/O penalty is the price of crash resilience and shared
   state across replicas — unavoidable once we move beyond a single process. *)
let max_attempts = 5

(* The one enforcement window, in seconds. check_q hardcodes the same 60.0
   literal in its SQL (see its comment); keep the two in lockstep — the
   cleanup retention below derives from this constant, so a longer window
   automatically lengthens retention and can never be undercut by cleanup. *)
let window_seconds = 60.0

(* Retention for the stored (ip, endpoint) rows: one full window of slack
   beyond the point where a row stops being enforceable (its next hit would
   reset it anyway). Rows STRICTLY older than [now - cleanup_after_seconds]
   are eligible; a row at exactly the boundary is kept — that strict-<
   rule is the documented boundary behavior and is pinned by the gated
   suite. *)
let cleanup_after_seconds = 2.0 *. window_seconds

(* Single atomic upsert: resets the window when expired, otherwise increments.
   Hardcoding 60.0 avoids a fourth bind parameter and keeps the query plan stable. *)
let check_q =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 string string float) ->! Caqti_type.int)
    {|INSERT INTO rate_limits (ip_address, endpoint, attempts, window_start)
    VALUES ($1, $2, 1, $3)
    ON CONFLICT (ip_address, endpoint) DO UPDATE
      SET attempts     = CASE WHEN rate_limits.window_start + 60.0 < EXCLUDED.window_start
                              THEN 1
                              ELSE rate_limits.attempts + 1
                         END,
          window_start = CASE WHEN rate_limits.window_start + 60.0 < EXCLUDED.window_start
                              THEN EXCLUDED.window_start
                              ELSE rate_limits.window_start
                         END
    RETURNING attempts|}

(* The per-endpoint allowance is applied in OCaml, so a second policy needs
   no second query and no schema change: the upsert always counts, and only
   the comparison differs. The 60s window stays shared and hardcoded in the
   SQL above. *)
let check_with ?(now = Unix.gettimeofday ()) ~max_attempts
    (module C : Caqti_lwt.CONNECTION) ip endpoint =
  C.find check_q (ip, endpoint, now) >>= function
  | Ok attempts ->
      if attempts > max_attempts then Lwt.return (Ok `Blocked)
      else Lwt.return (Ok `Allowed)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let check ?now db ip endpoint = check_with ?now ~max_attempts db ip endpoint

(* Image uploads get their own bucket rather than sharing the
   authentication allowance: an upload is far more expensive than a login
   attempt, but a member legitimately edits several images in a row, so the
   two policies want different numbers. The endpoint name is a constant,
   never a request path, so all three upload routes share one budget. *)
let upload_endpoint = "image-upload"
let upload_max_attempts = 10

let check_upload db ip =
  check_with ~max_attempts:upload_max_attempts db ip upload_endpoint

(* One bounded cleanup batch: deletes at most [batch] expired rows so a
   large backlog can never stall a request-path connection. The only bind
   parameter is a timestamp — no IP or endpoint value can appear in the
   query, its parameters, or a Caqti error string, so cleanup logging is
   IP-free by construction. Concurrent executions are harmless (DELETE of
   already-deleted ctids matches nothing). Returns the number of rows
   removed. *)
let cleanup_batch = 500

let cleanup_q =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 float int) ->! Caqti_type.int)
    {|WITH doomed AS (
      SELECT ctid FROM rate_limits WHERE window_start < $1 LIMIT $2
    ), deleted AS (
      DELETE FROM rate_limits WHERE ctid IN (SELECT ctid FROM doomed)
      RETURNING 1
    ) SELECT COUNT(*)::int FROM deleted|}

let cleanup_expired ?(now = Unix.gettimeofday ())
    (module C : Caqti_lwt.CONNECTION) =
  C.find cleanup_q (now -. cleanup_after_seconds, cleanup_batch) >>= function
  | Ok deleted -> Lwt.return (Ok deleted)
  | Error e -> Lwt.return (Error (Caqti_error.show e))
