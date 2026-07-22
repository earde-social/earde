(* Bounded maintenance retry for durable PostHog person-deletion jobs
   (analytics spec §3.3). Manually runnable:

     DATABASE_URL=... dune exec bin/retry_posthog_deletions.exe

   Claims up to 25 oldest eligible pending jobs (atomic claim: status + lease,
   SKIP LOCKED — concurrent invocations are safe), attempts each against the
   private Persons API with the same worker used by the immediate attempt, and
   terminates. Failures stay durably pending with a bounded safe diagnostic.
   Output is aggregate counts plus job ids/statuses only — never credentials,
   URLs, response bodies, email, or username. *)

let batch_limit = Earde.Posthog_deletion.default_batch_limit

(* Belt-and-braces for the one identifier we do print alongside job ids: only
   the expected immutable "user:<numeric-id>" shape is ever echoed. *)
let printable_distinct_id distinct_id =
  let expected =
    match String.index_opt distinct_id ':' with
    | Some 4 ->
        String.sub distinct_id 0 5 = "user:"
        && String.length distinct_id > 5
        && String.for_all
             (function '0' .. '9' -> true | _ -> false)
             (String.sub distinct_id 5 (String.length distinct_id - 5))
    | _ -> false
  in
  if expected then distinct_id else "<unexpected-distinct-id>"

let run url =
  let ( let* ) = Lwt.bind in
  let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
  match conn with
  | Error err ->
      prerr_endline ("FATAL: database connection failed: " ^ Caqti_error.show err);
      Lwt.return 1
  | Ok conn -> (
      (* The claim and each outcome mark are single autocommit statements; no
         transaction is open — and this dedicated CLI connection is idle —
         while PostHog HTTP is in flight. *)
      let claimed_jobs = ref [] in
      let* summary =
        Earde.Posthog_deletion.process_batch
          ~claim:(fun () ->
            let* jobs =
              Earde.Db.claim_posthog_deletion_batch conn ~limit:batch_limit ()
            in
            (match jobs with
            | Ok jobs ->
                claimed_jobs := jobs;
                List.iter
                  (fun (job_id, distinct_id) ->
                    Printf.printf "claimed job %d (%s)\n%!" job_id
                      (printable_distinct_id distinct_id))
                  jobs
            | Error _ -> ());
            Lwt.return jobs)
          ~mark_completed:(fun job_id ->
            Printf.printf "job %d: completed\n%!" job_id;
            Earde.Db.complete_posthog_deletion_job conn job_id)
          ~mark_failed:(fun job_id err ->
            Printf.printf "job %d: still pending (%s)\n%!" job_id err;
            Earde.Db.fail_posthog_deletion_job conn job_id err)
          ()
      in
      match summary with
      | Error err ->
          prerr_endline ("FATAL: claiming pending jobs failed: " ^ err);
          Lwt.return 1
      | Ok { Earde.Posthog_deletion.claimed; completed; left_pending } ->
          Printf.printf "done: claimed=%d completed=%d still_pending=%d\n%!"
            claimed completed left_pending;
          Lwt.return 0)

let () =
  match Sys.getenv_opt "DATABASE_URL" with
  | None | Some "" ->
      prerr_endline "FATAL: DATABASE_URL environment variable is required.";
      exit 1
  | Some url -> exit (Lwt_main.run (run url))
