open Lwt.Infix

(* Applied selectively to hot paths — per-query instrumentation on every call
   adds two gettimeofday syscalls and a Yojson allocation per request. *)
let with_query_timer ~name f =
  let t0 = Unix.gettimeofday () in
  f () >>= fun result ->
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  let status = match result with Ok _ -> "ok" | Error _ -> "error" in
  Logs.info (fun m ->
    m "%s" (Yojson.Safe.to_string (`Assoc [
      ("query",        `String name);
      ("execution_ms", `Float ms);
      ("status",       `String status);
    ]))
  );
  Lwt.return result
