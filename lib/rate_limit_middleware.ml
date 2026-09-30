(* One generic 503 for "this protected action cannot be safely processed
   right now": the rate limiter's storage being unavailable, and the auth
   mail dispatcher being full. Deliberately identical for every account
   state, and it names no cause. *)
let temporarily_unavailable ~return_url request =
  Dream.respond ~status:`Service_Unavailable
    (Site_pages.msg_page ~auth:true ~title:"Temporarily unavailable"
       ~message:
         "We can't process this request right now. Please try again in a few \
          minutes."
       ~alert_type:"error" ~return_url request)

(* DB-backed rate limit trades a synchronous Hashtbl lookup for a round-trip to
   Postgres; the ~1ms I/O penalty is the price of crash resilience and shared
   state across replicas — unavoidable once we move beyond a single process. *)
(* Opportunistic expiry cleanup, piggybacked on rate-limited requests at a
   bounded cadence: at most one batch per [cleanup_every_seconds] per
   process, off the response path (Lwt.async, same pattern as the PostHog
   deletion attempts). The retention rule lives in Rate_limit_store
   (rows strictly older than 2x the enforcement window); a cleanup failure
   only logs a bounded, IP-free error and never affects the limiter's
   Allowed/Blocked decision. A racing double-fire between the read and the
   write of [last_cleanup] just runs a second idempotent batch. *)
let cleanup_every_seconds = 600.0
let last_cleanup = ref 0.0

let maybe_cleanup request =
  let now = Unix.gettimeofday () in
  if now -. !last_cleanup >= cleanup_every_seconds then begin
    last_cleanup := now;
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            match%lwt
              Dream.sql request (fun db -> Rate_limit_store.cleanup_expired db)
            with
            | Ok _deleted -> Lwt.return_unit
            | Error e ->
                Dream.log "rate-limit cleanup failed: %s" e;
                Lwt.return_unit)
          (fun exn ->
            Dream.log "rate-limit cleanup skipped: %s" (Printexc.to_string exn);
            Lwt.return_unit))
  end

let check_in_database request ~ip ~endpoint =
  Dream.sql request (fun db -> Rate_limit_store.check db ip endpoint)

(* Fail closed: only a positive [`Allowed] reaches the wrapped handler.
   The limiter guards credential guessing and mail-sending routes, so a
   lookup that errors, a promise that rejects, or a pool that cannot hand
   out a connection must refuse the request rather than wave it through
   unmetered — before this, a result error invoked the handler exactly as
   if it were allowed. The wrapped handler runs OUTSIDE the catch: an
   exception it raises is its own and propagates as before, never
   relabelled as a limiter outage. *)
let make_middleware ~check ~cleanup inner_handler request =
  let ip = Dream.client request in
  (* Path only: the rate-limit table must never persist query values (reset
     tokens, OAuth state/code, search terms), and /login?x=y must share
     /login's bucket rather than minting a fresh one per query string. *)
  let endpoint = Request_target_redaction.path_only (Dream.target request) in
  (* Cleanup is best effort and must not be able to change the decision
     below, even by raising synchronously. *)
  (try cleanup request
   with exn ->
     Dream.log "rate-limit cleanup skipped: %s" (Printexc.to_string exn));
  let%lwt decision =
    Lwt.catch
      (fun () ->
        Lwt.map
          (function Ok d -> `Decided d | Error _ -> `Unavailable)
          (check request ~ip ~endpoint))
      (function
        | Lwt.Canceled -> Lwt.reraise Lwt.Canceled
        | _ -> Lwt.return `Unavailable)
  in
  match decision with
  | `Decided `Allowed -> inner_handler request
  | `Decided `Blocked ->
      let user = Dream.session_field request "username" in
      (* The blocked page's return link reuses the path-only endpoint: echoing
         the full target would leak query secrets (OAuth code/state, reset
         tokens) into the rendered HTML. *)
      Dream.html
        (Site_pages.msg_page ~auth:true ?user ~title:"Too Many Attempts"
           ~message:"Too many attempts. Please try again later."
           ~alert_type:"error" ~return_url:endpoint request)
  | `Unavailable ->
      (* Neither the IP nor the storage error is logged: the error text can
         carry connection and query detail, and the bucket key is an IP. *)
      Dream.log "rate-limit enforcement unavailable; request refused";
      temporarily_unavailable ~return_url:endpoint request

let middleware inner_handler request =
  make_middleware ~check:check_in_database ~cleanup:maybe_cleanup inner_handler
    request
