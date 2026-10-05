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

type operation =
  | Login
  | Signup
  | Forgot_password
  | Github_installation_start
  | Project_repository_selection
  | Project_creation
  | Project_home_request
  | Project_home_provisioning
  | Project_home_accept
  | Project_home_reject
  | Project_home_removal_by_project
  | Project_home_removal_by_community
  | Network_community_publication
  | Community_connection_request
  | Community_connection_accept
  | Community_connection_reject
  | Community_connection_removal
  | Shared_thread_share_request
  | Shared_thread_accept
  | Shared_thread_reject
  | Shared_thread_withdrawal
  | Shared_thread_removal

(* The bucket is the operation, never the request target. Keying on the
   target let every spelling Dream's router maps to the same handler —
   /%6cogin, ///login, a trailing query, a different :slug — open a fresh
   bucket, so the limit never bound. These labels are fixed strings chosen
   here; the "op:" prefix keeps them apart from the upload bucket. *)
let bucket = function
  | Login -> "op:login"
  | Signup -> "op:signup"
  | Forgot_password -> "op:forgot-password"
  | Github_installation_start -> "op:github-installation-start"
  | Project_repository_selection -> "op:project-repository-selection"
  | Project_creation -> "op:project-creation"
  | Project_home_request -> "op:project-home-request"
  | Project_home_provisioning -> "op:project-home-provisioning"
  | Project_home_accept -> "op:project-home-accept"
  | Project_home_reject -> "op:project-home-reject"
  | Project_home_removal_by_project -> "op:project-home-removal-by-project"
  | Project_home_removal_by_community -> "op:project-home-removal-by-community"
  | Network_community_publication -> "op:network-community-publication"
  | Community_connection_request -> "op:community-connection-request"
  | Community_connection_accept -> "op:community-connection-accept"
  | Community_connection_reject -> "op:community-connection-reject"
  | Community_connection_removal -> "op:community-connection-removal"
  | Shared_thread_share_request -> "op:shared-thread-share-request"
  | Shared_thread_accept -> "op:shared-thread-accept"
  | Shared_thread_reject -> "op:shared-thread-reject"
  | Shared_thread_withdrawal -> "op:shared-thread-withdrawal"
  | Shared_thread_removal -> "op:shared-thread-removal"

let is_unreserved c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let encode_segment segment =
  let buf = Buffer.create (String.length segment) in
  String.iter
    (fun c ->
      if is_unreserved c then Buffer.add_char buf c
      else Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    segment;
  Buffer.contents buf

(* The link back from the blocked and unavailable pages. It is rebuilt from
   the routed path rather than echoed: the query is dropped (it can carry
   reset tokens or OAuth codes), empty segments are dropped as Dream's
   router drops them, and every decoded segment is re-encoded with nothing
   but unreserved characters left bare, so the result is always one rooted
   path that cannot turn into a protocol-relative or off-site URL. *)
let return_path target =
  let segments =
    String.split_on_char '/' (Request_target_redaction.path_only target)
    |> List.filter (fun s -> s <> "")
    |> List.map (fun s -> encode_segment (Dream.from_percent_encoded s))
    |> List.filter (fun s -> s <> "")
  in
  "/" ^ String.concat "/" segments

(* Fail closed: only a positive [`Allowed] reaches the wrapped handler.
   The limiter guards credential guessing and mail-sending routes, so a
   lookup that errors, a promise that rejects, or a pool that cannot hand
   out a connection must refuse the request rather than wave it through
   unmetered — before this, a result error invoked the handler exactly as
   if it were allowed. The wrapped handler runs OUTSIDE the catch: an
   exception it raises is its own and propagates as before, never
   relabelled as a limiter outage. *)
let make_middleware ~check ~cleanup operation inner_handler request =
  let ip = Dream.client request in
  let endpoint = bucket operation in
  let return_url = return_path (Dream.target request) in
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
      Dream.html
        (Site_pages.msg_page ~auth:true ?user ~title:"Too Many Attempts"
           ~message:"Too many attempts. Please try again later."
           ~alert_type:"error" ~return_url request)
  | `Unavailable ->
      (* Neither the IP nor the storage error is logged: the error text can
         carry connection and query detail, and the bucket key is an IP. *)
      Dream.log "rate-limit enforcement unavailable; request refused";
      temporarily_unavailable ~return_url request

let middleware operation inner_handler request =
  make_middleware ~check:check_in_database ~cleanup:maybe_cleanup operation
    inner_handler request
