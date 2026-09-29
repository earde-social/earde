(* The bounds are deliberately small and fixed in code: the dispatcher is a
   pressure valve for a best-effort side effect, not a mail platform. 64
   outstanding requests is far above normal signup/reset traffic (which the
   per-IP limiter already caps) while keeping memory and provider
   concurrency trivially bounded; 2 concurrent deliveries is enough to drain
   that backlog within a few seconds on a healthy provider; 15 s covers a
   slow provider round-trip without letting a hung one pin a worker. *)
type config = { capacity : int; concurrency : int; timeout_seconds : float }

let default_config = { capacity = 64; concurrency = 2; timeout_seconds = 15.0 }

type 'job transport = 'job -> (unit, string) result Lwt.t

type 'job t = {
  config : config;
  sleep : float -> unit Lwt.t;
  label : 'job -> string;
  transport : 'job transport;
  queue : 'job Queue.t;
  (* Reservations + queued + running. Admission compares this, and only
     this, against [capacity], so an admitted request that has not yet
     decided whether it needs mail already holds its place. *)
  mutable outstanding : int;
  mutable running : int;
}

type stats = { outstanding : int; queued : int; running : int }

let create ?(config = default_config) ?(sleep = Lwt_unix.sleep) ~label
    ~transport () =
  if config.capacity <= 0 || config.concurrency <= 0
     || not (config.timeout_seconds > 0.0)
  then invalid_arg "Auth_mail_dispatcher.create: bounds must be positive";
  {
    config;
    sleep;
    label;
    transport;
    queue = Queue.create ();
    outstanding = 0;
    running = 0;
  }

let stats (t : _ t) =
  { outstanding = t.outstanding; queued = Queue.length t.queue; running = t.running }

let run_bounded ?(sleep = Lwt_unix.sleep) ~timeout_seconds transport job =
  let attempt =
    Lwt.catch
      (fun () ->
        Lwt.map
          (function Ok () -> `Delivered | Error cls -> `Failed cls)
          (transport job))
      (* The exception text is not logged: provider and socket errors can
         carry hostnames, addresses or echoed request detail. *)
      (function
        | Lwt.Canceled -> Lwt.return (`Failed "cancelled")
        | _ -> Lwt.return (`Failed "exception"))
  in
  let timer = Lwt.map (fun () -> `Timed_out) (Lwt.apply sleep timeout_seconds) in
  (* Whichever settles first wins and the other is cancelled: a finished
     attempt cancels its timer, and an expired timer cancels the attempt —
     which is what makes the transport close its connection rather than
     merely being abandoned. *)
  Lwt.catch
    (fun () -> Lwt.pick [ attempt; timer ])
    (fun _ -> Lwt.return (`Failed "exception"))

let log_outcome (t : _ t) job = function
  | `Delivered -> ()
  | `Failed cls -> Dream.log "auth mail %s: delivery failed (%s)" (t.label job) cls
  | `Timed_out -> Dream.log "auth mail %s: delivery timed out" (t.label job)

let rec pump (t : _ t) =
  if t.running < t.config.concurrency && not (Queue.is_empty t.queue) then begin
    let job = Queue.pop t.queue in
    t.running <- t.running + 1;
    (* The slot and the admission place are returned only once the bounded
       attempt has settled, so [running] never exceeds [concurrency] and a
       timed-out delivery cannot keep occupying capacity. *)
    let finished () =
      t.running <- t.running - 1;
      t.outstanding <- t.outstanding - 1;
      pump t
    in
    let attempt =
      Lwt.map
        (fun outcome -> log_outcome t job outcome)
        (run_bounded ~sleep:t.sleep ~timeout_seconds:t.config.timeout_seconds
           t.transport job)
    in
    Lwt.on_any attempt finished (fun _ -> finished ());
    pump t
  end

let admit (t : _ t) work =
  if t.outstanding >= t.config.capacity then Lwt.return `Refused
  else begin
    t.outstanding <- t.outstanding + 1;
    let settled = ref false in
    let settle job =
      if not !settled then begin
        settled := true;
        match job with
        | None -> t.outstanding <- t.outstanding - 1
        | Some job ->
            Queue.push job t.queue;
            pump t
      end
    in
    Lwt.try_bind work
      (fun (result, job) ->
        settle job;
        Lwt.return (`Admitted result))
      (fun exn ->
        settle None;
        Lwt.reraise exn)
  end
