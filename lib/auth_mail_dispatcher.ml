(* The bounds are deliberately small and fixed in code: the dispatcher is a
   pressure valve for a best-effort side effect, not a mail platform. 64
   outstanding requests is far above normal signup/reset traffic (which the
   per-IP limiter already caps) while keeping memory and provider
   concurrency trivially bounded. 15 s covers a slow provider round-trip,
   and because every slot lasts exactly that long, 2 slots mean a sustained
   ceiling of 8 admitted requests per minute. *)
type config = { capacity : int; concurrency : int; timeout_seconds : float }

let default_config = { capacity = 64; concurrency = 2; timeout_seconds = 15.0 }

type 'job transport = 'job -> (unit, string) result Lwt.t

type 'job t = {
  config : config;
  sleep : float -> unit Lwt.t;
  label : 'job -> string;
  transport : 'job transport;
  (* Settled entries awaiting a slot, real ([Some]) and no-send ([None])
     alike, in settlement order. *)
  queue : 'job option Queue.t;
  (* Reservations + queued + in service. Admission compares this, and only
     this, against [capacity], so an admitted request that has not yet
     decided whether it needs mail already holds its place. *)
  mutable outstanding : int;
  mutable running : int;
}

type stats = { outstanding : int; queued : int; running : int }

let monotonic_sleep seconds =
  let deadline = Int64.add (Mtime_clock.now_ns ()) (Int64.of_float (seconds *. 1e9)) in
  (* Lwt's timer may run on wall-clock time, depending on the engine. Checking
     the monotonic clock and sleeping again for any remainder makes a
     forward clock step unable to end the slot early. *)
  let rec wait () =
    let remaining = Int64.sub deadline (Mtime_clock.now_ns ()) in
    if Int64.compare remaining 0L <= 0 then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep (Int64.to_float remaining /. 1e9)) wait
  in
  wait ()

let create ?(config = default_config) ?(sleep = monotonic_sleep) ~label ~transport () =
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

(* Starts one attempt and returns what the slot deadline needs: a closure
   that cancels the attempt if it is still running. That closure and the
   attempt's own callbacks capture only the fixed label, never [job]. Once
   the attempt settles, nothing from the message is left while the slot
   runs out. *)
let start_delivery (t : _ t) job =
  let label = try t.label job with _ -> "unlabelled" in
  let timed_out = ref false in
  let attempt =
    Lwt.catch
      (fun () -> t.transport job)
      (* The exception text is not logged: provider and socket errors can
         carry hostnames, addresses or echoed request detail. *)
      (function
        | Lwt.Canceled -> Lwt.return (Error "cancelled")
        | _ -> Lwt.return (Error "exception"))
  in
  Lwt.on_success attempt (function
    | Ok () -> ()
    | Error _ when !timed_out -> ()
    | Error cls -> Dream.log "auth mail %s: delivery failed (%s)" label cls);
  fun () ->
    if Lwt.is_sleeping attempt then begin
      timed_out := true;
      Dream.log "auth mail %s: delivery timed out" label;
      (* Cancelling makes the transport close its connection rather than
         merely being abandoned. *)
      Lwt.cancel attempt
    end

let rec pump (t : _ t) =
  while t.running < t.config.concurrency && not (Queue.is_empty t.queue) do
    service t (Queue.pop t.queue)
  done

(* One service slot. Only the slot timer ends it: the attempt's outcome
   (success, refusal, error, exception, hang) and the entry's kind do not.
   So occupancy is the same whether or not this entry sends mail. *)
and service (t : _ t) entry =
  t.running <- t.running + 1;
  let cancel_attempt = ref ignore in
  let released = ref false in
  let finish () =
    if not !released then begin
      released := true;
      !cancel_attempt ();
      cancel_attempt := ignore;
      t.running <- t.running - 1;
      t.outstanding <- t.outstanding - 1;
      pump t
    end
  in
  let slot = Lwt.apply t.sleep t.config.timeout_seconds in
  (* The timer is registered before any delivery starts, so a crash while
     starting one can never leave the slot without a release. A timer that
     fails ends the slot like one that fires. *)
  Lwt.on_any slot finish (fun _ -> finish ());
  match entry with
  | Some job when not !released -> cancel_attempt := start_delivery t job
  | Some _ | None -> ()

let admit (t : _ t) work =
  if t.outstanding >= t.config.capacity then Lwt.return `Refused
  else begin
    t.outstanding <- t.outstanding + 1;
    let settled = ref false in
    let settle entry =
      if not !settled then begin
        settled := true;
        Queue.push entry t.queue;
        pump t
      end
    in
    (* A failed or cancelled request still settles into an entry, a no-send
       one: releasing its place early would be a shortcut that only some
       private branches (those that fail) could take. *)
    Lwt.try_bind work
      (fun (result, job) ->
        settle job;
        Lwt.return (`Admitted result))
      (fun exn ->
        settle None;
        Lwt.reraise exn)
  end
