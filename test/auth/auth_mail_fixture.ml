(* Shared infrastructure for the auth-mail and account-privacy cases: a
   virtual monotonic clock, controlled transports and occupancy traces for
   the dispatcher, log capture, and message token extraction. *)

let ( let* ) = Lwt.bind


module D = Earde.Auth_mail_dispatcher
module R = Earde.Auth_mail_resolver
module H = Earde.Handlers
module LV = Earde.Login_verification

let contains = Html_assert.contains

let count_occurrences = Html_assert.count_sub

let must label hay needle =
  if not (contains hay needle) then Alcotest.failf "%s: expected text missing" label

let must_not label hay needle =
  if contains hay needle then Alcotest.failf "%s: forbidden text present" label

let rec settle n = if n <= 0 then Lwt.return_unit else Lwt.bind (Lwt.pause ()) (fun () -> settle (n - 1))

(* Polls real time for a condition that local IO will make true; bounded so
   a regression fails instead of hanging the suite. *)
let eventually label ?(seconds = 10.0) cond =
  let deadline = Unix.gettimeofday () +. seconds in
  let rec go () =
    if cond () then Lwt.return_unit
    else if Unix.gettimeofday () > deadline then Alcotest.failf "%s: never happened" label
    else Lwt.bind (Lwt_unix.sleep 0.005) go
  in
  go ()

(* ------------------------------------------------------------------ *)
(* Log capture: every diagnostic emitted while [f] runs.               *)
(* ------------------------------------------------------------------ *)

let with_captured_logs f =
  (* Dream installs its own reporter on its first log call; initialize it
     now so that cannot replace the capture midway. *)
  Dream.initialize_log ~level:`Info ();
  let buffer = Buffer.create 1024 in
  let previous_reporter = Logs.reporter () in
  let previous_level = Logs.level () in
  let report _src _level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun s ->
            Buffer.add_string buffer s;
            Buffer.add_char buffer '\n';
            over ();
            k ())
          fmt)
  in
  Logs.set_reporter { Logs.report };
  (* Dream's production level for every source. Debug-level library tracing
     (cohttp dumps raw request bytes) is off in production and out of scope. *)
  Logs.set_level ~all:true (Some Logs.Info);
  Lwt.finalize
    (fun () -> f (fun () -> Buffer.contents buffer))
    (fun () ->
      Logs.set_reporter previous_reporter;
      Logs.set_level ~all:true previous_level;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Dispatcher: virtual clock and controlled transports                  *)
(* ------------------------------------------------------------------ *)

(* Virtual monotonic time. Timers fire only when the test advances the
   clock, in (deadline, arming) order, and a timer armed by a firing
   callback also fires if it falls due within the same advance. So every
   schedule below is exact and needs no real waiting. *)
module Vclock = struct
  type t = {
    mutable now : float;
    mutable timers : (float * int * unit Lwt.u) list;
    mutable seq : int;
  }

  let create () = { now = 0.0; timers = []; seq = 0 }

  (* Like the production timer, not cancelable. *)
  let sleep c d =
    let p, u = Lwt.wait () in
    c.seq <- c.seq + 1;
    c.timers <- (c.now +. d, c.seq, u) :: c.timers;
    p

  let next_due c limit =
    List.fold_left
      (fun best ((at, seq, _) as timer) ->
        if at > limit then best
        else
          match best with
          | Some (bat, bseq, _) when bat < at || (bat = at && bseq < seq) -> best
          | _ -> Some timer)
      None c.timers

  let rec advance_to ?(on_fire = fun (_ : float) -> ()) c limit =
    match next_due c limit with
    | None -> if limit > c.now then c.now <- limit
    | Some ((at, _, u) as timer) ->
        c.timers <- List.filter (fun t -> t != timer) c.timers;
        c.now <- at;
        Lwt.wakeup u ();
        on_fire at;
        advance_to ~on_fire c limit
end

let occupancy d =
  let s = D.stats d in
  (s.D.outstanding, s.D.queued, s.D.running)

(* The exact occupancy schedule up to [limit]: (time, (outstanding, queued,
   running)) at every timer firing that changed it. Also enforces the
   two-slot bound at every firing. *)
let trace clock d limit =
  let events = ref [] and last = ref (occupancy d) in
  Vclock.advance_to clock limit ~on_fire:(fun at ->
      let ((_, _, running) as s) = occupancy d in
      if running > D.default_config.D.concurrency then
        Alcotest.failf "%d slots in use at t=%.3f" running at;
      if s <> !last then begin
        events := (at, s) :: !events;
        last := s
      end);
  List.rev !events

let schedule = Alcotest.(list (pair (float 0.0) (triple int int int)))

module Fake = struct
  type t = {
    clock : Vclock.t;
    mutable started : (int * float) list;
    mutable finished : int list;
    mutable cancelled : int list;
    gates : (int, (unit, string) result Lwt.u) Hashtbl.t;
  }

  let create clock =
    { clock; started = []; finished = []; cancelled = []; gates = Hashtbl.create 16 }

  (* Every delivery blocks on its own cancelable promise until the test
     resolves it, or until the dispatcher cancels it at the slot deadline,
     which is recorded as the transport's resource release. *)
  let transport f job =
    f.started <- f.started @ [ (job, f.clock.Vclock.now) ];
    let p, u = Lwt.task () in
    Lwt.on_cancel p (fun () -> f.cancelled <- f.cancelled @ [ job ]);
    Hashtbl.replace f.gates job u;
    Lwt.map
      (fun r ->
        f.finished <- f.finished @ [ job ];
        r)
      p

  let resolve f job r =
    let u = Hashtbl.find f.gates job in
    Hashtbl.remove f.gates job;
    Lwt.wakeup u r

  let started_jobs f = List.map fst f.started
end

(* What a transport does with an attempt, from the dispatcher's point of
   view. None of these keeps a reference to the job. *)
type behaviour =
  | Stall  (* never answers; honours cancellation *)
  | Fast_ok
  | Fast_error
  | Sync_raise
  | Async_raise
  | After of float * (unit, string) result  (* on the virtual clock; ignores cancellation *)

let behaviour_name = function
  | Stall -> "stall"
  | Fast_ok -> "fast success"
  | Fast_error -> "fast failure"
  | Sync_raise -> "synchronous raise"
  | Async_raise -> "rejected promise"
  | After (s, Ok ()) -> Printf.sprintf "success after %.0fs" s
  | After (s, Error _) -> Printf.sprintf "failure after %.0fs" s

let behaviour_transport clock ~record behaviour job =
  record job;
  match behaviour with
  | Stall -> fst (Lwt.task ())
  | Fast_ok -> Lwt.return (Ok ())
  | Fast_error -> Lwt.return (Error "http_503")
  | Sync_raise -> failwith "sync transport failure"
  | Async_raise -> Lwt.fail (Failure "async transport failure")
  | After (s, r) -> Lwt.map (fun () -> r) (Vclock.sleep clock s)

let check_stats label d ~outstanding ~queued ~running =
  Alcotest.(check (list int))
    (label ^ ": outstanding/queued/running")
    [ outstanding; queued; running ]
    (let o, q, r = occupancy d in [ o; q; r ])

let make_dispatcher ?config clock fake =
  D.create ?config ~sleep:(Vclock.sleep clock) ~label:(fun _ -> "test")
    ~transport:(Fake.transport fake) ()

let submit d job = D.admit d (fun () -> Lwt.return ((), job))

let pure_case name f = Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f ()))

let is_refused = function `Refused -> true | `Admitted _ -> false

let token_of_message m =
  let link = Earde.Email.link m in
  match Uri.get_query_param (Uri.of_string link) "token" with
  | Some t -> t
  | None -> Alcotest.fail "message link carries no token"
