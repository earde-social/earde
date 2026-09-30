(* Account privacy and fail-closed authentication boundaries.

   DB-free component suites pin the auth-mail scheduler (fixed service
   slots for real and no-send entries alike on a controlled clock: exact
   capacity, FIFO order, two slots, release only at the deadline), the
   resolver gate (physical lookups own their permits), the Brevo
   transport's connection release against local servers, the login
   dummy-verification contract, the fail-closed rate-limit decision, and
   the strict response/cookie comparator. The gated suites
   (EARDE_TEST_DATABASE_URL) drive the real signup, login, password-reset
   and limiter handlers over routed pipelines against PostgreSQL, including
   paired target/probe sequences at the capacity edge. Fixture names use the
   b2x_ prefix and the reserved b2.invalid domain; every gated case cleans
   before and after. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module D = Earde.Auth_mail_dispatcher
module R = Earde.Auth_mail_resolver
module H = Earde.Handlers
module LV = Earde.Login_verification

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let count_occurrences hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i acc =
    if n = 0 || i + n > h then acc
    else if String.sub hay i n = needle then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

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

let default_bounds_case =
  pure_case "default bounds are 64 outstanding, 2 slots, 15 s per slot" (fun () ->
      Alcotest.(check int) "capacity" 64 D.default_config.D.capacity;
      Alcotest.(check int) "concurrency" 2 D.default_config.D.concurrency;
      Alcotest.(check (float 0.0)) "slot" 15.0 D.default_config.D.timeout_seconds;
      Alcotest.check_raises "zero capacity rejected"
        (Invalid_argument "Auth_mail_dispatcher.create: bounds must be positive")
        (fun () ->
          ignore
            (D.create ~config:{ D.default_config with D.capacity = 0 }
               ~label:(fun _ -> "x") ~transport:(fun _ -> Lwt.return (Ok ())) ()
              : int D.t));
      Lwt.return_unit)

let capacity_case =
  pure_case
    "capacity: 64 open reservations fill it, the 65th is refused without \
     running its work, and a no-mail settlement keeps its place until its \
     fixed slot ends" (fun () ->
      let clock = Vclock.create () in
      let fake = Fake.create clock in
      let d = make_dispatcher clock fake in
      let holds =
        List.init 64 (fun _ ->
            let p, u = Lwt.wait () in
            (u, D.admit d (fun () -> p)))
      in
      check_stats "64 held" d ~outstanding:64 ~queued:0 ~running:0;
      let ran = ref false in
      let* r =
        D.admit d (fun () ->
            ran := true;
            Lwt.return ((), None))
      in
      Alcotest.(check bool) "65th refused" true (is_refused r);
      Alcotest.(check bool) "refused work never ran" false !ran;
      let settle_hold i job =
        let u, r = List.nth holds i in
        Lwt.wakeup u ((), job);
        r
      in
      let* _ = settle_hold 0 None in
      let* _ = settle_hold 1 (Some 1) in
      let* _ = settle_hold 2 None in
      let* _ = settle_hold 3 (Some 2) in
      check_stats "settled entries keep their places" d ~outstanding:64 ~queued:2 ~running:2;
      Alcotest.(check (list int)) "only the real entry in service reached the transport" [ 1 ]
        (Fake.started_jobs fake);
      let* r = submit d None in
      Alcotest.(check bool) "a no-mail settlement freed nothing" true (is_refused r);
      Vclock.advance_to clock 14.999;
      check_stats "just before the deadline" d ~outstanding:64 ~queued:2 ~running:2;
      let* r = submit d None in
      Alcotest.(check bool) "still refused just before the deadline" true (is_refused r);
      Vclock.advance_to clock 15.0;
      check_stats "at the deadline" d ~outstanding:62 ~queued:0 ~running:2;
      Alcotest.(check (list (pair int (float 0.0)))) "the queued real entry starts at 15 s"
        [ (1, 0.0); (2, 15.0) ] fake.Fake.started;
      let* r = submit d None in
      Alcotest.(check bool) "admitted at the deadline" false (is_refused r);
      List.iteri (fun i (u, _) -> if i >= 4 then Lwt.wakeup u ((), None)) holds;
      let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) holds) in
      check_stats "every settlement queued" d ~outstanding:63 ~queued:61 ~running:2;
      Vclock.advance_to clock 10_000.0;
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Alcotest.(check (list int)) "no transport call for no-send entries" [ 1; 2 ]
        (Fake.started_jobs fake);
      Lwt.return_unit)

let fifo_case =
  pure_case
    "FIFO: real and no-send entries share one queue in settlement order, at \
     most two slots run, and queued and servicing entries fill the 64" (fun () ->
      let clock = Vclock.create () in
      let fake = Fake.create clock in
      let d = make_dispatcher clock fake in
      let entries = [ Some 1; None; None; Some 2; None; Some 3; Some 4; None ] in
      let* () = Lwt_list.iter_s (fun e -> Lwt.map ignore (submit d e)) entries in
      check_stats "8 admitted" d ~outstanding:8 ~queued:6 ~running:2;
      let* () = Lwt_list.iter_s (fun _ -> Lwt.map ignore (submit d None)) (List.init 56 Fun.id) in
      check_stats "full" d ~outstanding:64 ~queued:62 ~running:2;
      let* r = submit d (Some 99) in
      Alcotest.(check bool) "queued entries fill capacity" true (is_refused r);
      let events = trace clock d 10_000.0 in
      Alcotest.(check (list (pair int (float 0.0)))) "real entries start in queue order, one slot pair per 15 s"
        [ (1, 0.0); (2, 15.0); (3, 30.0); (4, 45.0) ] fake.Fake.started;
      Alcotest.(check int) "one release per entry" 64 (List.length events);
      Alcotest.check schedule "the first deadline releases two slots, each refilled"
        [ (15.0, (63, 61, 2)); (15.0, (62, 60, 2)) ]
        (List.filteri (fun i _ -> i < 2) events);
      Alcotest.check schedule "32 deadlines drain it"
        [ (480.0, (1, 0, 1)); (480.0, (0, 0, 0)) ]
        (List.filteri (fun i _ -> i >= 62) events);
      Alcotest.(check (list int)) "every stalled attempt was cancelled at its deadline"
        [ 1; 2; 3; 4 ] fake.Fake.cancelled;
      Lwt.return_unit)

(* The core invariant: nothing private changes the schedule. Same arrivals,
   different entry kinds and different provider behaviour — the occupancy
   schedule must be identical, entry for entry. *)
let schedule_independence_case =
  pure_case
    "schedule: occupancy over time is identical whatever the entries' kinds \
     and whatever the provider does" (fun () ->
      let kinds =
        [ ("all no-send", List.init 12 (fun _ -> false));
          ("all real", List.init 12 (fun _ -> true));
          ("alternating", List.init 12 (fun i -> i mod 2 = 0));
          ("mixed", [ true; false; false; true; true; false; true; false; false; false; true; true ]) ]
      in
      let behaviours =
        [ Stall; Fast_ok; Fast_error; Sync_raise; Async_raise; After (7.0, Ok ());
          After (3.0, Error "http_500"); After (15.0, Ok ()); After (20.0, Ok ()) ]
      in
      let run kinds behaviour =
        let clock = Vclock.create () in
        let calls = ref 0 in
        let d =
          D.create ~sleep:(Vclock.sleep clock) ~label:(fun _ -> "test")
            ~transport:(behaviour_transport clock ~record:(fun _ -> incr calls) behaviour) ()
        in
        let first, later = List.filteri (fun i _ -> i < 9) kinds, List.filteri (fun i _ -> i >= 9) kinds in
        let* () = Lwt_list.iter_s (fun real -> Lwt.map ignore (submit d (if real then Some () else None))) first in
        (* More arrivals mid-schedule, between two deadlines. *)
        let early = trace clock d 20.0 in
        let* () = Lwt_list.iter_s (fun real -> Lwt.map ignore (submit d (if real then Some () else None))) later in
        let rest = trace clock d 10_000.0 in
        Lwt.return (early @ rest, !calls)
      in
      let* reference, _ = run (snd (List.hd kinds)) Stall in
      (* 9 entries at t=0, 3 more at t=20: two per deadline, nothing sooner. *)
      Alcotest.check schedule "reference schedule"
        [ (15.0, (8, 6, 2)); (15.0, (7, 5, 2)); (30.0, (9, 7, 2)); (30.0, (8, 6, 2));
          (45.0, (7, 5, 2)); (45.0, (6, 4, 2)); (60.0, (5, 3, 2)); (60.0, (4, 2, 2));
          (75.0, (3, 1, 2)); (75.0, (2, 0, 2)); (90.0, (1, 0, 1)); (90.0, (0, 0, 0)) ]
        reference;
      Lwt_list.iter_s
        (fun (kname, ks) ->
          Lwt_list.iter_s
            (fun b ->
              let* s, calls = run ks b in
              Alcotest.check schedule (kname ^ " / " ^ behaviour_name b) reference s;
              Alcotest.(check int) (kname ^ " / " ^ behaviour_name b ^ ": transport calls = real entries")
                (List.length (List.filter Fun.id ks)) calls;
              Lwt.return_unit)
            behaviours)
        kinds)

type secret_job = { id : int; secret : Bytes.t }

let early_completion_case =
  pure_case
    "early completion: success, failure or a raise does not release the slot \
     before its deadline, and the message is not kept meanwhile" (fun () ->
      Lwt_list.iter_s
        (fun behaviour ->
          let label = behaviour_name behaviour in
          let clock = Vclock.create () in
          let d =
            D.create ~sleep:(Vclock.sleep clock) ~label:(fun _ -> "test")
              ~transport:(behaviour_transport clock ~record:ignore behaviour) ()
          in
          let collected = ref false in
          let* _ =
            D.admit d (fun () ->
                let job = { id = 1; secret = Bytes.make 64 's' } in
                Gc.finalise (fun _ -> collected := true) job;
                Lwt.return ((), Some job))
          in
          (* Past any early completion, before the deadline. *)
          Vclock.advance_to clock 7.5;
          Gc.full_major ();
          Gc.full_major ();
          check_stats (label ^ ": slot still held") d ~outstanding:1 ~queued:0 ~running:1;
          if behaviour <> Stall then
            Alcotest.(check bool) (label ^ ": message released once the attempt settled") true !collected;
          let* r = submit d None in
          Alcotest.(check bool) (label ^ ": admission unaffected") false (is_refused r);
          check_stats (label ^ ": the free slot takes it") d ~outstanding:2 ~queued:0 ~running:2;
          Vclock.advance_to clock 14.999;
          check_stats (label ^ ": just before the deadline") d ~outstanding:2 ~queued:0 ~running:2;
          Vclock.advance_to clock 15.0;
          check_stats (label ^ ": at the deadline") d ~outstanding:1 ~queued:0 ~running:1;
          Gc.full_major ();
          Gc.full_major ();
          Alcotest.(check bool) (label ^ ": message released after the deadline") true !collected;
          Vclock.advance_to clock 30.0;
          check_stats (label ^ ": drained") d ~outstanding:0 ~queued:0 ~running:0;
          Lwt.return_unit)
        [ Stall; Fast_ok; Fast_error; Sync_raise; Async_raise; After (7.0, Ok ()) ])

let deadline_case =
  pure_case
    "deadline: a still-running attempt is cancelled exactly at the slot \
     deadline, the next entry then starts, and a late or ignored outcome \
     cannot release twice" (fun () ->
      let clock = Vclock.create () in
      let fake = Fake.create clock in
      let d = make_dispatcher clock fake in
      let* () = Lwt_list.iter_s (fun j -> Lwt.map ignore (submit d (Some j))) [ 1; 2; 3 ] in
      check_stats "2 in service" d ~outstanding:3 ~queued:1 ~running:2;
      Vclock.advance_to clock 14.999;
      Alcotest.(check (list int)) "nothing cancelled before the deadline" [] fake.Fake.cancelled;
      Vclock.advance_to clock 15.0;
      Alcotest.(check (list int)) "both hung attempts cancelled at the deadline" [ 1; 2 ]
        (List.sort compare fake.Fake.cancelled);
      Alcotest.(check (list (pair int (float 0.0)))) "queued entry then started"
        [ (1, 0.0); (2, 0.0); (3, 15.0) ] fake.Fake.started;
      check_stats "one left" d ~outstanding:1 ~queued:0 ~running:1;
      Fake.resolve fake 3 (Ok ());
      check_stats "success keeps the slot" d ~outstanding:1 ~queued:0 ~running:1;
      Vclock.advance_to clock 30.0;
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Alcotest.(check (list int)) "a settled attempt is not cancelled" [ 1; 2 ]
        (List.sort compare fake.Fake.cancelled);
      (* A transport that ignores cancellation still cannot hold a slot past
         the deadline, and its late answer changes nothing. *)
      let late = ref None in
      let stuck =
        D.create ~sleep:(Vclock.sleep clock) ~label:(fun _ -> "test")
          ~transport:(fun _ ->
            let p, u = Lwt.wait () in
            late := Some u;
            p)
          ()
      in
      let* _ = submit stuck (Some 1) in
      check_stats "stuck in service" stuck ~outstanding:1 ~queued:0 ~running:1;
      Vclock.advance_to clock 45.0;
      check_stats "stuck freed at its deadline" stuck ~outstanding:0 ~queued:0 ~running:0;
      Option.iter (fun u -> Lwt.wakeup u (Ok ())) !late;
      check_stats "late answer changes nothing" stuck ~outstanding:0 ~queued:0 ~running:0;
      let* r = submit stuck None in
      Alcotest.(check bool) "later admissions unaffected" false (is_refused r);
      check_stats "one new entry" stuck ~outstanding:1 ~queued:0 ~running:1;
      Lwt.return_unit)

let admission_cleanup_case =
  pure_case
    "admission: work that raises, rejects or is cancelled propagates its \
     exception and settles into a no-send entry that holds a full slot" (fun () ->
      let clock = Vclock.create () in
      let fake = Fake.create clock in
      let d = make_dispatcher clock fake in
      let* r =
        Lwt.catch
          (fun () -> Lwt.map (fun _ -> "returned") (D.admit d (fun () -> failwith "sync work failure")))
          (function Failure _ -> Lwt.return "raised" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "sync raise propagates" "raised" r;
      check_stats "held after a raise" d ~outstanding:1 ~queued:0 ~running:1;
      let* r =
        Lwt.catch
          (fun () -> Lwt.map (fun _ -> "returned") (D.admit d (fun () -> Lwt.fail (Failure "x"))))
          (function Failure _ -> Lwt.return "raised" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "rejection propagates" "raised" r;
      check_stats "held after a rejection" d ~outstanding:2 ~queued:0 ~running:2;
      let work, _ = Lwt.task () in
      let admitted = D.admit d (fun () -> work) in
      check_stats "reservation while working" d ~outstanding:3 ~queued:0 ~running:2;
      Lwt.cancel admitted;
      let* r =
        Lwt.catch (fun () -> Lwt.map (fun _ -> "returned") admitted)
          (function Lwt.Canceled -> Lwt.return "cancelled" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "cancellation propagates" "cancelled" r;
      check_stats "cancelled work queued as no-send" d ~outstanding:3 ~queued:1 ~running:2;
      Vclock.advance_to clock 14.999;
      check_stats "before the deadline" d ~outstanding:3 ~queued:1 ~running:2;
      Vclock.advance_to clock 15.0;
      check_stats "at the deadline" d ~outstanding:1 ~queued:0 ~running:1;
      Vclock.advance_to clock 30.0;
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Alcotest.(check (list int)) "nothing delivered" [] (Fake.started_jobs fake);
      Lwt.return_unit)

let job_after_work_case =
  pure_case
    "ordering: no delivery starts before the admitted work has returned its \
     job, and admission answers at settlement without waiting for the slot \
     or the provider" (fun () ->
      let clock = Vclock.create () in
      let fake = Fake.create clock in
      let d = make_dispatcher clock fake in
      let committed, commit = Lwt.wait () in
      let r = D.admit d (fun () -> Lwt.map (fun () -> ((), Some 7)) committed) in
      let* () = settle 5 in
      Alcotest.(check (list int)) "nothing started while work runs" [] (Fake.started_jobs fake);
      Lwt.wakeup commit ();
      Alcotest.(check bool) "admission answered" true (Lwt.state r = Lwt.Return (`Admitted ()));
      Alcotest.(check (list int)) "started after" [ 7 ] (Fake.started_jobs fake);
      check_stats "provider still busy, slot held" d ~outstanding:1 ~queued:0 ~running:1;
      Fake.resolve fake 7 (Ok ());
      Vclock.advance_to clock 15.0;
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

let degenerate_timer_case =
  pure_case
    "a slot timer that is already over or fails ends its slot without \
     starting a delivery or drifting the counters" (fun () ->
      Lwt_list.iter_s
        (fun (label, sleep) ->
          let calls = ref 0 in
          let d =
            D.create ~sleep ~label:(fun _ -> "test")
              ~transport:(fun _ ->
                incr calls;
                Lwt.return (Ok ()))
              ()
          in
          let* () = Lwt_list.iter_s (fun e -> Lwt.map ignore (submit d e)) [ Some 1; None; Some 2 ] in
          check_stats label d ~outstanding:0 ~queued:0 ~running:0;
          Alcotest.(check int) (label ^ ": no delivery outside a slot") 0 !calls;
          Lwt.return_unit)
        [ ("resolved timer", fun _ -> Lwt.return_unit); ("failed timer", fun _ -> Lwt.fail Exit) ])

let monotonic_sleep_case =
  pure_case "monotonic sleep waits at least the requested time" (fun () ->
      let t0 = Unix.gettimeofday () in
      let* () = D.monotonic_sleep 0.05 in
      let elapsed = Unix.gettimeofday () -. t0 in
      if elapsed < 0.045 then Alcotest.failf "woke after %.3fs" elapsed;
      let* () = D.monotonic_sleep 0.0 in
      let* () = D.monotonic_sleep (-1.0) in
      Lwt.return_unit)

let dispatcher_suite =
  [ default_bounds_case; capacity_case; fifo_case; schedule_independence_case;
    early_completion_case; deadline_case; admission_cleanup_case; job_after_work_case;
    degenerate_timer_case; monotonic_sleep_case ]

(* ------------------------------------------------------------------ *)
(* Resolver gate                                                         *)
(* ------------------------------------------------------------------ *)

let resolver_gate_case =
  pure_case
    "resolver gate: two physical lookups at most, no waiting queue, a \
     cancelled caller keeps the permit until the lookup ends, each permit \
     released once" (fun () ->
      Alcotest.check_raises "permits must be positive"
        (Invalid_argument "Auth_mail_resolver.create: permits must be positive")
        (fun () -> ignore (R.create ~permits:0 ~resolve:(fun _ -> Lwt.return ()) : unit R.t));
      Alcotest.(check int) "default permits" 2 R.default_permits;
      (* Physical operations: plain wakeable promises that, like a thread in
         getaddrinfo, ignore cancellation. *)
      let ops = ref [] and calls = ref 0 in
      let physical _ =
        incr calls;
        let p, u = Lwt.wait () in
        ops := !ops @ [ (p, u) ];
        p
      in
      let gate = R.create ~permits:2 ~resolve:physical in
      let uri = Uri.of_string "https://b2-gate.invalid/" in
      let a = R.resolve gate uri in
      let b = R.resolve gate uri in
      Alcotest.(check int) "two outstanding" 2 (R.outstanding gate);
      let* c = R.resolve gate uri in
      Alcotest.(check bool) "third is busy at once" true (c = Error `Busy);
      Alcotest.(check int) "busy started nothing" 2 !calls;
      Lwt.cancel a;
      Alcotest.(check bool) "caller view cancelled" true (Lwt.state a = Lwt.Fail Lwt.Canceled);
      let pa, ua = List.nth !ops 0 in
      Alcotest.(check bool) "physical lookup still running" true (Lwt.is_sleeping pa);
      Alcotest.(check int) "cancelled caller keeps its permit" 2 (R.outstanding gate);
      let* c = R.resolve gate uri in
      Alcotest.(check bool) "still busy" true (c = Error `Busy);
      Lwt.wakeup ua 10;
      Alcotest.(check int) "physical end releases the permit" 1 (R.outstanding gate);
      Alcotest.(check bool) "abandoned caller never sees the answer" true
        (Lwt.state a = Lwt.Fail Lwt.Canceled);
      let _, ub = List.nth !ops 1 in
      Lwt.wakeup_exn ub (Failure "lookup failed");
      Alcotest.(check int) "failure releases too" 0 (R.outstanding gate);
      let* rb = Lwt.catch (fun () -> Lwt.map (fun _ -> "value") b) (fun _ -> Lwt.return "failed") in
      Alcotest.(check string) "failure reaches the caller" "failed" rb;
      let raising = R.create ~permits:2 ~resolve:(fun _ -> failwith "sync lookup failure") in
      let* r = Lwt.catch (fun () -> Lwt.map (fun _ -> "value") (R.resolve raising uri)) (fun _ -> Lwt.return "failed") in
      Alcotest.(check string) "a synchronous raise fails the caller" "failed" r;
      Alcotest.(check int) "and releases its permit" 0 (R.outstanding raising);
      let d = R.resolve gate uri in
      let _, ud = List.nth !ops 2 in
      Lwt.wakeup ud 42;
      let* rd = d in
      Alcotest.(check bool) "recovers" true (rd = Ok 42);
      Alcotest.(check int) "nothing left" 0 (R.outstanding gate);
      Lwt.return_unit)

let resolver_suite = [ resolver_gate_case ]

(* ------------------------------------------------------------------ *)
(* Email messages and the real transport against local servers          *)
(* ------------------------------------------------------------------ *)

let token_of_message m =
  let link = Earde.Email.link m in
  match Uri.get_query_param (Uri.of_string link) "token" with
  | Some t -> t
  | None -> Alcotest.fail "message link carries no token"

let message_case =
  pure_case "messages: fixed kind labels, per-kind link paths, recipient kept" (fun () ->
      let s = Earde.Email.pending_signup_confirmation ~to_email:"a@b2.invalid" ~token:"tokA" in
      let r = Earde.Email.password_reset ~to_email:"b@b2.invalid" ~token:"tokB" in
      let v = Earde.Email.verification ~to_email:"c@b2.invalid" ~token:"tokC" in
      Alcotest.(check (list string)) "labels"
        [ "signup_confirmation"; "password_reset"; "verification" ]
        (List.map Earde.Email.label [ s; r; v ]);
      Alcotest.(check (list string)) "paths"
        [ "/confirm-email"; "/reset-password"; "/verify" ]
        (List.map (fun m -> Uri.path (Uri.of_string (Earde.Email.link m))) [ s; r; v ]);
      Alcotest.(check string) "token" "tokB" (token_of_message r);
      Alcotest.(check string) "recipient" "a@b2.invalid" (Earde.Email.recipient s);
      Lwt.return_unit)

(* Reads one HTTP request: headers up to the blank line, then
   Content-Length bytes of body. *)
let read_http_request fd =
  let buf = Bytes.create 4096 in
  let rec go acc =
    let* n = Lwt_unix.read fd buf 0 (Bytes.length buf) in
    let acc = acc ^ Bytes.sub_string buf 0 n in
    if n = 0 then Lwt.return acc
    else
      let rec find i =
        if i + 3 >= String.length acc then None
        else if String.sub acc i 4 = "\r\n\r\n" then Some (i + 4)
        else find (i + 1)
      in
      match find 0 with
      | None -> go acc
      | Some body_start ->
          let lower = String.lowercase_ascii acc in
          let key = "content-length:" in
          let len =
            let rec scan i =
              if i + String.length key > String.length lower then 0
              else if String.sub lower i (String.length key) = key then
                let j = ref (i + String.length key) in
                while !j < String.length lower && lower.[!j] = ' ' do incr j done;
                let k = ref !j in
                while !k < String.length lower && lower.[!k] >= '0' && lower.[!k] <= '9' do incr k done;
                int_of_string (String.sub lower !j (!k - !j))
              else scan (i + 1)
            in
            scan 0
          in
          if String.length acc - body_start >= len then Lwt.return acc else go acc
  in
  go ""

let loopback_listener () =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen sock 16;
  let port =
    match Lwt_unix.getsockname sock with Unix.ADDR_INET (_, p) -> p | _ -> assert false
  in
  Lwt.return (sock, port)

(* A loopback HTTP endpoint the test fully controls. [respond] decides what
   happens after the complete request has been read. *)
let with_local_server ~respond f =
  let* sock, port = loopback_listener () in
  let received, got_request = Lwt.wait () in
  let served =
    let* fd, _ = Lwt_unix.accept sock in
    let* request = read_http_request fd in
    Lwt.wakeup_later got_request request;
    respond fd
  in
  Lwt.finalize
    (fun () -> f ~port ~received ~served)
    (fun () ->
      Lwt.cancel served;
      Lwt_unix.close sock)

(* After the request: answer nothing, and report whether (and when) the
   client closes its end. *)
let stall_until_client_closes fd =
  let buf = Bytes.create 64 in
  let rec wait () =
    let* n = Lwt.catch (fun () -> Lwt_unix.read fd buf 0 64) (fun _ -> Lwt.return 0) in
    if n = 0 then Lwt.return `Client_closed else wait ()
  in
  Lwt.finalize wait (fun () -> Lwt_unix.close fd)

let respond_with status fd =
  let body = "{}" in
  let reply =
    Printf.sprintf "HTTP/1.1 %d X\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      status (String.length body) body
  in
  let* _ = Lwt_unix.write_string fd reply 0 (String.length reply) in
  let* () = Lwt_unix.close fd in
  Lwt.return `Answered

(* Accepts any number of connections, counts them and the complete requests
   they carry, and answers each with 201. *)
let with_counting_server f =
  let* sock, port = loopback_listener () in
  let connections = ref 0 and requests = ref 0 in
  let rec accept_loop () =
    let* fd, _ = Lwt_unix.accept sock in
    incr connections;
    Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let* _ = read_http_request fd in
            incr requests;
            Lwt.map ignore (respond_with 201 fd))
          (fun _ -> Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)));
    accept_loop ()
  in
  let loop = accept_loop () in
  Lwt.finalize
    (fun () -> f ~port ~connections ~requests)
    (fun () ->
      Lwt.cancel loop;
      Lwt_unix.close sock)

let fake_key = "b2-fake-provider-key-000"

let clocked_real_dispatcher ?resolver clock ~endpoint =
  D.create ~sleep:(Vclock.sleep clock) ~label:Earde.Email.label
    ~transport:(Earde.Email.deliver_via ?resolver ~endpoint ~api_key:fake_key) ()

let safety_net seconds p =
  Lwt.pick [ Lwt.map (fun r -> Some r) p; Lwt.map (fun () -> None) (Lwt_unix.sleep seconds) ]

let stalled_provider_case =
  pure_case
    "transport: a provider that never answers is cancelled at the slot \
     deadline and the client socket is actually closed" (fun () ->
      with_local_server ~respond:stall_until_client_closes (fun ~port ~received ~served ->
          let endpoint = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/v3/smtp/email" port) in
          let clock = Vclock.create () in
          let d = clocked_real_dispatcher clock ~endpoint in
          let m = Earde.Email.pending_signup_confirmation ~to_email:"stall@b2.invalid" ~token:"stalltoken" in
          let* _ = D.admit d (fun () -> Lwt.return ((), Some m)) in
          (* The server holds the full request: the attempt is provably
             mid-flight when the deadline comes. *)
          let* request = received in
          must "provider saw the request" request "POST /v3/smtp/email";
          check_stats "in service" d ~outstanding:1 ~queued:0 ~running:1;
          Vclock.advance_to clock 15.0;
          check_stats "released at the deadline" d ~outstanding:0 ~queued:0 ~running:0;
          (* Bounded only as a safety net: the socket closes immediately on
             cancellation; a leaked connection would stay open forever. *)
          let* closed = safety_net 10.0 served in
          Alcotest.(check bool) "client closed its connection" true (closed = Some `Client_closed);
          Lwt.return_unit))

(* The server accepts TCP and never answers the ClientHello: the deadline
   must cancel the pending TLS connect and close the socket. *)
let tls_pending_case =
  pure_case "transport: the slot deadline during a stalled TLS handshake closes the socket" (fun () ->
      let* sock, port = loopback_listener () in
      let hello, got_hello = Lwt.wait () in
      let served =
        let* fd, _ = Lwt_unix.accept sock in
        let buf = Bytes.create 4096 in
        let rec drain total =
          let* n = Lwt.catch (fun () -> Lwt_unix.read fd buf 0 4096) (fun _ -> Lwt.return 0) in
          if n > 0 && Lwt.is_sleeping hello then Lwt.wakeup_later got_hello ();
          if n = 0 then Lwt.return total else drain (total + n)
        in
        Lwt.finalize (fun () -> drain 0) (fun () -> Lwt_unix.close fd)
      in
      let endpoint = Uri.of_string (Printf.sprintf "https://localhost:%d/v3/smtp/email" port) in
      let clock = Vclock.create () in
      let d = clocked_real_dispatcher clock ~endpoint in
      let m = Earde.Email.password_reset ~to_email:"tls@b2.invalid" ~token:"tlstoken" in
      let* _ = D.admit d (fun () -> Lwt.return ((), Some m)) in
      (* The handshake is provably pending: the server holds the ClientHello. *)
      let* got = safety_net 10.0 hello in
      if got = None then Alcotest.fail "no ClientHello reached the server";
      check_stats "in service" d ~outstanding:1 ~queued:0 ~running:1;
      Vclock.advance_to clock 15.0;
      let* closed = safety_net 10.0 served in
      let* () = Lwt_unix.close sock in
      (match closed with
       | Some n -> if n = 0 then Alcotest.fail "no ClientHello was sent"
       | None -> Alcotest.fail "socket still open after the deadline");
      check_stats "released" d ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

let provider_status_case =
  pure_case
    "transport: 2xx is delivered, non-2xx is a bounded failure class, and no \
     diagnostic names the recipient, token or key" (fun () ->
      with_captured_logs (fun logs ->
          let m = Earde.Email.password_reset ~to_email:"status@b2.invalid" ~token:"statustoken" in
          let* ok =
            with_local_server ~respond:(respond_with 201) (fun ~port ~received:_ ~served:_ ->
                Earde.Email.deliver_via
                  ~endpoint:(Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/x" port))
                  ~api_key:fake_key m)
          in
          Alcotest.(check bool) "201 delivered" true (ok = Ok ());
          (* Through the dispatcher, so its failure log line is exercised. *)
          let* () =
            with_local_server ~respond:(respond_with 502) (fun ~port ~received:_ ~served ->
                let endpoint = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/x" port) in
                let clock = Vclock.create () in
                let d = clocked_real_dispatcher clock ~endpoint in
                let* r = D.admit d (fun () -> Lwt.return ((), Some m)) in
                Alcotest.(check bool) "admitted" true (r = `Admitted ());
                let* _ = served in
                let* () = eventually "failure logged" (fun () -> contains (logs ()) "delivery failed") in
                check_stats "slot still held after the failure" d ~outstanding:1 ~queued:0 ~running:1;
                Vclock.advance_to clock 15.0;
                check_stats "released" d ~outstanding:0 ~queued:0 ~running:0;
                Lwt.return_unit)
          in
          let text = logs () in
          must "failure logged by kind and class" text "auth mail password_reset: delivery failed (http_502)";
          must_not "no recipient" text "status@b2.invalid";
          must_not "no token" text "statustoken";
          must_not "no key" text fake_key;
          Lwt.return_unit))

(* Nothing listens on the port: the attempt must fail on its own, promptly,
   releasing its transport resources, while its slot still runs to the
   deadline. *)
let refused_provider_case =
  pure_case
    "transport: a refused connection fails at once, releasing its transport \
     resources, while the scheduling slot runs to its deadline" (fun () ->
      let probe = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let* () = Lwt_unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
      let port =
        match Lwt_unix.getsockname probe with Unix.ADDR_INET (_, p) -> p | _ -> assert false
      in
      let* () = Lwt_unix.close probe in
      let endpoint = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/x" port) in
      let outcome = ref None in
      let clock = Vclock.create () in
      let d =
        D.create ~sleep:(Vclock.sleep clock) ~label:Earde.Email.label
          ~transport:(fun m ->
            let p = Earde.Email.deliver_via ~endpoint ~api_key:fake_key m in
            Lwt.on_success p (fun r -> outcome := Some r);
            p)
          ()
      in
      let m = Earde.Email.password_reset ~to_email:"refused@b2.invalid" ~token:"refusedtoken" in
      let* _ = D.admit d (fun () -> Lwt.return ((), Some m)) in
      let* () = eventually "refused attempt settled" (fun () -> !outcome <> None) in
      Alcotest.(check bool) "failure class" true (!outcome = Some (Error "transport_error"));
      check_stats "slot held after the prompt failure" d ~outstanding:1 ~queued:0 ~running:1;
      Vclock.advance_to clock 15.0;
      check_stats "released at the deadline" d ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

(* An uninterruptible resolver (the physical lookup ignores cancellation,
   like a thread blocked in getaddrinfo) under twelve deliveries whose
   deadlines pass: before the gate, every abandoned lookup kept running
   with its request continuation (payload, recipient, token, key) and the
   next attempt started another one. *)
let resolver_bound_case =
  pure_case
    "transport: abandoned lookups keep their permits, at most two exist at \
     once, no payload outlives its attempt, nothing connects late, and \
     delivery recovers" (fun () ->
      with_captured_logs (fun logs ->
          with_counting_server (fun ~port ~connections ~requests ->
              let loopback = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/v3/smtp/email" port) in
              let* loop_endp =
                Cohttp_lwt_unix.Net.resolve ~ctx:(Lazy.force Cohttp_lwt_unix.Net.default_ctx) loopback
              in
              let active = ref 0 and peak = ref 0 and started = ref 0 in
              let blocked = ref [] in
              let mode = ref `Block in
              let physical _ =
                incr started;
                incr active;
                peak := max !peak !active;
                let p =
                  match !mode with
                  | `Block ->
                      let p, u = Lwt.wait () in
                      blocked := u :: !blocked;
                      p
                  | `Fail -> Lwt.fail (Failure "b2 lookup failure")
                  | `Answer -> Lwt.return loop_endp
                in
                Lwt.on_termination p (fun () -> decr active);
                p
              in
              let gate = R.create ~permits:R.default_permits ~resolve:physical in
              (* [physical] answers every lookup: this name never reaches DNS. *)
              let endpoint = Uri.of_string (Printf.sprintf "http://b2-hang.invalid:%d/v3/smtp/email" port) in
              let clock = Vclock.create () in
              let d = clocked_real_dispatcher ~resolver:gate clock ~endpoint in
              (* A large token makes a retained payload unmistakable. *)
              let token i = String.make (256 * 1024) (Char.chr (97 + i)) in
              let* () =
                Lwt_list.iter_s
                  (fun i ->
                    Lwt.map ignore
                      (D.admit d (fun () ->
                           Lwt.return
                             ((), Some (Earde.Email.password_reset
                                          ~to_email:(Printf.sprintf "dns%d@b2.invalid" i)
                                          ~token:(token i))))))
                  (List.init 12 Fun.id)
              in
              let rec rounds k =
                if k > 6 then Lwt.return_unit
                else begin
                  Vclock.advance_to clock (15.0 *. float_of_int k);
                  let* () = settle 10 in
                  if !active > 2 || R.outstanding gate > 2 then
                    Alcotest.failf "round %d: %d lookups outstanding" k !active;
                  rounds (k + 1)
                end
              in
              let* () = rounds 1 in
              check_stats "scheduler drained while lookups hang" d ~outstanding:0 ~queued:0 ~running:0;
              Alcotest.(check int) "peak outstanding lookups" 2 !peak;
              Alcotest.(check int) "only two lookups ever started for twelve attempts" 2 !started;
              Alcotest.(check int) "both abandoned lookups still hold their permits" 2 (R.outstanding gate);
              Alcotest.(check int) "later attempts failed at once as busy" 10
                (count_occurrences (logs ()) "delivery failed (resolver_busy)");
              Alcotest.(check int) "the first two timed out" 2
                (count_occurrences (logs ()) "delivery timed out");
              (* What the hanging lookups keep alive: nothing but the gate's
                 counter callback, whereas one payload alone is ~64k words. *)
              Gc.full_major ();
              let retained = Obj.reachable_words (Obj.repr !blocked) in
              if retained > 16_384 then
                Alcotest.failf "hanging lookups retain %d words (a payload is retained)" retained;
              (* The lookups finish: no abandoned attempt may connect now. *)
              List.iter (fun u -> Lwt.wakeup u loop_endp) !blocked;
              blocked := [];
              let* () = Lwt_unix.sleep 0.3 in
              Alcotest.(check int) "no late connection" 0 !connections;
              Alcotest.(check int) "permits back" 0 (R.outstanding gate);
              (* Recovery. *)
              mode := `Answer;
              let* _ =
                D.admit d (fun () ->
                    Lwt.return ((), Some (Earde.Email.password_reset ~to_email:"again@b2.invalid" ~token:"againtoken")))
              in
              let* () = eventually "recovered delivery reached the provider" (fun () -> !requests = 1) in
              Alcotest.(check int) "one connection" 1 !connections;
              (* A failing lookup fails its attempt promptly and returns its permit. *)
              mode := `Fail;
              let* _ =
                D.admit d (fun () ->
                    Lwt.return ((), Some (Earde.Email.password_reset ~to_email:"fail@b2.invalid" ~token:"failtoken")))
              in
              let* () =
                eventually "lookup failure reported" (fun () ->
                    count_occurrences (logs ()) "delivery failed (transport_error)" = 1)
              in
              Alcotest.(check int) "failed lookup returned its permit" 0 (R.outstanding gate);
              Vclock.advance_to clock 1_000.0;
              check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
              let text = logs () in
              must_not "no recipient in diagnostics" text "dns3@b2.invalid";
              must_not "no key in diagnostics" text fake_key;
              Lwt.return_unit)))

let transport_suite =
  [ message_case; stalled_provider_case; tls_pending_case; provider_status_case;
    refused_provider_case; resolver_bound_case ]

(* ------------------------------------------------------------------ *)
(* Login verification contract                                          *)
(* ------------------------------------------------------------------ *)

let dummy_fixture_case =
  pure_case
    "dummy hash: a valid Argon2id encoding with the production cost that \
     verifies its public password with full work" (fun () ->
      let expected_prefix =
        Printf.sprintf "$argon2id$v=19$m=%d,t=%d,p=%d$" Earde.Auth.m_cost Earde.Auth.t_cost
          Earde.Auth.parallelism
      in
      Alcotest.(check string) "production parameters" expected_prefix
        (String.sub LV.dummy_hash 0 (String.length expected_prefix));
      Alcotest.(check (list int)) "production cost is unchanged" [ 65536; 2; 1 ]
        [ Earde.Auth.m_cost; Earde.Auth.t_cost; Earde.Auth.parallelism ];
      (* A malformed dummy would fail instantly instead of doing the work:
         only a true positive proves the encoding is complete. *)
      Alcotest.(check bool) "fixture verifies its password" true
        (Argon2.verify ~pwd:LV.dummy_password ~encoded:LV.dummy_hash ~kind:Argon2.ID = Ok true);
      let* wrong = LV.argon2_verifier ~password:"not the dummy password" ~hash:LV.dummy_hash in
      Alcotest.(check bool) "other passwords do not" false wrong;
      let* garbage = LV.argon2_verifier ~password:"x" ~hash:"not-a-hash" in
      Alcotest.(check bool) "malformed hash is false" false garbage;
      Lwt.return_unit)

let counting () =
  let calls = ref [] in
  let verify ~answer ~password:_ ~hash =
    calls := hash :: !calls;
    Lwt.return answer
  in
  (calls, verify)

let authenticate_case =
  pure_case
    "authenticate: exactly one verification on every path; a missing account \
     never authenticates, even on a dummy match" (fun () ->
      let calls, verify = counting () in
      let* r = LV.authenticate ~verify:(verify ~answer:true) ~password:LV.dummy_password None in
      Alcotest.(check bool) "dummy match is not a login" true (r = None);
      Alcotest.(check (list string)) "missing: verified against the dummy" [ LV.dummy_hash ] !calls;
      calls := [];
      let* r = LV.authenticate ~verify:(verify ~answer:false) ~password:"p" None in
      Alcotest.(check bool) "missing: none" true (r = None);
      Alcotest.(check (list string)) "missing: one call" [ LV.dummy_hash ] !calls;
      calls := [];
      let* r = LV.authenticate ~verify:(verify ~answer:false) ~password:"p" (Some ("stored", 7)) in
      Alcotest.(check bool) "wrong password: none" true (r = None);
      Alcotest.(check (list string)) "wrong: own hash" [ "stored" ] !calls;
      calls := [];
      let* r = LV.authenticate ~verify:(verify ~answer:true) ~password:"p" (Some ("stored", 7)) in
      Alcotest.(check bool) "right password: the account" true (r = Some 7);
      Alcotest.(check (list string)) "right: own hash" [ "stored" ] !calls;
      let raising ~password:_ ~hash:_ = failwith "verifier crashed" in
      let* r = LV.authenticate ~verify:raising ~password:"p" (Some ("stored", 7)) in
      Alcotest.(check bool) "crashing verifier authenticates nobody" true (r = None);
      Lwt.return_unit)

let login_pure_suite = [ dummy_fixture_case; authenticate_case ]

(* ------------------------------------------------------------------ *)
(* Rate limiter decision, DB-free                                        *)
(* ------------------------------------------------------------------ *)

let limiter_app ~check ~cleanup ~hits ?(inner = fun _ -> incr hits; Dream.respond "b2-inner-ran") () =
  Dream.set_secret "b2-limiter-secret" @@ Dream.memory_sessions
  @@ H.Rate_limit.make_middleware ~check ~cleanup (fun req -> inner req)

let run_limited app ~target =
  let* response = app (Dream.request ~method_:`POST ~target "") in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

let no_cleanup _ = ()

let limiter_decision_case =
  pure_case
    "limiter: only Allowed invokes the handler; Error, raise and rejection \
     refuse with a generic 503" (fun () ->
      let seen_endpoint = ref "" in
      let check_with result request ~ip:_ ~endpoint =
        ignore request;
        seen_endpoint := endpoint;
        result ()
      in
      let cases =
        [ ("allowed", (fun () -> Lwt.return (Ok `Allowed)), 200, 1);
          ("blocked", (fun () -> Lwt.return (Ok `Blocked)), 200, 0);
          ("result error", (fun () -> Lwt.return (Error "relation rate_limits does not exist")), 503, 0);
          ("sync raise", (fun () -> failwith "pool exhausted"), 503, 0);
          ("rejected promise", (fun () -> Lwt.fail (Failure "connection refused")), 503, 0) ]
      in
      Lwt_list.iter_s
        (fun (label, result, status, expected_hits) ->
          let hits = ref 0 in
          let app = limiter_app ~check:(check_with result) ~cleanup:no_cleanup ~hits () in
          let* s, body = run_limited app ~target:"/login?token=b2-query-secret" in
          Alcotest.(check int) (label ^ ": status") status s;
          Alcotest.(check int) (label ^ ": handler invocations") expected_hits !hits;
          Alcotest.(check string) (label ^ ": bucket is the path only") "/login" !seen_endpoint;
          must_not (label ^ ": no query secret") body "b2-query-secret";
          if status = 503 then begin
            must label body "Temporarily unavailable";
            List.iter (must_not label body)
              [ "rate_limits"; "relation"; "pool exhausted"; "connection refused"; "Failure" ]
          end;
          if label = "blocked" then must label body "Too Many Attempts";
          Lwt.return_unit)
        cases)

let limiter_cleanup_case =
  pure_case
    "limiter: a failing cleanup never changes the decision in either direction" (fun () ->
      let raising_cleanup _ = failwith "cleanup exploded" in
      let async_failing_cleanup _ = Lwt.async (fun () -> Lwt.return_unit) in
      Lwt_list.iter_s
        (fun cleanup ->
          let hits = ref 0 in
          let allowed = limiter_app ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed)) ~cleanup ~hits () in
          let* s, _ = run_limited allowed ~target:"/login" in
          Alcotest.(check (pair int int)) "allowed still runs" (200, 1) (s, !hits);
          let hits = ref 0 in
          let failing = limiter_app ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Error "x")) ~cleanup ~hits () in
          let* s, _ = run_limited failing ~target:"/login" in
          Alcotest.(check (pair int int)) "error still refuses" (503, 0) (s, !hits);
          let hits = ref 0 in
          let blocked = limiter_app ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Blocked)) ~cleanup ~hits () in
          let* s, body = run_limited blocked ~target:"/login" in
          Alcotest.(check (pair int int)) "blocked still blocks" (200, 0) (s, !hits);
          must "blocked page" body "Too Many Attempts";
          Lwt.return_unit)
        [ raising_cleanup; async_failing_cleanup ])

let limiter_inner_exception_case =
  pure_case
    "limiter: an exception raised by the allowed handler is its own, not a \
     limiter outage" (fun () ->
      let hits = ref 0 in
      let app =
        limiter_app
          ~check:(fun _ ~ip:_ ~endpoint:_ -> Lwt.return (Ok `Allowed))
          ~cleanup:no_cleanup ~hits
          ~inner:(fun _ -> incr hits; failwith "b2 handler bug")
          ()
      in
      let* r =
        Lwt.catch
          (fun () -> Lwt.map (fun _ -> "responded") (run_limited app ~target:"/login"))
          (function Failure m when m = "b2 handler bug" -> Lwt.return "propagated" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "handler exception propagates" "propagated" r;
      Alcotest.(check int) "handler ran once" 1 !hits;
      Lwt.return_unit)

let limiter_pure_suite =
  [ limiter_decision_case; limiter_cleanup_case; limiter_inner_exception_case ]

(* ================================================================== *)
(* Gated suites                                                          *)
(* ================================================================== *)

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DROP SCHEMA IF EXISTS b2_shadow CASCADE";
      "DELETE FROM password_resets WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')";
      "DELETE FROM pending_signups WHERE email LIKE '%@b2.invalid' OR username LIKE 'b2x\\_%'";
      "DELETE FROM rate_limits WHERE endpoint LIKE '/b2-%'";
      "DELETE FROM users WHERE username LIKE 'b2x\\_%'" ]

let env_keys =
  [ "EARDE_SIGNUPS_ENABLED"; "TURNSTILE_SITE_KEY"; "TURNSTILE_SECRET_KEY"; "EARDE_TURNSTILE_REQUIRED" ]

(* Signup open, Turnstile disabled — exactly the dev configuration; every
   variable is emptied again afterwards (an empty value reads as unset). *)
let with_signup_env f =
  List.iter (fun k -> Unix.putenv k "") env_keys;
  Unix.putenv "EARDE_SIGNUPS_ENABLED" "1";
  Lwt.finalize f (fun () ->
      List.iter (fun k -> Unix.putenv k "") env_keys;
      Lwt.return_unit)

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   or_fail "cleanup" r)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> with_signup_env (fun () -> f ~url (module C : Caqti_lwt.CONNECTION)))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === fixtures === *)

let strip_nuls s = match String.index_opt s '\000' with Some i -> String.sub s 0 i | None -> s

let real_hash password =
  let* h = Earde.Auth.hash_password password in
  let* h = or_fail_s "hash" h in
  Lwt.return (strip_nuls h)

let q_user =
  (Caqti_type.(t3 string string string) ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $2, $3, TRUE) RETURNING id"

let q_ban = (Caqti_type.int ->. Caqti_type.unit) "UPDATE users SET is_banned = TRUE WHERE id = $1"

(* hours may be negative: an already-expired reservation. *)
let q_pending =
  (Caqti_type.(t4 string string string float) ->. Caqti_type.unit)
  "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
   VALUES ($1, $2, 'b2-fixture-not-a-hash', $3, NOW() + ($4 * INTERVAL '1 hour'))"

let q_pending_rows =
  (Caqti_type.unit ->* Caqti_type.(t3 string string string))
  "SELECT LOWER(username), LOWER(email), token_hash FROM pending_signups
    WHERE consumed_at IS NULL AND (email LIKE '%@b2.invalid' OR username LIKE 'b2x\\_%')
    ORDER BY 1, 2"

let q_users_snapshot =
  (Caqti_type.unit ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(id::text || '|' || username || '|' || email || '|' || password_hash
            || '|' || is_banned::text || '|' || is_email_verified::text, ';' ORDER BY id), '')
     FROM users WHERE username LIKE 'b2x\\_%'"

let q_user_email =
  (Caqti_type.string ->? Caqti_type.string) "SELECT email FROM users WHERE username = $1"

let q_reset_rows =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM password_resets
    WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')"

let q_reset_token_exists =
  (Caqti_type.string ->! Caqti_type.bool)
  "SELECT EXISTS (SELECT 1 FROM password_resets WHERE token = $1)"

let q_pending_token_exists =
  (Caqti_type.string ->! Caqti_type.bool)
  "SELECT EXISTS (SELECT 1 FROM pending_signups WHERE token_hash = $1 AND consumed_at IS NULL)"

let q_pending_json =
  (Caqti_type.unit ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(row_to_json(p)::text, ';'), '') FROM pending_signups p
    WHERE email LIKE '%@b2.invalid' OR username LIKE 'b2x\\_%'"

let q_reset_json =
  (Caqti_type.unit ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(row_to_json(r)::text, ';'), '') FROM password_resets r
    WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')"

let q_lock_waiters =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM pg_locks l JOIN pg_stat_activity a ON a.pid = l.pid
    WHERE NOT l.granted AND a.datname = current_database()"

let sha256 s = Digestif.SHA256.(digest_string s |> to_hex)

(* === mail doubles === *)

type fake_mail = {
  clock : Vclock.t;
  mutable started : Earde.Email.message list;
  mutable delivered : Earde.Email.message list;
  mutable gate : unit Lwt.t;
  mutable at_start : Earde.Email.message -> unit Lwt.t;
  mutable in_flight : int;
}

(* Virtual time stands still unless the test moves it, so a stuck delivery
   stays stuck (like one lost with a restarted process) and a slot ends only
   when [drained] or the test advances the clock. *)
let fake_dispatcher ?(gate = Lwt.return_unit) () =
  let fm =
    { clock = Vclock.create (); started = []; delivered = []; gate;
      at_start = (fun _ -> Lwt.return_unit); in_flight = 0 }
  in
  let transport m =
    fm.started <- fm.started @ [ m ];
    fm.in_flight <- fm.in_flight + 1;
    Lwt.finalize
      (fun () ->
        let* () = fm.at_start m in
        let* () = fm.gate in
        fm.delivered <- fm.delivered @ [ m ];
        Lwt.return (Ok ()))
      (fun () ->
        fm.in_flight <- fm.in_flight - 1;
        Lwt.return_unit)
  in
  let d =
    D.create ~sleep:(Vclock.sleep fm.clock) ~label:Earde.Email.label ~transport ()
  in
  (fm, d)

(* Runs the schedule to the end: lets every started delivery finish (it may
   query the database on another connection), then moves virtual time one
   slot forward, until nothing is left. Caqti connections are not
   shareable, so tests wait for this before touching their own again. *)
let drained fm d =
  let rec go n =
    let* () = eventually "deliveries finished" (fun () -> fm.in_flight = 0) in
    let s = D.stats d in
    if s.D.outstanding = 0 then Lwt.return_unit
    else if n > 1000 then Alcotest.fail "dispatcher never drained"
    else if s.D.running = 0 && s.D.queued = 0 then
      (* Only reservations: requests still doing their work. *)
      Lwt.bind (Lwt_unix.sleep 0.005) (fun () -> go (n + 1))
    else begin
      Vclock.advance_to fm.clock (fm.clock.Vclock.now +. D.default_config.D.timeout_seconds);
      go (n + 1)
    end
  in
  go 0

(* === the routed pipeline === *)

let current_mail : Earde.Email.message D.t option ref = ref None
let verified_hashes : string list ref = ref []

let counting_verifier ~password ~hash =
  verified_hashes := hash :: !verified_hashes;
  LV.argon2_verifier ~password ~hash

let mail () =
  match !current_mail with Some d -> d | None -> Alcotest.fail "no dispatcher installed"

let pipelines : (string, Dream.handler) Hashtbl.t = Hashtbl.create 4

let limited_hits = ref 0

(* The request session as the server holds it: its id and every field. *)
let session_state request =
  let fields =
    Dream.all_session_fields request |> List.sort compare
    |> List.map (fun (k, v) -> k ^ "=" ^ v)
  in
  String.concat ";" (("id=" ^ Dream.session_id request) :: fields)

let build_pipeline url =
  Dream.sql_pool ~size:4 url @@ Dream.set_secret "b2-test-secret-value" @@ Dream.memory_sessions
  @@ Dream.router
       [ (* Each browser session carries a random marker, so a later look at
            the same cookie shows whether the session survived unchanged. *)
         Dream.get "/token" (fun req ->
             let* () =
               Dream.set_session_field req "b2_marker" (Dream.to_base64url (Dream.random 9))
             in
             Dream.respond (Dream.csrf_token req));
         Dream.get "/b2-session" (fun req -> Dream.respond (session_state req));
         Dream.get "/whoami" (fun req ->
             match Dream.session_field req "user_id" with
             | Some uid -> Dream.respond ("uid:" ^ uid)
             | None -> Dream.respond "anon");
         Dream.post "/signup" (fun req -> H.make_signup_handler ~mail:(mail ()) req);
         Dream.post "/forgot-password" (fun req -> H.make_forgot_password_handler ~mail:(mail ()) req);
         Dream.post "/login" (H.make_login_handler ~verify:counting_verifier);
         Dream.post "/login-prod" H.login_handler;
         Dream.get "/confirm-email" H.confirm_email_handler;
         Dream.post "/reset-password" H.reset_password_handler;
         (* The production limiter in front of real handlers, at paths whose
            buckets this suite owns. *)
         Dream.post "/b2-rl/login"
           (H.Rate_limit.middleware (fun req ->
                incr limited_hits;
                H.make_login_handler ~verify:counting_verifier req));
         Dream.post "/b2-rl/forgot"
           (H.Rate_limit.middleware (fun req ->
                incr limited_hits;
                H.make_forgot_password_handler ~mail:(mail ()) req));
         Dream.post "/b2-rl/signup"
           (H.Rate_limit.middleware (fun req ->
                incr limited_hits;
                H.make_signup_handler ~mail:(mail ()) req)) ]

let pipeline url =
  match Hashtbl.find_opt pipelines url with
  | Some p -> p
  | None ->
      let p = build_pipeline url in
      Hashtbl.replace pipelines url p;
      p

let with_search_path url schema =
  Uri.to_string (Uri.add_query_param' (Uri.of_string url) ("options", "-csearch_path=" ^ schema))

type reply = {
  status : int;
  body : string;
  headers : (string * string) list;
  sent_cookie : string option;  (* the request's own session cookie *)
  at : float;  (* when the response was captured *)
  session_before : string;  (* [session_state] of the request's session, "" if unobserved *)
  session_after : string;  (* the same cookie looked up again after the response *)
}

let session_cookie response =
  match
    List.find_opt (fun v -> contains v "dream.session") (Dream.headers response "Set-Cookie")
  with
  | Some v -> (match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)
  | None -> Alcotest.fail "no session cookie"

let form_body fields =
  String.concat "&"
    (List.map (fun (k, v) -> Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v) fields)

let observe_session ~url cookie =
  let* r = (pipeline url) (Dream.request ~method_:`GET ~target:"/b2-session" ~headers:[ ("Cookie", cookie) ] "") in
  Dream.body r

(* Every POST comes from a fresh anonymous browser session with its own valid
   CSRF token, like a first visit to the form. The session is looked at
   before and after, through the same cookie. *)
let post ~url ~target fields =
  let p = pipeline url in
  let* minted = p (Dream.request ~method_:`GET ~target:"/token" "") in
  let cookie = session_cookie minted in
  let* token = Dream.body minted in
  let* session_before = observe_session ~url cookie in
  let request =
    Dream.request ~method_:`POST ~target
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded"); ("Cookie", cookie) ]
      ""
  in
  Dream.set_body request (form_body (("dream.csrf", token) :: fields));
  let* response = p request in
  let at = Unix.gettimeofday () in
  let* body = Dream.body response in
  let* session_after = observe_session ~url cookie in
  Lwt.return
    ( { status = Dream.status_to_int (Dream.status response); body;
        headers = Dream.all_headers response; sent_cookie = Some cookie; at; session_before;
        session_after },
      cookie )

let get ~url ?cookie target =
  let p = pipeline url in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = p (Dream.request ~method_:`GET ~target ~headers "") in
  let at = Unix.gettimeofday () in
  let* body = Dream.body response in
  Lwt.return
    { status = Dream.status_to_int (Dream.status response); body;
      headers = Dream.all_headers response; sent_cookie = cookie; at; session_before = "";
      session_after = "" }

(* === the response comparator === *)

(* CSRF values in bodies are per-session random by design. *)
let mask_csrf body =
  let key = "dream.csrf" in
  let b = Buffer.create (String.length body) in
  let n = String.length body in
  let rec go i =
    if i >= n then ()
    else if i + String.length key <= n && String.sub body i (String.length key) = key then begin
      Buffer.add_string b key;
      let j = i + String.length key in
      (* Mask the next value="..." or value='...' within a short window. *)
      let rec find k =
        if k + 7 > n || k > j + 80 then None
        else if String.sub body k 6 = "value=" && (body.[k + 6] = '"' || body.[k + 6] = '\'') then Some k
        else find (k + 1)
      in
      match find j with
      | None -> go j
      | Some k ->
          let q = body.[k + 6] in
          let close = try String.index_from body (k + 7) q with Not_found -> n - 1 in
          Buffer.add_string b (String.sub body j (k + 7 - j));
          Buffer.add_string b "MASKED";
          go close
    end
    else begin
      Buffer.add_char b body.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents b

(* Cookies whose values are random by design: Dream's signed session id.
   Only these values are masked, and only down to their session meaning
   (cleared, reused, rotated); every other cookie value is compared as is. *)
let random_cookie_names = [ "dream.session" ]

type cookie_view = {
  name : string;
  value : string;
  attributes : (string * string) list;  (* lower-cased names, in order *)
  expires : float option;  (* absolute time of a parseable Expires *)
  max_age : int option;  (* a parseable Max-Age, compared by meaning *)
}

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let weekdays = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

(* IMF-fixdate ("Wed, 21 Oct 2015 07:28:00 GMT") to Unix time, by the
   days-from-civil algorithm: independent of the local time zone. *)
let parse_http_date s =
  try
    Scanf.sscanf s "%_s@, %d %s %d %d:%d:%d GMT%!" (fun day mon year h mi se ->
        let rec index i = if months.(i) = mon then i + 1 else index (i + 1) in
        let m = index 0 in
        let y = if m <= 2 then year - 1 else year in
        let era = (if y >= 0 then y else y - 399) / 400 in
        let yoe = y - (era * 400) in
        let mp = (m + 9) mod 12 in
        let doy = ((153 * mp) + 2) / 5 + day - 1 in
        let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
        let days = (era * 146097) + doe - 719468 in
        Some (float_of_int ((days * 86400) + (h * 3600) + (mi * 60) + se)))
  with _ -> None

let http_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d GMT" weekdays.(tm.Unix.tm_wday) tm.Unix.tm_mday
    months.(tm.Unix.tm_mon) (1900 + tm.Unix.tm_year) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

(* One Set-Cookie header. Each cookie arrives as its own header, so an
   Expires date's comma never splits anything; attributes split on ';'. *)
let cookie_view ~sent_cookie raw =
  let parts = String.split_on_char ';' raw |> List.map String.trim in
  let nv, attrs = match parts with nv :: attrs -> (nv, attrs) | [] -> ("", []) in
  let name, value =
    match String.index_opt nv '=' with
    | Some i -> (String.sub nv 0 i, String.sub nv (i + 1) (String.length nv - i - 1))
    | None -> (nv, "")
  in
  let expires = ref None and max_age = ref None in
  let attributes =
    List.map
      (fun a ->
        let k, v =
          match String.index_opt a '=' with
          | Some i -> (String.lowercase_ascii (String.sub a 0 i), String.sub a (i + 1) (String.length a - i - 1))
          | None -> (String.lowercase_ascii a, "")
        in
        match (k, parse_http_date v, int_of_string_opt v) with
        | "expires", Some t, _ ->
            expires := Some t;
            (k, "<date>")
        | "max-age", _, Some n ->
            max_age := Some n;
            (k, "<seconds>")
        | _ -> (k, v))
      attrs
  in
  let value =
    if not (List.mem name random_cookie_names) then value
    else if value = "" then "<cleared>"
    else if Some (name ^ "=" ^ value) = sent_cookie then "<reused>"
    else "<rotated>"
  in
  { name; value; attributes; expires = !expires; max_age = !max_age }

(* A pair of responses is captured seconds apart, HTTP dates have 1 s
   resolution, and Dream derives a live session's Max-Age from its
   remaining lifetime; a real lifetime difference is far larger. Expired or
   clearing (Max-Age <= 0) versus live is never tolerated. *)
let expires_tolerance = 5.0

let set_cookies r =
  List.filter_map (fun (k, v) -> if String.lowercase_ascii k = "set-cookie" then Some v else None) r.headers

let cookie_views r = List.map (cookie_view ~sent_cookie:r.sent_cookie) (set_cookies r)

let cookie_diff (ra, a) (rb, b) =
  if a.name <> b.name then Some (Printf.sprintf "cookie %s vs %s" a.name b.name)
  else if a.value <> b.value then Some (Printf.sprintf "cookie %s: value %s vs %s" a.name a.value b.value)
  else if a.attributes <> b.attributes then Some (Printf.sprintf "cookie %s: attributes differ" a.name)
  else
    match (a.max_age, b.max_age) with
    | Some ma, Some mb when (ma > 0) <> (mb > 0) -> Some (Printf.sprintf "cookie %s: cleared vs live Max-Age" a.name)
    | Some ma, Some mb when ma > 0 && Float.abs (float_of_int (ma - mb)) > expires_tolerance ->
        Some (Printf.sprintf "cookie %s: Max-Age %d vs %d" a.name ma mb)
    | _ ->
    match (a.expires, b.expires) with
    | None, None -> None
    | Some ea, Some eb ->
        let live_a = ea > ra.at and live_b = eb > rb.at in
        if live_a <> live_b then Some (Printf.sprintf "cookie %s: expired vs live" a.name)
        else if live_a && Float.abs ((ea -. ra.at) -. (eb -. rb.at)) > expires_tolerance then
          Some (Printf.sprintf "cookie %s: lifetimes differ" a.name)
        else None
    | _ -> Some (Printf.sprintf "cookie %s: Expires on one side only" a.name)

(* What the request did to its own session, seen through its cookie. *)
let session_effect r =
  if r.session_before = "" then "unobserved"
  else
    let id s = List.hd (String.split_on_char ';' s) in
    let authority = List.exists (fun k -> contains r.session_after (k ^ "=")) [ "user_id"; "username"; "is_admin" ] in
    Printf.sprintf "%s%s"
      (if r.session_after = r.session_before then "unchanged"
       else if id r.session_after = id r.session_before then "modified"
       else "replaced")
      (if authority then "+authority" else "")

let other_headers r =
  List.filter_map
    (fun (k, v) ->
      let k = String.lowercase_ascii k in
      if k = "set-cookie" then None else Some (k, v))
    r.headers
  |> List.sort compare

let reply_diff a b =
  if a.status <> b.status then Some (Printf.sprintf "status %d vs %d" a.status b.status)
  else if other_headers a <> other_headers b then Some "headers differ"
  else
    let ca = cookie_views a and cb = cookie_views b in
    if List.length ca <> List.length cb then
      Some (Printf.sprintf "%d vs %d Set-Cookie headers" (List.length ca) (List.length cb))
    else
      match List.find_map (fun (x, y) -> cookie_diff (a, x) (b, y)) (List.combine ca cb) with
      | Some d -> Some d
      | None ->
          if session_effect a <> session_effect b then
            Some (Printf.sprintf "session %s vs %s" (session_effect a) (session_effect b))
          else if mask_csrf a.body <> mask_csrf b.body then Some "bodies differ"
          else None

let same_shape label a b =
  match reply_diff a b with None -> () | Some d -> Alcotest.failf "%s: %s" label d

(* A private-equivalent outcome must leave the browser exactly as it was:
   no cookie, the same session (id and fields), no identity or authority. *)
let private_session_preserved label r =
  (match set_cookies r with [] -> () | _ -> Alcotest.failf "%s: sets a cookie" label);
  if r.session_before = "" then Alcotest.failf "%s: session not observed" label;
  Alcotest.(check string) (label ^ ": request session reused unchanged") r.session_before r.session_after;
  Alcotest.(check string) (label ^ ": session effect") "unchanged" (session_effect r)

(* The comparator's own controls, on synthetic responses: what must count
   as different, and the legitimately random values that must not. *)
let comparator_case =
  pure_case
    "comparator: attribute-only, lifetime, count and session differences are \
     rejected; random session values and date jitter are not" (fun () ->
      let at = 1_800_000_000.0 in
      let base =
        { status = 200; body = "b"; headers = [ ("Content-Type", "text/html") ];
          sent_cookie = Some "dream.session=REQ"; at; session_before = "id=s1;b2_marker=m";
          session_after = "id=s1;b2_marker=m" }
      in
      let with_cookies cs = { base with headers = base.headers @ List.map (fun c -> ("Set-Cookie", c)) cs } in
      let std = "; Max-Age=1209599; Path=/; HttpOnly; SameSite=Lax" in
      let session v rest = "dream.session=" ^ v ^ rest in
      let equal label a b =
        match reply_diff (with_cookies a) (with_cookies b) with
        | None -> ()
        | Some d -> Alcotest.failf "%s: wrongly rejected (%s)" label d
      in
      let differ label a b =
        match reply_diff (with_cookies a) (with_cookies b) with
        | Some _ -> ()
        | None -> Alcotest.failf "%s: difference not detected" label
      in
      equal "rotated session ids are random" [ session "AAA" std ] [ session "BBB" std ];
      equal "no cookies" [] [];
      (* The mutant the earlier name-only reduction missed. *)
      let names r = List.map (fun c -> c.name) (cookie_views r) in
      let m0 = with_cookies [ session "AAA" "; Max-Age=0; Path=/" ] in
      let m1 = with_cookies [ session "BBB" "; Max-Age=3600; Path=/" ] in
      Alcotest.(check bool) "control: names alone cannot tell them apart" true (names m0 = names m1);
      Alcotest.(check bool) "Max-Age only: rejected" true (reply_diff m0 m1 <> None);
      differ "Max-Age cleared vs short-lived" [ session "A" "; Max-Age=0" ] [ session "B" "; Max-Age=5" ];
      differ "Max-Age lifetimes" [ session "A" "; Max-Age=3600" ] [ session "B" "; Max-Age=60" ];
      equal "Max-Age jitter of a live session" [ session "A" "; Max-Age=1209599" ] [ session "B" "; Max-Age=1209600" ];
      differ "Path" [ session "A" "; Path=/" ] [ session "B" "; Path=/x" ];
      differ "HttpOnly dropped" [ session "A" std ] [ session "B" "; Max-Age=1209599; Path=/; SameSite=Lax" ];
      differ "SameSite" [ session "A" std ] [ session "B" "; Max-Age=1209599; Path=/; HttpOnly; SameSite=Strict" ];
      differ "Secure added" [ session "A" std ] [ session "B" (std ^ "; Secure") ];
      differ "Domain added" [ session "A" std ] [ session "B" (std ^ "; Domain=earde.invalid") ];
      differ "reused vs rotated" [ session "REQ" std ] [ session "B" std ];
      differ "cleared vs set" [ session "" "; Max-Age=0; Path=/" ] [ session "B" "; Max-Age=0; Path=/" ];
      differ "one cookie vs two" [ session "A" std ] [ session "B" std; "other=1; Path=/" ];
      differ "a non-random value is literal" [ "consent=yes; Path=/" ] [ "consent=no; Path=/" ];
      let expires t = "; Path=/; Expires=" ^ http_date t in
      Alcotest.(check (option (float 0.0))) "an Expires date parses, comma and all" (Some (at +. 3600.0))
        (List.hd (cookie_views (with_cookies [ session "A" (expires (at +. 3600.0)) ]))).expires;
      Alcotest.(check int) "a dated cookie stays one cookie" 1
        (List.length (cookie_views (with_cookies [ session "A" (expires (at +. 3600.0)) ])));
      differ "expired vs live" [ session "A" (expires 0.0) ] [ session "B" (expires (at +. 3600.0)) ];
      differ "different lifetimes" [ session "A" (expires (at +. 3600.0)) ] [ session "B" (expires (at +. 60.0)) ];
      differ "Expires on one side" [ session "A" (expires (at +. 3600.0)) ] [ session "B" "; Path=/" ];
      equal "the same lifetime, clock jitter" [ session "A" (expires (at +. 3600.0)) ] [ session "B" (expires (at +. 3602.0)) ];
      equal "two expired clearings" [ session "" (expires 0.0) ] [ session "" (expires 86400.0) ];
      (match reply_diff base { base with session_after = "id=s2;b2_marker=m" } with
       | Some _ -> ()
       | None -> Alcotest.fail "a replaced session was not detected");
      (match reply_diff base { base with session_after = "id=s1;b2_marker=m;user_id=7" } with
       | Some _ -> ()
       | None -> Alcotest.fail "an authenticated session was not detected");
      Lwt.return_unit)

let comparator_suite = [ comparator_case ]

let signup ~url ?(target = "/signup") ~username ~email ?(password = "b2 long password") () =
  Lwt.map fst (post ~url ~target [ ("username", username); ("email", email); ("password", password) ])

let forgot ~url ?(target = "/forgot-password") email =
  Lwt.map fst (post ~url ~target [ ("email", email) ])

let pending_rows (module C : Caqti_lwt.CONNECTION) =
  let* r = C.collect_list q_pending_rows () in
  or_fail "pending rows" r

let find_one (module C : Caqti_lwt.CONNECTION) q v label =
  let* r = C.find q v in
  or_fail label r

(* Apostrophes are HTML-escaped in the rendered page: match around them. *)
let neutral_signup = "email you a confirmation link. Click it within 24 hours"
let username_taken = "That username is already taken."
let neutral_reset = "If an account with that email exists, we"

(* ------------------------------------------------------------------ *)
(* Login                                                                 *)
(* ------------------------------------------------------------------ *)

let login ~url ?(target = "/login") identifier password =
  post ~url ~target [ ("identifier", identifier); ("password", password) ]

let login_equivalence_case =
  db_case
    "login: a missing account and a wrong password get the same response \
     after one full verification each; the dummy password never logs in"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = real_hash "b2 correct horse" in
      let* _uid = find_one c q_user ("b2x_login", "login@b2.invalid", hash) "user" in
      verified_hashes := [];
      let* wrong, wrong_cookie = login ~url "b2x_login" "b2 wrong password" in
      Alcotest.(check (list string)) "wrong password: own hash verified" [ hash ] !verified_hashes;
      verified_hashes := [];
      let* missing, missing_cookie = login ~url "b2x_nobody" "b2 wrong password" in
      Alcotest.(check (list string)) "missing account: dummy verified" [ LV.dummy_hash ]
        !verified_hashes;
      same_shape "miss vs wrong" missing wrong;
      must "generic failure" wrong.body "Invalid username or password.";
      verified_hashes := [];
      let* dummy, _ = login ~url "b2x_nobody" LV.dummy_password in
      Alcotest.(check (list string)) "dummy password: dummy verified" [ LV.dummy_hash ]
        !verified_hashes;
      same_shape "dummy password vs wrong" dummy wrong;
      List.iter (fun (l, r) -> private_session_preserved l r)
        [ ("wrong password", wrong); ("missing account", missing); ("dummy password", dummy) ];
      (* The production-wired handler too: its verifier is the real Argon2. *)
      let* prod_dummy, prod_cookie = login ~url ~target:"/login-prod" "b2x_nobody" LV.dummy_password in
      must "production handler refuses the dummy password" prod_dummy.body "Invalid username or password.";
      let* who = get ~url ~cookie:prod_cookie "/whoami" in
      Alcotest.(check string) "no session identity" "anon" who.body;
      let* who = get ~url ~cookie:wrong_cookie "/whoami" in
      Alcotest.(check string) "wrong password: anon" "anon" who.body;
      let* who = get ~url ~cookie:missing_cookie "/whoami" in
      Alcotest.(check string) "missing: anon" "anon" who.body;
      Lwt.return_unit)

let login_success_and_ban_case =
  db_case
    "login: valid credentials rotate into a fresh session; a ban is \
     disclosed only after valid credentials"
    (fun ~url c ->
      let* hash = real_hash "b2 correct horse" in
      let* uid = find_one c q_user ("b2x_ok", "ok@b2.invalid", hash) "user" in
      let* banned_hash = real_hash "b2 banned pass" in
      let* banned = find_one c q_user ("b2x_banned", "banned@b2.invalid", banned_hash) "banned" in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* r = C.exec q_ban banned in
      let* () = or_fail "ban" r in
      verified_hashes := [];
      let* ok, pre_cookie = login ~url "b2x_ok" "b2 correct horse" in
      Alcotest.(check int) "redirect" 303 ok.status;
      Alcotest.(check (list string)) "one verification" [ hash ] !verified_hashes;
      let rotated =
        match List.assoc_opt "Set-Cookie" ok.headers with
        | Some v -> (match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)
        | None -> Alcotest.fail "no rotated session"
      in
      Alcotest.(check bool) "session id rotated" true (rotated <> pre_cookie);
      (match cookie_views ok with
       | [ c ] ->
           Alcotest.(check string) "rotated, not reused" "<rotated>" c.value;
           Alcotest.(check (list (pair string string))) "session cookie attributes"
             [ ("max-age", "<seconds>"); ("path", "/"); ("httponly", ""); ("samesite", "Lax") ]
             c.attributes;
           (match c.max_age with
            | Some n when n > 1_209_590 && n <= 1_209_600 -> ()
            | _ -> Alcotest.fail "session cookie lifetime is not the 14-day session")
       | cs -> Alcotest.failf "login success sets %d cookies" (List.length cs));
      Alcotest.(check string) "the pre-login session is replaced" "replaced" (session_effect ok);
      let* who = get ~url ~cookie:rotated "/whoami" in
      Alcotest.(check string) "authenticated" ("uid:" ^ string_of_int uid) who.body;
      let* who = get ~url ~cookie:pre_cookie "/whoami" in
      Alcotest.(check string) "pre-login session holds no identity" "anon" who.body;
      (* Also by email identifier. *)
      let* by_email, _ = login ~url "ok@b2.invalid" "b2 correct horse" in
      Alcotest.(check int) "email identifier" 303 by_email.status;
      let* banned_ok, banned_cookie = login ~url "b2x_banned" "b2 banned pass" in
      must "ban disclosed after valid credentials" banned_ok.body "Account Banned";
      let* who = get ~url ~cookie:banned_cookie "/whoami" in
      Alcotest.(check string) "banned: no session" "anon" who.body;
      let* banned_wrong, _ = login ~url "b2x_banned" "b2 wrong password" in
      let* missing, _ = login ~url "b2x_nobody" "b2 wrong password" in
      must_not "ban hidden on wrong password" banned_wrong.body "Banned";
      same_shape "banned+wrong vs missing" banned_wrong missing;
      Lwt.return_unit)

let login_db_suite = [ login_equivalence_case; login_success_and_ban_case ]

(* ------------------------------------------------------------------ *)
(* Signup privacy                                                        *)
(* ------------------------------------------------------------------ *)

let holder_old_token = "b2-holder-old-token"
let holder_old_hash = sha256 holder_old_token

let signup_matrix_case =
  db_case
    "signup: username feedback depends on the username alone; every private \
     email/reservation state gets the neutral response and only legitimate \
     submissions mint a confirmation"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* owner_hash = real_hash "b2 owner password" in
      let* _ = find_one c q_user ("b2x_owner", "owner@b2.invalid", owner_hash) "owner" in
      let exec q v label = let* r = C.exec q v in or_fail label r in
      let* () = exec q_pending ("b2x_reserved", "holder@b2.invalid", holder_old_hash, 24.0) "reserved" in
      let* () = exec q_pending ("b2x_mine", "mine@b2.invalid", "mine-old-hash", 24.0) "own" in
      let* () = exec q_pending ("b2x_stale", "gone@b2.invalid", "stale-old-hash", -1.0) "expired" in
      let* users_before = find_one c q_users_snapshot () "users before" in
      (* A real, confirmable token for the holder's reservation, so its later
         death proves the retry replaced it. *)
      let* live = find_one c q_pending_token_exists holder_old_hash "holder token" in
      Alcotest.(check bool) "control: the holder's old token is live" true live;
      with_captured_logs (fun logs ->
          (* --- public: a real account's username --- *)
          let* t_new = signup ~url ~username:"b2x_owner" ~email:"fresh1@b2.invalid" () in
          let* t_reg = signup ~url ~username:"b2x_owner" ~email:"owner@b2.invalid" () in
          let* t_pend = signup ~url ~username:"b2x_owner" ~email:"mine@b2.invalid" () in
          must "taken is explicit" t_new.body username_taken;
          same_shape "taken: registered email" t_reg t_new;
          same_shape "taken: pending email" t_pend t_new;
          Alcotest.(check int) "taken: no mail" 0 (List.length fm.started);
          (* --- private-equivalent class --- *)
          let* n_new = signup ~url ~username:"b2x_new" ~email:"new@b2.invalid" () in
          let* n_reg = signup ~url ~username:"b2x_regmail" ~email:"owner@b2.invalid" () in
          let* n_foreign = signup ~url ~username:"b2x_reserved" ~email:"intruder@b2.invalid" () in
          let* n_foreign_reg = signup ~url ~username:"b2x_reserved" ~email:"owner@b2.invalid" () in
          let* n_holder = signup ~url ~username:"b2x_reserved" ~email:"holder@b2.invalid" () in
          let* n_mine_again = signup ~url ~username:"b2x_mine2" ~email:"mine@b2.invalid" () in
          let* n_stale = signup ~url ~username:"b2x_stale" ~email:"late@b2.invalid" () in
          must "neutral" n_new.body neutral_signup;
          must_not "no delivery claim" n_new.body "we've sent";
          List.iter
            (fun (label, r) -> same_shape label r n_new)
            [ ("registered email", n_reg); ("foreign reservation", n_foreign);
              ("foreign reservation + registered email", n_foreign_reg);
              ("holder retry", n_holder); ("same email, new username", n_mine_again);
              ("expired reservation", n_stale) ];
          List.iter
            (fun (label, r) -> private_session_preserved label r)
            [ ("new email", n_new); ("registered email", n_reg); ("foreign reservation", n_foreign);
              ("holder retry", n_holder); ("expired reservation", n_stale) ];
          (* Every entry, real or no-send, runs through its slot. *)
          let* () = drained fm d in
          (* --- durable state --- *)
          let* rows = pending_rows c in
          let row_for name = List.find_opt (fun (u, _, _) -> u = name) rows in
          let email_of name = Option.map (fun (_, e, _) -> e) (row_for name) in
          let hash_of name = Option.map (fun (_, _, h) -> h) (row_for name) in
          Alcotest.(check (option string)) "new reservation" (Some "new@b2.invalid") (email_of "b2x_new");
          Alcotest.(check (option string)) "no reservation for a registered email" None (email_of "b2x_regmail");
          Alcotest.(check (option string)) "foreign reservation kept by its holder"
            (Some "holder@b2.invalid") (email_of "b2x_reserved");
          Alcotest.(check bool) "holder retry replaced the token" true
            (hash_of "b2x_reserved" <> Some holder_old_hash);
          Alcotest.(check (option string)) "own earlier signup replaced" None (email_of "b2x_mine");
          Alcotest.(check (option string)) "resubmission recorded" (Some "mine@b2.invalid") (email_of "b2x_mine2");
          Alcotest.(check (option string)) "expired squatter replaced" (Some "late@b2.invalid") (email_of "b2x_stale");
          Alcotest.(check bool) "no pending row names the registered email" false
            (List.exists (fun (_, e, _) -> e = "owner@b2.invalid") rows);
          let* users_after = find_one c q_users_snapshot () "users after" in
          Alcotest.(check string) "existing accounts untouched, none created" users_before users_after;
          (* --- mail: only the four legitimate submissions, to their own addresses --- *)
          let recipients = List.map Earde.Email.recipient fm.delivered |> List.sort compare in
          Alcotest.(check (list string)) "confirmations"
            [ "holder@b2.invalid"; "late@b2.invalid"; "mine@b2.invalid"; "new@b2.invalid" ]
            recipients;
          List.iter
            (fun m ->
              let t = token_of_message m in
              match List.find_opt (fun (_, e, _) -> e = Earde.Email.recipient m) rows with
              | Some (_, _, h) -> Alcotest.(check string) "mail token matches its own row" (sha256 t) h
              | None -> Alcotest.fail "mail for a row that does not exist")
            fm.delivered;
          (* --- confirmation authority --- *)
          let msg_to e = List.find (fun m -> Earde.Email.recipient m = e) fm.delivered in
          let* ok = get ~url ("/confirm-email?token=" ^ token_of_message (msg_to "new@b2.invalid")) in
          must "confirmed" ok.body "Email Confirmed!";
          let* email = C.find_opt q_user_email "b2x_new" in
          let* email = or_fail "new user" email in
          Alcotest.(check (option string)) "account created for its own email" (Some "new@b2.invalid") email;
          let* who = get ~url "/whoami" in
          Alcotest.(check string) "no auto-login" "anon" who.body;
          let* still = C.find q_pending_token_exists holder_old_hash in
          let* still = or_fail "old holder token" still in
          Alcotest.(check bool) "the holder's old token row is gone" false still;
          let* old = get ~url ("/confirm-email?token=" ^ holder_old_token) in
          must "superseded holder token is dead" old.body "Confirmation Failed";
          let* holder = get ~url ("/confirm-email?token=" ^ token_of_message (msg_to "holder@b2.invalid")) in
          must "holder's fresh token confirms" holder.body "Email Confirmed!";
          let* email = C.find_opt q_user_email "b2x_reserved" in
          let* email = or_fail "holder user" email in
          Alcotest.(check (option string)) "reservation goes to its holder" (Some "holder@b2.invalid") email;
          (* --- nothing credential-bearing persisted or logged --- *)
          let* json = find_one c q_pending_json () "pending json" in
          must_not "no raw password in pending rows" json "b2 long password";
          List.iter (fun m -> must_not "no raw token in pending rows" json (token_of_message m)) fm.delivered;
          let text = logs () in
          must_not "no raw password in logs" text "b2 long password";
          List.iter
            (fun m ->
              must_not "no token in logs" text (token_of_message m);
              must_not "no recipient in logs" text (Earde.Email.recipient m))
            fm.delivered;
          Lwt.return_unit))

(* A concurrent submission that commits the same username or email while
   this one is in flight: the insert finds the row the moment the other
   transaction commits, so it writes nothing and mails nothing. *)
let race_case =
  db_case
    "signup: losing a uniqueness race (same username or same email) is the \
     neutral response with no row and no mail"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* n_ref = signup ~url ~username:"b2x_refname" ~email:"ref@b2.invalid" () in
      let delivered_before = List.length fm.delivered in
      let race ~held_username ~held_email ~held_hash ~username ~email =
        let* r = C.start () in
        let* () = or_fail "start" r in
        let* r = C.exec q_pending (held_username, held_email, held_hash, 24.0) in
        let* () = or_fail "held insert" r in
        let request = signup ~url ~username ~email () in
        let rec wait_blocked n =
          let* w = C.find q_lock_waiters () in
          let* w = or_fail "lock waiters" w in
          if w > 0 then Lwt.return_unit
          else if n > 400 then Alcotest.fail "submission never reached the contested insert"
          else Lwt.bind (Lwt_unix.sleep 0.025) (fun () -> wait_blocked (n + 1))
        in
        let* () = wait_blocked 0 in
        let* r = C.commit () in
        let* () = or_fail "commit winner" r in
        request
      in
      let* by_name =
        race ~held_username:"b2x_contested" ~held_email:"winner@b2.invalid"
          ~held_hash:"race-winner-hash-1" ~username:"b2x_contested" ~email:"loser@b2.invalid"
      in
      same_shape "username race" by_name n_ref;
      let* by_email =
        race ~held_username:"b2x_winner2" ~held_email:"contested@b2.invalid"
          ~held_hash:"race-winner-hash-2" ~username:"b2x_loser2" ~email:"contested@b2.invalid"
      in
      same_shape "email race" by_email n_ref;
      let* rows = pending_rows c in
      Alcotest.(check bool) "losers wrote nothing" false
        (List.exists (fun (u, e, _) -> e = "loser@b2.invalid" || u = "b2x_loser2") rows);
      Alcotest.(check bool) "winners intact" true
        (List.exists (fun (u, _, h) -> u = "b2x_contested" && h = "race-winner-hash-1") rows
         && List.exists (fun (u, _, h) -> u = "b2x_winner2" && h = "race-winner-hash-2") rows);
      let* () = drained fm d in
      Alcotest.(check int) "no mail for a lost race" delivered_before (List.length fm.delivered);
      Lwt.return_unit)

let signup_db_suite = [ signup_matrix_case; race_case ]

(* ------------------------------------------------------------------ *)
(* Asynchronous, bounded, durable-first mail                             *)
(* ------------------------------------------------------------------ *)

(* The request must answer while the provider is still stalled. The 5 s
   pick is only a safety net so an awaiting implementation fails instead of
   hanging the suite; the assertion is on ordering, not on elapsed time. *)
let answers_before_provider label request =
  let* r =
    Lwt.pick
      [ Lwt.map (fun r -> `Answered r) request;
        Lwt.map (fun () -> `Stalled) (Lwt_unix.sleep 5.0) ]
  in
  match r with
  | `Answered r -> Lwt.return r
  | `Stalled -> Alcotest.failf "%s: response waited for the stalled provider" label

let stalled_provider_case =
  db_case
    "mail: signup and reset requests (known and unknown) answer while the \
     provider is stalled; releasing it then delivers working links"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let gate, release = Lwt.wait () in
      let fm, d = fake_dispatcher ~gate () in
      current_mail := Some d;
      let* hash = real_hash "b2 before reset" in
      let* uid = find_one c q_user ("b2x_resetme", "resetme@b2.invalid", hash) "user" in
      ignore uid;
      let* _ = find_one c q_user ("b2x_taken", "taken@b2.invalid", hash) "registered" in
      let* s_new = answers_before_provider "signup new" (signup ~url ~username:"b2x_async" ~email:"async@b2.invalid" ()) in
      let* s_reg = answers_before_provider "signup registered" (signup ~url ~username:"b2x_async2" ~email:"taken@b2.invalid" ()) in
      let* r_known = answers_before_provider "reset known" (forgot ~url "resetme@b2.invalid") in
      let* r_unknown = answers_before_provider "reset unknown" (forgot ~url "nobody@b2.invalid") in
      same_shape "signup registered vs new" s_reg s_new;
      same_shape "reset unknown vs known" r_unknown r_known;
      must "reset neutral" r_known.body neutral_reset;
      List.iter (fun (l, r) -> private_session_preserved l r)
        [ ("signup new", s_new); ("signup registered", s_reg); ("reset known", r_known);
          ("reset unknown", r_unknown) ];
      (* Two slots: the new signup's mail (stalled) and the registered one's
         no-send entry. The reset entries wait for the next deadline. *)
      Alcotest.(check int) "provider entered for the first real job" 1 (List.length fm.started);
      check_stats "four admitted" d ~outstanding:4 ~queued:2 ~running:2;
      Alcotest.(check int) "nothing delivered yet" 0 (List.length fm.delivered);
      Lwt.wakeup release ();
      let* () = settle 20 in
      Alcotest.(check int) "delivered once released" 1 (List.length fm.delivered);
      check_stats "delivery does not end its slot" d ~outstanding:4 ~queued:2 ~running:2;
      let* () = drained fm d in
      Alcotest.(check (list string)) "both delivered, on the slot schedule"
        [ "async@b2.invalid"; "resetme@b2.invalid" ]
        (List.map Earde.Email.recipient fm.delivered |> List.sort compare);
      let find kind_path =
        List.find (fun m -> contains (Earde.Email.link m) kind_path) fm.delivered
      in
      let* ok = get ~url ("/confirm-email?token=" ^ token_of_message (find "/confirm-email")) in
      must "confirmation link works" ok.body "Email Confirmed!";
      let* reset, _ =
        post ~url ~target:"/reset-password"
          [ ("token", token_of_message (find "/reset-password"));
            ("password", "b2 after reset"); ("confirm_password", "b2 after reset") ]
      in
      must "reset link works" reset.body "Password Updated";
      let* logged, _ = login ~url "b2x_resetme" "b2 after reset" in
      Alcotest.(check int) "new password logs in" 303 logged.status;
      Lwt.return_unit)

(* 64 admitted requests are held open; every further signup/reset — whatever
   the account state — gets the same 503 and writes nothing. *)
let overload_case =
  db_case
    "mail: a full dispatcher refuses signup and reset identically across \
     account states, before any write; capacity then recovers"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 overload" in
      let* _ = find_one c q_user ("b2x_full", "full@b2.invalid", hash) "user" in
      let* r = C.exec q_pending ("b2x_fullpend", "fullpend@b2.invalid", "fullpend-hash", 24.0) in
      let* () = or_fail "pending" r in
      let holds =
        List.init 64 (fun _ ->
            let p, u = Lwt.wait () in
            (u, D.admit d (fun () -> p)))
      in
      Alcotest.(check int) "saturated" 64 (D.stats d).D.outstanding;
      let* rows_before = pending_rows c in
      let* resets_before = find_one c q_reset_rows () "resets" in
      let* a = signup ~url ~username:"b2x_fullnew" ~email:"fullnew@b2.invalid" () in
      let* b = signup ~url ~username:"b2x_fullreg" ~email:"full@b2.invalid" () in
      let* c1 = signup ~url ~username:"b2x_fullpend" ~email:"fullpend@b2.invalid" () in
      let* c2 = signup ~url ~username:"b2x_fullpend" ~email:"other@b2.invalid" () in
      Alcotest.(check int) "503" 503 a.status;
      must "generic" a.body "Temporarily unavailable";
      List.iter (fun (l, r) -> same_shape l r a)
        [ ("registered email", b); ("own reservation", c1); ("foreign reservation", c2) ];
      let* k = forgot ~url "full@b2.invalid" in
      let* u = forgot ~url "nobody@b2.invalid" in
      Alcotest.(check int) "reset 503" 503 k.status;
      same_shape "reset known vs unknown" u k;
      (* The public username answer does not depend on the dispatcher. *)
      let* t = signup ~url ~username:"b2x_full" ~email:"fullnew@b2.invalid" () in
      must "taken still answered" t.body username_taken;
      let* rows_after = pending_rows c in
      let* resets_after = find_one c q_reset_rows () "resets" in
      Alcotest.(check bool) "no pending write" true (rows_before = rows_after);
      Alcotest.(check int) "no reset token" resets_before resets_after;
      Alcotest.(check int) "no job" 0 (List.length fm.started);
      Alcotest.(check int) "still exactly full" 64 (D.stats d).D.outstanding;
      List.iter (fun (u, _) -> Lwt.wakeup u ((), None)) holds;
      let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) holds) in
      (* Settling without mail frees nothing: each place is held for a full
         slot, exactly as a mail job would hold it. *)
      check_stats "no-mail settlements keep their places" d ~outstanding:64 ~queued:62 ~running:2;
      let* still = forgot ~url "nobody@b2.invalid" in
      same_shape "still full until a slot ends" still k;
      Vclock.advance_to fm.clock 14.999;
      let* still = forgot ~url "nobody@b2.invalid" in
      same_shape "still full just before the deadline" still k;
      Vclock.advance_to fm.clock 15.0;
      check_stats "two slots ended" d ~outstanding:62 ~queued:60 ~running:2;
      let* again = signup ~url ~username:"b2x_fullnew" ~email:"fullnew@b2.invalid" () in
      must "recovered" again.body neutral_signup;
      let* () = drained fm d in
      Alcotest.(check int) "and mails" 1 (List.length fm.delivered);
      Alcotest.(check int) "drained" 0 (D.stats d).D.outstanding;
      Lwt.return_unit)

let durable_first_case =
  db_case
    "mail: the token row is committed before its job runs; storage failures \
     send nothing and answer like success"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let seen = ref [] in
      (* Runs inside the delivery, on a different connection: it sees only
         committed state. *)
      fm.at_start <-
        (fun m ->
          let t = token_of_message m in
          let q = if contains (Earde.Email.link m) "/confirm-email" then q_pending_token_exists else q_reset_token_exists in
          let* r = C.find q (sha256 t) in
          let* present = or_fail "durable check" r in
          seen := present :: !seen;
          Lwt.return_unit);
      let* hash = real_hash "b2 durable" in
      let* _ = find_one c q_user ("b2x_durable", "durable@b2.invalid", hash) "user" in
      let* s_ok = signup ~url ~username:"b2x_dnew" ~email:"dnew@b2.invalid" () in
      let* () = drained fm d in
      let* r_ok = forgot ~url "durable@b2.invalid" in
      let* () = drained fm d in
      Alcotest.(check (list bool)) "both rows committed before delivery" [ true; true ] !seen;
      (* Shadow schema: users resolves, pending_signups and password_resets do
         not — the private write itself fails. *)
      let exec sql = let* r = C.exec ((Caqti_type.unit ->. Caqti_type.unit) sql) () in or_fail sql r in
      let* () = exec "CREATE SCHEMA b2_shadow" in
      let* () = exec "CREATE VIEW b2_shadow.users AS SELECT * FROM public.users" in
      let shadow = with_search_path url "b2_shadow" in
      let before = List.length fm.started in
      let* s_fail =
        with_captured_logs (fun _ -> signup ~url:shadow ~username:"b2x_dfail" ~email:"dfail@b2.invalid" ())
      in
      let* r_fail = with_captured_logs (fun _ -> forgot ~url:shadow "durable@b2.invalid") in
      same_shape "signup storage failure looks like success" s_fail s_ok;
      same_shape "reset storage failure looks like success" r_fail r_ok;
      List.iter (must_not "no storage detail" s_fail.body) [ "b2_shadow"; "relation"; "pending_signups" ];
      let* () = drained fm d in
      Alcotest.(check int) "no mail after a failed write" before (List.length fm.started);
      let* rows = pending_rows c in
      Alcotest.(check bool) "no row for the failed signup" false
        (List.exists (fun (u, _, _) -> u = "b2x_dfail") rows);
      (* Reset token rows hold only the hash. *)
      let* json = find_one c q_reset_json () "reset json" in
      List.iter
        (fun m ->
          if contains (Earde.Email.link m) "/reset-password" then
            must_not "no raw reset token stored" json (token_of_message m))
        fm.delivered;
      Lwt.return_unit)

let volatile_resend_case =
  db_case
    "mail: a job lost with the dispatcher is recovered by a fresh request; \
     the superseded signup link dies, the new ones work"
    (fun ~url c ->
      let lost_gate, _never = Lwt.wait () in
      let lost, lost_d = fake_dispatcher ~gate:lost_gate () in
      current_mail := Some lost_d;
      let* hash = real_hash "b2 volatile" in
      let* _ = find_one c q_user ("b2x_vol", "vol@b2.invalid", hash) "user" in
      let* _ = signup ~url ~username:"b2x_volnew" ~email:"volnew@b2.invalid" () in
      let* _ = forgot ~url "vol@b2.invalid" in
      Alcotest.(check int) "both stuck in the lost dispatcher" 2 (List.length lost.started);
      Alcotest.(check int) "never delivered" 0 (List.length lost.delivered);
      let lost_signup = List.find (fun m -> contains (Earde.Email.link m) "/confirm-email") lost.started in
      (* "Restart": a fresh dispatcher; the user simply asks again. *)
      let fresh, fresh_d = fake_dispatcher () in
      current_mail := Some fresh_d;
      let* again = signup ~url ~username:"b2x_volnew" ~email:"volnew@b2.invalid" () in
      must "resend accepted" again.body neutral_signup;
      let* _ = forgot ~url "vol@b2.invalid" in
      Alcotest.(check int) "fresh links delivered" 2 (List.length fresh.delivered);
      let* dead = get ~url ("/confirm-email?token=" ^ token_of_message lost_signup) in
      must "lost token superseded" dead.body "Confirmation Failed";
      let find p = List.find (fun m -> contains (Earde.Email.link m) p) fresh.delivered in
      let* ok = get ~url ("/confirm-email?token=" ^ token_of_message (find "/confirm-email")) in
      must "fresh confirmation works" ok.body "Email Confirmed!";
      let* reset, _ =
        post ~url ~target:"/reset-password"
          [ ("token", token_of_message (find "/reset-password"));
            ("password", "b2 volatile new"); ("confirm_password", "b2 volatile new") ]
      in
      must "fresh reset works" reset.body "Password Updated";
      let* replay, _ =
        post ~url ~target:"/reset-password"
          [ ("token", token_of_message (find "/reset-password"));
            ("password", "b2 volatile again"); ("confirm_password", "b2 volatile again") ]
      in
      must "reset token single-use" replay.body "Link Expired";
      Lwt.return_unit)

let mail_db_suite = [ stalled_provider_case; overload_case; durable_first_case; volatile_resend_case ]

(* ------------------------------------------------------------------ *)
(* Rate limiter over the real routed boundary                            *)
(* ------------------------------------------------------------------ *)

let limiter_routed_case =
  db_case
    "limiter: a failing enforcement lookup and an unreachable pool refuse \
     with 503 and invoke no protected handler; Allowed and Blocked controls \
     still hold"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 limiter" in
      let* _ = find_one c q_user ("b2x_rl", "rl@b2.invalid", hash) "user" in
      let run_all url =
        verified_hashes := [];
        limited_hits := 0;
        let* a, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
        let* b = forgot ~url ~target:"/b2-rl/forgot" "rl@b2.invalid" in
        let* s = signup ~url ~target:"/b2-rl/signup" ~username:"b2x_rlsign" ~email:"rlsign@b2.invalid" () in
        Lwt.return [ a; b; s ]
      in
      (* 1. Result error: the lookup's relation cannot be resolved. *)
      let* broken = run_all (with_search_path url "b2_void") in
      (* 2. Rejected promise: the pool cannot connect at all. *)
      let* dbs = C.find ((Caqti_type.unit ->! Caqti_type.int) "SELECT COUNT(*)::int FROM pg_database WHERE datname = 'earde_b2_absent_db'") () in
      let* dbs = or_fail "absent db check" dbs in
      Alcotest.(check int) "the target database really is absent" 0 dbs;
      let absent = Uri.to_string (Uri.with_path (Uri.of_string url) "/earde_b2_absent_db") in
      let* unreachable = run_all absent in
      List.iter
        (fun r ->
          Alcotest.(check int) "503" 503 r.status;
          must "generic page" r.body "Temporarily unavailable";
          List.iter (must_not "no detail" r.body) [ "b2_void"; "relation"; "earde_b2_absent_db"; "Caqti"; "rate_limits" ])
        (broken @ unreachable);
      same_shape "result error vs pool failure" (List.hd broken) (List.hd unreachable);
      Alcotest.(check int) "no protected handler ran" 0 !limited_hits;
      Alcotest.(check (list string)) "no verification ran" [] !verified_hashes;
      Alcotest.(check int) "no mail admitted" 0 (D.stats d).D.outstanding;
      Alcotest.(check int) "no mail" 0 (List.length fm.started);
      let* rows = pending_rows c in
      Alcotest.(check bool) "no signup write" false (List.exists (fun (u, _, _) -> u = "b2x_rlsign") rows);
      let* resets = find_one c q_reset_rows () "resets" in
      Alcotest.(check int) "no reset write" 0 resets;
      (* 3. Controls on the healthy pool: Allowed reaches the handler, and the
         sixth attempt in the window is blocked without reaching it. *)
      limited_hits := 0;
      let* first, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
      Alcotest.(check int) "allowed login proceeds" 303 first.status;
      let rec attempts n =
        if n = 0 then Lwt.return_unit
        else
          let* _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 wrong" in
          attempts (n - 1)
      in
      let* () = attempts 4 in
      Alcotest.(check int) "five allowed" 5 !limited_hits;
      let* sixth, _ = login ~url ~target:"/b2-rl/login" "b2x_rl" "b2 limiter" in
      must "blocked" sixth.body "Too Many Attempts";
      Alcotest.(check int) "blocked never reaches the handler" 5 !limited_hits;
      Lwt.return_unit)

let limiter_db_suite = [ limiter_routed_case ]

(* ------------------------------------------------------------------ *)
(* Paired sequences at the capacity edge                                 *)
(* ------------------------------------------------------------------ *)

(* Promoted from the independent review's reproduction. Each pair is two
   arms that start from the same state: 63 of 64 places taken (2 entries in
   service, 56 queued entries, 5 reservations still working), a controlled
   clock at t=0. The queued entries are either all real mail (the review's
   shape) or a mix of real and no-send entries. Then a target request whose private
   outcome differs between the arms (mail vs no mail), then harmless probes
   through the same routed handlers, CSRF and production limiter. Before
   fixed slots, the no-mail arm gave its place back at once, so the next
   probe got 200 there and 503 in the mail arm. Now every public outcome
   and the exact release schedule must match, whatever the provider does. *)

let q_arm_reset =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM rate_limits WHERE endpoint LIKE '/b2-%'";
      "DELETE FROM password_resets WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'b2x\\_%')";
      "DELETE FROM pending_signups WHERE email LIKE '%@b2.invalid' OR username LIKE 'b2x\\_%'" ]

type arm = {
  target : reply;
  probes : reply list;
  points : (int * int * int) list;
  sched : (float * (int * int * int)) list;
  hits : int;
  mails : int;
  resets : int;
}

let fill_arm ~mixed d =
  let fill_msg i =
    Earde.Email.password_reset ~to_email:(Printf.sprintf "fill%d@b2.invalid" i) ~token:"b2-fill-token"
  in
  let* () =
    Lwt_list.iter_s
      (fun i ->
        let job = if (not mixed) || i < 2 || i mod 3 <> 0 then Some (fill_msg i) else None in
        Lwt.map ignore (D.admit d (fun () -> Lwt.return ((), job))))
      (List.init 58 Fun.id)
  in
  Lwt.return
    (List.init 5 (fun _ ->
         let p, u = Lwt.wait () in
         (u, D.admit d (fun () -> p))))

let run_arm ~url c ~mixed ~behaviour ~prepare ~target =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* () =
    Lwt_list.iter_s (fun q -> let* r = C.exec q () in or_fail "arm reset" r) q_arm_reset
  in
  let* () = prepare () in
  let clock = Vclock.create () in
  let mails = ref 0 in
  let d =
    D.create ~sleep:(Vclock.sleep clock) ~label:Earde.Email.label
      ~transport:(behaviour_transport clock ~record:(fun _ -> incr mails) behaviour) ()
  in
  current_mail := Some d;
  let* holds = fill_arm ~mixed d in
  let at_fill = occupancy d in
  limited_hits := 0;
  let* resets0 = find_one c q_reset_rows () "resets" in
  let* t = target () in
  let after_target = occupancy d in
  let probe name = forgot ~url ~target:"/b2-rl/forgot" (name ^ "-probe@b2.invalid") in
  let* p1 = probe "first" in
  Vclock.advance_to clock 14.999;
  let before_deadline = occupancy d in
  let* p2 = probe "early" in
  Vclock.advance_to clock 15.0;
  let at_deadline = occupancy d in
  let* p3 = probe "deadline" in
  let after_probes = occupancy d in
  (* The working reservations settle without mail, identically in each arm. *)
  List.iter (fun (u, _) -> Lwt.wakeup u ((), None)) holds;
  let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) holds) in
  let sched = trace clock d 100_000.0 in
  let* resets1 = find_one c q_reset_rows () "resets" in
  current_mail := None;
  Lwt.return
    { target = t; probes = [ p1; p2; p3 ];
      points = [ at_fill; after_target; before_deadline; at_deadline; after_probes; occupancy d ];
      sched; hits = !limited_hits; mails = !mails; resets = resets1 - resets0 }

let capacity_sequence_case =
  db_case
    "capacity: from the same near-full state, a target's private outcome \
     changes no later probe and no release time, for reset, signup and \
     reservation pairs under every provider behaviour"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = real_hash "b2 sequence victim" in
      let* _ = find_one c q_user ("b2x_seqvictim", "seqvictim@b2.invalid", hash) "victim" in
      let no_prepare () = Lwt.return_unit in
      let held_reservation () =
        let* r = C.exec q_pending ("b2x_seqheld", "seqholder@b2.invalid", sha256 "b2-seq-held", 24.0) in
        or_fail "reservation" r
      in
      let sign ~username ~email () = signup ~url ~target:"/b2-rl/signup" ~username ~email () in
      let pairs =
        [ ("reset known vs unknown", no_prepare,
           (fun () -> forgot ~url ~target:"/b2-rl/forgot" "seqvictim@b2.invalid"),
           (fun () -> forgot ~url ~target:"/b2-rl/forgot" "seqnobody@b2.invalid"));
          ("signup new vs registered email", no_prepare,
           sign ~username:"b2x_seqnew" ~email:"seqnew@b2.invalid",
           sign ~username:"b2x_seqnew" ~email:"seqvictim@b2.invalid");
          ("signup own vs foreign reservation", held_reservation,
           sign ~username:"b2x_seqheld" ~email:"seqholder@b2.invalid",
           sign ~username:"b2x_seqheld" ~email:"seqintruder@b2.invalid") ]
      in
      let behaviours = [ Stall; After (7.0, Ok ()); Fast_ok; Fast_error ] in
      print_newline ();
      Lwt_list.iter_s
        (fun (fill, mixed) ->
      Lwt_list.iter_s
        (fun behaviour ->
          Lwt_list.iter_s
            (fun (pair, prepare, mail_target, quiet_target) ->
              let label = pair ^ " / " ^ behaviour_name behaviour ^ " / " ^ fill in
              let* m = run_arm ~url c ~mixed ~behaviour ~prepare ~target:mail_target in
              let* q = run_arm ~url c ~mixed ~behaviour ~prepare ~target:quiet_target in
              let st r = string_of_int r.status in
              Printf.printf "[B2] %-64s probes mail=%s quiet=%s releases=%d last=%.0fs mails=%d/%d limiter=%d/%d\n%!"
                label (String.concat "," (List.map st m.probes)) (String.concat "," (List.map st q.probes))
                (List.length m.sched) (fst (List.nth m.sched (List.length m.sched - 1))) m.mails q.mails
                m.hits q.hits;
              (* The arms really differ privately... *)
              Alcotest.(check int) (label ^ ": only the mail arm's target produced mail") (q.mails + 1) m.mails;
              if pair = "reset known vs unknown" then
                Alcotest.(check (pair int int)) (label ^ ": token rows") (1, 0) (m.resets, q.resets);
              (* ...and nothing public does. *)
              same_shape (label ^ ": target") m.target q.target;
              private_session_preserved (label ^ ": mail target") m.target;
              private_session_preserved (label ^ ": quiet target") q.target;
              List.iteri
                (fun i (a, b) -> same_shape (Printf.sprintf "%s: probe %d" label (i + 1)) a b)
                (List.combine m.probes q.probes);
              Alcotest.(check (list (triple int int int))) (label ^ ": occupancy at each point") m.points q.points;
              Alcotest.check schedule (label ^ ": release schedule") m.sched q.sched;
              (* Absolute expectations, after the paired ones. *)
              Alcotest.(check int) (label ^ ": limiter allowed every request") 4 m.hits;
              Alcotest.(check int) (label ^ ": limiter allowed every request (quiet)") 4 q.hits;
              Alcotest.(check (list int)) (label ^ ": probes full, full just before, admitted at the deadline")
                [ 503; 503; 200 ] (List.map (fun r -> r.status) m.probes);
              Alcotest.(check (list (triple int int int))) (label ^ ": expected occupancy")
                [ (63, 56, 2); (64, 57, 2); (64, 57, 2); (62, 55, 2); (63, 56, 2); (0, 0, 0) ] m.points;
              Lwt.return_unit)
            pairs)
        behaviours)
        [ ("real-only fill", false); ("mixed fill", true) ])

let capacity_sequence_db_suite = [ capacity_sequence_case ]

(* ------------------------------------------------------------------ *)
(* Cookies and sessions over the routed boundary                         *)
(* ------------------------------------------------------------------ *)

let cookie_session_case =
  db_case
    "cookies: private-equivalent pairs set no cookie, keep the request \
     session and authenticate nobody; valid logins rotate identically"
    (fun ~url c ->
      let fm, d = fake_dispatcher () in
      current_mail := Some d;
      let* hash = real_hash "b2 cookie pass" in
      let* _ = find_one c q_user ("b2x_cookie", "cookie@b2.invalid", hash) "user" in
      let* _ = find_one c q_user ("b2x_cookie2", "cookie2@b2.invalid", hash) "user 2" in
      let pair label (a, ca) (b, cb) =
        same_shape label a b;
        private_session_preserved (label ^ " (first)") a;
        private_session_preserved (label ^ " (second)") b;
        let* wa = get ~url ~cookie:ca "/whoami" in
        let* wb = get ~url ~cookie:cb "/whoami" in
        Alcotest.(check (pair string string)) (label ^ ": nobody authenticated") ("anon", "anon") (wa.body, wb.body);
        Lwt.return_unit
      in
      let* k = post ~url ~target:"/forgot-password" [ ("email", "cookie@b2.invalid") ] in
      let* u = post ~url ~target:"/forgot-password" [ ("email", "nobody@b2.invalid") ] in
      let* () = pair "reset known vs unknown" k u in
      let* sn = post ~url ~target:"/signup" [ ("username", "b2x_ck1"); ("email", "cknew@b2.invalid"); ("password", "b2 long password") ] in
      let* sr = post ~url ~target:"/signup" [ ("username", "b2x_ck2"); ("email", "cookie@b2.invalid"); ("password", "b2 long password") ] in
      let* () = pair "signup new vs registered email" sn sr in
      let* lw = login ~url "b2x_cookie" "b2 wrong" in
      let* lm = login ~url "b2x_nobody" "b2 wrong" in
      let* () = pair "login wrong password vs missing account" lw lm in
      (* Controls: two valid logins differ from the failures, and from each
         other only by their random session ids. *)
      let* s1, pre1 = login ~url "b2x_cookie" "b2 cookie pass" in
      let* s2, _ = login ~url "b2x_cookie2" "b2 cookie pass" in
      same_shape "two valid logins" s1 s2;
      Alcotest.(check bool) "a valid login is distinguishable" true (reply_diff s1 (fst lw) <> None);
      Alcotest.(check string) "valid login replaces the session" "replaced" (session_effect s1);
      let* who = get ~url ~cookie:pre1 "/whoami" in
      Alcotest.(check string) "the pre-login cookie holds no identity" "anon" who.body;
      let* () = drained fm d in
      Lwt.return_unit)

let cookie_db_suite = [ cookie_session_case ]

let suites =
    (* Account privacy and fail-closed authentication boundaries. DB-free:
       the bounded auth-mail dispatcher, the provider transport's
       connection release, the login dummy-verification contract and the
       fail-closed limiter decision. Gated: the real signup, login, reset
       and limiter handlers over routed pipelines. *)
  [ ("b2_auth_mail_dispatcher", dispatcher_suite)
  ; ("b2_auth_mail_resolver", resolver_suite)
  ; ("b2_auth_mail_transport", transport_suite)
  ; ("b2_login_verification", login_pure_suite)
  ; ("b2_rate_limit_decision", limiter_pure_suite)
  ; ("b2_login_equivalence", login_db_suite)
  ; ("b2_signup_privacy", signup_db_suite)
  ; ("b2_auth_mail_async", mail_db_suite)
  ; ("b2_rate_limit_routed", limiter_db_suite)
  ; ("b2_response_comparator", comparator_suite)
  ; ("b2_capacity_sequences", capacity_sequence_db_suite)
  ; ("b2_cookie_session", cookie_db_suite)
  ]
