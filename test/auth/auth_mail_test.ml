(* The bounded auth-mail path, component by component: the dispatcher's
   fixed service slots for real and no-send entries alike on a controlled
   clock (exact capacity, FIFO order, two slots, release only at the
   deadline), the resolver gate whose permits belong to the physical
   lookups, and the Brevo transport's connection release against local
   servers. DB-free. *)

open Auth_mail_fixture

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

let suites =
  [ ("b2_auth_mail_dispatcher", dispatcher_suite)
  ; ("b2_auth_mail_resolver", resolver_suite)
  ; ("b2_auth_mail_transport", transport_suite)
  ]
