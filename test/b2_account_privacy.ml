(* Account privacy and fail-closed authentication boundaries.

   DB-free component suites pin the bounded auth-mail dispatcher (exact
   capacity/concurrency, cleanup on error/timeout/cancellation), the Brevo
   transport's connection release against a local stalled server, the
   login dummy-verification contract, and the fail-closed rate-limit
   decision. The gated suites (EARDE_TEST_DATABASE_URL) drive the real
   signup, login, password-reset and limiter handlers over routed
   pipelines against PostgreSQL. Fixture names use the b2x_ prefix and the
   reserved b2.invalid domain; every gated case cleans before and after. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module D = Earde.Auth_mail_dispatcher
module H = Earde.Handlers
module LV = Earde.Login_verification

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let must label hay needle =
  if not (contains hay needle) then Alcotest.failf "%s: expected text missing" label

let must_not label hay needle =
  if contains hay needle then Alcotest.failf "%s: forbidden text present" label

let rec settle n = if n <= 0 then Lwt.return_unit else Lwt.bind (Lwt.pause ()) (fun () -> settle (n - 1))

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
(* Dispatcher: controlled clock and controlled transport               *)
(* ------------------------------------------------------------------ *)

module Clock = struct
  type t = {
    mutable timers : unit Lwt.u list;
    mutable armed : int;
    mutable cancelled : int;
  }

  let create () = { timers = []; armed = 0; cancelled = 0 }

  let sleep c (_ : float) =
    let p, u = Lwt.task () in
    Lwt.on_cancel p (fun () -> c.cancelled <- c.cancelled + 1);
    c.timers <- u :: c.timers;
    c.armed <- c.armed + 1;
    p

  (* Expire every timer still pending. *)
  let expire_all c =
    let timers = c.timers in
    c.timers <- [];
    List.iter (fun u -> try Lwt.wakeup u () with Invalid_argument _ -> ()) timers
end

module Fake = struct
  type t = {
    mutable started : int list;
    mutable finished : int list;
    mutable cancelled : int list;
    gates : (int, (unit, string) result Lwt.u) Hashtbl.t;
  }

  let create () =
    { started = []; finished = []; cancelled = []; gates = Hashtbl.create 16 }

  (* Every delivery blocks on its own cancelable promise until the test
     resolves it — or until the dispatcher cancels it on timeout, which is
     recorded as the transport's resource release. *)
  let transport f job =
    f.started <- f.started @ [ job ];
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

  let in_flight f =
    List.filter (fun j -> Hashtbl.mem f.gates j && not (List.mem j f.cancelled)) f.started
end

let check_stats label d ~outstanding ~queued ~running =
  let s = D.stats d in
  Alcotest.(check (list int))
    (label ^ ": outstanding/queued/running")
    [ outstanding; queued; running ]
    [ s.D.outstanding; s.D.queued; s.D.running ]

let make_dispatcher ?config clock fake =
  D.create ?config ~sleep:(Clock.sleep clock) ~label:(fun _ -> "test")
    ~transport:(Fake.transport fake) ()

let submit d job = D.admit d (fun () -> Lwt.return ((), Some job))

let pure_case name f = Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f ()))

let default_bounds_case =
  pure_case "default bounds are 64 outstanding, 2 concurrent, 15 s" (fun () ->
      Alcotest.(check int) "capacity" 64 D.default_config.D.capacity;
      Alcotest.(check int) "concurrency" 2 D.default_config.D.concurrency;
      Alcotest.(check (float 0.0)) "timeout" 15.0 D.default_config.D.timeout_seconds;
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
     running its work, a no-mail settlement frees a place" (fun () ->
      let clock = Clock.create () and fake = Fake.create () in
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
      Alcotest.(check bool) "65th refused" true (r = `Refused);
      Alcotest.(check bool) "refused work never ran" false !ran;
      (* One admitted request settles without mail: its place is free. *)
      let u0, r0 = List.hd holds in
      Lwt.wakeup u0 ((), None);
      let* r0 = r0 in
      Alcotest.(check bool) "settled" true (r0 = `Admitted ());
      check_stats "63 held" d ~outstanding:63 ~queued:0 ~running:0;
      let* r = D.admit d (fun () -> Lwt.return ((), None)) in
      Alcotest.(check bool) "admitted again" true (r = `Admitted ());
      Alcotest.(check (list int)) "no delivery for no-mail settlements" [] fake.Fake.started;
      List.iter (fun (u, _) -> Lwt.wakeup u ((), None)) (List.tl holds);
      let* _ = Lwt.join (List.map (fun (_, r) -> Lwt.map ignore r) (List.tl holds)) in
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

let concurrency_case =
  pure_case
    "concurrency: exactly 2 deliveries run; queued and running jobs count \
     toward the 64 capacity" (fun () ->
      let clock = Clock.create () and fake = Fake.create () in
      let d = make_dispatcher clock fake in
      let* () = Lwt_list.iter_s (fun j -> Lwt.map ignore (submit d j)) [ 1; 2; 3; 4; 5 ] in
      Alcotest.(check (list int)) "two started" [ 1; 2 ] fake.Fake.started;
      check_stats "5 admitted" d ~outstanding:5 ~queued:3 ~running:2;
      Fake.resolve fake 1 (Ok ());
      Alcotest.(check (list int)) "third starts only after one ends" [ 1; 2; 3 ]
        fake.Fake.started;
      check_stats "after one" d ~outstanding:4 ~queued:2 ~running:2;
      let* () =
        Lwt_list.iter_s (fun j -> Lwt.map ignore (submit d j)) (List.init 60 (fun i -> 100 + i))
      in
      check_stats "full" d ~outstanding:64 ~queued:62 ~running:2;
      let* r = submit d 999 in
      Alcotest.(check bool) "queued work fills capacity" true (r = `Refused);
      (* Drain everything; never more than two in flight at once. *)
      let max_seen = ref 0 in
      let rec drain () =
        match Fake.in_flight fake with
        | [] -> Lwt.return_unit
        | j :: _ ->
            max_seen := max !max_seen (D.stats d).D.running;
            Fake.resolve fake j (Ok ());
            drain ()
      in
      let* () = drain () in
      Alcotest.(check int) "max concurrent" 2 !max_seen;
      Alcotest.(check int) "all 65 admitted jobs delivered" 65 (List.length fake.Fake.finished);
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

let failure_isolation_case =
  pure_case
    "failures: an Error, a synchronous raise and a rejected promise each free \
     their slot and never poison the next job" (fun () ->
      let delivered = ref [] in
      let transport = function
        | 1 -> Lwt.return (Error "boom")
        | 2 -> failwith "sync transport failure"
        | 3 -> Lwt.fail (Failure "async transport failure")
        | n ->
            delivered := n :: !delivered;
            Lwt.return (Ok ())
      in
      let clock = Clock.create () in
      let d = D.create ~sleep:(Clock.sleep clock) ~label:(fun _ -> "test") ~transport () in
      let* () = Lwt_list.iter_s (fun j -> Lwt.map ignore (D.admit d (fun () -> Lwt.return ((), Some j)))) [ 1; 2; 3; 4; 5 ] in
      Alcotest.(check (list int)) "later jobs delivered" [ 5; 4 ] !delivered;
      check_stats "nothing held" d ~outstanding:0 ~queued:0 ~running:0;
      Alcotest.(check int) "every timer was cancelled" clock.Clock.armed clock.Clock.cancelled;
      Lwt.return_unit)

let timeout_case =
  pure_case
    "timeout: an expired delivery is cancelled (its resources released) and \
     frees its slot for the next queued job" (fun () ->
      let clock = Clock.create () and fake = Fake.create () in
      let d = make_dispatcher clock fake in
      let* () = Lwt_list.iter_s (fun j -> Lwt.map ignore (submit d j)) [ 1; 2; 3 ] in
      check_stats "2 running" d ~outstanding:3 ~queued:1 ~running:2;
      Clock.expire_all clock;
      Alcotest.(check (list int)) "both hung attempts cancelled" [ 1; 2 ]
        (List.sort compare fake.Fake.cancelled);
      Alcotest.(check (list int)) "queued job then started" [ 1; 2; 3 ] fake.Fake.started;
      check_stats "one left" d ~outstanding:1 ~queued:0 ~running:1;
      let before = clock.Clock.cancelled in
      Fake.resolve fake 3 (Ok ());
      Alcotest.(check int) "its timer is cancelled on success" (before + 1) clock.Clock.cancelled;
      check_stats "drained" d ~outstanding:0 ~queued:0 ~running:0;
      (* A transport that ignores cancellation still cannot hold a slot past
         the timeout. *)
      let stuck = D.create ~sleep:(Clock.sleep clock) ~label:(fun _ -> "test")
          ~transport:(fun _ -> fst (Lwt.wait ())) () in
      let* _ = D.admit stuck (fun () -> Lwt.return ((), Some 1)) in
      check_stats "stuck running" stuck ~outstanding:1 ~queued:0 ~running:1;
      Clock.expire_all clock;
      check_stats "stuck freed" stuck ~outstanding:0 ~queued:0 ~running:0;
      Lwt.return_unit)

let admission_cleanup_case =
  pure_case
    "admission: work that raises, rejects or is cancelled releases its \
     reservation and queues nothing" (fun () ->
      let clock = Clock.create () and fake = Fake.create () in
      let d = make_dispatcher clock fake in
      let* r =
        Lwt.catch
          (fun () -> Lwt.map (fun _ -> "returned") (D.admit d (fun () -> failwith "sync work failure")))
          (function Failure _ -> Lwt.return "raised" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "sync raise propagates" "raised" r;
      let* r =
        Lwt.catch
          (fun () -> Lwt.map (fun _ -> "returned") (D.admit d (fun () -> Lwt.fail (Failure "x"))))
          (function Failure _ -> Lwt.return "raised" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "rejection propagates" "raised" r;
      check_stats "released" d ~outstanding:0 ~queued:0 ~running:0;
      let work, _ = Lwt.task () in
      let admitted = D.admit d (fun () -> work) in
      check_stats "held while working" d ~outstanding:1 ~queued:0 ~running:0;
      Lwt.cancel admitted;
      let* r =
        Lwt.catch (fun () -> Lwt.map (fun _ -> "returned") admitted)
          (function Lwt.Canceled -> Lwt.return "cancelled" | _ -> Lwt.return "other")
      in
      Alcotest.(check string) "cancellation propagates" "cancelled" r;
      check_stats "released after cancel" d ~outstanding:0 ~queued:0 ~running:0;
      Alcotest.(check (list int)) "nothing delivered" [] fake.Fake.started;
      Lwt.return_unit)

let job_after_work_case =
  pure_case "ordering: no delivery starts before the admitted work has returned its job" (fun () ->
      let clock = Clock.create () and fake = Fake.create () in
      let d = make_dispatcher clock fake in
      let committed, commit = Lwt.wait () in
      let r = D.admit d (fun () -> Lwt.map (fun () -> ((), Some 7)) committed) in
      let* () = settle 5 in
      Alcotest.(check (list int)) "nothing started while work runs" [] fake.Fake.started;
      Lwt.wakeup commit ();
      let* _ = r in
      Alcotest.(check (list int)) "started after" [ 7 ] fake.Fake.started;
      Fake.resolve fake 7 (Ok ());
      Lwt.return_unit)

let dispatcher_suite =
  [ default_bounds_case; capacity_case; concurrency_case; failure_isolation_case;
    timeout_case; admission_cleanup_case; job_after_work_case ]

(* ------------------------------------------------------------------ *)
(* Email messages and the real transport against a local server       *)
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

(* A loopback HTTP endpoint the test fully controls. [respond] decides what
   happens after the complete request has been read. *)
let with_local_server ~respond f =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
  Lwt_unix.listen sock 4;
  let port =
    match Lwt_unix.getsockname sock with Unix.ADDR_INET (_, p) -> p | _ -> assert false
  in
  let received, got_request = Lwt.wait () in
  let buf = Bytes.create 4096 in
  let rec read_request fd acc =
    let* n = Lwt_unix.read fd buf 0 (Bytes.length buf) in
    let acc = acc ^ Bytes.sub_string buf 0 n in
    match String.index_opt acc '\n' with
    | _ when n = 0 -> Lwt.return acc
    | _ -> (
        (* Headers end at the first blank line; the body is Content-Length. *)
        let rec find i =
          if i + 3 >= String.length acc then None
          else if String.sub acc i 4 = "\r\n\r\n" then Some (i + 4)
          else find (i + 1)
        in
        match find 0 with
        | None -> read_request fd acc
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
            if String.length acc - body_start >= len then Lwt.return acc
            else read_request fd acc)
  in
  let served =
    let* fd, _ = Lwt_unix.accept sock in
    let* request = read_request fd "" in
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

let fake_key = "b2-fake-provider-key-000"

let stalled_provider_case =
  pure_case
    "transport: a provider that never answers is timed out and the client \
     socket is actually closed" (fun () ->
      with_local_server ~respond:stall_until_client_closes (fun ~port ~received ~served ->
          let endpoint = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/v3/smtp/email" port) in
          let m = Earde.Email.pending_signup_confirmation ~to_email:"stall@b2.invalid" ~token:"stalltoken" in
          (* The dispatcher's timer "expires" exactly when the server holds
             the full request, so the attempt is provably mid-flight. *)
          let* outcome =
            D.run_bounded ~sleep:(fun _ -> Lwt.map ignore received) ~timeout_seconds:1.0
              (Earde.Email.deliver_via ~endpoint ~api_key:fake_key) m
          in
          Alcotest.(check bool) "timed out" true (outcome = `Timed_out);
          let* request = received in
          must "provider saw the request" request "POST /v3/smtp/email";
          (* Bounded only as a safety net: the socket closes immediately on
             cancellation; a leaked connection would stay open forever. *)
          let* closed =
            Lwt.pick
              [ Lwt.map (fun r -> (r :> [ `Client_closed | `Answered | `Still_open ])) served;
                Lwt.map (fun () -> `Still_open) (Lwt_unix.sleep 10.0) ]
          in
          Alcotest.(check bool) "client closed its connection" true (closed = `Client_closed);
          Lwt.return_unit))

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
                let d =
                  D.create ~label:Earde.Email.label
                    ~transport:(Earde.Email.deliver_via ~endpoint ~api_key:fake_key) ()
                in
                let* r = D.admit d (fun () -> Lwt.return ((), Some m)) in
                Alcotest.(check bool) "admitted" true (r = `Admitted ());
                let* _ = served in
                let rec wait n =
                  if (D.stats d).D.outstanding = 0 then Lwt.return_unit
                  else if n > 500 then Alcotest.fail "delivery never settled"
                  else Lwt.bind (Lwt_unix.sleep 0.01) (fun () -> wait (n + 1))
                in
                wait 0)
          in
          let text = logs () in
          must "failure logged by kind and class" text "auth mail password_reset: delivery failed (http_502)";
          must_not "no recipient" text "status@b2.invalid";
          must_not "no token" text "statustoken";
          must_not "no key" text fake_key;
          Lwt.return_unit))

(* Nothing listens on the port: the attempt must fail on its own, promptly,
   not sit in a delivery slot until the timer — which here never fires. *)
let refused_provider_case =
  pure_case "transport: a refused connection fails at once, without waiting for the timeout" (fun () ->
      let probe = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let* () = Lwt_unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) in
      let port =
        match Lwt_unix.getsockname probe with Unix.ADDR_INET (_, p) -> p | _ -> assert false
      in
      let* () = Lwt_unix.close probe in
      let endpoint = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/x" port) in
      let m = Earde.Email.password_reset ~to_email:"refused@b2.invalid" ~token:"refusedtoken" in
      let* outcome =
        Lwt.pick
          [ Lwt.map (fun o -> `Settled o)
              (D.run_bounded ~sleep:(fun _ -> fst (Lwt.task ())) ~timeout_seconds:15.0
                 (Earde.Email.deliver_via ~endpoint ~api_key:fake_key) m);
            Lwt.map (fun () -> `Hung) (Lwt_unix.sleep 10.0) ]
      in
      (match outcome with
       | `Settled (`Failed cls) -> Alcotest.(check string) "failure class" "transport_error" cls
       | `Settled `Delivered -> Alcotest.fail "delivered to a closed port"
       | `Settled `Timed_out -> Alcotest.fail "timer cannot fire here"
       | `Hung -> Alcotest.fail "a refused connection waited for the timeout");
      Lwt.return_unit)

let transport_suite =
  [ message_case; stalled_provider_case; provider_status_case; refused_provider_case ]

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
  mutable started : Earde.Email.message list;
  mutable delivered : Earde.Email.message list;
  mutable gate : unit Lwt.t;
  mutable at_start : Earde.Email.message -> unit Lwt.t;
}

(* A dispatcher whose timer never fires: a stuck job stays stuck, like one
   lost with a restarted process. *)
let fake_dispatcher ?(gate = Lwt.return_unit) () =
  let fm = { started = []; delivered = []; gate; at_start = (fun _ -> Lwt.return_unit) } in
  let transport m =
    fm.started <- fm.started @ [ m ];
    let* () = fm.at_start m in
    let* () = fm.gate in
    fm.delivered <- fm.delivered @ [ m ];
    Lwt.return (Ok ())
  in
  let d =
    D.create ~sleep:(fun _ -> fst (Lwt.task ())) ~label:Earde.Email.label ~transport ()
  in
  (fm, d)

(* Deliveries run after the response; wait for them before the test touches
   its own connection again (Caqti connections are not shareable). *)
let drained d =
  let rec go n =
    if (D.stats d).D.outstanding = 0 then Lwt.return_unit
    else if n > 1000 then Alcotest.fail "dispatcher never drained"
    else Lwt.bind (Lwt_unix.sleep 0.005) (fun () -> go (n + 1))
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

let build_pipeline url =
  Dream.sql_pool ~size:4 url @@ Dream.set_secret "b2-test-secret-value" @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/token" (fun req -> Dream.respond (Dream.csrf_token req));
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

type reply = { status : int; body : string; headers : (string * string) list }

let session_cookie response =
  match
    List.find_opt (fun v -> contains v "dream.session") (Dream.headers response "Set-Cookie")
  with
  | Some v -> (match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)
  | None -> Alcotest.fail "no session cookie"

let form_body fields =
  String.concat "&"
    (List.map (fun (k, v) -> Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v) fields)

(* Every POST comes from a fresh anonymous browser session with its own valid
   CSRF token, like a first visit to the form. *)
let post ~url ~target fields =
  let p = pipeline url in
  let* minted = p (Dream.request ~method_:`GET ~target:"/token" "") in
  let cookie = session_cookie minted in
  let* token = Dream.body minted in
  let request =
    Dream.request ~method_:`POST ~target
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded"); ("Cookie", cookie) ]
      ""
  in
  Dream.set_body request (form_body (("dream.csrf", token) :: fields));
  let* response = p request in
  let* body = Dream.body response in
  Lwt.return
    ( { status = Dream.status_to_int (Dream.status response); body;
        headers = Dream.all_headers response },
      cookie )

let get ~url ?cookie target =
  let p = pipeline url in
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = p (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return { status = Dream.status_to_int (Dream.status response); body; headers = Dream.all_headers response }

(* The comparable part of a response: status, body with CSRF values masked
   (they are per-session random by design), and headers with Set-Cookie
   reduced to the cookie name. *)
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

let shape r =
  let headers =
    List.map
      (fun (k, v) ->
        let k = String.lowercase_ascii k in
        if k = "set-cookie" then (k, match String.index_opt v '=' with Some i -> String.sub v 0 i | None -> v)
        else (k, v))
      r.headers
    |> List.sort compare
  in
  (r.status, mask_csrf r.body, headers)

let same_shape label a b =
  let sa, ba, ha = shape a and sb, bb, hb = shape b in
  Alcotest.(check int) (label ^ ": status") sa sb;
  Alcotest.(check (list (pair string string))) (label ^ ": headers") ha hb;
  if ba <> bb then Alcotest.failf "%s: bodies differ" label

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
      let* () = exec q_pending ("b2x_reserved", "holder@b2.invalid", "holder-old-hash", 24.0) "reserved" in
      let* () = exec q_pending ("b2x_mine", "mine@b2.invalid", "mine-old-hash", 24.0) "own" in
      let* () = exec q_pending ("b2x_stale", "gone@b2.invalid", "stale-old-hash", -1.0) "expired" in
      let* users_before = find_one c q_users_snapshot () "users before" in
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
            (hash_of "b2x_reserved" <> Some "holder-old-hash");
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
          let* old = get ~url ("/confirm-email?token=" ^ "holder-old-token") in
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
      Alcotest.(check int) "provider entered for the two real jobs" 2 (List.length fm.started);
      Alcotest.(check int) "nothing delivered yet" 0 (List.length fm.delivered);
      Lwt.wakeup release ();
      let* () = settle 20 in
      Alcotest.(check (list string)) "both delivered after release"
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
      let* again = signup ~url ~username:"b2x_fullnew" ~email:"fullnew@b2.invalid" () in
      must "recovered" again.body neutral_signup;
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
      let* () = drained d in
      let* r_ok = forgot ~url "durable@b2.invalid" in
      let* () = drained d in
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
