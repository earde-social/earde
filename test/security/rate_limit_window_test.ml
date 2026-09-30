(* The rate-limit window is 60 seconds from a bucket's first attempt, at the
   resolution the clock gives. window_start used to be a REAL column: at
   current epochs a float4 only holds multiples of 128 seconds, so a window
   snapped to 128-second steps and a step boundary reset a bucket's count
   mid-window. The attempts below straddle such a boundary (the midpoint
   between two representable float4 values) one second apart. Gated like
   every DB suite. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let ip = "rlw-127.0.0.1"
let endpoint = "/rlw-login"

let q_cleanup =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DELETE FROM rate_limits WHERE ip_address LIKE 'rlw-%'"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

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
               let* r = C.exec q_cleanup () in
               or_fail "cleanup" r
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let decide conn ~now =
  let* r = Earde.Rate_limit_store.check ~now conn ip endpoint in
  match r with Ok d -> Lwt.return d | Error e -> Alcotest.failf "check: %s" e

let decision =
  Alcotest.testable
    (fun ppf d ->
      Format.pp_print_string ppf
        (match d with `Allowed -> "Allowed" | `Blocked -> "Blocked"))
    ( = )

(* A float4 step is 128 s wide for epochs in [2^30, 2^31); the midpoint
   between two representable values is where rounding flips. *)
let straddle_midpoint () =
  let step = 128.0 in
  let base = Float.of_int (int_of_float (Unix.gettimeofday () /. step)) in
  (base *. step) +. (step /. 2.0)

let straddle_case =
  db_case
    "window: the sixth attempt one second after the first is blocked, even \
     across a float4 rounding boundary" (fun conn ->
      let mid = straddle_midpoint () in
      let first = mid -. 0.5 in
      let rec five n =
        if n = 0 then Lwt.return_unit
        else
          let* d = decide conn ~now:first in
          Alcotest.(check decision) "within the allowance" `Allowed d;
          five (n - 1)
      in
      let* () = five 5 in
      let* sixth = decide conn ~now:(first +. 1.0) in
      Alcotest.(check decision) "sixth in the window" `Blocked sixth;
      Lwt.return_unit)

let window_case =
  db_case "window: still blocked just inside 60 s, a fresh count after it"
    (fun conn ->
      let first = straddle_midpoint () -. 30.0 in
      let rec five n =
        if n = 0 then Lwt.return_unit
        else
          let* _ = decide conn ~now:first in
          five (n - 1)
      in
      let* () = five 5 in
      let* inside = decide conn ~now:(first +. 59.0) in
      Alcotest.(check decision) "59 s later" `Blocked inside;
      let* after = decide conn ~now:(first +. 61.0) in
      Alcotest.(check decision) "61 s later, a new window" `Allowed after;
      Lwt.return_unit)

let suites = [ ("rate_limit_window", [ straddle_case; window_case ]) ]
