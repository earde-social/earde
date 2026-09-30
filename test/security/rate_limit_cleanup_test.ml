(* Rate_limit_store.cleanup_expired: retention derives from the single
   enforcement window (2x, so no configured window can outlive cleanup), the
   boundary rule is strict-< (a row at exactly now - cleanup_after_seconds is
   kept), active buckets survive, a forced DELETE failure surfaces as a
   bounded Error that carries no fixture IP and leaves the limiter's check
   path fully working, and the failure is transient (cleanup succeeds once
   the fault is removed). Gated like every DB suite; the derivation case is
   pure and always runs. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS rlc_fail_delete ON rate_limits";
      "DROP FUNCTION IF EXISTS rlc_fail_fn()";
      "DELETE FROM rate_limits WHERE ip_address LIKE 'rlc-%'";
    ]

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

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
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f conn (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_row =
  (Caqti_type.(t2 (t2 string string) (t2 int float)) ->. Caqti_type.unit)
    "INSERT INTO rate_limits (ip_address, endpoint, attempts, window_start)\n\
    \   VALUES ($1, $2, $3, $4)"

let q_surviving_ips =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT ip_address FROM rate_limits WHERE ip_address LIKE 'rlc-%' ORDER BY \
     ip_address"

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE OR REPLACE FUNCTION rlc_fail_fn() RETURNS trigger AS 'BEGIN RAISE \
     EXCEPTION ''rlc forced failure''; END' LANGUAGE plpgsql"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER rlc_fail_delete BEFORE DELETE ON rate_limits FOR EACH ROW \
     EXECUTE FUNCTION rlc_fail_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS rlc_fail_delete ON rate_limits"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit) "DROP FUNCTION IF EXISTS rlc_fail_fn()"

(* Pure: the retention rule is derived from the enforcement window, never
   invented — so a longer configured window automatically lengthens
   retention and cleanup can never prune a row a current window needs. *)
let derivation_case =
  Alcotest.test_case "retention derives from the enforcement window" `Quick
    (fun () ->
      Alcotest.(check (float 0.0001))
        "one-minute window" 60.0 Earde.Rate_limit_store.window_seconds;
      Alcotest.(check (float 0.0001))
        "retention is exactly two windows"
        (2.0 *. Earde.Rate_limit_store.window_seconds)
        Earde.Rate_limit_store.cleanup_after_seconds;
      Alcotest.(check bool)
        "retention can never undercut the window" true
        (Earde.Rate_limit_store.cleanup_after_seconds
       >= Earde.Rate_limit_store.window_seconds))

let expiry_case =
  db_case "expired rows go, boundary and active rows stay" (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let now = 2_000_000.0 in
      let retention = Earde.Rate_limit_store.cleanup_after_seconds in
      let insert ip age =
        let* r = C.exec q_insert_row ((ip, "/login"), (3, now -. age)) in
        or_fail "fixture row" r
      in
      let* () = insert "rlc-expired" (retention +. 0.5) in
      (* Exactly at the boundary: the documented strict-< rule keeps it. *)
      let* () = insert "rlc-boundary" retention in
      let* () = insert "rlc-active" 10.0 in
      let* deleted = Earde.Rate_limit_store.cleanup_expired ~now conn in
      let* deleted = or_fail_s "cleanup" deleted in
      Alcotest.(check int) "exactly the expired row" 1 deleted;
      let* ips = C.collect_list q_surviving_ips () in
      let* ips = or_fail "surviving ips" ips in
      Alcotest.(check (list string))
        "boundary and active rows survive"
        [ "rlc-active"; "rlc-boundary" ]
        ips;
      Lwt.return_unit)

let failure_case =
  db_case "cleanup failure is bounded, IP-free, and leaves limiting working"
    (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let now = 2_000_000.0 in
      let retention = Earde.Rate_limit_store.cleanup_after_seconds in
      let* r =
        C.exec q_insert_row
          (("rlc-203.0.113.9", "/login"), (3, now -. retention -. 5.0))
      in
      let* () = or_fail "fixture row" r in
      let* r = C.exec q_create_fail_fn () in
      let* () = or_fail "create fn" r in
      let* r = C.exec q_create_fail_trigger () in
      let* () = or_fail "create trigger" r in
      let* result = Earde.Rate_limit_store.cleanup_expired ~now conn in
      let* err =
        match result with
        | Error e -> Lwt.return e
        | Ok _ -> Alcotest.fail "expected forced cleanup failure"
      in
      (* The DELETE binds only a timestamp — no stored IP can surface in
         the bounded error string the middleware would log. *)
      Alcotest.(check bool)
        "error carries no fixture IP" false
        (Html_assert.contains err "203.0.113");
      (* The limiter's own path is untouched by a broken cleanup. *)
      let* check = Earde.Rate_limit_store.check c "rlc-fresh" "/login" in
      let* check = or_fail_s "check still works" check in
      Alcotest.(check bool) "fresh request allowed" true (check = `Allowed);
      let* r = C.exec q_drop_fail_trigger () in
      let* () = or_fail "drop trigger" r in
      let* r = C.exec q_drop_fail_fn () in
      let* () = or_fail "drop fn" r in
      (* Transient: with the fault removed the same cleanup succeeds. *)
      let* deleted = Earde.Rate_limit_store.cleanup_expired ~now conn in
      let* deleted = or_fail_s "cleanup after recovery" deleted in
      Alcotest.(check bool) "expired rows now removed" true (deleted >= 1);
      Lwt.return_unit)

let suite = [ derivation_case; expiry_case; failure_case ]
let suites = [ ("rate_limit_cleanup", suite) ]
