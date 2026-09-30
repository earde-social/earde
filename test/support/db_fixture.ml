(* Connection and query helpers for the cases gated on
   EARDE_TEST_DATABASE_URL, plus plain user rows. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let returning_ids_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM comments WHERE content LIKE 'step3ret %'";
      "DELETE FROM posts WHERE title LIKE 'step3ret %'";
      "DELETE FROM communities WHERE slug = 'step3ret-c'";
      "DELETE FROM users WHERE username IN ('step3ret_author', \
       'step3ret_confirmed', 'step3ret_taken')";
      "DELETE FROM pending_signups WHERE username IN ('step3ret_confirmed', \
       'step3ret_taken', 'step3ret_expired')";
    ]

let reject label = function
  | Ok _ -> Alcotest.failf "%s: unexpectedly accepted" label
  | Error (_ : Caqti_error.t) -> Lwt.return_unit

(* Guards the audit against passing vacuously on a missing table. *)
let q_column_count =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*) FROM information_schema.columns\n\
    \   WHERE table_schema = 'public' AND table_name = $1"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let returning_ids_db_case name f =
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
                 returning_ids_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f conn (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let insert_user conn username =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_user username in
  or_fail ("user " ^ username) uid

(* Second connection for the races; db_case only runs under the gate, so
   the URL is present. *)
let with_second_connection f =
  let url =
    match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
    | Some url -> url
    | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
  in
  let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
  let* conn2 = or_fail "second connect" conn2 in
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  Lwt.finalize (fun () -> f conn2) (fun () -> C2.disconnect ())

let exec conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.exec q arg in
  let* () = or_fail label r in
  Lwt.return_unit

let find conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q arg in
  or_fail label r

let collect conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.collect_list q arg in
  or_fail label r

(* Mutation-first serialization: the second connection executes the
   durable mutation inside an open transaction (its row locks held),
   the review is launched against the main connection, and only then
   does the mutation commit — so the review can never observe the
   pre-mutation committed state and the outcome is deterministic
   without sleeps. *)
let serialized_mutation_first conn2 ~mutate ~launch =
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  let* r = C2.start () in
  let* () = or_fail "second begin" r in
  let* () = mutate () in
  let review_promise = launch () in
  let* r = C2.commit () in
  let* () = or_fail "second commit" r in
  review_promise
