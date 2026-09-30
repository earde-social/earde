module AnT = Earde.Analytics.For_testing

(* Real handlers + real DB + the HTTP stub, EARDE_TEST_DATABASE_URL-gated. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS step7_fail_insert ON posthog_person_deletion_jobs";
      "DROP FUNCTION IF EXISTS step7_fail_insert_fn()";
      "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id IN (SELECT \
       'user:' || u.id::text FROM users u WHERE u.username LIKE 'step7_%')";
      "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id LIKE \
       'user:9700%'"
      (* Orphaned jobs whose fixture user was hard-deleted by a test. *);
      "DELETE FROM posthog_person_deletion_jobs WHERE NOT EXISTS (SELECT 1 \
       FROM users u WHERE 'user:' || u.id::text = \
       posthog_person_deletion_jobs.distinct_id)"
      (* Anonymized fixture leftovers from interrupted runs: their usernames
         no longer match step7_/step6_, so match the anonymization rewrite. *);
      "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id IN (SELECT \
       'user:' || u.id::text FROM users u WHERE u.password_hash = '' AND \
       u.email LIKE 'deleted\\_%@earde.local')";
      "DELETE FROM users WHERE password_hash = '' AND email LIKE \
       'deleted\\_%@earde.local'";
      "DELETE FROM users WHERE username LIKE 'step7_%'";
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
               (fun () -> f ~url conn (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_job =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id) VALUES ($1) \
     RETURNING id"

let q_insert_job_aged =
  (Caqti_type.(t2 string int) ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id, created_at)\n\
    \   VALUES ($1, NOW() - ($2 * INTERVAL '1 hour')) RETURNING id"

let q_backdate_attempt =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs\n\
    \   SET last_attempt_at = NOW() - INTERVAL '2 hours' WHERE id = $1"

(* (status, attempts, last_error) by job id. *)
let q_job_state =
  (Caqti_type.int ->? Caqti_type.(t3 string int (option string)))
    "SELECT status, attempts, last_error FROM posthog_person_deletion_jobs \
     WHERE id = $1"

let q_count_jobs_for =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*) FROM posthog_person_deletion_jobs WHERE distinct_id = $1"

(* plpgsql failure trigger: a DB-only mechanism to force the job INSERT to
   fail inside the transaction — no production test hook. Quoted-body form
   avoids $$, which the Caqti query parser reserves. *)
let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE OR REPLACE FUNCTION step7_fail_insert_fn() RETURNS trigger AS \
     'BEGIN RAISE EXCEPTION ''step7 forced failure''; END' LANGUAGE plpgsql"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER step7_fail_insert BEFORE INSERT ON \
     posthog_person_deletion_jobs FOR EACH ROW EXECUTE FUNCTION \
     step7_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS step7_fail_insert ON posthog_person_deletion_jobs"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS step7_fail_insert_fn()"

let job_state c job_id =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* row = C.find_opt q_job_state job_id in
  or_fail "job state" row

let q_set_profile =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET bio = 'step7 bio', avatar_url = \
     '/static/uploads/step7.webp'\n\
    \   WHERE id = $1"

let q_profile_by_id =
  (Caqti_type.int ->? Caqti_type.(t2 (option string) (option string)))
    "SELECT bio, avatar_url FROM users WHERE id = $1"

let atomic_case =
  db_case "atomic anonymize+enqueue: one idempotent job, user anonymized"
    (fun ~url:_ conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step7_atomic", "x") in
      let* uid = or_fail "user" uid in
      let* r = C.exec q_set_profile uid in
      let* () = or_fail "profile fixture" r in
      let did = "user:" ^ string_of_int uid in
      let* r =
        Earde.Posthog_deletion_job_store.anonymize_and_enqueue conn uid
      in
      let* job_id, distinct_id = or_fail_s "anonymize+enqueue" r in
      Alcotest.(check string) "immutable distinct id" did distinct_id;
      let* name = C.find_opt Analytics_fixture.q_username_by_id uid in
      let* name = or_fail "row" name in
      Alcotest.(check (option string))
        "anonymized"
        (Some (Printf.sprintf "[deleted_%d]" uid))
        name;
      (* The rewrite also erases the user-authored profile fields — the
         settings copy and /privacy both promise their removal. *)
      let* profile = C.find_opt q_profile_by_id uid in
      let* profile = or_fail "profile row" profile in
      (match profile with
      | Some (bio, avatar) ->
          Alcotest.(check (option string)) "bio cleared" None bio;
          Alcotest.(check (option string)) "avatar cleared" None avatar
      | None -> Alcotest.fail "anonymized user row missing");
      let* state = job_state c job_id in
      (match state with
      | Some (status, attempts, last_error) ->
          Alcotest.(check string) "pending" "pending" status;
          Alcotest.(check int) "no attempts yet" 0 attempts;
          Alcotest.(check (option string)) "no error" None last_error
      | None -> Alcotest.fail "job row missing");
      (* Duplicate call converges on the SAME job — no competitors. *)
      let* r2 =
        Earde.Posthog_deletion_job_store.anonymize_and_enqueue conn uid
      in
      let* job_id2, _ = or_fail_s "second call" r2 in
      Alcotest.(check int) "same job id" job_id job_id2;
      let* count = C.find q_count_jobs_for did in
      let* count = or_fail "count" count in
      Alcotest.(check int) "exactly one job" 1 count;
      Lwt.return_unit)

let rollback_case =
  db_case "forced job-insert failure rolls back the anonymization too"
    (fun ~url:_ conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid =
        C.find Analytics_fixture.q_insert_user ("step7_rollback", "x")
      in
      let* uid = or_fail "user" uid in
      let* r = C.exec q_create_fail_fn () in
      let* () = or_fail "create fn" r in
      let* r = C.exec q_create_fail_trigger () in
      let* () = or_fail "create trigger" r in
      let* result =
        Earde.Posthog_deletion_job_store.anonymize_and_enqueue conn uid
      in
      (match result with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "expected forced failure");
      let* r = C.exec q_drop_fail_trigger () in
      let* () = or_fail "drop trigger" r in
      let* r = C.exec q_drop_fail_fn () in
      let* () = or_fail "drop fn" r in
      (* BOTH changes rolled back: username untouched, no job row. *)
      let* name = C.find_opt Analytics_fixture.q_username_by_id uid in
      let* name = or_fail "row" name in
      Alcotest.(check (option string))
        "anonymization rolled back" (Some "step7_rollback") name;
      let* count = C.find q_count_jobs_for ("user:" ^ string_of_int uid) in
      let* count = or_fail "count" count in
      Alcotest.(check int) "no job row" 0 count;
      Lwt.return_unit)

let claim_case =
  db_case "claim: attempts once, lease blocks, stale lease re-eligible"
    (fun ~url:_ conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* job_id = C.find q_insert_job "user:9700001" in
      let* job_id = or_fail "job" job_id in
      let* claimed = Earde.Posthog_deletion_job_store.claim conn job_id in
      let* claimed = or_fail_s "claim" claimed in
      Alcotest.(check (option string))
        "claim returns the distinct id" (Some "user:9700001") claimed;
      let* state = job_state c job_id in
      (match state with
      | Some (_, attempts, _) ->
          Alcotest.(check int) "attempts incremented exactly once" 1 attempts
      | None -> Alcotest.fail "job vanished");
      (* Fresh lease: a concurrent attempt cannot claim it. *)
      let* again = Earde.Posthog_deletion_job_store.claim conn job_id in
      let* again = or_fail_s "concurrent claim" again in
      Alcotest.(check (option string)) "lease blocks reclaim" None again;
      (* Failure records the safe class; the stale lease re-opens the job. *)
      let* r =
        Earde.Posthog_deletion_job_store.mark_failed conn job_id "timeout"
      in
      let* () = or_fail_s "mark failed" r in
      let* r = C.exec q_backdate_attempt job_id in
      let* () = or_fail "backdate" r in
      let* reclaimed = Earde.Posthog_deletion_job_store.claim conn job_id in
      let* reclaimed = or_fail_s "stale reclaim" reclaimed in
      Alcotest.(check (option string))
        "stale lease eligible again" (Some "user:9700001") reclaimed;
      (* Completion clears the error and closes the job for good. *)
      let* r = Earde.Posthog_deletion_job_store.mark_completed conn job_id in
      let* () = or_fail_s "complete" r in
      let* r = C.exec q_backdate_attempt job_id in
      let* () = or_fail "backdate completed" r in
      let* never = Earde.Posthog_deletion_job_store.claim conn job_id in
      let* never = or_fail_s "claim completed" never in
      Alcotest.(check (option string)) "completed jobs never claimed" None never;
      let* state = job_state c job_id in
      (match state with
      | Some (status, attempts, last_error) ->
          Alcotest.(check string) "completed" "completed" status;
          Alcotest.(check int) "two attempts total" 2 attempts;
          Alcotest.(check (option string)) "last_error cleared" None last_error
      | None -> Alcotest.fail "job vanished");
      Lwt.return_unit)

let batch_case =
  db_case "batch claim: bounded and oldest-first" (fun ~url:_ conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* oldest = C.find q_insert_job_aged ("user:9700011", 3) in
      let* oldest = or_fail "oldest" oldest in
      let* middle = C.find q_insert_job_aged ("user:9700012", 2) in
      let* middle = or_fail "middle" middle in
      let* newest = C.find q_insert_job_aged ("user:9700013", 1) in
      let* newest = or_fail "newest" newest in
      let* claimed =
        Earde.Posthog_deletion_job_store.claim_batch conn ~limit:2 ()
      in
      let* claimed = or_fail_s "batch claim" claimed in
      Alcotest.(check (list (pair int string)))
        "bound respected; oldest two, oldest first"
        [ (oldest, "user:9700011"); (middle, "user:9700012") ]
        claimed;
      let* state = job_state c newest in
      (match state with
      | Some (_, attempts, _) ->
          Alcotest.(check int) "unclaimed job untouched" 0 attempts
      | None -> Alcotest.fail "newest vanished");
      Lwt.return_unit)

let batch_worker_case =
  db_case "process_batch: deleted+absent complete, failure stays pending"
    (fun ~url:_ conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* j_found = C.find q_insert_job_aged ("user:9700021", 3) in
      let* j_found = or_fail "found job" j_found in
      let* j_absent = C.find q_insert_job_aged ("user:9700022", 2) in
      let* j_absent = or_fail "absent job" j_absent in
      let* j_fail = C.find q_insert_job_aged ("user:9700023", 1) in
      let* j_fail = or_fail "fail job" j_fail in
      let handler (req : Posthog_persons_stub.req) =
        if req.meth = "DELETE" then (204, "")
        else
          match List.assoc_opt "distinct_id" req.query with
          | Some [ "user:9700021" ] ->
              ( 200,
                Posthog_persons_stub.results_body
                  [
                    Posthog_persons_stub.person_json
                      Posthog_persons_stub.stub_uuid;
                  ] )
          | Some [ "user:9700022" ] ->
              (200, Posthog_persons_stub.results_body [])
          | Some [ "user:9700023" ] -> (500, {|{"detail":"server error"}|})
          | _ -> (404, "")
      in
      let* base_url, _seen, stop = Posthog_persons_stub.start handler in
      AnT.use_deletion_test_configuration ~ui_host:base_url
        ~project_id:(Some "42")
        ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
      Lwt.finalize
        (fun () ->
          (* Same worker the maintenance executable runs. *)
          let* summary =
            Earde.Posthog_deletion.process_batch
              ~claim:(fun () ->
                Earde.Posthog_deletion_job_store.claim_batch conn ~limit:25 ())
              ~mark_completed:(fun job_id ->
                Earde.Posthog_deletion_job_store.mark_completed conn job_id)
              ~mark_failed:(fun job_id err ->
                Earde.Posthog_deletion_job_store.mark_failed conn job_id err)
              ()
          in
          let* summary = or_fail_s "process_batch" summary in
          Alcotest.(check int)
            "claimed" 3 summary.Earde.Posthog_deletion.claimed;
          Alcotest.(check int)
            "completed" 2 summary.Earde.Posthog_deletion.completed;
          Alcotest.(check int)
            "left pending" 1 summary.Earde.Posthog_deletion.left_pending;
          let expect label job expected_status expected_error =
            let* state = job_state c job in
            match state with
            | Some (status, _, last_error) ->
                Alcotest.(check string)
                  (label ^ " status") expected_status status;
                Alcotest.(check (option string))
                  (label ^ " error") expected_error last_error;
                Lwt.return_unit
            | None -> Alcotest.failf "%s vanished" label
          in
          let* () = expect "deleted person" j_found "completed" None in
          let* () = expect "already-absent person" j_absent "completed" None in
          let* () =
            expect "failed lookup" j_fail "pending" (Some "lookup_http_500")
          in
          Lwt.return_unit)
        (fun () ->
          AnT.clear_configuration_override ();
          stop ();
          Lwt.return_unit))

(* Runs the real delete_account_handler, keeping sink + configuration
   installed until the post-response cleanup chain finishes ([done_pred]
   polls the durable job through the case's own connection). *)
let run_delete_account ~url ~configure ?(consent = Some "granted")
    ?(on_capture = fun () -> ()) ~uid ~done_pred () =
  let payloads = ref [] in
  configure ();
  AnT.set_capture_sink (fun p ->
      payloads := !payloads @ [ p ];
      on_capture ());
  Lwt.finalize
    (fun () ->
      let pipeline =
        Dream.sql_pool url @@ Dream.memory_sessions
        @@ fun req ->
        let* () = Dream.set_session_field req "user_id" (string_of_int uid) in
        let* () = Dream.set_session_field req "username" "step7_deleting" in
        let csrf = Dream.csrf_token req in
        Dream.set_body req
          (Http_fixture.encoded_form_body [ ("dream.csrf", csrf) ]);
        Earde.Account_handlers.delete_account_handler req
      in
      let request =
        Dream.request ~method_:`POST ~target:"/delete-account"
          ~headers:
            ([ ("Content-Type", "application/x-www-form-urlencoded") ]
            @ Analytics_fixture.consent_header consent)
          ""
      in
      let* response = pipeline request in
      let* () =
        Analytics_fixture.wait_until ~label:"deletion cleanup chain" done_pred
      in
      Lwt.return (Dream.status_to_int (Dream.status response), !payloads))
    (fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ();
      Lwt.return_unit)

let job_completed conn did () =
  let* job = Earde.Posthog_deletion_job_store.get_by_distinct_id conn did in
  match job with
  | Ok (Some (_, "completed", _, _)) -> Lwt.return true
  | _ -> Lwt.return false

let job_has_error conn did () =
  let* job = Earde.Posthog_deletion_job_store.get_by_distinct_id conn did in
  match job with
  | Ok (Some (_, _, _, Some _)) -> Lwt.return true
  | _ -> Lwt.return false

let drop_job_and_user c ~did ~uid =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* job = Earde.Posthog_deletion_job_store.get_by_distinct_id c did in
  let* () =
    match job with
    | Ok (Some (job_id, _, _, _)) ->
        let* r = C.exec Analytics_fixture.q_delete_job job_id in
        or_fail "drop job" r
    | _ -> Lwt.return_unit
  in
  let* r = C.exec Analytics_fixture.q_delete_user uid in
  or_fail "drop user" r

let stub_config base_url () =
  AnT.use_deletion_test_configuration ~ui_host:base_url ~project_id:(Some "42")
    ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ()

let consented_flow_case =
  db_case "handler: personless metric + real-identity deletion job completes"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step7_flow", "x") in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      let order = ref [] in
      let* base_url, seen, stop =
        Posthog_persons_stub.start (fun req ->
            order := !order @ [ req.Posthog_persons_stub.meth ];
            if req.Posthog_persons_stub.meth = "GET" then
              ( 200,
                Posthog_persons_stub.results_body
                  [
                    Posthog_persons_stub.person_json
                      Posthog_persons_stub.stub_uuid;
                  ] )
            else (204, ""))
      in
      Lwt.finalize
        (fun () ->
          let* status, payloads =
            run_delete_account ~url ~configure:(stub_config base_url) ~uid
              ~on_capture:(fun () -> order := !order @ [ "capture" ])
              ~done_pred:(job_completed conn did) ()
          in
          Alcotest.(check bool)
            "response preserved (redirect)" true
            (Http_fixture.is_redirect status);
          (match payloads with
          | [ p ] ->
              Alcotest.(check string)
                "event" "account_deleted"
                (Analytics_fixture.event_of p);
              Alcotest.(check string)
                "constant non-user distinct id"
                Earde.Analytics.account_deletion_distinct_id
                (Analytics_fixture.distinct_of p);
              Alcotest.(check bool)
                "metric never mentions the deleted user" false
                (Html_assert.contains (Yojson.Safe.to_string p) did)
          | l -> Alcotest.failf "expected 1 metric, got %d" (List.length l));
          (* HTTP request sequence only — the metric request happens to run
             first, but person-safety comes from the metric being personless
             by construction, NOT from this ordering (which proves nothing
             about PostHog's ingestion pipeline). *)
          Alcotest.(check (list string))
            "HTTP request sequence: metric, lookup, delete"
            [ "capture"; "GET"; "DELETE" ]
            !order;
          (* The Persons lookup and the durable job keep the REAL
             user:<database_id>. *)
          (match
             List.find_opt
               (fun (q : Posthog_persons_stub.req) -> q.meth = "GET")
               !seen
           with
          | Some lookup ->
              Alcotest.(check (option (list string)))
                "Persons lookup uses the real user distinct id" (Some [ did ])
                (List.assoc_opt "distinct_id" lookup.Posthog_persons_stub.query)
          | None -> Alcotest.fail "no Persons lookup recorded");
          let* job =
            Earde.Posthog_deletion_job_store.get_by_distinct_id conn did
          in
          let* job = or_fail_s "job" job in
          (match job with
          | Some (_, status, attempts, last_error) ->
              Alcotest.(check string) "completed" "completed" status;
              Alcotest.(check int) "one attempt" 1 attempts;
              Alcotest.(check (option string)) "no error" None last_error
          | None -> Alcotest.fail "no job");
          let* name = C.find_opt Analytics_fixture.q_username_by_id uid in
          let* name = or_fail "row" name in
          Alcotest.(check (option string))
            "anonymized"
            (Some (Printf.sprintf "[deleted_%d]" uid))
            name;
          drop_job_and_user c ~did ~uid)
        (fun () ->
          stop ();
          Lwt.return_unit))

let denied_consent_case =
  db_case "handler: denied consent skips capture, still deletes remotely"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step7_denied", "x") in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      let* base_url, seen, stop =
        Posthog_persons_stub.start (fun req ->
            if req.Posthog_persons_stub.meth = "GET" then
              (200, Posthog_persons_stub.results_body [])
            else (404, ""))
      in
      Lwt.finalize
        (fun () ->
          let* status, payloads =
            run_delete_account ~url ~configure:(stub_config base_url)
              ~consent:(Some "denied") ~uid ~done_pred:(job_completed conn did)
              ()
          in
          Alcotest.(check bool)
            "redirects" true
            (Http_fixture.is_redirect status);
          Alcotest.(check int)
            "no capture without consent" 0 (List.length payloads);
          Alcotest.(check bool)
            "deletion still attempted" true
            (List.length !seen >= 1);
          drop_job_and_user c ~did ~uid)
        (fun () ->
          stop ();
          Lwt.return_unit))

let capture_failure_case =
  db_case "handler: capture transport failure still proceeds to deletion"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid =
        C.find Analytics_fixture.q_insert_user ("step7_capfail", "x")
      in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      let* base_url, seen, stop =
        Posthog_persons_stub.start (fun req ->
            if req.Posthog_persons_stub.meth = "GET" then
              (200, Posthog_persons_stub.results_body [])
            else (404, ""))
      in
      Lwt.finalize
        (fun () ->
          let* status, _payloads =
            run_delete_account ~url ~configure:(stub_config base_url) ~uid
              ~on_capture:(fun () -> failwith "capture transport down")
              ~done_pred:(job_completed conn did) ()
          in
          Alcotest.(check bool)
            "redirects" true
            (Http_fixture.is_redirect status);
          Alcotest.(check bool)
            "deletion still attempted" true
            (List.length !seen >= 1);
          let* job =
            Earde.Posthog_deletion_job_store.get_by_distinct_id conn did
          in
          let* job = or_fail_s "job" job in
          (match job with
          | Some (_, status, _, _) ->
              Alcotest.(check string)
                "completed despite capture failure" "completed" status
          | None -> Alcotest.fail "no job");
          drop_job_and_user c ~did ~uid)
        (fun () ->
          stop ();
          Lwt.return_unit))

let missing_config_case =
  db_case "handler: missing configuration leaves a durable pending job"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step7_noconf", "x") in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      let* status, payloads =
        run_delete_account ~url ~configure:AnT.use_enabled_test_configuration
          ~uid ~done_pred:(job_has_error conn did) ()
      in
      Alcotest.(check bool)
        "local deletion still succeeds" true
        (Http_fixture.is_redirect status);
      Alcotest.(check int)
        "capture still emitted (consented)" 1 (List.length payloads);
      let* job = Earde.Posthog_deletion_job_store.get_by_distinct_id conn did in
      let* job = or_fail_s "job" job in
      (match job with
      | Some (_, status, attempts, last_error) ->
          Alcotest.(check string) "pending" "pending" status;
          Alcotest.(check int) "one attempt" 1 attempts;
          Alcotest.(check (option string))
            "safe marker" (Some "missing_configuration") last_error
      | None -> Alcotest.fail "no durable job");
      drop_job_and_user c ~did ~uid)

let posthog_down_case =
  db_case "handler: PostHog failure never changes the product response"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step7_phdown", "x") in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      let* base_url, _seen, stop =
        Posthog_persons_stub.start (fun _req ->
            (500, {|{"detail":"server error"}|}))
      in
      Lwt.finalize
        (fun () ->
          let* status, _payloads =
            run_delete_account ~url ~configure:(stub_config base_url) ~uid
              ~done_pred:(job_has_error conn did) ()
          in
          Alcotest.(check bool)
            "redirects" true
            (Http_fixture.is_redirect status);
          let* job =
            Earde.Posthog_deletion_job_store.get_by_distinct_id conn did
          in
          let* job = or_fail_s "job" job in
          (match job with
          | Some (_, status, _, last_error) ->
              Alcotest.(check string) "pending" "pending" status;
              Alcotest.(check (option string))
                "safe status class" (Some "lookup_http_500") last_error
          | None -> Alcotest.fail "no job");
          let* name = C.find_opt Analytics_fixture.q_username_by_id uid in
          let* name = or_fail "row" name in
          Alcotest.(check (option string))
            "local deletion applied anyway"
            (Some (Printf.sprintf "[deleted_%d]" uid))
            name;
          drop_job_and_user c ~did ~uid)
        (fun () ->
          stop ();
          Lwt.return_unit))

let suite =
  [
    atomic_case;
    rollback_case;
    claim_case;
    batch_case;
    batch_worker_case;
    consented_flow_case;
    denied_consent_case;
    capture_failure_case;
    missing_config_case;
    posthog_down_case;
  ]

let suites = [ ("posthog_deletion_jobs", suite) ]
