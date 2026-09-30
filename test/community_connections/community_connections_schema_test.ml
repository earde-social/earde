(* The two new tables' own constraints (migration 20260731120000): the shape
   CHECKs, the unordered-pair partial unique index, the foreign-key deletion
   behavior, and the append-only audit table. Raw rows throughout — the store
   is deliberately not involved, because the database is the subject.
   Database-gated on EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let insert_community = Community_fixture.insert_community
let contains haystack needle = Html_assert.occurs haystack ~needle

(* Audit events RESTRICT-protect both their connection and both communities,
   so they go first; connections then fall away before the communities they
   reference. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM community_connection_audit_events WHERE \
       requester_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccns-%') OR recipient_community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'ccns-%')";
      "DELETE FROM community_connections WHERE requester_community_id IN \
       (SELECT id FROM communities WHERE slug LIKE 'ccns-%') OR \
       recipient_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccns-%')";
      "DELETE FROM communities WHERE slug LIKE 'ccns-%'";
      "DELETE FROM users WHERE username LIKE 'ccns_%'";
    ]

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
               (fun () -> f conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* Raw row fixtures. Every interpolated fragment is either a fixture row id
   produced in this file or a literal written here — no external or user
   value reaches the string, and the statements exist only to put the
   table's own constraints under test. *)
let stmt sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let try_stmt conn sql =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.exec (stmt sql) ()

let row_sql ~requester ~recipient ?(status = "'pending'") ?(note = "NULL")
    ?(requested_by = "NULL") ?(reviewed_by = "NULL") ?(removed_by = "NULL")
    ?(created = "NOW()") ?(updated = "NOW()") ?(reviewed = "NULL")
    ?(removed = "NULL") () =
  Printf.sprintf
    "INSERT INTO community_connections (requester_community_id, \
     recipient_community_id, status, request_note, requested_by_user_id, \
     reviewed_by_user_id, removed_by_user_id, created_at, updated_at, \
     reviewed_at, removed_at) VALUES (%d, %d, %s, %s, %s, %s, %s, %s, %s, %s, \
     %s)"
    requester recipient status note requested_by reviewed_by removed_by created
    updated reviewed removed

let accepts conn label sql =
  let* r = try_stmt conn sql in
  let* () = or_fail label r in
  Lwt.return_unit

let refuses conn label sql =
  let* r = try_stmt conn sql in
  reject label r

let q_count_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connections WHERE \
     LEAST(requester_community_id, recipient_community_id) = LEAST($1, $2) AND \
     GREATEST(requester_community_id, recipient_community_id) = GREATEST($1, \
     $2)"

let q_actors =
  (Caqti_type.int64 ->! Caqti_type.(t3 (option int) (option int) (option int)))
    "SELECT requested_by_user_id, reviewed_by_user_id, removed_by_user_id FROM \
     community_connections WHERE id = $1"

let q_sole_id =
  (Caqti_type.(t2 int int) ->! Caqti_type.int64)
    "SELECT id FROM community_connections WHERE LEAST(requester_community_id, \
     recipient_community_id) = LEAST($1, $2) AND \
     GREATEST(requester_community_id, recipient_community_id) = GREATEST($1, \
     $2) ORDER BY id LIMIT 1"

let q_absent_community_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

let q_indexdefs =
  (Caqti_type.string ->* Caqti_type.string)
    "SELECT indexdef FROM pg_indexes WHERE schemaname = 'public' AND tablename \
     = $1 ORDER BY indexname"

let q_fk_deltypes =
  (Caqti_type.string ->* Caqti_type.(t2 string string))
    "SELECT conname, confdeltype::text FROM pg_constraint WHERE conrelid = \
     $1::regclass AND contype = 'f' ORDER BY conname"

let q_text_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT column_name FROM information_schema.columns WHERE table_schema = \
     'public' AND table_name = 'community_connection_audit_events' AND \
     data_type NOT IN ('integer', 'bigint', 'timestamp with time zone') ORDER \
     BY column_name"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_delete_community =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM communities WHERE id = $1"

let q_insert_audit =
  (Caqti_type.(t2 (t2 string (option int)) (t3 int64 int int))
  ->! Caqti_type.int64)
    "INSERT INTO community_connection_audit_events (action, actor_user_id, \
     connection_id, requester_community_id, recipient_community_id) VALUES \
     ($1, $2, $3, $4, $5) RETURNING id"

let q_audit_actor =
  (Caqti_type.int64 ->! Caqti_type.(option int))
    "SELECT actor_user_id FROM community_connection_audit_events WHERE id = $1"

let q_audit_count =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connection_audit_events WHERE \
     connection_id = $1"

let two_communities conn tag =
  let* a = insert_community conn ("ccns-" ^ tag ^ "-a") in
  let* b = insert_community conn ("ccns-" ^ tag ^ "-b") in
  Lwt.return (a, b)

(* === row shape === *)

let valid_shapes_case =
  db_case "schema: each legal status shape is storable" (fun conn ->
      let* a, b = two_communities conn "shapes" in
      let* uid = insert_user conn "ccns_actor" in
      let actor = string_of_int uid in
      let* () =
        accepts conn "pending"
          (row_sql ~requester:a ~recipient:b ~requested_by:actor ())
      in
      (* The active slot is per unordered pair, so each further shape goes
         on its own pair. *)
      let* c = insert_community conn "ccns-shapes-c" in
      let* () =
        accepts conn "accepted"
          (row_sql ~requester:a ~recipient:c ~status:"'accepted'"
             ~requested_by:actor ~reviewed_by:actor ~reviewed:"NOW()" ())
      in
      let* d = insert_community conn "ccns-shapes-d" in
      let* () =
        accepts conn "rejected"
          (row_sql ~requester:a ~recipient:d ~status:"'rejected'"
             ~requested_by:actor ~reviewed_by:actor ~reviewed:"NOW()" ())
      in
      let* () =
        accepts conn "removed"
          (row_sql ~requester:b ~recipient:c ~status:"'removed'"
             ~requested_by:actor ~reviewed_by:actor ~removed_by:actor
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* n = find conn "pair count" q_count_pair (a, b) in
      Alcotest.(check int) "one row for the first pair" 1 n;
      Lwt.return_unit)

let self_connection_case =
  db_case "schema: a community cannot connect to itself" (fun conn ->
      let* a, _ = two_communities conn "self" in
      refuses conn "self pair" (row_sql ~requester:a ~recipient:a ()))

let status_vocabulary_case =
  db_case "schema: only the four canonical statuses are storable" (fun conn ->
      let* a, b = two_communities conn "vocab" in
      Lwt_list.iter_s
        (fun raw ->
          refuses conn ("status " ^ raw)
            (row_sql ~requester:a ~recipient:b
               ~status:(Printf.sprintf "'%s'" raw)
               ~reviewed:"NOW()" ()))
        [ "Pending"; "PENDING"; "active"; "connected"; "cancelled"; "" ])

let note_length_case =
  db_case "schema: request_note is capped at 2000 characters" (fun conn ->
      let* a, b = two_communities conn "note" in
      let* () =
        accepts conn "2000 characters"
          (row_sql ~requester:a ~recipient:b ~note:"repeat('x', 2000)" ())
      in
      let* c = insert_community conn "ccns-note-c" in
      let* () =
        refuses conn "2001 characters"
          (row_sql ~requester:a ~recipient:c ~note:"repeat('x', 2001)" ())
      in
      (* Counted per character, not per byte, exactly like the domain. *)
      refuses conn "2001 wide characters"
        (row_sql ~requester:a ~recipient:c ~note:"repeat('é', 2001)" ()))

let timestamp_order_case =
  db_case "schema: no timestamp may precede the one it follows" (fun conn ->
      let* a, b = two_communities conn "clock" in
      let* () =
        refuses conn "updated before created"
          (row_sql ~requester:a ~recipient:b
             ~updated:"NOW() - INTERVAL '1 second'" ())
      in
      let* () =
        refuses conn "reviewed before created"
          (row_sql ~requester:a ~recipient:b ~status:"'accepted'"
             ~reviewed:"NOW() - INTERVAL '1 second'" ())
      in
      let* () =
        refuses conn "removed before created"
          (row_sql ~requester:a ~recipient:b ~status:"'removed'"
             ~reviewed:"NOW()" ~removed:"NOW() - INTERVAL '1 second'" ())
      in
      refuses conn "removed before reviewed"
        (row_sql ~requester:a ~recipient:b ~status:"'removed'"
           ~reviewed:"NOW() + INTERVAL '10 seconds'" ~removed:"NOW()" ()))

let pending_shape_case =
  db_case "schema: a pending row carries no review or removal" (fun conn ->
      let* a, b = two_communities conn "pshape" in
      let* uid = insert_user conn "ccns_pshape" in
      let actor = string_of_int uid in
      let* () =
        refuses conn "pending with reviewed_at"
          (row_sql ~requester:a ~recipient:b ~reviewed:"NOW()" ())
      in
      let* () =
        refuses conn "pending with removed_at"
          (row_sql ~requester:a ~recipient:b ~removed:"NOW()" ())
      in
      let* () =
        refuses conn "pending with a reviewer"
          (row_sql ~requester:a ~recipient:b ~reviewed_by:actor ())
      in
      refuses conn "pending with a remover"
        (row_sql ~requester:a ~recipient:b ~removed_by:actor ()))

let reviewed_shape_case =
  db_case "schema: accepted and rejected rows are reviewed, never removed"
    (fun conn ->
      let* a, b = two_communities conn "rshape" in
      let* uid = insert_user conn "ccns_rshape" in
      let actor = string_of_int uid in
      Lwt_list.iter_s
        (fun status ->
          let quoted = Printf.sprintf "'%s'" status in
          let* () =
            refuses conn
              (status ^ " without reviewed_at")
              (row_sql ~requester:a ~recipient:b ~status:quoted ())
          in
          let* () =
            refuses conn
              (status ^ " with removed_at")
              (row_sql ~requester:a ~recipient:b ~status:quoted
                 ~reviewed:"NOW()" ~removed:"NOW()" ())
          in
          refuses conn
            (status ^ " with a remover")
            (row_sql ~requester:a ~recipient:b ~status:quoted ~reviewed:"NOW()"
               ~removed_by:actor ()))
        [ "accepted"; "rejected" ])

let removed_shape_case =
  db_case "schema: a removed row keeps the review that preceded it" (fun conn ->
      let* a, b = two_communities conn "xshape" in
      let* () =
        refuses conn "removed without removed_at"
          (row_sql ~requester:a ~recipient:b ~status:"'removed'"
             ~reviewed:"NOW()" ())
      in
      refuses conn "removed without reviewed_at"
        (row_sql ~requester:a ~recipient:b ~status:"'removed'" ~removed:"NOW()"
           ()))

let nullable_actors_case =
  db_case "schema: every actor column is optional and survives deletion"
    (fun conn ->
      let* a, b = two_communities conn "actors" in
      let* uid = insert_user conn "ccns_gone" in
      let actor = string_of_int uid in
      let* () =
        accepts conn "all actors present"
          (row_sql ~requester:a ~recipient:b ~status:"'removed'"
             ~requested_by:actor ~reviewed_by:actor ~removed_by:actor
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* id = find conn "row id" q_sole_id (a, b) in
      let* () = exec conn "delete actor" q_delete_user uid in
      let* requested, reviewed, removed = find conn "actors" q_actors id in
      Alcotest.(check (option int)) "requester nulled" None requested;
      Alcotest.(check (option int)) "reviewer nulled" None reviewed;
      Alcotest.(check (option int)) "remover nulled" None removed;
      let* n = find conn "row survives" q_count_pair (a, b) in
      Alcotest.(check int) "the connection itself survives" 1 n;
      (* Actor columns are NULL-able from the start, so a row can also be
         written with no actor at all. *)
      let* c = insert_community conn "ccns-actors-c" in
      accepts conn "no actor at all" (row_sql ~requester:a ~recipient:c ()))

let missing_community_case =
  db_case "schema: both sides must reference a real community" (fun conn ->
      let* a, _ = two_communities conn "fk" in
      let* absent = find conn "absent id" q_absent_community_id () in
      let* () =
        refuses conn "absent requester"
          (row_sql ~requester:absent ~recipient:a ())
      in
      refuses conn "absent recipient"
        (row_sql ~requester:a ~recipient:absent ()))

(* === the unordered active pair === *)

let one_active_pair_case =
  db_case "schema: one active connection per unordered pair" (fun conn ->
      let* a, b = two_communities conn "slot" in
      let* () =
        accepts conn "first pending" (row_sql ~requester:a ~recipient:b ())
      in
      let* () =
        refuses conn "second pending, same direction"
          (row_sql ~requester:a ~recipient:b ())
      in
      let* () =
        refuses conn "second pending, reversed direction"
          (row_sql ~requester:b ~recipient:a ())
      in
      let* () =
        refuses conn "accepted alongside pending"
          (row_sql ~requester:a ~recipient:b ~status:"'accepted'"
             ~reviewed:"NOW()" ())
      in
      refuses conn "accepted alongside pending, reversed"
        (row_sql ~requester:b ~recipient:a ~status:"'accepted'"
           ~reviewed:"NOW()" ()))

let history_frees_slot_case =
  db_case "schema: rejected and removed history never occupies the slot"
    (fun conn ->
      let* a, b = two_communities conn "history" in
      let* () =
        accepts conn "rejected history"
          (row_sql ~requester:a ~recipient:b ~status:"'rejected'"
             ~reviewed:"NOW()" ())
      in
      let* () =
        accepts conn "removed history, reversed direction"
          (row_sql ~requester:b ~recipient:a ~status:"'removed'"
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* () =
        accepts conn "a second removed history row"
          (row_sql ~requester:a ~recipient:b ~status:"'removed'"
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* () =
        accepts conn "a fresh request afterwards"
          (row_sql ~requester:b ~recipient:a ())
      in
      let* n = find conn "history retained" q_count_pair (a, b) in
      Alcotest.(check int) "all four rows kept" 4 n;
      refuses conn "but only one active at a time"
        (row_sql ~requester:a ~recipient:b ()))

let index_shape_case =
  db_case "schema: the declared indexes exist with the intended shape"
    (fun conn ->
      let* defs = collect conn "indexes" q_indexdefs "community_connections" in
      Alcotest.(check int)
        "one primary key plus four indexes" 5 (List.length defs);
      let joined = String.concat "\n" defs in
      let unique =
        List.filter
          (fun d ->
            contains d "UNIQUE" && contains d "LEAST" && contains d "GREATEST")
          defs
      in
      (match unique with
      | [ d ] ->
          Alcotest.(check bool)
            "active-pair index is partial" true
            (contains d "WHERE" && contains d "pending" && contains d "accepted")
      | l ->
          Alcotest.failf "expected one unordered-pair unique index, found %d"
            (List.length l));
      Alcotest.(check bool)
        "an incoming-queue index exists" true
        (contains joined "recipient_community_id, status, created_at");
      Alcotest.(check bool)
        "an outgoing-queue index exists" true
        (contains joined "requester_community_id, status, created_at");
      let* audit_defs =
        collect conn "audit indexes" q_indexdefs
          "community_connection_audit_events"
      in
      Alcotest.(check int)
        "one primary key plus four audit indexes" 5 (List.length audit_defs);
      Lwt.return_unit)

let deletion_behavior_case =
  db_case "schema: foreign keys cascade the pair and null the actors"
    (fun conn ->
      let* rows =
        collect conn "connection fks" q_fk_deltypes "community_connections"
      in
      let of_kind kind =
        List.filter (fun (_, d) -> d = kind) rows |> List.length
      in
      Alcotest.(check int) "both communities cascade" 2 (of_kind "c");
      Alcotest.(check int) "all three actors are set null" 3 (of_kind "n");
      Alcotest.(check int) "no other deletion behavior" 5 (List.length rows);
      let* audit_rows =
        collect conn "audit fks" q_fk_deltypes
          "community_connection_audit_events"
      in
      let audit_of_kind kind =
        List.filter (fun (_, d) -> d = kind) audit_rows |> List.length
      in
      Alcotest.(check int) "the audit actor is set null" 1 (audit_of_kind "n");
      Alcotest.(check int)
        "every audit subject is protected" 3 (audit_of_kind "a");
      Alcotest.(check int)
        "no other audit deletion behavior" 4 (List.length audit_rows);
      Lwt.return_unit)

(* === the audit table === *)

let audit_vocabulary_case =
  db_case "schema: the audit action vocabulary is closed" (fun conn ->
      let* a, b = two_communities conn "audit" in
      let* () =
        accepts conn "subject row" (row_sql ~requester:a ~recipient:b ())
      in
      let* id = find conn "row id" q_sole_id (a, b) in
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* () =
        Lwt_list.iter_s
          (fun action ->
            let* r = C.find q_insert_audit ((action, None), (id, a, b)) in
            let* _ = or_fail ("audit " ^ action) r in
            Lwt.return_unit)
          [
            "community_connection_requested";
            "community_connection_accepted";
            "community_connection_rejected";
            "community_connection_removed";
          ]
      in
      let* n = find conn "audit count" q_audit_count id in
      Alcotest.(check int) "four events stored" 4 n;
      Lwt_list.iter_s
        (fun action ->
          let* r = C.find q_insert_audit ((action, None), (id, a, b)) in
          reject ("audit rejects " ^ action) r)
        [
          "connection_requested";
          "community_connection_created";
          "Community_Connection_Accepted";
          "community_connection_removed ";
        ])

let audit_privacy_case =
  db_case "schema: the audit table stores no prose beyond its action"
    (fun conn ->
      let* columns = collect conn "text columns" q_text_columns () in
      Alcotest.(check (list string))
        "the only non-numeric column is the closed action" [ "action" ] columns;
      Lwt.return_unit)

let audit_survives_actor_case =
  db_case "schema: audit events survive their actor's deletion" (fun conn ->
      let* a, b = two_communities conn "aactor" in
      let* uid = insert_user conn "ccns_aactor" in
      let* () =
        accepts conn "subject row" (row_sql ~requester:a ~recipient:b ())
      in
      let* id = find conn "row id" q_sole_id (a, b) in
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* event =
        C.find q_insert_audit
          (("community_connection_requested", Some uid), (id, a, b))
      in
      let* event = or_fail "audit insert" event in
      let* () = exec conn "delete actor" q_delete_user uid in
      let* actor = find conn "actor" q_audit_actor event in
      Alcotest.(check (option int)) "actor nulled, event kept" None actor;
      let* n = find conn "still one event" q_audit_count id in
      Alcotest.(check int) "the event survives" 1 n;
      Lwt.return_unit)

let audit_blocks_deletion_case =
  db_case "schema: audit history confronts a community deletion" (fun conn ->
      let* a, b = two_communities conn "block" in
      let* () =
        accepts conn "subject row" (row_sql ~requester:a ~recipient:b ())
      in
      let* id = find conn "row id" q_sole_id (a, b) in
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* event =
        C.find q_insert_audit
          (("community_connection_requested", None), (id, a, b))
      in
      let* _ = or_fail "audit insert" event in
      (* The community→connection CASCADE cannot run silently past the
         audit event that references both. *)
      let* r = C.exec q_delete_community a in
      let* () = reject "community deletion refused" r in
      let* n = find conn "connection survives" q_count_pair (a, b) in
      Alcotest.(check int) "nothing was cascaded away" 1 n;
      (* Without audit history the cascade is free to run. *)
      let* c = insert_community conn "ccns-block-c" in
      let* d = insert_community conn "ccns-block-d" in
      let* () =
        accepts conn "unaudited row" (row_sql ~requester:c ~recipient:d ())
      in
      let* () = exec conn "delete unaudited community" q_delete_community c in
      let* n = find conn "cascade ran" q_count_pair (c, d) in
      Alcotest.(check int) "the unaudited connection cascaded away" 0 n;
      Lwt.return_unit)

let suite =
  [
    valid_shapes_case;
    self_connection_case;
    status_vocabulary_case;
    note_length_case;
    timestamp_order_case;
    pending_shape_case;
    reviewed_shape_case;
    removed_shape_case;
    nullable_actors_case;
    missing_community_case;
    one_active_pair_case;
    history_frees_slot_case;
    index_shape_case;
    deletion_behavior_case;
    audit_vocabulary_case;
    audit_privacy_case;
    audit_survives_actor_case;
    audit_blocks_deletion_case;
  ]

let suites = [ ("community_connections_schema", suite) ]
