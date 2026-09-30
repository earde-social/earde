(* The two new tables' durable constraints: lifecycle shapes, the
   active-(post, destination) partial uniqueness, deletion behavior, the
   audit vocabulary and its subject protection, and the notification
   shape branch with its deduplication. Database-gated on
   EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail

let reject = Db_fixture.reject

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

(* Legacy (non-network) communities: the network lifecycle CHECK ties a
   network community's visibility flags to its onboarding state, and
   these cases drift the flags freely. connection_eligible is
   deliberately blind to is_network_community, so nothing is lost. *)
let insert_community conn slug =
  Community_fixture.insert_community ~network:false conn slug

(* Audit events protect their placement, post, and both communities with
   no-action FKs, so they go first; placements next (they also cascade
   from posts, but only once the audit rows are gone); then posts before
   the users and communities they reference. Notifications cascade from
   their subjects but are deleted explicitly so recipient-side fixture
   users can always be dropped. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications \
       WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'stps_%')"
    ; "DELETE FROM notifications \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'stps-%')"
    ; "DELETE FROM shared_thread_placement_audit_events \
       WHERE origin_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'stps-%') \
          OR destination_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'stps-%')"
    ; "DELETE FROM shared_thread_placements \
       WHERE origin_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'stps-%') \
          OR destination_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'stps-%')"
    ; "DELETE FROM posts \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'stps-%')"
    ; "DELETE FROM community_sections \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'stps-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'stps-%'"
    ; "DELETE FROM users WHERE username LIKE 'stps_%'"
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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* Raw row fixtures. Every interpolated fragment is either a fixture row
   id produced in this file or a literal written here — no external or
   user value reaches the string, and the statements exist only to put
   the table's own constraints under test. *)
let stmt sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let try_stmt conn sql =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.exec (stmt sql) ()

let accepts conn label sql =
  let* r = try_stmt conn sql in
  let* () = or_fail label r in
  Lwt.return_unit

let refuses conn label sql =
  let* r = try_stmt conn sql in
  reject label r

let row_sql ~post ~origin ~destination ?(status = "'pending'")
    ?(section = "NULL") ?(note = "NULL") ?(requested_by = "NULL")
    ?(reviewed_by = "NULL") ?(removed_by = "NULL") ?(withdrawn_by = "NULL")
    ?(created = "NOW()") ?(updated = "NOW()") ?(reviewed = "NULL")
    ?(removed = "NULL") ?(withdrawn = "NULL") () =
  Printf.sprintf
    "INSERT INTO shared_thread_placements \
       (post_id, origin_community_id, destination_community_id, \
        destination_section_id, status, request_note, \
        requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
        withdrawn_by_user_id, created_at, updated_at, reviewed_at, \
        removed_at, withdrawn_at) \
     VALUES (%d, %d, %d, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)"
    post origin destination section status note requested_by reviewed_by
    removed_by withdrawn_by created updated reviewed removed withdrawn

let q_insert_post =
  (Caqti_type.(t3 int int (option int)) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, community_id, user_id, section_id) \
   VALUES ('stp thread', 'stp body', $1, $2, $3) RETURNING id"

let insert_post ?section conn ~community ~author =
  find conn "post fixture" q_insert_post (community, author, section)

let q_insert_section =
  (Caqti_type.(t2 int string) ->! Caqti_type.int)
  "INSERT INTO community_sections (community_id, name, slug) \
   VALUES ($1, $2, $2) RETURNING id"

let q_delete_section =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM community_sections WHERE id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_count_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COUNT(*) FROM shared_thread_placements \
   WHERE post_id = $1 AND destination_community_id = $2"

let q_sole_id =
  (Caqti_type.(t2 int int) ->! Caqti_type.int64)
  "SELECT id FROM shared_thread_placements \
   WHERE post_id = $1 AND destination_community_id = $2 \
   ORDER BY id LIMIT 1"

let q_actors =
  (Caqti_type.int64
   ->! Caqti_type.(t4 (option int) (option int) (option int) (option int)))
  "SELECT requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
          withdrawn_by_user_id \
   FROM shared_thread_placements WHERE id = $1"

let q_section_status =
  (Caqti_type.int64 ->! Caqti_type.(t2 (option int) string))
  "SELECT destination_section_id, status \
   FROM shared_thread_placements WHERE id = $1"

let q_indexdefs =
  (Caqti_type.string ->* Caqti_type.string)
  "SELECT indexdef FROM pg_indexes \
   WHERE schemaname = 'public' AND tablename = $1 ORDER BY indexname"

let q_insert_audit =
  (Caqti_type.(t2 (t3 string (option int) int64) (t3 int int int))
   ->! Caqti_type.int64)
  "INSERT INTO shared_thread_placement_audit_events \
     (action, actor_user_id, placement_id, post_id, \
      origin_community_id, destination_community_id) \
   VALUES ($1, $2, $3, $4, $5, $6) RETURNING id"

let q_audit_actor =
  (Caqti_type.int64 ->! Caqti_type.(option int))
  "SELECT actor_user_id FROM shared_thread_placement_audit_events \
   WHERE id = $1"

let q_insert_stp_notif =
  (Caqti_type.(t2 (t2 int string) (t2 int int64)) ->! Caqti_type.int)
  "INSERT INTO notifications \
     (user_id, notif_type, community_id, shared_thread_placement_id) \
   VALUES ($1, $2, $3, $4) RETURNING id"

let base conn tag =
  let* author = insert_user conn ("stps_" ^ tag) in
  let* o = insert_community conn ("stps-" ^ tag ^ "-o") in
  let* d = insert_community conn ("stps-" ^ tag ^ "-d") in
  let* post = insert_post conn ~community:o ~author in
  Lwt.return (author, o, d, post)

(* === row shape === *)

let valid_shapes_case =
  db_case "schema: each legal status shape is storable" (fun conn ->
      let* author, o, d, post = base conn "shapes" in
      let actor = string_of_int author in
      let* () =
        accepts conn "pending"
          (row_sql ~post ~origin:o ~destination:d ~requested_by:actor ())
      in
      (* The active slot is per (post, destination), so the accepted
         shape goes to its own destination; the historical shapes share
         the first pair freely. *)
      let* e = insert_community conn "stps-shapes-e" in
      let* section = find conn "section" q_insert_section (e, "gen") in
      let* () =
        accepts conn "accepted with section"
          (row_sql ~post ~origin:o ~destination:e ~status:"'accepted'"
             ~section:(string_of_int section) ~requested_by:actor
             ~reviewed_by:actor ~reviewed:"NOW()" ())
      in
      let* () =
        accepts conn "rejected"
          (row_sql ~post ~origin:o ~destination:d ~status:"'rejected'"
             ~requested_by:actor ~reviewed_by:actor ~reviewed:"NOW()" ())
      in
      let* () =
        accepts conn "removed"
          (row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
             ~requested_by:actor ~reviewed_by:actor ~removed_by:actor
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* () =
        accepts conn "withdrawn"
          (row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
             ~requested_by:actor ~withdrawn_by:actor ~withdrawn:"NOW()" ())
      in
      let* n = find conn "pair count" q_count_pair (post, d) in
      Alcotest.(check int) "four rows on the shared pair" 4 n;
      Lwt.return_unit)

let same_community_case =
  db_case "schema: origin and destination cannot be equal" (fun conn ->
      let* _, o, _, post = base conn "self" in
      refuses conn "self pair"
        (row_sql ~post ~origin:o ~destination:o ()))

let status_vocabulary_case =
  db_case "schema: only the five canonical statuses are storable"
    (fun conn ->
      let* _, o, d, post = base conn "vocab" in
      Lwt_list.iter_s
        (fun raw ->
          refuses conn ("status " ^ raw)
            (row_sql ~post ~origin:o ~destination:d
               ~status:(Printf.sprintf "'%s'" raw) ~reviewed:"NOW()"
               ~withdrawn:"NOW()" ()))
        [ "Pending"; "PENDING"; "active"; "cancelled"; "withdraw"; "" ])

let lifecycle_shape_case =
  db_case "schema: every incoherent lifecycle shape is refused" (fun conn ->
      let* _, o, d, post = base conn "badshape" in
      let* section = find conn "section" q_insert_section (d, "gen") in
      let section = string_of_int section in
      Lwt_list.iter_s
        (fun (label, sql) -> refuses conn label sql)
        [ ( "pending with a review time"
          , row_sql ~post ~origin:o ~destination:d ~reviewed:"NOW()" () )
        ; ( "pending with a section"
          , row_sql ~post ~origin:o ~destination:d ~section () )
        ; ( "pending with a withdrawal time"
          , row_sql ~post ~origin:o ~destination:d ~withdrawn:"NOW()" () )
        ; ( "accepted without a review time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'accepted'" ()
          )
        ; ( "accepted with a withdrawal time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'accepted'"
              ~reviewed:"NOW()" ~withdrawn:"NOW()" () )
        ; ( "rejected with a section"
          , row_sql ~post ~origin:o ~destination:d ~status:"'rejected'"
              ~reviewed:"NOW()" ~section () )
        ; ( "removed without a review time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
              ~removed:"NOW()" () )
        ; ( "removed without a removal time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
              ~reviewed:"NOW()" () )
        ; ( "removed with a withdrawal time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
              ~reviewed:"NOW()" ~removed:"NOW()" ~withdrawn:"NOW()" () )
        ; ( "withdrawn with a review time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
              ~withdrawn:"NOW()" ~reviewed:"NOW()" () )
        ; ( "withdrawn with a section"
          , row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
              ~withdrawn:"NOW()" ~section () )
        ; ( "withdrawn without a withdrawal time"
          , row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'" ()
          )
        ])

let timestamp_order_case =
  db_case "schema: timestamps cannot precede their causes" (fun conn ->
      let* _, o, d, post = base conn "clock" in
      Lwt_list.iter_s
        (fun (label, sql) -> refuses conn label sql)
        [ ( "review before creation"
          , row_sql ~post ~origin:o ~destination:d ~status:"'accepted'"
              ~reviewed:"NOW() - INTERVAL '1 hour'" () )
        ; ( "removal before review"
          , row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
              ~created:"NOW() - INTERVAL '2 hour'"
              ~updated:"NOW() - INTERVAL '2 hour'" ~reviewed:"NOW()"
              ~removed:"NOW() - INTERVAL '1 hour'" () )
        ; ( "withdrawal before creation"
          , row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
              ~withdrawn:"NOW() - INTERVAL '1 hour'" () )
        ; ( "update before creation"
          , row_sql ~post ~origin:o ~destination:d
              ~updated:"NOW() - INTERVAL '1 hour'" () )
        ])

let note_length_case =
  db_case "schema: request_note is capped at 2000 characters" (fun conn ->
      let* _, o, d, post = base conn "note" in
      let* () =
        accepts conn "2000 fits"
          (row_sql ~post ~origin:o ~destination:d
             ~note:("'" ^ String.make 2000 'a' ^ "'") ())
      in
      let* e = insert_community conn "stps-note-e" in
      refuses conn "2001 refused"
        (row_sql ~post ~origin:o ~destination:e
           ~note:("'" ^ String.make 2001 'a' ^ "'") ()))

let one_active_case =
  db_case "schema: at most one active placement per post and destination"
    (fun conn ->
      let* author, o, d, post = base conn "active" in
      let* () =
        accepts conn "first pending" (row_sql ~post ~origin:o ~destination:d ())
      in
      let* () =
        refuses conn "second pending"
          (row_sql ~post ~origin:o ~destination:d ())
      in
      (* An accepted row occupies the slot exactly like a pending one. *)
      let* e = insert_community conn "stps-active-e" in
      let* () =
        accepts conn "accepted elsewhere"
          (row_sql ~post ~origin:o ~destination:e ~status:"'accepted'"
             ~reviewed:"NOW()" ())
      in
      let* () =
        refuses conn "pending beside accepted"
          (row_sql ~post ~origin:o ~destination:e ())
      in
      (* Distinct destinations and distinct posts each get their own
         slot. *)
      let* f = insert_community conn "stps-active-f" in
      let* () =
        accepts conn "third destination"
          (row_sql ~post ~origin:o ~destination:f ())
      in
      let* post2 = insert_post conn ~community:o ~author in
      let* () =
        accepts conn "second post, same destination"
          (row_sql ~post:post2 ~origin:o ~destination:d ())
      in
      let* n = find conn "pair count" q_count_pair (post, d) in
      Alcotest.(check int) "one row for the contested pair" 1 n;
      Lwt.return_unit)

let history_frees_slot_case =
  db_case "schema: every terminal state frees the active slot" (fun conn ->
      let* _, o, d, post = base conn "hist" in
      let* () =
        accepts conn "rejected history"
          (row_sql ~post ~origin:o ~destination:d ~status:"'rejected'"
             ~reviewed:"NOW()" ())
      in
      let* () =
        accepts conn "removed history"
          (row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* () =
        accepts conn "withdrawn history"
          (row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
             ~withdrawn:"NOW()" ())
      in
      let* () =
        accepts conn "fresh pending beside the whole history"
          (row_sql ~post ~origin:o ~destination:d ())
      in
      let* n = find conn "pair count" q_count_pair (post, d) in
      Alcotest.(check int) "history retained beside the fresh row" 4 n;
      Lwt.return_unit)

let actor_deletion_case =
  db_case "schema: deleting an actor keeps the row and nulls provenance"
    (fun conn ->
      let* _, o, d, post = base conn "ghost" in
      (* A dedicated actor who authored nothing, so the user row can be
         deleted (posts block their author's deletion). *)
      let* actor = insert_user conn "stps_ghost_actor" in
      let a = string_of_int actor in
      let* () =
        accepts conn "removed row"
          (row_sql ~post ~origin:o ~destination:d ~status:"'removed'"
             ~requested_by:a ~reviewed_by:a ~removed_by:a
             ~reviewed:"NOW()" ~removed:"NOW()" ())
      in
      let* () =
        accepts conn "withdrawn row"
          (row_sql ~post ~origin:o ~destination:d ~status:"'withdrawn'"
             ~requested_by:a ~withdrawn_by:a ~withdrawn:"NOW()" ())
      in
      let* () = exec conn "delete actor" q_delete_user actor in
      let* id = find conn "removed id" q_sole_id (post, d) in
      let* requested, reviewed, removed, withdrawn =
        find conn "actors" q_actors id
      in
      Alcotest.(check (option int)) "requester nulled" None requested;
      Alcotest.(check (option int)) "reviewer nulled" None reviewed;
      Alcotest.(check (option int)) "remover nulled" None removed;
      Alcotest.(check (option int)) "withdrawer nulled" None withdrawn;
      let* n = find conn "rows kept" q_count_pair (post, d) in
      Alcotest.(check int) "both rows survive" 2 n;
      Lwt.return_unit)

let section_set_null_case =
  db_case "schema: deleting a destination section releases the placement"
    (fun conn ->
      let* _, o, d, post = base conn "setnull" in
      let* section = find conn "section" q_insert_section (d, "gen") in
      let* () =
        accepts conn "accepted into the section"
          (row_sql ~post ~origin:o ~destination:d ~status:"'accepted'"
             ~section:(string_of_int section) ~reviewed:"NOW()" ())
      in
      let* () = exec conn "delete section" q_delete_section section in
      let* id = find conn "placement id" q_sole_id (post, d) in
      let* stored_section, status =
        find conn "after deletion" q_section_status id
      in
      Alcotest.(check (option int)) "section released" None stored_section;
      Alcotest.(check string) "acceptance survives" "accepted" status;
      Lwt.return_unit)

let index_shape_case =
  db_case "schema: the active partial unique index exists as declared"
    (fun conn ->
      let* defs =
        collect conn "indexdefs" q_indexdefs "shared_thread_placements"
      in
      let uniq =
        List.filter
          (fun d ->
            Html_assert.occurs d
              ~needle:"shared_thread_placements_one_active_destination_idx")
          defs
      in
      match uniq with
      | [ def ] ->
          List.iter
            (fun needle ->
              if not (Html_assert.occurs def ~needle) then
                Alcotest.failf "index definition lacks %S: %s" needle def)
            [ "UNIQUE"; "post_id"; "destination_community_id"; "pending"
            ; "accepted" ];
          Lwt.return_unit
      | _ -> Alcotest.fail "the active partial unique index is missing")

(* === audit === *)

let audit_vocabulary_case =
  db_case "schema: the audit action vocabulary is closed" (fun conn ->
      let* author, o, d, post = base conn "audvoc" in
      let* () =
        accepts conn "placement"
          (row_sql ~post ~origin:o ~destination:d ())
      in
      let* placement = find conn "placement id" q_sole_id (post, d) in
      let* () =
        Lwt_list.iter_s
          (fun action ->
            let* _ =
              find conn ("audit " ^ action) q_insert_audit
                ((action, Some author, placement), (post, o, d))
            in
            Lwt.return_unit)
          [ "shared_thread_requested"; "shared_thread_accepted"
          ; "shared_thread_rejected"; "shared_thread_removed"
          ; "shared_thread_withdrawn" ]
      in
      refuses conn "off-vocabulary action"
        (Printf.sprintf
           "INSERT INTO shared_thread_placement_audit_events \
              (action, placement_id, post_id, origin_community_id, \
               destination_community_id) \
            VALUES ('shared_thread_created', %Ld, %d, %d, %d)"
           placement post o d))

let audit_protects_subjects_case =
  db_case "schema: audit events survive actors and block subject deletion"
    (fun conn ->
      let* _, o, d, post = base conn "audlock" in
      let* actor = insert_user conn "stps_audlock_actor" in
      let* () =
        accepts conn "placement" (row_sql ~post ~origin:o ~destination:d ())
      in
      let* placement = find conn "placement id" q_sole_id (post, d) in
      let* event =
        find conn "event" q_insert_audit
          (("shared_thread_requested", Some actor, placement), (post, o, d))
      in
      (* Subjects are protected while the event exists... *)
      let* () =
        refuses conn "placement deletion blocked"
          (Printf.sprintf
             "DELETE FROM shared_thread_placements WHERE id = %Ld" placement)
      in
      let* () =
        refuses conn "post deletion blocked"
          (Printf.sprintf "DELETE FROM posts WHERE id = %d" post)
      in
      let* () =
        refuses conn "community deletion blocked"
          (Printf.sprintf "DELETE FROM communities WHERE id = %d" d)
      in
      (* ...while a deleted actor only nulls provenance. *)
      let* () = exec conn "delete actor" q_delete_user actor in
      let* stored = find conn "actor" q_audit_actor event in
      Alcotest.(check (option int)) "actor nulled, event kept" None stored;
      Lwt.return_unit)

(* === notifications === *)

let notif_shape_case =
  db_case "schema: the shared-thread notification branch is exclusive"
    (fun conn ->
      let* author, o, d, post = base conn "nshape" in
      let* () =
        accepts conn "placement" (row_sql ~post ~origin:o ~destination:d ())
      in
      let* placement = find conn "placement id" q_sole_id (post, d) in
      let* _ =
        find conn "valid structured row" q_insert_stp_notif
          ((author, "shared_thread_requested"), (d, placement))
      in
      Lwt_list.iter_s
        (fun (label, sql) -> refuses conn label sql)
        [ ( "prose on a structured kind"
          , Printf.sprintf
              "INSERT INTO notifications \
                 (user_id, notif_type, community_id, \
                  shared_thread_placement_id, message) \
               VALUES (%d, 'shared_thread_accepted', %d, %Ld, 'hello')"
              author d placement )
        ; ( "post link on a structured kind"
          , Printf.sprintf
              "INSERT INTO notifications \
                 (user_id, notif_type, community_id, \
                  shared_thread_placement_id, post_id) \
               VALUES (%d, 'shared_thread_accepted', %d, %Ld, %d)"
              author d placement post )
        ; ( "structured kind without a community"
          , Printf.sprintf
              "INSERT INTO notifications \
                 (user_id, notif_type, shared_thread_placement_id) \
               VALUES (%d, 'shared_thread_rejected', %Ld)"
              author placement )
        ; ( "structured kind without a placement"
          , Printf.sprintf
              "INSERT INTO notifications \
                 (user_id, notif_type, community_id) \
               VALUES (%d, 'shared_thread_rejected', %d)"
              author d )
        ; ( "legacy kind with a placement"
          , Printf.sprintf
              "INSERT INTO notifications \
                 (user_id, notif_type, message, \
                  shared_thread_placement_id) \
               VALUES (%d, 'mention', 'hi', %Ld)"
              author placement )
        ])

let notif_dedup_case =
  db_case "schema: one notification per recipient, kind, and placement"
    (fun conn ->
      let* author, o, d, post = base conn "ndedup" in
      let* other = insert_user conn "stps_ndedup_other" in
      let* () =
        accepts conn "placement" (row_sql ~post ~origin:o ~destination:d ())
      in
      let* placement = find conn "placement id" q_sole_id (post, d) in
      let* _ =
        find conn "first" q_insert_stp_notif
          ((author, "shared_thread_requested"), (d, placement))
      in
      let* () =
        refuses conn "replayed duplicate"
          (Printf.sprintf
             "INSERT INTO notifications \
                (user_id, notif_type, community_id, \
                 shared_thread_placement_id) \
              VALUES (%d, 'shared_thread_requested', %d, %Ld)"
             author d placement)
      in
      (* A different kind and a different recipient both coexist. *)
      let* _ =
        find conn "same placement, next kind" q_insert_stp_notif
          ((author, "shared_thread_withdrawn"), (d, placement))
      in
      let* _ =
        find conn "same kind, other recipient" q_insert_stp_notif
          ((other, "shared_thread_requested"), (d, placement))
      in
      Lwt.return_unit)

let suite =
  [ valid_shapes_case; same_community_case; status_vocabulary_case
  ; lifecycle_shape_case; timestamp_order_case; note_length_case
  ; one_active_case; history_frees_slot_case; actor_deletion_case
  ; section_set_null_case; index_shape_case; audit_vocabulary_case
  ; audit_protects_subjects_case; notif_shape_case; notif_dedup_case ]

let suites =
  [ ("shared_thread_placements_schema", suite)
  ]
