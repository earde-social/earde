(* The transactional store and its read model: request, accept, reject and
   remove, each writing exactly one audit event inside its own transaction;
   the unordered-pair arbitration under real concurrency; the symmetric and
   queue reads. Database-gated on EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Cc = Earde.Community_connections
module Store = Earde.Community_connections_store
module Read = Earde.Community_connections_read_model
module Audit = Earde.Community_connection_audit

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let insert_community = Community_fixture.insert_community
let status_str = Cc.string_of_status

let read_error_str : Read.error -> string = function
  | Read.Invalid_connection_id -> "Invalid_connection_id"
  | Read.Invalid_community_id -> "Invalid_community_id"
  | Read.Inconsistent_data -> "Inconsistent_data"
  | Read.Storage_error -> "Storage_error"

let action_str : Audit.action -> string = Audit.string_of_action

(* Audit events RESTRICT-protect their connection and both communities, so
   they go first; connections then fall away before their communities. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS ccon_fail_audit ON \
       community_connection_audit_events";
      "DROP TRIGGER IF EXISTS ccon_fail_business ON community_connections";
      "DROP FUNCTION IF EXISTS ccon_fail_fn()";
      "DELETE FROM community_connection_audit_events WHERE \
       requester_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccon-%') OR recipient_community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'ccon-%')";
      "DELETE FROM community_connections WHERE requester_community_id IN \
       (SELECT id FROM communities WHERE slug LIKE 'ccon-%') OR \
       recipient_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccon-%')";
      "DELETE FROM communities WHERE slug LIKE 'ccon-%'";
      "DELETE FROM users WHERE username LIKE 'ccon_%'";
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

(* === observation === *)

let q_row =
  (Caqti_type.int64
  ->! Caqti_type.(
        t2
          (t2 string (option string))
          (t3 (option int) (option int) (option int))))
    "SELECT status, request_note, requested_by_user_id, reviewed_by_user_id, \
     removed_by_user_id FROM community_connections WHERE id = $1"

let q_stamps =
  (Caqti_type.int64 ->! Caqti_type.(t3 bool bool bool))
    "SELECT reviewed_at IS NOT NULL, removed_at IS NOT NULL, updated_at >= \
     created_at FROM community_connections WHERE id = $1"

let q_count_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connections WHERE \
     LEAST(requester_community_id, recipient_community_id) = LEAST($1, $2) AND \
     GREATEST(requester_community_id, recipient_community_id) = GREATEST($1, \
     $2)"

let q_count_active_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connections WHERE \
     LEAST(requester_community_id, recipient_community_id) = LEAST($1, $2) AND \
     GREATEST(requester_community_id, recipient_community_id) = GREATEST($1, \
     $2) AND status IN ('pending', 'accepted')"

(* One tuple per event, in append (id) order. *)
let q_events =
  (Caqti_type.int64 ->* Caqti_type.(t2 (t2 string (option int)) (t2 int int)))
    "SELECT action, actor_user_id, requester_community_id, \
     recipient_community_id FROM community_connection_audit_events WHERE \
     connection_id = $1 ORDER BY id"

(* Everything ever recorded about one community, however the connection
   row itself ended up — the count a rolled-back mutation must not move. *)
let q_events_for_community =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connection_audit_events WHERE \
     requester_community_id = $1 OR recipient_community_id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_absent_community_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

let q_absent_connection_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM community_connections"

let event_t = Alcotest.(pair (pair string (option int)) (pair int int))

let check_events label conn ~connection expected =
  let* rows = collect conn (label ^ ": events") q_events connection in
  Alcotest.(check (list event_t)) (label ^ ": exact events") expected rows;
  Lwt.return_unit

let check_event_count label conn ~community expected =
  let* n = find conn (label ^ ": count") q_events_for_community community in
  Alcotest.(check int) (label ^ ": community event count") expected n;
  Lwt.return_unit

let check_row label conn id ~status ~note ~requested_by ~reviewed_by ~removed_by
    =
  let* ( (stored_status, stored_note),
         (stored_requested, stored_reviewed, stored_removed) ) =
    find conn (label ^ ": row") q_row id
  in
  Alcotest.(check string) (label ^ ": status") status stored_status;
  Alcotest.(check (option string)) (label ^ ": note") note stored_note;
  Alcotest.(check (option int))
    (label ^ ": requester actor")
    requested_by stored_requested;
  Alcotest.(check (option int))
    (label ^ ": reviewer") reviewed_by stored_reviewed;
  Alcotest.(check (option int)) (label ^ ": remover") removed_by stored_removed;
  Lwt.return_unit

(* === call helpers === *)

let pending_value ?note ~requester ~recipient () =
  match
    Cc.create_pending ~requester_community_id:requester
      ~recipient_community_id:recipient ~request_note:note
  with
  | Ok v -> v
  | Error _ -> Alcotest.fail "fixture: pure pending value refused"

let request conn ~actor ?note ~requester ~recipient () =
  Store.request conn ~actor_user_id:actor
    ~connection:(pending_value ?note ~requester ~recipient ())

let request_ok label conn ~actor ?note ~requester ~recipient () =
  let* r = request conn ~actor ?note ~requester ~recipient () in
  match r with
  | Ok created -> Lwt.return (Store.created_connection_id created)
  | Error e -> Alcotest.failf "%s: %s" label (Community_fixture.error_str e)

let request_expect label expected conn ~actor ?note ~requester ~recipient () =
  let* r = request conn ~actor ?note ~requester ~recipient () in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Community_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Community_fixture.error_str expected)
        (Community_fixture.error_str e);
      Lwt.return_unit

let review conn ~reviewer ~connection ~recipient decision =
  Store.review conn ~reviewer_user_id:reviewer ~connection_id:connection
    ~recipient_community_id:recipient ~decision

let review_ok label conn ~reviewer ~connection ~recipient decision expected =
  let* r = review conn ~reviewer ~connection ~recipient decision in
  match r with
  | Ok reviewed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str expected)
        (status_str (Store.reviewed_status reviewed));
      Lwt.return reviewed
  | Error e -> Alcotest.failf "%s: %s" label (Community_fixture.error_str e)

let review_expect label expected conn ~reviewer ~connection ~recipient decision
    =
  let* r = review conn ~reviewer ~connection ~recipient decision in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Community_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Community_fixture.error_str expected)
        (Community_fixture.error_str e);
      Lwt.return_unit

let remove conn ~actor ~connection ~acting =
  Store.remove conn ~actor_user_id:actor ~connection_id:connection
    ~acting_community_id:acting

let remove_ok label conn ~actor ~connection ~acting =
  let* r = remove conn ~actor ~connection ~acting in
  match r with
  | Ok removed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str Cc.Removed)
        (status_str (Store.removed_status removed));
      Lwt.return removed
  | Error e -> Alcotest.failf "%s: %s" label (Community_fixture.error_str e)

let remove_expect label expected conn ~actor ~connection ~acting =
  let* r = remove conn ~actor ~connection ~acting in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Community_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Community_fixture.error_str expected)
        (Community_fixture.error_str e);
      Lwt.return_unit

(* Two communities and one actor, the shape every case starts from. *)
let fixture conn tag =
  let* actor = insert_user conn ("ccon_" ^ tag) in
  let* a = insert_community conn ("ccon-" ^ tag ^ "-a") in
  let* b = insert_community conn ("ccon-" ^ tag ^ "-b") in
  Lwt.return (actor, a, b)

(* === request === *)

let request_case =
  db_case "request: one pending row and one requested event" (fun conn ->
      let* actor, a, b = fixture conn "req" in
      let* id =
        request_ok "request" conn ~actor ~note:"  let's connect\r\n  "
          ~requester:a ~recipient:b ()
      in
      let* () =
        check_row "request" conn id ~status:"pending"
          ~note:(Some "let's connect") ~requested_by:(Some actor)
          ~reviewed_by:None ~removed_by:None
      in
      let* reviewed_at, removed_at, coherent = find conn "stamps" q_stamps id in
      Alcotest.(check bool) "no review time" false reviewed_at;
      Alcotest.(check bool) "no removal time" false removed_at;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      check_events "request" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

let request_blank_note_case =
  db_case "request: a blank note is stored as absent" (fun conn ->
      let* actor, a, b = fixture conn "blank" in
      let* id =
        request_ok "request" conn ~actor ~note:"   \r\n\t " ~requester:a
          ~recipient:b ()
      in
      check_row "blank" conn id ~status:"pending" ~note:None
        ~requested_by:(Some actor) ~reviewed_by:None ~removed_by:None)

let request_validation_case =
  db_case "request: invalid inputs are refused before any SQL" (fun conn ->
      let* actor, a, b = fixture conn "reqval" in
      let* () =
        request_expect "zero actor" Store.Invalid_user_id conn ~actor:0
          ~requester:a ~recipient:b ()
      in
      let* () =
        request_expect "negative actor" Store.Invalid_user_id conn ~actor:(-4)
          ~requester:a ~recipient:b ()
      in
      (* An already-transitioned pure value is never silently reset. *)
      let accepted =
        match
          Cc.apply (pending_value ~requester:a ~recipient:b ()) Cc.Accept
        with
        | Ok v -> v
        | Error _ -> Alcotest.fail "fixture: accept"
      in
      let* r = Store.request conn ~actor_user_id:actor ~connection:accepted in
      (match r with
      | Error Store.Invalid_connection -> ()
      | Error e ->
          Alcotest.failf "accepted value: %s" (Community_fixture.error_str e)
      | Ok _ -> Alcotest.fail "accepted value was stored as a request");
      let* n = find conn "no rows" q_count_pair (a, b) in
      Alcotest.(check int) "nothing written" 0 n;
      check_event_count "validation" conn ~community:a 0)

let request_missing_community_case =
  db_case "request: a missing community on either side is one error"
    (fun conn ->
      let* actor, a, _ = fixture conn "gone" in
      let* absent = find conn "absent id" q_absent_community_id () in
      let* () =
        request_expect "absent recipient" Store.Community_unavailable conn
          ~actor ~requester:a ~recipient:absent ()
      in
      let* () =
        request_expect "absent requester" Store.Community_unavailable conn
          ~actor ~requester:absent ~recipient:a ()
      in
      check_event_count "missing" conn ~community:a 0)

let request_duplicate_case =
  db_case "request: a same-direction duplicate loses and writes nothing"
    (fun conn ->
      let* actor, a, b = fixture conn "dup" in
      let* id = request_ok "first" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        request_expect "duplicate" Store.Active_connection_exists conn ~actor
          ~requester:a ~recipient:b ()
      in
      let* n = find conn "row count" q_count_pair (a, b) in
      Alcotest.(check int) "still exactly one row" 1 n;
      check_events "duplicate" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

let request_reversed_case =
  db_case "request: the mirrored direction loses against a live request"
    (fun conn ->
      let* actor, a, b = fixture conn "mirror" in
      let* id = request_ok "first" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        request_expect "reversed" Store.Active_connection_exists conn ~actor
          ~requester:b ~recipient:a ()
      in
      let* n = find conn "row count" q_count_pair (a, b) in
      Alcotest.(check int) "still exactly one row" 1 n;
      (* And equally against an accepted connection. *)
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* () =
        request_expect "reversed against accepted"
          Store.Active_connection_exists conn ~actor ~requester:b ~recipient:a
          ()
      in
      let* n = find conn "row count again" q_count_pair (a, b) in
      Alcotest.(check int) "still exactly one row" 1 n;
      check_event_count "mirror" conn ~community:a 2)

(* === review === *)

let accept_case =
  db_case "accept: the row becomes accepted with one accepted event"
    (fun conn ->
      let* actor, a, b = fixture conn "acc" in
      let* reviewer = insert_user conn "ccon_acc_mod" in
      let* id =
        request_ok "request" conn ~actor ~note:"hello" ~requester:a ~recipient:b
          ()
      in
      let* reviewed =
        review_ok "accept" conn ~reviewer ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      Alcotest.(check int)
        "requester crosses back" a
        (Store.reviewed_requester_community_id reviewed);
      Alcotest.(check int)
        "recipient crosses back" b
        (Store.reviewed_recipient_community_id reviewed);
      let* () =
        check_row "accept" conn id ~status:"accepted" ~note:(Some "hello")
          ~requested_by:(Some actor) ~reviewed_by:(Some reviewer)
          ~removed_by:None
      in
      let* reviewed_at, removed_at, coherent = find conn "stamps" q_stamps id in
      Alcotest.(check bool) "review time set" true reviewed_at;
      Alcotest.(check bool) "no removal time" false removed_at;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      check_events "accept" conn ~connection:id
        [
          (("community_connection_requested", Some actor), (a, b));
          (("community_connection_accepted", Some reviewer), (a, b));
        ])

let reject_case =
  db_case "reject: the row becomes rejected with one rejected event"
    (fun conn ->
      let* actor, a, b = fixture conn "rej" in
      let* reviewer = insert_user conn "ccon_rej_mod" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* _ =
        review_ok "reject" conn ~reviewer ~connection:id ~recipient:b
          Store.Reject Cc.Rejected
      in
      let* () =
        check_row "reject" conn id ~status:"rejected" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:(Some reviewer)
          ~removed_by:None
      in
      let* reviewed_at, removed_at, _ = find conn "stamps" q_stamps id in
      Alcotest.(check bool) "review time set" true reviewed_at;
      Alcotest.(check bool) "no removal time" false removed_at;
      check_events "reject" conn ~connection:id
        [
          (("community_connection_requested", Some actor), (a, b));
          (("community_connection_rejected", Some reviewer), (a, b));
        ])

let review_wrong_recipient_case =
  db_case "review: the recipient is verified in the mutation boundary"
    (fun conn ->
      let* actor, a, b = fixture conn "wrongrec" in
      let* c = insert_community conn "ccon-wrongrec-c" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        review_expect "another community's queue" Store.Review_unavailable conn
          ~reviewer:actor ~connection:id ~recipient:c Store.Accept
      in
      let* () =
        (* Not even the requesting community may review its own request
           through the recipient boundary. *)
        review_expect "the requester itself" Store.Review_unavailable conn
          ~reviewer:actor ~connection:id ~recipient:a Store.Accept
      in
      let* () =
        check_row "untouched" conn id ~status:"pending" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:None ~removed_by:None
      in
      check_events "no event" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

let review_stale_case =
  db_case "review: a reviewed row cannot be reviewed again" (fun conn ->
      let* actor, a, b = fixture conn "stale" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* () =
        review_expect "second accept" Store.Review_unavailable conn
          ~reviewer:actor ~connection:id ~recipient:b Store.Accept
      in
      let* () =
        review_expect "late reject" Store.Review_unavailable conn
          ~reviewer:actor ~connection:id ~recipient:b Store.Reject
      in
      let* absent = find conn "absent id" q_absent_connection_id () in
      let* () =
        review_expect "absent connection" Store.Review_unavailable conn
          ~reviewer:actor ~connection:absent ~recipient:b Store.Accept
      in
      let* () =
        check_row "still accepted" conn id ~status:"accepted" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:(Some actor) ~removed_by:None
      in
      check_events "one review only" conn ~connection:id
        [
          (("community_connection_requested", Some actor), (a, b));
          (("community_connection_accepted", Some actor), (a, b));
        ])

let review_validation_case =
  db_case "review: invalid inputs are refused before any SQL" (fun conn ->
      let* actor, a, b = fixture conn "revval" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        review_expect "zero reviewer" Store.Invalid_user_id conn ~reviewer:0
          ~connection:id ~recipient:b Store.Accept
      in
      let* () =
        review_expect "zero connection" Store.Invalid_connection_id conn
          ~reviewer:actor ~connection:0L ~recipient:b Store.Accept
      in
      let* () =
        review_expect "negative connection" Store.Invalid_connection_id conn
          ~reviewer:actor ~connection:(-9L) ~recipient:b Store.Accept
      in
      let* () =
        review_expect "zero recipient" Store.Invalid_community_id conn
          ~reviewer:actor ~connection:id ~recipient:0 Store.Accept
      in
      check_events "still just the request" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

(* === removal === *)

let remove_from_either_side_case =
  db_case "remove: either community may remove an accepted connection"
    (fun conn ->
      let* actor, a, b = fixture conn "rm" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* remover = insert_user conn "ccon_rm_mod" in
      let* removed =
        remove_ok "remove from the requester side" conn ~actor:remover
          ~connection:id ~acting:a
      in
      Alcotest.(check int)
        "requester crosses back" a
        (Store.removed_requester_community_id removed);
      Alcotest.(check int)
        "recipient crosses back" b
        (Store.removed_recipient_community_id removed);
      let* () =
        check_row "removed" conn id ~status:"removed" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:(Some actor)
          ~removed_by:(Some remover)
      in
      let* reviewed_at, removed_at, coherent = find conn "stamps" q_stamps id in
      Alcotest.(check bool) "the review survives" true reviewed_at;
      Alcotest.(check bool) "removal time set" true removed_at;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      let* () =
        check_events "remove" conn ~connection:id
          [
            (("community_connection_requested", Some actor), (a, b));
            (("community_connection_accepted", Some actor), (a, b));
            (("community_connection_removed", Some remover), (a, b));
          ]
      in
      (* The other side can remove just as unilaterally. *)
      let* second =
        request_ok "second request" conn ~actor ~requester:a ~recipient:b ()
      in
      let* _ =
        review_ok "accept again" conn ~reviewer:actor ~connection:second
          ~recipient:b Store.Accept Cc.Accepted
      in
      let* _ =
        remove_ok "remove from the recipient side" conn ~actor:remover
          ~connection:second ~acting:b
      in
      check_row "removed again" conn second ~status:"removed" ~note:None
        ~requested_by:(Some actor) ~reviewed_by:(Some actor)
        ~removed_by:(Some remover))

let remove_stale_case =
  db_case "remove: only an accepted row, and only from inside the pair"
    (fun conn ->
      let* actor, a, b = fixture conn "rmstale" in
      let* stranger = insert_community conn "ccon-rmstale-c" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        remove_expect "a pending row" Store.Removal_unavailable conn ~actor
          ~connection:id ~acting:a
      in
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* () =
        remove_expect "an outside community" Store.Removal_unavailable conn
          ~actor ~connection:id ~acting:stranger
      in
      let* absent = find conn "absent id" q_absent_connection_id () in
      let* () =
        remove_expect "an absent connection" Store.Removal_unavailable conn
          ~actor ~connection:absent ~acting:a
      in
      let* _ = remove_ok "first removal" conn ~actor ~connection:id ~acting:a in
      let* () =
        remove_expect "a second removal" Store.Removal_unavailable conn ~actor
          ~connection:id ~acting:b
      in
      let* () =
        remove_expect "a rejected row is not removable"
          Store.Removal_unavailable conn ~actor ~connection:id ~acting:a
      in
      (* Exactly one removed event survives all of that. *)
      check_events "one removal only" conn ~connection:id
        [
          (("community_connection_requested", Some actor), (a, b));
          (("community_connection_accepted", Some actor), (a, b));
          (("community_connection_removed", Some actor), (a, b));
        ])

let remove_validation_case =
  db_case "remove: invalid inputs are refused before any SQL" (fun conn ->
      let* actor, a, b = fixture conn "rmval" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        remove_expect "zero actor" Store.Invalid_user_id conn ~actor:0
          ~connection:id ~acting:a
      in
      let* () =
        remove_expect "zero connection" Store.Invalid_connection_id conn ~actor
          ~connection:0L ~acting:a
      in
      let* () =
        remove_expect "zero community" Store.Invalid_community_id conn ~actor
          ~connection:id ~acting:0
      in
      check_events "still just the request" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

(* === history and a fresh start === *)

let fresh_request_after_history_case =
  db_case "history: a rejected or removed pair accepts a fresh request"
    (fun conn ->
      let* actor, a, b = fixture conn "fresh" in
      let* first =
        request_ok "first" conn ~actor ~requester:a ~recipient:b ()
      in
      let* _ =
        review_ok "reject" conn ~reviewer:actor ~connection:first ~recipient:b
          Store.Reject Cc.Rejected
      in
      (* A rejection frees the slot — and the other side may now ask. *)
      let* second =
        request_ok "after rejection, reversed" conn ~actor ~requester:b
          ~recipient:a ()
      in
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:second ~recipient:a
          Store.Accept Cc.Accepted
      in
      let* _ = remove_ok "remove" conn ~actor ~connection:second ~acting:b in
      let* third =
        request_ok "after removal" conn ~actor ~requester:a ~recipient:b ()
      in
      let* total = find conn "history retained" q_count_pair (a, b) in
      Alcotest.(check int) "all three rows kept" 3 total;
      let* active = find conn "active" q_count_active_pair (a, b) in
      Alcotest.(check int) "exactly one active" 1 active;
      let* () =
        check_row "the fresh request" conn third ~status:"pending" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:None ~removed_by:None
      in
      (* Each row keeps its own trail; nothing is reopened or reused. *)
      let* () =
        check_events "first" conn ~connection:first
          [
            (("community_connection_requested", Some actor), (a, b));
            (("community_connection_rejected", Some actor), (a, b));
          ]
      in
      check_events "third" conn ~connection:third
        [ (("community_connection_requested", Some actor), (a, b)) ])

let actor_deletion_case =
  db_case "history: deleting an actor keeps every row and nulls provenance"
    (fun conn ->
      let* actor, a, b = fixture conn "ghost" in
      let* reviewer = insert_user conn "ccon_ghost_mod" in
      let* id =
        request_ok "request" conn ~actor ~note:"kept" ~requester:a ~recipient:b
          ()
      in
      let* _ =
        review_ok "accept" conn ~reviewer ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* _ =
        remove_ok "remove" conn ~actor:reviewer ~connection:id ~acting:b
      in
      let* () = exec conn "delete requester" q_delete_user actor in
      let* () = exec conn "delete reviewer" q_delete_user reviewer in
      let* () =
        check_row "provenance nulled" conn id ~status:"removed"
          ~note:(Some "kept") ~requested_by:None ~reviewed_by:None
          ~removed_by:None
      in
      check_events "events kept, actors nulled" conn ~connection:id
        [
          (("community_connection_requested", None), (a, b));
          (("community_connection_accepted", None), (a, b));
          (("community_connection_removed", None), (a, b));
        ])

(* === atomicity === *)

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION ccon_fail_fn() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN \
     RAISE EXCEPTION ''ccon fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS ccon_fail_fn()"

let q_poison_audit =
  ddl
    "CREATE TRIGGER ccon_fail_audit BEFORE INSERT ON \
     community_connection_audit_events FOR EACH ROW EXECUTE FUNCTION \
     ccon_fail_fn()"

let q_unpoison_audit =
  ddl
    "DROP TRIGGER IF EXISTS ccon_fail_audit ON \
     community_connection_audit_events"

let q_poison_insert =
  ddl
    "CREATE TRIGGER ccon_fail_business AFTER INSERT ON community_connections \
     FOR EACH ROW EXECUTE FUNCTION ccon_fail_fn()"

let q_poison_update =
  ddl
    "CREATE TRIGGER ccon_fail_business AFTER UPDATE ON community_connections \
     FOR EACH ROW EXECUTE FUNCTION ccon_fail_fn()"

let q_unpoison_business =
  ddl "DROP TRIGGER IF EXISTS ccon_fail_business ON community_connections"

let with_poison conn ~install ~remove f =
  let* () = exec conn "create fail fn" q_create_fail_fn () in
  Lwt.finalize
    (fun () ->
      let* () = exec conn "install poison trigger" install () in
      Lwt.finalize f (fun () -> exec conn "drop poison trigger" remove ()))
    (fun () -> exec conn "drop fail fn" q_drop_fail_fn ())

let audit_failure_rolls_back_case =
  db_case "atomicity: a failed audit insert rolls the whole mutation back"
    (fun conn ->
      let* actor, a, b = fixture conn "auditfail" in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            request_expect "request with audit poisoned" Store.Storage_error
              conn ~actor ~requester:a ~recipient:b ())
      in
      let* n = find conn "no connection row" q_count_pair (a, b) in
      Alcotest.(check int) "no connection committed" 0 n;
      let* () = check_event_count "no events" conn ~community:a 0 in
      (* And once the poison is gone the very same request commits. *)
      let* id =
        request_ok "request afterwards" conn ~actor ~requester:a ~recipient:b ()
      in
      check_events "committed together" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

let mutation_failure_leaves_no_event_case =
  db_case "atomicity: a failed mutation leaves no audit event behind"
    (fun conn ->
      let* actor, a, b = fixture conn "bizfail" in
      let* () =
        with_poison conn ~install:q_poison_insert ~remove:q_unpoison_business
          (fun () ->
            request_expect "request with the insert poisoned"
              Store.Storage_error conn ~actor ~requester:a ~recipient:b ())
      in
      let* n = find conn "no connection row" q_count_pair (a, b) in
      Alcotest.(check int) "no connection committed" 0 n;
      let* () = check_event_count "no events" conn ~community:a 0 in
      (* The same holds for the update-driven transitions. *)
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        with_poison conn ~install:q_poison_update ~remove:q_unpoison_business
          (fun () ->
            review_expect "accept with the update poisoned" Store.Storage_error
              conn ~reviewer:actor ~connection:id ~recipient:b Store.Accept)
      in
      let* () =
        check_row "still pending" conn id ~status:"pending" ~note:None
          ~requested_by:(Some actor) ~reviewed_by:None ~removed_by:None
      in
      check_events "only the request survives" conn ~connection:id
        [ (("community_connection_requested", Some actor), (a, b)) ])

(* === concurrency === *)

let concurrent_same_direction_case =
  db_case "concurrency: identical requests leave exactly one active row"
    (fun conn ->
      let* actor, a, b = fixture conn "race" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (request conn ~actor ~requester:a ~recipient:b ())
              (request conn2 ~actor ~requester:a ~recipient:b ())
          in
          (match (r1, r2) with
          | Ok _, Error Store.Active_connection_exists
          | Error Store.Active_connection_exists, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both requests won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)"
                (Community_fixture.error_str e)
                (Community_fixture.error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s"
                (Community_fixture.error_str e));
          let* total = find conn "row count" q_count_pair (a, b) in
          Alcotest.(check int) "exactly one row" 1 total;
          let* events = find conn "events" q_events_for_community a in
          Alcotest.(check int) "exactly one event" 1 events;
          Lwt.return_unit))

let concurrent_opposite_direction_case =
  db_case "concurrency: opposite-direction requests leave one active row"
    (fun conn ->
      let* actor, a, b = fixture conn "race2" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (request conn ~actor ~requester:a ~recipient:b ())
              (request conn2 ~actor ~requester:b ~recipient:a ())
          in
          (match (r1, r2) with
          | Ok _, Error Store.Active_connection_exists
          | Error Store.Active_connection_exists, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both directions won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)"
                (Community_fixture.error_str e)
                (Community_fixture.error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s"
                (Community_fixture.error_str e));
          let* total = find conn "row count" q_count_pair (a, b) in
          Alcotest.(check int) "exactly one row" 1 total;
          (* Which direction won is deliberately unasserted. *)
          let* active = find conn "active" q_count_active_pair (a, b) in
          Alcotest.(check int) "exactly one active row" 1 active;
          Lwt.return_unit))

let concurrent_review_case =
  db_case "concurrency: only one review of a pending row commits" (fun conn ->
      let* actor, a, b = fixture conn "race3" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (review conn ~reviewer:actor ~connection:id ~recipient:b
                 Store.Accept)
              (review conn2 ~reviewer:actor ~connection:id ~recipient:b
                 Store.Reject)
          in
          (match (r1, r2) with
          | Ok _, Error Store.Review_unavailable
          | Error Store.Review_unavailable, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both reviews won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)"
                (Community_fixture.error_str e)
                (Community_fixture.error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s"
                (Community_fixture.error_str e));
          let* events = collect conn "events" q_events id in
          Alcotest.(check int)
            "the request plus exactly one review" 2 (List.length events);
          Lwt.return_unit))

(* === reads === *)

let read_ok label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (read_error_str e)

let read_load_case =
  db_case "read: one connection loads with its whole durable shape" (fun conn ->
      let* actor, a, b = fixture conn "load" in
      let* id =
        request_ok "request" conn ~actor ~note:"why not" ~requester:a
          ~recipient:b ()
      in
      let* loaded = Read.load conn ~connection_id:id in
      let* loaded = read_ok "load" loaded in
      (match loaded with
      | None -> Alcotest.fail "the connection did not load"
      | Some row ->
          Alcotest.(check int64) "id" id row.Read.id;
          Alcotest.(check int) "requester" a row.Read.requester_community_id;
          Alcotest.(check int) "recipient" b row.Read.recipient_community_id;
          Alcotest.(check string)
            "status" "pending"
            (status_str row.Read.status);
          Alcotest.(check (option string))
            "note" (Some "why not") row.Read.request_note;
          Alcotest.(check (option int))
            "requester actor" (Some actor) row.Read.requested_by_user_id;
          Alcotest.(check (option int))
            "no reviewer" None row.Read.reviewed_by_user_id;
          Alcotest.(check (option int))
            "no remover" None row.Read.removed_by_user_id;
          Alcotest.(check bool)
            "created_at present" true
            (String.length row.Read.created_at > 0);
          Alcotest.(check (option string))
            "no review time" None row.Read.reviewed_at;
          Alcotest.(check (option string))
            "no removal time" None row.Read.removed_at);
      let* absent = find conn "absent id" q_absent_connection_id () in
      let* missing = Read.load conn ~connection_id:absent in
      let* missing = read_ok "absent load" missing in
      Alcotest.(check bool) "an absent id is simply absent" true (missing = None);
      let* invalid = Read.load conn ~connection_id:0L in
      (match invalid with
      | Error Read.Invalid_connection_id -> ()
      | Error e -> Alcotest.failf "zero id: %s" (read_error_str e)
      | Ok _ -> Alcotest.fail "zero id accepted");
      Lwt.return_unit)

let read_active_pair_case =
  db_case "read: the active pair reads the same from either order" (fun conn ->
      let* actor, a, b = fixture conn "pair" in
      let* none_yet = Read.active_for_pair conn ~community_a:a ~community_b:b in
      let* none_yet = read_ok "empty pair" none_yet in
      Alcotest.(check bool) "no active connection yet" true (none_yet = None);
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let check label ~community_a ~community_b =
        let* found = Read.active_for_pair conn ~community_a ~community_b in
        let* found = read_ok label found in
        match found with
        | Some row ->
            Alcotest.(check int64) (label ^ ": id") id row.Read.id;
            Lwt.return_unit
        | None -> Alcotest.failf "%s: nothing found" label
      in
      let* () = check "forward" ~community_a:a ~community_b:b in
      let* () = check "reversed" ~community_a:b ~community_b:a in
      (* A rejected row leaves the pair free, and the read says so. *)
      let* _ =
        review_ok "reject" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Reject Cc.Rejected
      in
      let* after = Read.active_for_pair conn ~community_a:a ~community_b:b in
      let* after = read_ok "after rejection" after in
      Alcotest.(check bool) "history does not hold the slot" true (after = None);
      let* self = Read.active_for_pair conn ~community_a:a ~community_b:a in
      (match self with
      | Error Read.Invalid_community_id -> ()
      | Error e -> Alcotest.failf "self pair: %s" (read_error_str e)
      | Ok _ -> Alcotest.fail "a self pair was accepted");
      Lwt.return_unit)

let read_symmetric_accepted_case =
  db_case "read: an accepted connection lists from both sides" (fun conn ->
      let* actor, a, b = fixture conn "sym" in
      let* c = insert_community conn "ccon-sym-c" in
      let* first = request_ok "a→b" conn ~actor ~requester:a ~recipient:b () in
      let* _ =
        review_ok "accept a→b" conn ~reviewer:actor ~connection:first
          ~recipient:b Store.Accept Cc.Accepted
      in
      let* second = request_ok "c→a" conn ~actor ~requester:c ~recipient:a () in
      let* _ =
        review_ok "accept c→a" conn ~reviewer:actor ~connection:second
          ~recipient:a Store.Accept Cc.Accepted
      in
      (* A pending and a removed row must not appear in accepted listings. *)
      let* third = request_ok "b→c" conn ~actor ~requester:b ~recipient:c () in
      ignore third;
      let ids label rows =
        let got = List.map (fun r -> r.Read.id) rows in
        Alcotest.(check int)
          (label ^ ": count") (List.length rows) (List.length got);
        got
      in
      let* from_a = Read.list_accepted conn ~community_id:a in
      let* from_a = read_ok "from a" from_a in
      Alcotest.(check (list int64))
        "a sees both of its accepted connections" [ second; first ]
        (ids "a" from_a);
      let* from_b = Read.list_accepted conn ~community_id:b in
      let* from_b = read_ok "from b" from_b in
      Alcotest.(check (list int64))
        "b sees the same connection from the recipient side" [ first ]
        (ids "b" from_b);
      let* from_c = Read.list_accepted conn ~community_id:c in
      let* from_c = read_ok "from c" from_c in
      Alcotest.(check (list int64))
        "c sees the one it requested" [ second ] (ids "c" from_c);
      (* Removal takes it out of both listings at once. *)
      let* _ = remove_ok "remove" conn ~actor ~connection:first ~acting:b in
      let* from_a = Read.list_accepted conn ~community_id:a in
      let* from_a = read_ok "from a after removal" from_a in
      Alcotest.(check (list int64))
        "a no longer sees it" [ second ] (ids "a" from_a);
      let* from_b = Read.list_accepted conn ~community_id:b in
      let* from_b = read_ok "from b after removal" from_b in
      Alcotest.(check (list int64))
        "b no longer sees it either" [] (ids "b" from_b);
      Lwt.return_unit)

let read_queues_case =
  db_case "read: the incoming and outgoing queues are direction-aware"
    (fun conn ->
      let* actor, a, b = fixture conn "queue" in
      let* c = insert_community conn "ccon-queue-c" in
      let* incoming =
        request_ok "b→a" conn ~actor ~requester:b ~recipient:a ()
      in
      let* outgoing =
        request_ok "a→c" conn ~actor ~requester:a ~recipient:c ()
      in
      let ids rows = List.map (fun r -> r.Read.id) rows in
      let* q_in = Read.list_incoming_pending conn ~community_id:a in
      let* q_in = read_ok "incoming" q_in in
      Alcotest.(check (list int64)) "a's review queue" [ incoming ] (ids q_in);
      let* q_out = Read.list_outgoing_pending conn ~community_id:a in
      let* q_out = read_ok "outgoing" q_out in
      Alcotest.(check (list int64))
        "a's awaiting queue" [ outgoing ] (ids q_out);
      (* Reviewing empties the queue it was in, and no other. *)
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:incoming
          ~recipient:a Store.Accept Cc.Accepted
      in
      let* q_in = Read.list_incoming_pending conn ~community_id:a in
      let* q_in = read_ok "incoming after review" q_in in
      Alcotest.(check (list int64))
        "the reviewed request left the queue" [] (ids q_in);
      let* q_out = Read.list_outgoing_pending conn ~community_id:a in
      let* q_out = read_ok "outgoing after review" q_out in
      Alcotest.(check (list int64))
        "the outgoing one is untouched" [ outgoing ] (ids q_out);
      let* invalid = Read.list_incoming_pending conn ~community_id:0 in
      (match invalid with
      | Error Read.Invalid_community_id -> ()
      | Error e -> Alcotest.failf "zero community: %s" (read_error_str e)
      | Ok _ -> Alcotest.fail "zero community accepted");
      Lwt.return_unit)

let read_audit_case =
  db_case "read: one connection's audit trail reads in append order"
    (fun conn ->
      let* actor, a, b = fixture conn "trail" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* _ =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept Cc.Accepted
      in
      let* _ = remove_ok "remove" conn ~actor ~connection:id ~acting:a in
      let* events = Read.list_audit_events conn ~connection_id:id in
      let* events = read_ok "trail" events in
      Alcotest.(check (list string))
        "the three actions in order"
        [
          "community_connection_requested";
          "community_connection_accepted";
          "community_connection_removed";
        ]
        (List.map (fun e -> action_str e.Read.event_action) events);
      List.iter
        (fun e ->
          Alcotest.(check (option int))
            "actor" (Some actor) e.Read.event_actor_user_id;
          Alcotest.(check int64) "connection" id e.Read.event_connection_id;
          Alcotest.(check int) "requester" a e.Read.event_requester_community_id;
          Alcotest.(check int) "recipient" b e.Read.event_recipient_community_id)
        events;
      let* absent = find conn "absent id" q_absent_connection_id () in
      let* empty = Read.list_audit_events conn ~connection_id:absent in
      let* empty = read_ok "absent trail" empty in
      Alcotest.(check int)
        "an absent connection has no trail" 0 (List.length empty);
      Lwt.return_unit)

let store_suite =
  [
    request_case;
    request_blank_note_case;
    request_validation_case;
    request_missing_community_case;
    request_duplicate_case;
    request_reversed_case;
    accept_case;
    reject_case;
    review_wrong_recipient_case;
    review_stale_case;
    review_validation_case;
    remove_from_either_side_case;
    remove_stale_case;
    remove_validation_case;
    fresh_request_after_history_case;
    actor_deletion_case;
  ]

let atomicity_suite =
  [ audit_failure_rolls_back_case; mutation_failure_leaves_no_event_case ]

let concurrency_suite =
  [
    concurrent_same_direction_case;
    concurrent_opposite_direction_case;
    concurrent_review_case;
  ]

let read_suite =
  [
    read_load_case;
    read_active_pair_case;
    read_symmetric_accepted_case;
    read_queues_case;
    read_audit_case;
  ]

let suites =
  [
    ("community_connections_store", store_suite);
    ("community_connections_atomicity", atomicity_suite);
    ("community_connections_concurrency", concurrency_suite);
    ("community_connections_reads", read_suite);
  ]
