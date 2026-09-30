(* The transactional store: request, accept, reject, withdraw, remove —
   each writing exactly one audit event and its notifications inside its
   own transaction; origin derivation from the post; connection,
   eligibility, tombstone, and section validation on the creating paths
   only; the active-(post, destination) arbitration under real
   concurrency; and rollback injection. Database-gated on
   EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module P = Earde.Shared_thread_placements

module Store = Earde.Shared_thread_placement_store

module Cc = Earde.Community_connections

module Ccs = Earde.Community_connections_store

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let status_str = P.string_of_status

(* === observation === *)

let q_row =
  (Caqti_type.int64
   ->! Caqti_type.(
         t2
           (t2 (t2 string (option int)) (t2 (option string) (option int)))
           (t3 (option int) (option int) (option int))))
  "SELECT status, destination_section_id, request_note, \
          requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
          withdrawn_by_user_id \
   FROM shared_thread_placements WHERE id = $1"

let check_row label conn id ~status ~section ~note ~requested_by
    ~reviewed_by ~removed_by ~withdrawn_by =
  let* ( ((stored_status, stored_section), (stored_note, stored_requested)),
         (stored_reviewed, stored_removed, stored_withdrawn) ) =
    find conn (label ^ ": row") q_row id
  in
  Alcotest.(check string) (label ^ ": status") status stored_status;
  Alcotest.(check (option int)) (label ^ ": section") section stored_section;
  Alcotest.(check (option string)) (label ^ ": note") note stored_note;
  Alcotest.(check (option int)) (label ^ ": requester") requested_by
    stored_requested;
  Alcotest.(check (option int)) (label ^ ": reviewer") reviewed_by
    stored_reviewed;
  Alcotest.(check (option int)) (label ^ ": remover") removed_by
    stored_removed;
  Alcotest.(check (option int)) (label ^ ": withdrawer") withdrawn_by
    stored_withdrawn;
  Lwt.return_unit

let q_origin_of =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT origin_community_id FROM shared_thread_placements WHERE id = $1"

let q_stamps =
  (Caqti_type.int64 ->! Caqti_type.(t4 bool bool bool bool))
  "SELECT reviewed_at IS NOT NULL, removed_at IS NOT NULL, \
          withdrawn_at IS NOT NULL, updated_at >= created_at \
   FROM shared_thread_placements WHERE id = $1"

let q_count_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COUNT(*) FROM shared_thread_placements \
   WHERE post_id = $1 AND destination_community_id = $2"

let q_count_active_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COUNT(*) FROM shared_thread_placements \
   WHERE post_id = $1 AND destination_community_id = $2 \
     AND status IN ('pending', 'accepted')"

(* One tuple per event, in append (id) order. *)
let q_events =
  (Caqti_type.int64
   ->* Caqti_type.(t2 (t2 string (option int)) (t3 int int int)))
  "SELECT action, actor_user_id, post_id, origin_community_id, \
          destination_community_id \
   FROM shared_thread_placement_audit_events \
   WHERE placement_id = $1 ORDER BY id"

let q_absent_post =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM posts"

let q_absent_community =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

let q_absent_placement =
  (Caqti_type.unit ->! Caqti_type.int64)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM shared_thread_placements"

let event_t = Alcotest.(pair (pair string (option int)) (triple int int int))

let check_events label conn ~placement expected =
  let* rows = collect conn (label ^ ": events") q_events placement in
  Alcotest.(check (list event_t)) (label ^ ": exact events") expected rows;
  Lwt.return_unit

let check_event_count label conn ~post expected =
  let* n = find conn (label ^ ": count") Shared_thread_fixture.q_events_for_post post in
  Alcotest.(check int) (label ^ ": post event count") expected n;
  Lwt.return_unit

let request_expect label expected conn ~actor ?note ~post ~destination () =
  let* r = Shared_thread_fixture.request conn ~actor ?note ~post ~destination () in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (Shared_thread_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string) label (Shared_thread_fixture.error_str expected) (Shared_thread_fixture.error_str e);
      Lwt.return_unit

let review_expect label expected conn ~reviewer ~placement ~destination
    decision =
  let* r = Shared_thread_fixture.review conn ~reviewer ~placement ~destination decision in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (Shared_thread_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string) label (Shared_thread_fixture.error_str expected) (Shared_thread_fixture.error_str e);
      Lwt.return_unit

let withdraw_expect label expected conn ~actor ~placement ~origin =
  let* r = Shared_thread_fixture.withdraw conn ~actor ~placement ~origin in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (Shared_thread_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string) label (Shared_thread_fixture.error_str expected) (Shared_thread_fixture.error_str e);
      Lwt.return_unit

let remove_expect label expected conn ~actor ~placement ~acting =
  let* r = Shared_thread_fixture.remove conn ~actor ~placement ~acting in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (Shared_thread_fixture.error_str expected)
  | Error e ->
      Alcotest.(check string) label (Shared_thread_fixture.error_str expected) (Shared_thread_fixture.error_str e);
      Lwt.return_unit

(* === request === *)

let request_case =
  Shared_thread_fixture.db_case "request: one pending row, derived origin, one requested event"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "req" in
      let* id =
        Shared_thread_fixture.request_ok "request" conn ~actor ~note:"  worth sharing\r\n  "
          ~post ~destination:d ()
      in
      let* () =
        check_row "request" conn id ~status:"pending" ~section:None
          ~note:(Some "worth sharing") ~requested_by:(Some actor)
          ~reviewed_by:None ~removed_by:None ~withdrawn_by:None
      in
      (* The origin is the post's own community — the API offers no way
         to claim another one, and the stored copy proves the
         derivation. *)
      let* origin = find conn "origin" q_origin_of id in
      Alcotest.(check int) "origin derived from the post" o origin;
      let* reviewed, removed, withdrawn, coherent =
        find conn "stamps" q_stamps id
      in
      Alcotest.(check bool) "no review time" false reviewed;
      Alcotest.(check bool) "no removal time" false removed;
      Alcotest.(check bool) "no withdrawal time" false withdrawn;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      check_events "request" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d)) ])

let request_blank_note_case =
  Shared_thread_fixture.db_case "request: a blank note is stored as absent" (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "blank" in
      let* id =
        Shared_thread_fixture.request_ok "request" conn ~actor ~note:"   \r\n\t " ~post
          ~destination:d ()
      in
      check_row "blank" conn id ~status:"pending" ~section:None ~note:None
        ~requested_by:(Some actor) ~reviewed_by:None ~removed_by:None
        ~withdrawn_by:None)

let request_validation_case =
  Shared_thread_fixture.db_case "request: invalid inputs are refused before any SQL" (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "reqval" in
      let* () =
        request_expect "zero actor" Store.Invalid_user_id conn ~actor:0
          ~post ~destination:d ()
      in
      let* () =
        request_expect "zero post" Store.Invalid_post_id conn ~actor
          ~post:0 ~destination:d ()
      in
      let* () =
        request_expect "zero destination" Store.Invalid_community_id conn
          ~actor ~post ~destination:0 ()
      in
      let* () =
        request_expect "invalid note" Store.Invalid_request_note conn
          ~actor ~note:"nul\x00" ~post ~destination:d ()
      in
      let* n = find conn "no rows" q_count_pair (post, d) in
      Alcotest.(check int) "nothing written" 0 n;
      check_event_count "validation" conn ~post 0)

let request_missing_subjects_case =
  Shared_thread_fixture.db_case "request: a missing post or destination is one closed error"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "gone" in
      let* absent_post = find conn "absent post" q_absent_post () in
      let* () =
        request_expect "absent post" Store.Post_unavailable conn ~actor
          ~post:absent_post ~destination:d ()
      in
      let* absent_community = find conn "absent community" q_absent_community () in
      let* () =
        request_expect "absent destination" Store.Community_unavailable conn
          ~actor ~post ~destination:absent_community ()
      in
      let* () =
        request_expect "origin as destination" Store.Same_community conn
          ~actor ~post ~destination:o ()
      in
      check_event_count "missing" conn ~post 0)

let request_connection_case =
  Shared_thread_fixture.db_case "request: only an accepted connection carries a placement"
    (fun conn ->
      let* actor, o, _, post, _ = Shared_thread_fixture.fixture conn "conn" in
      (* No connection at all. *)
      let* stranger = Shared_thread_fixture.insert_community conn "stp-conn-stranger" in
      let* () =
        request_expect "no connection" Store.No_accepted_connection conn
          ~actor ~post ~destination:stranger ()
      in
      (* A still-pending connection is not an accepted one. *)
      let* pending_target = Shared_thread_fixture.insert_community conn "stp-conn-pending" in
      let value =
        match
          Cc.create_pending ~requester_community_id:o
            ~recipient_community_id:pending_target ~request_note:None
        with
        | Ok v -> v
        | Error _ -> Alcotest.fail "fixture: pending connection value"
      in
      let* requested =
        Ccs.request conn ~actor_user_id:actor ~connection:value
      in
      let* () =
        match requested with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "fixture: pending connection %s"
              (Community_fixture.error_str e)
      in
      let* () =
        request_expect "pending connection" Store.No_accepted_connection
          conn ~actor ~post ~destination:pending_target ()
      in
      (* A removed connection no longer carries placements either. *)
      let* removed_target = Shared_thread_fixture.insert_community conn "stp-conn-removed" in
      let* connection = Shared_thread_fixture.connect conn ~actor o removed_target in
      let* () =
        Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:o
      in
      let* () =
        request_expect "removed connection" Store.No_accepted_connection
          conn ~actor ~post ~destination:removed_target ()
      in
      check_event_count "connection" conn ~post 0)

let request_eligibility_case =
  Shared_thread_fixture.db_case "request: both sides are revalidated under the held locks"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "elig" in
      let* () = exec conn "origin private" Community_fixture.q_make_private o in
      let* () =
        request_expect "private origin" Store.Origin_ineligible conn ~actor
          ~post ~destination:d ()
      in
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      let* () = exec conn "destination draft" Community_fixture.q_make_draft_state d in
      let* () =
        request_expect "draft destination" Store.Destination_ineligible
          conn ~actor ~post ~destination:d ()
      in
      let* () = exec conn "restore destination" Shared_thread_fixture.q_make_eligible d in
      let* _ = Shared_thread_fixture.request_ok "eligible again" conn ~actor ~post ~destination:d () in
      check_event_count "eligibility" conn ~post 1)

let request_tombstone_case =
  Shared_thread_fixture.db_case "request: a tombstoned canonical thread is not shareable"
    (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "tomb" in
      let* () = Shared_thread_fixture.tombstone conn post "[removed by moderator]" in
      let* () =
        request_expect "moderator tombstone" Store.Post_tombstoned conn
          ~actor ~post ~destination:d ()
      in
      let* () = Shared_thread_fixture.tombstone conn post "[deleted]" in
      let* () =
        request_expect "author tombstone" Store.Post_tombstoned conn ~actor
          ~post ~destination:d ()
      in
      check_event_count "tombstone" conn ~post 0)

let request_duplicate_case =
  Shared_thread_fixture.db_case "request: a duplicate loses against pending and accepted alike"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "dup" in
      let* id = Shared_thread_fixture.request_ok "first" conn ~actor ~post ~destination:d () in
      let* () =
        request_expect "duplicate against pending"
          Store.Active_placement_exists conn ~actor ~post ~destination:d ()
      in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      let* () =
        request_expect "duplicate against accepted"
          Store.Active_placement_exists conn ~actor ~post ~destination:d ()
      in
      let* n = find conn "row count" q_count_pair (post, d) in
      Alcotest.(check int) "still exactly one row" 1 n;
      check_events "duplicate" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d))
        ; (("shared_thread_accepted", Some actor), (post, o, d)) ])

let request_multiple_destinations_case =
  Shared_thread_fixture.db_case "request: one thread reaches several destinations as rows"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "multi" in
      let* e = Shared_thread_fixture.insert_community conn "stp-multi-e" in
      let* _ = Shared_thread_fixture.connect conn ~actor o e in
      let* first = Shared_thread_fixture.request_ok "to d" conn ~actor ~post ~destination:d () in
      let* second = Shared_thread_fixture.request_ok "to e" conn ~actor ~post ~destination:e () in
      Alcotest.(check bool) "distinct rows" false (first = second);
      let* nd = find conn "d count" q_count_pair (post, d) in
      let* ne = find conn "e count" q_count_pair (post, e) in
      Alcotest.(check int) "one row toward d" 1 nd;
      Alcotest.(check int) "one row toward e" 1 ne;
      check_event_count "multi" conn ~post 2)

(* === review === *)

let accept_into_section_case =
  Shared_thread_fixture.db_case "accept: a sectioned destination accepts into its own section"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "accsec" in
      let* () = exec conn "sectioned destination" Shared_thread_fixture.q_set_sections (d, true) in
      let* section = find conn "section" Shared_thread_fixture.q_insert_section (d, "general") in
      let* reviewer = insert_user conn "stp_accsec_mod" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* reviewed =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer ~placement:id ~destination:d
          (Store.Accept (Some section)) P.Accepted
      in
      Alcotest.(check int) "post crosses back" post
        (Store.reviewed_post_id reviewed);
      Alcotest.(check int) "origin crosses back" o
        (Store.reviewed_origin_community_id reviewed);
      Alcotest.(check int) "destination crosses back" d
        (Store.reviewed_destination_community_id reviewed);
      Alcotest.(check (option int)) "section crosses back" (Some section)
        (Store.reviewed_destination_section_id reviewed);
      let* () =
        check_row "accept" conn id ~status:"accepted"
          ~section:(Some section) ~note:None ~requested_by:(Some actor)
          ~reviewed_by:(Some reviewer) ~removed_by:None ~withdrawn_by:None
      in
      check_events "accept" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d))
        ; (("shared_thread_accepted", Some reviewer), (post, o, d)) ])

let accept_flat_case =
  Shared_thread_fixture.db_case "accept: a flat destination accepts with no section" (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "accflat" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      (* A section supplied to a flat destination is refused, not
         silently dropped. *)
      let* () =
        review_expect "section on a flat destination"
          Store.Invalid_destination_section conn ~reviewer:actor
          ~placement:id ~destination:d (Store.Accept (Some 12345))
      in
      let* reviewed =
        Shared_thread_fixture.review_ok "flat accept" conn ~reviewer:actor ~placement:id
          ~destination:d (Store.Accept None) P.Accepted
      in
      Alcotest.(check (option int)) "no section" None
        (Store.reviewed_destination_section_id reviewed);
      check_row "flat accept" conn id ~status:"accepted" ~section:None
        ~note:None ~requested_by:(Some actor) ~reviewed_by:(Some actor)
        ~removed_by:None ~withdrawn_by:None)

let accept_section_boundary_case =
  Shared_thread_fixture.db_case "accept: the section must be a live section of the destination"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "secbound" in
      let* () = exec conn "sectioned destination" Shared_thread_fixture.q_set_sections (d, true) in
      let* foreign = find conn "origin section" Shared_thread_fixture.q_insert_section (o, "own") in
      let* stale = find conn "stale section" Shared_thread_fixture.q_insert_section (d, "old") in
      let* () = exec conn "delete stale" Shared_thread_fixture.q_delete_section stale in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () =
        review_expect "no section chosen" Store.Invalid_destination_section
          conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None)
      in
      let* () =
        review_expect "another community's section"
          Store.Invalid_destination_section conn ~reviewer:actor
          ~placement:id ~destination:d (Store.Accept (Some foreign))
      in
      let* () =
        review_expect "a deleted section"
          Store.Invalid_destination_section conn ~reviewer:actor
          ~placement:id ~destination:d (Store.Accept (Some stale))
      in
      let* () =
        review_expect "a non-positive section"
          Store.Invalid_destination_section conn ~reviewer:actor
          ~placement:id ~destination:d (Store.Accept (Some 0))
      in
      let* () =
        check_row "untouched" conn id ~status:"pending" ~section:None
          ~note:None ~requested_by:(Some actor) ~reviewed_by:None
          ~removed_by:None ~withdrawn_by:None
      in
      check_event_count "section boundary" conn ~post 1)

let accept_revalidation_case =
  Shared_thread_fixture.db_case
    "accept: connection, eligibility, and content are revalidated; \
     reject is not gated"
    (fun conn ->
      (* Connection removed before acceptance. *)
      let* actor, o, d, post, connection = Shared_thread_fixture.fixture conn "reval" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () = Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:d in
      let* () =
        review_expect "accept after disconnect"
          Store.No_accepted_connection conn ~reviewer:actor ~placement:id
          ~destination:d (Store.Accept None)
      in
      (* Eligibility lost before acceptance. *)
      let* connection = Shared_thread_fixture.connect conn ~actor o d in
      let* () = exec conn "destination private" Community_fixture.q_make_private d in
      let* () =
        review_expect "accept while destination ineligible"
          Store.Destination_ineligible conn ~reviewer:actor ~placement:id
          ~destination:d (Store.Accept None)
      in
      let* () = exec conn "restore destination" Shared_thread_fixture.q_make_eligible d in
      let* () = exec conn "origin private" Community_fixture.q_make_private o in
      let* () =
        review_expect "accept while origin ineligible"
          Store.Origin_ineligible conn ~reviewer:actor ~placement:id
          ~destination:d (Store.Accept None)
      in
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      (* Content tombstoned before acceptance. *)
      let* () = Shared_thread_fixture.tombstone conn post "[removed by admin]" in
      let* () =
        review_expect "accept a tombstoned thread" Store.Post_tombstoned
          conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None)
      in
      (* And after ALL of that — disconnect again, break eligibility,
         keep the tombstone — rejection still closes the request. *)
      let* () = Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:o in
      let* () = exec conn "destination private again" Community_fixture.q_make_private d in
      let* _ =
        Shared_thread_fixture.review_ok "reject the stale request" conn ~reviewer:actor
          ~placement:id ~destination:d Store.Reject P.Rejected
      in
      check_row "rejected" conn id ~status:"rejected" ~section:None
        ~note:None ~requested_by:(Some actor) ~reviewed_by:(Some actor)
        ~removed_by:None ~withdrawn_by:None)

let accept_precedence_case =
  Shared_thread_fixture.db_case
    "accept: the locked post answers before the connection gate"
    (fun conn ->
      (* The connection lock is acquired ahead of the post lock (the
         single shared lock order), but its absence is judged after the
         locked post has answered: a thread both tombstoned and
         disconnected refuses as Post_tombstoned, never as
         No_accepted_connection. *)
      let* actor, _o, d, post, connection = Shared_thread_fixture.fixture conn "prec" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () = Shared_thread_fixture.tombstone conn post "[removed by admin]" in
      let* () = Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:d in
      let* () =
        review_expect "tombstoned and disconnected" Store.Post_tombstoned
          conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None)
      in
      let* () =
        check_row "untouched" conn id ~status:"pending" ~section:None
          ~note:None ~requested_by:(Some actor) ~reviewed_by:None
          ~removed_by:None ~withdrawn_by:None
      in
      check_event_count "only the request" conn ~post 1)

let review_boundary_case =
  Shared_thread_fixture.db_case "review: the destination is verified in the mutation boundary"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "revbound" in
      let* c = Shared_thread_fixture.insert_community conn "stp-revbound-c" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () =
        review_expect "another community's queue" Store.Review_unavailable
          conn ~reviewer:actor ~placement:id ~destination:c
          (Store.Accept None)
      in
      let* () =
        (* Not even the origin may review its own request through the
           destination boundary. *)
        review_expect "the origin itself" Store.Review_unavailable conn
          ~reviewer:actor ~placement:id ~destination:o (Store.Accept None)
      in
      let* () =
        check_row "untouched" conn id ~status:"pending" ~section:None
          ~note:None ~requested_by:(Some actor) ~reviewed_by:None
          ~removed_by:None ~withdrawn_by:None
      in
      check_events "no event" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d)) ])

let review_stale_case =
  Shared_thread_fixture.db_case "review: a settled row cannot be reviewed again" (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "revstale" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      let* () =
        review_expect "second accept" Store.Review_unavailable conn
          ~reviewer:actor ~placement:id ~destination:d (Store.Accept None)
      in
      let* () =
        review_expect "late reject" Store.Review_unavailable conn
          ~reviewer:actor ~placement:id ~destination:d Store.Reject
      in
      let* absent = find conn "absent id" q_absent_placement () in
      let* () =
        review_expect "absent placement" Store.Review_unavailable conn
          ~reviewer:actor ~placement:absent ~destination:d
          (Store.Accept None)
      in
      let* () =
        review_expect "zero reviewer" Store.Invalid_user_id conn
          ~reviewer:0 ~placement:id ~destination:d (Store.Accept None)
      in
      check_events "one review only" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d))
        ; (("shared_thread_accepted", Some actor), (post, o, d)) ])

(* === withdraw === *)

let withdraw_case =
  Shared_thread_fixture.db_case "withdraw: pending becomes withdrawn and frees the slot"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "wd" in
      let* id =
        Shared_thread_fixture.request_ok "request" conn ~actor ~note:"kept" ~post ~destination:d ()
      in
      let* withdrawn = Shared_thread_fixture.withdraw_ok "withdraw" conn ~actor ~placement:id ~origin:o in
      Alcotest.(check int) "post crosses back" post
        (Store.withdrawn_post_id withdrawn);
      Alcotest.(check int) "origin crosses back" o
        (Store.withdrawn_origin_community_id withdrawn);
      Alcotest.(check int) "destination crosses back" d
        (Store.withdrawn_destination_community_id withdrawn);
      let* () =
        check_row "withdrawn" conn id ~status:"withdrawn" ~section:None
          ~note:(Some "kept") ~requested_by:(Some actor) ~reviewed_by:None
          ~removed_by:None ~withdrawn_by:(Some actor)
      in
      let* reviewed, removed, withdrawn_at, coherent =
        find conn "stamps" q_stamps id
      in
      Alcotest.(check bool) "no review time" false reviewed;
      Alcotest.(check bool) "no removal time" false removed;
      Alcotest.(check bool) "withdrawal time set" true withdrawn_at;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      let* () =
        check_events "withdraw" conn ~placement:id
          [ (("shared_thread_requested", Some actor), (post, o, d))
          ; (("shared_thread_withdrawn", Some actor), (post, o, d)) ]
      in
      (* The slot is free: a fresh request is a new row. *)
      let* fresh = Shared_thread_fixture.request_ok "fresh request" conn ~actor ~post ~destination:d () in
      Alcotest.(check bool) "a new row, not a reopened one" false
        (fresh = id);
      let* total = find conn "history" q_count_pair (post, d) in
      Alcotest.(check int) "history retained" 2 total;
      let* active = find conn "active" q_count_active_pair (post, d) in
      Alcotest.(check int) "exactly one active" 1 active;
      Lwt.return_unit)

let withdraw_survives_drift_case =
  Shared_thread_fixture.db_case
    "withdraw: still possible after disconnect, ineligibility, and \
     tombstoning"
    (fun conn ->
      let* actor, o, d, post, connection = Shared_thread_fixture.fixture conn "wddrift" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () = Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:o in
      let* () = exec conn "origin private" Community_fixture.q_make_private o in
      let* () = exec conn "destination draft" Community_fixture.q_make_draft_state d in
      let* () = Shared_thread_fixture.tombstone conn post "[deleted]" in
      let* _ = Shared_thread_fixture.withdraw_ok "withdraw the stale request" conn ~actor
          ~placement:id ~origin:o
      in
      check_row "withdrawn" conn id ~status:"withdrawn" ~section:None
        ~note:None ~requested_by:(Some actor) ~reviewed_by:None
        ~removed_by:None ~withdrawn_by:(Some actor))

let withdraw_stale_case =
  Shared_thread_fixture.db_case "withdraw: only a pending row, and only through the origin"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "wdstale" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () =
        (* The destination cannot withdraw the origin's request. *)
        withdraw_expect "the destination boundary"
          Store.Withdrawal_unavailable conn ~actor ~placement:id ~origin:d
      in
      let* _ = Shared_thread_fixture.withdraw_ok "withdraw" conn ~actor ~placement:id ~origin:o in
      let* () =
        withdraw_expect "a second withdrawal" Store.Withdrawal_unavailable
          conn ~actor ~placement:id ~origin:o
      in
      (* An accepted row is not withdrawable — that is removal's job. *)
      let* second = Shared_thread_fixture.request_ok "fresh request" conn ~actor ~post ~destination:d () in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:second
          ~destination:d (Store.Accept None) P.Accepted
      in
      let* () =
        withdraw_expect "an accepted row" Store.Withdrawal_unavailable conn
          ~actor ~placement:second ~origin:o
      in
      let* absent = find conn "absent id" q_absent_placement () in
      let* () =
        withdraw_expect "an absent placement" Store.Withdrawal_unavailable
          conn ~actor ~placement:absent ~origin:o
      in
      let* () =
        withdraw_expect "zero actor" Store.Invalid_user_id conn ~actor:0
          ~placement:second ~origin:o
      in
      check_event_count "stale withdrawals" conn ~post 4)

(* === remove === *)

let remove_case =
  Shared_thread_fixture.db_case "remove: either side detaches an accepted placement" (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "rm" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      let* remover = insert_user conn "stp_rm_actor" in
      let* removed = Shared_thread_fixture.remove_ok "remove from the origin side" conn
          ~actor:remover ~placement:id ~acting:o
      in
      Alcotest.(check int) "post crosses back" post
        (Store.removed_post_id removed);
      Alcotest.(check int) "origin crosses back" o
        (Store.removed_origin_community_id removed);
      Alcotest.(check int) "destination crosses back" d
        (Store.removed_destination_community_id removed);
      let* () =
        check_row "removed" conn id ~status:"removed" ~section:None
          ~note:None ~requested_by:(Some actor) ~reviewed_by:(Some actor)
          ~removed_by:(Some remover) ~withdrawn_by:None
      in
      let* reviewed, removed_at, withdrawn, coherent =
        find conn "stamps" q_stamps id
      in
      Alcotest.(check bool) "the review survives" true reviewed;
      Alcotest.(check bool) "removal time set" true removed_at;
      Alcotest.(check bool) "no withdrawal time" false withdrawn;
      Alcotest.(check bool) "updated_at coherent" true coherent;
      let* () =
        check_events "remove" conn ~placement:id
          [ (("shared_thread_requested", Some actor), (post, o, d))
          ; (("shared_thread_accepted", Some actor), (post, o, d))
          ; (("shared_thread_removed", Some remover), (post, o, d)) ]
      in
      (* The destination detaches just as unilaterally. *)
      let* second = Shared_thread_fixture.request_ok "second request" conn ~actor ~post ~destination:d () in
      let* _ =
        Shared_thread_fixture.review_ok "accept again" conn ~reviewer:actor ~placement:second
          ~destination:d (Store.Accept None) P.Accepted
      in
      let* _ = Shared_thread_fixture.remove_ok "remove from the destination side" conn
          ~actor:remover ~placement:second ~acting:d
      in
      check_row "removed again" conn second ~status:"removed" ~section:None
        ~note:None ~requested_by:(Some actor) ~reviewed_by:(Some actor)
        ~removed_by:(Some remover) ~withdrawn_by:None)

let remove_survives_drift_case =
  Shared_thread_fixture.db_case
    "remove: still possible after disconnect, ineligibility, \
     tombstoning, and section deletion"
    (fun conn ->
      let* actor, o, d, post, connection = Shared_thread_fixture.fixture conn "rmdrift" in
      let* () = exec conn "sectioned destination" Shared_thread_fixture.q_set_sections (d, true) in
      let* section = find conn "section" Shared_thread_fixture.q_insert_section (d, "gen") in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept (Some section)) P.Accepted
      in
      let* () = Shared_thread_fixture.disconnect conn ~actor ~connection ~acting:d in
      let* () = exec conn "origin private" Community_fixture.q_make_private o in
      let* () = Shared_thread_fixture.tombstone conn post "[removed by moderator]" in
      let* () = exec conn "delete section" Shared_thread_fixture.q_delete_section section in
      let* _ = Shared_thread_fixture.remove_ok "remove the stale placement" conn ~actor
          ~placement:id ~acting:d
      in
      (* The accepted section was already released by the deletion; the
         removal keeps that history as-is. *)
      check_row "removed" conn id ~status:"removed" ~section:None
        ~note:None ~requested_by:(Some actor) ~reviewed_by:(Some actor)
        ~removed_by:(Some actor) ~withdrawn_by:None)

let remove_scope_case =
  Shared_thread_fixture.db_case "remove: only the selected placement changes" (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "rmscope" in
      let* e = Shared_thread_fixture.insert_community conn "stp-rmscope-e" in
      let* () = exec conn "flat e" Shared_thread_fixture.q_set_sections (e, false) in
      let* _ = Shared_thread_fixture.connect conn ~actor o e in
      let* commenter = insert_user conn "stp_rmscope_commenter" in
      let* _ = find conn "comment" Shared_thread_fixture.q_insert_comment (post, commenter) in
      let* first = Shared_thread_fixture.request_ok "to d" conn ~actor ~post ~destination:d () in
      let* second = Shared_thread_fixture.request_ok "to e" conn ~actor ~post ~destination:e () in
      let* _ =
        Shared_thread_fixture.review_ok "accept d" conn ~reviewer:actor ~placement:first
          ~destination:d (Store.Accept None) P.Accepted
      in
      let* _ =
        Shared_thread_fixture.review_ok "accept e" conn ~reviewer:actor ~placement:second
          ~destination:e (Store.Accept None) P.Accepted
      in
      let* _ = Shared_thread_fixture.remove_ok "remove d" conn ~actor ~placement:first ~acting:d in
      (* The sibling placement, the canonical post, and its comments are
         untouched. *)
      let* () =
        check_row "sibling untouched" conn second ~status:"accepted"
          ~section:None ~note:None ~requested_by:(Some actor)
          ~reviewed_by:(Some actor) ~removed_by:None ~withdrawn_by:None
      in
      let* title, content = find conn "post" Shared_thread_fixture.q_post_row post in
      Alcotest.(check string) "title kept" "stp thread" title;
      Alcotest.(check (option string)) "content kept" (Some "stp body")
        content;
      let* comments = find conn "comments" Shared_thread_fixture.q_comment_count post in
      Alcotest.(check int) "comments kept" 1 comments;
      Lwt.return_unit)

let remove_stale_case =
  Shared_thread_fixture.db_case "remove: only an accepted row, and only from inside the pair"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "rmstale" in
      let* stranger = Shared_thread_fixture.insert_community conn "stp-rmstale-c" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () =
        remove_expect "a pending row" Store.Removal_unavailable conn ~actor
          ~placement:id ~acting:o
      in
      let* _ =
        Shared_thread_fixture.review_ok "accept" conn ~reviewer:actor ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      let* () =
        remove_expect "an outside community" Store.Removal_unavailable conn
          ~actor ~placement:id ~acting:stranger
      in
      let* absent = find conn "absent id" q_absent_placement () in
      let* () =
        remove_expect "an absent placement" Store.Removal_unavailable conn
          ~actor ~placement:absent ~acting:o
      in
      let* _ = Shared_thread_fixture.remove_ok "first removal" conn ~actor ~placement:id ~acting:o in
      let* () =
        remove_expect "a second removal" Store.Removal_unavailable conn
          ~actor ~placement:id ~acting:d
      in
      (* Exactly one removed event survives all of that. *)
      check_events "one removal only" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d))
        ; (("shared_thread_accepted", Some actor), (post, o, d))
        ; (("shared_thread_removed", Some actor), (post, o, d)) ])

let q_poison_audit =
  Shared_thread_fixture.ddl
    "CREATE TRIGGER stp_fail_audit \
     BEFORE INSERT ON shared_thread_placement_audit_events \
     FOR EACH ROW EXECUTE FUNCTION stp_fail_fn()"

let q_unpoison_audit =
  Shared_thread_fixture.ddl
    "DROP TRIGGER IF EXISTS stp_fail_audit \
     ON shared_thread_placement_audit_events"

let q_poison_insert =
  Shared_thread_fixture.ddl
    "CREATE TRIGGER stp_fail_business \
     AFTER INSERT ON shared_thread_placements \
     FOR EACH ROW EXECUTE FUNCTION stp_fail_fn()"

let q_poison_update =
  Shared_thread_fixture.ddl
    "CREATE TRIGGER stp_fail_business \
     AFTER UPDATE ON shared_thread_placements \
     FOR EACH ROW EXECUTE FUNCTION stp_fail_fn()"

let q_unpoison_business =
  Shared_thread_fixture.ddl "DROP TRIGGER IF EXISTS stp_fail_business ON shared_thread_placements"

let audit_failure_rolls_back_case =
  Shared_thread_fixture.db_case "atomicity: a failed audit insert rolls the whole mutation back"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "auditfail" in
      let* () =
        Shared_thread_fixture.with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            request_expect "request with audit poisoned" Store.Storage_error
              conn ~actor ~post ~destination:d ())
      in
      let* n = find conn "no placement row" q_count_pair (post, d) in
      Alcotest.(check int) "no placement committed" 0 n;
      let* () = check_event_count "no events" conn ~post 0 in
      let* notifs = find conn "no notifications" Shared_thread_fixture.q_notifs_for_post post in
      Alcotest.(check int) "no notifications committed" 0 notifs;
      (* And once the poison is gone the very same request commits. *)
      let* id = Shared_thread_fixture.request_ok "request afterwards" conn ~actor ~post
          ~destination:d ()
      in
      check_events "committed together" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d)) ])

let mutation_failure_leaves_no_event_case =
  Shared_thread_fixture.db_case "atomicity: a failed mutation leaves no event or notification"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "bizfail" in
      let* () =
        Shared_thread_fixture.with_poison conn ~install:q_poison_insert ~remove:q_unpoison_business
          (fun () ->
            request_expect "request with the insert poisoned"
              Store.Storage_error conn ~actor ~post ~destination:d ())
      in
      let* n = find conn "no placement row" q_count_pair (post, d) in
      Alcotest.(check int) "no placement committed" 0 n;
      let* () = check_event_count "no events" conn ~post 0 in
      (* The same holds for the update-driven transitions. *)
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      let* () =
        Shared_thread_fixture.with_poison conn ~install:q_poison_update ~remove:q_unpoison_business
          (fun () ->
            review_expect "accept with the update poisoned"
              Store.Storage_error conn ~reviewer:actor ~placement:id
              ~destination:d (Store.Accept None))
      in
      let* () =
        check_row "still pending" conn id ~status:"pending" ~section:None
          ~note:None ~requested_by:(Some actor) ~reviewed_by:None
          ~removed_by:None ~withdrawn_by:None
      in
      check_events "only the request survives" conn ~placement:id
        [ (("shared_thread_requested", Some actor), (post, o, d)) ])

let notification_failure_rolls_back_case =
  Shared_thread_fixture.db_case
    "atomicity: a failed notification insert rolls mutation and audit back"
    (fun conn ->
      let* actor, o, d, post, _ = Shared_thread_fixture.fixture conn "notiffail" in
      (* A destination top moderator, so the request path really attempts
         a notification insert. *)
      let* tm = insert_user conn "stp_notiffail_tm" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (tm, d, "top_mod")
      in
      let* () =
        Shared_thread_fixture.with_poison conn ~install:Shared_thread_fixture.q_poison_notif ~remove:Shared_thread_fixture.q_unpoison_notif
          (fun () ->
            request_expect "request with notifications poisoned"
              Store.Storage_error conn ~actor ~post ~destination:d ())
      in
      let* n = find conn "no placement row" q_count_pair (post, d) in
      Alcotest.(check int) "no placement committed" 0 n;
      let* () = check_event_count "no events" conn ~post 0 in
      let* notifs = find conn "no notifications" Shared_thread_fixture.q_notifs_for_post post in
      Alcotest.(check int) "no notifications committed" 0 notifs;
      (* Afterwards the same request commits whole: row, event, and the
         top moderator's notification. *)
      let* id = Shared_thread_fixture.request_ok "request afterwards" conn ~actor ~post
          ~destination:d ()
      in
      let* () =
        check_events "committed together" conn ~placement:id
          [ (("shared_thread_requested", Some actor), (post, o, d)) ]
      in
      let* notifs = find conn "one notification" Shared_thread_fixture.q_notifs_for_post post in
      Alcotest.(check int) "exactly one notification" 1 notifs;
      Lwt.return_unit)

(* === concurrency === *)

let concurrent_request_case =
  Shared_thread_fixture.db_case "concurrency: identical requests leave exactly one active row"
    (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "race" in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Shared_thread_fixture.request conn ~actor ~post ~destination:d ())
              (Shared_thread_fixture.request conn2 ~actor ~post ~destination:d ())
          in
          (match (r1, r2) with
          | Ok _, Error Store.Active_placement_exists
          | Error Store.Active_placement_exists, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both requests won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)" (Shared_thread_fixture.error_str e)
                (Shared_thread_fixture.error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s" (Shared_thread_fixture.error_str e));
          let* total = find conn "row count" q_count_pair (post, d) in
          Alcotest.(check int) "exactly one row" 1 total;
          let* events = find conn "events" Shared_thread_fixture.q_events_for_post post in
          Alcotest.(check int) "exactly one event" 1 events;
          Lwt.return_unit))

let concurrent_review_case =
  Shared_thread_fixture.db_case "concurrency: only one review of a pending row commits"
    (fun conn ->
      let* actor, _, d, post, _ = Shared_thread_fixture.fixture conn "race2" in
      let* id = Shared_thread_fixture.request_ok "request" conn ~actor ~post ~destination:d () in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Shared_thread_fixture.review conn ~reviewer:actor ~placement:id ~destination:d
                 (Store.Accept None))
              (Shared_thread_fixture.review conn2 ~reviewer:actor ~placement:id ~destination:d
                 Store.Reject)
          in
          (match (r1, r2) with
          | Ok _, Error Store.Review_unavailable
          | Error Store.Review_unavailable, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both reviews won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)" (Shared_thread_fixture.error_str e)
                (Shared_thread_fixture.error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s" (Shared_thread_fixture.error_str e));
          let* events = find conn "events" Shared_thread_fixture.q_events_for_post post in
          Alcotest.(check int) "the request plus exactly one review" 2
            events;
          Lwt.return_unit))

let request_suite =
  [ request_case; request_blank_note_case; request_validation_case
  ; request_missing_subjects_case; request_connection_case
  ; request_eligibility_case; request_tombstone_case
  ; request_duplicate_case; request_multiple_destinations_case ]

let review_suite =
  [ accept_into_section_case; accept_flat_case
  ; accept_section_boundary_case; accept_revalidation_case
  ; accept_precedence_case; review_boundary_case; review_stale_case ]

let withdraw_suite =
  [ withdraw_case; withdraw_survives_drift_case; withdraw_stale_case ]

let remove_suite =
  [ remove_case; remove_survives_drift_case; remove_scope_case
  ; remove_stale_case ]

let atomicity_suite =
  [ audit_failure_rolls_back_case; mutation_failure_leaves_no_event_case
  ; notification_failure_rolls_back_case ]

let concurrency_suite = [ concurrent_request_case; concurrent_review_case ]

let suites =
  [ ("shared_thread_placement_store_request", request_suite)
  ; ("shared_thread_placement_store_review", review_suite)
  ; ("shared_thread_placement_store_withdraw", withdraw_suite)
  ; ("shared_thread_placement_store_remove", remove_suite)
  ; ("shared_thread_placement_atomicity", atomicity_suite)
  ; ("shared_thread_placement_concurrency", concurrency_suite)
  ]
