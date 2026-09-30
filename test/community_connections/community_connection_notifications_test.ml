(* Community connections, slice 3 — transactional notifications.

   The store's three mutations now write notification rows on the same
   transaction as the guarded UPDATE and its audit event. These cases assert
   the recipient rule (exact top_mod of one named community), the exclusions
   (actor, ordinary and legacy moderators, global admins), deduplication, the
   atomic outcome under forced failure and under concurrency, the durable row
   shapes the CHECK constraint now has to keep unambiguous, and the read and
   render path — including that neither the request note nor any username can
   reach either. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Cc = Earde.Community_connections
module Store = Earde.Community_connections_store
module Notif = Earde.Community_connection_notifications

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let insert_community = Community_fixture.insert_community
let make_project = Home_request_fixture.make_project
let contains haystack needle = Html_assert.occurs haystack ~needle
let status_of = Http_fixture.status_of
let error_str = Community_fixture.error_str

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let add_top_mod conn ~user ~community = add_role conn ~user ~community "top_mod"

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

(* Failure-injection DDL is dropped first so a crashed case can never leave
   a trigger behind. Then the dependency order: notifications (which
   reference connections and communities), the connection audit trail (which
   RESTRICT-protects both its connection and its communities), the
   connections themselves, and only then the project-home fixtures the shape
   cases need, the communities, the users, and the installations. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS ccnt_fail_notif ON notifications";
      "DROP FUNCTION IF EXISTS ccnt_fail_fn()";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'ccnt_%')";
      "DELETE FROM notifications WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'ccnt-%')";
      "DELETE FROM community_connection_audit_events WHERE \
       requester_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccnt-%') OR recipient_community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'ccnt-%')";
      "DELETE FROM community_connections WHERE requester_community_id IN \
       (SELECT id FROM communities WHERE slug LIKE 'ccnt-%') OR \
       recipient_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'ccnt-%')";
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 964100001 \
       AND 964100999)";
      "DELETE FROM project_home_audit_events WHERE community_id IN (SELECT id \
       FROM communities WHERE slug LIKE 'ccnt-%')";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       964100001 AND 964100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 964000001 AND 964000999)";
      "DELETE FROM communities WHERE slug LIKE 'ccnt-%'";
      "DELETE FROM users WHERE username LIKE 'ccnt_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       964000001 AND 964000999";
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
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === observation === *)

(* One tuple per notification of one connection, ordered by recipient so
   expectations do not depend on insertion order. *)
let q_notifs =
  (Caqti_type.int64 ->* Caqti_type.(t2 (t2 int string) (t2 (option int) int)))
    "SELECT user_id, notif_type, actor_user_id, community_id FROM \
     notifications WHERE connection_id = $1 ORDER BY user_id, notif_type"

let q_notif_count =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE connection_id = $1"

(* Every stored byte of one connection's notifications, for the boolean
   absence sweep over notes and usernames. *)
let q_notif_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(n::text, '|'), '<none>') FROM notifications n \
     WHERE connection_id = $1"

(* Creation must never mark anything read. *)
let q_all_unread =
  (Caqti_type.int64 ->! Caqti_type.bool)
    "SELECT COALESCE(BOOL_AND(NOT is_read), TRUE) FROM notifications WHERE \
     connection_id = $1"

let q_event_count =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connection_audit_events WHERE \
     connection_id = $1"

let q_pair_event_count =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_connection_audit_events WHERE \
     requester_community_id = $1 OR recipient_community_id = $1"

let q_pair_notif_count =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE community_id = $1"

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM community_connections WHERE id = $1"

let notif_t = Alcotest.(pair (pair int string) (pair (option int) int))

let check_notifs label conn ~connection expected =
  let* rows = collect conn (label ^ ": notifications") q_notifs connection in
  Alcotest.(check (list notif_t))
    (label ^ ": exact notifications")
    expected rows;
  let* unread = find conn (label ^ ": unread") q_all_unread connection in
  Alcotest.(check bool) (label ^ ": all rows unread") true unread;
  Lwt.return_unit

let check_notif_count label conn ~connection expected =
  let* n = find conn (label ^ ": count") q_notif_count connection in
  Alcotest.(check int) (label ^ ": notification count") expected n;
  Lwt.return_unit

let check_events label conn ~connection expected =
  let* n = find conn (label ^ ": events") q_event_count connection in
  Alcotest.(check int) (label ^ ": audit events") expected n;
  Lwt.return_unit

let check_status label conn id expected =
  let* status = find conn (label ^ ": status") q_status id in
  Alcotest.(check string) (label ^ ": status") expected status;
  Lwt.return_unit

(* === store call helpers, always through the real store === *)

let request conn ~actor ?note ~requester ~recipient () =
  match
    Cc.create_pending ~requester_community_id:requester
      ~recipient_community_id:recipient ~request_note:note
  with
  | Error _ -> Alcotest.fail "fixture: pure pending value refused"
  | Ok connection -> Store.request conn ~actor_user_id:actor ~connection

let request_ok label conn ~actor ?note ~requester ~recipient () =
  let* r = request conn ~actor ?note ~requester ~recipient () in
  match r with
  | Ok created -> Lwt.return (Store.created_connection_id created)
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let request_expect label expected conn ~actor ?note ~requester ~recipient () =
  let* r = request conn ~actor ?note ~requester ~recipient () in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let review conn ~reviewer ~connection ~recipient decision =
  Store.review conn ~reviewer_user_id:reviewer ~connection_id:connection
    ~recipient_community_id:recipient ~decision

let review_ok label conn ~reviewer ~connection ~recipient decision =
  let* r = review conn ~reviewer ~connection ~recipient decision in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let review_expect label expected conn ~reviewer ~connection ~recipient decision
    =
  let* r = review conn ~reviewer ~connection ~recipient decision in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let remove conn ~actor ~connection ~acting =
  Store.remove conn ~actor_user_id:actor ~connection_id:connection
    ~acting_community_id:acting

let remove_ok label conn ~actor ~connection ~acting =
  let* r = remove conn ~actor ~connection ~acting in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let remove_expect label expected conn ~actor ~connection ~acting =
  let* r = remove conn ~actor ~connection ~acting in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* === recipients === *)

let request_recipients_case =
  db_case "request: only the recipient community's exact top mods are notified"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_rq_actor" in
      let* atop = insert_user conn "ccnt_rq_atop" in
      let* btop1 = insert_user conn "ccnt_rq_btop1" in
      let* btop2 = insert_user conn "ccnt_rq_btop2" in
      let* bmod = insert_user conn "ccnt_rq_bmod" in
      let* blegacy = insert_user conn "ccnt_rq_blegacy" in
      let* bmember = insert_user conn "ccnt_rq_bmember" in
      let* admin = insert_user conn "ccnt_rq_admin" in
      let* a = insert_community conn "ccnt-rq-a" in
      let* b = insert_community conn "ccnt-rq-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop1 ~community:b in
      let* () = add_top_mod conn ~user:btop2 ~community:b in
      let* () = add_role conn ~user:bmod ~community:b "mod" in
      let* () = add_role conn ~user:blegacy ~community:b "legacy_mod" in
      let* () = add_member conn ~user:bmember ~community:b in
      let* () = set_admin conn ~user:admin true in
      let* id =
        request_ok "request" conn ~actor ~note:"private note" ~requester:a
          ~recipient:b ()
      in
      (* The recipient community's two top mods, and nobody else: not the
         requesting side's own top mod, not an ordinary or legacy moderator,
         not a member, and never a broadcast to global admins. *)
      let expected =
        List.sort compare
          [
            ((btop1, "community_connection_requested"), (Some actor, b));
            ((btop2, "community_connection_requested"), (Some actor, b));
          ]
      in
      check_notifs "request" conn ~connection:id expected)

let review_recipients_case =
  db_case
    "accept and reject: only the requesting community's top mods are notified"
    (fun ~url:_ conn ->
      let* rtop = insert_user conn "ccnt_rv_rtop" in
      let* atop1 = insert_user conn "ccnt_rv_atop1" in
      let* atop2 = insert_user conn "ccnt_rv_atop2" in
      let* amod = insert_user conn "ccnt_rv_amod" in
      let* alegacy = insert_user conn "ccnt_rv_alegacy" in
      let* a = insert_community conn "ccnt-rv-a" in
      let* b = insert_community conn "ccnt-rv-b" in
      let* c = insert_community conn "ccnt-rv-c" in
      let* () = add_top_mod conn ~user:atop1 ~community:a in
      let* () = add_top_mod conn ~user:atop2 ~community:a in
      let* () = add_role conn ~user:amod ~community:a "mod" in
      let* () = add_role conn ~user:alegacy ~community:a "legacy_mod" in
      let* () = add_top_mod conn ~user:rtop ~community:b in
      let* () = add_top_mod conn ~user:rtop ~community:c in
      (* Accept. *)
      let* accepted =
        request_ok "request b" conn ~actor:atop1 ~requester:a ~recipient:b ()
      in
      let* () =
        review_ok "accept" conn ~reviewer:rtop ~connection:accepted ~recipient:b
          Store.Accept
      in
      let* () =
        check_notifs "accept" conn ~connection:accepted
          (List.sort compare
             [
               ((rtop, "community_connection_requested"), (Some atop1, b));
               ((atop1, "community_connection_accepted"), (Some rtop, a));
               ((atop2, "community_connection_accepted"), (Some rtop, a));
             ])
      in
      (* The ordinary and legacy moderators of the requesting community are
         absent from that exact list, as is the reviewing side itself beyond
         the request row it was owed. Reject, on a second pair. *)
      let* rejected =
        request_ok "request c" conn ~actor:atop2 ~requester:a ~recipient:c ()
      in
      let* () =
        review_ok "reject" conn ~reviewer:rtop ~connection:rejected ~recipient:c
          Store.Reject
      in
      check_notifs "reject" conn ~connection:rejected
        (List.sort compare
           [
             ((rtop, "community_connection_requested"), (Some atop2, c));
             ((atop1, "community_connection_rejected"), (Some rtop, a));
             ((atop2, "community_connection_rejected"), (Some rtop, a));
           ]))

let removal_recipients_case =
  db_case
    "remove: the opposite community's top mods are notified, from either side"
    (fun ~url:_ conn ->
      let* atop = insert_user conn "ccnt_rm_atop" in
      let* btop = insert_user conn "ccnt_rm_btop" in
      let* a = insert_community conn "ccnt-rm-a" in
      let* b = insert_community conn "ccnt-rm-b" in
      let* c = insert_community conn "ccnt-rm-c" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* () = add_top_mod conn ~user:btop ~community:c in
      (* Removed by the requester side: the recipient side hears about it. *)
      let* first =
        request_ok "request" conn ~actor:atop ~requester:a ~recipient:b ()
      in
      let* () =
        review_ok "accept" conn ~reviewer:btop ~connection:first ~recipient:b
          Store.Accept
      in
      let* () =
        remove_ok "remove from a" conn ~actor:atop ~connection:first ~acting:a
      in
      let* () =
        check_notifs "removed by requester" conn ~connection:first
          (List.sort compare
             [
               ((btop, "community_connection_requested"), (Some atop, b));
               ((atop, "community_connection_accepted"), (Some btop, a));
               ((btop, "community_connection_removed"), (Some atop, b));
             ])
      in
      (* Removed by the recipient side: the requester side hears about it. *)
      let* second =
        request_ok "request 2" conn ~actor:atop ~requester:a ~recipient:c ()
      in
      let* () =
        review_ok "accept 2" conn ~reviewer:btop ~connection:second ~recipient:c
          Store.Accept
      in
      let* () =
        remove_ok "remove from c" conn ~actor:btop ~connection:second ~acting:c
      in
      check_notifs "removed by recipient" conn ~connection:second
        (List.sort compare
           [
             ((btop, "community_connection_requested"), (Some atop, c));
             ((atop, "community_connection_accepted"), (Some btop, a));
             ((atop, "community_connection_removed"), (Some btop, a));
           ]))

let actor_excluded_case =
  db_case
    "the acting user is never notified, even holding top_mod on the notified \
     side" (fun ~url:_ conn ->
      let* both = insert_user conn "ccnt_ax_both" in
      let* other = insert_user conn "ccnt_ax_other" in
      let* a = insert_community conn "ccnt-ax-a" in
      let* b = insert_community conn "ccnt-ax-b" in
      (* The actor is top_mod of BOTH communities, so every recipient set
         below would contain them if the exclusion were not unconditional. *)
      let* () = add_top_mod conn ~user:both ~community:a in
      let* () = add_top_mod conn ~user:both ~community:b in
      let* () = add_top_mod conn ~user:other ~community:b in
      let* id =
        request_ok "request" conn ~actor:both ~requester:a ~recipient:b ()
      in
      let* () =
        check_notifs "request excludes its actor" conn ~connection:id
          [ ((other, "community_connection_requested"), (Some both, b)) ]
      in
      (* And on the review, whose recipient set is the actor's own other
         community. *)
      let* () =
        review_ok "accept" conn ~reviewer:both ~connection:id ~recipient:b
          Store.Accept
      in
      check_notifs "accept excludes its actor" conn ~connection:id
        [ ((other, "community_connection_requested"), (Some both, b)) ])

let dedup_case =
  db_case "recipients are deduplicated: one row per user per kind"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_dd_actor" in
      let* dup = insert_user conn "ccnt_dd_dup" in
      let* a = insert_community conn "ccnt-dd-a" in
      let* b = insert_community conn "ccnt-dd-b" in
      let* () = add_top_mod conn ~user:dup ~community:a in
      let* () = add_top_mod conn ~user:dup ~community:b in
      (* End to end: dup is top_mod on both sides, so both the request and
         the review name them — once each, never twice for one kind. *)
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        review_ok "accept" conn ~reviewer:actor ~connection:id ~recipient:b
          Store.Accept
      in
      let* () =
        check_notifs "one row per kind" conn ~connection:id
          (List.sort compare
             [
               ((dup, "community_connection_requested"), (Some actor, b));
               ((dup, "community_connection_accepted"), (Some actor, a));
             ])
      in
      (* And directly: a caller-supplied list carrying the same id three
         times, plus the actor, still yields exactly one row. The unique
         index would refuse a second, so this proves the filter runs before
         SQL rather than being rescued by it. *)
      let* fresh = insert_user conn "ccnt_dd_fresh" in
      let* c = insert_community conn "ccnt-dd-c" in
      let* () = add_top_mod conn ~user:fresh ~community:c in
      let* direct =
        request_ok "second request" conn ~actor ~requester:a ~recipient:c ()
      in
      let* r =
        Notif.insert_many conn ~kind:Notif.Connection_removed
          ~actor_user_id:actor ~community_id:c ~connection_id:direct
          ~recipient_user_ids:[ fresh; fresh; actor; fresh ]
      in
      (match r with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "direct insert_many refused");
      check_notifs "deduplicated directly" conn ~connection:direct
        (List.sort compare
           [
             ((fresh, "community_connection_requested"), (Some actor, c));
             ((fresh, "community_connection_removed"), (Some actor, c));
           ]))

let zero_recipients_case =
  db_case "a transition with no eligible recipient still commits"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_zr_actor" in
      let* bmod = insert_user conn "ccnt_zr_bmod" in
      let* a = insert_community conn "ccnt-zr-a" in
      let* b = insert_community conn "ccnt-zr-b" in
      (* b has an ordinary moderator and no top mod at all. *)
      let* () = add_role conn ~user:bmod ~community:b "mod" in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () = check_status "committed" conn id "pending" in
      let* () = check_events "audit written" conn ~connection:id 1 in
      let* () = check_notif_count "no notifications" conn ~connection:id 0 in
      (* The same holds for a review and a removal. *)
      let* () =
        review_ok "accept" conn ~reviewer:bmod ~connection:id ~recipient:b
          Store.Accept
      in
      let* () = check_status "accepted" conn id "accepted" in
      let* () = check_events "second event" conn ~connection:id 2 in
      let* () = check_notif_count "still none" conn ~connection:id 0 in
      let* () = remove_ok "remove" conn ~actor ~connection:id ~acting:a in
      let* () = check_events "third event" conn ~connection:id 3 in
      check_notif_count "still none after removal" conn ~connection:id 0)

let stale_transition_case =
  db_case "a stale duplicate accept, reject, or remove creates no notification"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_st_actor" in
      let* atop = insert_user conn "ccnt_st_atop" in
      let* btop = insert_user conn "ccnt_st_btop" in
      let* a = insert_community conn "ccnt-st-a" in
      let* b = insert_community conn "ccnt-st-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      let* () =
        review_ok "accept" conn ~reviewer:btop ~connection:id ~recipient:b
          Store.Accept
      in
      let* () = check_notif_count "one each so far" conn ~connection:id 2 in
      (* Every stale replay: a second accept, a reject after acceptance. *)
      let* () =
        review_expect "second accept" Store.Review_unavailable conn
          ~reviewer:btop ~connection:id ~recipient:b Store.Accept
      in
      let* () =
        review_expect "reject after accept" Store.Review_unavailable conn
          ~reviewer:btop ~connection:id ~recipient:b Store.Reject
      in
      let* () = check_events "still two events" conn ~connection:id 2 in
      let* () = check_notif_count "still two" conn ~connection:id 2 in
      (* And a second removal after the first committed. *)
      let* () = remove_ok "remove" conn ~actor:atop ~connection:id ~acting:a in
      let* () =
        remove_expect "second remove" Store.Removal_unavailable conn ~actor:atop
          ~connection:id ~acting:a
      in
      let* () =
        remove_expect "remove from the other side" Store.Removal_unavailable
          conn ~actor:btop ~connection:id ~acting:b
      in
      let* () = check_events "three events" conn ~connection:id 3 in
      check_notifs "exactly one set per committed transition" conn
        ~connection:id
        (List.sort compare
           [
             ((btop, "community_connection_requested"), (Some actor, b));
             ((atop, "community_connection_accepted"), (Some btop, a));
             ((btop, "community_connection_removed"), (Some atop, b));
           ]))

(* === atomicity === *)

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION ccnt_fail_fn() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN \
     RAISE EXCEPTION ''ccnt fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS ccnt_fail_fn()"

let q_poison_notif =
  ddl
    "CREATE TRIGGER ccnt_fail_notif BEFORE INSERT ON notifications FOR EACH \
     ROW EXECUTE FUNCTION ccnt_fail_fn()"

let q_unpoison_notif =
  ddl "DROP TRIGGER IF EXISTS ccnt_fail_notif ON notifications"

let with_poisoned_notifications conn f =
  let* () = exec conn "create fail fn" q_create_fail_fn () in
  Lwt.finalize
    (fun () ->
      let* () = exec conn "install poison" q_poison_notif () in
      Lwt.finalize f (fun () -> exec conn "drop poison" q_unpoison_notif ()))
    (fun () -> exec conn "drop fail fn" q_drop_fail_fn ())

let notification_failure_rolls_back_case =
  db_case
    "atomicity: a failed notification insert rolls the mutation and its audit \
     event back" (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_nf_actor" in
      let* atop = insert_user conn "ccnt_nf_atop" in
      let* btop = insert_user conn "ccnt_nf_btop" in
      let* a = insert_community conn "ccnt-nf-a" in
      let* b = insert_community conn "ccnt-nf-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* () =
        with_poisoned_notifications conn (fun () ->
            request_expect "request with notifications poisoned"
              Store.Storage_error conn ~actor ~requester:a ~recipient:b ())
      in
      let* events = find conn "pair events" q_pair_event_count a in
      Alcotest.(check int) "no audit event survived" 0 events;
      let* notifs = find conn "pair notifs" q_pair_notif_count b in
      Alcotest.(check int) "no notification survived" 0 notifs;
      (* And once the poison is gone the same request commits all three. *)
      let* id =
        request_ok "request afterwards" conn ~actor ~requester:a ~recipient:b ()
      in
      let* () = check_events "one event" conn ~connection:id 1 in
      let* () = check_notif_count "one notification" conn ~connection:id 1 in
      (* The same atomicity on a review, whose mutation is an UPDATE. *)
      let* () =
        with_poisoned_notifications conn (fun () ->
            review_expect "accept with notifications poisoned"
              Store.Storage_error conn ~reviewer:btop ~connection:id
              ~recipient:b Store.Accept)
      in
      let* () = check_status "still pending" conn id "pending" in
      let* () = check_events "still one event" conn ~connection:id 1 in
      check_notif_count "still one notification" conn ~connection:id 1)

let concurrent_review_case =
  db_case
    "concurrency: the winning review commits exactly one audit event and one \
     notification set" (fun ~url:_ conn ->
      let* actor = insert_user conn "ccnt_cc_actor" in
      let* atop = insert_user conn "ccnt_cc_atop" in
      let* btop = insert_user conn "ccnt_cc_btop" in
      let* a = insert_community conn "ccnt-cc-a" in
      let* b = insert_community conn "ccnt-cc-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* id = request_ok "request" conn ~actor ~requester:a ~recipient:b () in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (review conn ~reviewer:btop ~connection:id ~recipient:b
                 Store.Accept)
              (review conn2 ~reviewer:btop ~connection:id ~recipient:b
                 Store.Accept)
          in
          (match (r1, r2) with
          | Ok _, Error Store.Review_unavailable
          | Error Store.Review_unavailable, Ok _ ->
              ()
          | Ok _, Ok _ -> Alcotest.fail "both reviews won"
          | Error e, Error e' ->
              Alcotest.failf "both failed (%s, %s)" (error_str e) (error_str e')
          | Ok _, Error e | Error e, Ok _ ->
              Alcotest.failf "unexpected loser error %s" (error_str e));
          let* () = check_status "accepted once" conn id "accepted" in
          let* () = check_events "two events total" conn ~connection:id 2 in
          check_notifs "one set per committed transition" conn ~connection:id
            (List.sort compare
               [
                 ((btop, "community_connection_requested"), (Some actor, b));
                 ((atop, "community_connection_accepted"), (Some btop, a));
               ])))

(* === durable shapes === *)

(* Every shape case writes through one of these, so the SQL of each case is
   the whole statement being judged and nothing else. Parameters are always
   $1 user, $2 community, $3 connection, $4 project, $5 relation. *)
let shape3 sql = (Caqti_type.(t3 int int int64) ->. Caqti_type.unit) sql

let shape5 sql =
  (Caqti_type.(t2 (t3 int int int64) (t2 int64 int64)) ->. Caqti_type.unit) sql

let q_valid_connection_shape =
  shape3
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     community_id, connection_id) VALUES ($1, 'community_connection_accepted', \
     NULL, $2, $3)"

let q_valid_legacy_shape =
  shape3
    "INSERT INTO notifications (user_id, notif_type, message) SELECT $1, \
     'comment_reply', 'someone replied to your post.' WHERE $2 > 0 AND $3 > 0"

let q_valid_project_home_shape =
  shape5
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     project_id, community_id, relation_id) SELECT $1, \
     'project_home_requested', NULL, $4, $2, $5 WHERE $3 > 0"

let q_connection_with_message =
  shape3
    "INSERT INTO notifications (user_id, notif_type, community_id, \
     connection_id, message) VALUES ($1, 'community_connection_accepted', $2, \
     $3, 'prose')"

let q_connection_with_post =
  shape3
    "INSERT INTO notifications (user_id, notif_type, community_id, \
     connection_id, post_id) VALUES ($1, 'community_connection_accepted', $2, \
     $3, 1)"

let q_connection_without_connection =
  shape3
    "INSERT INTO notifications (user_id, notif_type, community_id) SELECT $1, \
     'community_connection_accepted', $2 WHERE $3 > 0"

let q_connection_without_community =
  shape3
    "INSERT INTO notifications (user_id, notif_type, connection_id) SELECT $1, \
     'community_connection_accepted', $3 WHERE $2 > 0"

let q_connection_with_project =
  shape5
    "INSERT INTO notifications (user_id, notif_type, community_id, \
     connection_id, project_id) SELECT $1, 'community_connection_accepted', \
     $2, $3, $4 WHERE $5 > 0"

let q_connection_with_relation =
  shape5
    "INSERT INTO notifications (user_id, notif_type, community_id, \
     connection_id, relation_id) SELECT $1, 'community_connection_accepted', \
     $2, $3, $5 WHERE $4 > 0"

let q_project_home_with_connection =
  shape5
    "INSERT INTO notifications (user_id, notif_type, project_id, community_id, \
     relation_id, connection_id) VALUES ($1, 'project_home_requested', $4, $2, \
     $5, $3)"

let q_legacy_with_connection =
  shape3
    "INSERT INTO notifications (user_id, notif_type, message, community_id, \
     connection_id) VALUES ($1, 'comment_reply', 'prose', $2, $3)"

let q_unknown_kind =
  shape3
    "INSERT INTO notifications (user_id, notif_type, community_id, \
     connection_id) VALUES ($1, 'community_connection_archived', $2, $3)"

let shape_case =
  db_case
    "schema: the three notification shapes are separately valid and every \
     mixture is refused" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* owner = insert_user conn "ccnt_sh_owner" in
      let* recipient = insert_user conn "ccnt_sh_recipient" in
      let* a = insert_community conn "ccnt-sh-a" in
      let* b = insert_community conn "ccnt-sh-b" in
      let* home = insert_community conn "ccnt-sh-home" in
      let* connection =
        request_ok "connection" conn ~actor:owner ~requester:a ~recipient:b ()
      in
      (* A genuine project-home relation, so the project-home branch is
         judged against real subject rows rather than invented ids. *)
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:964000001L ~slug:"ccnt-sh-proj"
      in
      let* relation =
        Home_audit_fixture.request_ok "home request" conn ~user:owner
          ~slug:"ccnt-sh-proj" ~community:home
      in
      let five = ((recipient, a, connection), (project, relation)) in
      (* Valid: all three shapes, each on its own terms. *)
      let* () =
        exec conn "connection shape" q_valid_connection_shape
          (recipient, a, connection)
      in
      let* () =
        exec conn "legacy shape" q_valid_legacy_shape (recipient, a, connection)
      in
      let* () =
        exec conn "project-home shape" q_valid_project_home_shape
          ((recipient, home, connection), (project, relation))
      in
      (* Invalid: every mixture of the three. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, q) ->
            let* r = C.exec q (recipient, a, connection) in
            reject label r)
          [
            ("connection kind carrying prose", q_connection_with_message);
            ("connection kind carrying a post link", q_connection_with_post);
            ( "connection kind without its connection",
              q_connection_without_connection );
            ( "connection kind without its community",
              q_connection_without_community );
            ("legacy kind carrying a connection", q_legacy_with_connection);
            ("an unknown kind", q_unknown_kind);
          ]
      in
      Lwt_list.iter_s
        (fun (label, q) ->
          let* r = C.exec q five in
          reject label r)
        [
          ("connection kind carrying a project", q_connection_with_project);
          ( "connection kind carrying a home relation",
            q_connection_with_relation );
          ( "project-home kind carrying a connection",
            q_project_home_with_connection );
        ])

(* === read model and rendering === *)

(* One sql_pool for the whole suite: see the sibling handler suites — the
   pool lives in the middleware closure, so a fresh one per request would
   leak a Postgres connection each time. *)
let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

let notifications_page_for ~url ~label user_id =
  let pipeline =
    sql_pool url @@ Dream.memory_sessions
    @@ fun req ->
    let* () = Dream.set_session_field req "user_id" (string_of_int user_id) in
    Earde.Account_handlers.notifications_handler req
  in
  let* response =
    pipeline (Dream.request ~method_:`GET ~target:"/notifications" "")
  in
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  let* body = Dream.body response in
  Lwt.return body

let notifications_of conn user_id =
  let* r =
    Earde.Notification_store.get_notifications conn ~session_admin:false user_id
  in
  match r with
  | Ok rows -> Lwt.return rows
  | Error e -> Alcotest.failf "get_notifications: %s" e

let counterpart_case =
  db_case
    "read model: the counterpart is derived from the stored context, from \
     either direction" (fun ~url:_ conn ->
      let* atop = insert_user conn "ccnt_cp_atop" in
      let* btop = insert_user conn "ccnt_cp_btop" in
      let* a = insert_community ~name:"Ccnt Alpha" conn "ccnt-cp-a" in
      let* b = insert_community ~name:"Ccnt Beta" conn "ccnt-cp-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* id =
        request_ok "request" conn ~actor:atop ~requester:a ~recipient:b ()
      in
      let* () =
        review_ok "accept" conn ~reviewer:btop ~connection:id ~recipient:b
          Store.Accept
      in
      (* b's top mod was notified about the request; their context is b, so
         the counterpart must be a — the requesting side. *)
      let* brows = notifications_of conn btop in
      (match brows with
      | [ n ] ->
          Alcotest.(check string)
            "b: kind" "community_connection_requested"
            n.Earde.Notification_store.notif_type;
          Alcotest.(check (option string))
            "b: context" (Some "ccnt-cp-b") n.community_slug;
          Alcotest.(check (option string))
            "b: counterpart" (Some "ccnt-cp-a")
            n.Earde.Notification_store.counterpart_slug;
          Alcotest.(check (option string))
            "b: counterpart name" (Some "Ccnt Alpha")
            n.Earde.Notification_store.counterpart_name
      | rows -> Alcotest.failf "b: %d rows" (List.length rows));
      (* a's top mod was notified about the acceptance; their context is a,
         so the same durable connection yields b as the counterpart. *)
      let* arows = notifications_of conn atop in
      (match arows with
      | [ n ] ->
          Alcotest.(check string)
            "a: kind" "community_connection_accepted"
            n.Earde.Notification_store.notif_type;
          Alcotest.(check (option string))
            "a: context" (Some "ccnt-cp-a") n.community_slug;
          Alcotest.(check (option string))
            "a: counterpart" (Some "ccnt-cp-b")
            n.Earde.Notification_store.counterpart_slug;
          Alcotest.(check (option string))
            "a: counterpart name" (Some "Ccnt Beta")
            n.Earde.Notification_store.counterpart_name
      | rows -> Alcotest.failf "a: %d rows" (List.length rows));
      Lwt.return_unit)

let render_case =
  db_case
    "ui: each kind renders its stable copy and links to the recipient's own \
     management context" (fun ~url conn ->
      let* atop = insert_user conn "ccnt_ui_atop" in
      let* btop = insert_user conn "ccnt_ui_btop" in
      let* a = insert_community ~name:"Ccnt Requesting" conn "ccnt-ui-a" in
      let* b = insert_community ~name:"Ccnt Reviewing" conn "ccnt-ui-b" in
      let* c = insert_community ~name:"Ccnt Rejecting" conn "ccnt-ui-c" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let* () = add_top_mod conn ~user:btop ~community:c in
      (* a → b: requested, accepted, then removed by b. *)
      let* accepted =
        request_ok "request b" conn ~actor:atop ~requester:a ~recipient:b ()
      in
      let* () =
        review_ok "accept" conn ~reviewer:btop ~connection:accepted ~recipient:b
          Store.Accept
      in
      let* () =
        remove_ok "remove" conn ~actor:btop ~connection:accepted ~acting:b
      in
      (* a → c: requested, then rejected. *)
      let* rejected =
        request_ok "request c" conn ~actor:atop ~requester:a ~recipient:c ()
      in
      let* () =
        review_ok "reject" conn ~reviewer:btop ~connection:rejected ~recipient:c
          Store.Reject
      in
      (* The requesting side sees accepted, rejected, and removed copy, each
         pointing at its OWN community's connections page. *)
      let* body = notifications_page_for ~url ~label:"requester" atop in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("requester sees: " ^ needle)
            true (contains body needle))
        [
          "Ccnt Reviewing accepted your connection request.";
          "Ccnt Rejecting rejected your connection request.";
          "Ccnt Reviewing removed the community connection.";
          "href='/c/ccnt-ui-a/settings/connections'";
        ];
      (* Never the other side's management context, and never a username. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("requester never sees: " ^ needle)
            false (contains body needle))
        [
          "/c/ccnt-ui-b/settings/connections";
          "/c/ccnt-ui-c/settings/connections";
          "ccnt_ui_btop";
          "ccnt_ui_atop";
        ];
      (* The reviewing side sees the request copy, in its own context. *)
      let* body = notifications_page_for ~url ~label:"reviewer" btop in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("reviewer sees: " ^ needle)
            true (contains body needle))
        [
          "Ccnt Requesting wants to connect with your community.";
          "href='/c/ccnt-ui-b/settings/connections'";
          "href='/c/ccnt-ui-c/settings/connections'";
        ];
      Alcotest.(check bool)
        "reviewer never sees the requester's context" false
        (contains body "/c/ccnt-ui-a/settings/connections");
      Lwt.return_unit)

let privacy_case =
  db_case
    "the request note and every username stay out of notification storage and \
     rendered output" (fun ~url conn ->
      let* atop = insert_user conn "ccnt_pv_atop" in
      let* btop = insert_user conn "ccnt_pv_btop" in
      let* a = insert_community ~name:"Ccnt Privacy A" conn "ccnt-pv-a" in
      let* b = insert_community ~name:"Ccnt Privacy B" conn "ccnt-pv-b" in
      let* () = add_top_mod conn ~user:atop ~community:a in
      let* () = add_top_mod conn ~user:btop ~community:b in
      let secret = "SECRETNOTEMARKER please connect with us" in
      let* id =
        request_ok "request" conn ~actor:atop ~note:secret ~requester:a
          ~recipient:b ()
      in
      let* () =
        review_ok "accept" conn ~reviewer:btop ~connection:id ~recipient:b
          Store.Accept
      in
      (* Storage: every column of every notification row of this connection,
         rendered as text, contains neither the note nor either username. *)
      let* blob = find conn "blob" q_notif_blob id in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("stored blob free of: " ^ needle)
            false (contains blob needle))
        [ "SECRETNOTEMARKER"; "ccnt_pv_atop"; "ccnt_pv_btop" ];
      (* Rendered output, on both sides. *)
      let* body_a = notifications_page_for ~url ~label:"a" atop in
      let* body_b = notifications_page_for ~url ~label:"b" btop in
      List.iter
        (fun (label, body) ->
          List.iter
            (fun needle ->
              Alcotest.(check bool)
                (label ^ " free of: " ^ needle)
                false (contains body needle))
            [ "SECRETNOTEMARKER"; "ccnt_pv_atop"; "ccnt_pv_btop" ])
        [ ("requester page", body_a); ("recipient page", body_b) ];
      Lwt.return_unit)

let recipient_suite =
  [
    request_recipients_case;
    review_recipients_case;
    removal_recipients_case;
    actor_excluded_case;
    dedup_case;
    zero_recipients_case;
  ]

let transaction_suite =
  [
    stale_transition_case;
    notification_failure_rolls_back_case;
    concurrent_review_case;
  ]

let schema_suite = [ shape_case ]
let ui_suite = [ counterpart_case; render_case; privacy_case ]

let suites =
  [
    ("community_connection_notifications", recipient_suite);
    ("community_connection_notification_txn", transaction_suite);
    ("community_connection_notification_schema", schema_suite);
    ("community_connection_notification_ui", ui_suite);
  ]
