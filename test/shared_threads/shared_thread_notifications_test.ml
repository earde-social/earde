(* The structured notification recipient policy: exact top_mod resolution
   per side, the requester/author pair on review outcomes, the
   deduplicated two-sided union on removal, actor exclusion, per-side
   community context, and the absence of prose. Database-gated on
   EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module P = Earde.Shared_thread_placements
module Store = Earde.Shared_thread_placement_store

let find = Db_fixture.find
let collect = Db_fixture.collect
let exec = Db_fixture.exec
let insert_user = Db_fixture.insert_user
let contains haystack needle = Html_assert.occurs haystack ~needle
let db_case = Shared_thread_fixture.db_case
let fixture = Shared_thread_fixture.fixture
let insert_post = Shared_thread_fixture.insert_post
let request_ok = Shared_thread_fixture.request_ok
let review_ok = Shared_thread_fixture.review_ok
let withdraw_ok = Shared_thread_fixture.withdraw_ok
let remove_ok = Shared_thread_fixture.remove_ok

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let add_top_mod conn ~user ~community = add_role conn ~user ~community "top_mod"

(* One tuple per notification of one placement, ordered by recipient so
   expectations do not depend on insertion order. *)
let q_notifs =
  (Caqti_type.int64 ->* Caqti_type.(t2 (t2 int string) (t2 (option int) int)))
    "SELECT user_id, notif_type, actor_user_id, community_id FROM \
     notifications WHERE shared_thread_placement_id = $1 ORDER BY user_id, \
     notif_type"

let q_notif_count =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE shared_thread_placement_id = $1"

(* Every stored byte of one placement's notifications, for the boolean
   absence sweep over notes and usernames. *)
let q_notif_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(n::text, '|'), '<none>') FROM notifications n \
     WHERE shared_thread_placement_id = $1"

(* Creation must never mark anything read. *)
let q_all_unread =
  (Caqti_type.int64 ->! Caqti_type.bool)
    "SELECT COALESCE(BOOL_AND(NOT is_read), TRUE) FROM notifications WHERE \
     shared_thread_placement_id = $1"

let notif_t = Alcotest.(pair (pair int string) (pair (option int) int))

let sorted expected =
  List.sort (fun ((a, ka), _) ((b, kb), _) -> compare (a, ka) (b, kb)) expected

let check_notifs label conn ~placement expected =
  let* rows = collect conn (label ^ ": notifications") q_notifs placement in
  Alcotest.(check (list notif_t))
    (label ^ ": exact notifications")
    (sorted expected) rows;
  let* unread = find conn (label ^ ": unread") q_all_unread placement in
  Alcotest.(check bool) (label ^ ": all rows unread") true unread;
  Lwt.return_unit

let requested_recipients_case =
  db_case "requested: destination exact top_mods only, in their context"
    (fun conn ->
      let* actor, o, d, post, _ = fixture conn "nreq" in
      let* tm1 = insert_user conn "stp_nreq_tm1" in
      let* tm2 = insert_user conn "stp_nreq_tm2" in
      let* ordinary = insert_user conn "stp_nreq_mod" in
      let* legacy = insert_user conn "stp_nreq_legacy" in
      let* origin_tm = insert_user conn "stp_nreq_origin_tm" in
      let* () = add_top_mod conn ~user:tm1 ~community:d in
      let* () = add_top_mod conn ~user:tm2 ~community:d in
      let* () = add_role conn ~user:ordinary ~community:d "mod" in
      let* () = add_role conn ~user:legacy ~community:d "legacy_mod" in
      let* () = add_top_mod conn ~user:origin_tm ~community:o in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      check_notifs "requested" conn ~placement:id
        [
          ((tm1, "shared_thread_requested"), (Some actor, d));
          ((tm2, "shared_thread_requested"), (Some actor, d));
        ])

let requested_actor_excluded_case =
  db_case "requested: the acting top_mod is never notified" (fun conn ->
      let* actor, _, d, post, _ = fixture conn "nself" in
      let* other = insert_user conn "stp_nself_other" in
      let* () = add_top_mod conn ~user:actor ~community:d in
      let* () = add_top_mod conn ~user:other ~community:d in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      check_notifs "actor excluded" conn ~placement:id
        [ ((other, "shared_thread_requested"), (Some actor, d)) ])

let zero_recipients_case =
  db_case "requested: zero recipients never fails the transition" (fun conn ->
      let* actor, _, d, post, _ = fixture conn "nzero" in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* n = find conn "count" q_notif_count id in
      Alcotest.(check int) "no notifications, request committed" 0 n;
      Lwt.return_unit)

let review_recipients_case =
  db_case
    "review: requester and author both hear the outcome, in origin context"
    (fun conn ->
      let* actor, o, d, _, _ = fixture conn "nrev" in
      let* author = insert_user conn "stp_nrev_author" in
      let* post = insert_post conn ~community:o ~author in
      let* reviewer = insert_user conn "stp_nrev_reviewer" in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        review_ok "accept" conn ~reviewer ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      check_notifs "accepted" conn ~placement:id
        [
          ((actor, "shared_thread_accepted"), (Some reviewer, o));
          ((author, "shared_thread_accepted"), (Some reviewer, o));
        ])

let review_dedup_case =
  db_case "review: a requesting author collapses to one notification"
    (fun conn ->
      let* actor, o, d, post, _ = fixture conn "ndedup2" in
      (* The fixture's actor authored the post and requests the share. *)
      let* reviewer = insert_user conn "stp_ndedup2_reviewer" in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        review_ok "reject" conn ~reviewer ~placement:id ~destination:d
          Store.Reject P.Rejected
      in
      check_notifs "one rejected notification" conn ~placement:id
        [ ((actor, "shared_thread_rejected"), (Some reviewer, o)) ])

let reviewer_is_requester_case =
  db_case "review: a reviewing requester is excluded, the author remains"
    (fun conn ->
      let* actor, o, d, _, _ = fixture conn "nrevself" in
      let* author = insert_user conn "stp_nrevself_author" in
      let* post = insert_post conn ~community:o ~author in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        review_ok "accept by the requester" conn ~reviewer:actor ~placement:id
          ~destination:d (Store.Accept None) P.Accepted
      in
      check_notifs "self-review" conn ~placement:id
        [ ((author, "shared_thread_accepted"), (Some actor, o)) ])

let withdrawn_recipients_case =
  db_case "withdrawn: destination top_mods hear it, minus the actor"
    (fun conn ->
      let* actor, o, d, post, _ = fixture conn "nwd" in
      let* tm = insert_user conn "stp_nwd_tm" in
      let* () = add_top_mod conn ~user:tm ~community:d in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ = withdraw_ok "withdraw" conn ~actor ~placement:id ~origin:o in
      check_notifs "withdrawn" conn ~placement:id
        [
          ((tm, "shared_thread_requested"), (Some actor, d));
          ((tm, "shared_thread_withdrawn"), (Some actor, d));
        ])

let removed_union_case =
  db_case
    "removed: both sides' top_mods, requester, and author — deduplicated, \
     actor excluded, per-side context" (fun conn ->
      let* actor, o, d, _, _ = fixture conn "nrm" in
      let* author = insert_user conn "stp_nrm_author" in
      let* post = insert_post conn ~community:o ~author in
      let* origin_tm = insert_user conn "stp_nrm_otm" in
      let* dest_tm = insert_user conn "stp_nrm_dtm" in
      let* both_tm = insert_user conn "stp_nrm_btm" in
      let* remover = insert_user conn "stp_nrm_remover" in
      let* () = add_top_mod conn ~user:origin_tm ~community:o in
      let* () = add_top_mod conn ~user:dest_tm ~community:d in
      let* () = add_top_mod conn ~user:both_tm ~community:o in
      let* () = add_top_mod conn ~user:both_tm ~community:d in
      let* id = request_ok "request" conn ~actor ~post ~destination:d () in
      let* _ =
        review_ok "accept" conn ~reviewer:remover ~placement:id ~destination:d
          (Store.Accept None) P.Accepted
      in
      let* _ = remove_ok "remove" conn ~actor:remover ~placement:id ~acting:o in
      let removed = "shared_thread_removed" in
      let* rows = collect conn "rows" q_notifs id in
      let removed_rows =
        List.filter (fun ((_, kind), _) -> kind = removed) rows
      in
      Alcotest.(check (list notif_t))
        "exact removal recipients"
        (List.sort compare
           [
             ((origin_tm, removed), (Some remover, o))
             (* A top moderator of both sides is notified once, in
                origin context. *);
             ((both_tm, removed), (Some remover, o));
             ((actor, removed), (Some remover, o));
             ((author, removed), (Some remover, o));
             ((dest_tm, removed), (Some remover, d));
           ])
        removed_rows;
      Lwt.return_unit)

let privacy_case =
  db_case "privacy: no note, name, or prose is stored on any row" (fun conn ->
      let* actor, _, d, post, _ = fixture conn "priv" in
      let* tm = insert_user conn "stp_priv_tm" in
      let* () = add_top_mod conn ~user:tm ~community:d in
      let* id =
        request_ok "request" conn ~actor ~note:"SECRETNOTE do not surface" ~post
          ~destination:d ()
      in
      let* blob = find conn "blob" q_notif_blob id in
      List.iter
        (fun needle ->
          if contains blob needle then
            Alcotest.failf "notifications carry %S" needle)
        [ "SECRETNOTE"; "stp_priv"; "stp thread"; "stp body" ];
      Lwt.return_unit)

let suite =
  [
    requested_recipients_case;
    requested_actor_excluded_case;
    zero_recipients_case;
    review_recipients_case;
    review_dedup_case;
    reviewer_is_requester_case;
    withdrawn_recipients_case;
    removed_union_case;
    privacy_case;
  ]

let suites = [ ("shared_thread_notifications", suite) ]
