(* Shared-thread placement fixtures: posts, sections, comments and the
   store calls, over a gated connection. *)

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
let status_str = P.string_of_status

(* Legacy (non-network) communities, as in the schema suite: eligibility
   drift cases flip visibility and onboarding freely, which the network
   lifecycle CHECK would otherwise refuse. *)
let insert_community conn slug =
  Community_fixture.insert_community ~network:false conn slug

let error_str : Store.error -> string = function
  | Store.Invalid_user_id -> "Invalid_user_id"
  | Store.Invalid_placement_id -> "Invalid_placement_id"
  | Store.Invalid_post_id -> "Invalid_post_id"
  | Store.Invalid_community_id -> "Invalid_community_id"
  | Store.Invalid_request_note -> "Invalid_request_note"
  | Store.Same_community -> "Same_community"
  | Store.Community_unavailable -> "Community_unavailable"
  | Store.Post_unavailable -> "Post_unavailable"
  | Store.Post_tombstoned -> "Post_tombstoned"
  | Store.No_accepted_connection -> "No_accepted_connection"
  | Store.Origin_ineligible -> "Origin_ineligible"
  | Store.Destination_ineligible -> "Destination_ineligible"
  | Store.Active_placement_exists -> "Active_placement_exists"
  | Store.Invalid_destination_section -> "Invalid_destination_section"
  | Store.Review_unavailable -> "Review_unavailable"
  | Store.Withdrawal_unavailable -> "Withdrawal_unavailable"
  | Store.Removal_unavailable -> "Removal_unavailable"
  | Store.Inconsistent_data -> "Inconsistent_data"
  | Store.Storage_error -> "Storage_error"

(* Failure-injection DDL is dropped first so a crashed case can never
   leave a trigger behind. Then the dependency order: notifications,
   the placement audit trail (which protects placements, posts, and
   communities), the placements, the connection fixtures' audit trail
   and rows, then posts before the users and communities they
   reference. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS stp_fail_audit ON \
       shared_thread_placement_audit_events";
      "DROP TRIGGER IF EXISTS stp_fail_business ON shared_thread_placements";
      "DROP TRIGGER IF EXISTS stp_fail_notif ON notifications";
      "DROP FUNCTION IF EXISTS stp_fail_fn()";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'stp_%')";
      "DELETE FROM notifications WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stp-%')";
      "DELETE FROM shared_thread_placement_audit_events WHERE \
       origin_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'stp-%') OR destination_community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'stp-%')";
      "DELETE FROM shared_thread_placements WHERE origin_community_id IN \
       (SELECT id FROM communities WHERE slug LIKE 'stp-%') OR \
       destination_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'stp-%')";
      "DELETE FROM community_connection_audit_events WHERE \
       requester_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'stp-%') OR recipient_community_id IN (SELECT id FROM communities WHERE \
       slug LIKE 'stp-%')";
      "DELETE FROM community_connections WHERE requester_community_id IN \
       (SELECT id FROM communities WHERE slug LIKE 'stp-%') OR \
       recipient_community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'stp-%')";
      "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'stp-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stp-%')";
      "DELETE FROM communities WHERE slug LIKE 'stp-%'";
      "DELETE FROM users WHERE username LIKE 'stp_%'";
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

let q_insert_post =
  (Caqti_type.(t3 int int (option int)) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id, section_id) \
     VALUES ('stp thread', 'stp body', $1, $2, $3) RETURNING id"

let insert_post ?section conn ~community ~author =
  find conn "post fixture" q_insert_post (community, author, section)

let q_tombstone =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE posts SET content = $2 WHERE id = $1"

let tombstone conn post label = exec conn "tombstone" q_tombstone (post, label)

let q_set_sections =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
    "UPDATE communities SET sections_enabled = $2 WHERE id = $1"

let q_insert_section =
  (Caqti_type.(t2 int string) ->! Caqti_type.int)
    "INSERT INTO community_sections (community_id, name, slug) VALUES ($1, $2, \
     $2) RETURNING id"

let q_delete_section =
  (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM community_sections WHERE id = $1"

let q_make_eligible =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET visibility = 'public', onboarding_state = \
     'published', indexable = TRUE, discoverable = TRUE WHERE id = $1"

let q_insert_comment =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id) VALUES ('kept', $1, $2) \
     RETURNING id"

(* Accepted community connection through the real connections store, so
   the standing this feature rides on is exactly the durable one. *)
let connect conn ~actor a b =
  let value =
    match
      Cc.create_pending ~requester_community_id:a ~recipient_community_id:b
        ~request_note:None
    with
    | Ok v -> v
    | Error _ -> Alcotest.fail "fixture: connection value refused"
  in
  let* requested = Ccs.request conn ~actor_user_id:actor ~connection:value in
  let connection =
    match requested with
    | Ok created -> Ccs.created_connection_id created
    | Error e ->
        Alcotest.failf "fixture: connection request %s"
          (Community_fixture.error_str e)
  in
  let* reviewed =
    Ccs.review conn ~reviewer_user_id:actor ~connection_id:connection
      ~recipient_community_id:b ~decision:Ccs.Accept
  in
  match reviewed with
  | Ok _ -> Lwt.return connection
  | Error e ->
      Alcotest.failf "fixture: connection accept %s"
        (Community_fixture.error_str e)

let disconnect conn ~actor ~connection ~acting =
  let* removed =
    Ccs.remove conn ~actor_user_id:actor ~connection_id:connection
      ~acting_community_id:acting
  in
  match removed with
  | Ok _ -> Lwt.return_unit
  | Error e ->
      Alcotest.failf "fixture: disconnect %s" (Community_fixture.error_str e)

(* One actor, a connected origin/destination pair with a flat (sections
   disabled) destination, and one canonical post — the shape every case
   starts from. *)
let fixture conn tag =
  let* actor = insert_user conn ("stp_" ^ tag) in
  let* o = insert_community conn ("stp-" ^ tag ^ "-o") in
  let* d = insert_community conn ("stp-" ^ tag ^ "-d") in
  let* () = exec conn "flat destination" q_set_sections (d, false) in
  let* connection = connect conn ~actor o d in
  let* post = insert_post conn ~community:o ~author:actor in
  Lwt.return (actor, o, d, post, connection)

(* Everything ever recorded about one post, however the placement rows
   themselves ended up — the count a rolled-back mutation must not
   move. *)
let q_events_for_post =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM shared_thread_placement_audit_events WHERE post_id = \
     $1"

let q_notifs_for_post =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications n WHERE n.shared_thread_placement_id \
     IN (SELECT id FROM shared_thread_placements WHERE post_id = $1)"

let q_post_row =
  (Caqti_type.int ->! Caqti_type.(t2 string (option string)))
    "SELECT title, content FROM posts WHERE id = $1"

let q_comment_count =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM comments WHERE post_id = $1"

let request conn ~actor ?note ~post ~destination () =
  Store.request conn ~actor_user_id:actor ~post_id:post
    ~destination_community_id:destination ~request_note:note

let request_ok label conn ~actor ?note ~post ~destination () =
  let* r = request conn ~actor ?note ~post ~destination () in
  match r with
  | Ok created -> Lwt.return (Store.created_placement_id created)
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let review conn ~reviewer ~placement ~destination decision =
  Store.review conn ~reviewer_user_id:reviewer ~placement_id:placement
    ~destination_community_id:destination ~decision

let review_ok label conn ~reviewer ~placement ~destination decision expected =
  let* r = review conn ~reviewer ~placement ~destination decision in
  match r with
  | Ok reviewed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (status_str expected)
        (status_str (Store.reviewed_status reviewed));
      Lwt.return reviewed
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let withdraw conn ~actor ~placement ~origin =
  Store.withdraw conn ~actor_user_id:actor ~placement_id:placement
    ~origin_community_id:origin

let withdraw_ok label conn ~actor ~placement ~origin =
  let* r = withdraw conn ~actor ~placement ~origin in
  match r with
  | Ok withdrawn -> Lwt.return withdrawn
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let remove conn ~actor ~placement ~acting =
  Store.remove conn ~actor_user_id:actor ~placement_id:placement
    ~acting_community_id:acting

let remove_ok label conn ~actor ~placement ~acting =
  let* r = remove conn ~actor ~placement ~acting in
  match r with
  | Ok removed -> Lwt.return removed
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION stp_fail_fn() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN \
     RAISE EXCEPTION ''stp fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS stp_fail_fn()"

let q_poison_notif =
  ddl
    "CREATE TRIGGER stp_fail_notif BEFORE INSERT ON notifications FOR EACH ROW \
     EXECUTE FUNCTION stp_fail_fn()"

let q_unpoison_notif =
  ddl "DROP TRIGGER IF EXISTS stp_fail_notif ON notifications"

let with_poison conn ~install ~remove f =
  let* () = exec conn "create fail fn" q_create_fail_fn () in
  Lwt.finalize
    (fun () ->
      let* () = exec conn "install poison trigger" install () in
      Lwt.finalize f (fun () -> exec conn "drop poison trigger" remove ()))
    (fun () -> exec conn "drop fail fn" q_drop_fail_fn ())
