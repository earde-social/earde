module Ob = Earde.Project_onboarding
module Phr = Earde.Project_home_relation

(* === Project-home lifecycle notifications
   (Project_home_notifications + the extended notifications table) ===
   The durable in-app notifications of the four counterparty-facing
   home-lifecycle transitions: the extended schema itself (the closed
   kind vocabulary via the notif_type CHECK, the two row shapes via the
   shape CHECK, required subject FKs with the
   user-facing CASCADE convention, SET NULL actor retention, the
   per-recipient/kind/relation dedup index, the down/up round-trip, and
   the absence of any note or credential column), the exact recipient
   set of each real store transition, whole-mutation rollback when the
   notification insertion itself fails, no notification on any refused
   or failed path, single delivery under concurrent attempts, the real
   notification-center handlers (labels, structural links, unread and
   mark-read behavior, recipient isolation, escaping, deleted actors,
   protected destinations), and a privacy sweep over rows, HTML, and
   headers. Fixtures use the reserved external-installation-id range
   960000001..960000999 (hence namespace ids 960100001..960100999),
   phnt_% usernames, and phnt-% community slugs so no suite shares
   fixtures. Notifications come only through the real stores except in
   the schema suite, whose raw rows probe the table's own constraints.
   Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Rms = Earde.Project_home_removal_store

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let make_project = Home_request_fixture.make_project
let insert_community = Community_fixture.insert_community
let make_draft = Network_community_fixture.make_draft
let contains haystack needle = Html_assert.occurs haystack ~needle
let status_of = Http_fixture.status_of

(* Store call and audit observation helpers shared with the audit
   suite — the notification suite always asserts the audit trail
   beside the notification set, because the two must commit or roll
   back together. *)
let request_ok = Home_audit_fixture.request_ok
let review_ok = Home_audit_fixture.review_ok
let review_expect = Home_audit_fixture.review_expect
let remove_ok = Home_audit_fixture.remove_ok
let remove_expect = Home_audit_fixture.remove_expect
let check_status = Home_audit_fixture.check_status
let check_events = Home_audit_fixture.check_events
let check_event_count = Home_audit_fixture.check_event_count
let sole_relation = Home_audit_fixture.sole_relation
let q_delete_user = Home_audit_fixture.q_delete_user

(* Failure-injection DDL is dropped first so a crashed case can never
   leave a trigger behind; notifications and audit events must go
   before projects and communities (audit RESTRICT-protects both),
   then the shared dependency order of the sibling suites. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP TRIGGER IF EXISTS phnt_fail_notif ON notifications";
      "DROP TRIGGER IF EXISTS phnt_fail_audit ON project_home_audit_events";
      "DROP FUNCTION IF EXISTS phnt_fail_fn()";
      "DELETE FROM notifications WHERE project_id IN (SELECT id FROM \
       open_source_projects WHERE forge_namespace_id BETWEEN 960100001 AND \
       960100999)";
      "DELETE FROM notifications WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'phnt-%')";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'phnt_%')";
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 960100001 \
       AND 960100999)";
      "DELETE FROM project_home_audit_events WHERE community_id IN (SELECT id \
       FROM communities WHERE slug LIKE 'phnt-%')";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       960100001 AND 960100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 960000001 AND 960000999)";
      "DELETE FROM communities WHERE slug LIKE 'phnt-%'";
      "DELETE FROM users WHERE username LIKE 'phnt_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       960000001 AND 960000999";
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

(* === notification observation === *)

(* One tuple per notification, in append (id) order. The project is
   the query key, so each row reduces to recipient, kind, actor,
   community, relation. *)
let q_notifs_for_project =
  (Caqti_type.int64
  ->* Caqti_type.(t2 (t2 int string) (t3 (option int) int int64)))
    "SELECT user_id, notif_type, actor_user_id, community_id, relation_id FROM \
     notifications WHERE project_id = $1 ORDER BY id"

let q_count_notifs_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE project_id = $1"

let q_count_kind_for_project =
  (Caqti_type.(t2 int64 string) ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE project_id = $1 AND notif_type = \
     $2"

(* Creation must never mark anything read. *)
let q_all_unread =
  (Caqti_type.int64 ->! Caqti_type.bool)
    "SELECT COALESCE(BOOL_AND(NOT is_read), TRUE) FROM notifications WHERE \
     project_id = $1"

(* Every stored byte of a project's notifications, for the boolean
   credential-absence sweep. *)
let q_notif_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(n::text, '|'), '<none>') FROM notifications n \
     WHERE project_id = $1"

let notif_t = Alcotest.(pair (pair int string) (triple (option int) int int64))

let check_notifs label conn ~project expected =
  let* rows =
    collect conn (label ^ ": notifications") q_notifs_for_project project
  in
  Alcotest.(check (list notif_t))
    (label ^ ": exact notifications")
    expected rows;
  let* unread = find conn (label ^ ": unread") q_all_unread project in
  Alcotest.(check bool) (label ^ ": all rows unread") true unread;
  Lwt.return_unit

let check_notif_count label conn ~project expected =
  let* n = find conn (label ^ ": count") q_count_notifs_for_project project in
  Alcotest.(check int) (label ^ ": notification count") expected n;
  Lwt.return_unit

let check_kind_count label conn ~project ~kind expected =
  let* n =
    find conn (label ^ ": kind count") q_count_kind_for_project (project, kind)
  in
  Alcotest.(check int) (label ^ ": " ^ kind ^ " count") expected n;
  Lwt.return_unit

(* === schema fixtures (raw rows: the table's own constraints are the
   subject here, so notifications do not go through the stores) === *)

let q_insert_project_raw =
  (Caqti_type.(t2 string int64) ->! Caqti_type.int64)
    "INSERT INTO open_source_projects (name, slug, kind, forge_namespace_id, \
     forge_namespace_login, forge_namespace_type) VALUES ('Phnt fixture', $1, \
     'project', $2, 'phnt-raw-owner', 'user') RETURNING id"

let q_insert_relation_raw =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at) VALUES ($1, $2, 'home', 'accepted', NOW()) RETURNING \
     id"

(* A second, historical relation for the same project: outside the
   active-home index predicate, so it can coexist with the accepted
   one. *)
let q_insert_relation_removed_raw =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at, removed_at) VALUES ($1, $2, 'home', 'removed', \
     NOW(), NOW()) RETURNING id"

let q_insert_post_raw =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id) VALUES ('phnt \
     post', 'phnt body', $1, $2) RETURNING id"

(* The structured project-home row shape. *)
let q_insert_notif_raw =
  (Caqti_type.(t2 (t2 int string) (t2 (option int) (t3 int64 int int64)))
  ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     project_id, community_id, relation_id) VALUES ($1, $2, $3, $4, $5, $6) \
     RETURNING id"

(* NULL-capable subject columns, for the required-reference probes. *)
let q_insert_notif_nullable =
  (Caqti_type.(
     t2 (t2 int string)
       (t2 (option int) (t3 (option int64) (option int) (option int64))))
  ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, actor_user_id, \
     project_id, community_id, relation_id) VALUES ($1, $2, $3, $4, $5, $6) \
     RETURNING id"

(* Forbidden mixed shapes. *)
let q_insert_home_with_message =
  (Caqti_type.(t2 (t2 int string) (t3 int64 int int64)) ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, message, project_id, \
     community_id, relation_id) VALUES ($1, $2, 'phnt prose', $3, $4, $5) \
     RETURNING id"

let q_insert_home_with_post =
  (Caqti_type.(t2 (t2 int string) (t2 int (t3 int64 int int64)))
  ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, post_id, project_id, \
     community_id, relation_id) VALUES ($1, $2, $3, $4, $5, $6) RETURNING id"

(* Legacy rows: prose message, optional post, no subject or actor. *)
let q_insert_legacy_raw =
  (Caqti_type.(t3 int string (option int)) ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, message, post_id) VALUES \
     ($1, $2, 'phnt legacy message', $3) RETURNING id"

let q_insert_legacy_with_actor =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO notifications (user_id, notif_type, message, actor_user_id) \
     VALUES ($1, 'comment_reply', 'phnt legacy message', $2) RETURNING id"

let q_notif_by_id =
  (Caqti_type.int
  ->! Caqti_type.(t2 (t2 int string) (t2 (option int) (t3 int64 int int64))))
    "SELECT user_id, notif_type, actor_user_id, project_id, community_id, \
     relation_id FROM notifications WHERE id = $1"

let q_notif_exists =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE id = $1"

let q_delete_project_raw =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM open_source_projects WHERE id = $1"

let q_delete_community_raw =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM communities WHERE id = $1"

let q_delete_relation_raw =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM community_projects WHERE id = $1"

let q_absent_project_id = Home_audit_fixture.q_absent_project_id
let q_absent_community_id = Home_audit_fixture.q_absent_community_id
let q_absent_relation_id = Home_audit_fixture.q_absent_relation_id
let q_absent_user_id = Home_audit_fixture.q_absent_user_id

(* === schema signatures === *)

let q_constraints =
  (Caqti_type.unit ->* Caqti_type.(t3 string string bool))
    "SELECT conname, contype::text, convalidated FROM pg_constraint WHERE \
     conrelid = 'notifications'::regclass ORDER BY conname"

let q_fk_deltypes =
  (Caqti_type.unit ->* Caqti_type.(t2 string string))
    "SELECT conname, confdeltype::text FROM pg_constraint WHERE conrelid = \
     'notifications'::regclass AND contype = 'f' ORDER BY conname"

let q_indexdefs =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT indexdef FROM pg_indexes WHERE schemaname = 'public' AND tablename \
     = 'notifications' ORDER BY indexname"

(* The only textual columns may be the closed kind vocabulary and the
   legacy prose message: no note, no email, no username, no URL. *)
let q_text_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT column_name FROM information_schema.columns WHERE table_schema = \
     'public' AND table_name = 'notifications' AND data_type NOT IN \
     ('integer', 'bigint', 'boolean', 'timestamp without time zone') ORDER BY \
     column_name"

let q_credential_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT column_name FROM information_schema.columns WHERE table_schema = \
     'public' AND table_name = 'notifications' AND (column_name ~* 'token' OR \
     column_name ~* 'secret' OR column_name ~* 'verifier' OR column_name ~* \
     'oauth' OR column_name ~* 'session' OR column_name ~* 'note' OR \
     column_name ~* 'email' OR column_name ~* 'username' OR column_name ~* \
     'login' OR column_name ~* 'json' OR column_name ~* 'github' OR \
     column_name ~* 'installation' OR column_name ~* 'account' OR column_name \
     ~* 'namespace' OR column_name ~* 'url')"

let q_extension_columns =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*) FROM information_schema.columns WHERE table_schema = \
     'public' AND table_name = 'notifications' AND column_name IN \
     ('actor_user_id', 'project_id', 'community_id', 'relation_id')"

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

(* Byte-for-byte the migration's down and up statements, for the
   transactional round-trip below. *)
let q_down_statements =
  List.map ddl
    [
      "DELETE FROM notifications WHERE notif_type IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed')";
      "DROP INDEX idx_notifications_actor";
      "DROP INDEX idx_notifications_relation";
      "DROP INDEX idx_notifications_community";
      "DROP INDEX idx_notifications_project";
      "DROP INDEX uq_notifications_recipient_kind_relation";
      "ALTER TABLE notifications DROP CONSTRAINT \
       notifications_project_home_shape_check";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_notif_type_check";
      "ALTER TABLE notifications DROP COLUMN relation_id, DROP COLUMN \
       community_id, DROP COLUMN project_id, DROP COLUMN actor_user_id";
      "ALTER TABLE notifications ALTER COLUMN message SET NOT NULL";
    ]

let q_up_statements =
  List.map ddl
    [
      "ALTER TABLE notifications ALTER COLUMN message DROP NOT NULL";
      "ALTER TABLE notifications ADD COLUMN actor_user_id INTEGER CONSTRAINT \
       notifications_actor_user_id_fkey REFERENCES users(id) ON DELETE SET \
       NULL, ADD COLUMN project_id BIGINT CONSTRAINT \
       notifications_project_id_fkey REFERENCES open_source_projects(id) ON \
       DELETE CASCADE, ADD COLUMN community_id INTEGER CONSTRAINT \
       notifications_community_id_fkey REFERENCES communities(id) ON DELETE \
       CASCADE, ADD COLUMN relation_id BIGINT CONSTRAINT \
       notifications_relation_id_fkey REFERENCES community_projects(id) ON \
       DELETE CASCADE";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_notif_type_check \
       CHECK ( notif_type IN ('comment_reply', 'mention', 'mod_action', \
       'project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') )";
      "ALTER TABLE notifications ADD CONSTRAINT \
       notifications_project_home_shape_check CHECK ( (notif_type IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') AND project_id IS NOT \
       NULL AND community_id IS NOT NULL AND relation_id IS NOT NULL AND \
       message IS NULL AND post_id IS NULL) OR (notif_type NOT IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') AND project_id IS NULL \
       AND community_id IS NULL AND relation_id IS NULL AND actor_user_id IS \
       NULL AND message IS NOT NULL) )";
      "CREATE UNIQUE INDEX uq_notifications_recipient_kind_relation ON \
       notifications (user_id, notif_type, relation_id) WHERE relation_id IS \
       NOT NULL";
      "CREATE INDEX idx_notifications_project ON notifications (project_id) \
       WHERE project_id IS NOT NULL";
      "CREATE INDEX idx_notifications_community ON notifications \
       (community_id) WHERE community_id IS NOT NULL";
      "CREATE INDEX idx_notifications_relation ON notifications (relation_id) \
       WHERE relation_id IS NOT NULL";
      "CREATE INDEX idx_notifications_actor ON notifications (actor_user_id) \
       WHERE actor_user_id IS NOT NULL";
    ]

(* A later migration (the community-connection notification kinds) now
   sits on top of this one and replaces its shape CHECK, so this
   migration's down statements are no longer runnable on their own.
   Migrations unwind in reverse order: these two lists are that later
   migration's own down and up, wrapped around the pair below so the
   round-trip is over the same stack production actually applies. *)
let q_later_down_statements =
  List.map ddl
    [
      "DELETE FROM notifications WHERE notif_type IN \
       ('community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed')";
      "DROP INDEX idx_notifications_connection";
      "DROP INDEX uq_notifications_recipient_kind_connection";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_shape_check";
      "ALTER TABLE notifications ADD CONSTRAINT \
       notifications_project_home_shape_check CHECK ( (notif_type IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') AND project_id IS NOT \
       NULL AND community_id IS NOT NULL AND relation_id IS NOT NULL AND \
       message IS NULL AND post_id IS NULL) OR (notif_type NOT IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') AND project_id IS NULL \
       AND community_id IS NULL AND relation_id IS NULL AND actor_user_id IS \
       NULL AND message IS NOT NULL) )";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_notif_type_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_notif_type_check \
       CHECK ( notif_type IN ('comment_reply', 'mention', 'mod_action', \
       'project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed') )";
      "ALTER TABLE notifications DROP COLUMN connection_id";
    ]

let q_later_up_statements =
  List.map ddl
    [
      "ALTER TABLE notifications ADD COLUMN connection_id BIGINT CONSTRAINT \
       notifications_connection_id_fkey REFERENCES community_connections(id) \
       ON DELETE CASCADE";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_notif_type_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_notif_type_check \
       CHECK ( notif_type IN ('comment_reply', 'mention', 'mod_action', \
       'project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed', \
       'community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed') )";
      "ALTER TABLE notifications DROP CONSTRAINT \
       notifications_project_home_shape_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_shape_check \
       CHECK ( (notif_type IN ('project_home_requested', \
       'project_home_accepted', 'project_home_rejected', \
       'project_home_removed') AND project_id IS NOT NULL AND community_id IS \
       NOT NULL AND relation_id IS NOT NULL AND connection_id IS NULL AND \
       message IS NULL AND post_id IS NULL) OR (notif_type IN \
       ('community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed') AND \
       community_id IS NOT NULL AND connection_id IS NOT NULL AND project_id \
       IS NULL AND relation_id IS NULL AND message IS NULL AND post_id IS \
       NULL) OR (notif_type NOT IN ('project_home_requested', \
       'project_home_accepted', 'project_home_rejected', \
       'project_home_removed', 'community_connection_requested', \
       'community_connection_accepted', 'community_connection_rejected', \
       'community_connection_removed') AND project_id IS NULL AND community_id \
       IS NULL AND relation_id IS NULL AND connection_id IS NULL AND \
       actor_user_id IS NULL AND message IS NOT NULL) )";
      "CREATE UNIQUE INDEX uq_notifications_recipient_kind_connection ON \
       notifications (user_id, notif_type, connection_id) WHERE connection_id \
       IS NOT NULL";
      "CREATE INDEX idx_notifications_connection ON notifications \
       (connection_id) WHERE connection_id IS NOT NULL";
    ]

(* The shared-thread placement migration sits on top of the
   community-connection one and replaces the shape CHECK again, so the
   stack now unwinds through its notification statements first and
   restores them last. Byte-for-byte that migration's notifications
   portion. *)
let q_latest_down_statements =
  List.map ddl
    [
      "DELETE FROM notifications WHERE notif_type IN \
       ('shared_thread_requested', 'shared_thread_accepted', \
       'shared_thread_rejected', 'shared_thread_removed', \
       'shared_thread_withdrawn')";
      "DROP INDEX idx_notifications_shared_thread_placement";
      "DROP INDEX uq_notifications_recipient_kind_placement";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_shape_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_shape_check \
       CHECK ( (notif_type IN ('project_home_requested', \
       'project_home_accepted', 'project_home_rejected', \
       'project_home_removed') AND project_id IS NOT NULL AND community_id IS \
       NOT NULL AND relation_id IS NOT NULL AND connection_id IS NULL AND \
       message IS NULL AND post_id IS NULL) OR (notif_type IN \
       ('community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed') AND \
       community_id IS NOT NULL AND connection_id IS NOT NULL AND project_id \
       IS NULL AND relation_id IS NULL AND message IS NULL AND post_id IS \
       NULL) OR (notif_type NOT IN ('project_home_requested', \
       'project_home_accepted', 'project_home_rejected', \
       'project_home_removed', 'community_connection_requested', \
       'community_connection_accepted', 'community_connection_rejected', \
       'community_connection_removed') AND project_id IS NULL AND community_id \
       IS NULL AND relation_id IS NULL AND connection_id IS NULL AND \
       actor_user_id IS NULL AND message IS NOT NULL) )";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_notif_type_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_notif_type_check \
       CHECK ( notif_type IN ('comment_reply', 'mention', 'mod_action', \
       'project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed', \
       'community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed') )";
      "ALTER TABLE notifications DROP COLUMN shared_thread_placement_id";
    ]

let q_latest_up_statements =
  List.map ddl
    [
      "ALTER TABLE notifications ADD COLUMN shared_thread_placement_id BIGINT \
       CONSTRAINT notifications_shared_thread_placement_id_fkey REFERENCES \
       shared_thread_placements(id) ON DELETE CASCADE";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_notif_type_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_notif_type_check \
       CHECK ( notif_type IN ('comment_reply', 'mention', 'mod_action', \
       'project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed', \
       'community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed', \
       'shared_thread_requested', 'shared_thread_accepted', \
       'shared_thread_rejected', 'shared_thread_removed', \
       'shared_thread_withdrawn') )";
      "ALTER TABLE notifications DROP CONSTRAINT notifications_shape_check";
      "ALTER TABLE notifications ADD CONSTRAINT notifications_shape_check \
       CHECK ( (notif_type IN ('project_home_requested', \
       'project_home_accepted', 'project_home_rejected', \
       'project_home_removed') AND project_id IS NOT NULL AND community_id IS \
       NOT NULL AND relation_id IS NOT NULL AND connection_id IS NULL AND \
       shared_thread_placement_id IS NULL AND message IS NULL AND post_id IS \
       NULL) OR (notif_type IN ('community_connection_requested', \
       'community_connection_accepted', 'community_connection_rejected', \
       'community_connection_removed') AND community_id IS NOT NULL AND \
       connection_id IS NOT NULL AND project_id IS NULL AND relation_id IS \
       NULL AND shared_thread_placement_id IS NULL AND message IS NULL AND \
       post_id IS NULL) OR (notif_type IN ('shared_thread_requested', \
       'shared_thread_accepted', 'shared_thread_rejected', \
       'shared_thread_removed', 'shared_thread_withdrawn') AND community_id IS \
       NOT NULL AND shared_thread_placement_id IS NOT NULL AND project_id IS \
       NULL AND relation_id IS NULL AND connection_id IS NULL AND message IS \
       NULL AND post_id IS NULL) OR (notif_type NOT IN \
       ('project_home_requested', 'project_home_accepted', \
       'project_home_rejected', 'project_home_removed', \
       'community_connection_requested', 'community_connection_accepted', \
       'community_connection_rejected', 'community_connection_removed', \
       'shared_thread_requested', 'shared_thread_accepted', \
       'shared_thread_rejected', 'shared_thread_removed', \
       'shared_thread_withdrawn') AND project_id IS NULL AND community_id IS \
       NULL AND relation_id IS NULL AND connection_id IS NULL AND \
       shared_thread_placement_id IS NULL AND actor_user_id IS NULL AND \
       message IS NOT NULL) )";
      "CREATE UNIQUE INDEX uq_notifications_recipient_kind_placement ON \
       notifications (user_id, notif_type, shared_thread_placement_id) WHERE \
       shared_thread_placement_id IS NOT NULL";
      "CREATE INDEX idx_notifications_shared_thread_placement ON notifications \
       (shared_thread_placement_id) WHERE shared_thread_placement_id IS NOT \
       NULL";
    ]

(* === failure injection (installed and dropped per case) === *)

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION phnt_fail_fn() RETURNS trigger LANGUAGE plpgsql AS 'BEGIN \
     RAISE EXCEPTION ''phnt fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS phnt_fail_fn()"

let q_poison_notifications =
  ddl
    "CREATE TRIGGER phnt_fail_notif BEFORE INSERT ON notifications FOR EACH \
     ROW EXECUTE FUNCTION phnt_fail_fn()"

let q_unpoison_notifications =
  ddl "DROP TRIGGER IF EXISTS phnt_fail_notif ON notifications"

let q_poison_audit =
  ddl
    "CREATE TRIGGER phnt_fail_audit BEFORE INSERT ON project_home_audit_events \
     FOR EACH ROW EXECUTE FUNCTION phnt_fail_fn()"

let q_unpoison_audit =
  ddl "DROP TRIGGER IF EXISTS phnt_fail_audit ON project_home_audit_events"

let with_poison conn ~install ~remove f =
  let* () = exec conn "create fail fn" q_create_fail_fn () in
  Lwt.finalize
    (fun () ->
      let* () = exec conn "install poison trigger" install () in
      Lwt.finalize f (fun () -> exec conn "drop poison trigger" remove ()))
    (fun () -> exec conn "drop fail fn" q_drop_fail_fn ())

(* === display-name fixtures for the UI cases === *)

let q_set_project_name =
  (Caqti_type.(t2 string int64) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = $1 WHERE id = $2"

(* ==================== schema suite ==================== *)

let raw_fixture conn ~pslug ~cslug ~ns =
  let* recipient = insert_user conn ("phnt_raw_" ^ pslug) in
  let* actor = insert_user conn ("phnt_act_" ^ pslug) in
  let* project =
    find conn "raw project" q_insert_project_raw ("phnt-" ^ pslug, ns)
  in
  let* cid = insert_community conn ("phnt-" ^ cslug) in
  let* rid = find conn "raw relation" q_insert_relation_raw (project, cid) in
  Lwt.return (recipient, actor, project, cid, rid)

let kinds_case =
  db_case
    "schema: the four project-home kinds and the three real legacy kinds are \
     the whole durable vocabulary" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-kind" ~cslug:"sch-kind-home" ~ns:960100001L
      in
      let* () =
        Lwt_list.iter_s
          (fun kind ->
            let* id =
              C.find q_insert_notif_raw
                ((recipient, kind), (Some actor, (project, cid, rid)))
            in
            let* id = or_fail ("accept " ^ kind) id in
            Alcotest.(check bool) (kind ^ ": positive id") true (id > 0);
            Lwt.return_unit)
          [
            "project_home_requested";
            "project_home_accepted";
            "project_home_rejected";
            "project_home_removed";
          ]
      in
      (* Every real legacy kind — exactly what create_notif emits and
         pages.ml renders — stays accepted in its legacy shape. *)
      let* () =
        Lwt_list.iter_s
          (fun kind ->
            let* id = C.find q_insert_legacy_raw (recipient, kind, None) in
            let* id = or_fail ("accept legacy " ^ kind) id in
            Alcotest.(check bool)
              ("legacy " ^ kind ^ ": positive id")
              true (id > 0);
            Lwt.return_unit)
          [ "comment_reply"; "mention"; "mod_action" ]
      in
      (* Anything else is refused even in structured shape — including
         a legacy kind trying to borrow the structured columns. *)
      Lwt_list.iter_s
        (fun kind ->
          let* r =
            C.find q_insert_notif_raw
              ((recipient, kind), (Some actor, (project, cid, rid)))
          in
          reject ("reject " ^ kind) r)
        [
          "project_home_bogus";
          "PROJECT_HOME_REQUESTED";
          "";
          " project_home_requested";
          "home_requested";
          "comment_reply";
          "mention";
          "mod_action";
        ])

let unknown_kind_shapes_case =
  db_case
    "schema: an unknown kind is rejected durably in legacy, structured, and \
     mixed shapes alike" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-unk" ~cslug:"sch-unk-home" ~ns:960100008L
      in
      (* A perfectly valid-looking legacy row: prose message, every
         structured column NULL. The shape CHECK alone accepts this,
         so only the closed-vocabulary CHECK stands in the way — the
         regression this case exists for. *)
      let* r =
        C.find q_insert_legacy_raw (recipient, "completely_unknown_kind", None)
      in
      let* () = reject "unknown kind, legacy shape" r in
      (* A perfectly valid-looking structured row. *)
      let* r =
        C.find q_insert_notif_raw
          ( (recipient, "completely_unknown_kind"),
            (Some actor, (project, cid, rid)) )
      in
      let* () = reject "unknown kind, structured shape" r in
      (* Mixed shapes: prose plus subjects, and a partial subject set. *)
      let* r =
        C.find q_insert_home_with_message
          ((recipient, "completely_unknown_kind"), (project, cid, rid))
      in
      let* () = reject "unknown kind, prose plus subjects" r in
      let* r =
        C.find q_insert_notif_nullable
          ( (recipient, "completely_unknown_kind"),
            (Some actor, (Some project, None, None)) )
      in
      reject "unknown kind, partial subjects" r)

let required_shape_case =
  db_case
    "schema: a project-home notification requires all three subject references \
     and no prose, post, or legacy mixing" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-shape" ~cslug:"sch-shape-home"
          ~ns:960100002L
      in
      (* NULL subjects: the shape CHECK refuses each. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, p, c, r) ->
            let* res =
              C.find q_insert_notif_nullable
                ((recipient, "project_home_requested"), (Some actor, (p, c, r)))
            in
            reject label res)
          [
            ("null project", None, Some cid, Some rid);
            ("null community", Some project, None, Some rid);
            ("null relation", Some project, Some cid, None);
          ]
      in
      (* Prose or a post link on a structured row: refused. *)
      let* post = find conn "legacy post" q_insert_post_raw (cid, recipient) in
      let* r =
        C.find q_insert_home_with_message
          ((recipient, "project_home_requested"), (project, cid, rid))
      in
      let* () = reject "prose on a project-home row" r in
      let* r =
        C.find q_insert_home_with_post
          ((recipient, "project_home_requested"), (post, (project, cid, rid)))
      in
      let* () = reject "post link on a project-home row" r in
      (* Legacy rows keep working, but can never borrow the new
         columns. *)
      let* id =
        C.find q_insert_legacy_raw (recipient, "comment_reply", Some post)
      in
      let* id = or_fail "legacy with post accepted" id in
      Alcotest.(check bool) "legacy with post: positive id" true (id > 0);
      let* id = C.find q_insert_legacy_raw (recipient, "mod_action", None) in
      let* id = or_fail "legacy without post accepted" id in
      Alcotest.(check bool) "legacy without post: positive id" true (id > 0);
      let* r = C.find q_insert_legacy_with_actor (recipient, actor) in
      let* () = reject "actor on a legacy row" r in
      (* A NULL actor is the legal retention shape for a structured
         row. *)
      let* id =
        C.find q_insert_notif_raw
          ((recipient, "project_home_requested"), (None, (project, cid, rid)))
      in
      let* id = or_fail "null actor accepted" id in
      Alcotest.(check bool) "null actor: positive id" true (id > 0);
      (* Dangling references: the FKs refuse each. *)
      let* ghost_p = find conn "ghost project" q_absent_project_id () in
      let* ghost_c = find conn "ghost community" q_absent_community_id () in
      let* ghost_r = find conn "ghost relation" q_absent_relation_id () in
      let* ghost_u = find conn "ghost user" q_absent_user_id () in
      Lwt_list.iter_s
        (fun (label, recip, a, p, c, r) ->
          let* res =
            C.find q_insert_notif_raw
              ((recip, "project_home_accepted"), (a, (p, c, r)))
          in
          reject label res)
        [
          ("dangling project", recipient, Some actor, ghost_p, cid, rid);
          ("dangling community", recipient, Some actor, project, ghost_c, rid);
          ("dangling relation", recipient, Some actor, project, cid, ghost_r);
          ("dangling actor", recipient, Some ghost_u, project, cid, rid);
          ("dangling recipient", ghost_u, Some actor, project, cid, rid);
        ])

let dedup_case =
  db_case
    "schema: at most one notification per recipient, kind, and relation; other \
     kinds, recipients, and relations coexist" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-dedup" ~cslug:"sch-dedup-home"
          ~ns:960100003L
      in
      let* other = insert_user conn "phnt_raw_sch_dedup_b" in
      let insert recip kind relation =
        C.find q_insert_notif_raw
          ((recip, kind), (Some actor, (project, cid, relation)))
      in
      let* id = insert recipient "project_home_requested" rid in
      let* id = or_fail "first" id in
      Alcotest.(check bool) "first: positive id" true (id > 0);
      (* The exact triple again: the unique index refuses it. *)
      let* r = insert recipient "project_home_requested" rid in
      let* () = reject "duplicate triple" r in
      (* Another kind, another recipient: both legal. *)
      let* id = insert recipient "project_home_accepted" rid in
      let* _ = or_fail "other kind" id in
      let* id = insert other "project_home_requested" rid in
      let* _ = or_fail "other recipient" id in
      (* A future replacement relation notifies again. *)
      let* rid2 =
        find conn "removed relation" q_insert_relation_removed_raw (project, cid)
      in
      let* id = insert recipient "project_home_requested" rid2 in
      let* id = or_fail "replacement relation" id in
      Alcotest.(check bool) "replacement: positive id" true (id > 0);
      Lwt.return_unit)

let actor_deletion_case =
  db_case
    "schema: deleting the actor nulls the reference and the notification \
     survives untouched" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-adel" ~cslug:"sch-adel-home" ~ns:960100004L
      in
      let* notif =
        C.find q_insert_notif_raw
          ( (recipient, "project_home_accepted"),
            (Some actor, (project, cid, rid)) )
      in
      let* notif = or_fail "notification" notif in
      let* () = exec conn "delete actor" q_delete_user actor in
      let* row = find conn "notification row" q_notif_by_id notif in
      Alcotest.(check notif_t)
        "notification survives with a NULL actor and intact subjects"
        ((recipient, "project_home_accepted"), (None, cid, rid))
        (let (recip, kind), (a, (_p, c, r)) = row in
         ((recip, kind), (a, c, r)));
      Lwt.return_unit)

let recipient_deletion_case =
  db_case
    "schema: deleting the recipient deletes the notification, as for every \
     other notification kind" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-rdel" ~cslug:"sch-rdel-home" ~ns:960100005L
      in
      let* notif =
        C.find q_insert_notif_raw
          ( (recipient, "project_home_removed"),
            (Some actor, (project, cid, rid)) )
      in
      let* notif = or_fail "notification" notif in
      let* () = exec conn "delete recipient" q_delete_user recipient in
      let* n = find conn "gone" q_notif_exists notif in
      Alcotest.(check int) "recipient deletion removes the row" 0 n;
      Lwt.return_unit)

let subject_deletion_case =
  db_case
    "schema: subject deletion follows the user-facing cascade convention for \
     relation, project, and community" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* recipient, actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-sdel" ~cslug:"sch-sdel-home" ~ns:960100006L
      in
      let insert kind relation =
        let* n =
          C.find q_insert_notif_raw
            ((recipient, kind), (Some actor, (project, cid, relation)))
        in
        or_fail ("notification " ^ kind) n
      in
      (* Relation deletion cascades its notification only. *)
      let* n1 = insert "project_home_requested" rid in
      let* () = exec conn "delete relation" q_delete_relation_raw rid in
      let* gone = find conn "n1 gone" q_notif_exists n1 in
      Alcotest.(check int) "relation cascade" 0 gone;
      (* Project deletion cascades through its relations and rows. *)
      let* rid2 = find conn "relation 2" q_insert_relation_raw (project, cid) in
      let* n2 = insert "project_home_accepted" rid2 in
      let* () = exec conn "delete project" q_delete_project_raw project in
      let* gone = find conn "n2 gone" q_notif_exists n2 in
      Alcotest.(check int) "project cascade" 0 gone;
      (* Community deletion cascades likewise. *)
      let* project2 =
        find conn "raw project 2" q_insert_project_raw
          ("phnt-sch-sdel2", 960100007L)
      in
      let* rid3 =
        find conn "relation 3" q_insert_relation_raw (project2, cid)
      in
      let* n3 =
        let* n =
          C.find q_insert_notif_raw
            ( (recipient, "project_home_rejected"),
              (Some actor, (project2, cid, rid3)) )
        in
        or_fail "notification 3" n
      in
      let* () = exec conn "delete community" q_delete_community_raw cid in
      let* gone = find conn "n3 gone" q_notif_exists n3 in
      Alcotest.(check int) "community cascade" 0 gone;
      Lwt.return_unit)

let constraints_indexes_case =
  db_case
    "schema: constraints and indexes are present, validated, and carry the \
     chosen deletion semantics" (fun ~url:_ conn ->
      let* rows = collect conn "constraints" q_constraints () in
      Alcotest.(check (list (triple string string bool)))
        "constraints present and validated"
        (* This case pins the whole notifications table, not just the
           columns this migration added, so the later
           community-connection and shared-thread-placement migrations'
           own columns, constraints, and indexes are part of the
           expected shape. The single shape CHECK is now named for what
           it enforces — all four row shapes — and the project-home
           branch inside it is unchanged. *)
        [
          ("notifications_actor_user_id_fkey", "f", true);
          ("notifications_community_id_fkey", "f", true);
          ("notifications_connection_id_fkey", "f", true);
          ("notifications_notif_type_check", "c", true);
          ("notifications_pkey", "p", true);
          ("notifications_post_id_fkey", "f", true);
          ("notifications_project_id_fkey", "f", true);
          ("notifications_relation_id_fkey", "f", true);
          ("notifications_shape_check", "c", true);
          ("notifications_shared_thread_placement_id_fkey", "f", true);
          ("notifications_user_id_fkey", "f", true);
        ]
        rows;
      let* deltypes = collect conn "fk deltypes" q_fk_deltypes () in
      Alcotest.(check (list (pair string string)))
        "SET NULL actor, CASCADE recipient and subjects"
        [
          ("notifications_actor_user_id_fkey", "n");
          ("notifications_community_id_fkey", "c");
          ("notifications_connection_id_fkey", "c");
          ("notifications_post_id_fkey", "c");
          ("notifications_project_id_fkey", "c");
          ("notifications_relation_id_fkey", "c");
          ("notifications_shared_thread_placement_id_fkey", "c");
          ("notifications_user_id_fkey", "c");
        ]
        deltypes;
      let* defs = collect conn "index defs" q_indexdefs () in
      Alcotest.(check (list string))
        "exactly the three dedup indexes, the six FK indexes, and the primary \
         key"
        [
          "CREATE INDEX idx_notifications_actor ON public.notifications USING \
           btree (actor_user_id) WHERE (actor_user_id IS NOT NULL)";
          "CREATE INDEX idx_notifications_community ON public.notifications \
           USING btree (community_id) WHERE (community_id IS NOT NULL)";
          "CREATE INDEX idx_notifications_connection ON public.notifications \
           USING btree (connection_id) WHERE (connection_id IS NOT NULL)";
          "CREATE INDEX idx_notifications_project ON public.notifications \
           USING btree (project_id) WHERE (project_id IS NOT NULL)";
          "CREATE INDEX idx_notifications_relation ON public.notifications \
           USING btree (relation_id) WHERE (relation_id IS NOT NULL)";
          "CREATE INDEX idx_notifications_shared_thread_placement ON \
           public.notifications USING btree (shared_thread_placement_id) WHERE \
           (shared_thread_placement_id IS NOT NULL)";
          "CREATE UNIQUE INDEX notifications_pkey ON public.notifications \
           USING btree (id)";
          "CREATE UNIQUE INDEX uq_notifications_recipient_kind_connection ON \
           public.notifications USING btree (user_id, notif_type, \
           connection_id) WHERE (connection_id IS NOT NULL)";
          "CREATE UNIQUE INDEX uq_notifications_recipient_kind_placement ON \
           public.notifications USING btree (user_id, notif_type, \
           shared_thread_placement_id) WHERE (shared_thread_placement_id IS \
           NOT NULL)";
          "CREATE UNIQUE INDEX uq_notifications_recipient_kind_relation ON \
           public.notifications USING btree (user_id, notif_type, relation_id) \
           WHERE (relation_id IS NOT NULL)";
        ]
        defs;
      Lwt.return_unit)

let column_privacy_case =
  db_case
    "schema: the closed kind and the legacy prose message are the only textual \
     columns and no credential-shaped column exists" (fun ~url:_ conn ->
      let* text_columns = collect conn "text columns" q_text_columns () in
      Alcotest.(check (list string))
        "only message and notif_type are textual"
        [ "message"; "notif_type" ]
        text_columns;
      let* bad = collect conn "credential columns" q_credential_columns () in
      Alcotest.(check (list string)) "no credential-shaped column" [] bad;
      Lwt.return_unit)

let roundtrip_case =
  db_case "schema: the down/up statement pair round-trips" (fun ~url:_ conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      (* The whole round-trip runs inside one transaction that is
         always rolled back, so the live table cannot be lost even if
         an assertion fails between the drop and the re-create. *)
      let* r = C.start () in
      let* () = or_fail "begin" r in
      let* () =
        Lwt.finalize
          (fun () ->
            (* Unwind in reverse migration order: the latest
               shared-thread-placement migration first, then the
               community-connection one, then this one. *)
            let* () =
              Lwt_list.iter_s
                (fun q -> exec conn "latest down" q ())
                q_latest_down_statements
            in
            let* () =
              Lwt_list.iter_s
                (fun q -> exec conn "later down" q ())
                q_later_down_statements
            in
            let* () =
              Lwt_list.iter_s (fun q -> exec conn "down" q ()) q_down_statements
            in
            let* n = find conn "absent" q_extension_columns () in
            Alcotest.(check int) "down removes the four columns" 0 n;
            let* () =
              Lwt_list.iter_s (fun q -> exec conn "up" q ()) q_up_statements
            in
            let* n = find conn "present" q_extension_columns () in
            Alcotest.(check int) "up restores the four columns" 4 n;
            let* () =
              Lwt_list.iter_s
                (fun q -> exec conn "later up" q ())
                q_later_up_statements
            in
            let* () =
              Lwt_list.iter_s
                (fun q -> exec conn "latest up" q ())
                q_latest_up_statements
            in
            let* rows = collect conn "constraints" q_constraints () in
            Alcotest.(check int)
              "up restores all eleven constraints" 11 (List.length rows);
            Lwt.return_unit)
          (fun () ->
            let* _ = C.rollback () in
            Lwt.return_unit)
      in
      let* n = find conn "live columns" q_extension_columns () in
      Alcotest.(check int) "live table untouched" 4 n;
      Lwt.return_unit)

let schema_suite =
  [
    kinds_case;
    unknown_kind_shapes_case;
    required_shape_case;
    dedup_case;
    actor_deletion_case;
    recipient_deletion_case;
    subject_deletion_case;
    constraints_indexes_case;
    column_privacy_case;
    roundtrip_case;
  ]

(* ==================== exact recipients ==================== *)

let request_recipients_case =
  db_case
    "recipients: a request notifies every other target top moderator and \
     nobody else" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_req_owner" in
      let* m1 = insert_user conn "phnt_req_m1" in
      let* m2 = insert_user conn "phnt_req_m2" in
      let* ordinary = insert_user conn "phnt_req_mod" in
      let* legacy = insert_user conn "phnt_req_legacy" in
      let* member = insert_user conn "phnt_req_member" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000001L ~slug:"phnt-req"
      in
      let* cid = insert_community conn "phnt-req-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator
          (m2, cid, "top_mod")
      in
      let* () =
        exec conn "ordinary mod" Community_fixture.q_insert_moderator
          (ordinary, cid, "mod")
      in
      let* () =
        exec conn "legacy mod" Community_fixture.q_insert_moderator
          (legacy, cid, "legacy_mod")
      in
      let* () =
        exec conn "member" Community_fixture.q_insert_member (member, cid)
      in
      (* The requesting actor is also a top moderator: still never
         notified. *)
      let* () =
        exec conn "actor top mod" Community_fixture.q_insert_moderator
          (owner, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-req" ~community:cid
      in
      check_notifs "request" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((m2, "project_home_requested"), (Some owner, cid, rid));
        ])

let request_zero_recipients_case =
  db_case
    "recipients: a target with no other top moderator commits the request with \
     zero notifications" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rq0_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000002L ~slug:"phnt-rq0"
      in
      let* cid = insert_community conn "phnt-rq0-home" in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-rq0" ~community:cid
      in
      let* () =
        check_events "request committed" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid)) ]
      in
      check_notif_count "zero recipients" conn ~project 0)

let accept_recipients_case =
  db_case
    "recipients: acceptance notifies exactly the requester — not the reviewer, \
     not a second steward" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_acc_owner" in
      let* steward2 = insert_user conn "phnt_acc_steward2" in
      let* m1 = insert_user conn "phnt_acc_m1" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:960000003L ~slug:"phnt-acc"
      in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, steward2, inst)
      in
      let* cid = insert_community conn "phnt-acc-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-acc" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-acc"
          ~community:"phnt-acc-home" Rvs.Accept Phr.Accepted
      in
      check_notifs "accept" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
        ])

let reject_recipients_case =
  db_case "recipients: rejection notifies exactly the requester"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rej_owner" in
      let* m1 = insert_user conn "phnt_rej_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000004L ~slug:"phnt-rej"
      in
      let* cid = insert_community conn "phnt-rej-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-rej" ~community:cid
      in
      let* () =
        review_ok "reject" conn ~reviewer:m1 ~slug:"phnt-rej"
          ~community:"phnt-rej-home" Rvs.Reject Phr.Rejected
      in
      check_notifs "reject" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_rejected"), (Some m1, cid, rid));
        ])

let deleted_requester_case =
  db_case
    "recipients: a deleted requester yields zero decision recipients and the \
     decision still commits" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_dreq_owner" in
      let* m1 = insert_user conn "phnt_dreq_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000005L ~slug:"phnt-dreq"
      in
      let* cid = insert_community conn "phnt-dreq-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-dreq" ~community:cid
      in
      (* A co-steward with fresh GitHub evidence keeps the project
         verified once the requester's account (and steward row) is gone;
         acceptance needs one. Decision recipients are the requester
         only, so the co-steward receives nothing. *)
      let* co = insert_user conn "phnt_dreq_co" in
      let* () =
        exec conn "co-steward" Project_fixture.q_insert_steward_fixture
          (project, co, _inst)
      in
      let* () = exec conn "delete requester" q_delete_user owner in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-dreq"
          ~community:"phnt-dreq-home" Rvs.Accept Phr.Accepted
      in
      let* () = check_status "decision committed" conn rid "accepted" in
      (* The request notification survives with a NULL actor; no
         decision notification exists. *)
      check_notifs "after decision" conn ~project
        [ ((m1, "project_home_requested"), (None, cid, rid)) ])

let self_review_case =
  db_case
    "recipients: a reviewer deciding their own request receives no \
     self-notification" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_self_owner" in
      let* m1 = insert_user conn "phnt_self_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000006L ~slug:"phnt-self"
      in
      let* cid = insert_community conn "phnt-self-home" in
      let* () =
        exec conn "owner top mod" Community_fixture.q_insert_moderator
          (owner, cid, "top_mod")
      in
      let* () =
        exec conn "other top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-self" ~community:cid
      in
      let* () =
        review_ok "self accept" conn ~reviewer:owner ~slug:"phnt-self"
          ~community:"phnt-self-home" Rvs.Accept Phr.Accepted
      in
      let* () = check_status "decision committed" conn rid "accepted" in
      check_notifs "after self decision" conn ~project
        [ ((m1, "project_home_requested"), (Some owner, cid, rid)) ])

let removal_recipients_case =
  db_case
    "recipients: removal notifies stewards and top moderators minus the actor, \
     and nobody else" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rem_owner" in
      let* steward2 = insert_user conn "phnt_rem_steward2" in
      let* m1 = insert_user conn "phnt_rem_m1" in
      let* m2 = insert_user conn "phnt_rem_m2" in
      let* ordinary = insert_user conn "phnt_rem_mod" in
      let* member = insert_user conn "phnt_rem_member" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:960000010L ~slug:"phnt-rem"
      in
      let* () =
        exec conn "second steward" Home_request_fixture.q_insert_steward
          (project, steward2, inst)
      in
      let* cid = insert_community conn "phnt-rem-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator
          (m2, cid, "top_mod")
      in
      let* () =
        exec conn "ordinary mod" Community_fixture.q_insert_moderator
          (ordinary, cid, "mod")
      in
      let* () =
        exec conn "member" Community_fixture.q_insert_member (member, cid)
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-rem" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-rem"
          ~community:"phnt-rem-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "steward-side removal" conn ~actor:owner ~slug:"phnt-rem"
          ~community:"phnt-rem-home"
      in
      check_notifs "removal" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((m2, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
          ((steward2, "project_home_removed"), (Some owner, cid, rid));
          ((m1, "project_home_removed"), (Some owner, cid, rid));
          ((m2, "project_home_removed"), (Some owner, cid, rid));
        ])

let removal_union_dedup_case =
  db_case
    "recipients: a counterparty in both groups is notified once and a \
     multiply-authorized actor is still excluded once" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_dup_owner" in
      let* both = insert_user conn "phnt_dup_both" in
      let* m1 = insert_user conn "phnt_dup_m1" in
      let* inst, project =
        make_project conn ~user:owner ~ext_id:960000011L ~slug:"phnt-dup"
      in
      (* [both] is steward and top moderator; the acting [owner] is
         steward and top moderator too. *)
      let* () =
        exec conn "both steward" Home_request_fixture.q_insert_steward
          (project, both, inst)
      in
      let* cid = insert_community conn "phnt-dup-home" in
      let* () =
        exec conn "both top mod" Community_fixture.q_insert_moderator
          (both, cid, "top_mod")
      in
      let* () =
        exec conn "owner top mod" Community_fixture.q_insert_moderator
          (owner, cid, "top_mod")
      in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-dup" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-dup"
          ~community:"phnt-dup-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "removal" conn ~actor:owner ~slug:"phnt-dup"
          ~community:"phnt-dup-home"
      in
      check_notifs "union dedup" conn ~project
        [
          ((both, "project_home_requested"), (Some owner, cid, rid));
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
          ((both, "project_home_removed"), (Some owner, cid, rid));
          ((m1, "project_home_removed"), (Some owner, cid, rid));
        ])

let removal_admin_actor_case =
  db_case
    "recipients: a durable-admin actor outside both groups notifies both sides \
     and is never notified" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_adm_owner" in
      let* m1 = insert_user conn "phnt_adm_m1" in
      let* admin = insert_user conn "phnt_adm_admin" in
      let* () =
        exec conn "make admin" Community_fixture.q_set_admin (admin, true)
      in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000012L ~slug:"phnt-adm"
      in
      let* cid = insert_community conn "phnt-adm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-adm" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-adm"
          ~community:"phnt-adm-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "admin removal" conn ~actor:admin ~slug:"phnt-adm"
          ~community:"phnt-adm-home"
      in
      check_notifs "admin removal" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
          ((owner, "project_home_removed"), (Some admin, cid, rid));
          ((m1, "project_home_removed"), (Some admin, cid, rid));
        ])

let recipient_suite =
  [
    request_recipients_case;
    request_zero_recipients_case;
    accept_recipients_case;
    reject_recipients_case;
    deleted_requester_case;
    self_review_case;
    removal_recipients_case;
    removal_union_dedup_case;
    removal_admin_actor_case;
  ]

(* ========= notification failure rolls the mutation back ========= *)

let request_rollback_case =
  db_case
    "rollback: a failed request notification insertion leaves no pending \
     relation, no audit event, and no notification" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rbq_owner" in
      let* m1 = insert_user conn "phnt_rbq_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000020L ~slug:"phnt-rbq"
      in
      let* cid = insert_community conn "phnt-rbq-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        with_poison conn ~install:q_poison_notifications
          ~remove:q_unpoison_notifications (fun () ->
            Home_request_fixture.create_expect "poisoned notification"
              Rq.Storage_error conn ~user:owner ~slug:"phnt-rbq" ~community:cid
              (Home_request_fixture.phr_fresh_pending ()))
      in
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "no pending relation" 0 relations;
      let* () = check_event_count "no audit event" conn ~project 0 in
      let* () = check_notif_count "no notification" conn ~project 0 in
      (* The un-poisoned path immediately succeeds whole. *)
      let* rid =
        request_ok "recovered" conn ~user:owner ~slug:"phnt-rbq" ~community:cid
      in
      check_notifs "recovered" conn ~project
        [ ((m1, "project_home_requested"), (Some owner, cid, rid)) ])

let review_rollback_case =
  db_case
    "rollback: a failed decision notification insertion leaves the relation \
     pending with no decision event" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rbv_owner" in
      let* m1 = insert_user conn "phnt_rbv_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000021L ~slug:"phnt-rbv"
      in
      let* cid = insert_community conn "phnt-rbv-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-rbv" ~community:cid
      in
      let* () =
        with_poison conn ~install:q_poison_notifications
          ~remove:q_unpoison_notifications (fun () ->
            review_expect "poisoned accept" Rvs.Storage_error conn ~reviewer:m1
              ~slug:"phnt-rbv" ~community:"phnt-rbv-home" Rvs.Accept)
      in
      let* () = check_status "after rollback" conn rid "pending" in
      let* () =
        check_events "request event alone" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid)) ]
      in
      check_notifs "request notification alone" conn ~project
        [ ((m1, "project_home_requested"), (Some owner, cid, rid)) ])

let removal_rollback_case =
  db_case
    "rollback: a failed removal notification insertion leaves the relation \
     accepted with no removal event" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rbm_owner" in
      let* m1 = insert_user conn "phnt_rbm_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000022L ~slug:"phnt-rbm"
      in
      let* cid = insert_community conn "phnt-rbm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-rbm" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-rbm"
          ~community:"phnt-rbm-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        with_poison conn ~install:q_poison_notifications
          ~remove:q_unpoison_notifications (fun () ->
            remove_expect "poisoned removal" Rms.Storage_error conn ~actor:owner
              ~slug:"phnt-rbm" ~community:"phnt-rbm-home")
      in
      let* () = check_status "after rollback" conn rid "accepted" in
      let* () =
        check_events "no removal event" conn ~project
          [
            (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some m1), (cid, rid));
          ]
      in
      let* () =
        check_kind_count "no removal notification" conn ~project
          ~kind:"project_home_removed" 0
      in
      check_notifs "earlier notifications intact" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
        ])

let audit_failure_case =
  db_case
    "rollback: a failed audit insertion also leaves no notification behind"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_rba_owner" in
      let* m1 = insert_user conn "phnt_rba_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000023L ~slug:"phnt-rba"
      in
      let* cid = insert_community conn "phnt-rba-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Home_request_fixture.create_expect "poisoned audit" Rq.Storage_error
              conn ~user:owner ~slug:"phnt-rba" ~community:cid
              (Home_request_fixture.phr_fresh_pending ()))
      in
      let* relations =
        find conn "relations" Home_request_fixture.q_count_for_project project
      in
      Alcotest.(check int) "no pending relation" 0 relations;
      let* () = check_event_count "no audit event" conn ~project 0 in
      check_notif_count "no notification" conn ~project 0)

let rollback_suite =
  [
    request_rollback_case;
    review_rollback_case;
    removal_rollback_case;
    audit_failure_case;
  ]

(* ============= no notification on any failure ============= *)

let request_failures_case =
  db_case
    "no notification: every refused request path leaves the notification set \
     empty or unchanged" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_frq_owner" in
      let* outsider = insert_user conn "phnt_frq_outsider" in
      let* m1 = insert_user conn "phnt_frq_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000030L ~slug:"phnt-frq"
      in
      let* cid = insert_community conn "phnt-frq-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      (* Invalid pure input. *)
      let* () =
        Home_request_fixture.create_expect "invalid user" Rq.Invalid_user_id
          conn ~user:0 ~slug:"phnt-frq" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      (* Unavailable project and community. *)
      let* () =
        Home_request_fixture.create_expect "missing project"
          Rq.Project_unavailable conn ~user:owner ~slug:"phnt-frq-none"
          ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      let* ghost_c = find conn "ghost community" q_absent_community_id () in
      let* () =
        Home_request_fixture.create_expect "missing community"
          Rq.Community_unavailable conn ~user:owner ~slug:"phnt-frq"
          ~community:ghost_c
          (Home_request_fixture.phr_fresh_pending ())
      in
      (* Unauthorized actor: not a steward. *)
      let* () =
        Home_request_fixture.create_expect "outsider" Rq.Project_unavailable
          conn ~user:outsider ~slug:"phnt-frq" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      let* () = check_notif_count "only failures so far" conn ~project 0 in
      (* Duplicate: the second request finds the active-home slot
         taken and adds nothing. *)
      let* rid =
        request_ok "winner" conn ~user:owner ~slug:"phnt-frq" ~community:cid
      in
      let* () =
        Home_request_fixture.create_expect "duplicate" Rq.Active_home_exists
          conn ~user:owner ~slug:"phnt-frq" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      check_notifs "after duplicate" conn ~project
        [ ((m1, "project_home_requested"), (Some owner, cid, rid)) ])

let review_failures_case =
  db_case
    "no notification: every refused review path leaves exactly the request \
     notifications" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_frv_owner" in
      let* m1 = insert_user conn "phnt_frv_m1" in
      let* m2 = insert_user conn "phnt_frv_m2" in
      let* plain = insert_user conn "phnt_frv_plain" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000031L ~slug:"phnt-frv"
      in
      let* cid = insert_community conn "phnt-frv-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator
          (m2, cid, "top_mod")
      in
      (* No pending request yet. *)
      let* () =
        review_expect "nothing to review" Rvs.Review_unavailable conn
          ~reviewer:m1 ~slug:"phnt-frv" ~community:"phnt-frv-home" Rvs.Accept
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-frv" ~community:cid
      in
      let requested_only label =
        check_notifs label conn ~project
          [
            ((m1, "project_home_requested"), (Some owner, cid, rid));
            ((m2, "project_home_requested"), (Some owner, cid, rid));
          ]
      in
      (* Unauthorized reviewer. *)
      let* () =
        review_expect "plain user" Rvs.Reviewer_unauthorized conn
          ~reviewer:plain ~slug:"phnt-frv" ~community:"phnt-frv-home" Rvs.Accept
      in
      let* () = requested_only "after unauthorized" in
      (* Acceptance refused on an ineligible target. *)
      let* () = exec conn "go draft" Community_fixture.q_make_draft_state cid in
      let* () =
        review_expect "ineligible target" Rvs.Target_ineligible conn
          ~reviewer:m1 ~slug:"phnt-frv" ~community:"phnt-frv-home" Rvs.Accept
      in
      let* () =
        exec conn "restore eligibility" Home_review_fixture.q_make_eligible cid
      in
      let* () = requested_only "after ineligible" in
      (* The winning decision, then a losing replay. *)
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-frv"
          ~community:"phnt-frv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        review_expect "losing replay" Rvs.Review_unavailable conn ~reviewer:m2
          ~slug:"phnt-frv" ~community:"phnt-frv-home" Rvs.Reject
      in
      check_notifs "one decision notification" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((m2, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
        ])

let removal_failures_case =
  db_case
    "no notification: refused, replayed, and draft-protected removals add \
     nothing" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_frm_owner" in
      let* m1 = insert_user conn "phnt_frm_m1" in
      let* plain = insert_user conn "phnt_frm_plain" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000032L ~slug:"phnt-frm"
      in
      let* cid = insert_community conn "phnt-frm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-frm" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-frm"
          ~community:"phnt-frm-home" Rvs.Accept Phr.Accepted
      in
      (* Unauthorized remover. *)
      let* () =
        remove_expect "unauthorized" Rms.Actor_unauthorized conn ~actor:plain
          ~slug:"phnt-frm" ~community:"phnt-frm-home"
      in
      let* () =
        check_kind_count "no removal notification yet" conn ~project
          ~kind:"project_home_removed" 0
      in
      (* Success, then replay. *)
      let* () =
        remove_ok "removal" conn ~actor:owner ~slug:"phnt-frm"
          ~community:"phnt-frm-home"
      in
      let* () =
        remove_expect "replay" Rms.Removal_unavailable conn ~actor:m1
          ~slug:"phnt-frm" ~community:"phnt-frm-home"
      in
      let* () =
        check_notifs "one removal set" conn ~project
          [
            ((m1, "project_home_requested"), (Some owner, cid, rid));
            ((owner, "project_home_accepted"), (Some m1, cid, rid));
            ((m1, "project_home_removed"), (Some owner, cid, rid));
          ]
      in
      (* Protected unpublished-draft removal on a second project —
         provisioning itself must also have created nothing. *)
      let* owner2 = insert_user conn "phnt_frm_owner2" in
      let* project2, _cid2, _rid2 =
        make_draft conn ~user:owner2 ~ext_id:960000033L
          ~project_slug:"phnt-frm2" ~slug:"phnt-frm2-home"
      in
      let* () =
        check_notif_count "provisioning creates no notification" conn
          ~project:project2 0
      in
      let* () =
        remove_expect "protected draft" Rms.Removal_unavailable conn
          ~actor:owner2 ~slug:"phnt-frm2" ~community:"phnt-frm2-home"
      in
      check_notif_count "draft protection adds nothing" conn ~project:project2 0)

let no_notification_suite =
  [ request_failures_case; review_failures_case; removal_failures_case ]

(* ==================== concurrency ==================== *)

let request_race_case =
  db_case
    "concurrency: two live identical requests leave one pending relation and \
     one notification set" (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_crq_owner" in
      let* m1 = insert_user conn "phnt_crq_m1" in
      let* m2 = insert_user conn "phnt_crq_m2" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000040L ~slug:"phnt-crq"
      in
      let* cid = insert_community conn "phnt-crq-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator
          (m2, cid, "top_mod")
      in
      let* _created =
        Db_fixture.with_second_connection (fun conn2 ->
            let* results =
              Lwt.both
                (Home_request_fixture.create conn ~user:owner ~slug:"phnt-crq"
                   ~community:cid
                   (Home_request_fixture.phr_fresh_pending ()))
                (Home_request_fixture.create conn2 ~user:owner ~slug:"phnt-crq"
                   ~community:cid
                   (Home_request_fixture.phr_fresh_pending ()))
            in
            Lwt.return
              (Home_request_fixture.ok_and_error "request race"
                 Rq.Active_home_exists results))
      in
      let* rid = sole_relation "one relation" conn project in
      check_notifs "one notification set" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((m2, "project_home_requested"), (Some owner, cid, rid));
        ])

let review_race_case =
  db_case "concurrency: two reviewers produce exactly one decision notification"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_crv_owner" in
      let* m1 = insert_user conn "phnt_crv_m1" in
      let* m2 = insert_user conn "phnt_crv_m2" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000041L ~slug:"phnt-crv"
      in
      let* cid = insert_community conn "phnt-crv-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator
          (m2, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-crv" ~community:cid
      in
      let* () =
        review_ok "first reviewer" conn ~reviewer:m1 ~slug:"phnt-crv"
          ~community:"phnt-crv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        review_expect "second reviewer" Rvs.Review_unavailable conn ~reviewer:m2
          ~slug:"phnt-crv" ~community:"phnt-crv-home" Rvs.Reject
      in
      check_notifs "one decision notification" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((m2, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
        ])

let removal_race_case =
  db_case
    "concurrency: two removers produce exactly one deduplicated removal set"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phnt_crm_owner" in
      let* m1 = insert_user conn "phnt_crm_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000042L ~slug:"phnt-crm"
      in
      let* cid = insert_community conn "phnt-crm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-crm" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-crm"
          ~community:"phnt-crm-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "first remover" conn ~actor:owner ~slug:"phnt-crm"
          ~community:"phnt-crm-home"
      in
      let* () =
        remove_expect "second remover" Rms.Removal_unavailable conn ~actor:m1
          ~slug:"phnt-crm" ~community:"phnt-crm-home"
      in
      check_notifs "one removal set" conn ~project
        [
          ((m1, "project_home_requested"), (Some owner, cid, rid));
          ((owner, "project_home_accepted"), (Some m1, cid, rid));
          ((m1, "project_home_removed"), (Some owner, cid, rid));
        ])

let concurrency_suite =
  [ request_race_case; review_race_case; removal_race_case ]

(* ==================== notification UI ====================
   The real notification-center handlers over the real database:
   sql_pool + memory sessions, exactly the production pipeline shape
   the sibling handler suites use. *)

let run_get ~url ?(session = []) ~target handler =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    (* Exactly where production mounts it: inside the pool and the session
       store, immediately outside the route handler. *)
    Earde.Notification_badge.middleware handler req
  in
  pipeline (Dream.request ~method_:`GET ~target "")

let notifications_page_for ~url ~label user_id =
  let* response =
    run_get ~url
      ~session:[ ("user_id", string_of_int user_id) ]
      ~target:"/notifications" Earde.Account_handlers.notifications_handler
  in
  Alcotest.(check int) (label ^ ": page 200") 200 (status_of response);
  let* body = Dream.body response in
  Lwt.return (response, body)

(* The unread count is observed exactly where a user sees it: on the top
   bar of an ordinary authenticated page, rendered by the shared document
   builder from the count the middleware resolved for that request. There
   is no count endpoint to interrogate instead. *)
let badge_probe request =
  Dream.html
    (Earde.Page_shell.launch_app_page ~request ~user:"phnt_probe"
       ~page_class:"launch-feed" ~title:"probe" ~content:Earde.Html.empty ())

let badge_for ~url ~label user_id expected =
  let* response =
    run_get ~url
      ~session:[ ("user_id", string_of_int user_id) ]
      ~target:"/feed" badge_probe
  in
  let* body = Dream.body response in
  (match expected with
  | None ->
      Alcotest.(check bool)
        (label ^ ": no badge element at all")
        false
        (contains body "notif-badge");
      Alcotest.(check bool)
        (label ^ ": bell still rendered")
        true
        (contains body "class='bell'")
  | Some count ->
      Alcotest.(check bool)
        (label ^ ": badge shows " ^ count)
        true
        (contains body
           (Printf.sprintf
              "<span id='notif-badge' class='bell__count'>%s</span>" count)));
  Lwt.return_unit

let check_contains label body needle =
  Alcotest.(check bool) label true (contains body needle)

let check_absent label body needle =
  Alcotest.(check bool) label false (contains body needle)

let ui_labels_case =
  db_case
    "ui: each kind renders its restrained label with the canonical structural \
     link, only for its recipient" (fun ~url conn ->
      let* owner = insert_user conn "phnt_ui_owner" in
      let* owner2 = insert_user conn "phnt_ui_owner2" in
      let* m1 = insert_user conn "phnt_ui_m1" in
      let* other = insert_user conn "phnt_ui_other" in
      let* _inst, project_a =
        make_project conn ~user:owner ~ext_id:960000050L ~slug:"phnt-ui"
      in
      let* _inst2, _project_b =
        make_project conn ~user:owner2 ~ext_id:960000051L ~slug:"phnt-ui2"
      in
      let* () =
        exec conn "name A" q_set_project_name ("Phnt UI Project A", project_a)
      in
      let* () =
        exec conn "name B" q_set_project_name ("Phnt UI Project B", _project_b)
      in
      let* cid = insert_community ~name:"Phnt UI Home" conn "phnt-ui-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      (* Project A: requested → accepted → removed (moderator-side,
         so the steward is the removal recipient). *)
      let* _rid_a =
        request_ok "request A" conn ~user:owner ~slug:"phnt-ui" ~community:cid
      in
      let* () =
        review_ok "accept A" conn ~reviewer:m1 ~slug:"phnt-ui"
          ~community:"phnt-ui-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "remove A" conn ~actor:m1 ~slug:"phnt-ui"
          ~community:"phnt-ui-home"
      in
      (* Project B: requested → rejected. *)
      let* _rid_b =
        request_ok "request B" conn ~user:owner2 ~slug:"phnt-ui2" ~community:cid
      in
      let* () =
        review_ok "reject B" conn ~reviewer:m1 ~slug:"phnt-ui2"
          ~community:"phnt-ui-home" Rvs.Reject Phr.Rejected
      in
      (* The moderator sees both request labels with the queue link. *)
      let* _, body = notifications_page_for ~url ~label:"moderator" m1 in
      check_contains "requested label A" body
        "Phnt UI Project A requested Phnt UI Home as its community home";
      check_contains "requested label B" body
        "Phnt UI Project B requested Phnt UI Home as its community home";
      check_contains "queue link" body
        "href='/c/phnt-ui-home/project-home-requests'";
      (* The requester of A sees acceptance and removal with the
         permanent request-home link. *)
      let* _, body = notifications_page_for ~url ~label:"steward A" owner in
      check_contains "accepted label" body
        "Phnt UI Home accepted the community-home request for Phnt UI Project A";
      check_contains "removed label" body
        "Phnt UI Project A is no longer connected to Phnt UI Home as its home";
      check_contains "request-home link" body
        "href='/projects/phnt-ui/request-home'";
      check_absent "no self requested notification" body
        "Phnt UI Project A requested Phnt UI Home";
      (* The requester of B sees the rejection. *)
      let* _, body = notifications_page_for ~url ~label:"steward B" owner2 in
      check_contains "rejected label" body
        "Phnt UI Home rejected the community-home request for Phnt UI Project B";
      check_contains "request-home link B" body
        "href='/projects/phnt-ui2/request-home'";
      check_absent "no cross-project leakage" body "Phnt UI Project A";
      (* A bystander sees none of it. *)
      let* _, body = notifications_page_for ~url ~label:"other" other in
      check_absent "no labels for a non-recipient" body "Phnt UI";
      check_contains "empty inbox" body "No notifications yet.";
      Lwt.return_unit)

let ui_unread_case =
  db_case
    "ui: creation leaves the notification unread, the badge counts it, and the \
     page load marks it read" (fun ~url conn ->
      let* owner = insert_user conn "phnt_uiu_owner" in
      let* m1 = insert_user conn "phnt_uiu_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000052L ~slug:"phnt-uiu"
      in
      let* cid = insert_community conn "phnt-uiu-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* () = badge_for ~url ~label:"before" m1 None in
      let* _rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-uiu" ~community:cid
      in
      (* Created unread, counted by the badge, invisible to others. *)
      let* unread = find conn "unread flag" q_all_unread project in
      Alcotest.(check bool) "created unread" true unread;
      let* () = badge_for ~url ~label:"recipient" m1 (Some "1") in
      let* () = badge_for ~url ~label:"non-recipient" owner None in
      (* The page load persists the read state. *)
      let* _, _body = notifications_page_for ~url ~label:"page" m1 in
      let* () = badge_for ~url ~label:"after page" m1 None in
      let* unread = find conn "read persisted" q_all_unread project in
      Alcotest.(check bool) "no longer unread" false unread;
      Lwt.return_unit)

let ui_escaping_case =
  db_case "ui: hostile project and community names render escaped"
    (fun ~url conn ->
      let* owner = insert_user conn "phnt_uie_owner" in
      let* m1 = insert_user conn "phnt_uie_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000053L ~slug:"phnt-uie"
      in
      let* () =
        exec conn "hostile project name" q_set_project_name
          ("Phnt <b>bold</b> & 'quoted'", project)
      in
      let* cid =
        insert_community ~name:"Phnt <script>alert(1)</script> Home" conn
          "phnt-uie-home"
      in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* _rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-uie" ~community:cid
      in
      let* _, body = notifications_page_for ~url ~label:"recipient" m1 in
      check_absent "no raw script tag" body "<script>alert(1)</script>";
      check_absent "no raw bold tag" body "<b>bold</b>";
      check_contains "escaped community name" body
        "Phnt &lt;script&gt;alert(1)&lt;/script&gt; Home";
      check_contains "escaped project name" body
        "Phnt &lt;b&gt;bold&lt;/b&gt; &amp; &#39;quoted&#39;";
      Lwt.return_unit)

let ui_deleted_actor_case =
  db_case "ui: a deleted actor renders safely without inventing a name"
    (fun ~url conn ->
      let* owner = insert_user conn "phnt_uid_owner" in
      let* m1 = insert_user conn "phnt_uid_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000054L ~slug:"phnt-uid"
      in
      let* () =
        exec conn "project name" q_set_project_name ("Phnt UID Project", project)
      in
      let* cid = insert_community ~name:"Phnt UID Home" conn "phnt-uid-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* _rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-uid" ~community:cid
      in
      let* () = exec conn "delete actor" q_delete_user owner in
      let* _, body = notifications_page_for ~url ~label:"recipient" m1 in
      check_contains "label survives actor deletion" body
        "Phnt UID Project requested Phnt UID Home as its community home";
      check_absent "no deleted username invented" body "phnt_uid_owner";
      Lwt.return_unit)

let ui_destination_protection_case =
  db_case
    "ui: the queue destination stays behind its own authorization for a mere \
     recipient or requester" (fun ~url conn ->
      let* owner = insert_user conn "phnt_uip_owner" in
      let* m1 = insert_user conn "phnt_uip_m1" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:960000055L ~slug:"phnt-uip"
      in
      let* cid = insert_community conn "phnt-uip-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* _rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-uip" ~community:cid
      in
      let queue_router =
        Dream.router
          [
            Dream.get "/c/:slug/project-home-requests"
              (Home_review_fixture.make_queue ~mode:Ob.Public);
          ]
      in
      (* The requester holds the notification's destination link but
         no moderator authority: the queue collapses to the generic
         404. *)
      let* response =
        run_get ~url
          ~session:[ ("user_id", string_of_int owner) ]
          ~target:"/c/phnt-uip-home/project-home-requests" queue_router
      in
      Alcotest.(check int) "requester: generic 404" 404 (status_of response);
      (* The moderator recipient passes. *)
      let* response =
        run_get ~url
          ~session:[ ("user_id", string_of_int m1) ]
          ~target:"/c/phnt-uip-home/project-home-requests" queue_router
      in
      Alcotest.(check int) "moderator: 200" 200 (status_of response);
      Lwt.return_unit)

(* Removal is the one kind notified to BOTH sides (every steward and
   every top moderator, minus the actor), so its destination must be
   one every recipient can actually open. A steward-only project route
   would 404 for the moderators — the regression this case pins. *)
let ui_removal_destination_case =
  db_case
    "ui: a steward-side removal gives the moderator recipient a destination \
     its own authorization admits" (fun ~url conn ->
      let* owner = insert_user conn "phnt_uir_owner" in
      let* m1 = insert_user conn "phnt_uir_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000056L ~slug:"phnt-uir"
      in
      let* () =
        exec conn "project name" q_set_project_name ("Phnt UIR Project", project)
      in
      let* cid = insert_community ~name:"Phnt UIR Home" conn "phnt-uir-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      let* _rid =
        request_ok "request" conn ~user:owner ~slug:"phnt-uir" ~community:cid
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-uir"
          ~community:"phnt-uir-home" Rvs.Accept Phr.Accepted
      in
      (* The STEWARD removes, so the top moderator is the recipient. *)
      let* () =
        remove_ok "steward-side removal" conn ~actor:owner ~slug:"phnt-uir"
          ~community:"phnt-uir-home"
      in
      let* _, body = notifications_page_for ~url ~label:"moderator" m1 in
      check_contains "removed label" body
        "Phnt UIR Project is no longer connected to Phnt UIR Home as its home";
      check_contains "community destination" body "href='/c/phnt-uir-home'";
      (* The steward-only project route must not be handed to a
         moderator who is not one of the project's stewards. *)
      check_absent "no steward-only destination" body
        "href='/projects/phnt-uir/request-home'";
      (* And the destination really is open to that recipient. *)
      let community_router =
        Dream.router
          [
            Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
          ]
      in
      let* response =
        run_get ~url
          ~session:[ ("user_id", string_of_int m1) ]
          ~target:"/c/phnt-uir-home" community_router
      in
      Alcotest.(check int) "moderator: destination 200" 200 (status_of response);
      Lwt.return_unit)

let ui_suite =
  [
    ui_labels_case;
    ui_unread_case;
    ui_escaping_case;
    ui_deleted_actor_case;
    ui_destination_protection_case;
    ui_removal_destination_case;
  ]

(* ==================== privacy sweep ==================== *)

let privacy_case =
  db_case
    "privacy: no note, credential, email, username, or GitHub identifier \
     reaches notification storage, HTML, or headers" (fun ~url conn ->
      let* owner = insert_user conn "phnt_priv_owner" in
      let* m1 = insert_user conn "phnt_priv_m1" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:960000060L ~slug:"phnt-priv"
      in
      let* () =
        exec conn "project name" q_set_project_name
          ("Phnt Priv Project", project)
      in
      let* cid =
        insert_community ~name:"Phnt Priv Home" conn "phnt-priv-home"
      in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator
          (m1, cid, "top_mod")
      in
      (* A deliberately credential-shaped private note travels through
         the real request; none of it may reach notification storage
         or rendering. *)
      let note =
        "PHNT_NOTE_SECRET_MARKER gho_PHNT_ACCESS_TOKEN \
         PHNT_SESSION_COOKIE_VALUE"
      in
      let relation =
        Home_request_fixture.phr_expect_ok
          (Phr.create_pending ~request_note:(Some note))
      in
      let* _created =
        Home_request_fixture.create_ok "request with note" conn ~user:owner
          ~slug:"phnt-priv" ~community:cid relation
      in
      let* () =
        review_ok "accept" conn ~reviewer:m1 ~slug:"phnt-priv"
          ~community:"phnt-priv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        remove_ok "remove" conn ~actor:owner ~slug:"phnt-priv"
          ~community:"phnt-priv-home"
      in
      let markers =
        [
          ("request note", "PHNT_NOTE_SECRET_MARKER");
          ("access token shape", "gho_");
          ("session value", "PHNT_SESSION_COOKIE_VALUE");
          ("username", "phnt_priv_owner");
          ("email", "@test.invalid");
          ("github namespace login", "pfin-owner");
        ]
      in
      let* blob = find conn "notification blob" q_notif_blob project in
      List.iter
        (fun (label, marker) ->
          Alcotest.(check bool)
            ("no " ^ label ^ " in notification storage")
            false (contains blob marker))
        markers;
      (* The recipient's rendered page: the public project and
         community identity only. *)
      let* response, body = notifications_page_for ~url ~label:"recipient" m1 in
      check_contains "public project identity" body "Phnt Priv Project";
      List.iter
        (fun (label, marker) ->
          Alcotest.(check bool)
            ("no " ^ label ^ " in notification HTML")
            false (contains body marker))
        markers;
      let headers =
        String.concat "|"
          (List.concat_map
             (fun (k, v) -> [ k; v ])
             (Dream.all_headers response))
      in
      List.iter
        (fun (label, marker) ->
          Alcotest.(check bool)
            ("no " ^ label ^ " in response headers")
            false (contains headers marker))
        markers;
      Lwt.return_unit)

let privacy_suite = [ privacy_case ]

let suites =
  [
    ("project_home_notifications_schema", schema_suite);
    ("project_home_notifications_recipients", recipient_suite);
    ("project_home_notifications_rollback", rollback_suite);
    ("project_home_notifications_no_notification", no_notification_suite);
    ("project_home_notifications_concurrency", concurrency_suite);
    ("project_home_notifications_ui", ui_suite);
    ("project_home_notifications_privacy", privacy_suite);
  ]
