module Phr = Earde.Project_home_relation
module Ncpf = Earde.Network_community_publication_form

(* === Project-home lifecycle audit
   (Project_home_audit + project_home_audit_events) ===
   The append-only audit trail of the six home-lifecycle transitions:
   the schema itself (closed action vocabulary, required subject FKs,
   SET NULL actor retention, RESTRICT-protected subjects, the four
   listing indexes, the down/up round-trip, and the absence of any
   private or credential-shaped column), exactly one event per
   successful store transition through the real production stores, no
   event on any refused/failed/replayed path, whole-mutation rollback
   when the audit insertion itself fails, event uniqueness under
   concurrent attempts, and actor deletion/privacy. Fixtures use the
   reserved external-installation-id range 958000001..958000999 (hence
   namespace ids 958100001..958100999), phau_% usernames, and phau-%
   community slugs so no suite shares fixtures. Events come only
   through the real stores except in the schema suite, whose raw rows
   probe the table's own constraints. Every per-case wrapper
   disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rq = Earde.Project_home_request_store

module Rvs = Earde.Project_home_review_store

module Rms = Earde.Project_home_removal_store

module Pv = Earde.Project_home_provisioning_store

module Pub = Earde.Network_community_publication_store

let or_fail = Db_fixture.or_fail

let reject = Db_fixture.reject

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let make_project = Home_provisioning_fixture.make_project

let insert_community = Community_fixture.insert_community

let make_draft = Network_community_fixture.make_draft

let status_str = Phr.string_of_status

let contains haystack needle = Html_assert.occurs haystack ~needle

(* Failure-injection DDL is dropped first so a crashed case can never
   leave a trigger behind; audit events must go before projects and
   communities because their subject FKs deliberately RESTRICT-protect
   both. Then the shared dependency order of the sibling suites. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DROP TRIGGER IF EXISTS phau_fail_audit ON project_home_audit_events"
    ; "DROP TRIGGER IF EXISTS phau_fail_business ON community_projects"
    ; "DROP FUNCTION IF EXISTS phau_fail_fn()"
    ; "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 958100001 AND 958100999)"
    ; "DELETE FROM project_home_audit_events \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'phau-%')"
    ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 958100001 AND 958100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 958000001 AND 958000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phau-%'"
    ; "DELETE FROM users WHERE username LIKE 'phau_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 958000001 AND 958000999"
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

(* The complete first event row as one text signature, for
   append-only/immutability assertions across later transitions. *)
let q_first_event_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT COALESCE((SELECT e::text FROM project_home_audit_events e \
                    WHERE e.project_id = $1 ORDER BY e.id LIMIT 1), \
                   '<none>')"

(* Every stored byte of a project's events, for the boolean
   credential-absence sweep. *)
let q_audit_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(e::text, '|'), '<none>') \
   FROM project_home_audit_events e WHERE e.project_id = $1"

(* === schema fixtures (raw rows: the table's own constraints are the
   subject here, so events do not go through the stores) === *)

let q_insert_project_raw =
  (Caqti_type.(t2 string int64) ->! Caqti_type.int64)
  "INSERT INTO open_source_projects \
     (name, slug, kind, forge_namespace_id, forge_namespace_login, \
      forge_namespace_type) \
   VALUES ('Phau fixture', $1, 'project', $2, 'phau-raw-owner', 'user') \
   RETURNING id"

let q_insert_relation_raw =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, reviewed_at) \
   VALUES ($1, $2, 'home', 'accepted', NOW()) RETURNING id"

let q_insert_event_raw =
  (Caqti_type.(t2 (t2 string (option int)) (t3 int64 int int64))
   ->! Caqti_type.int64)
  "INSERT INTO project_home_audit_events \
     (action, actor_user_id, project_id, community_id, relation_id) \
   VALUES ($1, $2, $3, $4, $5) RETURNING id"

(* NULL-capable subject columns, for the required-reference probes. *)
let q_insert_event_nullable =
  (Caqti_type.(t2 (t2 string (option int))
                  (t3 (option int64) (option int) (option int64)))
   ->! Caqti_type.int64)
  "INSERT INTO project_home_audit_events \
     (action, actor_user_id, project_id, community_id, relation_id) \
   VALUES ($1, $2, $3, $4, $5) RETURNING id"

let q_event_row =
  (Caqti_type.int64
   ->! Caqti_type.(t2 (t2 string (option int)) (t3 int64 int int64)))
  "SELECT action, actor_user_id, project_id, community_id, relation_id \
   FROM project_home_audit_events WHERE id = $1"

let q_event_exists =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_home_audit_events WHERE id = $1"

let q_delete_event =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM project_home_audit_events WHERE id = $1"

let q_delete_project_raw =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM open_source_projects WHERE id = $1"

let q_delete_community_raw =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM communities WHERE id = $1"

let q_delete_relation_raw =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM community_projects WHERE id = $1"

(* === schema signatures === *)

let q_constraints =
  (Caqti_type.unit ->* Caqti_type.(t3 string string bool))
  "SELECT conname, contype::text, convalidated FROM pg_constraint \
   WHERE conrelid = 'project_home_audit_events'::regclass \
   ORDER BY conname"

let q_fk_deltypes =
  (Caqti_type.unit ->* Caqti_type.(t2 string string))
  "SELECT conname, confdeltype::text FROM pg_constraint \
   WHERE conrelid = 'project_home_audit_events'::regclass \
     AND contype = 'f' \
   ORDER BY conname"

let q_indexdefs =
  (Caqti_type.unit ->* Caqti_type.string)
  "SELECT indexdef FROM pg_indexes \
   WHERE schemaname = 'public' \
     AND tablename = 'project_home_audit_events' \
   ORDER BY indexname"

(* The only non-numeric, non-timestamp column may be the closed action
   vocabulary itself: no note, no email, no username, no free text. *)
let q_text_columns =
  (Caqti_type.unit ->* Caqti_type.string)
  "SELECT column_name FROM information_schema.columns \
   WHERE table_schema = 'public' \
     AND table_name = 'project_home_audit_events' \
     AND data_type NOT IN ('integer', 'bigint', \
                           'timestamp with time zone') \
   ORDER BY column_name"

let q_credential_columns =
  (Caqti_type.unit ->* Caqti_type.string)
  "SELECT column_name FROM information_schema.columns \
   WHERE table_schema = 'public' \
     AND table_name = 'project_home_audit_events' \
     AND (column_name ~* 'token' OR column_name ~* 'secret' \
          OR column_name ~* 'verifier' OR column_name ~* 'oauth' \
          OR column_name ~* 'session' OR column_name ~* 'note' \
          OR column_name ~* 'email' OR column_name ~* 'username' \
          OR column_name ~* 'login' OR column_name ~* 'json' \
          OR column_name ~* 'github' OR column_name ~* 'installation' \
          OR column_name ~* 'account' OR column_name ~* 'namespace')"

let q_table_exists =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COUNT(*) FROM pg_class \
   WHERE relname = 'project_home_audit_events' AND relkind = 'r'"

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_drop_table = ddl "DROP TABLE project_home_audit_events"

(* Byte-for-byte the migration's up statements, for the transactional
   round-trip below. *)
let q_recreate_table =
  ddl
    "CREATE TABLE project_home_audit_events ( \
       id            BIGSERIAL   PRIMARY KEY, \
       action        TEXT        NOT NULL \
                                 CHECK (action IN ('project_home_requested', \
                                                   'project_home_accepted', \
                                                   'project_home_rejected', \
                                                   'project_home_removed', \
                                                   'dedicated_home_provisioned', \
                                                   'network_community_published')), \
       actor_user_id INTEGER     REFERENCES users(id) ON DELETE SET NULL, \
       project_id    BIGINT      NOT NULL REFERENCES open_source_projects(id), \
       community_id  INTEGER     NOT NULL REFERENCES communities(id), \
       relation_id   BIGINT      NOT NULL REFERENCES community_projects(id), \
       created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW() \
     )"

let q_recreate_indexes =
  List.map ddl
    [ "CREATE INDEX idx_project_home_audit_events_project \
       ON project_home_audit_events (project_id, created_at DESC)"
    ; "CREATE INDEX idx_project_home_audit_events_community \
       ON project_home_audit_events (community_id, created_at DESC)"
    ; "CREATE INDEX idx_project_home_audit_events_relation \
       ON project_home_audit_events (relation_id, created_at DESC)"
    ; "CREATE INDEX idx_project_home_audit_events_actor \
       ON project_home_audit_events (actor_user_id, created_at DESC)"
    ]

(* === failure injection (installed and dropped per case) === *)

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION phau_fail_fn() RETURNS trigger \
     LANGUAGE plpgsql \
     AS 'BEGIN RAISE EXCEPTION ''phau fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS phau_fail_fn()"

let q_poison_audit =
  ddl
    "CREATE TRIGGER phau_fail_audit \
     BEFORE INSERT ON project_home_audit_events \
     FOR EACH ROW EXECUTE FUNCTION phau_fail_fn()"

let q_unpoison_audit =
  ddl "DROP TRIGGER IF EXISTS phau_fail_audit ON project_home_audit_events"

let q_poison_relation_insert =
  ddl
    "CREATE TRIGGER phau_fail_business \
     AFTER INSERT ON community_projects \
     FOR EACH ROW EXECUTE FUNCTION phau_fail_fn()"

let q_poison_relation_update =
  ddl
    "CREATE TRIGGER phau_fail_business \
     AFTER UPDATE ON community_projects \
     FOR EACH ROW EXECUTE FUNCTION phau_fail_fn()"

let q_unpoison_business =
  ddl "DROP TRIGGER IF EXISTS phau_fail_business ON community_projects"

let with_poison conn ~install ~remove f =
  let* () = exec conn "create fail fn" q_create_fail_fn () in
  Lwt.finalize
    (fun () ->
      let* () = exec conn "install poison trigger" install () in
      Lwt.finalize f
        (fun () -> exec conn "drop poison trigger" remove ()))
    (fun () -> exec conn "drop fail fn" q_drop_fail_fn ())

(* ==================== schema suite ==================== *)

let raw_fixture conn ~pslug ~cslug ~ext =
  let* actor = insert_user conn ("phau_raw_" ^ pslug) in
  let* project = find conn "raw project" q_insert_project_raw
                   (("phau-" ^ pslug), ext) in
  let* cid = insert_community conn ("phau-" ^ cslug) in
  let* rid = find conn "raw relation" q_insert_relation_raw (project, cid) in
  Lwt.return (actor, project, cid, rid)

let actions_case =
  db_case "schema: exactly the six closed actions are accepted; anything \
           else is rejected" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-act" ~cslug:"sch-act-home"
          ~ext:958100001L
      in
      let* () =
        Lwt_list.iter_s
          (fun action ->
            let* id =
              C.find q_insert_event_raw
                ((action, Some actor), (project, cid, rid))
            in
            let* id = or_fail ("accept " ^ action) id in
            Alcotest.(check bool) (action ^ ": positive id") true
              (Int64.compare id 0L > 0);
            Lwt.return_unit)
          [ "project_home_requested"; "project_home_accepted";
            "project_home_rejected"; "project_home_removed";
            "dedicated_home_provisioned"; "network_community_published"
          ]
      in
      Lwt_list.iter_s
        (fun action ->
          let* r =
            C.find q_insert_event_raw
              ((action, Some actor), (project, cid, rid))
          in
          reject ("reject " ^ action) r)
        [ "made_up_action"; "PROJECT_HOME_REQUESTED"; "";
          " project_home_requested"; "home_requested"
        ])

let required_references_case =
  db_case "schema: project, community, and relation references are \
           required and must resolve; the actor may be NULL" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-ref" ~cslug:"sch-ref-home"
          ~ext:958100002L
      in
      (* NULL subjects: NOT NULL refuses each. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, p, c, r) ->
            let* res =
              C.find q_insert_event_nullable
                (("project_home_requested", Some actor), (p, c, r))
            in
            reject label res)
          [ ("null project", None, Some cid, Some rid);
            ("null community", Some project, None, Some rid);
            ("null relation", Some project, Some cid, None)
          ]
      in
      (* Dangling subjects and a dangling actor: the FKs refuse each. *)
      let* ghost_p = find conn "ghost project" Home_audit_fixture.q_absent_project_id () in
      let* ghost_c = find conn "ghost community" Home_audit_fixture.q_absent_community_id () in
      let* ghost_r = find conn "ghost relation" Home_audit_fixture.q_absent_relation_id () in
      let* ghost_u = find conn "ghost user" Home_audit_fixture.q_absent_user_id () in
      let* () =
        Lwt_list.iter_s
          (fun (label, a, p, c, r) ->
            let* res =
              C.find q_insert_event_raw
                (("project_home_requested", a), (p, c, r))
            in
            reject label res)
          [ ("dangling project", Some actor, ghost_p, cid, rid);
            ("dangling community", Some actor, project, ghost_c, rid);
            ("dangling relation", Some actor, project, cid, ghost_r);
            ("dangling actor", Some ghost_u, project, cid, rid)
          ]
      in
      (* A NULL actor is the legal retention shape. *)
      let* id =
        C.find q_insert_event_raw
          (("project_home_requested", None), (project, cid, rid))
      in
      let* id = or_fail "null actor accepted" id in
      Alcotest.(check bool) "null actor: positive id" true
        (Int64.compare id 0L > 0);
      Lwt.return_unit)

let actor_deletion_case =
  db_case "schema: deleting the actor nulls the reference and the event \
           survives untouched" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-del" ~cslug:"sch-del-home"
          ~ext:958100003L
      in
      let* event =
        C.find q_insert_event_raw
          (("project_home_accepted", Some actor), (project, cid, rid))
      in
      let* event = or_fail "event" event in
      let* () = exec conn "delete actor" Home_audit_fixture.q_delete_user actor in
      let* row = find conn "event row" q_event_row event in
      Alcotest.(check
                  (pair (pair string (option int))
                     (triple int64 int int64)))
        "event survives with a NULL actor and intact subjects"
        (("project_home_accepted", None), (project, cid, rid))
        (let (action_actor, (p, c, r)) = row in
         (action_actor, (p, c, r)));
      Lwt.return_unit)

let subject_protection_case =
  db_case "schema: audit history RESTRICT-protects project, community, \
           and relation until explicitly purged" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* actor, project, cid, rid =
        raw_fixture conn ~pslug:"sch-pro" ~cslug:"sch-pro-home"
          ~ext:958100004L
      in
      let* event =
        C.find q_insert_event_raw
          (("project_home_removed", Some actor), (project, cid, rid))
      in
      let* event = or_fail "event" event in
      (* Each subject deletion is refused while the event exists. The
         relation delete is probed directly; project and community
         deletes are refused through their community_projects
         cascades reaching the same protected relation — and the
         community delete would also trip the community FK itself. *)
      let* r = C.exec q_delete_relation_raw rid in
      let* () = reject "relation delete refused" r in
      let* r = C.exec q_delete_project_raw project in
      let* () = reject "project delete refused" r in
      let* r = C.exec q_delete_community_raw cid in
      let* () = reject "community delete refused" r in
      (* After the explicit purge, ordinary deletion resumes. *)
      let* () = exec conn "purge event" q_delete_event event in
      let* () = exec conn "project delete succeeds" q_delete_project_raw
                  project in
      let* n = find conn "event gone" q_event_exists event in
      Alcotest.(check int) "purged event gone" 0 n;
      Lwt.return_unit)

let constraints_indexes_case =
  db_case "schema: constraints and the four listing indexes are present, \
           validated, and carry the chosen deletion semantics" (fun conn ->
      let* rows = collect conn "constraints" q_constraints () in
      Alcotest.(check (list (triple string string bool)))
        "constraints present and validated"
        [ ("project_home_audit_events_action_check", "c", true);
          ("project_home_audit_events_actor_user_id_fkey", "f", true);
          ("project_home_audit_events_community_id_fkey", "f", true);
          ("project_home_audit_events_pkey", "p", true);
          ("project_home_audit_events_project_id_fkey", "f", true);
          ("project_home_audit_events_relation_id_fkey", "f", true)
        ]
        rows;
      let* deltypes = collect conn "fk deltypes" q_fk_deltypes () in
      Alcotest.(check (list (pair string string)))
        "SET NULL actor, NO ACTION subjects"
        [ ("project_home_audit_events_actor_user_id_fkey", "n");
          ("project_home_audit_events_community_id_fkey", "a");
          ("project_home_audit_events_project_id_fkey", "a");
          ("project_home_audit_events_relation_id_fkey", "a")
        ]
        deltypes;
      let* defs = collect conn "index defs" q_indexdefs () in
      Alcotest.(check (list string))
        "exactly the four listing indexes plus the primary key"
        [ "CREATE INDEX idx_project_home_audit_events_actor ON \
           public.project_home_audit_events USING btree (actor_user_id, \
           created_at DESC)"
        ; "CREATE INDEX idx_project_home_audit_events_community ON \
           public.project_home_audit_events USING btree (community_id, \
           created_at DESC)"
        ; "CREATE INDEX idx_project_home_audit_events_project ON \
           public.project_home_audit_events USING btree (project_id, \
           created_at DESC)"
        ; "CREATE INDEX idx_project_home_audit_events_relation ON \
           public.project_home_audit_events USING btree (relation_id, \
           created_at DESC)"
        ; "CREATE UNIQUE INDEX project_home_audit_events_pkey ON \
           public.project_home_audit_events USING btree (id)"
        ]
        defs;
      Lwt.return_unit)

let column_privacy_case =
  db_case "schema: the closed action is the only textual column and no \
           credential-shaped column exists" (fun conn ->
      let* text_columns = collect conn "text columns" q_text_columns () in
      Alcotest.(check (list string))
        "only the action column is non-numeric" [ "action" ] text_columns;
      let* bad = collect conn "credential columns" q_credential_columns () in
      Alcotest.(check (list string)) "no credential-shaped column" [] bad;
      Lwt.return_unit)

let roundtrip_case =
  db_case "schema: the down/up statement pair round-trips" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      (* The whole round-trip runs inside one transaction that is
         always rolled back, so the live table cannot be lost even if
         an assertion fails between the drop and the re-create. *)
      let* r = C.start () in
      let* () = or_fail "begin" r in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = exec conn "down" q_drop_table () in
            let* n = find conn "absent" q_table_exists () in
            Alcotest.(check int) "down removes the table" 0 n;
            let* () = exec conn "up table" q_recreate_table () in
            let* () =
              Lwt_list.iter_s (fun q -> exec conn "up index" q ()) q_recreate_indexes
            in
            let* n = find conn "present" q_table_exists () in
            Alcotest.(check int) "up restores the table" 1 n;
            let* rows = collect conn "constraints" q_constraints () in
            Alcotest.(check int)
              "up restores all six constraints" 6 (List.length rows);
            Lwt.return_unit)
          (fun () ->
            let* _ = C.rollback () in
            Lwt.return_unit)
      in
      let* n = find conn "live table" q_table_exists () in
      Alcotest.(check int) "live table untouched" 1 n;
      Lwt.return_unit)

let schema_suite =
  [ actions_case; required_references_case; actor_deletion_case;
    subject_protection_case; constraints_indexes_case; column_privacy_case;
    roundtrip_case
  ]

(* ============ one event per successful transition ============ *)

let request_event_case =
  db_case "events: a successful request appends exactly one requested \
           event with the exact actor and subjects" (fun conn ->
      let* owner = insert_user conn "phau_req_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000001L ~slug:"phau-req"
      in
      let* cid = insert_community conn "phau-req-home" in
      let* () = Home_audit_fixture.check_event_count "before" conn ~project 0 in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-req"
          ~community:cid
      in
      Home_audit_fixture.check_events "request" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let accept_event_case =
  db_case "events: a moderator acceptance appends exactly one accepted \
           event beside the untouched request event" (fun conn ->
      let* owner = insert_user conn "phau_acc_owner" in
      let* moderator = insert_user conn "phau_acc_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000002L ~slug:"phau-acc"
      in
      let* cid = insert_community conn "phau-acc-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-acc"
          ~community:cid
      in
      let* first_sig = find conn "first sig" q_first_event_sig project in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-acc"
          ~community:"phau-acc-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        Home_audit_fixture.check_events "accept" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid))
          ]
      in
      let* first_sig_after = find conn "first sig after" q_first_event_sig
                               project in
      Alcotest.(check string)
        "the earlier event is byte-unchanged by the later transition"
        first_sig first_sig_after;
      Lwt.return_unit)

let reject_event_case =
  db_case "events: a durable-admin rejection appends exactly one rejected \
           event" (fun conn ->
      let* owner = insert_user conn "phau_rej_owner" in
      let* admin = insert_user conn "phau_rej_admin" in
      let* () = exec conn "make admin" Community_fixture.q_set_admin (admin, true) in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000003L ~slug:"phau-rej"
      in
      let* cid = insert_community conn "phau-rej-home" in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-rej"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "reject" conn ~reviewer:admin ~slug:"phau-rej"
          ~community:"phau-rej-home" Rvs.Reject Phr.Rejected
      in
      Home_audit_fixture.check_events "reject" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid));
          (("project_home_rejected", Some admin), (cid, rid))
        ])

let removal_event_case =
  db_case "events: a removal appends exactly one removed event and every \
           earlier event survives byte-identical" (fun conn ->
      let* owner = insert_user conn "phau_rem_owner" in
      let* moderator = insert_user conn "phau_rem_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000004L ~slug:"phau-rem"
      in
      let* cid = insert_community conn "phau-rem-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-rem"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-rem"
          ~community:"phau-rem-home" Rvs.Accept Phr.Accepted
      in
      let* first_sig = find conn "first sig" q_first_event_sig project in
      let* () =
        Home_audit_fixture.remove_ok "remove" conn ~actor:owner ~slug:"phau-rem"
          ~community:"phau-rem-home"
      in
      let* () =
        Home_audit_fixture.check_events "remove" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid));
            (("project_home_removed", Some owner), (cid, rid))
          ]
      in
      let* first_sig_after = find conn "first sig after" q_first_event_sig
                               project in
      Alcotest.(check string)
        "the request event is byte-unchanged across the whole lifecycle"
        first_sig first_sig_after;
      Lwt.return_unit)

let provision_event_case =
  db_case "events: dedicated provisioning appends exactly one provisioned \
           event for the new community and relation" (fun conn ->
      let* owner = insert_user conn "phau_prv_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000005L
          ~project_slug:"phau-prv" ~slug:"phau-prv-home"
      in
      Home_audit_fixture.check_events "provision" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid)) ])

let publication_event_case =
  db_case "events: publication appends exactly one published event beside \
           the provisioned one" (fun conn ->
      let* owner = insert_user conn "phau_pub_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000006L
          ~project_slug:"phau-pub" ~slug:"phau-pub-home"
      in
      let* _ =
        Network_community_fixture.publish_ok "publish" conn ~actor:owner
          ~current:"phau-pub-home" ~expect_slug:"phau-pub-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name:"Phau Pub Hub" ~slug:"phau-pub-hub" ())
      in
      Home_audit_fixture.check_events "publish" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid));
          (("network_community_published", Some owner), (cid, rid))
        ])

let event_suite =
  [ request_event_case; accept_event_case; reject_event_case;
    removal_event_case; provision_event_case; publication_event_case
  ]

(* ================= no event on any failure ================= *)

let request_failures_case =
  db_case "no event: every refused request path leaves the audit history \
           empty or unchanged" (fun conn ->
      let* owner = insert_user conn "phau_frq_owner" in
      let* outsider = insert_user conn "phau_frq_outsider" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000010L ~slug:"phau-frq"
      in
      let* cid = insert_community conn "phau-frq-home" in
      (* Invalid pure input. *)
      let* () =
        Home_request_fixture.create_expect "invalid user" Rq.Invalid_user_id conn ~user:0
          ~slug:"phau-frq" ~community:cid (Home_request_fixture.phr_fresh_pending ())
      in
      (* Unavailable project (missing slug) and community (absent id). *)
      let* () =
        Home_request_fixture.create_expect "missing project" Rq.Project_unavailable conn
          ~user:owner ~slug:"phau-frq-none" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      let* ghost_c = find conn "ghost community" Home_audit_fixture.q_absent_community_id () in
      let* () =
        Home_request_fixture.create_expect "missing community" Rq.Community_unavailable
          conn ~user:owner ~slug:"phau-frq" ~community:ghost_c
          (Home_request_fixture.phr_fresh_pending ())
      in
      (* Unauthorized actor: not a steward. *)
      let* () =
        Home_request_fixture.create_expect "outsider" Rq.Project_unavailable conn
          ~user:outsider ~slug:"phau-frq" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      let* () = Home_audit_fixture.check_event_count "only failures so far" conn ~project 0 in
      (* Duplicate: the second request finds the active-home slot taken. *)
      let* rid =
        Home_audit_fixture.request_ok "winner" conn ~user:owner ~slug:"phau-frq"
          ~community:cid
      in
      let* () =
        Home_request_fixture.create_expect "duplicate" Rq.Active_home_exists conn
          ~user:owner ~slug:"phau-frq" ~community:cid
          (Home_request_fixture.phr_fresh_pending ())
      in
      Home_audit_fixture.check_events "after duplicate" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let review_failures_case =
  db_case "no event: every refused review path leaves exactly the request \
           event" (fun conn ->
      let* owner = insert_user conn "phau_frv_owner" in
      let* moderator = insert_user conn "phau_frv_mod" in
      let* plain = insert_user conn "phau_frv_plain" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000011L ~slug:"phau-frv"
      in
      let* cid = insert_community conn "phau-frv-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      (* No pending request yet. *)
      let* () =
        Home_audit_fixture.review_expect "nothing to review" Rvs.Review_unavailable conn
          ~reviewer:moderator ~slug:"phau-frv" ~community:"phau-frv-home"
          Rvs.Accept
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-frv"
          ~community:cid
      in
      let requested_only label =
        Home_audit_fixture.check_events label conn ~project
          [ (("project_home_requested", Some owner), (cid, rid)) ]
      in
      (* Unauthorized reviewer. *)
      let* () =
        Home_audit_fixture.review_expect "plain user" Rvs.Reviewer_unauthorized conn
          ~reviewer:plain ~slug:"phau-frv" ~community:"phau-frv-home"
          Rvs.Accept
      in
      let* () = requested_only "after unauthorized" in
      (* Acceptance refused on an ineligible target: the legal
         unpublished-draft lifecycle, which the request may outlive. *)
      let* () = exec conn "go draft" Community_fixture.q_make_draft_state cid in
      let* () =
        Home_audit_fixture.review_expect "ineligible target" Rvs.Target_ineligible conn
          ~reviewer:moderator ~slug:"phau-frv" ~community:"phau-frv-home"
          Rvs.Accept
      in
      let* () = exec conn "restore eligibility" Home_review_fixture.q_make_eligible cid in
      let* () = requested_only "after ineligible" in
      (* Acceptance refused while verification is revoked. *)
      let* () =
        exec conn "revoke" Home_request_fixture.q_set_verification (project, "revoked")
      in
      let* () =
        Home_audit_fixture.review_expect "revoked project" Rvs.Project_unavailable conn
          ~reviewer:moderator ~slug:"phau-frv" ~community:"phau-frv-home"
          Rvs.Accept
      in
      let* () =
        exec conn "restore verification" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* () = requested_only "after revoked" in
      (* Replay: a second decision finds nothing pending. *)
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-frv"
          ~community:"phau-frv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        Home_audit_fixture.review_expect "replay" Rvs.Review_unavailable conn
          ~reviewer:moderator ~slug:"phau-frv" ~community:"phau-frv-home"
          Rvs.Accept
      in
      Home_audit_fixture.check_events "after replay" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid));
          (("project_home_accepted", Some moderator), (cid, rid))
        ])

let removal_failures_case =
  db_case "no event: refused, replayed, and draft-protected removals \
           append nothing" (fun conn ->
      let* owner = insert_user conn "phau_frm_owner" in
      let* moderator = insert_user conn "phau_frm_mod" in
      let* plain = insert_user conn "phau_frm_plain" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000012L ~slug:"phau-frm"
      in
      let* cid = insert_community conn "phau-frm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-frm"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-frm"
          ~community:"phau-frm-home" Rvs.Accept Phr.Accepted
      in
      (* Unauthorized remover. *)
      let* () =
        Home_audit_fixture.remove_expect "unauthorized" Rms.Actor_unauthorized conn
          ~actor:plain ~slug:"phau-frm" ~community:"phau-frm-home"
      in
      let* () =
        Home_audit_fixture.check_events "after unauthorized" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid))
          ]
      in
      (* Success, then replay. *)
      let* () =
        Home_audit_fixture.remove_ok "remove" conn ~actor:moderator ~slug:"phau-frm"
          ~community:"phau-frm-home"
      in
      let* () =
        Home_audit_fixture.remove_expect "replay" Rms.Removal_unavailable conn
          ~actor:moderator ~slug:"phau-frm" ~community:"phau-frm-home"
      in
      let* () =
        Home_audit_fixture.check_events "after replay" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid));
            (("project_home_removed", Some moderator), (cid, rid))
          ]
      in
      (* Protected unpublished-draft removal on a second project. *)
      let* owner2 = insert_user conn "phau_frm_owner2" in
      let* project2, cid2, rid2 =
        make_draft conn ~user:owner2 ~ext_id:958000013L
          ~project_slug:"phau-frm2" ~slug:"phau-frm2-home"
      in
      let* () =
        Home_audit_fixture.remove_expect "protected draft" Rms.Removal_unavailable conn
          ~actor:owner2 ~slug:"phau-frm2" ~community:"phau-frm2-home"
      in
      Home_audit_fixture.check_events "draft protection appends nothing" conn
        ~project:project2
        [ (("dedicated_home_provisioned", Some owner2), (cid2, rid2)) ])

let provisioning_failures_case =
  db_case "no event: slug and active-home conflicts leave provisioning \
           history empty" (fun conn ->
      let* owner = insert_user conn "phau_fpv_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000014L ~slug:"phau-fpv"
      in
      (* Slug conflict with an existing community. *)
      let* _taken = insert_community conn "phau-fpv-taken" in
      let* () =
        Home_provisioning_fixture.provision_expect "slug conflict" Pv.Community_slug_unavailable
          conn ~actor:owner ~slug:"phau-fpv"
          (Home_provisioning_fixture.identity ~name:"Phau Fpv" ~slug:"phau-fpv-taken" ())
      in
      let* () = Home_audit_fixture.check_event_count "after slug conflict" conn ~project 0 in
      (* Active-home conflict: a pending request occupies the slot. *)
      let* cid = insert_community conn "phau-fpv-home" in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-fpv"
          ~community:cid
      in
      let* () =
        Home_provisioning_fixture.provision_expect "active home" Pv.Active_home_exists conn
          ~actor:owner ~slug:"phau-fpv"
          (Home_provisioning_fixture.identity ~name:"Phau Fpv" ~slug:"phau-fpv-fresh" ())
      in
      Home_audit_fixture.check_events "after active-home conflict" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let publication_failures_case =
  db_case "no event: unauthorized, slug-conflicted, and replayed \
           publications append nothing" (fun conn ->
      let* owner = insert_user conn "phau_fpb_owner" in
      let* outsider = insert_user conn "phau_fpb_outsider" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000015L
          ~project_slug:"phau-fpb" ~slug:"phau-fpb-home"
      in
      let provisioned_only label =
        Home_audit_fixture.check_events label conn ~project
          [ (("dedicated_home_provisioned", Some owner), (cid, rid)) ]
      in
      (* Unauthorized publisher. *)
      let* () =
        Network_community_fixture.publish_expect "outsider" Pub.Draft_unavailable conn
          ~actor:outsider ~current:"phau-fpb-home"
          (Network_community_fixture.publication ~name:"Phau Fpb Hub" ~slug:"phau-fpb-hub" ())
      in
      let* () = provisioned_only "after unauthorized" in
      (* Slug conflict with an existing community. *)
      let* _taken = insert_community conn "phau-fpb-taken" in
      let* () =
        Network_community_fixture.publish_expect "slug conflict" Pub.Community_slug_unavailable
          conn ~actor:owner ~current:"phau-fpb-home"
          (Network_community_fixture.publication ~name:"Phau Fpb Hub" ~slug:"phau-fpb-taken" ())
      in
      let* () = provisioned_only "after slug conflict" in
      (* Success, then replay against the published community. *)
      let* _ =
        Network_community_fixture.publish_ok "publish" conn ~actor:owner
          ~current:"phau-fpb-home" ~expect_slug:"phau-fpb-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name:"Phau Fpb Hub" ~slug:"phau-fpb-hub" ())
      in
      let* () =
        Network_community_fixture.publish_expect "replay" Pub.Draft_unavailable conn
          ~actor:owner ~current:"phau-fpb-hub"
          (Network_community_fixture.publication ~name:"Phau Fpb Hub" ~slug:"phau-fpb-hub" ())
      in
      Home_audit_fixture.check_events "after replay" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid));
          (("network_community_published", Some owner), (cid, rid))
        ])

let business_failure_injection_case =
  db_case "no event: a failure at or after the business mutation but \
           before the audit insertion appends nothing" (fun conn ->
      let* owner = insert_user conn "phau_fbz_owner" in
      let* moderator = insert_user conn "phau_fbz_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000016L ~slug:"phau-fbz"
      in
      let* cid = insert_community conn "phau-fbz-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      (* The request's own relation insert fails: the audit layer is
         never reached and nothing persists. *)
      let* () =
        with_poison conn ~install:q_poison_relation_insert
          ~remove:q_unpoison_business (fun () ->
            Home_request_fixture.create_expect "poisoned request" Rq.Storage_error conn
              ~user:owner ~slug:"phau-fbz" ~community:cid
              (Home_request_fixture.phr_fresh_pending ()))
      in
      let* relations = find conn "relations" Home_request_fixture.q_count_for_project
                         project in
      Alcotest.(check int) "no relation" 0 relations;
      let* () = Home_audit_fixture.check_event_count "no event" conn ~project 0 in
      (* The review's guarded update fails after the request committed:
         the request event stays alone and the row stays pending. *)
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-fbz"
          ~community:cid
      in
      let* () =
        with_poison conn ~install:q_poison_relation_update
          ~remove:q_unpoison_business (fun () ->
            Home_audit_fixture.review_expect "poisoned review" Rvs.Storage_error conn
              ~reviewer:moderator ~slug:"phau-fbz"
              ~community:"phau-fbz-home" Rvs.Accept)
      in
      let* () = Home_audit_fixture.check_status "after poisoned review" conn rid "pending" in
      Home_audit_fixture.check_events "request event alone" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let no_event_suite =
  [ request_failures_case; review_failures_case; removal_failures_case;
    provisioning_failures_case; publication_failures_case;
    business_failure_injection_case
  ]

(* ============ audit failure rolls the mutation back ============ *)

let request_rollback_case =
  db_case "rollback: a failed request audit insertion leaves no pending \
           relation" (fun conn ->
      let* owner = insert_user conn "phau_rbq_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000020L ~slug:"phau-rbq"
      in
      let* cid = insert_community conn "phau-rbq-home" in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Home_request_fixture.create_expect "poisoned audit" Rq.Storage_error conn
              ~user:owner ~slug:"phau-rbq" ~community:cid
              (Home_request_fixture.phr_fresh_pending ()))
      in
      let* relations = find conn "relations" Home_request_fixture.q_count_for_project
                         project in
      Alcotest.(check int) "no pending relation" 0 relations;
      let* () = Home_audit_fixture.check_event_count "no event" conn ~project 0 in
      (* The un-poisoned path immediately succeeds whole. *)
      let* rid =
        Home_audit_fixture.request_ok "recovered" conn ~user:owner ~slug:"phau-rbq"
          ~community:cid
      in
      Home_audit_fixture.check_events "recovered" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let review_rollback_case =
  db_case "rollback: a failed review audit insertion leaves the relation \
           pending" (fun conn ->
      let* owner = insert_user conn "phau_rbv_owner" in
      let* moderator = insert_user conn "phau_rbv_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000021L ~slug:"phau-rbv"
      in
      let* cid = insert_community conn "phau-rbv-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-rbv"
          ~community:cid
      in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Home_audit_fixture.review_expect "poisoned accept" Rvs.Storage_error conn
              ~reviewer:moderator ~slug:"phau-rbv"
              ~community:"phau-rbv-home" Rvs.Accept)
      in
      let* () = Home_audit_fixture.check_status "after rollback" conn rid "pending" in
      Home_audit_fixture.check_events "request event alone" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let removal_rollback_case =
  db_case "rollback: a failed removal audit insertion leaves the relation \
           accepted" (fun conn ->
      let* owner = insert_user conn "phau_rbm_owner" in
      let* moderator = insert_user conn "phau_rbm_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000022L ~slug:"phau-rbm"
      in
      let* cid = insert_community conn "phau-rbm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-rbm"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-rbm"
          ~community:"phau-rbm-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Home_audit_fixture.remove_expect "poisoned removal" Rms.Storage_error conn
              ~actor:owner ~slug:"phau-rbm" ~community:"phau-rbm-home")
      in
      let* () = Home_audit_fixture.check_status "after rollback" conn rid "accepted" in
      Home_audit_fixture.check_events "no removal event" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid));
          (("project_home_accepted", Some moderator), (cid, rid))
        ])

(* Provisioning rollback leaves none of the six draft rows behind. *)
let q_draft_leftovers =
  (Caqti_type.(t2 string int64)
   ->! Caqti_type.(t2 (t3 int int int) (t3 int int int)))
  "SELECT \
     (SELECT COUNT(*) FROM communities WHERE slug = $1), \
     (SELECT COUNT(*) FROM community_members WHERE community_id IN \
        (SELECT id FROM communities WHERE slug = $1)), \
     (SELECT COUNT(*) FROM community_moderators WHERE community_id IN \
        (SELECT id FROM communities WHERE slug = $1)), \
     (SELECT COUNT(*) FROM community_sections WHERE community_id IN \
        (SELECT id FROM communities WHERE slug = $1)), \
     (SELECT COUNT(*) FROM channels WHERE community_id IN \
        (SELECT id FROM communities WHERE slug = $1)), \
     (SELECT COUNT(*) FROM community_projects WHERE project_id = $2)"

let provisioning_rollback_case =
  db_case "rollback: a failed provisioning audit insertion leaves no \
           community, membership, moderator, shell, or relation"
    (fun conn ->
      let* owner = insert_user conn "phau_rbp_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000023L ~slug:"phau-rbp"
      in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Home_provisioning_fixture.provision_expect "poisoned provisioning" Pv.Storage_error
              conn ~actor:owner ~slug:"phau-rbp"
              (Home_provisioning_fixture.identity ~name:"Phau Rbp" ~slug:"phau-rbp-home" ()))
      in
      let* leftovers =
        find conn "leftovers" q_draft_leftovers ("phau-rbp-home", project)
      in
      Alcotest.(check (pair (triple int int int) (triple int int int)))
        "the whole draft rolled back" ((0, 0, 0), (0, 0, 0)) leftovers;
      Home_audit_fixture.check_event_count "no event" conn ~project 0)

let publication_rollback_case =
  db_case "rollback: a failed publication audit insertion leaves slug, \
           identity, lifecycle, relation, and shell unchanged" (fun conn ->
      let* owner = insert_user conn "phau_rbb_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000024L
          ~project_slug:"phau-rbb" ~slug:"phau-rbb-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let* () =
        with_poison conn ~install:q_poison_audit ~remove:q_unpoison_audit
          (fun () ->
            Network_community_fixture.publish_expect "poisoned publication" Pub.Storage_error
              conn ~actor:owner ~current:"phau-rbb-home"
              (Network_community_fixture.publication ~name:"Phau Rbb Hub" ~slug:"phau-rbb-hub"
                 ()))
      in
      let* () =
        Network_community_fixture.check_unchanged "after rollback" conn ~cid ~project ~rid
          before
      in
      let* hub = find conn "loser slug" Home_provisioning_fixture.q_count_by_slug
                   "phau-rbb-hub" in
      Alcotest.(check int) "no community under the requested slug" 0 hub;
      Home_audit_fixture.check_events "provisioned event alone" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid)) ])

let rollback_suite =
  [ request_rollback_case; review_rollback_case; removal_rollback_case;
    provisioning_rollback_case; publication_rollback_case
  ]

(* ==================== concurrency ==================== *)

let request_race_case =
  db_case "concurrency: two live identical requests leave one pending \
           relation and exactly one request event" (fun conn ->
      let* owner = insert_user conn "phau_crq_owner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000030L ~slug:"phau-crq"
      in
      let* cid = insert_community conn "phau-crq-home" in
      let* _created =
        Db_fixture.with_second_connection (fun conn2 ->
            let* results =
              Lwt.both
                (Home_request_fixture.create conn ~user:owner ~slug:"phau-crq"
                   ~community:cid (Home_request_fixture.phr_fresh_pending ()))
                (Home_request_fixture.create conn2 ~user:owner ~slug:"phau-crq"
                   ~community:cid (Home_request_fixture.phr_fresh_pending ()))
            in
            Lwt.return
              (Home_request_fixture.ok_and_error "request race" Rq.Active_home_exists
                 results))
      in
      let* rid = Home_audit_fixture.sole_relation "one relation" conn project in
      Home_audit_fixture.check_events "one event" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid)) ])

let review_race_case =
  db_case "concurrency: two reviewers produce exactly one decision event"
    (fun conn ->
      let* owner = insert_user conn "phau_crv_owner" in
      let* mod1 = insert_user conn "phau_crv_mod1" in
      let* mod2 = insert_user conn "phau_crv_mod2" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000031L ~slug:"phau-crv"
      in
      let* cid = insert_community conn "phau-crv-home" in
      let* () =
        exec conn "top mod 1" Community_fixture.q_insert_moderator (mod1, cid, "top_mod")
      in
      let* () =
        exec conn "top mod 2" Community_fixture.q_insert_moderator (mod2, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-crv"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "first reviewer" conn ~reviewer:mod1 ~slug:"phau-crv"
          ~community:"phau-crv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        Home_audit_fixture.review_expect "second reviewer" Rvs.Review_unavailable conn
          ~reviewer:mod2 ~slug:"phau-crv" ~community:"phau-crv-home"
          Rvs.Reject
      in
      Home_audit_fixture.check_events "one decision event" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid));
          (("project_home_accepted", Some mod1), (cid, rid))
        ])

let removal_race_case =
  db_case "concurrency: two removers produce exactly one removal event"
    (fun conn ->
      let* owner = insert_user conn "phau_crm_owner" in
      let* moderator = insert_user conn "phau_crm_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000032L ~slug:"phau-crm"
      in
      let* cid = insert_community conn "phau-crm-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-crm"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-crm"
          ~community:"phau-crm-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        Home_audit_fixture.remove_ok "first remover" conn ~actor:owner ~slug:"phau-crm"
          ~community:"phau-crm-home"
      in
      let* () =
        Home_audit_fixture.remove_expect "second remover" Rms.Removal_unavailable conn
          ~actor:moderator ~slug:"phau-crm" ~community:"phau-crm-home"
      in
      Home_audit_fixture.check_events "one removal event" conn ~project
        [ (("project_home_requested", Some owner), (cid, rid));
          (("project_home_accepted", Some moderator), (cid, rid));
          (("project_home_removed", Some owner), (cid, rid))
        ])

let provisioning_race_case =
  db_case "concurrency: a second provisioning attempt creates no second \
           provision event and no loser community" (fun conn ->
      let* owner = insert_user conn "phau_crp_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000033L
          ~project_slug:"phau-crp" ~slug:"phau-crp-home"
      in
      let* () =
        Home_provisioning_fixture.provision_expect "loser" Pv.Active_home_exists conn
          ~actor:owner ~slug:"phau-crp"
          (Home_provisioning_fixture.identity ~name:"Phau Crp" ~slug:"phau-crp-loser" ())
      in
      let* loser = find conn "loser slug" Home_provisioning_fixture.q_count_by_slug
                     "phau-crp-loser" in
      Alcotest.(check int) "no loser community" 0 loser;
      Home_audit_fixture.check_events "one provision event" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid)) ])

let publication_race_case =
  db_case "concurrency: a second publication attempt creates no second \
           publication event" (fun conn ->
      let* owner = insert_user conn "phau_crb_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:958000034L
          ~project_slug:"phau-crb" ~slug:"phau-crb-home"
      in
      let* _ =
        Network_community_fixture.publish_ok "winner" conn ~actor:owner
          ~current:"phau-crb-home" ~expect_slug:"phau-crb-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name:"Phau Crb Hub" ~slug:"phau-crb-hub" ())
      in
      let* () =
        Network_community_fixture.publish_expect "loser" Pub.Draft_unavailable conn
          ~actor:owner ~current:"phau-crb-hub"
          (Network_community_fixture.publication ~name:"Phau Crb Hub" ~slug:"phau-crb-hub" ())
      in
      Home_audit_fixture.check_events "one publication event" conn ~project
        [ (("dedicated_home_provisioned", Some owner), (cid, rid));
          (("network_community_published", Some owner), (cid, rid))
        ])

let concurrency_suite =
  [ request_race_case; review_race_case; removal_race_case;
    provisioning_race_case; publication_race_case
  ]

(* ============== actor deletion and privacy ============== *)

let actor_deletion_retention_case =
  db_case "retention: deleting an actor nulls only that actor's events; \
           history and subjects survive" (fun conn ->
      let* owner = insert_user conn "phau_ret_owner" in
      let* moderator = insert_user conn "phau_ret_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000040L ~slug:"phau-ret"
      in
      let* cid = insert_community conn "phau-ret-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      let* rid =
        Home_audit_fixture.request_ok "request" conn ~user:owner ~slug:"phau-ret"
          ~community:cid
      in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-ret"
          ~community:"phau-ret-home" Rvs.Accept Phr.Accepted
      in
      let* () = exec conn "delete requester" Home_audit_fixture.q_delete_user owner in
      let* () =
        Home_audit_fixture.check_events "after actor deletion" conn ~project
          [ (("project_home_requested", None), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid))
          ]
      in
      let* () = Home_audit_fixture.check_status "relation intact" conn rid "accepted" in
      Lwt.return_unit)

let privacy_case =
  db_case "privacy: no note, credential, email, username, or GitHub \
           identifier reaches audit storage" (fun conn ->
      let* owner = insert_user conn "phau_priv_owner" in
      let* moderator = insert_user conn "phau_priv_mod" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:958000041L ~slug:"phau-priv"
      in
      let* cid = insert_community conn "phau-priv-home" in
      let* () =
        exec conn "top mod" Community_fixture.q_insert_moderator (moderator, cid, "top_mod")
      in
      (* A deliberately credential-shaped private note travels through
         the real request; none of it may reach audit storage. *)
      let note =
        "PHAU_NOTE_SECRET_MARKER gho_PHAU_ACCESS_TOKEN \
         PHAU_SESSION_COOKIE_VALUE"
      in
      let relation =
        Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:(Some note))
      in
      let* created =
        Home_request_fixture.create_ok "request with note" conn ~user:owner
          ~slug:"phau-priv" ~community:cid relation
      in
      let rid = Rq.relation_id created in
      let* () =
        Home_audit_fixture.review_ok "accept" conn ~reviewer:moderator ~slug:"phau-priv"
          ~community:"phau-priv-home" Rvs.Accept Phr.Accepted
      in
      let* () =
        Home_audit_fixture.remove_ok "remove" conn ~actor:owner ~slug:"phau-priv"
          ~community:"phau-priv-home"
      in
      let* () =
        Home_audit_fixture.check_events "full lifecycle" conn ~project
          [ (("project_home_requested", Some owner), (cid, rid));
            (("project_home_accepted", Some moderator), (cid, rid));
            (("project_home_removed", Some owner), (cid, rid))
          ]
      in
      let* blob = find conn "audit blob" q_audit_blob project in
      List.iter
        (fun (label, marker) ->
          Alcotest.(check bool)
            ("no " ^ label ^ " in audit storage")
            false (contains blob marker))
        [ ("request note", "PHAU_NOTE_SECRET_MARKER");
          ("access token shape", "gho_");
          ("session value", "PHAU_SESSION_COOKIE_VALUE");
          ("username", "phau_priv_owner");
          ("email", "@test.invalid");
          ("github namespace login", "pfin-owner")
        ];
      Lwt.return_unit)

let actor_privacy_suite = [ actor_deletion_retention_case; privacy_case ]

let suites =
    (* Append-only lifecycle audit (project_home_audit_events +
       Project_home_audit): the schema's closed action vocabulary,
       required subject FKs, SET NULL actor retention and
       RESTRICT-protected subjects, index/constraint signatures, the
       down/up round-trip, and column privacy; exactly one event per
       successful transition through each real store; no event on any
       refused, replayed, conflicted, or injected-failure path;
       whole-mutation rollback when the audit insertion itself fails;
       event uniqueness under concurrent attempts; actor deletion
       retention and the credential-absence sweep. Database-gated. *)
  [ ("project_home_audit_schema", schema_suite)
  ; ("project_home_audit_events", event_suite)
  ; ("project_home_audit_no_event", no_event_suite)
  ; ("project_home_audit_rollback", rollback_suite)
  ; ("project_home_audit_concurrency", concurrency_suite)
  ; ("project_home_audit_actor_privacy", actor_privacy_suite)
  ]
