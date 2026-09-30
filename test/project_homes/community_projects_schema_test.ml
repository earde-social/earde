(* === Project home-relation schema (community_projects) ===
   SQL-only slice: no store or lifecycle operations exist yet, so these
   cases pin the Postgres behavior directly — closed relation/status
   vocabularies, the per-status row-shape check, timestamp ordering, the
   one-active-home partial unique index (the race arbiter), historical
   rejected/removed rows, provenance SET NULLs, and cascades from both
   FK ends. Same EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope;
   probes run in autocommit with cprj_% users, cprj-% community and
   project slugs, and a reserved namespace-id range 944100001..944100999
   (every fixture project uses it) cleaned before and after each case. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject

(* Projects first (their deletion cascades community_projects rows),
   then communities (likewise), then users. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 944100001 \
       AND 944100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       944100001 AND 944100999";
      "DELETE FROM communities WHERE slug LIKE 'cprj-%'";
      "DELETE FROM users WHERE username LIKE 'cprj_%'";
    ]

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let q_insert_community =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO communities (slug, name) VALUES ($1, $1) RETURNING id"

let q_insert_project =
  (Caqti_type.(t2 string int64) ->! Caqti_type.int64)
    "INSERT INTO open_source_projects\n\
    \     (name, slug, kind, forge_namespace_id, forge_namespace_login,\n\
    \      forge_namespace_type)\n\
    \   VALUES ('Cprj fixture', $1, 'project', $2, 'cprj-owner', 'user')\n\
    \   RETURNING id"

(* Positive ids guaranteed absent, for the FK-failure probes. *)
let q_absent_community_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM communities"

(* Full-width relation insert. reviewed_at/removed_at arrive as
   second offsets from the row's own NOW() default (NULL stays NULL),
   so probes can produce exact-equal, later, and earlier-than-created
   timestamps without leaving SQL time. *)
let q_insert_relation =
  (Caqti_type.(
     t2
       (t2 (t2 int64 int) (t2 string string))
       (t2
          (t2 (option int) (option int))
          (t2 (option string) (t2 (option int) (option int)))))
  ->! Caqti_type.int64)
    "INSERT INTO community_projects\n\
    \     (project_id, community_id, relation_type, status,\n\
    \      requested_by_user_id, reviewed_by_user_id, request_note,\n\
    \      reviewed_at, removed_at)\n\
    \   VALUES ($1, $2, $3, $4, $5, $6, $7,\n\
    \           NOW() + ($8::int * INTERVAL '1 second'),\n\
    \           NOW() + ($9::int * INTERVAL '1 second'))\n\
    \   RETURNING id"

(* updated_at-ordering probe: everything else canonical. *)
let q_insert_relation_backdated =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.int64)
    "INSERT INTO community_projects\n\
    \     (project_id, community_id, status, updated_at)\n\
    \   VALUES ($1, $2, 'pending', NOW() - INTERVAL '10 seconds')\n\
    \   RETURNING id"

(* Everything durable on one relation row; timestamp columns reduce to
   presence/ordering booleans (their exact values are NOW()-relative). *)
let q_relation_row =
  Caqti_type.(
    int64
    ->! t2
          (t2 (t2 int64 int) (t2 string string))
          (t2
             (t2 (option int) (option int))
             (t2 (option string) (t3 bool bool bool))))
    "SELECT project_id, community_id, relation_type, status,\n\
    \          requested_by_user_id, reviewed_by_user_id, request_note,\n\
    \          reviewed_at IS NOT NULL, removed_at IS NOT NULL,\n\
    \          updated_at >= created_at\n\
    \   FROM community_projects WHERE id = $1"

(* Store-shaped transitions with coherent timestamps, used to free the
   active slot mid-case. Lifecycle legality itself is future-store
   territory — the schema only checks the resulting row shape. *)
let q_mark_rejected =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects\n\
    \   SET status = 'rejected', reviewed_at = NOW(), updated_at = NOW()\n\
    \   WHERE id = $1"

let q_mark_removed =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE community_projects\n\
    \   SET status = 'removed', reviewed_at = COALESCE(reviewed_at, NOW()),\n\
    \       removed_at = NOW(), updated_at = NOW()\n\
    \   WHERE id = $1"

let q_relation_exists =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_projects WHERE id = $1"

let q_count_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_projects WHERE project_id = $1"

let q_community_exists =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM communities WHERE id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_delete_community =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM communities WHERE id = $1"

let q_delete_project =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM open_source_projects WHERE id = $1"

(* Schema audit scoped to exactly the one new table. The pattern list
   is wider than the other suites' on purpose: this table must carry
   no GitHub identifiers or raw JSON either. *)
let q_credential_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT column_name FROM information_schema.columns\n\
    \   WHERE table_schema = 'public' AND table_name = 'community_projects'\n\
    \     AND (column_name ~* 'token' OR column_name ~* 'secret'\n\
    \          OR column_name ~* 'verifier'\n\
    \          OR column_name ~* 'authorization_code'\n\
    \          OR column_name ~* 'oauth_code' OR column_name ~* 'raw_state'\n\
    \          OR column_name ~* 'session_binding'\n\
    \          OR column_name ~* 'private_key'\n\
    \          OR column_name ~* 'installation_id'\n\
    \          OR column_name ~* 'account_id'\n\
    \          OR column_name ~* 'repository_id' OR column_name ~* 'json')"

(* No column may directly assign a role: inserting a relation grants
   neither project stewardship, community moderation, nor global
   administration — those stay in their own tables. *)
let q_role_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT column_name FROM information_schema.columns\n\
    \   WHERE table_schema = 'public' AND table_name = 'community_projects'\n\
    \     AND (column_name ~* 'role' OR column_name ~* 'steward'\n\
    \          OR column_name ~* 'moderator' OR column_name ~* 'admin')"

(* One signature per index: name, uniqueness, predicate (NULL when the
   index is total), and the key columns in index order — so assertions
   inspect real metadata, not generated names alone. *)
let q_index_signatures =
  Caqti_type.(unit ->* t2 (t2 string bool) (t2 (option string) string))
    "SELECT ci.relname, ix.indisunique,\n\
    \          pg_get_expr(ix.indpred, ix.indrelid),\n\
    \          (SELECT string_agg(a.attname, ',' ORDER BY k.ord)\n\
    \           FROM unnest(ix.indkey::int2[]) WITH ORDINALITY AS k(attnum, ord)\n\
    \           JOIN pg_attribute a\n\
    \             ON a.attrelid = ix.indrelid AND a.attnum = k.attnum)\n\
    \   FROM pg_index ix\n\
    \   JOIN pg_class ci ON ci.oid = ix.indexrelid\n\
    \   JOIN pg_class ct ON ct.oid = ix.indrelid\n\
    \   WHERE ct.relname = 'community_projects'\n\
    \   ORDER BY ci.relname"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. *)
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

let insert_user conn username =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_user username in
  or_fail ("user " ^ username) uid

let insert_community conn slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* cid = C.find q_insert_community slug in
  or_fail ("community " ^ slug) cid

let insert_project conn slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* pid = C.find q_insert_project (slug, 944100001L) in
  or_fail ("project " ^ slug) pid

(* Relation-insert helper: ?reviewed/?removed are second offsets from
   the row's created_at (0 = exactly created_at); omitted means NULL. *)
let insert_relation ?(relation_type = "home") ~status ?requester ?reviewer ?note
    ?reviewed ?removed conn ~project ~community =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.find q_insert_relation
    ( ((project, community), (relation_type, status)),
      ((requester, reviewer), (note, (reviewed, removed))) )

let insert_relation_ok label ?relation_type ~status ?requester ?reviewer ?note
    ?reviewed ?removed conn ~project ~community =
  let* r =
    insert_relation ?relation_type ~status ?requester ?reviewer ?note ?reviewed
      ?removed conn ~project ~community
  in
  or_fail label r

let relation_row conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* row = C.find q_relation_row id in
  or_fail "relation row" row

(* Every structurally coherent lifecycle shape round-trips exactly:
   a pending request with requester and note, the auto-provisioned
   accepted home (reviewed_at present, reviewer legitimately NULL —
   no fake review), the moderator-reviewed accepted home, a historical
   rejection, a historical removal, and the fully minimal pending row
   (no requester, no note). One project per active row — the active
   slot is per-project. *)
let shapes_case =
  db_case "valid shapes: every lifecycle shape round-trips exactly" (fun conn ->
      let* requester = insert_user conn "cprj_a" in
      let* reviewer = insert_user conn "cprj_b" in
      let* community = insert_community conn "cprj-home" in
      let* p1 = insert_project conn "cprj-shape-pending" in
      let* rel =
        insert_relation_ok "pending with requester and note" ~status:"pending"
          ~requester ~note:"Nota di richiesta — ✓" conn ~project:p1 ~community
      in
      let* ( ((rp, rc), (rtype, status)),
             ((req, rev), (note, (has_reviewed, has_removed, upd_ge))) ) =
        relation_row conn rel
      in
      Alcotest.(check int64) "project fk round-trips" p1 rp;
      Alcotest.(check int) "community fk round-trips" community rc;
      Alcotest.(check string) "relation type defaults to home" "home" rtype;
      Alcotest.(check string) "status" "pending" status;
      Alcotest.(check (option int)) "requester stored" (Some requester) req;
      Alcotest.(check (option int)) "no reviewer on pending" None rev;
      Alcotest.(check (option string))
        "note byte-exact" (Some "Nota di richiesta — ✓") note;
      Alcotest.(check bool) "pending: no reviewed_at" false has_reviewed;
      Alcotest.(check bool) "pending: no removed_at" false has_removed;
      Alcotest.(check bool) "updated_at >= created_at" true upd_ge;
      let* p2 = insert_project conn "cprj-shape-auto" in
      let* rel =
        insert_relation_ok "accepted auto-provisioned home" ~status:"accepted"
          ~requester ~reviewed:0 conn ~project:p2 ~community
      in
      let* _, ((_, rev), (_, (has_reviewed, has_removed, _))) =
        relation_row conn rel
      in
      Alcotest.(check (option int))
        "auto-provisioned home has no reviewer" None rev;
      Alcotest.(check bool)
        "auto-provisioned home still records reviewed_at" true has_reviewed;
      Alcotest.(check bool) "accepted: no removed_at" false has_removed;
      let* p3 = insert_project conn "cprj-shape-reviewed" in
      let* rel =
        insert_relation_ok "accepted moderator-reviewed home" ~status:"accepted"
          ~requester ~reviewer ~reviewed:0 conn ~project:p3 ~community
      in
      let* _, ((_, rev), (_, (has_reviewed, _, _))) = relation_row conn rel in
      Alcotest.(check (option int)) "reviewer stored" (Some reviewer) rev;
      Alcotest.(check bool) "reviewed_at present" true has_reviewed;
      let* p4 = insert_project conn "cprj-shape-rejected" in
      let* _ =
        insert_relation_ok "rejected request" ~status:"rejected" ~requester
          ~reviewer ~reviewed:0 conn ~project:p4 ~community
      in
      let* p5 = insert_project conn "cprj-shape-removed" in
      let* rel =
        insert_relation_ok "removed former home" ~status:"removed" ~requester
          ~reviewer ~reviewed:0 ~removed:5 conn ~project:p5 ~community
      in
      let* _, (_, (_, (has_reviewed, has_removed, _))) =
        relation_row conn rel
      in
      Alcotest.(check bool) "removed keeps reviewed_at" true has_reviewed;
      Alcotest.(check bool) "removed_at present" true has_removed;
      let* p6 = insert_project conn "cprj-shape-minimal" in
      let* rel =
        insert_relation_ok "minimal pending: no requester, no note"
          ~status:"pending" conn ~project:p6 ~community
      in
      let* _, ((req, _), (note, _)) = relation_row conn rel in
      Alcotest.(check (option int)) "requester nullable" None req;
      Alcotest.(check (option string)) "note nullable" None note;
      Lwt.return_unit)

(* Closed vocabularies and the per-status row shape: every incoherent
   combination rejects independently. Failed inserts leave no row, so
   one project/community pair serves every probe. *)
let status_case =
  db_case "status constraints: every incoherent shape rejects" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* reviewer = insert_user conn "cprj_b" in
      let* community = insert_community conn "cprj-status" in
      let* project = insert_project conn "cprj-status" in
      let probe label ?relation_type ~status ?reviewer ?reviewed ?removed () =
        let* r =
          insert_relation ?relation_type ~status ?reviewer ?reviewed ?removed
            conn ~project ~community
        in
        reject label r
      in
      let* () = probe "unknown status" ~status:"draft" () in
      let* () =
        probe "unknown relation type" ~relation_type:"related" ~status:"pending"
          ()
      in
      let* () =
        probe "pending with reviewed_at" ~status:"pending" ~reviewed:0 ()
      in
      let* () = probe "pending with reviewer" ~status:"pending" ~reviewer () in
      let* () =
        probe "pending with removed_at" ~status:"pending" ~removed:0 ()
      in
      let* () = probe "accepted without reviewed_at" ~status:"accepted" () in
      let* () =
        probe "accepted with removed_at" ~status:"accepted" ~reviewed:0
          ~removed:5 ()
      in
      let* () = probe "rejected without reviewed_at" ~status:"rejected" () in
      let* () =
        probe "rejected with removed_at" ~status:"rejected" ~reviewed:0
          ~removed:5 ()
      in
      let* () =
        probe "removed without reviewed_at" ~status:"removed" ~removed:5 ()
      in
      let* () =
        probe "removed without removed_at" ~status:"removed" ~reviewed:0 ()
      in
      let* () =
        probe "reviewed_at < created_at" ~status:"accepted" ~reviewed:(-10) ()
      in
      let* () =
        probe "removed_at < created_at" ~status:"removed" ~reviewed:0
          ~removed:(-10) ()
      in
      let* () =
        probe "removed_at < reviewed_at" ~status:"removed" ~reviewed:10
          ~removed:5 ()
      in
      let* r = C.find q_insert_relation_backdated (project, community) in
      reject "updated_at < created_at" r)

(* Request note bounds. Blank text is structurally allowed on purpose:
   blank→NULL canonicalization (with UTF-8 and control validation)
   belongs to the future pure domain layer, not SQL. *)
let note_case =
  db_case "request note: verbatim storage, exact 2000-char bound" (fun conn ->
      let* community = insert_community conn "cprj-note" in
      let* p1 = insert_project conn "cprj-note-blank" in
      let* rel =
        insert_relation_ok "blank note is structurally allowed"
          ~status:"pending" ~note:"   " conn ~project:p1 ~community
      in
      let* _, (_, (note, _)) = relation_row conn rel in
      Alcotest.(check (option string)) "blank stored verbatim" (Some "   ") note;
      let* p2 = insert_project conn "cprj-note-utf8" in
      let* rel =
        insert_relation_ok "UTF-8 note" ~status:"pending"
          ~note:"Città — proposta 🏠" conn ~project:p2 ~community
      in
      let* _, (_, (note, _)) = relation_row conn rel in
      Alcotest.(check (option string))
        "UTF-8 byte-exact" (Some "Città — proposta 🏠") note;
      let* p3 = insert_project conn "cprj-note-max" in
      let* _ =
        insert_relation_ok "2000-char note (char_length counts chars)"
          ~status:"pending" ~note:(String.make 2000 'n') conn ~project:p3
          ~community
      in
      let* p4 = insert_project conn "cprj-note-over" in
      let* r =
        insert_relation ~status:"pending" ~note:(String.make 2001 'n') conn
          ~project:p4 ~community
      in
      reject "2001-char note" r)

(* The load-bearing rule: at most one ACTIVE home relation per project,
   regardless of target community. Every probe inserts directly and
   lets the partial unique index answer — no test-side precheck — so
   the database, not the application, is proven to be the arbiter. *)
let active_home_case =
  db_case "one active home per project: the index is the arbiter" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* requester = insert_user conn "cprj_a" in
      let* c1 = insert_community conn "cprj-active-1" in
      let* c2 = insert_community conn "cprj-active-2" in
      let* project = insert_project conn "cprj-active" in
      let* pending1 =
        insert_relation_ok "first pending" ~status:"pending" ~requester conn
          ~project ~community:c1
      in
      let* r =
        insert_relation ~status:"pending" ~requester conn ~project ~community:c2
      in
      let* () = reject "second pending (different community)" r in
      let* r =
        insert_relation ~status:"accepted" ~reviewed:0 conn ~project
          ~community:c2
      in
      let* () = reject "accepted while a pending exists" r in
      let* r = C.exec q_mark_rejected pending1 in
      let* () = or_fail "reject pending1" r in
      let* pending2 =
        insert_relation_ok "fresh pending after rejection" ~status:"pending"
          ~requester conn ~project ~community:c2
      in
      let* r = C.exec q_mark_rejected pending2 in
      let* () = or_fail "reject pending2" r in
      let* accepted1 =
        insert_relation_ok "accepted home after the slot cleared"
          ~status:"accepted" ~reviewed:0 conn ~project ~community:c1
      in
      let* r =
        insert_relation ~status:"accepted" ~reviewed:0 conn ~project
          ~community:c2
      in
      let* () = reject "two accepted homes" r in
      let* r =
        insert_relation ~status:"pending" ~requester conn ~project ~community:c2
      in
      let* () = reject "pending while an accepted exists" r in
      let* r = C.exec q_mark_removed accepted1 in
      let* () = or_fail "remove accepted1" r in
      let* accepted2 =
        insert_relation_ok "new accepted home after removal" ~status:"accepted"
          ~reviewed:0 conn ~project ~community:c2
      in
      let* r = C.exec q_mark_removed accepted2 in
      let* () = or_fail "remove accepted2" r in
      let* _ =
        insert_relation_ok "new pending after removal" ~status:"pending"
          ~requester conn ~project ~community:c1
      in
      Lwt.return_unit)

(* Historical rejected and removed rows for one project/community pair
   coexist with a later active relation: they fall outside the partial
   index predicate and never occupy the active slot. *)
let historical_case =
  db_case "historical rows: rejected/removed never occupy the slot" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* community = insert_community conn "cprj-history" in
      let* project = insert_project conn "cprj-history" in
      let* _ =
        insert_relation_ok "historical rejection" ~status:"rejected" ~reviewed:0
          conn ~project ~community
      in
      let* _ =
        insert_relation_ok "historical removal" ~status:"removed" ~reviewed:0
          ~removed:5 conn ~project ~community
      in
      let* _ =
        insert_relation_ok "later active pending on the same pair"
          ~status:"pending" conn ~project ~community
      in
      let* n = C.find q_count_for_project project in
      let* n = or_fail "history count" n in
      Alcotest.(check int) "all three rows coexist" 3 n;
      Lwt.return_unit)

(* The active slot is per-project: one community may simultaneously be
   the accepted home of several projects and the target of several
   pending requests, while each project stays constrained on its own. *)
let multi_project_case =
  db_case "multiple projects: independently constrained, shared targets"
    (fun conn ->
      let* community = insert_community conn "cprj-shared" in
      let* p1 = insert_project conn "cprj-multi-1" in
      let* p2 = insert_project conn "cprj-multi-2" in
      let* p3 = insert_project conn "cprj-multi-3" in
      let* p4 = insert_project conn "cprj-multi-4" in
      let* _ =
        insert_relation_ok "project 1 accepted home" ~status:"accepted"
          ~reviewed:0 conn ~project:p1 ~community
      in
      let* _ =
        insert_relation_ok "project 2 accepted home, same community"
          ~status:"accepted" ~reviewed:0 conn ~project:p2 ~community
      in
      let* _ =
        insert_relation_ok "project 3 pending, same community" ~status:"pending"
          conn ~project:p3 ~community
      in
      let* _ =
        insert_relation_ok "project 4 pending, same community" ~status:"pending"
          conn ~project:p4 ~community
      in
      let* r = insert_relation ~status:"pending" conn ~project:p1 ~community in
      reject "project 1 still holds its own active slot" r)

(* FK ends: ghosts reject; deleting one side cascades only the
   relation rows and leaves the other side standing; deleting a
   provenance user nulls only the provenance field. *)
let fk_deletion_case =
  db_case "foreign keys: ghosts, cascades, provenance SET NULLs" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* requester = insert_user conn "cprj_a" in
      let* reviewer = insert_user conn "cprj_b" in
      let* community = insert_community conn "cprj-fk" in
      let* project = insert_project conn "cprj-fk" in
      let* ghost_p = C.find Project_fixture.q_absent_project_id () in
      let* ghost_p = or_fail "ghost project id" ghost_p in
      let* r =
        insert_relation ~status:"pending" conn ~project:ghost_p ~community
      in
      let* () = reject "nonexistent project" r in
      let* ghost_c = C.find q_absent_community_id () in
      let* ghost_c = or_fail "ghost community id" ghost_c in
      let* r =
        insert_relation ~status:"pending" conn ~project ~community:ghost_c
      in
      let* () = reject "nonexistent community" r in
      let* ghost_u = C.find Project_fixture.q_absent_user_id () in
      let* ghost_u = or_fail "ghost user id" ghost_u in
      let* r =
        insert_relation ~status:"pending" ~requester:ghost_u conn ~project
          ~community
      in
      let* () = reject "nonexistent requester" r in
      let* r =
        insert_relation ~status:"accepted" ~reviewer:ghost_u ~reviewed:0 conn
          ~project ~community
      in
      let* () = reject "nonexistent reviewer" r in
      (* Project deletion cascades the relation, community survives. *)
      let* rel =
        insert_relation_ok "home to cascade from project" ~status:"accepted"
          ~reviewed:0 conn ~project ~community
      in
      let* r = C.exec q_delete_project project in
      let* () = or_fail "delete project" r in
      let* n = C.find q_relation_exists rel in
      let* n = or_fail "relation after project delete" n in
      Alcotest.(check int) "project delete cascades the relation" 0 n;
      let* n = C.find q_community_exists community in
      let* n = or_fail "community after project delete" n in
      Alcotest.(check int) "community survives project deletion" 1 n;
      (* Community deletion cascades the relation, project survives. *)
      let* project = insert_project conn "cprj-fk-2" in
      let* rel =
        insert_relation_ok "home to cascade from community" ~status:"accepted"
          ~reviewed:0 conn ~project ~community
      in
      let* r = C.exec q_delete_community community in
      let* () = or_fail "delete community" r in
      let* n = C.find q_relation_exists rel in
      let* n = or_fail "relation after community delete" n in
      Alcotest.(check int) "community delete cascades the relation" 0 n;
      let* n = C.find Project_fixture.q_project_exists project in
      let* n = or_fail "project after community delete" n in
      Alcotest.(check int) "project survives community deletion" 1 n;
      (* Requester deletion: provenance nulls, the row and its note
         survive untouched. *)
      let* community = insert_community conn "cprj-fk-b" in
      let* rel =
        insert_relation_ok "pending with doomed requester" ~status:"pending"
          ~requester ~note:"provenance" conn ~project ~community
      in
      let* r = C.exec q_delete_user requester in
      let* () = or_fail "delete requester" r in
      let* ( (_, (_, status)),
             ((req, _), (note, (has_reviewed, has_removed, upd_ge))) ) =
        relation_row conn rel
      in
      Alcotest.(check (option int)) "requester went NULL" None req;
      Alcotest.(check string) "status survives" "pending" status;
      Alcotest.(check (option string)) "note survives" (Some "provenance") note;
      Alcotest.(check bool)
        "timestamps coherent" true
        (upd_ge && (not has_reviewed) && not has_removed);
      (* Reviewer deletion: same, with reviewed_at staying behind. *)
      let* r = C.exec q_mark_rejected rel in
      let* () = or_fail "clear the active slot" r in
      let* rel =
        insert_relation_ok "accepted with doomed reviewer" ~status:"accepted"
          ~reviewer ~reviewed:0 conn ~project ~community
      in
      let* r = C.exec q_delete_user reviewer in
      let* () = or_fail "delete reviewer" r in
      let* (_, (_, status)), ((_, rev), (_, (has_reviewed, _, _))) =
        relation_row conn rel
      in
      Alcotest.(check (option int)) "reviewer went NULL" None rev;
      Alcotest.(check string) "status survives" "accepted" status;
      Alcotest.(check bool)
        "reviewed_at survives the reviewer" true has_reviewed;
      Lwt.return_unit)

(* Credential/identifier and role-column audits; the pinned column
   count proves the table was actually inspected, so neither audit can
   pass vacuously. *)
let audit_case =
  db_case "column audit: 12 columns, no credentials, no role grants"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* n = C.find Db_fixture.q_column_count "community_projects" in
      let* n = or_fail "column count" n in
      Alcotest.(check int) "community_projects column count" 12 n;
      let* offenders = C.collect_list q_credential_columns () in
      let* offenders = or_fail "credential-shaped columns" offenders in
      Alcotest.(check (list string)) "no credential-shaped column" [] offenders;
      let* offenders = C.collect_list q_role_columns () in
      let* offenders = or_fail "role-shaped columns" offenders in
      Alcotest.(check (list string)) "no role-granting column" [] offenders;
      Lwt.return_unit)

(* Index metadata: the active-home index must be unique, keyed on
   project_id alone, and partial over exactly home/pending/accepted;
   the moderation-queue and project-history indexes must exist with
   their expected key order. Predicates and key columns come from
   pg_index — names alone are not trusted. *)
let index_case =
  db_case "index metadata: active-home predicate, queue, history" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* indexes = C.collect_list q_index_signatures () in
      let* indexes = or_fail "index signatures" indexes in
      Alcotest.(check int)
        "exactly four indexes (pkey + three)" 4 (List.length indexes);
      let find_by_columns ?(unique = false) ?(partial = false) label cols =
        match
          List.find_opt
            (fun ((_, u), (pred, c)) ->
              c = cols && u = unique && Option.is_some pred = partial)
            indexes
        with
        | Some ix -> ix
        | None -> Alcotest.failf "%s: no index on (%s)" label cols
      in
      let (_, _), (pred, _) =
        find_by_columns ~unique:true ~partial:true "active-home index"
          "project_id"
      in
      let pred =
        match pred with
        | Some p -> p
        | None -> Alcotest.fail "active-home index lost its predicate"
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("active-home predicate covers " ^ needle)
            true
            (Html_assert.occurs ~needle pred))
        [ "relation_type"; "'home'"; "status"; "'pending'"; "'accepted'" ];
      let _ =
        find_by_columns "moderation-queue index"
          "community_id,status,created_at"
      in
      let _ = find_by_columns "project-history index" "project_id" in
      let _ = find_by_columns ~unique:true "primary key" "id" in
      Lwt.return_unit)

let suite =
  [
    shapes_case;
    status_case;
    note_case;
    active_home_case;
    historical_case;
    multi_project_case;
    fk_deletion_case;
    audit_case;
    index_case;
  ]

let suites =
  (* Project/community home-relation schema: SQL-only slice, no
       store yet — closed relation/status vocabularies, per-status row
       shapes, the one-active-home partial unique index as race
       arbiter, historical rejected/removed rows, provenance SET
       NULLs, cascades from both FK ends, and the credential/role
       column audit; same EARDE_TEST_DATABASE_URL gate (each case
       skips without it). *)
  [ ("community_projects_schema", suite) ]
