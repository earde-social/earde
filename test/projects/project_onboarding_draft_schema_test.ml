(* === Project-onboarding draft schema (SQL-only slice) ===
   No draft store exists yet, so these cases pin the PostgreSQL contract
   directly: explicit draft ownership (user_id — never inferred from
   github_installations.connected_by_user_id), the local
   installation-record FK (CASCADE from users, RESTRICT from
   github_installations), the closed status lifecycle with exactly one
   timestamp shape per status, derived expiry (no 'expired' status — an
   expired-but-active row keeps its unique active slot for in-place
   refresh), per-draft snapshot ordering and uniqueness (deliberately not
   global: a snapshot is not a project claim), primary-implies-selected,
   and the absence of any credential-shaped column. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope. Constraint-violation
   probes run in autocommit on purpose — an aborted transaction would
   block every follow-up statement — with poddraft_% users and a reserved
   external-installation-id range cleaned before and after each case. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

(* Fixtures — reserved external-installation-id range
   937000001..937000999 and fixed poddraft_% usernames so cleanup is
   targeted and idempotent. Drafts go first (installations are
   RESTRICT-protected while referenced); snapshots cascade from
   drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 937000001 AND 937000999)";
      "DELETE FROM users WHERE username LIKE 'poddraft_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       937000001 AND 937000999";
    ]

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

(* Direct installation fixture (as in the OAuth-callback suite): these
   cases pin the drafts schema, not installation persistence. The local
   row id is what drafts reference; the external id only namespaces
   cleanup. *)
let q_insert_installation =
  (Caqti_type.(t2 int64 (option int)) ->! Caqti_type.int64)
    "INSERT INTO github_installations\n\
    \     (github_installation_id, github_account_id, github_account_login,\n\
    \      github_account_type, connected_by_user_id)\n\
    \   VALUES ($1, $1 + 100000, 'poddraft-account', 'user', $2)\n\
    \   RETURNING id"

(* Positive ids guaranteed absent, for the FK-failure probes. *)
let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

let q_absent_installation_record_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM github_installations"

(* Store-shaped insert: explicit lifetime, everything else defaulted. *)
let q_insert_draft =
  (Caqti_type.(t2 int int64) ->! Caqti_type.int64)
    "INSERT INTO project_onboarding_drafts\n\
    \     (user_id, github_installation_record_id, expires_at)\n\
    \   VALUES ($1, $2, NOW() + INTERVAL '24 hours') RETURNING id"

(* Lifecycle probe: status plus each lifecycle timestamp as a
   present/absent flag. *)
let q_insert_draft_lifecycle =
  (Caqti_type.(t2 (t2 int int64) (t3 string bool bool)) ->! Caqti_type.int64)
    "INSERT INTO project_onboarding_drafts\n\
    \     (user_id, github_installation_record_id, expires_at, status,\n\
    \      completed_at, cancelled_at)\n\
    \   VALUES ($1, $2, NOW() + INTERVAL '24 hours', $3,\n\
    \           CASE WHEN $4 THEN NOW() END,\n\
    \           CASE WHEN $5 THEN NOW() END)\n\
    \   RETURNING id"

(* Timestamp probe/fixture: expires/created/updated as second offsets
   from NOW() — also builds the expired-but-still-active fixture (both
   created_at and expires_at must be backdated: expires > created). *)
let q_insert_draft_at =
  (Caqti_type.(t2 (t2 int int64) (t3 int int int)) ->! Caqti_type.int64)
    "INSERT INTO project_onboarding_drafts\n\
    \     (user_id, github_installation_record_id, expires_at, created_at,\n\
    \      updated_at)\n\
    \   VALUES ($1, $2, NOW() + $3 * INTERVAL '1 second',\n\
    \           NOW() + $4 * INTERVAL '1 second',\n\
    \           NOW() + $5 * INTERVAL '1 second')\n\
    \   RETURNING id"

(* Everything durable on one draft row: ownership, installation record,
   lifecycle shape, and the derived-expiry inputs. *)
let q_draft_row =
  Caqti_type.(
    int64 ->! t2 (t3 int int64 string) (t2 (t2 bool bool) (t2 bool float)))
    "SELECT user_id, github_installation_record_id, status,\n\
    \          completed_at IS NULL, cancelled_at IS NULL,\n\
    \          updated_at >= created_at,\n\
    \          EXTRACT(EPOCH FROM (expires_at - created_at))::float8\n\
    \   FROM project_onboarding_drafts WHERE id = $1"

let q_draft_owner =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT user_id FROM project_onboarding_drafts WHERE id = $1"

let q_complete_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts\n\
    \   SET status = 'completed', completed_at = NOW(), updated_at = NOW()\n\
    \   WHERE id = $1"

let q_cancel_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts\n\
    \   SET status = 'cancelled', cancelled_at = NOW(), updated_at = NOW()\n\
    \   WHERE id = $1"

let q_count_drafts_for_user =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_onboarding_drafts WHERE user_id = $1"

let q_count_repos_for_draft =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_delete_installation =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM github_installations WHERE id = $1"

let q_delete_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM project_onboarding_drafts WHERE id = $1"

let q_set_installation_provenance =
  (Caqti_type.(t2 int64 (option int)) ->. Caqti_type.unit)
    "UPDATE github_installations SET connected_by_user_id = $2 WHERE id = $1"

(* Full-width snapshot insert; nested tuples per the Caqti convention. *)
let q_insert_repo =
  (Caqti_type.(
     t2
       (t2 (t2 int64 int) (t2 int64 int64))
       (t2
          (t4 string string string string)
          (t2 (t2 (option string) string) (t3 bool bool bool))))
  ->! Caqti_type.int64)
    "INSERT INTO project_onboarding_draft_repositories\n\
    \     (draft_id, position, github_repository_id, github_owner_id,\n\
    \      owner_login, name, full_name, html_url, description,\n\
    \      default_branch, is_archived, is_selected, is_primary)\n\
    \   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)\n\
    \   RETURNING id"

(* One text signature per snapshot row, ordered by position — pins the
   exact stored metadata and the ordering in a single comparison. *)
let q_repo_signatures =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT position::text || '|' || github_repository_id::text || '|' ||\n\
    \          github_owner_id::text || '|' || owner_login || '|' || name\n\
    \          || '|' || full_name || '|' || html_url || '|' ||\n\
    \          COALESCE(description, '<null>') || '|' || default_branch\n\
    \          || '|' || is_archived::text || '|' || is_selected::text\n\
    \          || '|' || is_primary::text\n\
    \   FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

(* Schema audit scoped to exactly the two new tables — deliberately not
   a sweep of unrelated existing tables. *)
let q_credential_columns =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT table_name || '.' || column_name\n\
    \   FROM information_schema.columns\n\
    \   WHERE table_schema = 'public'\n\
    \     AND table_name IN ('project_onboarding_drafts',\n\
    \                        'project_onboarding_draft_repositories')\n\
    \     AND (column_name ~* 'token' OR column_name ~* 'secret'\n\
    \          OR column_name ~* 'verifier'\n\
    \          OR column_name ~* 'authorization_code'\n\
    \          OR column_name ~* 'oauth_code' OR column_name ~* 'raw_state'\n\
    \          OR column_name ~* 'session_binding'\n\
    \          OR column_name ~* 'private_key')"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. *)
let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = Db_fixture.or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = Db_fixture.or_fail "cleanup" r in
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
  Db_fixture.or_fail ("user " ^ username) uid

let insert_installation ?connected_by conn ext_id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rid = C.find q_insert_installation (ext_id, connected_by) in
  Db_fixture.or_fail "installation" rid

let insert_draft conn ~user ~installation =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.find q_insert_draft (user, installation)

let insert_draft_ok label conn ~user ~installation =
  let* r = insert_draft conn ~user ~installation in
  Db_fixture.or_fail label r

(* Snapshot-insert helper: canonical valid metadata, overridable per
   probe. full_name/html_url derive from login/name unless a probe pins
   them explicitly. *)
let insert_repo ?(login = "poddraft-owner") ?(name = "repo") ?full_name
    ?html_url ?description ?(branch = "main") ?(archived = false)
    ?(selected = false) ?(primary = false) ?(owner_id = 937500001L) ~repo_id
    ~position conn draft =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let full_name =
    match full_name with Some f -> f | None -> login ^ "/" ^ name
  in
  let html_url =
    match html_url with
    | Some u -> u
    | None -> "https://github.com/" ^ full_name
  in
  C.find q_insert_repo
    ( ((draft, position), (repo_id, owner_id)),
      ( (login, name, full_name, html_url),
        ((description, branch), (archived, selected, primary)) ) )

let insert_repo_ok label ?login ?name ?full_name ?html_url ?description ?branch
    ?archived ?selected ?primary ?owner_id ~repo_id ~position conn draft =
  let* r =
    insert_repo ?login ?name ?full_name ?html_url ?description ?branch ?archived
      ?selected ?primary ?owner_id ~repo_id ~position conn draft
  in
  Db_fixture.or_fail label r

(* Fresh draft + snapshot: ownership and installation record stored
   exactly, active shape, explicit 24h expiry, snapshot metadata
   byte-exact in response order, selection defaults false, and no
   token-shaped bytes anywhere in the stored values. *)
let fresh_case =
  db_case "fresh active draft and snapshot store exactly what was given"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "poddraft_a" in
      let* inst = insert_installation conn 937000001L in
      let* draft = insert_draft_ok "draft" conn ~user:uid ~installation:inst in
      let* row = C.find q_draft_row draft in
      let* (u, i, status), ((no_completed, no_cancelled), (updated_ge, expiry))
          =
        Db_fixture.or_fail "draft row" row
      in
      Alcotest.(check int) "owner is the exact Earde user" uid u;
      Alcotest.(check int64) "references the local installation record" inst i;
      Alcotest.(check string) "status defaults to active" "active" status;
      Alcotest.(check bool) "completed_at is NULL" true no_completed;
      Alcotest.(check bool) "cancelled_at is NULL" true no_cancelled;
      Alcotest.(check bool) "updated_at >= created_at" true updated_ge;
      Alcotest.(check (float 0.5)) "explicit 24h expiry stored" 86400.0 expiry;
      let* _ =
        insert_repo_ok "repo 1" ~login:"poddraft-owner" ~name:"alpha"
          ~description:"Prima descrizione — byte exact" ~repo_id:937600001L
          ~position:1 conn draft
      in
      let* _ =
        insert_repo_ok "repo 2" ~name:"beta" ~archived:true ~repo_id:937600002L
          ~position:2 conn draft
      in
      let* _ =
        insert_repo_ok "repo 3" ~name:"gamma" ~branch:"release/v1"
          ~repo_id:937600003L ~position:3 conn draft
      in
      let* sigs = C.collect_list q_repo_signatures draft in
      let* sigs = Db_fixture.or_fail "signatures" sigs in
      Alcotest.(check (list string))
        "metadata and order preserved exactly"
        [
          "1|937600001|937500001|poddraft-owner|alpha|poddraft-owner/alpha|https://github.com/poddraft-owner/alpha|Prima \
           descrizione — byte exact|main|false|false|false";
          "2|937600002|937500001|poddraft-owner|beta|poddraft-owner/beta|https://github.com/poddraft-owner/beta|<null>|main|true|false|false";
          "3|937600003|937500001|poddraft-owner|gamma|poddraft-owner/gamma|https://github.com/poddraft-owner/gamma|<null>|release/v1|false|false|false";
        ]
        sigs;
      let blob = String.concat "|" sigs in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            (needle ^ " absent from stored rows")
            false
            (Html_assert.contains_nonempty ~needle blob))
        [ "token"; "secret"; "verifier" ];
      Lwt.return_unit)

(* Ownership: user_id is the owner; connected_by_user_id is provenance
   only and moving it never moves a draft. *)
let ownership_case =
  db_case "draft ownership is user_id, independent of provenance" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* a = insert_user conn "poddraft_a" in
      let* b = insert_user conn "poddraft_b" in
      let* i1 = insert_installation ~connected_by:a conn 937000011L in
      let* i2 = insert_installation conn 937000012L in
      let* da = insert_draft_ok "A on I1" conn ~user:a ~installation:i1 in
      let* db =
        insert_draft_ok "B on I1 (same installation, other owner)" conn ~user:b
          ~installation:i1
      in
      let* _ =
        insert_draft_ok "A on I2 (same owner, other installation)" conn ~user:a
          ~installation:i2
      in
      let* owner = C.find q_draft_owner db in
      let* owner = Db_fixture.or_fail "B draft owner" owner in
      Alcotest.(check int)
        "draft owned by B though A connected the installation" b owner;
      (* Provenance changes — clear it, then hand it to B — must leave
         both drafts' owners untouched. *)
      let* r = C.exec q_set_installation_provenance (i1, None) in
      let* () = Db_fixture.or_fail "clear provenance" r in
      let* r = C.exec q_set_installation_provenance (i1, Some b) in
      let* () = Db_fixture.or_fail "move provenance" r in
      let* oa = C.find q_draft_owner da in
      let* oa = Db_fixture.or_fail "A draft owner" oa in
      let* ob = C.find q_draft_owner db in
      let* ob = Db_fixture.or_fail "B draft owner after provenance moves" ob in
      Alcotest.(check int) "A draft still owned by A" a oa;
      Alcotest.(check int) "B draft still owned by B" b ob;
      Lwt.return_unit)

(* Active-draft uniqueness: one active slot per (user, installation
   record); terminal states free the slot; expiry does not. *)
let active_unique_case =
  db_case "at most one active draft per user and installation record"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "poddraft_a" in
      let* i1 = insert_installation conn 937000021L in
      let* i2 = insert_installation conn 937000022L in
      let* d1 =
        insert_draft_ok "first active" conn ~user:uid ~installation:i1
      in
      let* dup = insert_draft conn ~user:uid ~installation:i1 in
      let* () =
        Db_fixture.reject "second active draft for same user+installation" dup
      in
      let* r = C.exec q_complete_draft d1 in
      let* () = Db_fixture.or_fail "complete first" r in
      let* d2 =
        insert_draft_ok "new active after completed" conn ~user:uid
          ~installation:i1
      in
      let* r = C.exec q_cancel_draft d2 in
      let* () = Db_fixture.or_fail "cancel second" r in
      let* _ =
        insert_draft_ok "new active after cancelled" conn ~user:uid
          ~installation:i1
      in
      (* Expired but still active: occupies the slot — the store must
         refresh this row, not insert a sibling. *)
      let* expired =
        C.find q_insert_draft_at ((uid, i2), (-86400, -172800, -172800))
      in
      let* _ = Db_fixture.or_fail "expired-but-active fixture" expired in
      let* blocked = insert_draft conn ~user:uid ~installation:i2 in
      Db_fixture.reject "expired-but-active row still holds the unique slot"
        blocked)

(* Lifecycle shapes: exactly one timestamp shape per status; every
   inconsistent combination and both timestamp-ordering violations are
   rejected independently. *)
let lifecycle_case =
  db_case "lifecycle and timestamp constraints reject every bad shape"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "poddraft_a" in
      let* inst = insert_installation conn 937000031L in
      let probe label shape =
        let* r = C.find q_insert_draft_lifecycle ((uid, inst), shape) in
        Db_fixture.reject label r
      in
      let* () = probe "active with completed_at" ("active", true, false) in
      let* () = probe "active with cancelled_at" ("active", false, true) in
      let* () =
        probe "completed without completed_at" ("completed", false, false)
      in
      let* () = probe "completed with cancelled_at" ("completed", true, true) in
      let* () =
        probe "cancelled without cancelled_at" ("cancelled", false, false)
      in
      let* () = probe "cancelled with completed_at" ("cancelled", true, true) in
      let* () =
        probe "unknown status (no 'expired' state exists)"
          ("expired", false, false)
      in
      (* The two valid terminal shapes must pass — proves the CHECK is
         exact, not merely strict. *)
      let* ok =
        C.find q_insert_draft_lifecycle ((uid, inst), ("completed", true, false))
      in
      let* _ = Db_fixture.or_fail "completed with completed_at accepted" ok in
      let* ok =
        C.find q_insert_draft_lifecycle ((uid, inst), ("cancelled", false, true))
      in
      let* _ = Db_fixture.or_fail "cancelled with cancelled_at accepted" ok in
      let* r = C.find q_insert_draft_at ((uid, inst), (0, 0, 0)) in
      let* () = Db_fixture.reject "expires_at = created_at" r in
      let* r = C.find q_insert_draft_at ((uid, inst), (-10, 0, 0)) in
      let* () = Db_fixture.reject "expires_at < created_at" r in
      let* r = C.find q_insert_draft_at ((uid, inst), (3600, 0, -10)) in
      Db_fixture.reject "updated_at < created_at" r)

(* Foreign keys: ghosts rejected; users CASCADE through drafts to
   snapshots; installations are RESTRICT-protected while referenced. *)
let fk_case =
  db_case "foreign keys: ghost rejection, user cascade, installation restrict"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "poddraft_a" in
      let* inst = insert_installation conn 937000041L in
      let* ghost_user = C.find q_absent_user_id () in
      let* ghost_user = Db_fixture.or_fail "ghost user id" ghost_user in
      let* r = insert_draft conn ~user:ghost_user ~installation:inst in
      let* () = Db_fixture.reject "draft for nonexistent user" r in
      let* ghost_inst = C.find q_absent_installation_record_id () in
      let* ghost_inst = Db_fixture.or_fail "ghost installation id" ghost_inst in
      let* r = insert_draft conn ~user:uid ~installation:ghost_inst in
      let* () =
        Db_fixture.reject "draft for nonexistent installation record" r
      in
      (* Deleting the owner cascades to the draft and its snapshots. *)
      let* d1 = insert_draft_ok "draft" conn ~user:uid ~installation:inst in
      let* _ = insert_repo_ok "repo" ~repo_id:937600041L ~position:1 conn d1 in
      let* r = C.exec q_delete_user uid in
      let* () = Db_fixture.or_fail "delete user" r in
      let* n = C.find q_count_drafts_for_user uid in
      let* n = Db_fixture.or_fail "drafts after user delete" n in
      Alcotest.(check int) "user delete cascades to drafts" 0 n;
      let* n = C.find q_count_repos_for_draft d1 in
      let* n = Db_fixture.or_fail "repos after user delete" n in
      Alcotest.(check int) "user delete cascades through to snapshots" 0 n;
      (* A referenced installation record cannot be deleted; once its
         draft is gone the same delete succeeds. *)
      let* b = insert_user conn "poddraft_b" in
      let* d2 = insert_draft_ok "B draft" conn ~user:b ~installation:inst in
      let* _ =
        insert_repo_ok "B repo" ~repo_id:937600042L ~position:1 conn d2
      in
      let* r = C.exec q_delete_installation inst in
      let* () =
        Db_fixture.reject "deleting a referenced installation record" r
      in
      let* r = C.exec q_delete_draft d2 in
      let* () = Db_fixture.or_fail "delete draft" r in
      let* n = C.find q_count_repos_for_draft d2 in
      let* n = Db_fixture.or_fail "repos after draft delete" n in
      Alcotest.(check int) "draft delete cascades to snapshots" 0 n;
      let* r = C.exec q_delete_installation inst in
      Db_fixture.or_fail "unreferenced installation record deletes fine" r)

(* Snapshot constraints: positive external ids and position, per-draft
   uniqueness only (a snapshot is not a project claim), non-empty
   identity fields, nullable description, archived rows welcome. *)
let repo_constraints_case =
  db_case "snapshot rows: ids, ordering, per-draft uniqueness, identity fields"
    (fun conn ->
      let* uid = insert_user conn "poddraft_a" in
      let* i1 = insert_installation conn 937000051L in
      let* i2 = insert_installation conn 937000052L in
      let* d1 = insert_draft_ok "draft 1" conn ~user:uid ~installation:i1 in
      let* d2 = insert_draft_ok "draft 2" conn ~user:uid ~installation:i2 in
      let* r = insert_repo ~repo_id:0L ~position:1 conn d1 in
      let* () = Db_fixture.reject "repository id zero" r in
      let* r = insert_repo ~repo_id:(-937600051L) ~position:1 conn d1 in
      let* () = Db_fixture.reject "repository id negative" r in
      let* r =
        insert_repo ~repo_id:937600051L ~owner_id:0L ~position:1 conn d1
      in
      let* () = Db_fixture.reject "owner id zero" r in
      let* r = insert_repo ~repo_id:937600051L ~position:0 conn d1 in
      let* () = Db_fixture.reject "position zero" r in
      let* _ =
        insert_repo_ok "valid snapshot" ~repo_id:937600051L ~name:"alpha"
          ~position:1 conn d1
      in
      let* r =
        insert_repo ~repo_id:937600052L ~name:"beta" ~position:1 conn d1
      in
      let* () = Db_fixture.reject "duplicate position in one draft" r in
      let* r =
        insert_repo ~repo_id:937600051L ~name:"beta" ~position:2 conn d1
      in
      let* () = Db_fixture.reject "duplicate repository id in one draft" r in
      let* r =
        insert_repo ~repo_id:937600053L ~name:"alpha" ~position:2 conn d1
      in
      let* () = Db_fixture.reject "duplicate full name in one draft" r in
      let* _ =
        insert_repo_ok
          "same repository id in a different draft (not a global claim)"
          ~repo_id:937600051L ~name:"alpha" ~position:1 conn d2
      in
      (* Identity fields reject emptiness independently. *)
      let* r =
        insert_repo ~login:"" ~full_name:"poddraft-owner/x"
          ~html_url:"https://github.com/poddraft-owner/x" ~repo_id:937600054L
          ~position:3 conn d1
      in
      let* () = Db_fixture.reject "empty owner_login" r in
      let* r =
        insert_repo ~name:"" ~full_name:"poddraft-owner/x"
          ~html_url:"https://github.com/poddraft-owner/x" ~repo_id:937600054L
          ~position:3 conn d1
      in
      let* () = Db_fixture.reject "empty name" r in
      let* r =
        insert_repo ~name:"x" ~full_name:""
          ~html_url:"https://github.com/poddraft-owner/x" ~repo_id:937600054L
          ~position:3 conn d1
      in
      let* () = Db_fixture.reject "empty full_name" r in
      let* r =
        insert_repo ~name:"x" ~html_url:"" ~repo_id:937600054L ~position:3 conn
          d1
      in
      let* () = Db_fixture.reject "empty html_url" r in
      let* r =
        insert_repo ~name:"x" ~branch:"" ~repo_id:937600054L ~position:3 conn d1
      in
      let* () = Db_fixture.reject "empty default_branch" r in
      let* _ =
        insert_repo_ok "nullable description" ~name:"delta" ?description:None
          ~repo_id:937600055L ~position:3 conn d1
      in
      let* _ =
        insert_repo_ok "archived snapshot stays insertable" ~name:"epsilon"
          ~archived:true ~repo_id:937600056L ~position:4 conn d1
      in
      Lwt.return_unit)

(* Selection: primary implies selected; at most one primary per draft;
   separate drafts each get their own primary. *)
let selection_case =
  db_case "selection state: primary implies selected, one primary per draft"
    (fun conn ->
      let* uid = insert_user conn "poddraft_a" in
      let* i1 = insert_installation conn 937000061L in
      let* i2 = insert_installation conn 937000062L in
      let* d1 = insert_draft_ok "draft 1" conn ~user:uid ~installation:i1 in
      let* d2 = insert_draft_ok "draft 2" conn ~user:uid ~installation:i2 in
      let* _ =
        insert_repo_ok "unselected, non-primary" ~name:"alpha"
          ~repo_id:937600061L ~position:1 conn d1
      in
      let* _ =
        insert_repo_ok "selected, non-primary" ~name:"beta" ~selected:true
          ~repo_id:937600062L ~position:2 conn d1
      in
      let* _ =
        insert_repo_ok "selected primary" ~name:"gamma" ~selected:true
          ~primary:true ~repo_id:937600063L ~position:3 conn d1
      in
      let* r =
        insert_repo ~name:"delta" ~primary:true ~repo_id:937600064L ~position:4
          conn d1
      in
      let* () = Db_fixture.reject "primary while unselected" r in
      let* r =
        insert_repo ~name:"epsilon" ~selected:true ~primary:true
          ~repo_id:937600065L ~position:5 conn d1
      in
      let* () = Db_fixture.reject "second primary in one draft" r in
      let* _ =
        insert_repo_ok "separate draft gets its own primary" ~name:"zeta"
          ~selected:true ~primary:true ~repo_id:937600066L ~position:1 conn d2
      in
      Lwt.return_unit)

(* Credential-column prohibition: no column of either table may carry a
   credential-shaped name; column counts pin that both tables were
   actually inspected. *)
let credential_columns_case =
  db_case "no token- or secret-shaped column exists on either table"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* n = C.find Db_fixture.q_column_count "project_onboarding_drafts" in
      let* n = Db_fixture.or_fail "drafts column count" n in
      Alcotest.(check int) "project_onboarding_drafts has 10 columns" 10 n;
      let* n =
        C.find Db_fixture.q_column_count "project_onboarding_draft_repositories"
      in
      let* n = Db_fixture.or_fail "repositories column count" n in
      Alcotest.(check int)
        "project_onboarding_draft_repositories has 16 columns" 16 n;
      let* offenders = C.collect_list q_credential_columns () in
      let* offenders =
        Db_fixture.or_fail "credential-shaped columns" offenders
      in
      Alcotest.(check (list string)) "no credential-shaped column" [] offenders;
      Lwt.return_unit)

let suite =
  [
    fresh_case;
    ownership_case;
    active_unique_case;
    lifecycle_case;
    fk_case;
    repo_constraints_case;
    selection_case;
    credential_columns_case;
  ]

let suites =
  (* Project-onboarding draft schema: SQL-only slice, no store module
       yet — ownership, lifecycle, active-slot uniqueness, snapshot
       ordering/uniqueness, and the credential-column audit live in
       Postgres; same EARDE_TEST_DATABASE_URL gate (each case skips
       without it). *)
  [ ("project_onboarding_drafts_schema", suite) ]
