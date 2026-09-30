(* === Verified-project domain schema (SQL-only slice) ===
   No project store exists yet, so these cases pin the PostgreSQL
   contract of the permanent verified-project tables directly: provenance
   references that survive deletion (source draft and creator are both
   SET NULL — a permanent project outlives its onboarding draft and its
   creator), steward authorization as its own table (CASCADE from users
   and projects, RESTRICT from github_installations), the closed
   kind/forge/namespace/verification vocabularies, canonical slug rules,
   per-project repository ordering and identity uniqueness, the GLOBAL
   one-repository-one-project claim, at most one primary per project,
   and the absence of any credential-shaped column. Cross-row rules —
   at least one repository, kind-driven primary requirements, copying
   from the selected draft — belong to the future atomic finalization
   store, so a zero-repository project is structurally legal here. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope; constraint probes
   run in autocommit with osproj_% users, a reserved
   external-installation-id range 941000001..941000999 and a reserved
   namespace-id range 941100001..941100999 (every fixture project uses
   it) cleaned before and after each case. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail

let reject = Db_fixture.reject

(* Projects go first (stewards and repositories cascade from them, and
   stewards RESTRICT-protect installations); drafts next (they also
   RESTRICT-protect installations); then users and installations. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 941100001 AND 941100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 941100001 AND 941100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 941000001 AND 941000999)"
    ; "DELETE FROM users WHERE username LIKE 'osproj_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 941000001 AND 941000999"
    ]

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

(* Isolated installation fixtures on purpose: existing drafts may
   independently RESTRICT-protect shared installations, which would
   contaminate the steward-restrict probes. *)
let q_insert_installation =
  (Caqti_type.int64 ->! Caqti_type.int64)
  "INSERT INTO github_installations
     (github_installation_id, github_account_id, github_account_login,
      github_account_type)
   VALUES ($1, $1 + 100000, 'osproj-account', 'user')
   RETURNING id"

let q_insert_draft =
  (Caqti_type.(t2 int int64) ->! Caqti_type.int64)
  "INSERT INTO project_onboarding_drafts
     (user_id, github_installation_record_id, expires_at)
   VALUES ($1, $2, NOW() + INTERVAL '24 hours') RETURNING id"

let q_absent_installation_record_id =
  (Caqti_type.unit ->! Caqti_type.int64)
  "SELECT COALESCE(MAX(id), 0) + 1000000 FROM github_installations"

(* updated_at-ordering probe: everything else canonical. *)
let q_insert_project_backdated =
  (Caqti_type.string ->! Caqti_type.int64)
  "INSERT INTO open_source_projects
     (name, slug, kind, forge_namespace_id, forge_namespace_login,
      forge_namespace_type, updated_at)
   VALUES ('Backdated', $1, 'project', 941100001, 'osproj-owner',
           'user', NOW() - INTERVAL '10 seconds')
   RETURNING id"

(* Everything durable on one project row, in insert order, plus the
   timestamp-ordering flag. *)
let q_project_row =
  (Caqti_type.(int64 ->!
      t2 (t2 (t2 (option int64) (option int)) (t2 string string))
         (t2 (t2 (option string) (option string))
             (t2 (t2 (t3 string string int64)
                     (t3 string string string))
                 bool))))
  "SELECT source_onboarding_draft_id, created_by_user_id, name, slug,
          description, website_url, kind, forge, forge_namespace_id,
          forge_namespace_login, forge_namespace_type,
          verification_status, updated_at >= created_at
   FROM open_source_projects WHERE id = $1"

let q_project_source =
  (Caqti_type.int64 ->! Caqti_type.(option int64))
  "SELECT source_onboarding_draft_id FROM open_source_projects
   WHERE id = $1"

let q_project_creator =
  (Caqti_type.int64 ->! Caqti_type.(option int))
  "SELECT created_by_user_id FROM open_source_projects WHERE id = $1"

let q_steward_role =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.string)
  "SELECT role FROM project_stewards
   WHERE project_id = $1 AND user_id = $2"

let q_count_stewards_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_stewards WHERE project_id = $1"

(* One text signature per repository row, ordered by position — pins
   the exact stored metadata and the ordering in one comparison. *)
let q_repo_signatures =
  (Caqti_type.int64 ->* Caqti_type.string)
  "SELECT position::text || '|' || github_repository_id::text || '|' ||
          full_name || '|' || html_url || '|' ||
          COALESCE(description, '<null>') || '|' || default_branch
          || '|' || is_primary::text || '|' || is_archived::text
   FROM project_repositories WHERE project_id = $1 ORDER BY position"

let q_count_repos_for_project =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_repositories WHERE project_id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM users WHERE id = $1"

let q_delete_installation =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM github_installations WHERE id = $1"

let q_delete_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM project_onboarding_drafts WHERE id = $1"

let q_delete_project =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM open_source_projects WHERE id = $1"

(* Schema audit scoped to exactly the three new tables. *)
let q_credential_columns =
  (Caqti_type.unit ->* Caqti_type.string)
  "SELECT table_name || '.' || column_name
   FROM information_schema.columns
   WHERE table_schema = 'public'
     AND table_name IN ('open_source_projects', 'project_stewards',
                        'project_repositories')
     AND (column_name ~* 'token' OR column_name ~* 'secret'
          OR column_name ~* 'verifier'
          OR column_name ~* 'authorization_code'
          OR column_name ~* 'oauth_code' OR column_name ~* 'raw_state'
          OR column_name ~* 'session_binding'
          OR column_name ~* 'private_key')"

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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let insert_user conn username =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* uid = C.find q_insert_user username in
  or_fail ("user " ^ username) uid

let insert_installation conn ext_id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rid = C.find q_insert_installation ext_id in
  or_fail "installation" rid

let insert_draft_ok label conn ~user ~installation =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* d = C.find q_insert_draft (user, installation) in
  or_fail label d

(* Project-insert helper: canonical valid values, overridable per
   probe; the namespace id defaults into the reserved cleanup range. *)
let insert_project ?source_draft ?creator ?(name = "Fixture Project")
    ~slug ?description ?website ?(kind = "project") ?(forge = "github")
    ?(namespace_id = 941100001L) ?(login = "osproj-owner")
    ?(namespace_type = "user") ?(status = "verified") conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.find Project_fixture.q_insert_project
    ( ((source_draft, creator), (name, slug))
    , ( (description, website)
      , ((kind, forge, namespace_id), (login, namespace_type, status)) )
    )

let insert_project_ok label ?source_draft ?creator ?name ~slug
    ?description ?website ?kind ?forge ?namespace_id ?login
    ?namespace_type ?status conn =
  let* r =
    insert_project ?source_draft ?creator ?name ~slug ?description
      ?website ?kind ?forge ?namespace_id ?login ?namespace_type ?status
      conn
  in
  or_fail label r

let insert_repo ?(full_name = "osproj-owner/repo") ?html_url
    ?description ?(branch = "main") ?(primary = false)
    ?(archived = false) ~repo_id ~position conn project =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let html_url =
    match html_url with
    | Some u -> u
    | None -> "https://github.com/" ^ full_name
  in
  C.find Project_fixture.q_insert_repo
    ( ((project, position), repo_id)
    , ((full_name, html_url, description), (branch, primary, archived))
    )

let insert_repo_ok label ?full_name ?html_url ?description ?branch
    ?primary ?archived ~repo_id ~position conn project =
  let* r =
    insert_repo ?full_name ?html_url ?description ?branch ?primary
      ?archived ~repo_id ~position conn project
  in
  or_fail label r

let insert_steward_ok label conn triple =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.exec Project_fixture.q_insert_steward triple in
  or_fail label r

(* Full round-trip of one maximal project (source draft, creator,
   description, website), acceptance of every kind, both namespace
   types, each verification status, nullable description/website (the
   kind fixtures carry neither), a defaulted-role steward, and an
   ordered repository set with one primary, an archived member, and a
   slash-bearing default branch. *)
let fresh_case =
  db_case "fresh projects: full round-trip, every closed vocabulary"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "osproj_a" in
      let* inst = insert_installation conn 941000001L in
      let* draft =
        insert_draft_ok "draft" conn ~user:uid ~installation:inst
      in
      let* p =
        insert_project_ok "full project" ~source_draft:draft ~creator:uid
          ~name:"OCaml Platform" ~slug:"osproj-full"
          ~description:"Descrizione — byte exact"
          ~website:"https://ocaml.org" conn
      in
      let* row = C.find q_project_row p in
      let* ( ((source, creator), (name, slug))
           , ( (description, website)
             , (((kind, forge, ns_id), (login, ns_type, status)),
                updated_ge) ) ) =
        or_fail "project row" row
      in
      Alcotest.(check (option int64)) "source draft stored" (Some draft)
        source;
      Alcotest.(check (option int)) "creator stored" (Some uid) creator;
      Alcotest.(check string) "name byte-exact" "OCaml Platform" name;
      Alcotest.(check string) "slug" "osproj-full" slug;
      Alcotest.(check (option string)) "description byte-exact"
        (Some "Descrizione — byte exact") description;
      Alcotest.(check (option string)) "website"
        (Some "https://ocaml.org") website;
      Alcotest.(check string) "kind" "project" kind;
      Alcotest.(check string) "forge" "github" forge;
      Alcotest.(check int64) "namespace id" 941100001L ns_id;
      Alcotest.(check string) "namespace login" "osproj-owner" login;
      Alcotest.(check string) "namespace type" "user" ns_type;
      Alcotest.(check string) "verification status" "verified" status;
      Alcotest.(check bool) "updated_at >= created_at" true updated_ge;
      (* Remaining kinds — draftless, creatorless, no description or
         website: the minimal permanent shape. *)
      let* () =
        Lwt_list.iter_s
          (fun (k, slug) ->
            let* _ = insert_project_ok ("kind " ^ k) ~kind:k ~slug conn in
            Lwt.return_unit)
          [ ("organization", "osproj-kind-organization")
          ; ("ecosystem", "osproj-kind-ecosystem")
          ; ("foundation", "osproj-kind-foundation")
          ; ("working_group", "osproj-kind-working-group")
          ; ("other", "osproj-kind-other")
          ]
      in
      let* _ =
        insert_project_ok "organization namespace"
          ~namespace_type:"organization" ~namespace_id:941100002L
          ~login:"osproj-org" ~slug:"osproj-ns-org" conn
      in
      let* _ =
        insert_project_ok "stale status" ~status:"stale"
          ~slug:"osproj-status-stale" conn
      in
      let* _ =
        insert_project_ok "revoked status" ~status:"revoked"
          ~slug:"osproj-status-revoked" conn
      in
      let* () = insert_steward_ok "steward" conn (p, uid, inst) in
      let* role = C.find q_steward_role (p, uid) in
      let* role = or_fail "steward role" role in
      Alcotest.(check string) "role defaults to steward" "steward" role;
      let* _ =
        insert_repo_ok "repo 1" ~full_name:"osproj-owner/alpha"
          ~description:"Prima" ~primary:true ~repo_id:941600001L
          ~position:1 conn p
      in
      let* _ =
        insert_repo_ok "repo 2" ~full_name:"osproj-owner/beta"
          ~archived:true ~repo_id:941600002L ~position:2 conn p
      in
      let* _ =
        insert_repo_ok "repo 3" ~full_name:"osproj-owner/gamma"
          ~branch:"release/v1" ~repo_id:941600003L ~position:3 conn p
      in
      let* sigs = C.collect_list q_repo_signatures p in
      let* sigs = or_fail "signatures" sigs in
      Alcotest.(check (list string))
        "repository metadata and order preserved exactly"
        [ "1|941600001|osproj-owner/alpha|\
           https://github.com/osproj-owner/alpha|Prima|main|true|false"
        ; "2|941600002|osproj-owner/beta|\
           https://github.com/osproj-owner/beta|<null>|main|false|true"
        ; "3|941600003|osproj-owner/gamma|\
           https://github.com/osproj-owner/gamma|<null>|release/v1|\
           false|false"
        ]
        sigs;
      Lwt.return_unit)

(* Slug canon: representative real-world slugs pass (including the
   80-char boundary); every malformation and the global duplicate are
   rejected independently. Reserved words are deliberately NOT a SQL
   concern — the future pure domain parser owns them. *)
let slug_case =
  db_case "slug: canonical forms accepted, every malformation rejected"
    (fun conn ->
      let* () =
        Lwt_list.iter_s
          (fun slug ->
            let* _ = insert_project_ok ("slug " ^ slug) ~slug conn in
            Lwt.return_unit)
          [ "ocaml"; "lwt"; "ocaml-platform"; "project2";
            String.make 80 'a' ]
      in
      let probe label slug =
        let* r = insert_project ~slug conn in
        reject label r
      in
      let* () = probe "blank slug" "" in
      let* () = probe "uppercase slug" "OCaml" in
      let* () = probe "leading hyphen" "-ocaml" in
      let* () = probe "trailing hyphen" "ocaml-" in
      let* () = probe "consecutive hyphens" "ocaml--platform" in
      let* () = probe "underscore" "ocaml_platform" in
      let* () = probe "inner whitespace" "ocaml platform" in
      let* () = probe "surrounding whitespace" " ocaml " in
      let* () = probe "slash" "ocaml/platform" in
      let* () = probe "overlength (81 chars)" (String.make 81 'a') in
      probe "duplicate slug (global uniqueness)" "ocaml")

(* Field bounds: the boundary values pass — the checks are exact, not
   merely strict — and every structural violation rejects on its own. *)
let fields_case =
  db_case "project fields: every structural bound rejects independently"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* _ =
        insert_project_ok "120-char name" ~name:(String.make 120 'n')
          ~slug:"osproj-name-max" conn
      in
      let* _ =
        insert_project_ok "2000-char description"
          ~description:(String.make 2000 'd') ~slug:"osproj-desc-max"
          conn
      in
      let* _ =
        insert_project_ok "2048-char website"
          ~website:("https://" ^ String.make 2040 'w')
          ~slug:"osproj-web-max" conn
      in
      let probe label ?name ?description ?website ?kind ?forge
          ?namespace_id ?login ?namespace_type ?status () =
        let* r =
          insert_project ?name ?description ?website ?kind ?forge
            ?namespace_id ?login ?namespace_type ?status
            ~slug:"osproj-reject" conn
        in
        reject label r
      in
      let* () = probe "blank name" ~name:"" () in
      let* () = probe "leading-space name" ~name:" Padded" () in
      let* () = probe "trailing-space name" ~name:"Padded " () in
      let* () = probe "overlength name" ~name:(String.make 121 'n') () in
      let* () =
        probe "overlength description"
          ~description:(String.make 2001 'd') ()
      in
      let* () = probe "blank website" ~website:"" () in
      let* () =
        probe "trim-damaged website" ~website:" https://ocaml.org" ()
      in
      let* () =
        probe "overlength website"
          ~website:("https://" ^ String.make 2041 'w') ()
      in
      let* () = probe "invalid kind" ~kind:"community" () in
      let* () = probe "invalid forge" ~forge:"gitlab" () in
      let* () = probe "namespace id zero" ~namespace_id:0L () in
      let* () =
        probe "namespace id negative" ~namespace_id:(-941100001L) ()
      in
      let* () = probe "blank namespace login" ~login:"" () in
      let* () =
        probe "trim-damaged namespace login" ~login:" osproj-owner" ()
      in
      let* () = probe "invalid namespace type" ~namespace_type:"bot" () in
      let* () =
        probe "invalid verification status" ~status:"pending" ()
      in
      let* r = C.find q_insert_project_backdated "osproj-backdated" in
      reject "updated_at < created_at" r)

(* Source draft: while the draft lives it can have produced at most one
   project; the reference dies before the project does — draft
   deletion, and user deletion cascading through drafts, both leave
   the permanent project standing with a NULL source. *)
let source_draft_case =
  db_case "source draft: one project per live draft, SET NULL survival"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "osproj_a" in
      let* i1 = insert_installation conn 941000011L in
      let* i2 = insert_installation conn 941000012L in
      let* d1 = insert_draft_ok "draft 1" conn ~user:uid ~installation:i1 in
      let* d2 = insert_draft_ok "draft 2" conn ~user:uid ~installation:i2 in
      let* p1 =
        insert_project_ok "from draft 1" ~source_draft:d1
          ~slug:"osproj-src-1" conn
      in
      let* r = insert_project ~source_draft:d1 ~slug:"osproj-src-1b" conn in
      let* () = reject "second project from the same live draft" r in
      let* _ =
        insert_project_ok "second draft produces its own project"
          ~source_draft:d2 ~slug:"osproj-src-2" conn
      in
      let* _ =
        insert_project_ok "draftless project (admin/import path)"
          ~slug:"osproj-src-none" conn
      in
      let* r = C.exec q_delete_draft d1 in
      let* () = or_fail "delete draft" r in
      let* source = C.find q_project_source p1 in
      let* source = or_fail "source after draft delete" source in
      Alcotest.(check (option int64)) "source reference went NULL" None
        source;
      let* n = C.find Project_fixture.q_project_exists p1 in
      let* n = or_fail "project after draft delete" n in
      Alcotest.(check int) "project survives draft deletion" 1 n;
      (* User deletion cascades onboarding drafts; the permanent
         project must ride it out. *)
      let* b = insert_user conn "osproj_b" in
      let* i3 = insert_installation conn 941000013L in
      let* d3 = insert_draft_ok "B draft" conn ~user:b ~installation:i3 in
      let* p2 =
        insert_project_ok "B project" ~source_draft:d3 ~creator:b
          ~namespace_id:941100003L ~slug:"osproj-src-3" conn
      in
      let* r = C.exec q_delete_user b in
      let* () = or_fail "delete user B" r in
      let* n = C.find Project_fixture.q_project_exists p2 in
      let* n = or_fail "project after user delete" n in
      Alcotest.(check int) "project survives source-user deletion" 1 n;
      let* source = C.find q_project_source p2 in
      let* source = or_fail "source after user delete" source in
      Alcotest.(check (option int64))
        "cascaded draft left a NULL source reference" None source;
      Lwt.return_unit)

(* Creator: provenance only. Deleting the creator nulls the field and
   nothing else — administration lives in project_stewards, shown here
   held by a different user who survives untouched. *)
let creator_case =
  db_case "creator: provenance only — SET NULL, never authorization"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* creator = insert_user conn "osproj_a" in
      let* steward = insert_user conn "osproj_b" in
      let* inst = insert_installation conn 941000021L in
      let* p =
        insert_project_ok "project" ~creator ~slug:"osproj-creator" conn
      in
      let* () = insert_steward_ok "steward" conn (p, steward, inst) in
      let* r = C.exec q_delete_user creator in
      let* () = or_fail "delete creator" r in
      let* n = C.find Project_fixture.q_project_exists p in
      let* n = or_fail "project after creator delete" n in
      Alcotest.(check int) "project survives creator deletion" 1 n;
      let* c = C.find q_project_creator p in
      let* c = or_fail "creator after delete" c in
      Alcotest.(check (option int)) "creator reference went NULL" None c;
      let* role = C.find q_steward_role (p, steward) in
      let* role = or_fail "surviving steward" role in
      Alcotest.(check string)
        "stewardship unaffected by creator deletion" "steward" role;
      Lwt.return_unit)

(* Stewards: one row per (project, user), multiple stewards allowed,
   ghosts rejected, user deletion removes only the stewardship,
   project deletion removes them all, and the installation proof is
   RESTRICT-protected while referenced. *)
let steward_case =
  db_case "stewards: uniqueness, ghosts, cascades, installation restrict"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* a = insert_user conn "osproj_a" in
      let* b = insert_user conn "osproj_b" in
      let* inst = insert_installation conn 941000031L in
      let* p = insert_project_ok "project" ~slug:"osproj-stewards" conn in
      let* () = insert_steward_ok "first steward" conn (p, a, inst) in
      let* () =
        insert_steward_ok "second steward, same project" conn (p, b, inst)
      in
      let* r = C.exec Project_fixture.q_insert_steward (p, a, inst) in
      let* () = reject "duplicate (project, user) stewardship" r in
      let* ghost_p = C.find Project_fixture.q_absent_project_id () in
      let* ghost_p = or_fail "ghost project id" ghost_p in
      let* r = C.exec Project_fixture.q_insert_steward (ghost_p, a, inst) in
      let* () = reject "steward for nonexistent project" r in
      let* ghost_u = C.find Project_fixture.q_absent_user_id () in
      let* ghost_u = or_fail "ghost user id" ghost_u in
      let* r = C.exec Project_fixture.q_insert_steward (p, ghost_u, inst) in
      let* () = reject "steward for nonexistent user" r in
      let* ghost_i = C.find q_absent_installation_record_id () in
      let* ghost_i = or_fail "ghost installation record id" ghost_i in
      let* r = C.exec Project_fixture.q_insert_steward (p, a, ghost_i) in
      let* () = reject "steward for nonexistent installation record" r in
      (* User deletion removes only that stewardship; the project and
         the other steward remain. *)
      let* r = C.exec q_delete_user b in
      let* () = or_fail "delete steward B" r in
      let* n = C.find q_count_stewards_for_project p in
      let* n = or_fail "stewards after user delete" n in
      Alcotest.(check int) "only B's stewardship cascaded" 1 n;
      let* n = C.find Project_fixture.q_project_exists p in
      let* n = or_fail "project after steward delete" n in
      Alcotest.(check int) "project survives steward deletion" 1 n;
      (* The installation proof cannot be deleted from under the
         remaining steward; project deletion frees it. *)
      let* r = C.exec q_delete_installation inst in
      let* () = reject "deleting a steward-referenced installation" r in
      let* r = C.exec q_delete_project p in
      let* () = or_fail "delete project" r in
      let* n = C.find q_count_stewards_for_project p in
      let* n = or_fail "stewards after project delete" n in
      Alcotest.(check int) "project delete cascades stewardships" 0 n;
      let* r = C.exec q_delete_installation inst in
      or_fail "unreferenced installation deletes fine" r)

(* Repository rows: positive ids and positions, per-project ordering
   and display-identity uniqueness, at most one primary per project
   (each project gets its own), nullable description, slash-bearing
   default branch, archived members, and CASCADE from the project. *)
let repo_constraints_case =
  db_case "repositories: ordering, per-project identity, one primary"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* p1 = insert_project_ok "project 1" ~slug:"osproj-repos-1" conn in
      let* p2 = insert_project_ok "project 2" ~slug:"osproj-repos-2" conn in
      let* r = insert_repo ~repo_id:0L ~position:1 conn p1 in
      let* () = reject "repository id zero" r in
      let* r = insert_repo ~repo_id:(-941600011L) ~position:1 conn p1 in
      let* () = reject "repository id negative" r in
      let* r = insert_repo ~repo_id:941600011L ~position:0 conn p1 in
      let* () = reject "position zero" r in
      let* r = insert_repo ~repo_id:941600011L ~position:(-1) conn p1 in
      let* () = reject "position negative" r in
      let* _ =
        insert_repo_ok "first" ~full_name:"osproj-owner/alpha"
          ~primary:true ~repo_id:941600011L ~position:1 conn p1
      in
      let* _ =
        insert_repo_ok "second" ~full_name:"osproj-owner/beta"
          ~repo_id:941600012L ~position:2 conn p1
      in
      let* r =
        insert_repo ~full_name:"osproj-owner/gamma" ~repo_id:941600013L
          ~position:2 conn p1
      in
      let* () = reject "duplicate position in one project" r in
      let* r =
        insert_repo ~full_name:"osproj-owner/alpha" ~repo_id:941600013L
          ~position:3 conn p1
      in
      let* () = reject "duplicate full name in one project" r in
      let* r =
        insert_repo ~full_name:"osproj-owner/gamma" ~primary:true
          ~repo_id:941600013L ~position:3 conn p1
      in
      let* () = reject "second primary in one project" r in
      let* _ =
        insert_repo_ok "other project's own primary"
          ~full_name:"osproj-owner/delta" ~primary:true
          ~repo_id:941600014L ~position:1 conn p2
      in
      let* r =
        insert_repo ~full_name:""
          ~html_url:"https://github.com/osproj-owner/eps"
          ~repo_id:941600015L ~position:3 conn p1
      in
      let* () = reject "empty full_name" r in
      let* r =
        insert_repo ~full_name:"osproj-owner/eps" ~html_url:""
          ~repo_id:941600015L ~position:3 conn p1
      in
      let* () = reject "empty html_url" r in
      let* r =
        insert_repo ~full_name:"osproj-owner/eps" ~branch:""
          ~repo_id:941600015L ~position:3 conn p1
      in
      let* () = reject "empty default_branch" r in
      let* _ =
        insert_repo_ok "nullable description, slash branch, archived"
          ~full_name:"osproj-owner/eps" ~branch:"release/2026/lts"
          ~archived:true ~repo_id:941600015L ~position:3 conn p1
      in
      let* r = C.exec q_delete_project p1 in
      let* () = or_fail "delete project" r in
      let* n = C.find q_count_repos_for_project p1 in
      let* n = or_fail "repos after project delete" n in
      Alcotest.(check int) "project delete cascades repositories" 0 n;
      Lwt.return_unit)

(* The load-bearing MVP claim rule: one GitHub repository belongs to at
   most one permanent project, globally — the database constraint is
   the arbiter of concurrent finalization races, so no disguise
   (different name, position, namespace, creator) may get around it.
   Draft snapshots stay exempt by design. *)
let global_claim_case =
  db_case "one GitHub repository belongs to at most one project globally"
    (fun conn ->
      let* a = insert_user conn "osproj_a" in
      let* b = insert_user conn "osproj_b" in
      let* p1 =
        insert_project_ok "project 1" ~creator:a ~slug:"osproj-claim-1"
          conn
      in
      let* p2 =
        insert_project_ok "project 2" ~creator:b
          ~namespace_id:941100002L ~namespace_type:"organization"
          ~login:"osproj-org" ~slug:"osproj-claim-2" conn
      in
      let* _ =
        insert_repo_ok "first claim" ~full_name:"osproj-owner/alpha"
          ~repo_id:941600021L ~position:1 conn p1
      in
      let* r =
        insert_repo ~full_name:"osproj-org/renamed" ~repo_id:941600021L
          ~position:7 conn p2
      in
      let* () =
        reject
          "same repository id in a second project (different full \
           name, position, namespace, creator)" r
      in
      let* _ =
        insert_repo_ok "different repository may reuse the position"
          ~full_name:"osproj-org/beta" ~repo_id:941600022L ~position:1
          conn p2
      in
      Lwt.return_unit)

(* "At least one repository" is deliberately NOT a schema rule: the
   future atomic finalization store creates project, steward, and
   repositories in one transaction and owns that guarantee (along with
   kind-driven primary requirements), so the schema must admit the
   zero-repository shape rather than be weakened to test it. *)
let zero_repos_case =
  db_case "a zero-repository project is structurally legal"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* p = insert_project_ok "bare project" ~slug:"osproj-bare" conn in
      let* n = C.find q_count_repos_for_project p in
      let* n = or_fail "repository count" n in
      Alcotest.(check int) "zero repositories" 0 n;
      let* n = C.find Project_fixture.q_project_exists p in
      let* n = or_fail "project exists" n in
      Alcotest.(check int) "project row exists" 1 n;
      Lwt.return_unit)

(* Credential-column prohibition across all three tables; the column
   counts pin that each table was actually inspected, so the audit
   cannot pass vacuously. *)
let credential_columns_case =
  db_case "no credential-shaped column exists on any of the three tables"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* () =
        Lwt_list.iter_s
          (fun (table, expected) ->
            let* n = C.find Db_fixture.q_column_count table in
            let* n = or_fail (table ^ " column count") n in
            Alcotest.(check int) (table ^ " column count") expected n;
            Lwt.return_unit)
          [ ("open_source_projects", 15)
          ; ("project_stewards", 6)
          ; ("project_repositories", 13)
          ]
      in
      let* offenders = C.collect_list q_credential_columns () in
      let* offenders = or_fail "credential-shaped columns" offenders in
      Alcotest.(check (list string)) "no credential-shaped column" []
        offenders;
      Lwt.return_unit)

let suite =
  [ fresh_case; slug_case; fields_case; source_draft_case; creator_case;
    steward_case; repo_constraints_case; global_claim_case;
    zero_repos_case; credential_columns_case ]

let suites =
    (* Verified-project domain schema: SQL-only slice, no store yet —
       provenance SET NULLs, steward cascades/restrict, closed
       vocabularies, slug canon, the global repository claim, one
       primary per project, and the credential-column audit live in
       Postgres; same EARDE_TEST_DATABASE_URL gate (each case skips
       without it). *)
  [ ( "open_source_projects_schema", suite )
  ]
