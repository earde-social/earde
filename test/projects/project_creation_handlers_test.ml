module Ob = Earde.Project_onboarding
module Pi = Earde.Project_identity

(* === Permanent project creation (Project_creation_handlers,
   Project_home_setup_read_model, Project_home_setup_pages) ===
   POST /projects and the permanent GET /projects/:slug/setup destination,
   plus the steward-authorized permanent read model and the pure
   "Project created" page renderer. DB-free suites cover rendering, the
   access gates, and the POST's origin/CSRF policy (every rejection
   precedes SQL). Database-gated suites run the real pipeline (sql_pool +
   secret + memory sessions + the real router shape) under the usual
   EARDE_TEST_DATABASE_URL opt-in, with their own reserved
   external-installation-id range 943000001..943000999 (hence account ids
   943100001..943100999, which also scope the permanent-project cleanup)
   and pcreate_% usernames so no suite shares fixtures. Credential and
   submitted-value assertions are boolean, so no fixture byte reaches test
   output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Hrm = Earde.Project_home_setup_read_model

module Php = Earde.Project_home_setup_pages

module Pc = Earde.Project_creation_handlers

module Psh = Earde.Project_setup_handlers

module Store = Earde.Project_onboarding_draft_store

module Gui = Earde.Github_user_installations

let case = Case.quick

let counting_loader = Http_fixture.counting_loader

let ok_loader = Http_fixture.ok_loader

let status_of = Http_fixture.status_of

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let find_opt conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find_opt q arg in
  or_fail label r

let post_target = "/projects"

let setup_target slug = Printf.sprintf "/projects/%s/setup" slug

let make_post ~mode ~load_config =
  Pc.make_project_creation_handler ~mode ~load_config

(* --- Fixtures --- *)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 943100001 AND 943100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 943100001 AND 943100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 943000001 AND 943000999)"
    ; "DELETE FROM users WHERE username LIKE 'pcreate_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 943000001 AND 943000999"
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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let repo ?(owner_login = "pcreate-owner") ~account_id ?description
    ?default_branch ?archived ~id name =
  Github_fixture.gur_repo ~owner_id:account_id ~owner_login ?description ?default_branch
    ?archived ~id ~name ()

(* Draft fixtures go through the real store against the real client
   chain, exactly as production writes them. *)
let make_draft ?(login = "pcreate-owner") ?(target = "User")
    ?(installation_type = "user") ?token_body ?connected_by conn ~user
    ~ext_id repos =
  let account_id = Int64.add ext_id 100000L in
  let* inst =
    Project_fixture.insert_installation ~login ~account_type:installation_type
      ?connected_by conn ~ext_id ~account_id
  in
  let* v =
    Project_fixture.verified ?token_body ~installation_id:ext_id ~account_id
      ~login ~target ()
  in
  let* set = Project_fixture.repo_set ?token_body ~installation:v (repos account_id) in
  let* draft = Project_fixture.refresh_ok "fixture refresh" conn ~user v set in
  Lwt.return (inst, Store.draft_id draft, account_id)

let snapshot_ids conn draft =
  collect conn "snapshot ids" Project_fixture.q_snapshot_ids draft

(* Direct permanent-project fixtures for the read-model and destination
   suites, through the schema suite's insert queries — states the
   finalization store deliberately never produces on demand. *)
let insert_project conn ?(kind = "project") ?(status = "verified")
    ?description ?website ?source ?creator ?(login = "pcreate-owner")
    ?(ns_type = "user") ~ns_id ~project_name ~slug () =
  find conn "insert project" Project_fixture.q_insert_project
    ( ((source, creator), (project_name, slug)),
      ((description, website), ((kind, "github", ns_id), (login, ns_type, status)))
    )

let insert_steward conn ~project ~user ~installation =
  exec conn "insert steward" Project_fixture.q_insert_steward
    (project, user, installation)

let insert_repo conn ~project ~position ?(owner = "pcreate-owner")
    ?description ?(branch = "main") ?(primary = false) ?(archived = false)
    ~gh_id name =
  let full_name = owner ^ "/" ^ name in
  let html_url = "https://github.com/" ^ owner ^ "/" ^ name in
  let* _ =
    find conn "insert repo" Project_fixture.q_insert_repo
      ( ((project, position), gh_id),
        ((full_name, html_url, description), (branch, primary, archived)) )
  in
  Lwt.return_unit

(* Isolated corruption fixtures for invariants the schema does not own. *)
let q_set_repo_html_url =
  (Caqti_type.(t3 int64 int string) ->. Caqti_type.unit)
  "UPDATE project_repositories SET html_url = $3 \
   WHERE project_id = $1 AND position = $2"

let q_poison_repo_description =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "UPDATE project_repositories \
   SET description = 'bad' || CHR(1) || 'description' \
   WHERE project_id = $1 AND position = $2"

let q_delete_repo_position =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "DELETE FROM project_repositories \
   WHERE project_id = $1 AND position = $2"

let q_set_project_status =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
  "UPDATE open_source_projects SET verification_status = $2 WHERE id = $1"

let q_set_project_creator =
  (Caqti_type.(t2 int64 (option int)) ->. Caqti_type.unit)
  "UPDATE open_source_projects SET created_by_user_id = $2 WHERE id = $1"

let q_draft_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM project_onboarding_drafts WHERE id = $1"

(* Race fixtures: the draft row lock serializes finalization, so holding
   it from the test connection makes "selection changed between the
   handler's read and its finalize" deterministic. *)
let q_lock_draft =
  (Caqti_type.int64 ->! Caqti_type.int64)
  "SELECT id FROM project_onboarding_drafts WHERE id = $1 FOR UPDATE"

let q_unselect_primary =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories \
   SET is_primary = FALSE, is_selected = FALSE \
   WHERE draft_id = $1 AND is_primary"

let q_unselect_all =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories \
   SET is_selected = FALSE, is_primary = FALSE WHERE draft_id = $1"

let tx_start conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.start () in
  or_fail "tx start" r

let tx_commit conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.commit () in
  or_fail "tx commit" r

(* Test-only failure injection for the finalization Storage_error case:
   a trigger scoped to one reserved fixture repository id, firing only
   on the permanent copy. Installed and dropped inside that case alone
   (IF EXISTS drops keep the cleanup idempotent); production migrations
   are untouched. *)
let pch_poison_repo_id = 943999999L

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION pch_fail_insert_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''pch fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER pch_fail_insert
   BEFORE INSERT ON project_repositories
   FOR EACH ROW WHEN (NEW.github_repository_id = 943999999)
   EXECUTE FUNCTION pch_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS pch_fail_insert ON project_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS pch_fail_insert_fn()"

(* === Read model: call helpers === *)

let hrm_error_str : Hrm.error -> string = function
  | Hrm.Invalid_user_id -> "Invalid_user_id"
  | Hrm.Invalid_slug -> "Invalid_slug"
  | Hrm.Inconsistent_data -> "Inconsistent_data"
  | Hrm.Storage_error -> "Storage_error"

let load conn ~user ~slug = Hrm.load_for_steward conn ~user_id:user ~slug

let load_project label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok (Some project) -> Lwt.return project
  | Ok None -> Alcotest.failf "%s: unexpectedly absent" label
  | Error e -> Alcotest.failf "%s: %s" label (hrm_error_str e)

let expect_none label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Alcotest.failf "%s: unexpectedly loaded" label
  | Error e -> Alcotest.failf "%s: %s" label (hrm_error_str e)

let expect_error label expected conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label
              (hrm_error_str expected)
  | Error e ->
      Alcotest.(check string) label (hrm_error_str expected)
        (hrm_error_str e);
      Lwt.return_unit

(* === Read model: database-gated cases === *)

let rm_input_case =
  db_case "read model: invalid users and slugs rejected before SQL"
    (fun ~url:_ conn ->
      let* () =
        Lwt_list.iter_s
          (fun user ->
            expect_error
              (Printf.sprintf "user %d" user)
              Hrm.Invalid_user_id conn ~user ~slug:"pch-valid")
          [ 0; -3 ]
      in
      Lwt_list.iter_s
        (fun slug ->
          expect_error ("slug " ^ String.escaped slug) Hrm.Invalid_slug
            conn ~user:42 ~slug)
        [ ""; "-lead"; "trail-"; "a--b"; "Upper"; "under_score";
          "spa ce"; " pch"; "pch "; "pch%2Da"; "héllo";
          String.make 81 'a';
        ])

let rm_owner_view_case =
  db_case "read model: steward loads exact metadata and repository order"
    (fun ~url:_ conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-owner" conn
          ~ext_id:943000001L ~account_id:943100001L
      in
      let* project =
        insert_project conn ~ns_id:943100001L
          ~description:"Descrizione — esatta"
          ~website:"https://example.com/pch?x=1#frag"
          ~project_name:"Pch Fixture" ~slug:"pch-owner-view" ()
      in
      let* () = insert_steward conn ~project ~user:uid ~installation:inst in
      let* () =
        insert_repo conn ~project ~position:1 ~gh_id:943600011L
          ~description:"Alpha description" ~primary:true "alpha"
      in
      let* () =
        insert_repo conn ~project ~position:2 ~gh_id:943600012L
          ~branch:"release/v1" ~archived:true "beta"
      in
      let* view = load_project "owner" conn ~user:uid ~slug:"pch-owner-view" in
      Alcotest.(check bool) "positive id" true
        (Int64.compare (Hrm.project_id view) 0L > 0);
      Alcotest.(check string) "name" "Pch Fixture" (Hrm.name view);
      Alcotest.(check string) "slug" "pch-owner-view" (Hrm.slug view);
      Alcotest.(check (option string)) "description"
        (Some "Descrizione — esatta") (Hrm.description view);
      Alcotest.(check (option string)) "website"
        (Some "https://example.com/pch?x=1#frag") (Hrm.website_url view);
      Alcotest.(check bool) "kind" true (Hrm.kind view = Pi.Project);
      Alcotest.(check string) "login" "pcreate-owner"
        (Hrm.namespace_login view);
      Alcotest.(check bool) "namespace type" true
        (Hrm.namespace_type view = Gui.User);
      let repos = Hrm.repositories view in
      Alcotest.(check int) "two repositories" 2 (List.length repos);
      let first = List.nth repos 0 and second = List.nth repos 1 in
      Alcotest.(check int) "first position" 1 (Hrm.position first);
      Alcotest.(check string) "first full name" "pcreate-owner/alpha"
        (Hrm.full_name first);
      Alcotest.(check string) "first url"
        "https://github.com/pcreate-owner/alpha" (Hrm.html_url first);
      Alcotest.(check (option string)) "first description"
        (Some "Alpha description")
        (Hrm.repository_description first);
      Alcotest.(check bool) "first primary" true (Hrm.is_primary first);
      Alcotest.(check bool) "first not archived" false
        (Hrm.is_archived first);
      Alcotest.(check int) "second position" 2 (Hrm.position second);
      Alcotest.(check string) "second full name" "pcreate-owner/beta"
        (Hrm.full_name second);
      Alcotest.(check string) "second branch" "release/v1"
        (Hrm.default_branch second);
      Alcotest.(check (option string)) "second description" None
        (Hrm.repository_description second);
      Alcotest.(check bool) "second archived" true (Hrm.is_archived second);
      Alcotest.(check bool) "second not primary" false
        (Hrm.is_primary second);
      (* Nothing credential-shaped can appear in returned text. *)
      let returned =
        String.concat "|"
          ([ Hrm.name view; Hrm.slug view; Hrm.namespace_login view ]
          @ (match Hrm.description view with Some d -> [ d ] | None -> [])
          @ (match Hrm.website_url view with Some w -> [ w ] | None -> [])
          @ List.concat_map
              (fun r ->
                [ Hrm.full_name r; Hrm.html_url r; Hrm.default_branch r ]
                @
                match Hrm.repository_description r with
                | Some d -> [ d ]
                | None -> [])
              repos)
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) "credential absent from returned text"
            false
            (Html_assert.contains_nonempty ~needle returned))
        [ Project_fixture.pods_access_fixture; Project_fixture.pods_refresh_fixture;
          Github_fixture.gte_code_string; Github_fixture.gte_verifier_string; Github_fixture.gte_client_secret;
        ];
      Lwt.return_unit)

let rm_authorization_case =
  db_case "read model: only stewards of a verified project can load"
    (fun ~url:_ conn ->
      let* a = insert_user conn "pcreate_a" in
      let* b = insert_user conn "pcreate_b" in
      let* creator = insert_user conn "pcreate_c" in
      let* outsider = insert_user conn "pcreate_d" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-owner"
          ~connected_by:creator conn ~ext_id:943000002L
          ~account_id:943100002L
      in
      let* project =
        insert_project conn ~ns_id:943100002L ~creator
          ~project_name:"Pch Auth" ~slug:"pch-auth" ()
      in
      let* () = insert_steward conn ~project ~user:a ~installation:inst in
      let* () = insert_steward conn ~project ~user:b ~installation:inst in
      let* () =
        insert_repo conn ~project ~position:1 ~gh_id:943600021L
          ~primary:true "alpha"
      in
      let* _ = load_project "owner steward" conn ~user:a ~slug:"pch-auth" in
      let* _ = load_project "second steward" conn ~user:b ~slug:"pch-auth" in
      (* Creator and installation provenance grant nothing. *)
      let* () =
        expect_none "creator without stewardship" conn ~user:creator
          ~slug:"pch-auth"
      in
      let* () =
        exec conn "flip creator" q_set_project_creator
          (project, Some outsider)
      in
      let* () =
        expect_none "flipped creator still not a steward" conn
          ~user:outsider ~slug:"pch-auth"
      in
      let* () =
        expect_none "unrelated user" conn ~user:outsider ~slug:"pch-auth"
      in
      let* () =
        expect_none "nonexistent slug" conn ~user:a ~slug:"pch-absent"
      in
      (* Stale and revoked projects collapse to the same absence for the
         steward as for everyone else. *)
      let* () = exec conn "stale" q_set_project_status (project, "stale") in
      let* () = expect_none "stale project" conn ~user:a ~slug:"pch-auth" in
      let* () =
        exec conn "revoked" q_set_project_status (project, "revoked")
      in
      expect_none "revoked project" conn ~user:a ~slug:"pch-auth")

let rm_kind_namespace_case =
  db_case "read model: kind and namespace mappings, primary rules"
    (fun ~url:_ conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-org"
          ~account_type:"organization" conn ~ext_id:943000003L
          ~account_id:943100003L
      in
      let* org =
        insert_project conn ~kind:"organization" ~ns_id:943100003L
          ~login:"pcreate-org" ~ns_type:"organization"
          ~project_name:"Pch Org" ~slug:"pch-org" ()
      in
      let* () = insert_steward conn ~project:org ~user:uid ~installation:inst in
      (* An organization project needs no primary. *)
      let* () =
        insert_repo conn ~project:org ~position:1 ~gh_id:943600031L
          ~owner:"pcreate-org" "alpha"
      in
      let* view = load_project "organization" conn ~user:uid ~slug:"pch-org" in
      Alcotest.(check bool) "organization kind" true
        (Hrm.kind view = Pi.Organization);
      Alcotest.(check bool) "organization namespace" true
        (Hrm.namespace_type view = Gui.Organization);
      let* wg =
        insert_project conn ~kind:"working_group" ~ns_id:943100003L
          ~login:"pcreate-org" ~ns_type:"organization"
          ~project_name:"Pch WG" ~slug:"pch-wg" ()
      in
      let* () = insert_steward conn ~project:wg ~user:uid ~installation:inst in
      let* () =
        insert_repo conn ~project:wg ~position:1 ~gh_id:943600032L
          ~owner:"pcreate-org" ~primary:true "beta"
      in
      let* view = load_project "working group" conn ~user:uid ~slug:"pch-wg" in
      Alcotest.(check bool) "working group kind" true
        (Hrm.kind view = Pi.Working_group);
      Lwt.return_unit)

let rm_inconsistent_case =
  db_case "read model: durable corruption is Inconsistent_data, not a view"
    (fun ~url:_ conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* other = insert_user conn "pcreate_b" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-owner" conn
          ~ext_id:943000004L ~account_id:943100004L
      in
      let fixture ~slug =
        let* project =
          insert_project conn ~ns_id:943100004L ~project_name:"Pch Bad"
            ~slug ()
        in
        let* () = insert_steward conn ~project ~user:uid ~installation:inst in
        Lwt.return project
      in
      (* Zero repositories: an error for the steward, absence for anyone
         else. *)
      let* _ = fixture ~slug:"pch-zero" in
      let* () =
        expect_error "zero repositories" Hrm.Inconsistent_data conn
          ~user:uid ~slug:"pch-zero"
      in
      let* () =
        expect_none "zero repositories, other user" conn ~user:other
          ~slug:"pch-zero"
      in
      (* Position gap. *)
      let* gap = fixture ~slug:"pch-gap" in
      let* () =
        insert_repo conn ~project:gap ~position:1 ~gh_id:943600041L
          ~primary:true "alpha"
      in
      let* () =
        insert_repo conn ~project:gap ~position:2 ~gh_id:943600042L "beta"
      in
      let* () = exec conn "gap" q_delete_repo_position (gap, 1) in
      let* () =
        expect_error "position gap" Hrm.Inconsistent_data conn ~user:uid
          ~slug:"pch-gap"
      in
      (* Canonical URL drift. *)
      let* drift = fixture ~slug:"pch-drift" in
      let* () =
        insert_repo conn ~project:drift ~position:1 ~gh_id:943600043L
          ~primary:true "alpha"
      in
      let* () =
        exec conn "drift" q_set_repo_html_url
          (drift, 1, "https://github.com/pcreate-owner/other")
      in
      let* () =
        expect_error "html_url drift" Hrm.Inconsistent_data conn ~user:uid
          ~slug:"pch-drift"
      in
      (* Control bytes in a description. *)
      let* poison = fixture ~slug:"pch-poison" in
      let* () =
        insert_repo conn ~project:poison ~position:1 ~gh_id:943600044L
          ~primary:true "alpha"
      in
      let* () = exec conn "poison" q_poison_repo_description (poison, 1) in
      let* () =
        expect_error "control bytes" Hrm.Inconsistent_data conn ~user:uid
          ~slug:"pch-poison"
      in
      (* kind = project demands exactly one primary. *)
      let* unprimary = fixture ~slug:"pch-unprimary" in
      let* () =
        insert_repo conn ~project:unprimary ~position:1 ~gh_id:943600045L
          "alpha"
      in
      expect_error "project kind without primary" Hrm.Inconsistent_data
        conn ~user:uid ~slug:"pch-unprimary")

let rm_storage_error_case =
  db_case "read model: a failing statement is a payload-free Storage_error"
    (fun ~url _conn ->
      (* A live connection whose database lacks the permanent tables: the
         one SELECT fails inside Caqti and must collapse to the
         payload-free Storage_error, never an exception or a detail. *)
      let bare_url = Uri.with_path (Uri.of_string url) "/template1" in
      let* conn2 = Caqti_lwt_unix.connect bare_url in
      let* conn2 = or_fail "bare connect" conn2 in
      let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
      Lwt.finalize
        (fun () ->
          expect_error "missing tables" Hrm.Storage_error conn2 ~user:42
            ~slug:"pch-any")
        (fun () -> C2.disconnect ()))

let read_model_db_suite =
  [ rm_input_case; rm_owner_view_case; rm_authorization_case;
    rm_kind_namespace_case; rm_inconsistent_case; rm_storage_error_case ]

(* === Permanent page renderer (pure, DB-free) === *)

let php_repo ?(full_name = "pcreate-owner/alpha")
    ?(html_url = "https://github.com/pcreate-owner/alpha") ?description
    ?(branch = "main") ?(primary = false) ?(archived = false) () :
    Php.repository =
  { Php.full_name; html_url; description; default_branch = branch;
    is_primary = primary; is_archived = archived }

let php_project ?(name = "Fixture Project") ?(slug = "fixture-project")
    ?description ?website ?(kind = Pi.Project)
    ?(login = "pcreate-owner") ?(atype = Php.Personal)
    ?(repos = [ php_repo ~primary:true () ]) () : Php.project =
  { Php.name; slug; description; website_url = website; kind;
    namespace_login = login; namespace_type = atype; repositories = repos }

let php_frag project =
  Html_assert.panel_fragment (Php.project_home_setup_page ~project ())

let php_structure_cases =
  [ case "summary: heading, verification copy, complete project facts"
      (fun () ->
        let frag =
          php_frag
            (php_project
               ~description:"A useful description"
               ~website:"https://example.com/site" ())
        in
        Html_assert.must frag "Project created";
        Html_assert.must frag "Project connected through GitHub";
        Html_assert.must frag "Fixture Project";
        Html_assert.must frag "fixture-project";
        Html_assert.must frag ">Project</dd>";
        Html_assert.must frag "pcreate-owner (Personal account)";
        Html_assert.must frag "A useful description";
        Html_assert.must frag "<a href='https://example.com/site' \
                      class='create-link'>https://example.com/site</a>")
  ; case "summary: organization namespace and optional fields absent"
      (fun () ->
        let frag =
          php_frag
            (php_project ~kind:Pi.Ecosystem ~atype:Php.Organization
               ~login:"pcreate-org" ())
        in
        Html_assert.must frag "pcreate-org (Organization)";
        Html_assert.must frag "Ecosystem";
        Html_assert.must_not frag "Website";
        Html_assert.must_not frag "Description")
  ; case "repositories: order, markers, branch as text, safe links"
      (fun () ->
        let frag =
          php_frag
            (php_project
               ~repos:
                 [ php_repo ~primary:true
                     ~description:"Alpha description" ()
                 ; php_repo ~full_name:"pcreate-owner/beta"
                     ~html_url:"https://github.com/pcreate-owner/beta"
                     ~branch:"release/v1" ~archived:true ()
                 ]
               ())
        in
        Html_assert.order frag "pcreate-owner/alpha" "pcreate-owner/beta";
        Html_assert.must frag
          "<a href='https://github.com/pcreate-owner/alpha' \
           class='create-link phs-repo-link'>pcreate-owner/alpha</a>";
        Html_assert.must frag "phs-repo-primary'>Primary";
        Html_assert.must frag "phs-repo-archived'>Archived";
        Html_assert.must frag "Alpha description";
        Html_assert.must frag "<code class='phs-repo-branch'>release/v1</code>";
        (* One primary marker, one archived marker. *)
        Alcotest.(check int) "one primary" 1
          (Html_assert.occurrences frag "phs-repo-primary");
        Alcotest.(check int) "one archived" 1
          (Html_assert.occurrences frag "phs-repo-archived"))
  ; case "continuation: two distinct navigation links, no form" (fun () ->
        let frag = php_frag (php_project ()) in
        Html_assert.must frag "Choose a community home";
        (* Both real navigation steps: exact structural links to the two
           permanent routes — and nothing else actionable. *)
        Html_assert.must frag
          "<a href='/projects/fixture-project/request-home' \
           class='create-link phs-next-request-link'>Connect to an \
           existing community</a>";
        Html_assert.must frag
          "<a href='/projects/fixture-project/community-home/new' \
           class='create-link phs-next-create-link'>Create a community \
           home</a>";
        (* Visibly distinct: two anchors, two destinations, one each. *)
        Alcotest.(check int) "two next-step links" 2 (Html_assert.occurrences frag "<a href='/projects/");
        Alcotest.(check int) "one connect link" 1
          (Html_assert.occurrences frag "phs-next-request-link");
        Alcotest.(check int) "one create link" 1
          (Html_assert.occurrences frag "phs-next-create-link");
        (* The former placeholder copy is gone — the option is real now. *)
        Html_assert.must_not frag "not available yet";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "<button";
        Html_assert.must_not frag "<input";
        Html_assert.must_not frag "disabled";
        Html_assert.must_not frag "<script")
  ; case "continuation: invalid project slug renders neither navigation link"
      (fun () ->
        let frag = php_frag (php_project ~slug:"Bad Slug!" ()) in
        Html_assert.must frag "Choose a community home";
        Html_assert.must_not frag "request-home";
        Html_assert.must_not frag "phs-next-request-link";
        Html_assert.must_not frag "community-home/new";
        Html_assert.must_not frag "phs-next-create-link";
        Html_assert.must_not frag "href='/projects/";
        (* The corrupt slug still renders only as escaped text. *)
        Html_assert.must frag "Bad Slug!";
        Html_assert.must_not frag "<form";
        Html_assert.must_not frag "<button")
  ; case "continuation: no project id ever reaches the navigation section"
      (fun () ->
        let frag = php_frag (php_project ()) in
        List.iter (Html_assert.must_not frag)
          [ "project_id"; "id='"; "value='" ])
  ]

let php_safety_cases =
  [ case "escaping: hostile text never reaches markup" (fun () ->
        let hostile = "<script>alert('x')</script>" in
        let frag =
          php_frag
            (php_project ~name:hostile ~description:hostile
               ~login:hostile
               ~repos:
                 [ php_repo ~full_name:hostile
                     ~description:hostile ~branch:hostile
                     ~primary:true ()
                 ]
               ())
        in
        Html_assert.must_not frag "<script";
        Html_assert.must frag "&lt;script&gt;")
  ; case "defense: invalid URLs are never actionable or emitted" (fun () ->
        let frag =
          php_frag
            (php_project
               ~website:"javascript:alert(1)"
               ~repos:
                 [ php_repo ~html_url:"javascript:alert(2)" ~primary:true ()
                 ]
               ())
        in
        Html_assert.must_not frag "javascript:";
        Html_assert.must_not frag "href='#'";
        (* The repository stays listed as plain text. *)
        Html_assert.must frag "phs-unlinked'>pcreate-owner/alpha")
  ; case "copy: no officiality claims, no scripts or inline styles"
      (fun () ->
        let frag = php_frag (php_project ()) in
        List.iter (Html_assert.must_not frag)
          [ "Official"; "GitHub-approved"; "GitHub-endorsed"; "<style";
            "style='"; "style=\""; "<script"; "window.location";
            "location.href"; "http-equiv";
          ])
  ]

let page_suite = php_structure_cases @ php_safety_cases

(* --- POST /projects access gates and origin policy --- *)

let post_run ?session ?(headers = []) ~mode ~load_config () =
  Http_fixture.gate_run ?session ~headers ~method_:`POST ~target:post_target
    (make_post ~mode ~load_config)

let post_off_case =
  case "POST off: clean /bring redirect, loader untouched" (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      let response =
        Http_fixture.gate_response "off"
          (post_run ~session:Http_fixture.admin_session ~mode:Ob.Off ~load_config:loader
             ())
      in
      Http_fixture.check_clean_redirect "off" "/bring" response;
      Alcotest.(check int) "loader never called" 0 !calls)

let post_anonymous_case =
  case "POST: anonymous and invalid sessions to /login, loader untouched"
    (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      Http_fixture.check_clean_redirect "anonymous" "/login"
        (Http_fixture.gate_response "anonymous"
           (post_run ~mode:Ob.Public ~load_config:loader ()));
      List.iter
        (fun raw ->
          Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
            (Http_fixture.gate_response ("user_id " ^ raw)
               (post_run
                  ~session:[ ("user_id", raw) ]
                  ~mode:Ob.Public ~load_config:loader ())))
        [ "not-a-number"; ""; "0"; "-3" ];
      Http_fixture.check_clean_redirect "is_admin only" "/login"
        (Http_fixture.gate_response "is_admin only"
           (post_run
              ~session:[ ("is_admin", "true") ]
              ~mode:Ob.Admins ~load_config:loader ()));
      Alcotest.(check int) "loader never called" 0 !calls)

let post_rollout_case =
  case "POST admins mode: non-admin to /bring before configuration"
    (fun () ->
      let loader, calls = counting_loader (ok_loader ()) in
      Http_fixture.check_clean_redirect "non-admin" "/bring"
        (Http_fixture.gate_response "non-admin"
           (post_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ~load_config:loader
              ()));
      Alcotest.(check int) "loader never called" 0 !calls;
      (* Passing the gates and the origin check, the missing content type
         is the next rejection: reaching that 400 proves mode,
         authentication, rollout, configuration, and origin all passed —
         with no SQL (no sql_pool is installed). *)
      let continues label session mode =
        let response =
          Http_fixture.gate_response label
            (post_run ~session
               ~headers:[ ("Origin", "https://earde.com") ]
               ~mode
               ~load_config:(fun () -> ok_loader ())
               ())
        in
        Alcotest.(check int) (label ^ ": 400 content-type") 400
          (status_of response)
      in
      continues "admin continues" Http_fixture.admin_session Ob.Admins;
      continues "public user continues" Http_fixture.logged_in Ob.Public)

let post_config_failure_case =
  case "POST: configuration error is a generic 503, no form, no SQL"
    (fun () ->
      let loader, calls =
        counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      let response =
        Http_fixture.gate_response "config failure"
          (post_run ~session:Http_fixture.logged_in
             ~headers:
               [ ("Origin", "https://earde.com");
                 ("Content-Type", "application/x-www-form-urlencoded");
               ]
             ~mode:Ob.Public ~load_config:loader ())
      in
      Alcotest.(check int) "503" 503 (status_of response);
      Alcotest.(check int) "loader called once" 1 !calls;
      let body = Lwt_main.run (Dream.body response) in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("body does not leak " ^ needle)
            false (Html_assert.contains body needle))
        [ "EARDE_PUBLIC_ORIGIN"; "Missing"; "Invalid"; "public_origin" ])

let origin_run label ?sec_fetch_site origin =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @
    match sec_fetch_site with
    | Some v -> [ ("Sec-Fetch-Site", v) ]
    | None -> []
  in
  post_run ~session:Http_fixture.logged_in ~headers ~mode:Ob.Public
    ~load_config:(fun () -> ok_loader ())
    ()
  |> Http_fixture.gate_response label

let origin_rejected label ?sec_fetch_site origin =
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int) (label ^ ": 403") 403 (status_of response)

let origin_accepted label ?sec_fetch_site origin =
  let response = origin_run label ?sec_fetch_site origin in
  Alcotest.(check int)
    (label ^ ": passes the origin gate")
    400 (status_of response)

let post_origin_case =
  case "POST origin gate: exact policy of the repository-selection POST"
    (fun () ->
      origin_accepted "exact origin" (Some "https://earde.com");
      origin_accepted "explicit default port" (Some "https://earde.com:443");
      origin_accepted "fetch metadata" ~sec_fetch_site:"same-origin" None;
      origin_rejected "cross-origin" (Some "https://evil.example");
      origin_rejected "same-site subdomain" (Some "https://www.earde.com");
      origin_rejected "wrong scheme" (Some "http://earde.com");
      origin_rejected "wrong port" (Some "https://earde.com:8443");
      origin_rejected "null" (Some "null");
      origin_rejected "blank" (Some "");
      origin_rejected "mismatch beats same-origin metadata"
        ~sec_fetch_site:"same-origin" (Some "https://evil.example");
      origin_rejected "no signals" None;
      origin_rejected "cross-site" ~sec_fetch_site:"cross-site" None;
      origin_rejected "same-site" ~sec_fetch_site:"same-site" None;
      origin_rejected "none" ~sec_fetch_site:"none" None;
      let response = origin_run "reflection" (Some "https://evil.example") in
      let body = Lwt_main.run (Dream.body response) in
      Alcotest.(check bool) "origin not reflected" false
        (Html_assert.contains body "evil.example"))

let post_gate_suite =
  [ post_off_case; post_anonymous_case; post_rollout_case;
    post_config_failure_case; post_origin_case ]

(* --- GET /projects/:slug/setup access gates --- *)

let setup_gate_target = setup_target "pch-any"

let setup_run ?session ~mode () =
  Http_fixture.gate_run ?session ~method_:`GET ~target:setup_gate_target
    (Http_fixture.make_setup ~mode)

let setup_off_case =
  case "GET setup off: clean /bring redirect before params or SQL"
    (fun () ->
      Http_fixture.check_clean_redirect "off" "/bring"
        (Http_fixture.gate_response "off" (setup_run ~session:Http_fixture.admin_session ~mode:Ob.Off ())))

let setup_anonymous_case =
  case "GET setup: anonymous and invalid sessions redirect to /login"
    (fun () ->
      Http_fixture.check_clean_redirect "anonymous" "/login"
        (Http_fixture.gate_response "anonymous" (setup_run ~mode:Ob.Public ()));
      List.iter
        (fun raw ->
          Http_fixture.check_clean_redirect ("user_id " ^ raw) "/login"
            (Http_fixture.gate_response ("user_id " ^ raw)
               (setup_run ~session:[ ("user_id", raw) ] ~mode:Ob.Public ())))
        [ "not-a-number"; ""; "0"; "-3" ];
      Http_fixture.check_clean_redirect "is_admin only" "/login"
        (Http_fixture.gate_response "is_admin only"
           (setup_run
              ~session:[ ("is_admin", "true") ]
              ~mode:Ob.Admins ())))

let setup_rollout_case =
  case "GET setup admins mode: non-admin to /bring, admin continues"
    (fun () ->
      Http_fixture.check_clean_redirect "non-admin" "/bring"
        (Http_fixture.gate_response "non-admin"
           (setup_run ~session:Http_fixture.logged_in ~mode:Ob.Admins ()));
      (* Past the gates, the routerless harness has no :slug parameter,
         which answers as the generic 404 — reaching it proves the gates
         passed with no SQL (no sql_pool is installed). *)
      let continues label session mode =
        let response = Http_fixture.gate_response label (setup_run ~session ~mode ()) in
        Alcotest.(check int) (label ^ ": generic 404") 404
          (status_of response)
      in
      continues "admin continues" Http_fixture.admin_session Ob.Admins;
      continues "public user continues" Http_fixture.logged_in Ob.Public)

let setup_gate_suite =
  [ setup_off_case; setup_anonymous_case; setup_rollout_case ]

let csrf_pipeline () =
  Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ fun req ->
  let* () = Dream.set_session_field req "user_id" "42" in
  match Dream.method_ req with
  | `GET ->
      Dream.respond
        (Dream.csrf_token req ^ "\n"
        ^ Dream.csrf_token ~valid_for:(-60.) req)
  | _ ->
      make_post ~mode:Ob.Public
        ~load_config:(fun () -> ok_loader ())
        req

let csrf_post ?cookie ?(content_type = true) pipeline fields =
  let headers =
    [ ("Origin", "https://earde.com") ]
    @ (if content_type then
         [ ("Content-Type", "application/x-www-form-urlencoded") ]
       else [])
    @ match cookie with Some c -> [ ("Cookie", c) ] | None -> []
  in
  match
    Lwt_main.run
      (pipeline
         (Dream.request ~method_:`POST ~target:post_target ~headers
            (Http_fixture.form_body fields)))
  with
  | response -> `Response response
  | exception _ -> `Db_boundary

let csrf_rejected label expected result =
  let response = Http_fixture.gate_response label result in
  Alcotest.(check int) (label ^ ": status") expected (status_of response)

let csrf_case =
  case "POST CSRF: Dream verification gates every submission" (fun () ->
      let pipeline = csrf_pipeline () in
      let cookie, fresh, expired = Http_fixture.mint_tokens "mint" pipeline in
      let fields = Http_fixture.identity_fields ~draft:"1" () in
      csrf_rejected "missing token" 403 (csrf_post ~cookie pipeline fields);
      csrf_rejected "invalid token" 403
        (csrf_post ~cookie pipeline
           (fields @ [ ("dream.csrf", "not-a-token") ]));
      csrf_rejected "expired token" 403
        (csrf_post ~cookie pipeline (fields @ [ ("dream.csrf", expired) ]));
      csrf_rejected "duplicate tokens" 403
        (csrf_post ~cookie pipeline
           (fields @ [ ("dream.csrf", fresh); ("dream.csrf", fresh) ]));
      csrf_rejected "wrong session" 403
        (csrf_post pipeline (fields @ [ ("dream.csrf", fresh) ]));
      csrf_rejected "wrong content type" 400
        (csrf_post ~cookie ~content_type:false pipeline
           (fields @ [ ("dream.csrf", fresh) ]));
      (* A verified token with valid application fields reaches the DB
         boundary: CSRF passed and Dream stripped its own field before
         the strict parser. *)
      Http_fixture.check_db_boundary "verified form continues"
        (csrf_post ~cookie pipeline (fields @ [ ("dream.csrf", fresh) ])))

let csrf_invalid_form_case =
  case "POST: unrecoverable malformed forms are one generic 400, no SQL"
    (fun () ->
      let pipeline = csrf_pipeline () in
      let cookie, fresh, _ = Http_fixture.mint_tokens "mint" pipeline in
      List.iter
        (fun (label, fields) ->
          let response =
            Http_fixture.gate_response label
              (csrf_post ~cookie pipeline
                 (fields @ [ ("dream.csrf", fresh) ]))
          in
          Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
          Alcotest.(check (option string)) (label ^ ": no redirect") None
            (Dream.header response "Location");
          let body = Lwt_main.run (Dream.body response) in
          Alcotest.(check bool) (label ^ ": nothing reflected") false
            (Html_assert.contains body "zz9zz"))
        [ ("missing draft id",
           List.remove_assoc "draft_id" (Http_fixture.identity_fields ~draft:"1" ()));
          ("malformed draft id", Http_fixture.identity_fields ~draft:"zz9zz" ());
          ("duplicate draft id",
           ("draft_id", "1") :: Http_fixture.identity_fields ~draft:"1" ());
        ];
      (* A recoverable draft id in a malformed form proceeds to the
         owner re-authorization read — the DB boundary here. *)
      Http_fixture.check_db_boundary "recoverable draft id continues"
        (csrf_post ~cookie pipeline
           (Http_fixture.identity_fields ~draft:"1" ()
           @ [ ("zz-unknown", "zz"); ("dream.csrf", fresh) ])))

let post_csrf_suite = [ csrf_case; csrf_invalid_form_case ]

(* === Database-gated: the real creation and destination paths === *)

(* The real production shape: sql_pool + secret + memory sessions + the
   real router paths, so the setup GET reads its :slug route parameter
   exactly as in bin/main. /mint exists only to hand replay cases a
   fresh CSRF token for an already-established session. *)
let app_pipeline ?session_user_id ~url () =
  Dream.sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ (fun handler request ->
       match session_user_id with
       | None -> handler request
       | Some uid ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           handler request)
  @@ Dream.router
       [ Dream.get "/projects/new" (fun req ->
             Psh.make_new_project_handler ~mode:Ob.Public req);
         Dream.get "/mint" (fun req -> Dream.respond (Dream.csrf_token req));
         Dream.post "/projects" (fun req ->
             make_post ~mode:Ob.Public
               ~load_config:(fun () -> ok_loader ())
               req);
         Dream.get "/projects/:slug/setup" (fun req ->
             Http_fixture.make_setup ~mode:Ob.Public req);
       ]

(* One GET that opens the identity step: page, session cookie, and CSRF
   token for the follow-up POST /projects. *)
let open_identity_form label ~draft pipeline =
  let* response, body =
    Http_fixture.do_get
      ~target:(Printf.sprintf "/projects/new?draft=%Ld&step=details" draft)
      pipeline
  in
  Http_fixture.check_page label response;
  let cookie = Http_fixture.session_cookie label response in
  let token = Http_fixture.csrf_of_page label body in
  Lwt.return (cookie, token, Html_assert.panel_fragment body)

let do_post ?(origin = Some "https://earde.com") ~cookie ~fields pipeline =
  let headers =
    (match origin with Some o -> [ ("Origin", o) ] | None -> [])
    @ [ ("Content-Type", "application/x-www-form-urlencoded");
        ("Cookie", cookie);
      ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target:post_target ~headers
       (Http_fixture.form_body fields))

let count_projects conn account_id =
  find conn "project count" Project_fixture.q_count_projects_for_namespace account_id

let check_no_projects label conn account_id =
  let* projects = count_projects conn account_id in
  Alcotest.(check int) (label ^ ": no permanent project") 0 projects;
  Lwt.return_unit

(* One steward-selected draft with a saved selection and primary,
   ready to finalize. *)
let ready_draft ?token_body ?(login = "pcreate-owner") ?target
    ?installation_type conn ~user ~ext_id repos ~select ~primary =
  let* inst, draft, account_id =
    make_draft ?token_body ~login ?target ?installation_type conn ~user
      ~ext_id repos
  in
  let* ids = snapshot_ids conn draft in
  let selected = List.map (List.nth ids) select in
  let primary = Option.map (List.nth ids) primary in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ?primary
      selected
  in
  Lwt.return (inst, draft, account_id, ids)

(* === POST /projects: successful creation, PRG, replay === *)

let post_success_case =
  db_case "POST: creation, permanent PRG redirect, owner GET, refresh"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* inst, draft, account_id, ids =
        ready_draft ~token_body:Project_fixture.refresh_token_body conn ~user:uid
          ~ext_id:943000011L
          (fun account_id ->
            [ repo ~account_id ~id:943600111L
                ~description:{|"Alpha description"|} "alpha"
            ; repo ~account_id ~id:943600112L "beta"
            ; repo ~account_id ~id:943600113L ~default_branch:"release/v1"
                ~archived:true "gamma"
            ; Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"pcreate-owner"
                ~private_flag:true ~visibility:"private"
                ~description:
                  (Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
                ~id:943600114L ~name:Github_fixture.gur_private_name ()
            ])
          ~select:[ 0; 2 ] ~primary:(Some 0)
      in
      let s1 = List.nth ids 0 in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, frag =
        open_identity_form "identity step" ~draft pipeline
      in
      Html_assert.must frag "action='/projects'";
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~name:"Pch Success Project" ~slug:"pch-success"
               ~description:"Creata — davvero"
               ~website:"https://example.com/pch"
               ~primary:(Int64.to_string s1) ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      (* The permanent PRG destination: canonical slug only — no id, no
         draft, no success flag, no submitted value. *)
      let* () =
        Http_fixture.check_redirect_lwt "created" "/projects/pch-success/setup" response
      in
      (* Exactly one permanent project, one steward, the selected copies
         renumbered from 1, and a completed draft. *)
      let* projects = count_projects conn account_id in
      Alcotest.(check int) "one project" 1 projects;
      let* project =
        find_opt conn "project id" Project_fixture.q_project_id_by_slug "pch-success"
      in
      let project =
        match project with
        | Some id -> id
        | None -> Alcotest.fail "created project not found by slug"
      in
      let* stewards = collect conn "stewards" Project_fixture.q_steward_sigs project in
      Alcotest.(check (list string)) "one steward"
        [ Printf.sprintf "%d|%Ld|steward" uid inst ]
        stewards;
      let* repo_rows = collect conn "repo copies" Project_fixture.q_repo_sigs project in
      Alcotest.(check (list string)) "selected copies in order"
        [ Project_fixture.repo_sig ~position:1 ~id:943600111L ~login:"pcreate-owner"
            ~description:"Alpha description" ~primary:true "alpha"
        ; Project_fixture.repo_sig ~position:2 ~id:943600113L ~login:"pcreate-owner"
            ~branch:"release/v1" ~archived:true "gamma"
        ]
        repo_rows;
      let* status = find conn "draft status" q_draft_status draft in
      Alcotest.(check string) "draft completed" "completed" status;
      (* The permanent destination renders the created data for the
         steward. *)
      let* response, body =
        Http_fixture.do_get ~cookie ~target:(setup_target "pch-success") pipeline
      in
      Http_fixture.check_page "permanent GET" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "Project created";
      Html_assert.must frag "Project connected through GitHub";
      Html_assert.must frag "Pch Success Project";
      Html_assert.must frag "pch-success";
      Html_assert.must frag "Creata — davvero";
      Html_assert.must frag "https://example.com/pch";
      Html_assert.must frag "pcreate-owner (Personal account)";
      Html_assert.must frag "pcreate-owner/alpha";
      Html_assert.must frag "pcreate-owner/gamma";
      Html_assert.must frag "phs-repo-primary'>Primary";
      Html_assert.must frag "phs-repo-archived'>Archived";
      Html_assert.must frag "Choose a community home";
      Html_assert.must_not frag "pcreate-owner/beta";
      Html_assert.must_not frag "<form";
      (* No internal, GitHub, or credential identity leaks into the
         permanent page, the stored text, or the redirect. *)
      let* blob =
        find conn "permanent text blob" Project_fixture.q_permanent_text_blob project
      in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool) (label ^ " absent from page") false
            (Html_assert.contains_nonempty ~needle body);
          Alcotest.(check bool) (label ^ " absent from stored text") false
            (Html_assert.contains_nonempty ~needle blob))
        [ ("access token", Project_fixture.pods_access_fixture)
        ; ("refresh token", Project_fixture.pods_refresh_fixture)
        ; ("authorization code", Github_fixture.gte_code_string)
        ; ("PKCE verifier", Github_fixture.gte_verifier_string)
        ; ("client secret", Github_fixture.gte_client_secret)
        ; ("OAuth state", Github_fixture.goc_fixture 'S')
        ; ("session binding", Github_fixture.goc_fixture 'B')
        ; ("private repository name", Github_fixture.gur_private_name)
        ; ("private repository description", Github_fixture.gur_private_description)
        ; ("installation id", "943000011")
        ; ("account id", "943100011")
        ; ("GitHub repository id", "943600111")
        ; ("draft id", Int64.to_string draft)
        ];
      (* Refreshing the destination is a plain GET: no second project. *)
      let* response, _ =
        Http_fixture.do_get ~cookie ~target:(setup_target "pch-success") pipeline
      in
      Http_fixture.check_page "refresh" response;
      let* projects = count_projects conn account_id in
      Alcotest.(check int) "still one project" 1 projects;
      Lwt.return_unit)

let post_replay_case =
  db_case "POST: replaying the finalization body creates no second project"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000012L
          (fun account_id -> [ repo ~account_id ~id:943600121L "alpha" ])
          ~select:[ 0 ] ~primary:(Some 0)
      in
      let s1 = List.nth ids 0 in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let fields =
        Http_fixture.identity_fields ~draft:(Int64.to_string draft) ~slug:"pch-replay"
          ~primary:(Int64.to_string s1) ()
      in
      let* response =
        do_post ~cookie ~fields:(fields @ [ ("dream.csrf", token) ]) pipeline
      in
      let* () =
        Http_fixture.check_redirect_lwt "created" "/projects/pch-replay/setup" response
      in
      (* Same body again, fresh valid token: the losing call maps through
         Draft_unavailable and leaks nothing about the permanent
         outcome. *)
      let* fresh = Http_fixture.mint_token "fresh token" ~cookie pipeline in
      let* response =
        do_post ~cookie ~fields:(fields @ [ ("dream.csrf", fresh) ]) pipeline
      in
      let* () =
        Http_fixture.check_redirect_lwt "replay" "/projects/new?selection=unavailable"
          response
      in
      (match Dream.header response "Location" with
      | Some location ->
          Html_assert.must_not location "pch-replay";
          Html_assert.must_not location (Int64.to_string draft)
      | None -> Alcotest.fail "replay: no Location");
      let* projects = count_projects conn account_id in
      Alcotest.(check int) "still one project" 1 projects;
      Lwt.return_unit)

(* === POST /projects: selection redirects === *)

let post_no_selection_case =
  db_case "POST: empty saved selection is the exact repository-step redirect"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, _ =
        ready_draft conn ~user:uid ~ext_id:943000013L
          (fun account_id -> [ repo ~account_id ~id:943600131L "alpha" ])
          ~select:[] ~primary:None
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      (* The identity step refuses to open without a selection, so mint
         the token directly. *)
      let* response, body = Http_fixture.do_get ~target:"/mint" pipeline in
      let cookie = Http_fixture.session_cookie "mint" response in
      let token = body in
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~kind:"ecosystem" ~slug:"pch-empty" ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      let* () =
        Http_fixture.check_redirect_lwt "no selection"
          (Printf.sprintf "/projects/new?draft=%Ld&selection=required"
             draft)
          response
      in
      check_no_projects "no selection" conn account_id)

(* Deterministic mid-request race: the test connection locks the draft
   row before the POST, so the handler's read model sees the seeded
   selection, its finalize blocks on the draft lock, and the selection
   changes underneath before the lock is released. *)
let race_case name ~mutate ~selection_suffix =
  db_case name (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000014L
          (fun account_id ->
            [ repo ~account_id ~id:943600141L "alpha"
            ; repo ~account_id ~id:943600142L "beta"
            ])
          ~select:[ 0; 1 ] ~primary:(Some 0)
      in
      let s1 = List.nth ids 0 in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let* () = tx_start conn in
      let* _ = find conn "lock draft" q_lock_draft draft in
      let post_promise =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~slug:"pch-race" ~primary:(Int64.to_string s1) ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      let* () = Lwt_unix.sleep 1.0 in
      let* () = exec conn "mutate selection" mutate draft in
      let* () = tx_commit conn in
      let* response = post_promise in
      let* () =
        Http_fixture.check_redirect_lwt "race redirect"
          (Printf.sprintf "/projects/new?draft=%Ld&selection=%s" draft
             selection_suffix)
          response
      in
      check_no_projects "race" conn account_id)

let post_stale_primary_case =
  race_case
    "POST: primary unselected under the draft lock is the stale redirect"
    ~mutate:q_unselect_primary ~selection_suffix:"stale"

let post_race_unselected_case =
  race_case
    "POST: selection emptied under the draft lock is the required redirect"
    ~mutate:q_unselect_all ~selection_suffix:"required"

(* === POST /projects: structural form failure === *)

let post_invalid_form_case =
  db_case "POST: malformed form recovers the owner draft, discloses nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* other = insert_user conn "pcreate_b" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000015L
          (fun account_id ->
            [ repo ~account_id ~id:943600151L "alpha"
            ; repo ~account_id ~id:943600152L "beta"
            ])
          ~select:[ 0 ] ~primary:None
      in
      let s1 = List.nth ids 0 in
      let* _, foreign, _, _ =
        ready_draft ~login:"pcreate-foreign" conn ~user:other
          ~ext_id:943000016L
          (fun account_id -> [ repo ~account_id ~id:943600161L "theirs" ])
          ~select:[ 0 ] ~primary:None
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      (* Recoverable owner draft: neutral identity re-render, generic
         invalid feedback, no malformed text preserved. *)
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~name:"zzMalformedNamezz" ()
            @ [ ("zz-unknown-field", "zzevil9"); ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "recovered: 400" 400 (status_of response);
      Alcotest.(check (option string)) "recovered: no redirect" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "We couldn't read those project details";
      Html_assert.must frag "action='/projects'";
      Html_assert.must frag (Printf.sprintf "value='%Ld'" s1);
      Html_assert.must frag "name='name' maxlength='120' value=''";
      Html_assert.must_not body "zzevil9";
      Html_assert.must_not body "zzMalformedNamezz";
      (* Foreign draft id: one generic 400, no existence oracle, no
         redirect carrying the untrusted id. *)
      let* fresh = Http_fixture.mint_token "fresh" ~cookie pipeline in
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string foreign) ()
            @ [ ("zz-unknown-field", "zz"); ("dream.csrf", fresh) ])
          pipeline
      in
      Alcotest.(check int) "foreign: 400" 400 (status_of response);
      Alcotest.(check (option string)) "foreign: no redirect" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      Html_assert.must_not body (Int64.to_string foreign);
      Html_assert.must_not body "pcreate-foreign";
      Html_assert.must_not body "theirs";
      (* No finalization happened anywhere above. *)
      check_no_projects "invalid form" conn account_id)

(* === POST /projects: domain validation (422) === *)

let post_domain_validation_case =
  db_case "POST: every identity field error re-renders as an exact 422"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000017L
          (fun account_id ->
            [ repo ~account_id ~id:943600171L "alpha"
            ; repo ~account_id ~id:943600172L "beta"
            ])
          ~select:[ 0 ] ~primary:None
      in
      let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, _, _ = open_identity_form "identity step" ~draft pipeline in
      let marker_name = "Rvx <'&> Marker" in
      let escaped_marker = "Rvx &lt;&#39;&amp;&gt; Marker" in
      let submit fields =
        let* fresh = Http_fixture.mint_token "token" ~cookie pipeline in
        do_post ~cookie ~fields:(fields @ [ ("dream.csrf", fresh) ]) pipeline
      in
      let expect_422 label fields copy =
        let* response = submit fields in
        Alcotest.(check int) (label ^ ": 422") 422 (status_of response);
        Alcotest.(check (option string)) (label ^ ": no-store")
          (Some "no-store")
          (Dream.header response "Cache-Control");
        Alcotest.(check (option string)) (label ^ ": referrer policy")
          (Some Earde.Request_origin.referrer_policy)
          (Dream.header response "Referrer-Policy");
        Alcotest.(check (option string)) (label ^ ": no redirect") None
          (Dream.header response "Location");
        (* No submitted value enters a cookie. *)
        List.iter
          (fun cookie_header ->
            Alcotest.(check bool) (label ^ ": value not in cookies") false
              (Html_assert.contains cookie_header "Rvx"))
          (Dream.headers response "Set-Cookie");
        let* body = Dream.body response in
        let frag = Html_assert.panel_fragment body in
        Html_assert.must frag copy;
        (* Submitted values are preserved, escaped, and the current
           repository options are re-offered with a fresh CSRF field. *)
        Html_assert.must frag escaped_marker;
        Html_assert.must_not body marker_name;
        Html_assert.must frag (Printf.sprintf "value='%Ld'" s1);
        Html_assert.must body Http_fixture.csrf_field_marker;
        Lwt.return_unit
      in
      let fields ?(kind = "project") ?(slug = "pch-domain")
          ?(description = "") ?(website = "")
          ?(primary = Int64.to_string s1) () =
        Http_fixture.identity_fields ~draft:(Int64.to_string draft) ~kind
          ~name:marker_name ~slug ~description ~website ~primary ()
      in
      let* () =
        (* The failing field is the blank name; the marker rides in the
           description so preservation still shows. *)
        expect_422 "invalid name"
          (Http_fixture.identity_fields ~draft:(Int64.to_string draft) ~name:"   "
             ~slug:"pch-domain" ~description:marker_name
             ~primary:(Int64.to_string s1) ())
          "Enter a valid project name."
      in
      let* () =
        expect_422 "invalid slug"
          (fields ~slug:"Bad_Slug" ())
          "Enter a valid project slug"
      in
      let* () =
        expect_422 "reserved slug" (fields ~slug:"new" ())
          "That project slug is reserved"
      in
      let* () =
        expect_422 "invalid description"
          (fields ~description:"bad\x01description" ())
          "Enter a valid description"
      in
      let* () =
        expect_422 "invalid website"
          (fields ~website:"not-a-url" ())
          "Enter a valid HTTP or HTTPS website address."
      in
      let* () =
        expect_422 "primary not selected"
          (fields ~primary:(Int64.to_string s2) ())
          "Choose a primary repository"
      in
      let* () =
        expect_422 "primary required" (fields ~primary:"" ())
          "must have a primary"
      in
      check_no_projects "domain validation" conn account_id)

(* === POST /projects: finalization conflicts (409 re-renders) === *)

let post_namespace_conflict_case =
  db_case "POST: organization kind on a personal namespace is a 409"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, _ =
        ready_draft conn ~user:uid ~ext_id:943000018L
          (fun account_id -> [ repo ~account_id ~id:943600181L "alpha" ])
          ~select:[ 0 ] ~primary:None
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~kind:"organization" ~name:"Pch Namespace Conflict"
               ~slug:"pch-ns-conflict" ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "409" 409 (status_of response);
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "must be connected through a GitHub organization";
      (* Submitted values re-render over fresh read-model data. *)
      Html_assert.must frag "value='Pch Namespace Conflict'";
      Html_assert.must frag "value='pch-ns-conflict'";
      Html_assert.must frag "pcreate-owner/alpha";
      Html_assert.must body Http_fixture.csrf_field_marker;
      check_no_projects "namespace conflict" conn account_id)

let post_slug_conflict_case =
  db_case "POST: a taken slug re-renders as a 409 without the rival"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, _ =
        ready_draft conn ~user:uid ~ext_id:943000019L
          (fun account_id -> [ repo ~account_id ~id:943600191L "alpha" ])
          ~select:[ 0 ] ~primary:None
      in
      (* The rival permanent project holding the slug. *)
      let* _ =
        find conn "rival project" Project_fixture.q_insert_project_fixture
          ((None, "pch-taken"), (943100999L, "pch-rival-login"))
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~kind:"ecosystem" ~name:"Pch Slug <Conflict>"
               ~slug:"pch-taken" ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "409" 409 (status_of response);
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "That project slug is already in use.";
      (* Submitted values preserved and escaped; the conflicting project
         stays unidentified. *)
      Html_assert.must frag "value='Pch Slug &lt;Conflict&gt;'";
      Html_assert.must frag "value='pch-taken'";
      Html_assert.must_not body "Pfin fixture";
      Html_assert.must_not body "pch-rival-login";
      let* projects = count_projects conn account_id in
      Alcotest.(check int) "no new project" 0 projects;
      Lwt.return_unit)

let post_claim_conflict_case =
  db_case "POST: an already-connected repository is a 409 without the rival"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, _ =
        ready_draft conn ~user:uid ~ext_id:943000021L
          (fun account_id -> [ repo ~account_id ~id:943600211L "alpha" ])
          ~select:[ 0 ] ~primary:None
      in
      let* rival =
        find conn "rival project" Project_fixture.q_insert_project_fixture
          ((None, "pch-claim-holder"), (943100998L, "pch-rival-login"))
      in
      let* _ =
        find conn "rival claim" Project_fixture.q_insert_repo_claim
          (rival, 943600211L)
      in
      let* holder = insert_user conn "pcreate_holder" in
      let* () =
        Project_fixture.hold_actively conn ~project:rival ~holder ~ext_id:943000998L
          ~account_id:943100998L
      in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let* response =
        do_post ~cookie
          ~fields:
            (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
               ~kind:"ecosystem" ~name:"Pch Claim Conflict"
               ~slug:"pch-claim" ()
            @ [ ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "409" 409 (status_of response);
      let* body = Dream.body response in
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "already connected to another Earde project";
      Html_assert.must frag "value='Pch Claim Conflict'";
      Html_assert.must_not body "pfin-claim/holder";
      Html_assert.must_not body "pch-claim-holder";
      Html_assert.must_not body "pch-rival-login";
      let* projects = count_projects conn account_id in
      Alcotest.(check int) "no new project" 0 projects;
      Lwt.return_unit)

(* === POST /projects: rejected security gates persist nothing === *)

let post_rejected_no_rows_case =
  db_case "POST: origin, CSRF, and content-type rejections persist nothing"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000022L
          (fun account_id -> [ repo ~account_id ~id:943600221L "alpha" ])
          ~select:[ 0 ] ~primary:(Some 0)
      in
      let s1 = List.nth ids 0 in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* cookie, token, _ =
        open_identity_form "identity step" ~draft pipeline
      in
      let fields =
        Http_fixture.identity_fields ~draft:(Int64.to_string draft)
          ~slug:"pch-rejected" ~primary:(Int64.to_string s1) ()
      in
      let* response =
        do_post ~origin:(Some "https://evil.example") ~cookie
          ~fields:(fields @ [ ("dream.csrf", token) ])
          pipeline
      in
      Alcotest.(check int) "cross-origin: 403" 403 (status_of response);
      let* response = do_post ~cookie ~fields pipeline in
      Alcotest.(check int) "missing CSRF: 403" 403 (status_of response);
      let* response =
        do_post ~cookie
          ~fields:(fields @ [ ("dream.csrf", "not-a-token") ])
          pipeline
      in
      Alcotest.(check int) "invalid CSRF: 403" 403 (status_of response);
      let* status = find conn "draft status" q_draft_status draft in
      Alcotest.(check string) "draft still active" "active" status;
      check_no_projects "rejected requests" conn account_id)

(* === POST /projects: storage failure === *)

let post_storage_error_case =
  db_case "POST: finalization storage failure is the generic 500"
    (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* _, draft, account_id, ids =
        ready_draft conn ~user:uid ~ext_id:943000023L
          (fun account_id ->
            [ repo ~account_id ~id:pch_poison_repo_id "poison" ])
          ~select:[ 0 ] ~primary:(Some 0)
      in
      let s1 = List.nth ids 0 in
      let* () = exec conn "fail fn" q_create_fail_fn () in
      let* () = exec conn "fail trigger" q_create_fail_trigger () in
      Lwt.finalize
        (fun () ->
          let pipeline = app_pipeline ~session_user_id:uid ~url () in
          let* cookie, token, _ =
            open_identity_form "identity step" ~draft pipeline
          in
          let* response =
            do_post ~cookie
              ~fields:
                (Http_fixture.identity_fields ~draft:(Int64.to_string draft)
                   ~slug:"pch-storage" ~primary:(Int64.to_string s1) ()
                @ [ ("dream.csrf", token) ])
              pipeline
          in
          Alcotest.(check int) "500" 500 (status_of response);
          Alcotest.(check (option string)) "no Location" None
            (Dream.header response "Location");
          let* body = Dream.body response in
          List.iter (Html_assert.must_not body)
            [ "pch fixture failure"; "Caqti"; "PostgreSQL" ];
          (* The transaction rolled back whole: no partial permanent
             state, and the draft is preserved. *)
          let* status = find conn "draft status" q_draft_status draft in
          Alcotest.(check string) "draft still active" "active" status;
          check_no_projects "storage failure" conn account_id)
        (fun () ->
          let* () = exec conn "drop trigger" q_drop_fail_trigger () in
          exec conn "drop fn" q_drop_fail_fn ()))

let post_db_suite =
  [ post_success_case; post_replay_case; post_no_selection_case;
    post_stale_primary_case; post_race_unselected_case;
    post_invalid_form_case; post_domain_validation_case;
    post_namespace_conflict_case; post_slug_conflict_case;
    post_claim_conflict_case; post_rejected_no_rows_case;
    post_storage_error_case ]

(* === GET /projects/:slug/setup: destination authorization === *)

let setup_authorization_case =
  db_case "GET setup: stewards only; every other state is one generic 404"
    (fun ~url conn ->
      let* steward = insert_user conn "pcreate_a" in
      let* outsider = insert_user conn "pcreate_b" in
      let* creator = insert_user conn "pcreate_c" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-owner" conn
          ~ext_id:943000024L ~account_id:943100024L
      in
      let* project =
        insert_project conn ~ns_id:943100024L ~creator
          ~project_name:"Pch Destination" ~slug:"pch-dest" ()
      in
      let* () =
        insert_steward conn ~project ~user:steward ~installation:inst
      in
      let* () =
        insert_repo conn ~project ~position:1 ~gh_id:943600241L
          ~primary:true "alpha"
      in
      let steward_pipeline = app_pipeline ~session_user_id:steward ~url () in
      let* response, body =
        Http_fixture.do_get ~target:(setup_target "pch-dest") steward_pipeline
      in
      Http_fixture.check_page "steward" response;
      let frag = Html_assert.panel_fragment body in
      Html_assert.must frag "Pch Destination";
      Html_assert.must frag "pcreate-owner/alpha";
      (* No local or GitHub identifier reaches the permanent page. *)
      Html_assert.must_not body "943000024";
      Html_assert.must_not body "943100024";
      Html_assert.must_not body "943600241";
      Html_assert.must_not body (Int64.to_string project);
      (* An unrelated authenticated user gets the generic 404 with no
         name leak and no redirect to the project. *)
      let outsider_pipeline =
        app_pipeline ~session_user_id:outsider ~url ()
      in
      let expect_404 label pipeline target =
        let* response, body = Http_fixture.do_get ~target pipeline in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        Alcotest.(check (option string)) (label ^ ": no redirect") None
          (Dream.header response "Location");
        Html_assert.must_not body "Pch Destination";
        Html_assert.must_not body "pcreate-owner/alpha";
        Lwt.return_unit
      in
      let* () =
        expect_404 "outsider" outsider_pipeline (setup_target "pch-dest")
      in
      (* Creator provenance alone grants nothing. *)
      let creator_pipeline = app_pipeline ~session_user_id:creator ~url () in
      let* () =
        expect_404 "creator" creator_pipeline (setup_target "pch-dest")
      in
      (* Anonymous: the login redirect, not a 404 and not the page. *)
      let anonymous_pipeline = app_pipeline ~url () in
      let* response, _ =
        Http_fixture.do_get ~target:(setup_target "pch-dest") anonymous_pipeline
      in
      Alcotest.(check int) "anonymous: 303" 303 (status_of response);
      Alcotest.(check (option string)) "anonymous: /login" (Some "/login")
        (Dream.header response "Location");
      (* Malformed slugs are the same generic 404. *)
      let* () =
        Lwt_list.iter_s
          (fun slug ->
            expect_404 ("slug " ^ slug) steward_pipeline
              (setup_target slug))
          [ "Bad_Slug"; "a--b"; "-lead"; "zz9zz-" ]
      in
      (* Stale and revoked projects vanish for the steward too. *)
      let* () = exec conn "stale" q_set_project_status (project, "stale") in
      let* () =
        expect_404 "stale" steward_pipeline (setup_target "pch-dest")
      in
      let* () =
        exec conn "revoked" q_set_project_status (project, "revoked")
      in
      expect_404 "revoked" steward_pipeline (setup_target "pch-dest"))

let setup_inconsistent_case =
  db_case "GET setup: durable corruption is the generic 500" (fun ~url conn ->
      let* uid = insert_user conn "pcreate_a" in
      let* inst =
        Project_fixture.insert_installation ~login:"pcreate-owner" conn
          ~ext_id:943000025L ~account_id:943100025L
      in
      (* The zero-repository corruption the finalization store can never
         produce. *)
      let* project =
        insert_project conn ~ns_id:943100025L ~project_name:"Pch Corrupt"
          ~slug:"pch-corrupt" ()
      in
      let* () = insert_steward conn ~project ~user:uid ~installation:inst in
      let pipeline = app_pipeline ~session_user_id:uid ~url () in
      let* response, body =
        Http_fixture.do_get ~target:(setup_target "pch-corrupt") pipeline
      in
      Alcotest.(check int) "500" 500 (status_of response);
      Alcotest.(check (option string)) "no-store" (Some "no-store")
        (Dream.header response "Cache-Control");
      List.iter (Html_assert.must_not body) [ "Caqti"; "PostgreSQL"; "SELECT " ];
      Lwt.return_unit)

let setup_db_suite = [ setup_authorization_case; setup_inconsistent_case ]

let suites =
    (* Permanent project creation: the pure "Project created" page, the
       DB-free access/origin/CSRF gates of POST /projects and the
       permanent GET destination, and the database-gated read-model,
       creation-flow (PRG, replay, conflicts, storage failure), and
       steward-authorization behavior. *)
  [ ("project_home_page", page_suite)
  ; ("project_creation_post_gates", post_gate_suite)
  ; ("project_creation_post_csrf", post_csrf_suite)
  ; ("project_home_setup_gates", setup_gate_suite)
  ; ("project_home_read_model_db", read_model_db_suite)
  ; ("project_creation_post_db", post_db_suite)
  ; ("project_home_setup_db", setup_db_suite)
  ]
