module Phr = Earde.Project_home_relation

(* === Community connected-projects read model
   (Community_connected_projects_read_model) ===
   The accepted project-home relations of one community, read for the
   "Connected projects" section of the existing community page. Accepted
   relations are produced through the real chain wherever the product has one
   — draft/selection/finalization for the project, the request store plus the
   transactional review store for a reviewed acceptance — and only an
   automatically provisioned home (which no store writes yet) uses a
   schema-valid production-shaped INSERT. Database-gated
   (EARDE_TEST_DATABASE_URL, same opt-in as Mod_scope) with its own reserved
   external-installation-id range 946200001..946200999 (hence account ids
   946300001..946300999, which also scope the permanent-project cleanup),
   ccpr_% usernames, and ccpr-% community slugs so no suite shares fixtures.
   Pure validation is proven pre-SQL against a deliberately disconnected
   connection. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Cp = Earde.Community_connected_projects_read_model
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Fin = Earde.Project_finalization_store

let error_str : Cp.error -> string = function
  | Cp.Invalid_community_slug -> "Invalid_community_slug"
  | Cp.Community_unavailable -> "Community_unavailable"
  | Cp.Inconsistent_data -> "Inconsistent_data"
  | Cp.Storage_error -> "Storage_error"

let verification_str : Cp.verification -> string = function
  | Cp.Verified -> "verified"
  | Cp.Stale -> "stale"
  | Cp.Revoked -> "revoked"

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let insert_community = Community_fixture.insert_community

(* Distinctive credential-shaped fixtures. None of these may ever reach a
   value the public API returns; assertions on them are boolean so no
   fixture byte reaches test output on failure. *)
let private_note = "ccpr private note ACCESS_TOKEN_gho_ccpr_secret"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 946300001 \
       AND 946300999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       946300001 AND 946300999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 946200001 AND 946200999)";
      "DELETE FROM communities WHERE slug LIKE 'ccpr-%'";
      "DELETE FROM users WHERE username LIKE 'ccpr_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       946200001 AND 946200999";
    ]

(* An automatically provisioned accepted home: no requester and no reviewer,
   exactly the shape the production status CHECK admits for 'accepted'. No
   store writes this yet, so the row is built directly rather than through a
   test-only constructor. *)
let q_provision_accepted =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at) VALUES ($1, $2, 'home', 'accepted', NOW())"

(* A historical removed relation — reviewed and removed, outside the
   one-active-home partial index, so it coexists with nothing active. *)
let q_insert_removed =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at, removed_at) VALUES ($1, $2, 'home', 'removed', \
     NOW(), NOW())"

let q_insert_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES \
     ($1, $2, $3)"

let q_delete_project =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM open_source_projects WHERE id = $1"

(* Audit history deliberately blocks subject deletion (no cascade on
   project_home_audit_events); a case that deletes a project after real
   lifecycle events must purge its trail explicitly first. *)
let q_delete_audit_trail =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM project_home_audit_events WHERE project_id = $1"

(* Targeted durable corruption that the production CHECKs already admit:
   forge_namespace_login only constrains btrim/length, website_url only
   constrains btrim/non-empty/length, html_url only btrim<>'', and position
   only > 0 with a per-project uniqueness rule. *)
let q_corrupt_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET forge_namespace_login = 'ccpr' || chr(1) \
     || 'bad' WHERE id = $1"

let q_restore_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET forge_namespace_login = 'pfin-owner' \
     WHERE id = $1"

let q_corrupt_name =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = 'ccpr' || chr(1) || 'bad' WHERE id \
     = $1"

let q_set_name =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = $2 WHERE id = $1"

let q_corrupt_website =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET website_url = 'javascript:alert(1)' WHERE \
     id = $1"

let q_clear_website =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET website_url = NULL WHERE id = $1"

let q_corrupt_repo_url =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_repositories SET html_url = 'https://evil.example/x' WHERE \
     project_id = $1 AND position = 1"

let q_restore_repo_url =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_repositories SET html_url = \
     'https://github.com/pfin-owner/alpha' WHERE project_id = $1 AND position \
     = 1"

let q_break_positions =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_repositories SET position = 7 WHERE project_id = $1 AND \
     position = 2"

let q_delete_repos =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "DELETE FROM project_repositories WHERE project_id = $1"

let q_set_verification =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET verification_status = $2 WHERE id = $1"

let q_drop_full_name_key =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE project_repositories DROP CONSTRAINT \
     project_repositories_project_full_name_key"

let q_add_full_name_key =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE project_repositories ADD CONSTRAINT \
     project_repositories_project_full_name_key UNIQUE (project_id, full_name)"

let q_duplicate_full_name =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_repositories SET full_name = 'pfin-owner/alpha', html_url \
     = 'https://github.com/pfin-owner/alpha' WHERE project_id = $1 AND \
     position = 2"

(* Emptying search_path hides the unqualified tables, so the first query
   fails at the SQL layer and Caqti returns an Error the read model maps to
   Storage_error — a genuine query failure, not a torn-down connection
   (which the driver signals by raising, not by Error). *)
let q_break_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO ''"

let q_reset_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

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

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () ->
          Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f conn))

(* === call helpers === *)

let load conn ~community = Cp.load_for_community conn ~community_slug:community

let load_ok label conn ~community =
  let* r = load conn ~community in
  match r with
  | Ok projects -> Lwt.return projects
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn ~community =
  let* r = load conn ~community in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let slugs projects = List.map Cp.project_slug projects

let check_slugs label expected projects =
  Alcotest.(check (list string)) label expected (slugs projects)

(* === fixtures === *)

(* Verified permanent projects come only through the real chain — draft
   store, selection store, finalization store — never fixture INSERTs. *)
let make_project ?kind ?name ?website ?(repos = [ "alpha" ]) conn ~user ~ext_id
    ~slug =
  let base = Int64.add ext_id 400000L in
  let* _inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        List.mapi
          (fun i n ->
            Project_fixture.repo ~account_id
              ~id:(Int64.add base (Int64.of_int i))
              n)
          repos)
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let primary = List.nth ids 0 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary ids
  in
  let identity =
    Project_fixture.identity_exn ?kind ?name ~slug ?website ~selected:ids
      ~primary ()
  in
  let* created =
    Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity
  in
  Lwt.return (Fin.project_id created)

let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" q_insert_moderator (user, community, "top_mod")

let request_pending label conn ~user ~slug ~community ?(note = private_note) ()
    =
  let relation =
    Home_request_fixture.phr_expect_ok
      (Phr.create_pending ~request_note:(Some note))
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: request fixture failed" label

let review label conn ~reviewer ~slug ~community_slug ~decision =
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community_slug ~decision
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: review fixture failed" label

(* A reviewed acceptance, end to end through the real stores. *)
let accept_reviewed label conn ~owner ~reviewer ~slug ~cid ~community_slug =
  let* () = request_pending label conn ~user:owner ~slug ~community:cid () in
  review label conn ~reviewer ~slug ~community_slug ~decision:Rvs.Accept

(* === pure input validation === *)

let pure_inputs_case =
  db_case "connected projects: invalid slugs rejected before any SQL"
    (fun _conn ->
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      (* A disconnected connection cannot answer a query, so every one of
         these returning Invalid_community_slug proves the check precedes
         SQL. *)
      Lwt_list.iter_s
        (fun bad ->
          load_expect "invalid community slug" Cp.Invalid_community_slug dead
            ~community:bad)
        [
          "";
          "ccpr c";
          " ccpr-c";
          "ccpr-c ";
          "ccpr/c";
          "ccpr\tc";
          "ccpr\nc";
          "ccpr\x01c";
          "ccpr\x7fc";
        ])

(* === community existence === *)

let missing_community_case =
  db_case
    "connected projects: a community that does not exist is \
     Community_unavailable; an existing one with no relations is empty"
    (fun conn ->
      let* () =
        load_expect "missing" Cp.Community_unavailable conn
          ~community:"ccpr-nope"
      in
      let* _cid = insert_community conn "ccpr-empty" in
      let* projects = load_ok "empty" conn ~community:"ccpr-empty" in
      Alcotest.(check int) "no connected projects" 0 (List.length projects);
      (* A community whose projects were all deleted returns to empty rather
         than to an error. *)
      Lwt.return_unit)

(* === accepted relation selection === *)

let reviewed_accept_case =
  db_case
    "connected projects: one moderator-reviewed accepted home is visible in \
     full, carrying no requester, reviewer, or note" (fun conn ->
      let* owner = insert_user conn "ccpr_owner" in
      let* moderator = insert_user conn "ccpr_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:946200001L ~slug:"ccpr-rev"
          ~name:"Ccpr Reviewed" ~website:"https://reviewed.example/"
      in
      let* cid = insert_community conn "ccpr-rev-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "reviewed accept" conn ~owner ~reviewer:moderator
          ~slug:"ccpr-rev" ~cid ~community_slug:"ccpr-rev-home"
      in
      let* projects = load_ok "reviewed" conn ~community:"ccpr-rev-home" in
      Alcotest.(check int) "one project" 1 (List.length projects);
      let p = List.hd projects in
      Alcotest.(check string) "name" "Ccpr Reviewed" (Cp.project_name p);
      Alcotest.(check string) "slug" "ccpr-rev" (Cp.project_slug p);
      Alcotest.(check string)
        "verification" "verified"
        (verification_str (Cp.project_verification p));
      Alcotest.(check string)
        "namespace" "pfin-owner"
        (Cp.project_namespace_login p);
      Alcotest.(check (option string))
        "website" (Some "https://reviewed.example/") (Cp.project_website_url p);
      let repos = Cp.project_repositories p in
      Alcotest.(check int) "one repository" 1 (List.length repos);
      Alcotest.(check string)
        "repository full name" "pfin-owner/alpha"
        (Cp.repository_full_name (List.hd repos));
      Alcotest.(check string)
        "repository url" "https://github.com/pfin-owner/alpha"
        (Cp.repository_html_url (List.hd repos));
      Alcotest.(check bool)
        "repository primary" true
        (Cp.repository_is_primary (List.hd repos));
      Alcotest.(check bool)
        "repository archived" false
        (Cp.repository_is_archived (List.hd repos));
      (* Nothing private or provenance-shaped can be reached through the
         public API: boolean only, so no fixture byte is printed. *)
      let exposed =
        String.concat "\n"
          ([
             Cp.project_name p;
             Cp.project_slug p;
             Cp.project_namespace_login p;
             Option.value ~default:"" (Cp.project_website_url p);
           ]
          @ List.concat_map
              (fun r -> [ Cp.repository_full_name r; Cp.repository_html_url r ])
              repos)
      in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool)
            label false
            (Html_assert.contains exposed needle))
        [
          ("no private note", private_note);
          ("no requester name", "ccpr_owner");
          ("no reviewer name", "ccpr_mod");
          ("no external installation id", "946200001");
          ("no external account id", "946300001");
          ("no external repository id", "946600001");
        ];
      Lwt.return_unit)

let provisioned_accept_case =
  db_case
    "connected projects: a provisioned accepted home with NULL requester and \
     reviewer appears exactly like a reviewed one" (fun conn ->
      let* owner = insert_user conn "ccpr_prov_owner" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200002L ~slug:"ccpr-prov"
          ~name:"Ccpr Provisioned"
      in
      let* cid = insert_community conn "ccpr-prov-home" in
      let* () =
        exec conn "provisioned accept" q_provision_accepted (project, cid)
      in
      let* projects = load_ok "provisioned" conn ~community:"ccpr-prov-home" in
      check_slugs "provisioned visible" [ "ccpr-prov" ] projects;
      Alcotest.(check string)
        "verification" "verified"
        (verification_str (Cp.project_verification (List.hd projects)));
      Alcotest.(check (option string))
        "no website" None
        (Cp.project_website_url (List.hd projects));
      Lwt.return_unit)

let excluded_statuses_case =
  db_case
    "connected projects: pending, rejected and removed relations are all \
     excluded" (fun conn ->
      let* owner = insert_user conn "ccpr_ex_owner" in
      let* moderator = insert_user conn "ccpr_ex_mod" in
      let* _pending =
        make_project conn ~user:owner ~ext_id:946200010L ~slug:"ccpr-pend"
      in
      let* _rejected =
        make_project conn ~user:owner ~ext_id:946200011L ~slug:"ccpr-rej"
      in
      let* removed =
        make_project conn ~user:owner ~ext_id:946200012L ~slug:"ccpr-rem"
      in
      let* _accepted =
        make_project conn ~user:owner ~ext_id:946200013L ~slug:"ccpr-acc"
      in
      let* cid = insert_community conn "ccpr-ex-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      (* pending: created and deliberately left unreviewed *)
      let* () =
        request_pending "pending" conn ~user:owner ~slug:"ccpr-pend"
          ~community:cid ()
      in
      (* rejected: through the real review store *)
      let* () =
        request_pending "reject seed" conn ~user:owner ~slug:"ccpr-rej"
          ~community:cid ()
      in
      let* () =
        review "reject" conn ~reviewer:moderator ~slug:"ccpr-rej"
          ~community_slug:"ccpr-ex-home" ~decision:Rvs.Reject
      in
      (* removed: a historical row, outside the active-home index *)
      let* () = exec conn "removed row" q_insert_removed (removed, cid) in
      (* accepted: the only one that must appear *)
      let* () =
        accept_reviewed "accept" conn ~owner ~reviewer:moderator
          ~slug:"ccpr-acc" ~cid ~community_slug:"ccpr-ex-home"
      in
      let* projects = load_ok "excluded" conn ~community:"ccpr-ex-home" in
      check_slugs "only the accepted relation" [ "ccpr-acc" ] projects;
      Lwt.return_unit)

(* === ordering === *)

let ordering_case =
  db_case
    "connected projects: deterministic lower(name), slug, id order — never \
     acceptance time" (fun conn ->
      let* owner = insert_user conn "ccpr_ord_owner" in
      let* moderator = insert_user conn "ccpr_ord_mod" in
      let* cid = insert_community conn "ccpr-ord-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      (* Accepted in an order that contradicts the required output order,
         and with a case mix that only lower(name) resolves. *)
      let* _ =
        make_project conn ~user:owner ~ext_id:946200020L ~slug:"ccpr-ord-z"
          ~name:"zulu tool"
      in
      let* _ =
        make_project conn ~user:owner ~ext_id:946200021L ~slug:"ccpr-ord-a"
          ~name:"Alpha tool"
      in
      let* _ =
        make_project conn ~user:owner ~ext_id:946200022L ~slug:"ccpr-ord-m"
          ~name:"middle tool"
      in
      let* () =
        accept_reviewed "z" conn ~owner ~reviewer:moderator ~slug:"ccpr-ord-z"
          ~cid ~community_slug:"ccpr-ord-home"
      in
      let* () =
        accept_reviewed "a" conn ~owner ~reviewer:moderator ~slug:"ccpr-ord-a"
          ~cid ~community_slug:"ccpr-ord-home"
      in
      let* () =
        accept_reviewed "m" conn ~owner ~reviewer:moderator ~slug:"ccpr-ord-m"
          ~cid ~community_slug:"ccpr-ord-home"
      in
      let* projects = load_ok "ordering" conn ~community:"ccpr-ord-home" in
      check_slugs "lower(name) order, not acceptance order"
        [ "ccpr-ord-a"; "ccpr-ord-m"; "ccpr-ord-z" ]
        projects;
      Lwt.return_unit)

let name_tiebreak_case =
  db_case
    "connected projects: identical names fall through to the slug tiebreaker"
    (fun conn ->
      let* owner = insert_user conn "ccpr_tie_owner" in
      let* moderator = insert_user conn "ccpr_tie_mod" in
      let* cid = insert_community conn "ccpr-tie-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* _ =
        make_project conn ~user:owner ~ext_id:946200030L ~slug:"ccpr-tie-b"
          ~name:"Same Name"
      in
      let* _ =
        make_project conn ~user:owner ~ext_id:946200031L ~slug:"ccpr-tie-a"
          ~name:"Same Name"
      in
      let* () =
        accept_reviewed "b" conn ~owner ~reviewer:moderator ~slug:"ccpr-tie-b"
          ~cid ~community_slug:"ccpr-tie-home"
      in
      let* () =
        accept_reviewed "a" conn ~owner ~reviewer:moderator ~slug:"ccpr-tie-a"
          ~cid ~community_slug:"ccpr-tie-home"
      in
      let* projects = load_ok "tiebreak" conn ~community:"ccpr-tie-home" in
      check_slugs "slug tiebreaker" [ "ccpr-tie-a"; "ccpr-tie-b" ] projects;
      Lwt.return_unit)

let repository_order_case =
  db_case "connected projects: repositories keep stored position order"
    (fun conn ->
      let* owner = insert_user conn "ccpr_repo_owner" in
      let* moderator = insert_user conn "ccpr_repo_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:946200040L ~slug:"ccpr-repos"
          ~repos:[ "alpha"; "beta"; "gamma" ]
      in
      let* cid = insert_community conn "ccpr-repos-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "repos" conn ~owner ~reviewer:moderator
          ~slug:"ccpr-repos" ~cid ~community_slug:"ccpr-repos-home"
      in
      let* projects = load_ok "repos" conn ~community:"ccpr-repos-home" in
      let repos = Cp.project_repositories (List.hd projects) in
      Alcotest.(check (list string))
        "position order"
        [ "pfin-owner/alpha"; "pfin-owner/beta"; "pfin-owner/gamma" ]
        (List.map Cp.repository_full_name repos);
      Alcotest.(check int)
        "exactly one primary" 1
        (List.length (List.filter Cp.repository_is_primary repos));
      Lwt.return_unit)

(* === lifecycle === *)

let verification_mapping_case =
  db_case
    "connected projects: verified, stale and revoked all stay visible and map \
     exactly" (fun conn ->
      let* owner = insert_user conn "ccpr_ver_owner" in
      let* moderator = insert_user conn "ccpr_ver_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200050L ~slug:"ccpr-ver"
      in
      let* cid = insert_community conn "ccpr-ver-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "ver" conn ~owner ~reviewer:moderator ~slug:"ccpr-ver"
          ~cid ~community_slug:"ccpr-ver-home"
      in
      Lwt_list.iter_s
        (fun (stored, expected) ->
          let* () =
            exec conn "set verification" q_set_verification (project, stored)
          in
          let* projects = load_ok stored conn ~community:"ccpr-ver-home" in
          Alcotest.(check int)
            (stored ^ ": still visible")
            1 (List.length projects);
          Alcotest.(check string)
            (stored ^ ": mapping") expected
            (verification_str (Cp.project_verification (List.hd projects)));
          Lwt.return_unit)
        [ ("verified", "verified"); ("stale", "stale"); ("revoked", "revoked") ])

let community_drift_case =
  db_case_lifecycle_relaxed
    "connected projects: public, unlisted and drifted (private, draft, legacy) \
     target communities all keep the accepted relation" (fun conn ->
      let* owner = insert_user conn "ccpr_drift_owner" in
      let* moderator = insert_user conn "ccpr_drift_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:946200060L ~slug:"ccpr-drift"
      in
      let* cid = insert_community conn "ccpr-drift-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "drift" conn ~owner ~reviewer:moderator
          ~slug:"ccpr-drift" ~cid ~community_slug:"ccpr-drift-home"
      in
      let expect_visible label =
        let* projects = load_ok label conn ~community:"ccpr-drift-home" in
        check_slugs (label ^ ": still connected") [ "ccpr-drift" ] projects;
        Lwt.return_unit
      in
      let* () = expect_visible "public listed" in
      let* () = exec conn "unlisted" Community_fixture.q_make_unlisted cid in
      let* () = expect_visible "unlisted" in
      let* () = exec conn "listed again" Community_fixture.q_make_listed cid in
      let* () = exec conn "private" Community_fixture.q_make_private cid in
      let* () = expect_visible "private drift" in
      let* () = exec conn "draft" Community_fixture.q_make_draft_state cid in
      let* () = expect_visible "draft drift" in
      let* () = exec conn "legacy" Community_fixture.q_make_legacy cid in
      expect_visible "legacy drift")

let cascade_case =
  db_case "connected projects: deleting the project cascades the relation away"
    (fun conn ->
      let* owner = insert_user conn "ccpr_casc_owner" in
      let* moderator = insert_user conn "ccpr_casc_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200070L ~slug:"ccpr-casc"
      in
      let* cid = insert_community conn "ccpr-casc-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "casc" conn ~owner ~reviewer:moderator ~slug:"ccpr-casc"
          ~cid ~community_slug:"ccpr-casc-home"
      in
      let* before = load_ok "before" conn ~community:"ccpr-casc-home" in
      Alcotest.(check int) "connected before delete" 1 (List.length before);
      (* The lifecycle audit trail RESTRICT-protects the project row; the
         explicit purge here is the deliberate step the FK now demands. *)
      let* () = exec conn "purge audit trail" q_delete_audit_trail project in
      let* () = exec conn "delete project" q_delete_project project in
      let* after = load_ok "after" conn ~community:"ccpr-casc-home" in
      Alcotest.(check int) "gone after delete" 0 (List.length after);
      Lwt.return_unit)

(* === website === *)

let website_case =
  db_case
    "connected projects: an absent website is None, a present one is \
     byte-preserved, and one outside the permanent grammar is \
     Inconsistent_data" (fun conn ->
      let* owner = insert_user conn "ccpr_web_owner" in
      let* moderator = insert_user conn "ccpr_web_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200080L ~slug:"ccpr-web"
          ~website:"https://example.test/path?q=1#frag"
      in
      let* cid = insert_community conn "ccpr-web-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "web" conn ~owner ~reviewer:moderator ~slug:"ccpr-web"
          ~cid ~community_slug:"ccpr-web-home"
      in
      let* projects = load_ok "website" conn ~community:"ccpr-web-home" in
      Alcotest.(check (option string))
        "byte-preserved" (Some "https://example.test/path?q=1#frag")
        (Cp.project_website_url (List.hd projects));
      let* () = exec conn "clear website" q_clear_website project in
      let* projects = load_ok "no website" conn ~community:"ccpr-web-home" in
      Alcotest.(check (option string))
        "absent" None
        (Cp.project_website_url (List.hd projects));
      let* () = exec conn "corrupt website" q_corrupt_website project in
      load_expect "non-http website" Cp.Inconsistent_data conn
        ~community:"ccpr-web-home")

(* === durable corruption === *)

let identity_corruption_case =
  db_case
    "connected projects: malformed project identity is Inconsistent_data, \
     never a silently dropped project" (fun conn ->
      let* owner = insert_user conn "ccpr_corr_owner" in
      let* moderator = insert_user conn "ccpr_corr_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200090L ~slug:"ccpr-corr"
          ~name:"Ccpr Corrupt"
      in
      let* cid = insert_community conn "ccpr-corr-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "corr" conn ~owner ~reviewer:moderator ~slug:"ccpr-corr"
          ~cid ~community_slug:"ccpr-corr-home"
      in
      let* () = exec conn "corrupt login" q_corrupt_login project in
      let* () =
        load_expect "control byte in namespace login" Cp.Inconsistent_data conn
          ~community:"ccpr-corr-home"
      in
      let* () = exec conn "restore login" q_restore_login project in
      let* () = exec conn "corrupt name" q_corrupt_name project in
      let* () =
        load_expect "control byte in project name" Cp.Inconsistent_data conn
          ~community:"ccpr-corr-home"
      in
      let* () = exec conn "restore name" q_set_name (project, "Ccpr Corrupt") in
      let* projects = load_ok "restored" conn ~community:"ccpr-corr-home" in
      check_slugs "restored" [ "ccpr-corr" ] projects;
      Lwt.return_unit)

let repository_corruption_case =
  db_case
    "connected projects: a malformed or missing repository set fails the whole \
     read" (fun conn ->
      let* owner = insert_user conn "ccpr_rc_owner" in
      let* moderator = insert_user conn "ccpr_rc_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200100L ~slug:"ccpr-rc"
          ~repos:[ "alpha"; "beta" ]
      in
      let* cid = insert_community conn "ccpr-rc-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "rc" conn ~owner ~reviewer:moderator ~slug:"ccpr-rc"
          ~cid ~community_slug:"ccpr-rc-home"
      in
      let* () = exec conn "corrupt repo url" q_corrupt_repo_url project in
      let* () =
        load_expect "non-canonical repository URL" Cp.Inconsistent_data conn
          ~community:"ccpr-rc-home"
      in
      let* () = exec conn "restore repo url" q_restore_repo_url project in
      let* () = exec conn "break positions" q_break_positions project in
      let* () =
        load_expect "non-contiguous positions" Cp.Inconsistent_data conn
          ~community:"ccpr-rc-home"
      in
      let* () = exec conn "delete repos" q_delete_repos project in
      load_expect "zero repositories" Cp.Inconsistent_data conn
        ~community:"ccpr-rc-home")

(* The per-project full-name uniqueness constraint makes this branch
   unreachable from data alone, so the constraint is dropped and restored
   around the assertion; Lwt.finalize restores it even if the check fails. *)
let duplicate_repository_case =
  db_case
    "connected projects: duplicate repository full names within one project \
     are Inconsistent_data" (fun conn ->
      let* owner = insert_user conn "ccpr_dup_owner" in
      let* moderator = insert_user conn "ccpr_dup_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200110L ~slug:"ccpr-dup"
          ~repos:[ "alpha"; "beta" ]
      in
      let* cid = insert_community conn "ccpr-dup-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "dup" conn ~owner ~reviewer:moderator ~slug:"ccpr-dup"
          ~cid ~community_slug:"ccpr-dup-home"
      in
      let* () = exec conn "drop full-name key" q_drop_full_name_key () in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "duplicate full name" q_duplicate_full_name project
          in
          load_expect "duplicate repository full name" Cp.Inconsistent_data conn
            ~community:"ccpr-dup-home")
        (fun () ->
          (* Remove the duplicate before restoring the unique constraint. *)
          let* () = exec conn "delete repos" q_delete_repos project in
          exec conn "restore full-name key" q_add_full_name_key ()))

(* Same shape: the production CHECK closes the verification vocabulary, so
   the off-enum branch needs the constraint lifted for the length of one
   assertion. *)
let malformed_status_case =
  db_case
    "connected projects: an off-enum project verification status is \
     Inconsistent_data" (fun conn ->
      let* owner = insert_user conn "ccpr_st_owner" in
      let* moderator = insert_user conn "ccpr_st_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:946200120L ~slug:"ccpr-st"
      in
      let* cid = insert_community conn "ccpr-st-home" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "st" conn ~owner ~reviewer:moderator ~slug:"ccpr-st"
          ~cid ~community_slug:"ccpr-st-home"
      in
      let* () =
        exec conn "drop verification check"
          Connected_projects_fixture.q_drop_verification_check ()
      in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "off-enum status" q_set_verification (project, "unknown")
          in
          load_expect "off-enum verification status" Cp.Inconsistent_data conn
            ~community:"ccpr-st-home")
        (fun () ->
          let* () =
            exec conn "restore status" q_set_verification (project, "verified")
          in
          exec conn "restore verification check"
            Connected_projects_fixture.q_add_verification_check ()))

let storage_error_case =
  db_case "connected projects: storage failure surfaces as Storage_error"
    (fun conn ->
      let* () = exec conn "break search_path" q_break_search_path () in
      let* () =
        load_expect "broken schema" Cp.Storage_error conn
          ~community:"ccpr-anything"
      in
      exec conn "reset search_path" q_reset_search_path ())

let suite =
  [
    pure_inputs_case;
    missing_community_case;
    reviewed_accept_case;
    provisioned_accept_case;
    excluded_statuses_case;
    ordering_case;
    name_tiebreak_case;
    repository_order_case;
    verification_mapping_case;
    community_drift_case;
    cascade_case;
    website_case;
    identity_corruption_case;
    repository_corruption_case;
    duplicate_repository_case;
    malformed_status_case;
    storage_error_case;
  ]

let suites =
  (* Community connected-projects read model: slug validation, community
       existence, accepted-relation selection across provisioned and
       reviewed homes, deterministic ordering, verification mapping,
       lifecycle drift, and project/repository/website validation.
       Database-gated. *)
  [ ("community_connected_projects_read_model", suite) ]
