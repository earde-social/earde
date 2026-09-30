module Pi = Earde.Project_identity

(* === Project finalization store (Project_finalization_store) ===
   The draft→project transaction lives entirely in Postgres — the
   draft-first lock order shared with the refresh and selection stores,
   whole-snapshot revalidation, constraint-arbitrated slug and global
   repository claims, atomic rollback, and completion of the source
   draft — so only a DB-backed suite can pin it down. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope. Reuses Pod_store's
   real-client fixture chain (token exchange → verify → list_public →
   refresh_verified) and the selection store for saved selections — no
   test-only constructor exists — with its own reserved
   external-installation-id range 942000001..942000999 and pfin_%
   usernames so the suites never share fixtures. Identities come only
   from Project_identity.create. Credential assertions are boolean, so
   no fixture bytes reach test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Fin = Earde.Project_finalization_store

module Store = Earde.Project_onboarding_draft_store

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let find_opt conn label q arg =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find_opt q arg in
  or_fail label r

(* Fixtures — reserved external-installation-id range
   942000001..942000999 (hence account ids 942100001..942100999, which
   also scope the permanent-project cleanup) and pfin_% usernames so
   cleanup is targeted and idempotent. Projects go first (stewards and
   repositories cascade from them, and stewards RESTRICT-protect
   installations); drafts next (installations are RESTRICT-protected
   while referenced); snapshots cascade from drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 942100001 AND 942100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 942100001 AND 942100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 942000001 AND 942000999)"
    ; "DELETE FROM users WHERE username LIKE 'pfin_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 942000001 AND 942000999"
    ]

(* Everything durable on one project row as one text signature
   (mirrored by project_sig below), including the timestamp-ordering
   flag. *)
let q_project_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT COALESCE(source_onboarding_draft_id::text, '<null>') || '|' ||
          name || '|' || slug || '|' ||
          COALESCE(description, '<null>') || '|' ||
          COALESCE(website_url, '<null>') || '|' || kind || '|' ||
          forge || '|' || forge_namespace_id::text || '|' ||
          forge_namespace_login || '|' || forge_namespace_type || '|' ||
          verification_status || '|' ||
          COALESCE(created_by_user_id::text, '<null>') || '|' ||
          (updated_at >= created_at)::text
   FROM open_source_projects WHERE id = $1"

let project_sig ~draft ~name ~slug ?(description = "<null>")
    ?(website = "<null>") ?(kind = "project") ~namespace_id
    ?(login = "pfin-owner") ?(namespace_type = "user") ~creator () =
  Printf.sprintf "%Ld|%s|%s|%s|%s|%s|github|%Ld|%s|%s|verified|%d|true"
    draft name slug description website kind namespace_id login
    namespace_type creator

let q_project_kind =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT kind FROM open_source_projects WHERE id = $1"

let q_count_projects_for_draft =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM open_source_projects \
   WHERE source_onboarding_draft_id = $1"

let q_count_stewards_for_user =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_stewards WHERE user_id = $1"

let q_claim_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM project_repositories \
   WHERE github_repository_id = $1"

(* Isolated corruption fixture: the one snapshot invariant no shared
   helper covers — the stored owner drifting from the verified
   installation account. *)
let q_set_owner_id =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories \
   SET github_owner_id = $3 WHERE draft_id = $1 AND position = $2"

(* Test-only failure injection for the rollback case: a trigger scoped
   to one reserved fixture repository id, firing only on the PERMANENT
   copy — so the project, steward, and earlier repository inserts
   succeed first and genuinely partial work exists to roll back.
   Installed and dropped inside that case alone (IF EXISTS drops keep
   the cleanup idempotent even after a mid-case failure); production
   migrations are untouched. The function body uses plain string
   quoting — Caqti templates reserve '$'. *)
let pfin_poison_repo_id = 942999999L

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION pfin_fail_insert_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''pfin fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER pfin_fail_insert
   BEFORE INSERT ON project_repositories
   FOR EACH ROW WHEN (NEW.github_repository_id = 942999999)
   EXECUTE FUNCTION pfin_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS pfin_fail_insert ON project_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS pfin_fail_insert_fn()"

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

let flags conn draft = collect conn "flags" Project_fixture.q_flags draft

let check_flags label expected conn draft =
  let* stored = flags conn draft in
  Alcotest.(check (list string)) label expected stored;
  Lwt.return_unit

let nth ids n = List.nth ids n

let count_projects conn account_id =
  find conn "project count" Project_fixture.q_count_projects_for_namespace account_id

let count_stewards conn uid =
  find conn "steward count" q_count_stewards_for_user uid

let check_no_permanent_rows label conn ~account_id ~user =
  let* projects = count_projects conn account_id in
  Alcotest.(check int) (label ^ ": no project") 0 projects;
  let* stewards = count_stewards conn user in
  Alcotest.(check int) (label ^ ": no steward") 0 stewards;
  Lwt.return_unit

(* One winner plus one exact loser, order unasserted. *)
let ok_and_error label expected (r1, r2) =
  match (r1, r2) with
  | Ok created, Error e when e = expected -> created
  | Error e, Ok created when e = expected -> created
  | Ok _, Ok _ -> Alcotest.failf "%s: both succeeded" label
  | Error a, Error b ->
      Alcotest.failf "%s: both failed (%s, %s)" label (Project_fixture.finalize_error_str a)
        (Project_fixture.finalize_error_str b)
  | Ok _, Error e | Error e, Ok _ ->
      Alcotest.failf "%s: unexpected loser error %s" label (Project_fixture.finalize_error_str e)

(* === input validation === *)

let invalid_input_case =
  db_case "finalize: invalid inputs rejected before SQL, state untouched"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000001L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600011L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-invalid" ~selected:[ s1 ] ~primary:s1 ()
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* before_flags = flags conn draft in
      let expect label e ~user ~draft =
        Project_fixture.finalize_expect label e conn ~user ~draft identity
      in
      (* User and draft ids, each independently; user id precedes. *)
      let* () = expect "user id 0" Fin.Invalid_user_id ~user:0 ~draft in
      let* () =
        expect "negative user id" Fin.Invalid_user_id ~user:(-7) ~draft
      in
      let* () =
        expect "user checked before draft" Fin.Invalid_user_id ~user:0
          ~draft:0L
      in
      let* () =
        expect "draft id 0" Fin.Invalid_draft_id ~user:uid ~draft:0L
      in
      let* () =
        expect "negative draft id" Fin.Invalid_draft_id ~user:uid
          ~draft:(-9L)
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" before_row
        after_row;
      let* () =
        check_flags "selection untouched" before_flags conn draft
      in
      check_no_permanent_rows "rejected inputs" conn ~account_id
        ~user:uid)

(* === successful finalization === *)

let success_case =
  db_case "finalize: selected subset becomes one exact verified project"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* inst, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000002L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600021L
                ~description:{|"Descrizione — esatta"|} "alpha"
            ; Project_fixture.repo ~account_id ~id:942600022L "beta"
            ; Project_fixture.repo ~account_id ~id:942600023L
                ~default_branch:"release/v1" ~archived:true "gamma"
            ; Project_fixture.repo ~account_id ~id:942600024L "delta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s3 [ s1; s3 ]
      in
      let* snapshot_before = Project_fixture.sigs conn draft in
      let* _, (_, (created_b, _, expires_b)) =
        Project_fixture.draft_row conn draft
      in
      let identity =
        Project_fixture.identity_exn ~name:"Progetto Pfin — Successo"
          ~slug:"pfin-success" ~description:"Line one\nLine two"
          ~website:"https://example.com/pfin?x=1#frag"
          ~selected:[ s1; s3 ] ~primary:s3 ()
      in
      let* created =
        Project_fixture.finalize_ok "finalize" conn ~user:uid ~draft identity
      in
      Alcotest.(check string) "returned slug" "pfin-success"
        (Fin.project_slug created);
      let* stored_id =
        find_opt conn "project by slug" Project_fixture.q_project_id_by_slug
          "pfin-success"
      in
      Alcotest.(check (option int64)) "returned id is the stored id"
        (Some (Fin.project_id created)) stored_id;
      let* n = count_projects conn account_id in
      Alcotest.(check int) "exactly one project" 1 n;
      let* stored_sig =
        find conn "project row" q_project_sig (Fin.project_id created)
      in
      Alcotest.(check string) "exact project row"
        (project_sig ~draft ~name:"Progetto Pfin — Successo"
           ~slug:"pfin-success" ~description:"Line one\nLine two"
           ~website:"https://example.com/pfin?x=1#frag"
           ~namespace_id:account_id ~creator:uid ())
        stored_sig;
      let* stewards =
        collect conn "stewards" Project_fixture.q_steward_sigs (Fin.project_id created)
      in
      Alcotest.(check (list string))
        "exactly one steward, proved by the local installation record"
        [ Printf.sprintf "%d|%Ld|steward" uid inst ]
        stewards;
      let* repos =
        collect conn "repositories" Project_fixture.q_repo_sigs (Fin.project_id created)
      in
      (* Only the selected rows, in snapshot order, renumbered from 1;
         metadata byte-exact; exactly the requested primary. *)
      Alcotest.(check (list string)) "exact permanent repository set"
        [ Project_fixture.repo_sig ~position:1 ~id:942600021L
            ~description:"Descrizione — esatta" "alpha"
        ; Project_fixture.repo_sig ~position:2 ~id:942600023L ~branch:"release/v1"
            ~primary:true ~archived:true "gamma"
        ]
        repos;
      let* ( (owner, installation_record, status),
             ((no_completed, no_cancelled), (created_a, _, expires_a)) )
          =
        Project_fixture.draft_row conn draft
      in
      Alcotest.(check string) "draft completed" "completed" status;
      Alcotest.(check bool) "completed_at set" false no_completed;
      Alcotest.(check bool) "cancelled_at still NULL" true no_cancelled;
      Alcotest.(check int) "owner untouched" uid owner;
      Alcotest.(check int64) "installation record untouched" inst
        installation_record;
      Alcotest.(check (float 0.)) "created_at untouched" created_b
        created_a;
      Alcotest.(check (float 0.)) "expires_at untouched" expires_b
        expires_a;
      let* snapshot_after = Project_fixture.sigs conn draft in
      Alcotest.(check (list string))
        "snapshot rows (including selection) remain stored"
        snapshot_before snapshot_after;
      (* A replay after successful completion is not idempotent: the
         completed draft is simply unavailable. *)
      Project_fixture.finalize_expect "replay is unavailable" Fin.Draft_unavailable conn
        ~user:uid ~draft identity)

(* === every kind === *)

let kind_case =
  db_case "finalize: every kind maps to its exact database string"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let kinds =
        [ (0, Pi.Project, "project")
        ; (1, Pi.Organization, "organization")
        ; (2, Pi.Ecosystem, "ecosystem")
        ; (3, Pi.Foundation, "foundation")
        ; (4, Pi.Working_group, "working_group")
        ; (5, Pi.Other, "other")
        ]
      in
      Lwt_list.iter_s
        (fun (index, kind, db_string) ->
          let ext_id = Int64.add 942000030L (Int64.of_int index) in
          (* The Organization kind needs an organization namespace; the
             other five run under a personal one. *)
          let installation_type, target =
            match kind with
            | Pi.Organization -> ("organization", "Organization")
            | _ -> ("user", "User")
          in
          let* _, draft, _, _ =
            Project_fixture.make_draft ~installation_type ~target conn ~user:uid ~ext_id
              (fun account_id ->
                [ Project_fixture.repo ~account_id
                    ~id:(Int64.add 942600300L (Int64.of_int index))
                    "alpha"
                ])
          in
          let* ids = Project_fixture.snapshot_ids conn draft in
          let s1 = nth ids 0 in
          let* () =
            Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
              [ s1 ]
          in
          let primary =
            match kind with Pi.Project -> Some s1 | _ -> None
          in
          let identity =
            Project_fixture.identity_exn ~kind
              ~slug:(Printf.sprintf "pfin-kind-%d" index)
              ~selected:[ s1 ] ?primary ()
          in
          let* created =
            Project_fixture.finalize_ok db_string conn ~user:uid ~draft identity
          in
          let* stored_kind =
            find conn "kind" q_project_kind (Fin.project_id created)
          in
          Alcotest.(check string) (db_string ^ " stored exactly")
            db_string stored_kind;
          Lwt.return_unit)
        kinds)

(* === organization namespace rule === *)

let namespace_case =
  db_case "finalize: Organization kind requires an organization namespace"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      (* Under a GitHub organization the kind is accepted, and the
         namespace triple records the organization. *)
      let* _, org_draft, _, org_account =
        Project_fixture.make_draft ~login:"pfin-org" ~target:"Organization"
          ~installation_type:"organization" conn ~user:uid
          ~ext_id:942000011L (fun account_id ->
            [ Project_fixture.repo ~owner_login:"pfin-org" ~account_id ~id:942600111L
                "alpha"
            ])
      in
      let* org_ids = Project_fixture.snapshot_ids conn org_draft in
      let o1 = nth org_ids 0 in
      let* () =
        Project_fixture.replace_ok "seed org selection" conn ~user:uid
          ~draft:org_draft [ o1 ]
      in
      let* created =
        Project_fixture.finalize_ok "organization under organization" conn ~user:uid
          ~draft:org_draft
          (Project_fixture.identity_exn ~kind:Pi.Organization ~slug:"pfin-org-ok"
             ~selected:[ o1 ] ())
      in
      let* stored_sig =
        find conn "org project row" q_project_sig
          (Fin.project_id created)
      in
      Alcotest.(check string) "organization namespace recorded"
        (project_sig ~draft:org_draft ~name:"Pfin Fixture Project"
           ~slug:"pfin-org-ok" ~kind:"organization"
           ~namespace_id:org_account ~login:"pfin-org"
           ~namespace_type:"organization" ~creator:uid ())
        stored_sig;
      (* Under a personal account the same kind is refused and nothing
         is created... *)
      let* _, personal_draft, _, personal_account =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000012L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600121L "alpha" ])
      in
      let* personal_ids = Project_fixture.snapshot_ids conn personal_draft in
      let p1 = nth personal_ids 0 in
      let* () =
        Project_fixture.replace_ok "seed personal selection" conn ~user:uid
          ~draft:personal_draft [ p1 ]
      in
      let* before_row = Project_fixture.draft_row conn personal_draft in
      let* () =
        Project_fixture.finalize_expect "organization under personal"
          Fin.Kind_namespace_mismatch conn ~user:uid
          ~draft:personal_draft
          (Project_fixture.identity_exn ~kind:Pi.Organization ~slug:"pfin-org-personal"
             ~selected:[ p1 ] ())
      in
      let* after_row = Project_fixture.draft_row conn personal_draft in
      Project_fixture.check_same_draft_row "personal draft untouched"
        before_row after_row;
      let* n = count_projects conn personal_account in
      Alcotest.(check int) "no project under the personal namespace" 0 n;
      (* ...while a normal project remains fine there. *)
      let* _ =
        Project_fixture.finalize_ok "project under personal" conn ~user:uid
          ~draft:personal_draft
          (Project_fixture.identity_exn ~slug:"pfin-personal-ok" ~selected:[ p1 ]
             ~primary:p1 ())
      in
      Lwt.return_unit)

(* === current namespace display login === *)

let login_drift_case =
  db_case "finalize: namespace login is current, snapshot values persist"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* inst, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000014L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600141L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      (* The account renamed after the snapshot was verified: stable id
         and type unchanged, display login current. *)
      let* () =
        exec conn "rename login" Project_fixture.q_set_installation_login
          (inst, "pfin-renamed")
      in
      let* created =
        Project_fixture.finalize_ok "finalize" conn ~user:uid ~draft
          (Project_fixture.identity_exn ~slug:"pfin-login-drift" ~selected:[ s1 ]
             ~primary:s1 ())
      in
      let* stored_sig =
        find conn "project row" q_project_sig (Fin.project_id created)
      in
      Alcotest.(check string) "namespace login is the current one"
        (project_sig ~draft ~name:"Pfin Fixture Project"
           ~slug:"pfin-login-drift" ~namespace_id:account_id
           ~login:"pfin-renamed" ~creator:uid ())
        stored_sig;
      let* repos =
        collect conn "repositories" Project_fixture.q_repo_sigs (Fin.project_id created)
      in
      Alcotest.(check (list string))
        "copied names and URLs keep their validated snapshot values"
        [ Project_fixture.repo_sig ~position:1 ~id:942600141L ~primary:true "alpha" ]
        repos;
      Lwt.return_unit)

(* === no selected repositories === *)

let none_selected_case =
  db_case "finalize: a cleared selection refuses without any mutation"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000021L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600211L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600212L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      (* The identity was constructed while a selection existed; the
         saved selection is then cleared server-side. *)
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-none-selected" ~selected:[ s1 ]
          ~primary:s1 ()
      in
      let* () =
        Project_fixture.replace_ok "clear selection" conn ~user:uid ~draft []
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* before_flags = flags conn draft in
      let* () =
        Project_fixture.finalize_expect "cleared selection" Fin.No_repositories_selected
          conn ~user:uid ~draft identity
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" before_row
        after_row;
      let* () =
        check_flags "selection untouched" before_flags conn draft
      in
      check_no_permanent_rows "cleared selection" conn ~account_id
        ~user:uid)

(* === stale primary === *)

let stale_primary_replaced_case =
  db_case "finalize: a primary unselected by a later replacement is stale"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000022L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600221L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600222L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "selection with A primary" conn ~user:uid
          ~draft ~primary:s1 [ s1; s2 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-stale-replaced" ~selected:[ s1; s2 ]
          ~primary:s1 ()
      in
      (* The saved selection moves on; A is no longer selected. *)
      let* () =
        Project_fixture.replace_ok "replace away from A" conn ~user:uid
          ~draft [ s2 ]
      in
      let* before_flags = flags conn draft in
      let* () =
        Project_fixture.finalize_expect "stale primary" Fin.Selection_stale conn
          ~user:uid ~draft identity
      in
      let* () =
        check_flags "latest selection untouched" before_flags conn draft
      in
      check_no_permanent_rows "stale primary" conn ~account_id ~user:uid)

let stale_primary_refreshed_case =
  db_case "finalize: a primary deleted by a snapshot refresh is stale"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let ext_id = 942000023L in
      let account_id = Int64.add ext_id 100000L in
      let* _, draft, v, _ =
        Project_fixture.make_draft conn ~user:uid ~ext_id (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600231L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-stale-refreshed" ~selected:[ s1 ]
          ~primary:s1 ()
      in
      (* A re-verification rebuilds the snapshot: new rows, new ids. *)
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [ Project_fixture.repo ~account_id ~id:942600232L "beta" ]
      in
      let* d2 = Project_fixture.refresh_ok "refresh" conn ~user:uid v set2 in
      Alcotest.(check int64) "same draft refreshed" draft
        (Store.draft_id d2);
      let* new_ids = Project_fixture.snapshot_ids conn draft in
      let n1 = nth new_ids 0 in
      let* () =
        Project_fixture.replace_ok "select the new row" conn ~user:uid ~draft
          [ n1 ]
      in
      let* before_flags = flags conn draft in
      let* () =
        Project_fixture.finalize_expect "old primary is stale" Fin.Selection_stale conn
          ~user:uid ~draft identity
      in
      let* () =
        check_flags "refreshed selection untouched" before_flags conn
          draft
      in
      check_no_permanent_rows "refreshed primary" conn ~account_id
        ~user:uid)

(* === latest selection is authoritative === *)

let latest_selection_case =
  db_case "finalize: the latest saved selection wins over the identity"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, _ =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000024L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600241L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600242L "beta"
            ; Project_fixture.repo ~account_id ~id:942600243L "gamma"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let a = nth ids 0 and b = nth ids 1 and c = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "selection A+B" conn ~user:uid ~draft
          ~primary:a [ a; b ]
      in
      (* Constructed against A+B; the saved selection then becomes A+C
         while the primary stays selected. *)
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-latest" ~selected:[ a; b ] ~primary:a ()
      in
      let* () =
        Project_fixture.replace_ok "selection A+C" conn ~user:uid ~draft
          ~primary:a [ a; c ]
      in
      let* created =
        Project_fixture.finalize_ok "finalize" conn ~user:uid ~draft identity
      in
      let* repos =
        collect conn "repositories" Project_fixture.q_repo_sigs (Fin.project_id created)
      in
      Alcotest.(check (list string))
        "exactly the latest saved selection is copied"
        [ Project_fixture.repo_sig ~position:1 ~id:942600241L ~primary:true "alpha"
        ; Project_fixture.repo_sig ~position:2 ~id:942600243L "gamma"
        ]
        repos;
      Lwt.return_unit)

(* === draft availability === *)

(* One unavailable-state scaffold: a seeded selection and identity, the
   mutation, then a rejected finalization that must leave the draft row
   (as mutated), the selection, and the permanent tables untouched. *)
let unavailable_case name ~ext_id ~slug mutate =
  db_case name (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* inst, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:(Int64.add ext_id 600000L) "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug ~selected:[ s1 ] ~primary:s1 ()
      in
      let* () = mutate conn ~inst ~draft in
      let* before_row = Project_fixture.draft_row conn draft in
      let* before_flags = flags conn draft in
      let* () =
        Project_fixture.finalize_expect name Fin.Draft_unavailable conn ~user:uid ~draft
          identity
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" before_row
        after_row;
      let* () =
        check_flags "selection untouched" before_flags conn draft
      in
      check_no_permanent_rows name conn ~account_id ~user:uid)

let unavailable_expired_case =
  unavailable_case "finalize: expired draft is unavailable"
    ~ext_id:942000031L ~slug:"pfin-unavailable-expired"
    (fun conn ~inst:_ ~draft ->
      exec conn "expire" Project_fixture.q_backdate_draft draft)

let unavailable_completed_case =
  unavailable_case "finalize: completed draft is unavailable"
    ~ext_id:942000032L ~slug:"pfin-unavailable-completed"
    (fun conn ~inst:_ ~draft ->
      exec conn "complete" Project_fixture.q_complete_draft draft)

let unavailable_cancelled_case =
  unavailable_case "finalize: cancelled draft is unavailable"
    ~ext_id:942000033L ~slug:"pfin-unavailable-cancelled"
    (fun conn ~inst:_ ~draft ->
      exec conn "cancel" Project_fixture.q_cancel_draft draft)

let unavailable_inaccessible_case =
  unavailable_case "finalize: inaccessible installation blocks the draft"
    ~ext_id:942000034L ~slug:"pfin-unavailable-inaccessible"
    (fun conn ~inst ~draft:_ ->
      exec conn "inaccessible" Project_fixture.q_set_installation_status
        (inst, "inaccessible", false))

let unavailable_revoked_case =
  unavailable_case "finalize: revoked installation blocks the draft"
    ~ext_id:942000035L ~slug:"pfin-unavailable-revoked"
    (fun conn ~inst ~draft:_ ->
      exec conn "revoked" Project_fixture.q_set_installation_status
        (inst, "revoked", false))

let unavailable_revoked_at_case =
  unavailable_case "finalize: non-NULL revoked_at blocks the draft"
    ~ext_id:942000036L ~slug:"pfin-unavailable-revoked-at"
    (fun conn ~inst ~draft:_ ->
      exec conn "revoked with timestamp" Project_fixture.q_set_installation_status
        (inst, "revoked", true))

let unavailable_absent_and_foreign_case =
  db_case "finalize: absent and foreign drafts collapse identically"
    (fun conn ->
      let* a = insert_user conn "pfin_a" in
      let* b = insert_user conn "pfin_b" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:a ~ext_id:942000037L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600371L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:a ~draft
          ~primary:s1 [ s1 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-unavailable-foreign" ~selected:[ s1 ]
          ~primary:s1 ()
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* absent =
        find conn "absent draft id" Project_fixture.q_absent_draft_id ()
      in
      let* () =
        Project_fixture.finalize_expect "nonexistent draft" Fin.Draft_unavailable conn
          ~user:a ~draft:absent identity
      in
      let* () =
        Project_fixture.finalize_expect "another user's draft" Fin.Draft_unavailable
          conn ~user:b ~draft identity
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "owner's draft untouched"
        before_row after_row;
      let* () =
        check_no_permanent_rows "absent draft" conn ~account_id ~user:a
      in
      let* stewards_b = count_stewards conn b in
      Alcotest.(check int) "no steward for the probing user" 0
        stewards_b;
      Lwt.return_unit)

(* === durable inconsistency === *)

(* One corruption scaffold: valid draft and identity, isolated direct-SQL
   corruption, then a rejected finalization that must roll back without
   touching the (corrupted) draft state or creating permanent rows. *)
let inconsistent_case name ~ext_id ~slug corrupt =
  db_case name (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:(Int64.add ext_id 600000L) "alpha"
            ; Project_fixture.repo ~account_id ~id:(Int64.add ext_id 600001L) "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s2 [ s1; s2 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug ~selected:[ s1; s2 ] ~primary:s2 ()
      in
      let* () = corrupt conn ~draft ~account_id in
      let* before_row = Project_fixture.draft_row conn draft in
      let* before_sigs = Project_fixture.sigs conn draft in
      (* The self-reference corruption itself plants one fixture
         project, so "no new project" is pinned as an unchanged count,
         not as zero. *)
      let* before_projects =
        find conn "projects for draft" q_count_projects_for_draft draft
      in
      let* () =
        Project_fixture.finalize_expect name Fin.Inconsistent_data conn ~user:uid ~draft
          identity
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" before_row
        after_row;
      let* after_sigs = Project_fixture.sigs conn draft in
      Alcotest.(check (list string)) "snapshot untouched" before_sigs
        after_sigs;
      let* stewards = count_stewards conn uid in
      Alcotest.(check int) "no steward" 0 stewards;
      let* after_projects =
        find conn "projects for draft" q_count_projects_for_draft draft
      in
      Alcotest.(check int) "no new project for the draft"
        before_projects after_projects;
      Lwt.return_unit)

let inconsistent_owner_case =
  inconsistent_case
    "finalize: snapshot owner drifting from the account is corruption"
    ~ext_id:942000041L ~slug:"pfin-bad-owner"
    (fun conn ~draft ~account_id ->
      exec conn "drift owner" q_set_owner_id
        (draft, 1, Int64.add account_id 1L))

let inconsistent_full_name_case =
  inconsistent_case "finalize: non-canonical full name is corruption"
    ~ext_id:942000042L ~slug:"pfin-bad-full-name"
    (fun conn ~draft ~account_id:_ ->
      exec conn "break full name" Project_fixture.q_set_full_name
        (draft, 1, "pfin-other/alpha"))

let inconsistent_html_url_case =
  inconsistent_case "finalize: non-canonical repository URL is corruption"
    ~ext_id:942000043L ~slug:"pfin-bad-html-url"
    (fun conn ~draft ~account_id:_ ->
      exec conn "break html url" Project_fixture.q_set_html_url
        (draft, 1, "http://github.com/pfin-owner/alpha"))

let inconsistent_position_case =
  inconsistent_case "finalize: non-contiguous snapshot is corruption"
    ~ext_id:942000044L ~slug:"pfin-bad-position"
    (fun conn ~draft ~account_id:_ ->
      exec conn "delete first position" Project_fixture.q_delete_position
        (draft, 1))

let inconsistent_self_reference_case =
  inconsistent_case
    "finalize: an active draft already referenced by a project is corruption"
    ~ext_id:942000045L ~slug:"pfin-self-reference"
    (fun conn ~draft ~account_id:_ ->
      let* _ =
        find conn "fixture project" Project_fixture.q_insert_project_fixture
          ((Some draft, "pfin-self-reference-existing"),
           (942100900L, "pfin-fixture"))
      in
      Lwt.return_unit)

(* === slug conflict === *)

let slug_conflict_case =
  db_case "finalize: a taken slug rolls the whole transaction back"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* fixture_project =
        find conn "fixture project" Project_fixture.q_insert_project_fixture
          ((None, "pfin-taken"), (942100901L, "pfin-fixture"))
      in
      let* fixture_before =
        find conn "fixture row" q_project_sig fixture_project
      in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000051L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600511L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* () =
        Project_fixture.finalize_expect "taken slug" Fin.Slug_unavailable conn ~user:uid
          ~draft
          (Project_fixture.identity_exn ~slug:"pfin-taken" ~selected:[ s1 ] ~primary:s1
             ())
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft remains active" before_row
        after_row;
      let* () =
        check_no_permanent_rows "losing finalization" conn ~account_id
          ~user:uid
      in
      let* fixture_after =
        find conn "fixture row" q_project_sig fixture_project
      in
      Alcotest.(check string) "existing project unchanged"
        fixture_before fixture_after;
      let* fixture_repos =
        collect conn "fixture repositories" Project_fixture.q_repo_sigs fixture_project
      in
      Alcotest.(check (list string)) "no repository leaked onto it" []
        fixture_repos;
      Lwt.return_unit)

(* === global repository claim === *)

let repository_claim_case =
  db_case "finalize: an already-claimed repository rolls everything back"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let claimed = 942700052L in
      let* fixture_project =
        find conn "fixture project" Project_fixture.q_insert_project_fixture
          ((None, "pfin-claim-holder"), (942100902L, "pfin-fixture"))
      in
      let* _ =
        find conn "fixture claim" Project_fixture.q_insert_repo_claim
          (fixture_project, claimed)
      in
      let* holder = insert_user conn "pfin_holder" in
      let* () =
        Project_fixture.hold_actively conn ~project:fixture_project ~holder
          ~ext_id:942000952L ~account_id:942100902L
      in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000052L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600521L "alpha"
            ; Project_fixture.repo ~account_id ~id:claimed "shared"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1; s2 ]
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* () =
        Project_fixture.finalize_expect "claimed repository"
          Fin.Repository_already_connected conn ~user:uid ~draft
          (Project_fixture.identity_exn ~slug:"pfin-claim-loser" ~selected:[ s1; s2 ]
             ~primary:s1 ())
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft remains active" before_row
        after_row;
      let* () =
        check_no_permanent_rows "losing finalization" conn ~account_id
          ~user:uid
      in
      (* The whole losing transaction vanished: the unclaimed sibling
         repository left no partial permanent row either. *)
      let* sibling = find conn "sibling claim" q_claim_count 942600521L in
      Alcotest.(check int) "no partial repository copy" 0 sibling;
      let* n = find conn "claim count" q_claim_count claimed in
      Alcotest.(check int) "exactly one global claim" 1 n;
      Lwt.return_unit)

(* === failure rollback after partial work === *)

let rollback_case =
  db_case "finalize: a late repository failure rolls back everything"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000053L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600531L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600532L "beta"
            ; Project_fixture.repo ~account_id ~id:pfin_poison_repo_id "poison"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1; s2; s3 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-rollback" ~selected:[ s1; s2; s3 ]
          ~primary:s1 ()
      in
      let* before_row = Project_fixture.draft_row conn draft in
      let* before_sigs = Project_fixture.sigs conn draft in
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          (* The poison row is ordered last, so the project, the
             steward, and two repository copies have already succeeded
             inside the transaction when the failure fires — genuinely
             partial work must vanish. *)
          let* () =
            Project_fixture.finalize_expect "poisoned finalization" Fin.Storage_error
              conn ~user:uid ~draft identity
          in
          let* after_row = Project_fixture.draft_row conn draft in
          Project_fixture.check_same_draft_row
            "draft still active, completed_at still NULL" before_row
            after_row;
          let* after_sigs = Project_fixture.sigs conn draft in
          Alcotest.(check (list string))
            "snapshot and selection exactly as seeded" before_sigs
            after_sigs;
          let* () =
            check_no_permanent_rows "poisoned finalization" conn
              ~account_id ~user:uid
          in
          let* alpha_claims =
            find conn "alpha claim" q_claim_count 942600531L
          in
          Alcotest.(check int) "earlier repository inserts rolled back"
            0 alpha_claims;
          Lwt.return_unit)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === same-draft concurrency === *)

let same_draft_concurrency_case =
  db_case "finalize: concurrent same-draft calls leave exactly one project"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* inst, draft, _, account_id =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000061L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600611L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600612L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1; s2 ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-same-draft" ~selected:[ s1; s2 ]
          ~primary:s1 ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* results =
            Lwt.both
              (Project_fixture.finalize conn ~user:uid ~draft identity)
              (Project_fixture.finalize conn2 ~user:uid ~draft identity)
          in
          let created =
            ok_and_error "same draft" Fin.Draft_unavailable results
          in
          let* n = count_projects conn account_id in
          Alcotest.(check int) "exactly one project" 1 n;
          let* stewards =
            collect conn "stewards" Project_fixture.q_steward_sigs
              (Fin.project_id created)
          in
          Alcotest.(check (list string)) "exactly one steward"
            [ Printf.sprintf "%d|%Ld|steward" uid inst ]
            stewards;
          let* repos =
            collect conn "repositories" Project_fixture.q_repo_sigs
              (Fin.project_id created)
          in
          Alcotest.(check (list string)) "one complete repository set"
            [ Project_fixture.repo_sig ~position:1 ~id:942600611L ~primary:true "alpha"
            ; Project_fixture.repo_sig ~position:2 ~id:942600612L "beta"
            ]
            repos;
          let* (_, _, status), _ = Project_fixture.draft_row conn draft in
          Alcotest.(check string) "draft completed once" "completed"
            status;
          Lwt.return_unit))

(* === slug concurrency === *)

let slug_concurrency_case =
  db_case "finalize: concurrent same-slug drafts leave one clean winner"
    (fun conn ->
      let* a = insert_user conn "pfin_a" in
      let* b = insert_user conn "pfin_b" in
      let* _, draft_a, _, account_a =
        Project_fixture.make_draft conn ~user:a ~ext_id:942000062L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600621L "alpha" ])
      in
      let* _, draft_b, _, account_b =
        Project_fixture.make_draft conn ~user:b ~ext_id:942000063L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600631L "beta" ])
      in
      let* ids_a = Project_fixture.snapshot_ids conn draft_a in
      let* ids_b = Project_fixture.snapshot_ids conn draft_b in
      let sa = nth ids_a 0 and sb = nth ids_b 0 in
      let* () =
        Project_fixture.replace_ok "A's selection" conn ~user:a ~draft:draft_a
          ~primary:sa [ sa ]
      in
      let* () =
        Project_fixture.replace_ok "B's selection" conn ~user:b ~draft:draft_b
          ~primary:sb [ sb ]
      in
      let identity_a =
        Project_fixture.identity_exn ~slug:"pfin-slug-race" ~selected:[ sa ] ~primary:sa
          ()
      in
      let identity_b =
        Project_fixture.identity_exn ~slug:"pfin-slug-race" ~selected:[ sb ] ~primary:sb
          ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Project_fixture.finalize conn ~user:a ~draft:draft_a identity_a)
              (Project_fixture.finalize conn2 ~user:b ~draft:draft_b identity_b)
          in
          let _ =
            ok_and_error "slug race" Fin.Slug_unavailable (r1, r2)
          in
          (* Which transaction wins is not asserted; the loser must
             keep its active draft and create nothing. *)
          let winner_account, loser_account, loser_user, loser_draft =
            match r1 with
            | Ok _ -> (account_a, account_b, b, draft_b)
            | Error _ -> (account_b, account_a, a, draft_a)
          in
          let* n = count_projects conn winner_account in
          Alcotest.(check int) "one project under the winner" 1 n;
          let* n = count_projects conn loser_account in
          Alcotest.(check int) "none under the loser" 0 n;
          let* stewards = count_stewards conn loser_user in
          Alcotest.(check int) "no loser steward" 0 stewards;
          let* (_, _, status), _ =
            Project_fixture.draft_row conn loser_draft
          in
          Alcotest.(check string) "loser draft remains active" "active"
            status;
          Lwt.return_unit))

(* === repository-claim concurrency === *)

let claim_concurrency_case =
  db_case "finalize: concurrent claims of one repository leave one owner"
    (fun conn ->
      let* a = insert_user conn "pfin_a" in
      let* b = insert_user conn "pfin_b" in
      let shared = 942700064L in
      let* _, draft_a, _, account_a =
        Project_fixture.make_draft conn ~user:a ~ext_id:942000064L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600641L "alpha"
            ; Project_fixture.repo ~account_id ~id:shared "shared"
            ])
      in
      let* _, draft_b, _, account_b =
        Project_fixture.make_draft conn ~user:b ~ext_id:942000065L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600651L "beta"
            ; Project_fixture.repo ~account_id ~id:shared "shared"
            ])
      in
      let* ids_a = Project_fixture.snapshot_ids conn draft_a in
      let* ids_b = Project_fixture.snapshot_ids conn draft_b in
      let* () =
        Project_fixture.replace_ok "A's selection" conn ~user:a ~draft:draft_a
          ~primary:(nth ids_a 0) ids_a
      in
      let* () =
        Project_fixture.replace_ok "B's selection" conn ~user:b ~draft:draft_b
          ~primary:(nth ids_b 0) ids_b
      in
      let identity_a =
        Project_fixture.identity_exn ~slug:"pfin-claim-race-a" ~selected:ids_a
          ~primary:(nth ids_a 0) ()
      in
      let identity_b =
        Project_fixture.identity_exn ~slug:"pfin-claim-race-b" ~selected:ids_b
          ~primary:(nth ids_b 0) ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Project_fixture.finalize conn ~user:a ~draft:draft_a identity_a)
              (Project_fixture.finalize conn2 ~user:b ~draft:draft_b identity_b)
          in
          let _ =
            ok_and_error "claim race" Fin.Repository_already_connected
              (r1, r2)
          in
          let* n = find conn "claim count" q_claim_count shared in
          Alcotest.(check int) "exactly one global claim" 1 n;
          let loser_account, loser_user, loser_draft, loser_unique =
            match r1 with
            | Ok _ -> (account_b, b, draft_b, 942600651L)
            | Error _ -> (account_a, a, draft_a, 942600641L)
          in
          let* n = count_projects conn loser_account in
          Alcotest.(check int) "no loser project" 0 n;
          let* stewards = count_stewards conn loser_user in
          Alcotest.(check int) "no loser steward" 0 stewards;
          let* n = find conn "loser claim" q_claim_count loser_unique in
          Alcotest.(check int) "no partial loser repository" 0 n;
          let* (_, _, status), _ =
            Project_fixture.draft_row conn loser_draft
          in
          Alcotest.(check string) "loser draft remains active" "active"
            status;
          Lwt.return_unit))

(* === selection/finalization serialization === *)

let selection_race_case =
  db_case "finalize: racing a selection replacement serializes cleanly"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let* _, draft, _, _ =
        Project_fixture.make_draft conn ~user:uid ~ext_id:942000066L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600661L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600662L "beta"
            ; Project_fixture.repo ~account_id ~id:942600663L "gamma"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let a = nth ids 0 and b = nth ids 1 and c = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "selection A+B" conn ~user:uid ~draft
          ~primary:a [ a; b ]
      in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-selection-race" ~selected:[ a; b ]
          ~primary:a ()
      in
      Db_fixture.with_second_connection (fun conn2 ->
          (* Both take the draft row lock first, so PostgreSQL
             serializes them without deadlock — no sleeps needed. The
             primary stays selected in both submissions, so the
             finalization succeeds under either order. *)
          let* r_fin, r_sel =
            Lwt.both
              (Project_fixture.finalize conn ~user:uid ~draft identity)
              (Project_fixture.replace conn2 ~user:uid ~draft ~primary:a
                 [ a; c ])
          in
          let created =
            match r_fin with
            | Ok created -> created
            | Error e ->
                Alcotest.failf "finalization: unexpected %s"
                  (Project_fixture.finalize_error_str e)
          in
          let* repos =
            collect conn "repositories" Project_fixture.q_repo_sigs
              (Fin.project_id created)
          in
          (* The permanent set must equal one COMPLETE serialized saved
             selection — the replacement's exactly when it committed
             first, the original's exactly when finalization did (the
             replacement then finds the draft completed). *)
          let set_ab =
            [ Project_fixture.repo_sig ~position:1 ~id:942600661L ~primary:true "alpha"
            ; Project_fixture.repo_sig ~position:2 ~id:942600662L "beta"
            ]
          in
          let set_ac =
            [ Project_fixture.repo_sig ~position:1 ~id:942600661L ~primary:true "alpha"
            ; Project_fixture.repo_sig ~position:2 ~id:942600663L "gamma"
            ]
          in
          (match r_sel with
          | Ok () ->
              Alcotest.(check (list string))
                "replacement first: its complete selection is copied"
                set_ac repos
          | Error Earde.Project_onboarding_draft_selection_store
              .Draft_unavailable ->
              Alcotest.(check (list string))
                "finalization first: the original selection is copied"
                set_ab repos
          | Error e ->
              Alcotest.failf "replacement: unexpected %s"
                (Project_fixture.selection_error_str e));
          let* (_, _, status), _ = Project_fixture.draft_row conn draft in
          Alcotest.(check string) "draft completed" "completed" status;
          Lwt.return_unit))

(* === refresh/finalization serialization === *)

let refresh_race_case =
  db_case "finalize: racing a snapshot refresh serializes cleanly"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let ext_id = 942000067L in
      let account_id = Int64.add ext_id 100000L in
      let* inst, draft, v, _ =
        Project_fixture.make_draft conn ~user:uid ~ext_id (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600671L "alpha"
            ; Project_fixture.repo ~account_id ~id:942600672L "beta"
            ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1; s2 ]
      in
      let* seeded_sigs = Project_fixture.sigs conn draft in
      let identity =
        Project_fixture.identity_exn ~slug:"pfin-refresh-race" ~selected:[ s1; s2 ]
          ~primary:s1 ()
      in
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [ Project_fixture.repo ~account_id ~id:942600673L "gamma" ]
      in
      let gamma_sigs =
        [ Project_fixture.sig_of ~position:1 ~id:942600673L ~account_id
            ~login:"pfin-owner" "gamma"
        ]
      in
      Db_fixture.with_second_connection (fun conn2 ->
          (* Both take the draft row lock first, so the two documented
             serialized outcomes are the only possibilities. *)
          let* r_fin, r_ref =
            Lwt.both
              (Project_fixture.finalize conn ~user:uid ~draft identity)
              (Project_fixture.refresh conn2 ~user:uid v set2)
          in
          let refreshed_draft =
            match r_ref with
            | Ok d -> Store.draft_id d
            | Error e ->
                Alcotest.failf "refresh: %s" (Project_fixture.draft_error_str e)
          in
          match r_fin with
          | Ok created ->
              (* Finalization committed first: the draft completed with
                 the old snapshot intact, the project holds the old
                 complete selection, and the refresh had to create a
                 DISTINCT new active draft rather than touching the
                 completed one in place. *)
              Alcotest.(check bool)
                "refresh created a distinct new draft" false
                (Int64.equal refreshed_draft draft);
              let* (_, _, status), _ = Project_fixture.draft_row conn draft in
              Alcotest.(check string) "source draft completed"
                "completed" status;
              let* old_sigs = Project_fixture.sigs conn draft in
              Alcotest.(check (list string))
                "completed draft's snapshot untouched" seeded_sigs
                old_sigs;
              let* new_sigs = Project_fixture.sigs conn refreshed_draft in
              Alcotest.(check (list string))
                "new draft holds the refreshed snapshot" gamma_sigs
                new_sigs;
              let* repos =
                collect conn "repositories" Project_fixture.q_repo_sigs
                  (Fin.project_id created)
              in
              Alcotest.(check (list string))
                "project holds the old complete selection"
                [ Project_fixture.repo_sig ~position:1 ~id:942600671L ~primary:true
                    "alpha"
                ; Project_fixture.repo_sig ~position:2 ~id:942600672L "beta"
                ]
                repos;
              let* stored =
                Project_fixture.active_draft_id conn ~user:uid
                  ~installation:inst
              in
              Alcotest.(check (option int64))
                "the new draft is the single active one"
                (Some refreshed_draft) stored;
              Lwt.return_unit
          | Error
              (Fin.No_repositories_selected | Fin.Selection_stale) ->
              (* Refresh committed first: the snapshot was rebuilt with
                 its selection reset, the same draft id survived, and
                 no permanent row was created. *)
              Alcotest.(check int64) "refresh kept the draft id" draft
                refreshed_draft;
              let* (_, _, status), _ = Project_fixture.draft_row conn draft in
              Alcotest.(check string) "draft still active" "active"
                status;
              let* new_sigs = Project_fixture.sigs conn draft in
              Alcotest.(check (list string))
                "one complete refreshed snapshot, entirely unselected"
                gamma_sigs new_sigs;
              check_no_permanent_rows "losing finalization" conn
                ~account_id ~user:uid
          | Error e ->
              Alcotest.failf "finalization: unexpected %s" (Project_fixture.finalize_error_str e)))

(* === creator and stewardship separation === *)

let provenance_case =
  db_case "finalize: provenance never becomes creatorship or stewardship"
    (fun conn ->
      let* a = insert_user conn "pfin_a" in
      let* b = insert_user conn "pfin_b" in
      (* B connected the installation; A owns the draft and finalizes.
         Provenance is then re-pointed before finalization — it must
         change nothing. *)
      let* inst, draft, _, account_id =
        Project_fixture.make_draft ~connected_by:b conn ~user:a ~ext_id:942000071L
          (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:942600711L "alpha" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:a ~draft
          ~primary:s1 [ s1 ]
      in
      let* () =
        exec conn "provenance to NULL" Project_fixture.q_set_provenance
          (inst, None)
      in
      let* created =
        Project_fixture.finalize_ok "finalize as A" conn ~user:a ~draft
          (Project_fixture.identity_exn ~slug:"pfin-provenance" ~selected:[ s1 ]
             ~primary:s1 ())
      in
      let* stored_sig =
        find conn "project row" q_project_sig (Fin.project_id created)
      in
      Alcotest.(check string) "creator is the supplied user, never B"
        (project_sig ~draft ~name:"Pfin Fixture Project"
           ~slug:"pfin-provenance" ~namespace_id:account_id ~creator:a
           ())
        stored_sig;
      let* stewards =
        collect conn "stewards" Project_fixture.q_steward_sigs (Fin.project_id created)
      in
      Alcotest.(check (list string))
        "stewardship is A's, proved by the installation record"
        [ Printf.sprintf "%d|%Ld|steward" a inst ]
        stewards;
      let* stewards_b = count_stewards conn b in
      Alcotest.(check int) "the connector holds no stewardship" 0
        stewards_b;
      Lwt.return_unit)

(* === credential prohibition === *)

let credential_case =
  db_case "finalize: no credential material in any permanent text column"
    (fun conn ->
      let* uid = insert_user conn "pfin_a" in
      let ext_id = 942000099L in
      let account_id = Int64.add ext_id 100000L in
      let* _ =
        Project_fixture.insert_installation ~login:"pfin-owner" conn ~ext_id
          ~account_id
      in
      (* The whole fixture chain rides the refresh-token exchange, so an
         access token, refresh token, code, verifier, and secret all
         exist to leak; the listing additionally carries a private
         repository whose name and description must never survive the
         client filter into any permanent row. *)
      let* v =
        Project_fixture.verified ~token_body:Project_fixture.refresh_token_body
          ~installation_id:ext_id ~account_id ~login:"pfin-owner"
          ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~token_body:Project_fixture.refresh_token_body
          ~installation:v
          [ Project_fixture.repo ~account_id ~id:942600991L
              ~description:{|"benign description"|} "alpha"
          ; Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"pfin-owner"
              ~private_flag:true ~visibility:"private"
              ~description:
                (Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
              ~id:942600992L ~name:Github_fixture.gur_private_name ()
          ]
      in
      let* draft = Project_fixture.refresh_ok "fixture refresh" conn ~user:uid v set in
      let draft = Store.draft_id draft in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* created =
        Project_fixture.finalize_ok "finalize" conn ~user:uid ~draft
          (Project_fixture.identity_exn ~slug:"pfin-credentials" ~selected:[ s1 ]
             ~primary:s1 ())
      in
      let* blob =
        find conn "permanent text blob" Project_fixture.q_permanent_text_blob
          (Fin.project_id created)
      in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool) (label ^ " absent from stored values")
            false
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
        ];
      Lwt.return_unit)

let suite =
  [ invalid_input_case; success_case; kind_case; namespace_case;
    login_drift_case; none_selected_case; stale_primary_replaced_case;
    stale_primary_refreshed_case; latest_selection_case;
    unavailable_expired_case; unavailable_completed_case;
    unavailable_cancelled_case; unavailable_inaccessible_case;
    unavailable_revoked_case; unavailable_revoked_at_case;
    unavailable_absent_and_foreign_case; inconsistent_owner_case;
    inconsistent_full_name_case; inconsistent_html_url_case;
    inconsistent_position_case; inconsistent_self_reference_case;
    slug_conflict_case; repository_claim_case; rollback_case;
    same_draft_concurrency_case; slug_concurrency_case;
    claim_concurrency_case; selection_race_case; refresh_race_case;
    provenance_case; credential_case ]

let suites =
    (* Finalization store: the atomic draft→project transaction —
       draft-first lock order, whole-snapshot revalidation,
       constraint-arbitrated slug and global repository claims,
       rollback atomicity, and cross-store concurrency; same
       EARDE_TEST_DATABASE_URL gate (each case skips without it). *)
  [ ( "project_finalization_store", suite )
  ]
