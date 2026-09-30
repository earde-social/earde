(* Onboarding drafts and verified projects built through the real
   draft, selection and finalization stores. *)

module GTE = Earde.Github_oauth_token_exchange
module GUI = Earde.Github_user_installations
module GUR = Earde.Github_user_installation_repositories
module Pi = Earde.Project_identity

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Sel = Earde.Project_onboarding_draft_selection_store
module Fin = Earde.Project_finalization_store
module Store = Earde.Project_onboarding_draft_store

let collect = Db_fixture.collect

let draft_error_str : Store.error -> string = function
  | Store.Invalid_user_id -> "Invalid_user_id"
  | Store.Installation_unavailable -> "Installation_unavailable"
  | Store.Storage_error -> "Storage_error"

let pods_access_fixture = "pods-access.TOKEN~1"
let pods_refresh_fixture = "pods-refresh.TOKEN~2"

let default_token_body =
  {|{"access_token":"pods-access.TOKEN~1","token_type":"bearer","scope":""}|}

(* An expiring configuration, so the credential-absence case also holds a
   refresh token that must never reach either table. *)
let refresh_token_body =
  {|{"access_token":"pods-access.TOKEN~1","token_type":"bearer","scope":"","expires_in":28800,"refresh_token":"pods-refresh.TOKEN~2","refresh_token_expires_in":15811200}|}

(* Lwt-native token fixture, as in the installation-store suite. *)
let token_set_lwt body =
  let captured = ref None in
  let* outcome =
    GTE.exchange
      ~transport:(Github_fixture.gte_transport (Ok (200, body)) captured)
      ~config:(Github_fixture.gte_config ())
      ~credentials:(Github_fixture.gte_credentials ())
      ~code:(Github_fixture.gte_code_exn Github_fixture.gte_code_string)
      ~verifier:(Github_fixture.gte_verifier ())
  in
  match outcome with
  | Ok tokens -> Lwt.return tokens
  | Error e ->
      Alcotest.failf "token fixture: %s" (Github_fixture.gte_show_error e)

(* Abstract verified-installation fixture through the real verify against
   a one-page scripted listing. *)
let verified ?(token_body = default_token_body) ~installation_id ~account_id
    ~login ~target () =
  let* token_set = token_set_lwt token_body in
  let body =
    Printf.sprintf
      {|{"total_count":1,"installations":[{"id":%Ld,"account":{"id":%Ld,"login":"%s"},"target_type":"%s"}]}|}
      installation_id account_id login target
  in
  let captured = ref [] in
  let* outcome =
    GUI.verify
      ~transport:(Github_fixture.gui_transport [ Ok (200, body) ] captured)
      ~token_set ~installation_id
  in
  match outcome with
  | Ok v -> Lwt.return v
  | Error e ->
      Alcotest.failf "verified fixture: %s" (Github_fixture.gui_show_error e)

(* Abstract repository-set fixture through the real list_public against a
   one-page scripted listing; entries are gur_repo JSON owned by the
   verified installation's account. *)
let repo_set ?(token_body = default_token_body) ~installation entries =
  let* token_set = token_set_lwt token_body in
  let body = Github_fixture.gur_body ~total:(List.length entries) entries in
  let captured = ref [] in
  let* outcome =
    GUR.list_public
      ~transport:(Github_fixture.gur_transport [ Ok (200, body) ] captured)
      ~token_set ~installation
  in
  match outcome with
  | Ok set -> Lwt.return set
  | Error e ->
      Alcotest.failf "repository-set fixture: %s"
        (Github_fixture.gur_show_error e)

(* Expected snapshot signature, mirroring q_sigs below: the client
   derives full_name and html_url from login and name, and the store must
   always reset both selection flags. *)
let sig_of ~position ~id ~account_id ~login ?(description = "<null>")
    ?(branch = "main") ?(archived = false) name =
  Printf.sprintf
    "%d|%Ld|%Ld|%s|%s|%s/%s|https://github.com/%s/%s|%s|%s|%b|false|false"
    position id account_id login name login name login name description branch
    archived

(* Direct installation-row fixture: these cases pin the store's
   authorization against arbitrary row states (inaccessible, revoked,
   mismatched identity), which the installation store deliberately never
   produces on demand. *)
let q_insert_installation =
  (Caqti_type.(t2 (t4 int64 int64 string string) (t3 string bool (option int)))
  ->! Caqti_type.int64)
    "INSERT INTO github_installations\n\
    \     (github_installation_id, github_account_id, github_account_login,\n\
    \      github_account_type, status, revoked_at, connected_by_user_id)\n\
    \   VALUES ($1, $2, $3, $4, $5, CASE WHEN $6 THEN NOW() END, $7)\n\
    \   RETURNING id"

(* Everything durable on one draft row: ownership, installation record,
   lifecycle shape, and the three timestamp epochs. *)
let q_draft_row =
  Caqti_type.(
    int64
    ->! t2 (t3 int int64 string) (t2 (t2 bool bool) (t3 float float float)))
    "SELECT user_id, github_installation_record_id, status,\n\
    \          completed_at IS NULL, cancelled_at IS NULL,\n\
    \          EXTRACT(EPOCH FROM created_at)::float8,\n\
    \          EXTRACT(EPOCH FROM updated_at)::float8,\n\
    \          EXTRACT(EPOCH FROM expires_at)::float8\n\
    \   FROM project_onboarding_drafts WHERE id = $1"

let q_active_draft_id =
  (Caqti_type.(t2 int int64) ->? Caqti_type.int64)
    "SELECT id FROM project_onboarding_drafts\n\
    \   WHERE user_id = $1 AND github_installation_record_id = $2\n\
    \     AND status = 'active'"

let q_count_for_user =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_onboarding_drafts WHERE user_id = $1"

let q_count_active_for_user =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_onboarding_drafts\n\
    \   WHERE user_id = $1 AND status = 'active'"

(* One text signature per snapshot row, ordered by position — pins the
   exact stored metadata, the ordering, and both selection flags in a
   single comparison (mirrored by sig_of above). *)
let q_sigs =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT position::text || '|' || github_repository_id::text || '|' ||\n\
    \          github_owner_id::text || '|' || owner_login || '|' || name\n\
    \          || '|' || full_name || '|' || html_url || '|' ||\n\
    \          COALESCE(description, '<null>') || '|' || default_branch\n\
    \          || '|' || is_archived::text || '|' || is_selected::text\n\
    \          || '|' || is_primary::text\n\
    \   FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

(* Expired-but-still-active fixture: created_at moves too (expires_at >
   created_at is a CHECK), and updated_at moves with it so the refresh's
   renewal is observable. *)
let q_backdate_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts\n\
    \   SET created_at = NOW() - INTERVAL '2 days',\n\
    \       updated_at = NOW() - INTERVAL '2 days',\n\
    \       expires_at = NOW() - INTERVAL '1 day'\n\
    \   WHERE id = $1"

(* Derived expiry, judged by the database clock — the schema has no
   'expired' status. *)
let q_is_expired =
  (Caqti_type.int64 ->! Caqti_type.bool)
    "SELECT expires_at <= NOW() FROM project_onboarding_drafts WHERE id = $1"

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

let insert_installation ?(login = "podstore-account") ?(account_type = "user")
    ?(status = "active") ?(revoked = false) ?connected_by conn ~ext_id
    ~account_id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rid =
    C.find q_insert_installation
      ( (ext_id, account_id, login, account_type),
        (status, revoked, connected_by) )
  in
  Db_fixture.or_fail "installation row" rid

let refresh conn ~user v set =
  Store.refresh_verified conn ~user_id:user ~installation:v ~repositories:set

let refresh_ok label conn ~user v set =
  let* r = refresh conn ~user v set in
  match r with
  | Ok draft -> Lwt.return draft
  | Error e -> Alcotest.failf "%s: %s" label (draft_error_str e)

let draft_row conn id =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_draft_row id in
  Db_fixture.or_fail "draft row" r

let sigs conn draft =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.collect_list q_sigs draft in
  Db_fixture.or_fail "signatures" r

let count_for_user conn uid =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_count_for_user uid in
  Db_fixture.or_fail "draft count" r

let active_draft_id conn ~user ~installation =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find_opt q_active_draft_id (user, installation) in
  Db_fixture.or_fail "active draft id" r

(* Componentwise identity: pins that an operation left every persisted
   field of a draft row exactly as captured. *)
let check_same_draft_row label
    ((u_b, i_b, status_b), ((nc_b, nx_b), (created_b, updated_b, expires_b)))
    ((u_a, i_a, status_a), ((nc_a, nx_a), (created_a, updated_a, expires_a))) =
  Alcotest.(check int) (label ^ ": user") u_b u_a;
  Alcotest.(check int64) (label ^ ": installation record") i_b i_a;
  Alcotest.(check string) (label ^ ": status") status_b status_a;
  Alcotest.(check bool) (label ^ ": completed_at NULL-ness") nc_b nc_a;
  Alcotest.(check bool) (label ^ ": cancelled_at NULL-ness") nx_b nx_a;
  Alcotest.(check (float 0.)) (label ^ ": created_at") created_b created_a;
  Alcotest.(check (float 0.)) (label ^ ": updated_at") updated_b updated_a;
  Alcotest.(check (float 0.)) (label ^ ": expires_at") expires_b expires_a

let q_set_installation_status =
  (Caqti_type.(t3 int64 string bool) ->. Caqti_type.unit)
    "UPDATE github_installations\n\
    \   SET status = $2, revoked_at = CASE WHEN $3 THEN NOW() END\n\
    \   WHERE id = $1"

let q_set_installation_login =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE github_installations SET github_account_login = $2 WHERE id = $1"

let q_set_provenance =
  (Caqti_type.(t2 int64 (option int)) ->. Caqti_type.unit)
    "UPDATE github_installations SET connected_by_user_id = $2 WHERE id = $1"

(* Ordering fixture: shifts a draft's activity into the past without
   touching expires_at, so the draft stays available (expires_at >
   created_at keeps holding — expiry stays in the future). *)
let q_backdate_updated =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts\n\
    \   SET created_at = created_at - $2 * INTERVAL '1 hour',\n\
    \       updated_at = updated_at - $2 * INTERVAL '1 hour'\n\
    \   WHERE id = $1"

(* A positive draft id guaranteed absent, for the enumeration probe. *)
let q_absent_draft_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM project_onboarding_drafts"

(* The zero-repository corruption fixture: an otherwise available draft
   inserted directly with no snapshot rows — the store can never produce
   this, which is exactly the point. *)
let q_insert_bare_draft =
  (Caqti_type.(t2 int int64) ->! Caqti_type.int64)
    "INSERT INTO project_onboarding_drafts\n\
    \     (user_id, github_installation_record_id, expires_at)\n\
    \   VALUES ($1, $2, NOW() + INTERVAL '24 hours') RETURNING id"

let q_snapshot_ids =
  (Caqti_type.int64 ->* Caqti_type.int64)
    "SELECT id FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

(* Isolated corruption fixtures: each violates exactly one invariant the
   schema deliberately does not own (see the constraint-coverage note at
   the end of the module for the states the schema already forbids). *)
let q_delete_position =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "DELETE FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 AND position = $2"

let q_set_full_name =
  (Caqti_type.(t3 int64 int string) ->. Caqti_type.unit)
    "UPDATE project_onboarding_draft_repositories\n\
    \   SET full_name = $3 WHERE draft_id = $1 AND position = $2"

let q_set_html_url =
  (Caqti_type.(t3 int64 int string) ->. Caqti_type.unit)
    "UPDATE project_onboarding_draft_repositories\n\
    \   SET html_url = $3 WHERE draft_id = $1 AND position = $2"

let selection_error_str : Sel.error -> string = function
  | Sel.Invalid_user_id -> "Invalid_user_id"
  | Sel.Invalid_draft_id -> "Invalid_draft_id"
  | Sel.Invalid_selection -> "Invalid_selection"
  | Sel.Draft_unavailable -> "Draft_unavailable"
  | Sel.Selection_stale -> "Selection_stale"
  | Sel.Inconsistent_data -> "Inconsistent_data"
  | Sel.Storage_error -> "Storage_error"

(* One text signature per snapshot row, ordered by position: the local
   row id and both flags — the complete selection state one replacement
   owns (mirrored by flag_sig below). *)
let q_flags =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT id::text || '|' || is_selected::text || '|' || is_primary::text\n\
    \   FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

let flag_sig id ~selected ~primary =
  Printf.sprintf "%Ld|%b|%b" id selected primary

let replace conn ~user ~draft ?primary selected =
  Sel.replace conn ~user_id:user ~draft_id:draft ~selected_snapshot_ids:selected
    ~primary_snapshot_id:primary

let replace_ok label conn ~user ~draft ?primary selected =
  let* r = replace conn ~user ~draft ?primary selected in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (selection_error_str e)

(* Positive ids guaranteed absent, for the FK-failure probes. *)
let q_absent_user_id =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

let q_absent_project_id =
  (Caqti_type.unit ->! Caqti_type.int64)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM open_source_projects"

(* Full-width project insert; nested tuples per the Caqti convention. *)
let q_insert_project =
  (Caqti_type.(
     t2
       (t2 (t2 (option int64) (option int)) (t2 string string))
       (t2
          (t2 (option string) (option string))
          (t2 (t3 string string int64) (t3 string string string))))
  ->! Caqti_type.int64)
    "INSERT INTO open_source_projects\n\
    \     (source_onboarding_draft_id, created_by_user_id, name, slug,\n\
    \      description, website_url, kind, forge, forge_namespace_id,\n\
    \      forge_namespace_login, forge_namespace_type, verification_status)\n\
    \   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)\n\
    \   RETURNING id"

let q_project_exists =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM open_source_projects WHERE id = $1"

let q_insert_steward =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
    "INSERT INTO project_stewards\n\
    \     (project_id, user_id, github_installation_record_id)\n\
    \   VALUES ($1, $2, $3)"

(* Full-width repository insert. *)
let q_insert_repo =
  (Caqti_type.(
     t2
       (t2 (t2 int64 int) int64)
       (t2 (t3 string string (option string)) (t3 string bool bool)))
  ->! Caqti_type.int64)
    "INSERT INTO project_repositories\n\
    \     (project_id, position, github_repository_id, full_name, html_url,\n\
    \      description, default_branch, is_primary, is_archived)\n\
    \   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)\n\
    \   RETURNING id"

let finalize_error_str : Fin.error -> string = function
  | Fin.Invalid_user_id -> "Invalid_user_id"
  | Fin.Invalid_draft_id -> "Invalid_draft_id"
  | Fin.Draft_unavailable -> "Draft_unavailable"
  | Fin.No_repositories_selected -> "No_repositories_selected"
  | Fin.Selection_stale -> "Selection_stale"
  | Fin.Kind_namespace_mismatch -> "Kind_namespace_mismatch"
  | Fin.Slug_unavailable -> "Slug_unavailable"
  | Fin.Repository_already_connected -> "Repository_already_connected"
  | Fin.Inconsistent_data -> "Inconsistent_data"
  | Fin.Storage_error -> "Storage_error"

let q_project_id_by_slug =
  (Caqti_type.string ->? Caqti_type.int64)
    "SELECT id FROM open_source_projects WHERE slug = $1"

let q_count_projects_for_namespace =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM open_source_projects WHERE forge_namespace_id = $1"

let q_steward_sigs =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT user_id::text || '|' ||\n\
    \          github_installation_record_id::text || '|' || role\n\
    \   FROM project_stewards WHERE project_id = $1 ORDER BY user_id"

(* One text signature per permanent repository row, ordered by position
   (mirrored by repo_sig below): identity, display metadata, both
   copied flags, and the timestamp-ordering flag. *)
let q_repo_sigs =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT position::text || '|' || github_repository_id::text || '|' ||\n\
    \          full_name || '|' || html_url || '|' ||\n\
    \          COALESCE(description, '<null>') || '|' || default_branch\n\
    \          || '|' || is_primary::text || '|' || is_archived::text\n\
    \          || '|' || (updated_at >= created_at)::text\n\
    \   FROM project_repositories WHERE project_id = $1 ORDER BY position"

let repo_sig ~position ~id ?(login = "pfin-owner") ?(description = "<null>")
    ?(branch = "main") ?(primary = false) ?(archived = false) name =
  Printf.sprintf "%d|%Ld|%s/%s|https://github.com/%s/%s|%s|%s|%b|%b|true"
    position id login name login name description branch primary archived

(* Pre-existing permanent-project fixture for the slug, claim, and
   self-reference cases; namespace id kept inside the reserved cleanup
   range. *)
let q_insert_project_fixture =
  (Caqti_type.(t2 (t2 (option int64) string) (t2 int64 string))
  ->! Caqti_type.int64)
    "INSERT INTO open_source_projects (source_onboarding_draft_id, name, slug, \
     kind, forge_namespace_id, forge_namespace_login, forge_namespace_type) \
     VALUES ($1, 'Pfin fixture', $2, 'project', $3, $4, 'user') RETURNING id"

let q_insert_repo_claim =
  (Caqti_type.(t2 int64 int64) ->! Caqti_type.int64)
    "INSERT INTO project_repositories (project_id, position, \
     github_repository_id, full_name, html_url, description, default_branch, \
     is_archived) VALUES ($1, 1, $2, 'pfin-claim/holder', \
     'https://github.com/pfin-claim/holder', NULL, 'main', FALSE) RETURNING id"

let q_insert_steward_fixture =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
    "INSERT INTO project_stewards (project_id, user_id, \
     github_installation_record_id, role) VALUES ($1, $2, $3, 'steward')"

(* A claim excludes other projects only while its project has a steward
   with fresh GitHub evidence (a claim without one is released by the
   next verified finalization), so conflict fixtures give the holder
   project such a steward. The column default dates the evidence now. *)
let hold_actively conn ~project ~holder ~ext_id ~account_id =
  let* inst =
    insert_installation ~login:"pfin-fixture" conn ~ext_id ~account_id
  in
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.exec q_insert_steward_fixture (project, holder, inst) in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "holder steward: %s" (Caqti_error.show e)

(* Every text column the three permanent tables hold for one project,
   concatenated for the boolean credential-absence checks. *)
let q_permanent_text_blob =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT p.name || '|' || p.slug || '|' ||\n\
    \          COALESCE(p.description, '') || '|' ||\n\
    \          COALESCE(p.website_url, '') || '|' || p.kind || '|' ||\n\
    \          p.forge || '|' || p.forge_namespace_login || '|' ||\n\
    \          p.forge_namespace_type || '|' || p.verification_status || '|'\n\
    \          || COALESCE((SELECT string_agg(s.role, '|')\n\
    \                       FROM project_stewards s\n\
    \                       WHERE s.project_id = p.id), '') || '|' ||\n\
    \          COALESCE((SELECT string_agg(r.full_name || '|' || r.html_url\n\
    \                                      || '|' ||\n\
    \                                      COALESCE(r.description, '') || '|'\n\
    \                                      || r.default_branch,\n\
    \                                      '|' ORDER BY r.position)\n\
    \                    FROM project_repositories r\n\
    \                    WHERE r.project_id = p.id), '')\n\
    \   FROM open_source_projects p WHERE p.id = $1"

let repo ?(owner_login = "pfin-owner") ~account_id ?description ?default_branch
    ?archived ~id name =
  Github_fixture.gur_repo ~owner_id:account_id ~owner_login ?description
    ?default_branch ?archived ~id ~name ()

(* Draft fixtures go through the real store against the real client
   chain, exactly as production writes them; the verified installation
   is returned so refresh races can rebuild the snapshot later. *)
let make_draft ?(login = "pfin-owner") ?(target = "User")
    ?(installation_type = "user") ?connected_by conn ~user ~ext_id repos =
  let account_id = Int64.add ext_id 100000L in
  let* inst =
    insert_installation ~login ~account_type:installation_type ?connected_by
      conn ~ext_id ~account_id
  in
  let* v = verified ~installation_id:ext_id ~account_id ~login ~target () in
  let* set = repo_set ~installation:v (repos account_id) in
  let* draft = refresh_ok "fixture refresh" conn ~user v set in
  Lwt.return (inst, Store.draft_id draft, v, account_id)

let snapshot_ids conn draft = collect conn "snapshot ids" q_snapshot_ids draft

(* Identities come only through the real constructor — no test-only
   builder exists. *)
let identity_exn ?(kind = Pi.Project) ?(name = "Pfin Fixture Project") ~slug
    ?description ?website ~selected ?primary () =
  match
    Pi.create ~kind ~name ~slug ~description ~website_url:website
      ~selected_snapshot_ids:selected ~primary_snapshot_id:primary
  with
  | Ok identity -> identity
  | Error _ -> Alcotest.fail "identity fixture rejected"

let finalize conn ~user ~draft identity =
  Fin.finalize conn ~user_id:user ~draft_id:draft ~identity

let finalize_ok label conn ~user ~draft identity =
  let* r = finalize conn ~user ~draft identity in
  match r with
  | Ok created -> Lwt.return created
  | Error e -> Alcotest.failf "%s: %s" label (finalize_error_str e)

let finalize_expect label expected conn ~user ~draft identity =
  let* r = finalize conn ~user ~draft identity in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (finalize_error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (finalize_error_str expected)
        (finalize_error_str e);
      Lwt.return_unit
