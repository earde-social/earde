(* Atomic finalization of one owner-authorized project-onboarding draft
   into a permanent verified project, all inside one explicit transaction.
   Lock order is the shared draft-first order (draft row, then its
   installation row, then the snapshot rows), so finalization serializes
   with Project_onboarding_draft_store.refresh_verified and
   Project_onboarding_draft_selection_store.replace on the draft row and
   can never deadlock or interleave with them. Slug and global repository
   claims are arbitrated by the unique constraints through
   ON CONFLICT DO NOTHING inserts — a read-before-insert check could not
   arbitrate concurrent finalizations. The identity value is an already-
   validated abstract input and is never reparsed; the CURRENT locked
   selection is the authoritative repository set, deliberately not the
   selected-id context the identity was constructed under. See the .mli
   for the full contract. *)

open Lwt.Infix

type created_project = { id : int64; slug : string }

let project_id { id; _ } = id
let project_slug { slug; _ } = slug

type error =
  | Invalid_user_id
  | Invalid_draft_id
  | Draft_unavailable
  | No_repositories_selected
  | Selection_stale
  | Kind_namespace_mismatch
  | Slug_unavailable
  | Repository_already_connected
  | Inconsistent_data
  | Storage_error

(* The GitHub client caps a complete listing at twenty 100-entry pages, so
   no honest snapshot can exceed this. *)
let snapshot_limit = 2000

(* Pure prechecks, before any transaction or SQL. Draft and snapshot ids
   stay int64 end to end: the BIGSERIAL columns must never be narrowed
   through OCaml int. *)
let valid_user_id user_id = user_id > 0
let positive id = Int64.compare id 0L > 0

(* Byte rules mirror the GitHub client and draft read model, the write-side
   gatekeepers: logins and names are single URL path segments (no
   whitespace, controls, DEL, or '/'); branches are opaque but
   slash-separated names are legitimate; descriptions are free text barred
   only from NUL and other controls. *)
let valid_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let valid_branch value =
  String.length value > 0
  && String.for_all
       (fun byte -> Char.code byte > 0x20 && Char.code byte <> 0x7f)
       value

let valid_description value =
  String.for_all (fun byte -> Char.code byte >= 0x20) value

(* Structured reconstruction, exactly as the client built the stored
   value — never string comparison against caller-influenced parts. *)
let canonical_html_url ~owner_login ~name =
  Uri.to_string
    (Uri.make ~scheme:"https" ~host:"github.com"
       ~path:("/" ^ owner_login ^ "/" ^ name)
       ())

(* Going through Github_onboarding keeps the canonical database strings
   defined in exactly one place; the closed variant makes the
   user/organization mapping exhaustive, and its error message (which
   echoes the raw value) is deliberately dropped. *)
let account_type_of_db value =
  match Github_onboarding.account_type_of_string value with
  | Ok parsed -> Some parsed
  | Error _ -> None

(* One validated snapshot row. Only what permanent copying and primary
   revalidation need travels past validation. *)
type snapshot_repo = {
  snapshot_id : int64;
  github_repository_id : int64;
  full_name : string;
  html_url : string;
  repo_description : string option;
  default_branch : string;
  is_archived : bool;
  is_selected : bool;
}

(* === queries === *)

(* Authorization and the draft row lock in one statement: the draft id is
   never fetched alone and checked in OCaml. FOR UPDATE here locks only
   the draft row — the same lock refresh_verified's upsert and the
   selection store's authorization take first, so all three operations
   serialize on it. The installation is deliberately not joined: it is
   locked second, under the draft lock, never before it. *)
let lock_draft_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.int64)
    "SELECT github_installation_record_id FROM project_onboarding_drafts WHERE \
     id = $1 AND user_id = $2 AND status = 'active' AND expires_at > NOW() FOR \
     UPDATE"

(* The draft's exact installation row, locked so its identity and status
   cannot change until finalization commits — the namespace triple copied
   into the project must be the one that was verified live. *)
let lock_installation_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.(t3 int64 string string))
    "SELECT github_account_id, github_account_login, github_account_type FROM \
     github_installations WHERE id = $1 AND status = 'active' AND revoked_at \
     IS NULL FOR UPDATE"

(* The complete current snapshot, locked under the already-held draft lock
   (never the other way around). *)
let lock_snapshot_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64
  ->* Caqti_type.(
        t2
          (t2 (t2 int64 int) (t2 int64 int64))
          (t2
             (t4 string string string string)
             (t2 (t2 (option string) string) (t3 bool bool bool)))))
    "SELECT id, position, github_repository_id, github_owner_id, owner_login, \
     name, full_name, html_url, description, default_branch, is_archived, \
     is_selected, is_primary FROM project_onboarding_draft_repositories WHERE \
     draft_id = $1 ORDER BY position FOR UPDATE"

(* A normally completed draft is already unavailable above; an existing
   project referencing a STILL-ACTIVE locked draft is durable corruption,
   never something to reuse or repair. *)
let draft_already_finalized_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.bool)
    "SELECT TRUE FROM open_source_projects WHERE source_onboarding_draft_id = \
     $1"

(* The slug's unique constraint arbitrates concurrent creation: zero
   returned rows means the slug is taken, and the transaction rolls back
   as a whole — never a read-before-insert check. *)
let insert_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.(
     t2
       (t2 (t2 int64 string) (t2 string (option string)))
       (t2 (t2 (option string) string) (t2 (t2 int64 string) (t2 string int))))
  ->? Caqti_type.int64)
    "INSERT INTO open_source_projects (source_onboarding_draft_id, name, slug, \
     description, website_url, kind, forge, forge_namespace_id, \
     forge_namespace_login, forge_namespace_type, verification_status, \
     created_by_user_id) VALUES ($1, $2, $3, $4, $5, $6, 'github', $7, $8, $9, \
     'verified', $10) ON CONFLICT (slug) DO NOTHING RETURNING id"

(* The permanent authorization record: the caller plus the locked
   installation record as proof — never created_by_user_id and never
   connected_by_user_id. Its evidence dates from the GitHub verification
   behind the locked draft's snapshot, not from finalization. *)
let insert_steward_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t4 int64 int int64 int64) ->? Caqti_type.int64)
    "INSERT INTO project_stewards (project_id, user_id, \
     github_installation_record_id, role, github_verified_at) SELECT $1, $2, \
     $3, 'steward', d.verified_at FROM project_onboarding_drafts d WHERE d.id \
     = $4 RETURNING project_id"

(* A claim whose project has no steward with fresh GitHub evidence no
   longer excludes anyone: its authority depended on access nobody has
   shown for the whole freshness window. Only this path releases it, and
   only for a caller who is finalizing a snapshot GitHub verified within
   the last 24 hours for the repository's own account, so a claimant
   without current access can never take a repository over.

   The stale project's claims are released together, since its authority
   lapsed as a whole. Its rows therefore stay all-active or all-released,
   and every reader keeps showing it one consistent repository set: the
   released one as history, next to its stale label. The project keeps
   its row, its homes and its communities. A claim with any fresh steward
   is untouched and still refuses the new one below. The partial unique
   index makes the subquery return at most one project. *)
let release_stale_claim_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_repositories r SET released_at = GREATEST(NOW(), \
     r.created_at), updated_at = GREATEST(NOW(), r.updated_at) WHERE \
     r.released_at IS NULL AND r.project_id = ( SELECT c.project_id FROM \
     project_repositories c WHERE c.github_repository_id = $1 AND \
     c.released_at IS NULL) AND NOT EXISTS ( SELECT 1 FROM project_stewards s \
     WHERE s.project_id = r.project_id AND \
     github_evidence_is_fresh(s.github_verified_at))"

(* The projects currently holding any selected repository, locked in
   ascending id order before any release is decided, so a concurrent
   renewal of their stewards' evidence (Project_onboarding_draft_store)
   either commits first and is seen by release_stale_claim_query's later
   snapshot, or waits for this transaction and then finds the claims
   released. It runs under the draft, installation and snapshot locks,
   in the same order renewal takes them. *)
let lock_claim_holders_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->* Caqti_type.int64)
    "SELECT p.id FROM open_source_projects p WHERE p.id IN ( SELECT \
     c.project_id FROM project_repositories c JOIN \
     project_onboarding_draft_repositories dr ON dr.github_repository_id = \
     c.github_repository_id WHERE dr.draft_id = $1 AND dr.is_selected AND \
     c.released_at IS NULL) ORDER BY p.id FOR UPDATE"

(* One prepared insert reused per selected row. The global unique
   constraint on github_repository_id arbitrates concurrent claims: zero
   returned rows means the repository already belongs to a permanent
   project and the whole transaction rolls back. *)
let insert_repository_query =
  let open Caqti_request.Infix in
  (Caqti_type.(
     t2
       (t2 (t2 int64 int) (t2 int64 string))
       (t2 (t2 string (option string)) (t2 string (t2 bool bool))))
  ->? Caqti_type.int64)
    "INSERT INTO project_repositories (project_id, position, \
     github_repository_id, full_name, html_url, description, default_branch, \
     is_primary, is_archived) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9) ON \
     CONFLICT (github_repository_id) WHERE released_at IS NULL DO NOTHING \
     RETURNING id"

(* Only lifecycle and modification move: ownership, installation record,
   expiry, and created_at are other slices' facts. The GREATEST clamp
   reuses the sibling stores' reasoning: NOW() is frozen at transaction
   start, which can predate the created_at of a draft row refreshed by a
   concurrent transaction this transaction then waited on — the
   updated_at >= created_at and lifecycle CHECKs must not turn that
   harmless skew into a failed finalization. RETURNING proves the locked
   row was updated exactly once. *)
let complete_draft_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.bool)
    "UPDATE project_onboarding_drafts SET status = 'completed', completed_at = \
     GREATEST(NOW(), created_at), cancelled_at = NULL, updated_at = \
     GREATEST(NOW(), created_at) WHERE id = $1 RETURNING TRUE"

(* === snapshot validation === *)

(* Structural invariants the write side and schema promise, re-checked as
   a whole before any permanent row is created; rows arrive ordered by
   position, so contiguity from 1 also rules out duplicate positions.
   Every owner id must be the locked installation's verified account id.
   Errors are deliberately unit: which rule failed on which value must
   not travel. On success returns the rows in position order. *)
let validate_snapshot ~account_id rows =
  let count = List.length rows in
  if count < 1 || count > snapshot_limit then Error ()
  else
    let valid_row expected
        ( ((snapshot_id, position), (github_repository_id, github_owner_id)),
          ( (owner_login, name, full_name, html_url),
            ( (repo_description, default_branch),
              (is_archived, is_selected, is_primary) ) ) ) =
      if
        positive snapshot_id && position = expected
        && positive github_repository_id
        && positive github_owner_id
        && Int64.equal github_owner_id account_id
        && valid_segment owner_login && valid_segment name
        && String.equal full_name (owner_login ^ "/" ^ name)
        && String.equal html_url (canonical_html_url ~owner_login ~name)
        && valid_branch default_branch
        && (match repo_description with
          | None -> true
          | Some text -> valid_description text)
        && ((not is_primary) || is_selected)
      then
        Ok
          ( {
              snapshot_id;
              github_repository_id;
              full_name;
              html_url;
              repo_description;
              default_branch;
              is_archived;
              is_selected;
            },
            is_primary )
      else Error ()
    in
    let rec per_row expected primaries acc = function
      | [] -> Ok (List.rev acc, primaries)
      | row :: rest -> (
          match valid_row expected row with
          | Error () -> Error ()
          | Ok (repo, is_primary) ->
              per_row (expected + 1)
                (if is_primary then primaries + 1 else primaries)
                (repo :: acc) rest)
    in
    match per_row 1 0 [] rows with
    | Error () -> Error ()
    | Ok (repos, primaries) ->
        let all_distinct project =
          List.length (List.sort_uniq compare (List.map project repos))
          = List.length repos
        in
        if
          primaries <= 1
          && all_distinct (fun r -> r.snapshot_id)
          && all_distinct (fun r -> r.github_repository_id)
          && all_distinct (fun r -> r.full_name)
        then Ok repos
        else Error ()

let finalize (module C : Caqti_lwt.CONNECTION) ~user_id ~draft_id ~identity =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else if not (positive draft_id) then Lwt.return (Error Invalid_draft_id)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free —
       error payloads can echo SQL parameters (identity fields, repository
       metadata) — and rollback failure adds nothing a caller may act on
       either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in

    (* Renumbered contiguous copying of the selected rows in snapshot
       order; a conflict-swallowed insert (zero rows back) is a repository
       already claimed by another permanent project. *)
    let primary_snapshot_id = Project_identity.primary_snapshot_id identity in
    let rec copy_repositories project_row_id position = function
      | [] -> (
          C.find_opt complete_draft_query draft_id >>= function
          | Error _ -> rollback_to Storage_error
          | Ok None ->
              (* The row was locked above; a vanished update is
                 corruption, never papered over. *)
              rollback_to Inconsistent_data
          | Ok (Some _) -> (
              C.commit () >>= function
              | Error _ -> Lwt.return (Error Storage_error)
              | Ok () ->
                  Lwt.return
                    (Ok
                       {
                         id = project_row_id;
                         slug = Project_identity.slug identity;
                       })))
      | repo :: rest -> (
          let is_primary =
            match primary_snapshot_id with
            | None -> false
            | Some primary -> Int64.equal primary repo.snapshot_id
          in
          C.exec release_stale_claim_query repo.github_repository_id
          >>= function
          | Error _ -> rollback_to Storage_error
          | Ok () -> (
              C.find_opt insert_repository_query
                ( ( (project_row_id, position),
                    (repo.github_repository_id, repo.full_name) ),
                  ( (repo.html_url, repo.repo_description),
                    (repo.default_branch, (is_primary, repo.is_archived)) ) )
              >>= function
              | Error _ -> rollback_to Storage_error
              | Ok None -> rollback_to Repository_already_connected
              | Ok (Some _) ->
                  copy_repositories project_row_id (position + 1) rest))
    in

    let create_rows ~installation_record_id ~account_id ~account_login
        ~account_type selected =
      C.find_opt insert_project_query
        ( ( (draft_id, Project_identity.name identity),
            ( Project_identity.slug identity,
              Project_identity.description identity ) ),
          ( ( Project_identity.website_url identity,
              Project_identity.string_of_kind (Project_identity.kind identity)
            ),
            ((account_id, account_login), (account_type, user_id)) ) )
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None -> rollback_to Slug_unavailable
      | Ok (Some project_row_id) -> (
          C.find_opt insert_steward_query
            (project_row_id, user_id, installation_record_id, draft_id)
          >>= function
          | Error _ -> rollback_to Storage_error
          (* The draft row is locked above; no row back is corruption. *)
          | Ok None -> rollback_to Inconsistent_data
          | Ok (Some _) -> (
              C.collect_list lock_claim_holders_query draft_id >>= function
              | Error _ -> rollback_to Storage_error
              | Ok _ -> copy_repositories project_row_id 1 selected))
    in

    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt lock_draft_query (draft_id, user_id) >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None ->
            (* Every zero-row cause (missing draft, another user's draft,
               expired, completed — including a post-success replay —
               cancelled) collapses into one error so the store cannot be
               used to probe drafts. *)
            rollback_to Draft_unavailable
        | Ok (Some installation_record_id) -> (
            if not (positive installation_record_id) then
              rollback_to Inconsistent_data
            else
              C.find_opt lock_installation_query installation_record_id
              >>= function
              | Error _ -> rollback_to Storage_error
              | Ok None ->
                  (* Missing, inaccessible, revoked, and revoked_at all
                     collapse: installations are not probeable either. *)
                  rollback_to Draft_unavailable
              | Ok (Some (account_id, account_login, account_type_raw)) -> (
                  match account_type_of_db account_type_raw with
                  | None -> rollback_to Inconsistent_data
                  | Some parsed_account_type -> (
                      if not (positive account_id && valid_segment account_login)
                      then rollback_to Inconsistent_data
                      else if
                        (* A personal namespace cannot carry the
                           "GitHub organization" kind; every other kind is
                           namespace-flexible. *)
                        Project_identity.kind identity
                        = Project_identity.Organization
                        && parsed_account_type = Github_onboarding.User
                      then rollback_to Kind_namespace_mismatch
                      else
                        C.collect_list lock_snapshot_query draft_id >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok rows -> (
                            match validate_snapshot ~account_id rows with
                            | Error () -> rollback_to Inconsistent_data
                            | Ok repos -> (
                                let selected =
                                  List.filter (fun r -> r.is_selected) repos
                                in
                                let primary_ok =
                                  match
                                    ( Project_identity.kind identity,
                                      primary_snapshot_id )
                                  with
                                  | Project_identity.Project, None ->
                                      (* The constructor guarantees a
                                         primary for kind Project; its
                                         absence is an impossible public-
                                         API state, reported as
                                         corruption. *)
                                      Error Inconsistent_data
                                  | _, None -> Ok ()
                                  | _, Some primary ->
                                      if
                                        List.exists
                                          (fun r ->
                                            Int64.equal r.snapshot_id primary)
                                          selected
                                      then Ok ()
                                      else Error Selection_stale
                                in
                                match (selected, primary_ok) with
                                | [], _ -> rollback_to No_repositories_selected
                                | _, Error err -> rollback_to err
                                | _ :: _, Ok () -> (
                                    C.find_opt draft_already_finalized_query
                                      draft_id
                                    >>= function
                                    | Error _ -> rollback_to Storage_error
                                    | Ok (Some _) ->
                                        rollback_to Inconsistent_data
                                    | Ok None ->
                                        create_rows ~installation_record_id
                                          ~account_id ~account_login
                                          ~account_type:
                                            (Github_onboarding
                                             .string_of_account_type
                                               parsed_account_type)
                                          selected)))))))
