(* Create-or-refresh of a user's project-onboarding draft plus complete
   snapshot replacement, all inside one explicit transaction. The two
   GitHub-derived inputs are abstract proof values whose constructors
   already validated every byte, so nothing is reparsed or repaired here;
   the drafts slot is arbitrated by the partial unique index (no
   read-before-insert), and expiry authority is the transaction's NOW(), so
   draft lifetime cannot drift with the application clock. See the .mli for
   the full ownership and privacy contract. *)

open Lwt.Infix

type draft = { id : int64 }

let draft_id { id } = id

type error =
  | Invalid_user_id
  | Installation_unavailable
  | Storage_error

(* Pure precheck, before any SQL: drafts only exist for authenticated users
   with real ids. *)
let valid_user_id user_id = user_id > 0

(* The two closed account-type variants map one-to-one; going through
   Github_onboarding keeps the canonical database strings defined in exactly
   one place, and never derives them from arbitrary input. *)
let domain_account_type = function
  | Github_user_installations.User -> Github_onboarding.User
  | Github_user_installations.Organization -> Github_onboarding.Organization

(* Authoritative lookup of the local installation record: the full verified
   identity (external installation id, account id, canonical account type)
   must match a live row. The renamable login is deliberately absent, and
   connected_by_user_id is provenance, never an authorization input.
   github_installation_id is UNIQUE, so at most one row can match. *)
let find_installation_record_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int64 int64 string) ->? Caqti_type.int64)
  "SELECT id FROM github_installations \
   WHERE github_installation_id = $1 \
     AND github_account_id = $2 \
     AND github_account_type = $3 \
     AND status = 'active' \
     AND revoked_at IS NULL"

(* One atomic insert-or-refresh arbitrated by the partial unique index on
   active drafts. A fresh insert claims the slot; a conflict means an
   existing ACTIVE row (expired or not — expiry never frees the slot), which
   is refreshed in place: only expires_at and updated_at change, so id,
   created_at, and the NULL lifecycle timestamps survive. Terminal rows are
   outside the index and therefore can never conflict or be touched.
   Concurrent callers for the same slot serialize on this statement's row
   lock, which then covers the snapshot replacement below until commit.
   The GREATEST clamp exists for exactly that race: NOW() is frozen at
   transaction start, so when the earlier-started transaction loses the
   insert and refreshes the winner's row, its NOW() can sit microseconds
   before the stored created_at — the updated_at >= created_at CHECK must
   not turn that harmless skew into a failed refresh. *)
let upsert_draft_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int64) ->! Caqti_type.int64)
  "INSERT INTO project_onboarding_drafts \
     (user_id, github_installation_record_id, status, expires_at) \
   VALUES ($1, $2, 'active', NOW() + INTERVAL '24 hours') \
   ON CONFLICT (user_id, github_installation_record_id) \
     WHERE status = 'active' \
   DO UPDATE \
   SET expires_at = NOW() + INTERVAL '24 hours', \
       updated_at = GREATEST(NOW(), project_onboarding_drafts.created_at) \
   RETURNING id"

let delete_snapshot_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM project_onboarding_draft_repositories WHERE draft_id = $1"

(* One prepared insert reused for every repository row. Selection state is
   the SQL literal FALSE — a refreshed verification is a new authoritative
   snapshot, so no previous selection survives — and the snapshot timestamps
   come from the column defaults. *)
let insert_snapshot_row_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 (t2 int64 int) (t2 int64 int64))
                  (t2 (t4 string string string string)
                      (t2 (t2 (option string) string) bool)))
   ->. Caqti_type.unit)
  "INSERT INTO project_onboarding_draft_repositories \
     (draft_id, position, github_repository_id, github_owner_id, \
      owner_login, name, full_name, html_url, description, \
      default_branch, is_archived, is_selected, is_primary) \
   VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, FALSE, FALSE)"

let refresh_verified (module C : Caqti_lwt.CONNECTION) ~user_id ~installation
    ~repositories =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else
    let account_type =
      Github_onboarding.string_of_account_type
        (domain_account_type
           (Github_user_installations.account_type installation))
    in
    (* As in the state store, every Caqti error is dropped payload-free —
       error payloads can echo SQL parameters (repository metadata) — and
       rollback failure adds nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in

    (* Sequential prepared inserts keep memory bounded at one row for the
       documented 2,000-repository maximum; the list order is the snapshot
       order, positions contiguous from 1. *)
    let rec insert_snapshot draft_row_id position = function
      | [] -> (
          C.commit () >>= function
          | Error _ -> Lwt.return (Error Storage_error)
          | Ok () -> Lwt.return (Ok { id = draft_row_id }))
      | repo :: rest -> (
          let module R = Github_user_installation_repositories in
          C.exec insert_snapshot_row_query
            ( ( (draft_row_id, position),
                (R.repository_id repo, R.owner_id repo) ),
              ( (R.owner_login repo, R.name repo, R.full_name repo,
                 R.html_url repo),
                ((R.description repo, R.default_branch repo),
                 R.is_archived repo) ) )
          >>= function
          | Error _ -> rollback_to Storage_error
          | Ok () -> insert_snapshot draft_row_id (position + 1) rest)
    in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt find_installation_record_query
          ( Github_user_installations.installation_id installation,
            Github_user_installations.account_id installation,
            account_type )
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None ->
            (* Every zero-row cause (missing, inaccessible, revoked,
               account-id or account-type mismatch) collapses into one
               error so the store cannot be used to probe installations. *)
            rollback_to Installation_unavailable
        | Ok (Some installation_record_id) -> (
            C.find upsert_draft_query (user_id, installation_record_id)
            >>= function
            | Error _ -> rollback_to Storage_error
            | Ok draft_row_id -> (
                C.exec delete_snapshot_query draft_row_id >>= function
                | Error _ -> rollback_to Storage_error
                | Ok () ->
                    insert_snapshot draft_row_id 1
                      (Github_user_installation_repositories.repositories
                         repositories))))
