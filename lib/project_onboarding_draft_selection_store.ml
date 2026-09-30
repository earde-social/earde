(* Owner-authorized replacement of the repository selection on one
   available project-onboarding draft, in one explicit transaction. The
   caller speaks in LOCAL snapshot-row ids, so a GitHub re-verification
   that rebuilds the snapshot automatically invalidates any stale browser
   submission — old ids simply no longer belong to the current snapshot.
   Lock ordering matches Project_onboarding_draft_store.refresh_verified
   (draft row first, snapshot rows second), so selection updates and
   snapshot refreshes serialize on the draft row and can never deadlock or
   interleave. See the .mli for the full contract. *)

open Lwt.Infix

type error =
  | Invalid_user_id
  | Invalid_draft_id
  | Invalid_selection
  | Draft_unavailable
  | Selection_stale
  | Inconsistent_data
  | Storage_error

(* The GitHub client caps a complete listing at twenty 100-entry pages, so
   no honest snapshot — and hence no honest selection — can exceed this. *)
let selection_limit = 2000

(* Pure prechecks, before any transaction or SQL. Draft and snapshot ids
   stay int64 end to end: the BIGSERIAL columns must never be narrowed
   through OCaml int. *)
let valid_user_id user_id = user_id > 0
let positive id = Int64.compare id 0L > 0

let all_distinct ids =
  List.length (List.sort_uniq Int64.compare ids) = List.length ids

(* An empty selection is legitimate (a draft may sit unconfigured); a
   supplied primary must be positive and — duplicates being rejected
   already — occur exactly once in the selected list. Project-kind policy
   (primary required for single projects, at-least-one-selected) belongs
   to final project creation, deliberately not here. *)
let valid_selection ~selected_snapshot_ids ~primary_snapshot_id =
  List.length selected_snapshot_ids <= selection_limit
  && List.for_all positive selected_snapshot_ids
  && all_distinct selected_snapshot_ids
  &&
  match primary_snapshot_id with
  | None -> true
  | Some id -> positive id && List.exists (Int64.equal id) selected_snapshot_ids

(* Authorization and the row lock in one statement: the draft id is never
   fetched alone and checked in OCaml. Availability mirrors the read
   model — an unexpired active draft backed by a live installation —
   and connected_by_user_id (provenance, never ownership) is deliberately
   absent. FOR UPDATE OF d locks only the draft row, the same lock
   refresh_verified's upsert takes first, so the two operations serialize
   there; the installation row is read, never locked or modified. *)
let authorize_draft_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.bool)
    "SELECT TRUE FROM project_onboarding_drafts d JOIN github_installations i \
     ON i.id = d.github_installation_record_id WHERE d.id = $1 AND d.user_id = \
     $2 AND d.status = 'active' AND d.expires_at > NOW() AND i.status = \
     'active' AND i.revoked_at IS NULL FOR UPDATE OF d"

(* The complete current snapshot, locked under the already-held draft
   lock (never the other way around). Only identity and selection state
   travel; repository metadata stays in the database. *)
let load_snapshot_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->* Caqti_type.(t2 (t2 int64 int) (t2 bool bool)))
    "SELECT id, position, is_selected, is_primary FROM \
     project_onboarding_draft_repositories WHERE draft_id = $1 ORDER BY \
     position FOR UPDATE"

(* Structural invariants the write side and schema promise; rows arrive
   ordered by position, so contiguity from 1 also rules out duplicate
   positions. Any durable violation is corruption, never repaired here.
   On success returns the snapshot ids in position order. *)
let validate_snapshot rows =
  let count = List.length rows in
  if count < 1 || count > selection_limit then Error ()
  else
    let rec per_row expected primaries ids = function
      | [] -> Ok (List.rev ids, primaries)
      | ((id, position), (is_selected, is_primary)) :: rest ->
          if
            positive id && position = expected
            && ((not is_primary) || is_selected)
          then
            per_row (expected + 1)
              (if is_primary then primaries + 1 else primaries)
              (id :: ids) rest
          else Error ()
    in
    match per_row 1 0 [] rows with
    | Error () -> Error ()
    | Ok (ids, primaries) ->
        if all_distinct ids && primaries <= 1 then Ok ids else Error ()

(* Complete replacement, step one: every current row drops to unselected
   and non-primary, so nothing omitted from the new submission can
   survive. Clearing is_primary here also keeps the primary-implies-
   selected CHECK and the one-primary-per-draft index satisfied while
   step two re-selects rows in sequence. *)
let reset_selection_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_draft_repositories SET is_selected = FALSE, \
     is_primary = FALSE, updated_at = NOW() WHERE draft_id = $1"

(* Step two, one prepared statement reused per selected row. id is the
   primary key, so RETURNING TRUE yields exactly zero or one row; the
   draft_id predicate re-scopes the row to the locked draft even though
   validation already proved membership. Selection and primary state are
   written together — is_selected is the literal TRUE, so the
   primary-implies-selected CHECK holds row by row. *)
let select_row_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int64 int64 bool) ->? Caqti_type.bool)
    "UPDATE project_onboarding_draft_repositories SET is_selected = TRUE, \
     is_primary = $3, updated_at = NOW() WHERE id = $1 AND draft_id = $2 \
     RETURNING TRUE"

(* Only the modification timestamp moves: expiry remains the verification
   freshness boundary that solely a new successful GitHub verification may
   renew, and status/lifecycle/ownership are other slices' business. The
   GREATEST clamp reuses Project_onboarding_draft_store's reasoning:
   NOW() is frozen at transaction start, which can predate the created_at
   of a draft row created or refreshed by a concurrent transaction this
   transaction then waited on — the updated_at >= created_at CHECK must
   not turn that harmless skew into a failure. *)
let touch_draft_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts SET updated_at = GREATEST(NOW(), \
     created_at) WHERE id = $1"

module Id_set = Set.Make (Int64)

let replace (module C : Caqti_lwt.CONNECTION) ~user_id ~draft_id
    ~selected_snapshot_ids ~primary_snapshot_id =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else if not (positive draft_id) then Lwt.return (Error Invalid_draft_id)
  else if not (valid_selection ~selected_snapshot_ids ~primary_snapshot_id) then
    Lwt.return (Error Invalid_selection)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free —
       error payloads can echo SQL parameters — and rollback failure adds
       nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    let is_primary id =
      match primary_snapshot_id with
      | None -> false
      | Some primary -> Int64.equal primary id
    in
    (* Sequential prepared updates over the validated selection; each must
       hit exactly the one locked row it names — anything else means the
       snapshot changed under our locks, which cannot happen and is
       reported as corruption, never papered over. *)
    let rec apply_selection = function
      | [] -> (
          C.exec touch_draft_query draft_id >>= function
          | Error _ -> rollback_to Storage_error
          | Ok () -> (
              C.commit () >>= function
              | Error _ -> Lwt.return (Error Storage_error)
              | Ok () -> Lwt.return (Ok ())))
      | id :: rest -> (
          C.find_opt select_row_query (id, draft_id, is_primary id) >>= function
          | Error _ -> rollback_to Storage_error
          | Ok None -> rollback_to Inconsistent_data
          | Ok (Some _) -> apply_selection rest)
    in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt authorize_draft_query (draft_id, user_id) >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None ->
            (* Every zero-row cause (missing draft, another user's draft,
               expired, terminal, inaccessible or revoked installation)
               collapses into one error so the store cannot be used to
               probe drafts or installations. *)
            rollback_to Draft_unavailable
        | Ok (Some _) -> (
            C.collect_list load_snapshot_query draft_id >>= function
            | Error _ -> rollback_to Storage_error
            | Ok rows -> (
                match validate_snapshot rows with
                | Error () -> rollback_to Inconsistent_data
                | Ok snapshot_ids -> (
                    (* Stale detection only after owner authorization: a
                       supplied id either names a row of this exact locked
                       snapshot or the submission is stale — where else the
                       id might live is never revealed. *)
                    let current = Id_set.of_list snapshot_ids in
                    let in_snapshot id = Id_set.mem id current in
                    if
                      not
                        (List.for_all in_snapshot selected_snapshot_ids
                        &&
                        match primary_snapshot_id with
                        | None -> true
                        | Some id -> in_snapshot id)
                    then rollback_to Selection_stale
                    else
                      C.exec reset_selection_query draft_id >>= function
                      | Error _ -> rollback_to Storage_error
                      | Ok () -> apply_selection selected_snapshot_ids))))
