(* Atomic removal of one exact accepted home relation, all inside one
   explicit transaction in the shared project-first lock order (project
   row, target community row, then the three durable authorization rows in
   a fixed order, then the exact accepted relation row) that the sibling
   request and review stores established. Both sides of the relation may
   remove it, so authorization is the union of project-side stewardship,
   community-side top moderation, and the durable global-administrator
   flag — all three consulted and locked on every call, in the same order,
   even once one has qualified, so no future mutation path can invert the
   authorization-row order and deadlock against this one. The lifecycle
   transition itself comes only from the pure Project_home_relation domain
   over the reconstructed accepted value. See the .mli for the full
   contract. *)

open Lwt.Infix

type removed_home = { resulting_status : Project_home_relation.status }

let resulting_status { resulting_status } = resulting_status

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_slug
  | Project_unavailable
  | Community_unavailable
  | Actor_unauthorized
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and the sibling stores require. Route values must already be
   canonical: nothing here lowercases, trims, or repairs. *)
let canonical_project_slug value =
  let length = String.length value in
  let is_alnum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  let rec check i =
    i >= length
    ||
    match value.[i] with
    | c when is_alnum c -> check (i + 1)
    | '-' -> i > 0 && is_alnum value.[i - 1] && check (i + 1)
    | _ -> false
  in
  length >= 1 && length <= 80 && is_alnum value.[length - 1] && check 0

(* Community slugs carry no schema grammar (legacy creation only trims),
   so the strongest canonical predicate the repository has is the shared
   addressability shape of the sibling stores: every community lives at
   /c/:slug, hence one non-empty URL path segment — no whitespace,
   controls, DEL, or '/'. Nothing stricter is imposed on legacy slugs, and
   nothing is repaired. *)
let canonical_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* A community name is free display text, so only blankness and the byte
   classes that cannot appear in rendered text at all are barred. *)
let nonblank value =
  String.exists
    (fun c ->
      not
        (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

let control_safe value =
  String.for_all
    (fun byte ->
      let code = Char.code byte in
      (code >= 0x20 && code <> 0x7f) || byte = '\t')
    value

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* The project row is locked first — the lock every sibling home operation
   takes first — by exact supplied slug. Deliberately no verification
   filter and no steward join: removal must remain possible while the
   project is verified, stale, or revoked (otherwise losing verification
   would durably weld the home in place), and project-side authority is
   locked separately below so the community side can remove too. *)
let lock_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t3 int64 string string))
    "SELECT p.id, p.slug, p.verification_status FROM open_source_projects p \
     WHERE p.slug = $1 FOR UPDATE OF p"

(* The exact closed schema vocabulary (mirrors the open_source_projects
   verification CHECK); anything else in the durable column is corruption,
   never a fourth state. All three known states permit removal. *)
let known_verification_status = function
  | "verified" | "stale" | "revoked" -> true
  | _ -> false

(* The exact community, locked second under the held project lock. No
   host-eligibility filter either: an accepted home must stay removable
   after the community went private, unpublished, or non-network. The
   lifecycle columns come back only for closed-value validation. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.string
  ->? Caqti_type.(
        t2
          (t2 (t2 int string) (t2 string string))
          (t2 string (t3 bool bool bool))))
    "SELECT id, slug, name, visibility, onboarding_state, \
     is_network_community, indexable, discoverable FROM communities WHERE slug \
     = $1 FOR UPDATE"

(* Structural validity of the locked community. The shared lifecycle rule
   decides most shapes; the one drifted shape it rejects but removal must
   still handle — a published network community that has since gone fully
   private (visibility reverted, nothing leaking through indexing or
   discovery) — is valid-but-ineligible drift, and removal never requires
   eligibility. Leaking shapes (an indexable draft, a discoverable private
   community, mixed published flags) remain corruption. *)
let community_structurally_valid ~is_network_community ~onboarding_state
    ~visibility ~indexable ~discoverable =
  Network_communities.lifecycle_state_valid ~is_network_community
    ~onboarding_state ~visibility ~indexable ~discoverable
  || is_network_community
     && onboarding_state = Community_types.Community_published
     && visibility = Community_types.Community_private
     && (not indexable) && not discoverable

(* The exact unpublished dedicated-community setup draft: a network
   community still in draft, private, and leaking through neither indexing
   nor discovery. Its provisioned accepted home is part of the draft's
   structural integrity — like its initial membership, top moderator,
   section, and channel — so while the draft is unpublished the relation
   cannot be detached by anyone. Removing it would leave a community that
   can never be published (the publication store requires the relation) and
   an orphan shell nothing else can reach. Publication as Public or Unlisted
   ends the protection: the ordinary unilateral policy resumes. *)
let unpublished_setup_draft ~is_network_community ~onboarding_state ~visibility
    ~indexable ~discoverable =
  is_network_community
  && onboarding_state = Community_types.Community_draft
  && visibility = Community_types.Community_private
  && (not indexable) && not discoverable

(* Actor authorization, third, fourth and fifth: all three durable sources
   are queried and locked inside SQL, always in this order and always in
   full, so a concurrent stewardship deletion, role removal or downgrade,
   or admin revocation serializes through the matching row lock (the
   predicate is re-evaluated after a blocking writer commits) and so every
   caller takes the same authorization-row order. First the project-side
   steward link — never created_by_user_id or installation provenance... *)
let lock_steward_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.string)
    "SELECT role FROM project_stewards WHERE project_id = $1 AND user_id = $2 \
     FOR UPDATE"

(* ...then the community-side current top moderator role of the locked
   community ('mod' and 'legacy_mod' do not qualify, and ordinary
   membership is not consulted at all)... *)
let lock_top_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "SELECT role FROM community_moderators WHERE user_id = $1 AND community_id \
     = $2 AND role = 'top_mod' FOR UPDATE"

(* ...then the durable global administrator flag on the users row — the
   only durable representation of Earde administrators. A session-only
   claim never reaches this store. *)
let lock_admin_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.bool)
    "SELECT is_admin FROM users WHERE id = $1 AND is_admin FOR UPDATE"

(* The exact accepted relation, locked last. The partial unique active-home
   index caps this at one row; every zero-row cause (no relation, pending,
   rejected, already removed, an accepted home in another community, a
   concurrent winner) collapses into one variant at the call site. The
   timestamp columns reduce to the presence and ordering booleans the row
   contract needs — their values are never exposed. *)
let lock_accepted_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
  ->? Caqti_type.(
        t2
          (t2 (t2 int64 string) (t2 string (option string)))
          (t2 (t2 (option int) (option int)) (t2 (t2 bool bool) (t2 bool bool))))
  )
    "SELECT id, relation_type, status, request_note, requested_by_user_id, \
     reviewed_by_user_id, reviewed_at IS NOT NULL, removed_at IS NULL, \
     COALESCE(reviewed_at >= created_at, FALSE), updated_at >= created_at FROM \
     community_projects WHERE project_id = $1 AND community_id = $2 AND \
     relation_type = 'home' AND status = 'accepted' FOR UPDATE"

(* The one mutation: the locked row, guarded again on accepted so a zero
   count is a completed concurrent removal, never a second write. The
   status vocabulary comes exclusively from the pure domain via
   string_of_status at the call site. There is no removed_by_user_id
   column, and reviewed_by_user_id is deliberately left alone — actor
   provenance is future audit/modlog territory, never a review rewrite.
   GREATEST clamps against the NOW()-before-created_at/reviewed_at race the
   schema ordering CHECKs would otherwise trip on same-transaction-batch
   fixtures. *)
let update_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 string)
  ->* Caqti_type.(t2 (t2 int64 string) (t2 (t2 bool bool) (t2 bool bool))))
    "UPDATE community_projects SET status = $2, removed_at = GREATEST(NOW(), \
     reviewed_at, created_at), updated_at = GREATEST(NOW(), reviewed_at, \
     created_at) WHERE id = $1 AND status = 'accepted' RETURNING id, status, \
     removed_at IS NOT NULL, COALESCE(removed_at >= reviewed_at, FALSE), \
     removed_at >= created_at, updated_at >= created_at"

(* The abstract accepted state, reconstructed through the public pure API
   only: the canonical note through the pending constructor, then the
   domain's own Accept, then Remove. It validates the status/note state and
   nothing else — the pure domain encodes no requester/reviewer provenance,
   so none is inferred from the row and none is written back. *)
let removed_status_of_note stored_note =
  match Project_home_relation.create_pending ~request_note:stored_note with
  | Error _ -> None
  | Ok pending -> (
      if Project_home_relation.request_note pending <> stored_note then None
      else
        match
          Project_home_relation.apply pending Project_home_relation.Accept
        with
        | Error _ -> None
        | Ok accepted -> (
            if
              Project_home_relation.status accepted
              <> Project_home_relation.Accepted
            then None
            else
              match
                Project_home_relation.apply accepted
                  Project_home_relation.Remove
              with
              | Error _ -> None
              | Ok removed ->
                  let status = Project_home_relation.status removed in
                  if status <> Project_home_relation.Removed then None
                  else Some status))

let remove (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~project_slug
    ~community_slug =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else if not (canonical_community_slug community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free
       — error payloads can echo SQL parameters — and rollback failure
       adds nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    (* Step 5: the exact locked row, guarded again on accepted, then the
       returned data validated before the commit. *)
    let write_removal ~project_id ~community_id ~relation_id ~new_status =
      let status_string = Project_home_relation.string_of_status new_status in
      C.collect_list update_relation_query (relation_id, status_string)
      >>= function
      (* Includes any unexpected constraint failure — the removed row
         leaves the active-home index predicate, so a unique conflict is
         impossible under the locked protocol. *)
      | Error _ -> rollback_to Storage_error
      | Ok [] -> rollback_to Removal_unavailable
      | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data
      | Ok
          [
            ( (updated_id, updated_status),
              ( (removed_present, removed_after_reviewed),
                (removed_after_created, updated_after_created) ) );
          ] -> (
          if
            not
              (Int64.equal updated_id relation_id
              && String.equal updated_status status_string
              && Project_home_relation.status_of_string updated_status
                 = Some Project_home_relation.Removed
              && removed_present && removed_after_reviewed
              && removed_after_created && updated_after_created)
          then rollback_to Inconsistent_data
          else
            (* The audit event rides the same transaction: inserted only
               after the guarded accepted→removed update validated, and
               any audit failure rolls the whole removal back. Only the
               caller's identity is recorded — never which of the three
               authorization sources qualified. *)
            Project_home_audit.insert
              (module C)
              ~action:Project_home_audit.Home_removed ~actor_user_id ~project_id
              ~community_id ~relation_id
            >>= function
            | Error Project_home_audit.Inconsistent_data ->
                rollback_to Inconsistent_data
            | Error Project_home_audit.Storage_error ->
                rollback_to Storage_error
            | Ok () -> (
                (* Durable notifications ride the same transaction: both
                   current counterparties — every project steward and
                   every community top moderator — resolved under the
                   held locks, deduplicated, minus the actor. The same
                   symmetric set serves steward-side, moderator-side,
                   admin, and multiply-authorized removals, so which
                   authority qualified never shapes (or leaks through)
                   the recipients. A notification failure rolls the
                   removal and its audit event back whole; a set left
                   empty by exclusion is legitimate. *)
                Project_home_notifications.project_steward_ids
                  (module C)
                  ~project_id
                >>= function
                | Error Project_home_notifications.Inconsistent_data ->
                    rollback_to Inconsistent_data
                | Error Project_home_notifications.Storage_error ->
                    rollback_to Storage_error
                | Ok steward_ids -> (
                    Project_home_notifications.community_top_moderator_ids
                      (module C)
                      ~community_id
                    >>= function
                    | Error Project_home_notifications.Inconsistent_data ->
                        rollback_to Inconsistent_data
                    | Error Project_home_notifications.Storage_error ->
                        rollback_to Storage_error
                    | Ok top_moderator_ids -> (
                        Project_home_notifications.insert_many
                          (module C)
                          ~kind:Project_home_notifications.Home_removed
                          ~actor_user_id ~project_id ~community_id ~relation_id
                          ~recipient_user_ids:(steward_ids @ top_moderator_ids)
                        >>= function
                        | Error Project_home_notifications.Inconsistent_data ->
                            rollback_to Inconsistent_data
                        | Error Project_home_notifications.Storage_error ->
                            rollback_to Storage_error
                        | Ok () -> (
                            C.commit () >>= function
                            | Error _ -> Lwt.return (Error Storage_error)
                            | Ok () ->
                                Lwt.return
                                  (Ok { resulting_status = new_status }))))))
    in
    (* Step 4: the exact accepted relation, locked last of all. *)
    let remove_relation ~project_id ~community_id ~setup_draft =
      C.find_opt lock_accepted_relation_query (project_id, community_id)
      >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None -> rollback_to Removal_unavailable
      | Ok
          (Some
             ( ((relation_id, relation_type), (status_raw, stored_note)),
               ( (requested_by, reviewed_by),
                 ((has_reviewed_at, removed_at_null), (reviewed_ge, updated_ge))
               ) )) -> (
          (* Requester and reviewer are both legitimately NULL — an
             auto-provisioned home never had either, and both foreign keys
             are ON DELETE SET NULL — so nullability proves nothing about
             provenance and none is inferred from it. *)
          let row_shape_ok =
            positive relation_id
            && String.equal relation_type "home"
            && String.equal status_raw "accepted"
            && (match requested_by with None -> true | Some id -> id > 0)
            && (match reviewed_by with None -> true | Some id -> id > 0)
            && has_reviewed_at && removed_at_null && reviewed_ge && updated_ge
          in
          if not row_shape_ok then rollback_to Inconsistent_data
          else if setup_draft then
            (* Community and relation are classified together, only here —
               after the project, the community, all three authorization
               rows, and the relation itself have been locked and
               validated, so an unauthorized caller still cannot learn that
               a community exists, let alone its lifecycle.

               The provisioning store writes the dedicated draft's home
               with no requester, no reviewer, and no note (row_shape_ok
               already proved reviewed_at present and removed_at NULL), and
               that is the only accepted shape a draft may carry: a
               dedicated draft has no moderator-reviewed home, because
               nobody ever requested or reviewed one. So the exact
               provisioned shape is the protected relation, and anything
               else on a draft is contradictory provenance rather than an
               ordinary removable relation. *)
            let provisioned_shape =
              requested_by = None && reviewed_by = None && stored_note = None
            in
            if provisioned_shape then
              (* Deliberately the same payload-free variant every zero-row
                 cause collapses into, so protection is not an oracle for
                 "unpublished draft". Nothing is written; the transaction
                 rolls back whole. *)
              rollback_to Removal_unavailable
            else rollback_to Inconsistent_data
          else
            match removed_status_of_note stored_note with
            | None -> rollback_to Inconsistent_data
            | Some new_status ->
                write_removal ~project_id ~community_id ~relation_id ~new_status
          )
    in
    (* Step 3: every authorization row is locked, in this fixed order,
       before any of them decides anything — short-circuiting would make
       the set of rows a caller locks depend on which authority it holds,
       and two such callers could then deadlock. Which authority was
       absent never leaves this step. *)
    let authorize ~project_id ~community_id k =
      C.find_opt lock_steward_query (project_id, actor_user_id) >>= function
      | Error _ -> rollback_to Storage_error
      | Ok steward_row -> (
          if
            not
              (match steward_row with
              | None -> true
              | Some role -> String.equal role "steward")
          then rollback_to Inconsistent_data
          else
            let steward_ok = steward_row <> None in
            C.find_opt lock_top_moderator_query (actor_user_id, community_id)
            >>= function
            | Error _ -> rollback_to Storage_error
            | Ok moderator_row -> (
                if
                  not
                    (match moderator_row with
                    | None -> true
                    | Some role -> String.equal role "top_mod")
                then rollback_to Inconsistent_data
                else
                  let top_mod_ok = moderator_row <> None in
                  C.find_opt lock_admin_query actor_user_id >>= function
                  | Error _ -> rollback_to Storage_error
                  | Ok admin_row ->
                      if admin_row = Some false then
                        rollback_to Inconsistent_data
                      else
                        let admin_ok = admin_row = Some true in
                        if steward_ok || top_mod_ok || admin_ok then k ()
                        else rollback_to Actor_unauthorized))
    in
    (* Step 2: the exact community, under the held project lock. *)
    let lock_community ~project_id =
      C.find_opt lock_community_query community_slug >>= function
      | Error _ -> rollback_to Storage_error
      | Ok None -> rollback_to Community_unavailable
      | Ok
          (Some
             ( ( (community_id, stored_community_slug),
                 (community_name, visibility_raw) ),
               (onboarding_raw, (is_network_community, indexable, discoverable))
             )) -> (
          (* Closed-value validation only — valid lifecycle drift is not
             corruption at this step, and removal never requires current
             host eligibility. *)
          match
            ( Community_types.community_visibility_of_string visibility_raw,
              Community_types.community_onboarding_state_of_string
                onboarding_raw )
          with
          | None, _ | _, Error _ -> rollback_to Inconsistent_data
          | Some visibility, Ok onboarding_state ->
              if
                not
                  (community_id > 0
                  && String.equal stored_community_slug community_slug
                  && nonblank community_name
                  && control_safe community_name
                  && community_structurally_valid ~is_network_community
                       ~onboarding_state ~visibility ~indexable ~discoverable)
              then rollback_to Inconsistent_data
              else
                (* Classified here because the lifecycle columns are only
                   available under the community lock, but consulted only
                   after authorization and the relation lock below. *)
                let setup_draft =
                  unpublished_setup_draft ~is_network_community
                    ~onboarding_state ~visibility ~indexable ~discoverable
                in
                authorize ~project_id ~community_id (fun () ->
                    remove_relation ~project_id ~community_id ~setup_draft))
    in
    (* Step 1: the project row, locked before anything else. *)
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt lock_project_query project_slug >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Project_unavailable
        | Ok (Some (project_id, stored_project_slug, stored_verification)) ->
            if
              not
                (positive project_id
                && String.equal stored_project_slug project_slug
                && known_verification_status stored_verification)
            then rollback_to Inconsistent_data
            else lock_community ~project_id)
