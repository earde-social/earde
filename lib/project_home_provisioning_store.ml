(* Atomic provisioning of one dedicated community home, all inside one
   explicit transaction in the shared project-first lock order (project
   row, then the current steward authorization row, then the active
   relation rows) that the request, review, and removal stores follow —
   so provisioning serializes with every project-level home operation on
   the project row itself. The new community cannot be locked before it
   exists; once inserted the transaction owns the row. Cooperative locks
   are never the final arbiter: communities_slug_key arbitrates the
   requested slug and the partial unique active-home index arbitrates the
   accepted relation, both through ON CONFLICT DO NOTHING — a
   read-before-insert check could not. Authorization happens inside the
   SQL (verified project plus current steward row only); the identity
   arrives already parsed by the pure form and is persisted byte-exactly,
   never repaired or re-derived. See the .mli for the full contract. *)

open Lwt.Infix

type provisioned_home = {
  slug : string;
  status : Project_home_relation.status;
}

let community_slug { slug; _ } = slug
let resulting_status { status; _ } = status

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Project_unavailable
  | Community_slug_unavailable
  | Active_home_exists
  | Inconsistent_data
  | Storage_error

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and every sibling home store requires on routes. Route values
   must already be canonical: nothing here lowercases, trims, or repairs,
   so an aliased spelling can never reach the database. *)
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

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* Step 1a: the exact permanent project, locked first — the lock every
   home operation takes before anything else, making the project row the
   global serialization point for provisioning, request, review, and
   removal alike. Verification is filtered in SQL and revalidated on the
   returned row. *)
let lock_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t3 int64 string string))
  "SELECT p.id, p.slug, p.verification_status \
   FROM open_source_projects p \
   WHERE p.slug = $1 AND p.verification_status = 'verified' \
   FOR UPDATE OF p"

(* Step 1b: the actor's current steward row, with fresh GitHub evidence
   (stewardship authorizes new homes only while it does), locked second under the held
   project lock so a concurrent stewardship revocation either lands before
   this lock (zero rows) or waits for the commit. The (project_id,
   user_id) primary key caps this at one row; role is filtered exactly and
   returned for structural validation. Nothing else authorizes — not
   created_by_user_id, not durable or session admin, not community
   roles. *)
let lock_steward_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.string)
  "SELECT role FROM project_stewards \
   WHERE project_id = $1 AND user_id = $2 AND role = 'steward' \
     AND github_evidence_is_fresh(github_verified_at) \
   FOR UPDATE"

(* Step 2: every active home relation for the locked project, under a
   locking read so a concurrently created row from a path that already
   committed is seen and a still-uncommitted one serializes on the project
   lock instead. Historical rejected/removed rows fall outside the
   predicate and never block. *)
let lock_active_relations_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64
   ->* Caqti_type.(t2 (t2 int64 int) (t2 string string)))
  "SELECT id, community_id, relation_type, status \
   FROM community_projects \
   WHERE project_id = $1 \
     AND relation_type = 'home' \
     AND status IN ('pending', 'accepted') \
   FOR UPDATE"

(* Step 3: exactly one new private draft network community with the parsed
   canonical identity. Lifecycle values are written explicitly — private,
   network, draft, neither indexable nor discoverable — and
   sections_enabled rides the post-pivot invariant (every community is a
   structured shell). Every other column keeps its production default.
   communities_slug_key is the final slug arbiter: zero returned rows
   means the slug is taken (pre-existing or a concurrent winner) and the
   transaction rolls back as a whole. *)
let insert_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 string string (option string))
   ->? Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string (option string)))
           (t2 (t2 string string) (t3 bool bool bool))))
  "INSERT INTO communities \
     (name, slug, description, sections_enabled, visibility, indexable, \
      is_network_community, onboarding_state, discoverable) \
   VALUES ($1, $2, $3, TRUE, 'private', FALSE, TRUE, 'draft', FALSE) \
   ON CONFLICT (slug) DO NOTHING \
   RETURNING id, name, slug, description, visibility, onboarding_state, \
             is_network_community, indexable, discoverable"

(* Step 4: the actor's initial membership, in the exact durable shape
   Membership_store.join_community writes — the two-column row is the whole schema. The
   community is brand new inside this transaction, so no conflict is
   possible and RETURNING proves exactly one row. Membership is required
   in its own right: private-community authorization reads members,
   moderators, and admins independently. *)
let insert_member_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.bool)
  "INSERT INTO community_members (user_id, community_id) \
   VALUES ($1, $2) RETURNING TRUE"

(* Step 5: the actor as initial top moderator, in the exact durable shape
   Moderator_store.add_top_moderator writes (explicit 'top_mod' — the column default is
   'mod'); promoted_at keeps its production default. *)
let insert_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
  "INSERT INTO community_moderators (user_id, community_id, role) \
   VALUES ($1, $2, 'top_mod') RETURNING role"

(* Step 6: the minimum community shell, byte-identical to what legacy
   creation writes and the default-structure migration backfilled — one
   General forum section and one general live channel; is_archived,
   indexable, and created_at keep their production defaults. Nothing
   speculative is created; the private setup workflow may edit later. *)
let insert_section_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "INSERT INTO community_sections \
     (community_id, name, slug, description, position, default_sort, \
      is_introduction_section) \
   VALUES ($1, 'General', 'general', 'General discussion', 0, 'new', FALSE) \
   RETURNING id"

let insert_channel_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "INSERT INTO channels (community_id, slug, name, topic, position) \
   VALUES ($1, 'general', 'general', 'General chat', 0) \
   RETURNING id"

(* Step 7: the accepted home relation, inserted once in its final shape —
   never a pending row updated afterwards. No review provenance is
   fabricated: requester, reviewer, and note are NULL; reviewed_at is the
   transaction's NOW(), which equals the created_at/updated_at defaults of
   a fresh row, so every ordering CHECK holds by construction. The
   conflict target is the partial unique active-home index — the only
   constraint this statement can conflict on (the id is sequence-fresh and
   both foreign keys were just locked/created) — so zero returned rows
   provably means an active home won through a path outside the
   cooperative project lock. The returned row is revalidated in full. *)
let insert_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int64 int string)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 int64) (t2 int string))
           (t2 (t2 string bool) (t2 bool bool))))
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, \
      requested_by_user_id, reviewed_by_user_id, request_note, \
      reviewed_at, removed_at) \
   VALUES ($1, $2, 'home', $3, NULL, NULL, NULL, NOW(), NULL) \
   ON CONFLICT (project_id) \
     WHERE relation_type = 'home' AND status IN ('pending', 'accepted') \
   DO NOTHING \
   RETURNING id, project_id, community_id, relation_type, status, \
             requested_by_user_id IS NULL AND reviewed_by_user_id IS NULL \
               AND request_note IS NULL, \
             reviewed_at IS NOT NULL AND removed_at IS NULL, \
             reviewed_at >= created_at AND updated_at >= created_at"

(* Final complete-state validation: ten bounded aggregate reads on the
   same connection prove the transaction contains exactly the coherent
   draft — one membership (the actor's), one moderator row (the actor's
   top_mod), exactly the two shell rows in their canonical shape, exactly
   one active home relation for the project, and no pending relation. The
   community row itself was validated on its RETURNING clause and
   communities_slug_key guarantees its uniqueness. *)
let validate_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int int int64)
   ->! Caqti_type.(
         t2 (t4 int int int int) (t2 (t4 int int int int) (t2 int int))))
  "SELECT \
     (SELECT COUNT(*) FROM community_members WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_members \
      WHERE community_id = $1 AND user_id = $2), \
     (SELECT COUNT(*) FROM community_moderators WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_moderators \
      WHERE community_id = $1 AND user_id = $2 AND role = 'top_mod'), \
     (SELECT COUNT(*) FROM community_sections WHERE community_id = $1), \
     (SELECT COUNT(*) FROM community_sections \
      WHERE community_id = $1 AND slug = 'general' AND name = 'General' \
        AND position = 0 AND default_sort = 'new' \
        AND NOT is_introduction_section), \
     (SELECT COUNT(*) FROM channels WHERE community_id = $1), \
     (SELECT COUNT(*) FROM channels \
      WHERE community_id = $1 AND slug = 'general' AND name = 'general' \
        AND position = 0 AND NOT is_archived), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE project_id = $3 AND relation_type = 'home' \
        AND status IN ('pending', 'accepted')), \
     (SELECT COUNT(*) FROM community_projects \
      WHERE project_id = $3 AND relation_type = 'home' \
        AND status = 'pending')"

let provision (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~project_slug
    ~identity =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else
    (* The pure domain owns the resulting status. Its dedicated
       provisioned-home constructor exists precisely because no pending
       request or moderator review took place; anything but Accepted from
       it would be an impossible pure-domain outcome. *)
    let relation = Project_home_relation.create_provisioned_home () in
    let resulting = Project_home_relation.status relation in
    if resulting <> Project_home_relation.Accepted then
      Lwt.return (Error Inconsistent_data)
    else
      let name = Project_home_provisioning_form.community_name identity in
      let requested_slug =
        Project_home_provisioning_form.community_slug identity
      in
      let description =
        Project_home_provisioning_form.community_description identity
      in
      let status_string = Project_home_relation.string_of_status resulting in
      (* As in the sibling stores, every Caqti error is dropped
         payload-free — error payloads can echo SQL parameters — and
         rollback failure adds nothing a caller may act on either. *)
      let rollback_to err =
        C.rollback () >>= fun _ -> Lwt.return (Error err)
      in

      (* The returned community must be byte-exactly the parsed identity
         in exactly the private-draft network lifecycle — anything else is
         corruption, never carried forward. *)
      let community_ok
          ( ((community_row_id, stored_name), (stored_slug, stored_description)),
            ((visibility_raw, onboarding_raw), (network, indexable, discoverable))
          ) =
        community_row_id > 0
        && String.equal stored_name name
        && String.equal stored_slug requested_slug
        && Option.equal String.equal stored_description description
        && network
        && (not indexable)
        && (not discoverable)
        &&
        match
          ( Community_types.community_visibility_of_string visibility_raw,
            Community_types.community_onboarding_state_of_string onboarding_raw )
        with
        | Some Community_types.Community_private, Ok Community_types.Community_draft ->
            Network_communities.lifecycle_state_valid
              ~is_network_community:network
              ~onboarding_state:Community_types.Community_draft
              ~visibility:Community_types.Community_private ~indexable ~discoverable
        | _, _ -> false
      in

      let relation_ok project_row_id community_row_id
          ( ((relation_id, stored_project), (stored_community, relation_type)),
            ((stored_status, provenance_null), (lifecycle_ok, times_ok)) ) =
        positive relation_id
        && Int64.equal stored_project project_row_id
        && stored_community = community_row_id
        && String.equal relation_type "home"
        && String.equal stored_status status_string
        && Project_home_relation.status_of_string stored_status
           = Some Project_home_relation.Accepted
        && provenance_null && lifecycle_ok && times_ok
      in

      let commit_validated project_row_id community_row_id relation_row_id =
        C.find validate_state_query
          (community_row_id, actor_user_id, project_row_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok counts ->
            if counts <> ((1, 1, 1, 1), ((1, 1, 1, 1), (1, 0))) then
              rollback_to Inconsistent_data
            else (
              (* The audit event rides the same transaction as the whole
                 draft: inserted only after the complete provisioned state
                 validated, and any audit failure rolls back community,
                 membership, moderator, shell, and relation together. *)
              Project_home_audit.insert
                (module C)
                ~action:Project_home_audit.Dedicated_home_provisioned
                ~actor_user_id ~project_id:project_row_id
                ~community_id:community_row_id ~relation_id:relation_row_id
              >>= function
              | Error Project_home_audit.Inconsistent_data ->
                  rollback_to Inconsistent_data
              | Error Project_home_audit.Storage_error ->
                  rollback_to Storage_error
              | Ok () -> (
                  C.commit () >>= function
                  | Error _ -> Lwt.return (Error Storage_error)
                  | Ok () ->
                      Lwt.return
                        (Ok { slug = requested_slug; status = resulting })))
      in

      let insert_relation project_row_id community_row_id =
        C.find_opt insert_relation_query
          (project_row_id, community_row_id, status_string)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Active_home_exists
        | Ok (Some returned) ->
            if not (relation_ok project_row_id community_row_id returned)
            then rollback_to Inconsistent_data
            else
              let ((relation_row_id, _), _), _ = returned in
              commit_validated project_row_id community_row_id
                relation_row_id
      in

      (* Steps 4–6 share one shape: a fresh-row insert whose RETURNING
         value is checked, where a vanished row (impossible without an
         interfering rule or trigger) is corruption rather than success. *)
      let insert_shell project_row_id community_row_id =
        C.find_opt insert_member_query (actor_user_id, community_row_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None | Ok (Some false) -> rollback_to Inconsistent_data
        | Ok (Some true) -> (
            C.find_opt insert_moderator_query (actor_user_id, community_row_id)
            >>= function
            | Error _ -> rollback_to Storage_error
            | Ok None -> rollback_to Inconsistent_data
            | Ok (Some role) ->
                if not (String.equal role "top_mod") then
                  rollback_to Inconsistent_data
                else (
                  C.find_opt insert_section_query community_row_id
                  >>= function
                  | Error _ -> rollback_to Storage_error
                  | Ok None -> rollback_to Inconsistent_data
                  | Ok (Some section_id) ->
                      if section_id <= 0 then rollback_to Inconsistent_data
                      else (
                        C.find_opt insert_channel_query community_row_id
                        >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok None -> rollback_to Inconsistent_data
                        | Ok (Some channel_id) ->
                            if channel_id <= 0 then
                              rollback_to Inconsistent_data
                            else insert_relation project_row_id
                                   community_row_id)))
      in

      let insert_community project_row_id =
        C.find_opt insert_community_query (name, requested_slug, description)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Community_slug_unavailable
        | Ok (Some returned) ->
            if not (community_ok returned) then rollback_to Inconsistent_data
            else
              let ((community_row_id, _), _), _ = returned in
              insert_shell project_row_id community_row_id
      in

      let arbitrate_active_home project_row_id =
        C.collect_list lock_active_relations_query project_row_id
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok [] -> insert_community project_row_id
        | Ok [ ((relation_id, community_id), (relation_type, status_raw)) ]
          ->
            (* One active row: validated defensively before it blocks —
               a malformed "active" row is corruption, not a home. *)
            let valid =
              positive relation_id && community_id > 0
              && String.equal relation_type "home"
              &&
              match Project_home_relation.status_of_string status_raw with
              | Some Project_home_relation.Pending
              | Some Project_home_relation.Accepted ->
                  true
              | Some _ | None -> false
            in
            if valid then rollback_to Active_home_exists
            else rollback_to Inconsistent_data
        | Ok (_ :: _ :: _) ->
            (* The partial unique index admits at most one active home;
               two is durable corruption. *)
            rollback_to Inconsistent_data
      in

      C.start () >>= function
      | Error _ -> Lwt.return (Error Storage_error)
      | Ok () -> (
          C.find_opt lock_project_query project_slug >>= function
          | Error _ -> rollback_to Storage_error
          | Ok None ->
              (* Missing, stale, and revoked all collapse before the
                 steward check even runs, so the store cannot probe
                 projects. *)
              rollback_to Project_unavailable
          | Ok (Some (project_row_id, stored_slug, stored_verification)) ->
              if
                not
                  (positive project_row_id
                  && String.equal stored_slug project_slug
                  && String.equal stored_verification "verified")
              then rollback_to Inconsistent_data
              else (
                C.find_opt lock_steward_query (project_row_id, actor_user_id)
                >>= function
                | Error _ -> rollback_to Storage_error
                | Ok None ->
                    (* Foreign project, creator without stewardship,
                       removed stewardship, durable admin without a
                       steward row — one indistinguishable error. *)
                    rollback_to Project_unavailable
                | Ok (Some role) ->
                    if not (String.equal role "steward") then
                      rollback_to Inconsistent_data
                    else arbitrate_active_home project_row_id))
