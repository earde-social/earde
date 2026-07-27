(* Atomic creation of one pending home-relation request, all inside one
   explicit transaction in the shared project-first lock order (project
   row, then target community row, then the relation insertion) that the
   future provisioning, review, and removal stores must follow — so
   request creation serializes against any project-level home operation
   on the project row itself. The partial unique active-home index
   arbitrates concurrent active-home races through ON CONFLICT DO NOTHING
   — a read-before-insert check could not. Authorization happens inside
   the SQL (steward link only); the relation value arrives already
   validated by the pure domain and is never reconstructed. See the .mli
   for the full contract. *)

open Lwt.Infix

type created_request = { relation_id : int64 }

let relation_id { relation_id } = relation_id

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_id
  | Invalid_relation
  | Project_unavailable
  | Community_unavailable
  | Active_home_exists
  | Inconsistent_data
  | Storage_error

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and the setup read model requires on routes. Route values must
   already be canonical: nothing here lowercases, trims, or repairs, so an
   aliased spelling can never reach the database. *)
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
   but every community is addressed at /c/:slug, so a canonical stored
   slug is a single non-empty URL path segment — no whitespace, controls,
   DEL, or '/'. A stored slug outside that shape cannot be addressed by
   the later review and rendering workflow and is durable corruption,
   never silently carried forward. *)
let canonical_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* Authorization and the project row lock in one statement: the project is
   never loaded and checked in OCaml, and stewardship is never queried
   afterward. FOR UPDATE OF p locks only the project row — the lock every
   sibling home operation takes first — not the steward row, whose
   (project_id, user_id) primary key also caps this join at one row. *)
let lock_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->? Caqti_type.(t3 int64 string string))
  "SELECT p.id, p.slug, p.verification_status \
   FROM open_source_projects p \
   JOIN project_stewards s ON s.project_id = p.id \
   WHERE p.slug = $1 \
     AND p.verification_status = 'verified' \
     AND s.user_id = $2 \
   FOR UPDATE OF p"

(* The exact target community, locked second under the held project lock.
   Eligibility is entirely in current durable columns: a network community
   (is_network_community), live (onboarding_state = 'published'), and not
   fully private (visibility = 'public' — which covers both published
   shapes, listed and unlisted; unlisted only clears the indexable and
   discoverable flags). Legacy communities and setup drafts fall out of
   the same predicate. The lifecycle flags come back for closed-value
   validation, not filtering. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->? Caqti_type.(
         t2 (t2 (t2 int string) (t2 string string)) (t3 bool bool bool)))
  "SELECT id, slug, visibility, onboarding_state, \
          is_network_community, indexable, discoverable \
   FROM communities \
   WHERE id = $1 \
     AND is_network_community \
     AND onboarding_state = 'published' \
     AND visibility = 'public' \
   FOR UPDATE"

(* The conflict target is the partial unique active-home index: zero
   returned rows means an active (pending or accepted) home already holds
   the slot — whether pre-existing or a concurrently committed winner —
   and the transaction rolls back as a whole. Historical rejected/removed
   rows fall outside the predicate and never conflict. Timestamps ride the
   database defaults; the pending row shape (no reviewer, no review or
   removal time) is written explicitly. *)
let insert_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 int) (t2 int (option string)))
   ->? Caqti_type.int64)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, \
      requested_by_user_id, reviewed_by_user_id, request_note, \
      reviewed_at, removed_at) \
   VALUES ($1, $2, 'home', 'pending', $3, NULL, $4, NULL, NULL) \
   ON CONFLICT (project_id) \
     WHERE relation_type = 'home' AND status IN ('pending', 'accepted') \
   DO NOTHING \
   RETURNING id"

let create (module C : Caqti_lwt.CONNECTION) ~user_id ~project_slug
    ~target_community_id ~relation =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else if target_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else if
    Project_home_relation.status relation <> Project_home_relation.Pending
  then Lwt.return (Error Invalid_relation)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free
       — error payloads can echo SQL parameters — and rollback failure
       adds nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt lock_project_query (project_slug, user_id) >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None ->
            (* Every zero-row cause (missing project, another user's,
               creator without stewardship, stale, revoked) collapses into
               one error so the store cannot probe projects. *)
            rollback_to Project_unavailable
        | Ok (Some (project_row_id, stored_slug, stored_verification)) ->
            if
              not
                (positive project_row_id
                && String.equal stored_slug project_slug
                && String.equal stored_verification "verified")
            then rollback_to Inconsistent_data
            else (
              C.find_opt lock_community_query target_community_id
              >>= function
              | Error _ -> rollback_to Storage_error
              | Ok None ->
                  (* Missing, legacy, setup draft, unpublished, fully
                     private, and every other ineligible state collapse:
                     communities are not probeable through this store
                     either. *)
                  rollback_to Community_unavailable
              | Ok
                  (Some
                    ( ( (community_row_id, community_slug),
                        (visibility_raw, onboarding_raw) ),
                      (is_network_community, indexable, discoverable) )) ->
                  (* Durable data the later workflow relies on, validated
                     through the closed variants and the shared lifecycle
                     rule (a published network community must be exactly
                     fully listed or fully unlisted — mixed flag shapes
                     are corruption, not a third mode). *)
                  let durable_ok =
                    community_row_id = target_community_id
                    && community_row_id > 0
                    && canonical_community_slug community_slug
                    && is_network_community
                    &&
                    match
                      ( Db.community_visibility_of_string visibility_raw,
                        Db.community_onboarding_state_of_string
                          onboarding_raw )
                    with
                    | Some visibility, Ok onboarding_state ->
                        Network_communities.lifecycle_state_valid
                          ~is_network_community ~onboarding_state
                          ~visibility ~indexable ~discoverable
                    | None, _ | _, Error _ -> false
                  in
                  if not durable_ok then rollback_to Inconsistent_data
                  else (
                    C.find_opt insert_relation_query
                      ( (project_row_id, community_row_id),
                        ( user_id,
                          Project_home_relation.request_note relation ) )
                    >>= function
                    | Error _ -> rollback_to Storage_error
                    | Ok None -> rollback_to Active_home_exists
                    | Ok (Some new_relation_id) ->
                        if not (positive new_relation_id) then
                          rollback_to Inconsistent_data
                        else (
                          (* The audit event rides the same transaction:
                             inserted only once the pending relation is
                             validated, and any audit failure rolls the
                             whole request back — a committed request
                             without its event cannot exist. *)
                          Project_home_audit.insert
                            (module C)
                            ~action:Project_home_audit.Home_requested
                            ~actor_user_id:user_id
                            ~project_id:project_row_id
                            ~community_id:community_row_id
                            ~relation_id:new_relation_id
                          >>= function
                          | Error Project_home_audit.Inconsistent_data ->
                              rollback_to Inconsistent_data
                          | Error Project_home_audit.Storage_error ->
                              rollback_to Storage_error
                          | Ok () -> (
                              (* Durable notifications ride the same
                                 transaction: the target community's
                                 current top moderators, resolved under
                                 the held locks, minus the requesting
                                 actor. A notification failure rolls
                                 the pending relation and its audit
                                 event back whole — a committed request
                                 without its notification set cannot
                                 exist, and zero other top moderators
                                 is a legitimate empty set. *)
                              Project_home_notifications
                              .community_top_moderator_ids
                                (module C)
                                ~community_id:community_row_id
                              >>= function
                              | Error
                                  Project_home_notifications
                                  .Inconsistent_data ->
                                  rollback_to Inconsistent_data
                              | Error
                                  Project_home_notifications.Storage_error
                                ->
                                  rollback_to Storage_error
                              | Ok top_moderator_ids -> (
                                  Project_home_notifications.insert_many
                                    (module C)
                                    ~kind:
                                      Project_home_notifications
                                      .Home_requested
                                    ~actor_user_id:user_id
                                    ~project_id:project_row_id
                                    ~community_id:community_row_id
                                    ~relation_id:new_relation_id
                                    ~recipient_user_ids:top_moderator_ids
                                  >>= function
                                  | Error
                                      Project_home_notifications
                                      .Inconsistent_data ->
                                      rollback_to Inconsistent_data
                                  | Error
                                      Project_home_notifications
                                      .Storage_error ->
                                      rollback_to Storage_error
                                  | Ok () -> (
                                      C.commit () >>= function
                                      | Error _ ->
                                          Lwt.return (Error Storage_error)
                                      | Ok () ->
                                          Lwt.return
                                            (Ok
                                               {
                                                 relation_id =
                                                   new_relation_id;
                                               }))))))))
