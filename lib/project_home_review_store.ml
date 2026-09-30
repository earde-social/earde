(* Atomic moderator review of one exact pending home request, all inside
   one explicit transaction in the shared project-first lock order
   (project row, then target community row, then the reviewer's durable
   authorization row, then the exact pending relation row) that the
   sibling request store established. Authorization is community-side and
   lives inside the SQL: the current top moderator role of the target
   community, or the durable global-administrator flag on the users row —
   never a handler boolean, project stewardship, or creator/GitHub
   provenance. The lifecycle transition itself comes only from the pure
   Project_home_relation domain over the reconstructed pending value. See
   the .mli for the full contract. *)

open Lwt.Infix

type decision =
  | Accept
  | Reject

type reviewed_request = { resulting_status : Project_home_relation.status }

let resulting_status { resulting_status } = resulting_status

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Invalid_community_slug
  | Project_unavailable
  | Community_unavailable
  | Reviewer_unauthorized
  | Review_unavailable
  | Target_ineligible
  | Inconsistent_data
  | Storage_error

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and the request store requires. Route values must already be
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
   so the strongest canonical predicate the repository has is the request
   store's addressability shape: every community lives at /c/:slug, hence
   one non-empty URL path segment — no whitespace, controls, DEL, or '/'.
   Nothing stricter is imposed on legacy slugs, and nothing is repaired. *)
let canonical_community_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* === queries === *)

(* The project row is locked first — the lock every sibling home operation
   takes first — by exact supplied slug. Review is community-authorized,
   so no steward join appears anywhere in this store. Unlike the request
   store there is no verified filter here: a pending request may
   legitimately outlive verification, and moderators must still be able
   to reject it — acceptance re-checks current verification separately
   (the effective status: 'verified' only while some steward's GitHub
   evidence is fresh),
   so a stale or revoked project can never strand its pending row in the
   active-home slot. *)
let lock_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t3 int64 string string))
  "SELECT p.id, p.slug, \
          project_github_verification(p.id, p.verification_status) \
   FROM open_source_projects p \
   WHERE p.slug = $1 \
   FOR UPDATE OF p"

(* The exact closed schema vocabulary (mirrors the open_source_projects
   verification CHECK); anything else in the durable column is
   corruption, never a fourth state. *)
let known_verification_status = function
  | "verified" | "stale" | "revoked" -> true
  | _ -> false

(* The exact target community, locked second under the held project lock.
   Unlike the request store there is no eligibility filter here: the
   request may legitimately predate a lifecycle change, and moderators
   must still be able to reject it — acceptance re-checks current
   eligibility separately. The lifecycle columns come back for closed
   validation and the eligibility decision. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.string
   ->? Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string string))
           (t2 string (t3 bool bool bool))))
  "SELECT id, slug, name, visibility, onboarding_state, \
          is_network_community, indexable, discoverable \
   FROM communities \
   WHERE slug = $1 \
   FOR UPDATE"

(* Reviewer authorization, third, both checked and locked inside SQL so a
   concurrent role removal or downgrade serializes through the row lock
   (the predicate is re-evaluated after a blocking writer commits): first
   the current top moderator role of the locked community... *)
let lock_top_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
  "SELECT role FROM community_moderators \
   WHERE user_id = $1 AND community_id = $2 AND role = 'top_mod' \
   FOR UPDATE"

(* ...then, only when no top moderator row exists, the durable global
   administrator flag on the users row — the only durable representation
   of Earde administrators. A session-only claim never reaches this
   store. *)
let lock_admin_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.bool)
  "SELECT is_admin FROM users \
   WHERE id = $1 AND is_admin \
   FOR UPDATE"

(* The exact pending relation, locked last. The partial unique active-home
   index caps this at one row; every zero-row cause (no relation, another
   community's request, already reviewed, removed, concurrent winner)
   collapses into one variant at the call site. *)
let lock_pending_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 string) (t2 string (option string)))
           (t2 (t2 (option int) (option int)) (t2 bool bool))))
  "SELECT id, relation_type, status, request_note, \
          requested_by_user_id, reviewed_by_user_id, \
          reviewed_at IS NULL, removed_at IS NULL \
   FROM community_projects \
   WHERE project_id = $1 AND community_id = $2 \
     AND relation_type = 'home' AND status = 'pending' \
   FOR UPDATE"

(* The one mutation: the locked row, guarded again on pending so a zero
   count is a completed concurrent review, never a second write. The
   status vocabulary comes exclusively from the pure domain via
   string_of_status at the call site. GREATEST clamps against the
   NOW()-before-created_at race the schema CHECKs would otherwise trip
   on same-transaction-batch fixtures. removed_at is written NULL
   explicitly to keep the reviewed row shape closed. *)
let update_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int64 string int) ->* Caqti_type.(t2 int64 string))
  "UPDATE community_projects \
   SET status = $2, \
       reviewed_by_user_id = $3, \
       reviewed_at = GREATEST(NOW(), created_at), \
       removed_at = NULL, \
       updated_at = GREATEST(NOW(), created_at) \
   WHERE id = $1 AND status = 'pending' \
   RETURNING id, status"

(* Current-eligibility predicate for acceptance — exactly the established
   host-eligibility rule the request store filters on: a published,
   fully public network community. The two legal publication flag shapes
   are already guaranteed by structural validation. *)
let currently_eligible ~is_network_community ~onboarding_state ~visibility =
  is_network_community
  && onboarding_state = Community_types.Community_published
  && visibility = Community_types.Community_public

(* Structural validity of the locked community. The shared lifecycle rule
   decides most shapes; the one drifted shape it rejects but review must
   still handle — a published network community that has since gone fully
   private (visibility reverted, nothing leaking through indexing or
   discovery) — is valid-but-ineligible here, because pending requests
   may legitimately outlive eligibility and moderators must still be able
   to reject them. Leaking shapes (an indexable draft, a discoverable
   private community, mixed published flags) remain corruption. *)
let community_structurally_valid ~is_network_community ~onboarding_state
    ~visibility ~indexable ~discoverable =
  Network_communities.lifecycle_state_valid ~is_network_community
    ~onboarding_state ~visibility ~indexable ~discoverable
  || is_network_community
     && onboarding_state = Community_types.Community_published
     && visibility = Community_types.Community_private
     && (not indexable) && not discoverable

let positive id = Int64.compare id 0L > 0

let review (module C : Caqti_lwt.CONNECTION) ~reviewer_user_id ~project_slug
    ~target_community_slug ~decision =
  if reviewer_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else if not (canonical_community_slug target_community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free
       — error payloads can echo SQL parameters — and rollback failure
       adds nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
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
            else
              (* Whether acceptance is currently permitted; decided only
                 after reviewer authorization and the pending-row lock, so
                 the collapsed error cannot be used to probe verification
                 state without both authority and a real request. *)
              let project_verified =
                String.equal stored_verification "verified"
              in
              (
              C.find_opt lock_community_query target_community_slug
              >>= function
              | Error _ -> rollback_to Storage_error
              | Ok None -> rollback_to Community_unavailable
              | Ok
                  (Some
                    ( ( (community_id, stored_community_slug),
                        (community_name, visibility_raw) ),
                      ( onboarding_raw,
                        (is_network_community, indexable, discoverable) ) ))
                -> (
                  (* Closed-value validation only — a valid ineligible
                     lifecycle is not corruption at this step. *)
                  match
                    ( Community_types.community_visibility_of_string visibility_raw,
                      Community_types.community_onboarding_state_of_string onboarding_raw
                    )
                  with
                  | None, _ | _, Error _ -> rollback_to Inconsistent_data
                  | Some visibility, Ok onboarding_state ->
                      if
                        not
                          (community_id > 0
                          && String.equal stored_community_slug
                               target_community_slug
                          && String.length community_name > 0
                          && community_structurally_valid
                               ~is_network_community ~onboarding_state
                               ~visibility ~indexable ~discoverable)
                      then rollback_to Inconsistent_data
                      else
                        (* Reviewer authorization: top moderator row
                           first; the durable admin flag only as the
                           fallback, so exactly the qualifying row is
                           locked. Every unauthorized cause collapses. *)
                        let authorize k =
                          C.find_opt lock_top_moderator_query
                            (reviewer_user_id, community_id)
                          >>= function
                          | Error _ -> rollback_to Storage_error
                          | Ok (Some role) ->
                              if String.equal role "top_mod" then k ()
                              else rollback_to Inconsistent_data
                          | Ok None -> (
                              C.find_opt lock_admin_query reviewer_user_id
                              >>= function
                              | Error _ -> rollback_to Storage_error
                              | Ok None -> rollback_to Reviewer_unauthorized
                              | Ok (Some is_admin) ->
                                  if is_admin then k ()
                                  else rollback_to Inconsistent_data)
                        in
                        authorize (fun () ->
                            C.find_opt lock_pending_relation_query
                              (project_id, community_id)
                            >>= function
                            | Error _ -> rollback_to Storage_error
                            | Ok None -> rollback_to Review_unavailable
                            | Ok
                                (Some
                                  ( ( (relation_id, relation_type),
                                      (status_raw, stored_note) ),
                                    ( (requested_by, reviewed_by),
                                      (reviewed_at_null, removed_at_null) )
                                  )) -> (
                                (* The locked row must be a well-formed
                                   pending request, and its note must
                                   reconstruct byte-exactly through the
                                   pure constructor — the reviewed value
                                   is the domain transition of that
                                   reconstruction, never ad hoc. *)
                                let row_shape_ok =
                                  positive relation_id
                                  && String.equal relation_type "home"
                                  && String.equal status_raw "pending"
                                  && (match requested_by with
                                     | None -> true
                                     | Some id -> id > 0)
                                  && reviewed_by = None && reviewed_at_null
                                  && removed_at_null
                                in
                                let reconstructed =
                                  Project_home_relation.create_pending
                                    ~request_note:stored_note
                                in
                                match (row_shape_ok, reconstructed) with
                                | false, _ | _, Error _ ->
                                    rollback_to Inconsistent_data
                                | true, Ok pending_relation ->
                                    if
                                      Project_home_relation.request_note
                                        pending_relation
                                      <> stored_note
                                    then rollback_to Inconsistent_data
                                    else if
                                      decision = Accept
                                      && not project_verified
                                    then
                                      (* Stale and revoked collapse into
                                         the same variant as a missing
                                         project: acceptance requires
                                         current verification, while
                                         rejection below stays available
                                         so the pending row can always be
                                         closed and the active-home slot
                                         freed. *)
                                      rollback_to Project_unavailable
                                    else if
                                      decision = Accept
                                      && not
                                           (currently_eligible
                                              ~is_network_community
                                              ~onboarding_state ~visibility)
                                    then
                                      (* Rejection stays available on a
                                         valid ineligible target; only
                                         acceptance requires current
                                         host eligibility. *)
                                      rollback_to Target_ineligible
                                    else
                                      let action =
                                        match decision with
                                        | Accept -> Project_home_relation.Accept
                                        | Reject -> Project_home_relation.Reject
                                      in
                                      (match
                                         Project_home_relation.apply
                                           pending_relation action
                                       with
                                      | Error _ ->
                                          (* The row was locked pending;
                                             a refused Pending+Accept/
                                             Reject transition can only
                                             mean domain/durable drift. *)
                                          rollback_to Inconsistent_data
                                      | Ok transitioned ->
                                          let new_status =
                                            Project_home_relation.status
                                              transitioned
                                          in
                                          let status_string =
                                            Project_home_relation
                                            .string_of_status new_status
                                          in
                                          C.collect_list
                                            update_relation_query
                                            ( relation_id,
                                              status_string,
                                              reviewer_user_id )
                                          >>= function
                                          | Error _ ->
                                              (* Includes any unexpected
                                                 constraint failure — the
                                                 pending row already holds
                                                 the active-home slot, so
                                                 a unique conflict here is
                                                 impossible under the
                                                 locked protocol. *)
                                              rollback_to Storage_error
                                          | Ok [] ->
                                              rollback_to Review_unavailable
                                          | Ok [ (updated_id, updated_status) ]
                                            ->
                                              if
                                                not
                                                  (Int64.equal updated_id
                                                     relation_id
                                                  && String.equal
                                                       updated_status
                                                       status_string)
                                              then
                                                rollback_to Inconsistent_data
                                              else
                                                (* The audit event rides
                                                   the same transaction:
                                                   inserted only after the
                                                   guarded transition
                                                   validated, and any
                                                   audit failure rolls the
                                                   whole review back. The
                                                   private request note
                                                   never reaches it. *)
                                                let audit_action =
                                                  match decision with
                                                  | Accept ->
                                                      Project_home_audit
                                                      .Home_accepted
                                                  | Reject ->
                                                      Project_home_audit
                                                      .Home_rejected
                                                in
                                                let notification_kind =
                                                  match decision with
                                                  | Accept ->
                                                      Project_home_notifications
                                                      .Home_accepted
                                                  | Reject ->
                                                      Project_home_notifications
                                                      .Home_rejected
                                                in
                                                (* The recipient is the
                                                   locked row's durable
                                                   requester — never a
                                                   browser value. A
                                                   NULL (deleted)
                                                   requester or a
                                                   self-review leaves
                                                   the set empty, and
                                                   the decision still
                                                   commits. *)
                                                let decision_recipients =
                                                  match requested_by with
                                                  | Some id
                                                    when id
                                                         <> reviewer_user_id
                                                    ->
                                                      [ id ]
                                                  | Some _ | None -> []
                                                in
                                                (Project_home_audit.insert
                                                   (module C)
                                                   ~action:audit_action
                                                   ~actor_user_id:
                                                     reviewer_user_id
                                                   ~project_id ~community_id
                                                   ~relation_id
                                                 >>= function
                                                 | Error
                                                     Project_home_audit
                                                     .Inconsistent_data ->
                                                     rollback_to
                                                       Inconsistent_data
                                                 | Error
                                                     Project_home_audit
                                                     .Storage_error ->
                                                     rollback_to
                                                       Storage_error
                                                 | Ok () -> (
                                                     (* The durable
                                                        notification
                                                        rides the same
                                                        transaction: a
                                                        failure rolls
                                                        the decision
                                                        and its audit
                                                        event back
                                                        whole. The
                                                        private request
                                                        note never
                                                        reaches it. *)
                                                     Project_home_notifications
                                                     .insert_many
                                                       (module C)
                                                       ~kind:
                                                         notification_kind
                                                       ~actor_user_id:
                                                         reviewer_user_id
                                                       ~project_id
                                                       ~community_id
                                                       ~relation_id
                                                       ~recipient_user_ids:
                                                         decision_recipients
                                                     >>= function
                                                     | Error
                                                         Project_home_notifications
                                                         .Inconsistent_data
                                                       ->
                                                         rollback_to
                                                           Inconsistent_data
                                                     | Error
                                                         Project_home_notifications
                                                         .Storage_error ->
                                                         rollback_to
                                                           Storage_error
                                                     | Ok () -> (
                                                         C.commit ()
                                                         >>= function
                                                         | Error _ ->
                                                             Lwt.return
                                                               (Error
                                                                  Storage_error)
                                                         | Ok () ->
                                                             Lwt.return
                                                               (Ok
                                                                  {
                                                                    resulting_status =
                                                                      new_status;
                                                                  }))))
                                          | Ok (_ :: _ :: _) ->
                                              rollback_to Inconsistent_data)
                                )))))
