(* Atomic writes for the community↔community mutual connection lifecycle.
   Each function is one explicit transaction taking the single shared lock
   order — both communities in ascending id order FOR SHARE, then the exact
   connection row FOR UPDATE — so no two functions can deadlock against each
   other: FOR SHARE locks are mutually compatible, and only one row is ever
   taken FOR UPDATE.

   review and remove do not know the pair until they have seen the row, so
   they discover it with an unlocked read first and only then take the locks
   in that one order. That read decides lock order, never race safety: the
   guarded UPDATE re-verifies status and subject under the row lock, and a
   pair that changed between the two reads simply fails the guard.

   Race safety is the partial unique active-pair index (through ON CONFLICT
   DO NOTHING) and the status-guarded UPDATE — never a read-before-write
   check. Every lifecycle transition comes from the pure
   Community_connections domain over the value reconstructed from the locked
   row; the status vocabulary reaching SQL is always string_of_status.

   Authorization is deliberately absent: the acting user is provenance and
   the acting community is the subject boundary the SQL verifies. See the
   .mli for the full contract. *)

open Lwt.Infix

module Cc = Community_connections
module Notifications = Community_connection_notifications

type decision =
  | Accept
  | Reject

type created_connection = { created_id : int64 }

let created_connection_id { created_id } = created_id

type reviewed_connection = {
  reviewed_id : int64;
  reviewed_requester : int;
  reviewed_recipient : int;
  reviewed_result : Cc.status;
}

let reviewed_connection_id { reviewed_id; _ } = reviewed_id

let reviewed_requester_community_id { reviewed_requester; _ } =
  reviewed_requester

let reviewed_recipient_community_id { reviewed_recipient; _ } =
  reviewed_recipient

let reviewed_status { reviewed_result; _ } = reviewed_result

type removed_connection = {
  removed_id : int64;
  removed_requester : int;
  removed_recipient : int;
  removed_result : Cc.status;
}

let removed_connection_id { removed_id; _ } = removed_id

let removed_requester_community_id { removed_requester; _ } = removed_requester

let removed_recipient_community_id { removed_recipient; _ } = removed_recipient

let removed_status { removed_result; _ } = removed_result

type error =
  | Invalid_user_id
  | Invalid_connection_id
  | Invalid_community_id
  | Invalid_connection
  | Community_unavailable
  | Requester_ineligible
  | Recipient_ineligible
  | Active_connection_exists
  | Review_unavailable
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* Existence, deletion protection, and the current eligibility facts for one
   referenced community, taken in ascending id order. FOR SHARE — not the
   weaker FOR KEY SHARE — because the decision below reads
   visibility/onboarding_state/discoverable: only FOR SHARE conflicts with
   the FOR NO KEY UPDATE a concurrent visibility or publication change takes,
   so eligibility cannot flip between this read and the commit. FOR SHARE
   locks are mutually compatible, so two connection transactions sharing a
   community never wait on each other and no ordering cycle can form among
   them. The row is fetched with no eligibility predicate: the pure domain
   decides, so a legitimately ineligible community is distinguishable from a
   missing one here and collapses only at the call site. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.(t2 (t2 int string) (t2 string bool)))
  "SELECT id, visibility, onboarding_state, discoverable \
   FROM communities \
   WHERE id = $1 \
   FOR SHARE"

(* The conflict target is the partial unique active-pair index, written
   exactly as the migration declares it so PostgreSQL infers that index and
   no other. Zero returned rows means an active (pending or accepted)
   connection already holds the unordered pair's slot — pre-existing,
   mirrored, or a concurrently committed winner — and the transaction rolls
   back as a whole. Historical rejected/removed rows fall outside the
   predicate and never conflict. Timestamps ride the database defaults; the
   pending row shape (no reviewer, no remover, no review or removal time) is
   written explicitly. *)
let insert_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int int) (t2 int (option string))) ->? Caqti_type.int64)
  "INSERT INTO community_connections \
     (requester_community_id, recipient_community_id, status, \
      requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
      request_note, reviewed_at, removed_at) \
   VALUES ($1, $2, 'pending', $3, NULL, NULL, $4, NULL, NULL) \
   ON CONFLICT (LEAST(requester_community_id, recipient_community_id), \
                GREATEST(requester_community_id, recipient_community_id)) \
     WHERE status IN ('pending', 'accepted') \
   DO NOTHING \
   RETURNING id"

(* The exact pending connection addressed to the exact recipient, locked as
   the only durable row this transaction mutates. Every zero-row cause (no
   connection, another community's request, already reviewed or removed,
   concurrent winner) collapses into one variant at the call site. *)
let lock_pending_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 int (option string)))
           (t2 (t3 (option int) (option int) (option int)) (t2 bool bool))))
  "SELECT id, requester_community_id, recipient_community_id, request_note, \
          requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
          reviewed_at IS NULL, removed_at IS NULL \
   FROM community_connections \
   WHERE id = $1 AND recipient_community_id = $2 AND status = 'pending' \
   FOR UPDATE"

(* The one review mutation: the locked row, guarded again on both the
   pending status and the expected recipient, so a zero count is a completed
   concurrent review — never a second write and never a request addressed
   elsewhere. The status vocabulary comes exclusively from the pure domain
   at the call site. GREATEST clamps against the NOW()-before-created_at
   race the schema CHECKs would otherwise trip on same-transaction-batch
   fixtures. The removal columns are written NULL explicitly to keep the
   reviewed row shape closed. *)
let review_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 string) (t2 int int))
   ->* Caqti_type.(t2 int64 string))
  "UPDATE community_connections \
   SET status = $2, \
       reviewed_by_user_id = $3, \
       reviewed_at = GREATEST(NOW(), created_at), \
       removed_by_user_id = NULL, \
       removed_at = NULL, \
       updated_at = GREATEST(NOW(), created_at) \
   WHERE id = $1 AND status = 'pending' AND recipient_community_id = $4 \
   RETURNING id, status"

(* The exact accepted connection one of whose two communities is the acting
   one — a mutual connection is symmetric, so the boundary is membership in
   the pair, not the recipient side. Locked as the only durable row this
   transaction mutates. *)
let lock_accepted_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 int (option string)))
           (t2 (t3 (option int) (option int) (option int)) (t2 bool bool))))
  "SELECT id, requester_community_id, recipient_community_id, request_note, \
          requested_by_user_id, reviewed_by_user_id, removed_by_user_id, \
          reviewed_at IS NOT NULL, removed_at IS NULL \
   FROM community_connections \
   WHERE id = $1 AND status = 'accepted' \
     AND (requester_community_id = $2 OR recipient_community_id = $2) \
   FOR UPDATE"

(* The one removal mutation, guarded again on the accepted status and on the
   acting community's membership in the pair. reviewed_at is NOT NULL on
   every accepted row, and GREATEST ignores NULLs, so the clamp cannot
   produce a removed_at before either earlier timestamp. reviewed_by_user_id
   is never overwritten: the reviewer stays as historical provenance beside
   the remover. *)
let remove_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 string) (t2 int int))
   ->* Caqti_type.(t2 int64 string))
  "UPDATE community_connections \
   SET status = $2, \
       removed_by_user_id = $3, \
       removed_at = GREATEST(NOW(), created_at, reviewed_at), \
       updated_at = GREATEST(NOW(), created_at, reviewed_at) \
   WHERE id = $1 AND status = 'accepted' \
     AND (requester_community_id = $4 OR recipient_community_id = $4) \
   RETURNING id, status"

(* The exact pair of one row, read WITHOUT a lock, solely so the two
   communities can then be locked in the one global ascending order. Never a
   race-safety check: the guarded UPDATE below re-verifies status and subject
   under the row lock, so a row reviewed or removed between this read and
   that guard simply fails the guard. *)
let discover_pending_pair_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.(t2 int int))
  "SELECT requester_community_id, recipient_community_id \
   FROM community_connections \
   WHERE id = $1 AND recipient_community_id = $2 AND status = 'pending'"

let discover_accepted_pair_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.(t2 int int))
  "SELECT requester_community_id, recipient_community_id \
   FROM community_connections \
   WHERE id = $1 AND status = 'accepted' \
     AND (requester_community_id = $2 OR recipient_community_id = $2)"

(* === shared helpers === *)

(* One locked community: closed-value validation of the durable row, then its
   current connection eligibility from the single pure predicate — never a
   second copy of the three booleans. A legitimately ineligible community is
   not corruption; only an off-enum value or a mismatched id is. *)
let with_locked_community (module C : Caqti_lwt.CONNECTION) ~rollback_to id k =
  C.find_opt lock_community_query id >>= function
  | Error _ -> rollback_to Storage_error
  | Ok None ->
      (* Which side is missing never crosses: the store cannot be used to
         probe community existence. *)
      rollback_to Community_unavailable
  | Ok (Some ((row_id, visibility_raw), (onboarding_raw, discoverable))) -> (
      match
        ( row_id = id && row_id > 0,
          Db.community_visibility_of_string visibility_raw,
          Db.community_onboarding_state_of_string onboarding_raw )
      with
      | true, Some visibility, Ok onboarding_state ->
          k (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
      | _ -> rollback_to Inconsistent_data)

(* Both communities of one pair, locked in ascending id order — the single
   global lock order every mutation here follows, whichever of the two is the
   requester. Each side's eligibility comes back labelled by its role, so no
   caller re-derives which lock answered for which community. *)
let with_locked_pair conn ~rollback_to ~requester ~recipient k =
  let first, second =
    if requester <= recipient then (requester, recipient)
    else (recipient, requester)
  in
  with_locked_community conn ~rollback_to first (fun first_eligible ->
      with_locked_community conn ~rollback_to second (fun second_eligible ->
          let requester_eligible, recipient_eligible =
            if requester = first then (first_eligible, second_eligible)
            else (second_eligible, first_eligible)
          in
          k ~requester_eligible ~recipient_eligible))

(* Recipient resolution and notification insertion for one committed
   transition, on the same open transaction as the mutation and its audit
   event and before the commit — so a notification failure rolls the mutation
   and the event back with it, and a stale or losing transition, which never
   reaches here, creates neither.

   [community_id] is always the notified moderators' own management context:
   the community whose top_mod rows are read, whose id is stored, and whose
   connections page the rendered notification links to. Zero recipients is a
   legitimate outcome and continues straight to [k]. *)
let with_notifications (module C : Caqti_lwt.CONNECTION) ~rollback_to ~kind
    ~actor_user_id ~community_id ~connection_id k =
  let of_notification_error = function
    | Notifications.Inconsistent_data -> Inconsistent_data
    | Notifications.Storage_error -> Storage_error
  in
  Notifications.community_top_moderator_ids (module C) ~community_id
  >>= function
  | Error e -> rollback_to (of_notification_error e)
  | Ok recipient_user_ids -> (
      Notifications.insert_many
        (module C)
        ~kind ~actor_user_id ~community_id ~connection_id ~recipient_user_ids
      >>= function
      | Error e -> rollback_to (of_notification_error e)
      | Ok () -> k ())

(* The locked row rebuilt through the pure domain, then replayed forward to
   [target] one legal transition at a time. Returns None on any structural
   incoherence — that is Inconsistent_data at the call site, never a
   silently repaired value. *)
let reconstruct ~requester ~recipient ~note ~target =
  match Cc.create_pending ~requester_community_id:requester
          ~recipient_community_id:recipient ~request_note:note with
  | Error _ -> None
  | Ok pending ->
      (* The stored note must round-trip byte-exactly: a durable note the
         canonicalizer would change is corruption, not input to repair. *)
      if Cc.request_note pending <> note then None
      else
        let rec replay value = function
          | [] -> Some value
          | action :: rest -> (
              match Cc.apply value action with
              | Error _ -> None
              | Ok next -> replay next rest)
        in
        let path =
          match target with
          | Cc.Accepted -> [ Cc.Accept ]
          | Cc.Rejected -> [ Cc.Reject ]
          | Cc.Removed -> [ Cc.Accept; Cc.Remove ]
          | Cc.Pending -> []
        in
        replay pending path

(* === request === *)

let request (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~connection =
  let requester = Cc.requester_community_id connection in
  let recipient = Cc.recipient_community_id connection in
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if Cc.status connection <> Cc.Pending then
    Lwt.return (Error Invalid_connection)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free —
       error payloads can echo SQL parameters — and rollback failure adds
       nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () ->
        with_locked_pair
          (module C)
          ~rollback_to ~requester ~recipient
          (fun ~requester_eligible ~recipient_eligible ->
            (* Creating a connection requires both sides currently eligible,
               revalidated here under the held FOR SHARE locks — the search
               and confirmation surfaces decided on an earlier snapshot, and
               either community may have gone private, unpublished, or
               undiscoverable since. The requesting side is answered first so
               its own moderators learn about their own community rather than
               about the target's state. *)
            if not requester_eligible then rollback_to Requester_ineligible
            else if not recipient_eligible then
              rollback_to Recipient_ineligible
            else
              C.find_opt insert_connection_query
                ((requester, recipient),
                 (actor_user_id, Cc.request_note connection))
              >>= function
              | Error _ -> rollback_to Storage_error
              | Ok None ->
                  (* The partial unique index refused the row: an active
                     connection already holds this unordered pair, in either
                     direction. The conflict never surfaces as a raw SQL
                     diagnostic. *)
                  rollback_to Active_connection_exists
              | Ok (Some new_id) ->
                  if not (positive new_id) then rollback_to Inconsistent_data
                  else (
                    (* The audit event rides the same transaction: inserted
                       only once the pending row is validated, and any audit
                       failure rolls the whole request back — a committed
                       request without its event cannot exist. The private
                       note never reaches it. *)
                    Community_connection_audit.insert
                      (module C)
                      ~action:Community_connection_audit.Connection_requested
                      ~actor_user_id ~connection_id:new_id
                      ~requester_community_id:requester
                      ~recipient_community_id:recipient
                    >>= function
                    | Error Community_connection_audit.Inconsistent_data ->
                        rollback_to Inconsistent_data
                    | Error Community_connection_audit.Storage_error ->
                        rollback_to Storage_error
                    | Ok () ->
                        (* A request is addressed to the recipient community,
                           so its own top moderators are the ones who need to
                           act on it, and their management context is that
                           community. *)
                        with_notifications
                          (module C)
                          ~rollback_to ~kind:Notifications.Connection_requested
                          ~actor_user_id ~community_id:recipient
                          ~connection_id:new_id
                          (fun () ->
                            C.commit () >>= function
                            | Error _ -> Lwt.return (Error Storage_error)
                            | Ok () ->
                                Lwt.return (Ok { created_id = new_id }))))

(* === review === *)

let review (module C : Caqti_lwt.CONNECTION) ~reviewer_user_id ~connection_id
    ~recipient_community_id ~decision =
  if reviewer_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (positive connection_id) then
    Lwt.return (Error Invalid_connection_id)
  else if recipient_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked pair discovery first, only to fix the lock order. *)
        C.find_opt discover_pending_pair_query
          (connection_id, recipient_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Review_unavailable
        | Ok (Some (discovered_requester, discovered_recipient)) ->
            if
              discovered_requester <= 0 || discovered_recipient <= 0
              || discovered_requester = discovered_recipient
              || discovered_recipient <> recipient_community_id
            then rollback_to Inconsistent_data
            else
              with_locked_pair
                (module C)
                ~rollback_to ~requester:discovered_requester
                ~recipient:discovered_recipient
                (fun ~requester_eligible ~recipient_eligible ->
        C.find_opt lock_pending_connection_query
          (connection_id, recipient_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Review_unavailable
        | Ok
            (Some
              ( ((row_id, requester), (recipient, stored_note)),
                ( (requested_by, reviewed_by, removed_by),
                  (reviewed_at_null, removed_at_null) ) )) -> (
            let row_shape_ok =
              Int64.equal row_id connection_id
              && recipient = recipient_community_id
              && requester > 0
              && requester <> recipient
              (* The locked row must still be the pair the locks were taken
                 for; the two columns are immutable, so a difference is
                 durable corruption rather than a race. *)
              && requester = discovered_requester
              && recipient = discovered_recipient
              && (match requested_by with None -> true | Some id -> id > 0)
              && reviewed_by = None && removed_by = None && reviewed_at_null
              && removed_at_null
            in
            let target =
              match decision with Accept -> Cc.Accepted | Reject -> Cc.Rejected
            in
            match
              ( row_shape_ok,
                reconstruct ~requester ~recipient ~note:stored_note ~target )
            with
            | false, _ | _, None -> rollback_to Inconsistent_data
            (* Accepting creates the connection, so it needs current
               eligibility on both sides, revalidated under the held locks.
               The reviewing community is answered first — that is the one
               its own moderators are entitled to hear about. Rejection
               deliberately reaches none of this and stays available on an
               ineligible pair. *)
            | true, Some _ when decision = Accept && not recipient_eligible ->
                rollback_to Recipient_ineligible
            | true, Some _ when decision = Accept && not requester_eligible ->
                rollback_to Requester_ineligible
            | true, Some reviewed -> (
                let new_status = Cc.status reviewed in
                let status_string = Cc.string_of_status new_status in
                C.collect_list review_connection_query
                  ((connection_id, status_string),
                   (reviewer_user_id, recipient_community_id))
                >>= function
                | Error _ ->
                    (* Includes any unexpected constraint failure — the
                       pending row already holds the active-pair slot, so a
                       unique conflict here is impossible under the locked
                       protocol. *)
                    rollback_to Storage_error
                | Ok [] -> rollback_to Review_unavailable
                | Ok [ (updated_id, updated_status) ] ->
                    if
                      not
                        (Int64.equal updated_id connection_id
                        && String.equal updated_status status_string)
                    then rollback_to Inconsistent_data
                    else (
                      Community_connection_audit.insert
                        (module C)
                        ~action:
                          (match decision with
                          | Accept ->
                              Community_connection_audit.Connection_accepted
                          | Reject ->
                              Community_connection_audit.Connection_rejected)
                        ~actor_user_id:reviewer_user_id ~connection_id
                        ~requester_community_id:requester
                        ~recipient_community_id:recipient
                      >>= function
                      | Error Community_connection_audit.Inconsistent_data ->
                          rollback_to Inconsistent_data
                      | Error Community_connection_audit.Storage_error ->
                          rollback_to Storage_error
                      | Ok () ->
                          (* A review answers the requesting community, so its
                             own top moderators hear about it and their
                             management context is that community. The
                             reviewing side is not notified: it is the side
                             that just acted. *)
                          with_notifications
                            (module C)
                            ~rollback_to
                            ~kind:
                              (match decision with
                              | Accept -> Notifications.Connection_accepted
                              | Reject -> Notifications.Connection_rejected)
                            ~actor_user_id:reviewer_user_id
                            ~community_id:requester ~connection_id
                            (fun () ->
                              C.commit () >>= function
                              | Error _ -> Lwt.return (Error Storage_error)
                              | Ok () ->
                                  Lwt.return
                                    (Ok
                                       {
                                         reviewed_id = connection_id;
                                         reviewed_requester = requester;
                                         reviewed_recipient = recipient;
                                         reviewed_result = new_status;
                                       })))
                | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data))))

(* === remove === *)


let remove (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~connection_id
    ~acting_community_id =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (positive connection_id) then
    Lwt.return (Error Invalid_connection_id)
  else if acting_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked pair discovery first, only to fix the lock order — the
           same discipline as review, so no mutation here can invert the
           community/connection order against another. Removal deliberately
           reads no eligibility: an accepted connection must stay removable
           after either side goes private, unpublished, or undiscoverable. *)
        C.find_opt discover_accepted_pair_query
          (connection_id, acting_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Removal_unavailable
        | Ok (Some (discovered_requester, discovered_recipient)) ->
            if
              discovered_requester <= 0 || discovered_recipient <= 0
              || discovered_requester = discovered_recipient
            then rollback_to Inconsistent_data
            else
              with_locked_pair
                (module C)
                ~rollback_to ~requester:discovered_requester
                ~recipient:discovered_recipient
                (fun ~requester_eligible:_ ~recipient_eligible:_ ->
        C.find_opt lock_accepted_connection_query
          (connection_id, acting_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Removal_unavailable
        | Ok
            (Some
              ( ((row_id, requester), (recipient, stored_note)),
                ( (requested_by, reviewed_by, removed_by),
                  (reviewed_at_present, removed_at_null) ) )) -> (
            let row_shape_ok =
              Int64.equal row_id connection_id
              && requester > 0 && recipient > 0
              && requester <> recipient
              && requester = discovered_requester
              && recipient = discovered_recipient
              && (requester = acting_community_id
                 || recipient = acting_community_id)
              && (match requested_by with None -> true | Some id -> id > 0)
              && (match reviewed_by with None -> true | Some id -> id > 0)
              && removed_by = None && reviewed_at_present && removed_at_null
            in
            match
              ( row_shape_ok,
                reconstruct ~requester ~recipient ~note:stored_note
                  ~target:Cc.Removed )
            with
            | false, _ | _, None -> rollback_to Inconsistent_data
            | true, Some removed -> (
                let new_status = Cc.status removed in
                let status_string = Cc.string_of_status new_status in
                C.collect_list remove_connection_query
                  ((connection_id, status_string),
                   (actor_user_id, acting_community_id))
                >>= function
                | Error _ -> rollback_to Storage_error
                | Ok [] -> rollback_to Removal_unavailable
                | Ok [ (updated_id, updated_status) ] ->
                    if
                      not
                        (Int64.equal updated_id connection_id
                        && String.equal updated_status status_string)
                    then rollback_to Inconsistent_data
                    else (
                      Community_connection_audit.insert
                        (module C)
                        ~action:Community_connection_audit.Connection_removed
                        ~actor_user_id ~connection_id
                        ~requester_community_id:requester
                        ~recipient_community_id:recipient
                      >>= function
                      | Error Community_connection_audit.Inconsistent_data ->
                          rollback_to Inconsistent_data
                      | Error Community_connection_audit.Storage_error ->
                          rollback_to Storage_error
                      | Ok () ->
                          (* Removal is symmetric — either side may detach —
                             so the notified side is whichever community did
                             not act, derived from the acting community the
                             guarded UPDATE already verified is in the pair.
                             That community is also the notified moderators'
                             own management context. *)
                          let opposite =
                            if acting_community_id = requester then recipient
                            else requester
                          in
                          with_notifications
                            (module C)
                            ~rollback_to ~kind:Notifications.Connection_removed
                            ~actor_user_id ~community_id:opposite ~connection_id
                            (fun () ->
                              C.commit () >>= function
                              | Error _ -> Lwt.return (Error Storage_error)
                              | Ok () ->
                                  Lwt.return
                                    (Ok
                                       {
                                         removed_id = connection_id;
                                         removed_requester = requester;
                                         removed_recipient = recipient;
                                         removed_result = new_status;
                                       })))
                | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data))))
