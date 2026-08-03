(* Atomic writes for the shared-thread placement lifecycle. Each function is
   one explicit transaction taking the single shared lock order — both
   communities in ascending id order FOR SHARE, then the pair's accepted
   community_connections row FOR SHARE (when creating or accepting), then
   the canonical posts row FOR SHARE (when its content or author is read),
   then the chosen destination community_sections row FOR SHARE (when
   accepting into a section), then the exact shared_thread_placements row
   FOR UPDATE — so no two functions can deadlock against each other:
   FOR SHARE locks are mutually compatible, and only one row is ever taken
   FOR UPDATE.

   Every function begins with an unlocked discovery read purely to learn
   the subjects (the post's origin community, or the placement's pair) and
   so fix the lock order. That read decides lock order, never race safety:
   the guarded UPDATE re-verifies status and subject under the row lock,
   and a row that changed between the two reads simply fails the guard.

   The origin community is always derived from the post row — first
   discovered, then re-verified against the locked post and the locked
   placement — and never taken from a caller: posts.community_id is
   immutable in the application, so a mismatch anywhere is durable
   corruption, not a race.

   Race safety is the partial unique active-(post, destination) index
   (through ON CONFLICT DO NOTHING) and the status-guarded UPDATE — never a
   read-before-write check. Every lifecycle transition comes from the pure
   Shared_thread_placements domain over the value reconstructed from the
   locked row; the status vocabulary reaching SQL is always
   string_of_status.

   Authorization is deliberately absent: the acting user is provenance and
   the acting community is the subject boundary the SQL verifies. See the
   .mli for the full contract. *)

open Lwt.Infix

module Stp = Shared_thread_placements
module Cc = Community_connections
module Audit = Shared_thread_placement_audit
module Notifications = Shared_thread_notifications

type decision =
  | Accept of int option
  | Reject

type created_placement = { created_id : int64 }

let created_placement_id { created_id } = created_id

type reviewed_placement = {
  reviewed_id : int64;
  reviewed_post : int;
  reviewed_origin : int;
  reviewed_destination : int;
  reviewed_result : Stp.status;
  reviewed_section : int option;
}

let reviewed_placement_id { reviewed_id; _ } = reviewed_id

let reviewed_post_id { reviewed_post; _ } = reviewed_post

let reviewed_origin_community_id { reviewed_origin; _ } = reviewed_origin

let reviewed_destination_community_id { reviewed_destination; _ } =
  reviewed_destination

let reviewed_status { reviewed_result; _ } = reviewed_result

let reviewed_destination_section_id { reviewed_section; _ } = reviewed_section

type withdrawn_placement = {
  withdrawn_id : int64;
  withdrawn_post : int;
  withdrawn_origin : int;
  withdrawn_destination : int;
}

let withdrawn_placement_id { withdrawn_id; _ } = withdrawn_id

let withdrawn_post_id { withdrawn_post; _ } = withdrawn_post

let withdrawn_origin_community_id { withdrawn_origin; _ } = withdrawn_origin

let withdrawn_destination_community_id { withdrawn_destination; _ } =
  withdrawn_destination

type removed_placement = {
  removed_id : int64;
  removed_post : int;
  removed_origin : int;
  removed_destination : int;
}

let removed_placement_id { removed_id; _ } = removed_id

let removed_post_id { removed_post; _ } = removed_post

let removed_origin_community_id { removed_origin; _ } = removed_origin

let removed_destination_community_id { removed_destination; _ } =
  removed_destination

type error =
  | Invalid_user_id
  | Invalid_placement_id
  | Invalid_post_id
  | Invalid_community_id
  | Invalid_request_note
  | Same_community
  | Community_unavailable
  | Post_unavailable
  | Post_tombstoned
  | No_accepted_connection
  | Origin_ineligible
  | Destination_ineligible
  | Active_placement_exists
  | Invalid_destination_section
  | Review_unavailable
  | Withdrawal_unavailable
  | Removal_unavailable
  | Inconsistent_data
  | Storage_error

let positive id = Int64.compare id 0L > 0

(* === queries === *)

(* The canonical post's immutable origin community, read WITHOUT a lock,
   solely so the two communities can then be locked in the one global
   ascending order. Never a race-safety check: the post is re-read under
   FOR SHARE inside the lock order and its community re-verified there. *)
let discover_post_origin_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT community_id FROM posts WHERE id = $1"

(* Existence, deletion protection, and the current eligibility facts for one
   referenced community, taken in ascending id order. FOR SHARE — not the
   weaker FOR KEY SHARE — because the decision below reads
   visibility/onboarding_state/discoverable (and, for the destination, the
   section mode): only FOR SHARE conflicts with the FOR NO KEY UPDATE a
   concurrent visibility or publication change takes, so eligibility cannot
   flip between this read and the commit. FOR SHARE locks are mutually
   compatible, so two placement transactions sharing a community never wait
   on each other and no ordering cycle can form among them. The row is
   fetched with no eligibility predicate: the pure domain decides, so a
   legitimately ineligible community is distinguishable from a missing one
   here and collapses only at the call site. *)
let lock_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->? Caqti_type.(t2 (t2 int string) (t3 string bool bool)))
  "SELECT id, visibility, onboarding_state, discoverable, sections_enabled \
   FROM communities \
   WHERE id = $1 \
   FOR SHARE"

(* The pair's accepted mutual connection, locked FOR SHARE so a concurrent
   connection removal (which takes the row FOR UPDATE) cannot commit between
   this check and our commit. The LEAST/GREATEST normalization is the same
   one the connections store and its active-pair index use, so either
   direction of the stored pair matches. Zero rows means the pair holds no
   accepted connection — none ever, a still-pending request, or a rejected/
   removed history — and collapses into one variant at the call site.
   Community_connections owns this relation; only its accepted state is
   read here, never duplicated policy. *)
let lock_accepted_connection_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.int64)
  "SELECT id FROM community_connections \
   WHERE LEAST(requester_community_id, recipient_community_id) \
         = LEAST($1, $2) \
     AND GREATEST(requester_community_id, recipient_community_id) \
         = GREATEST($1, $2) \
     AND status = 'accepted' \
   FOR SHARE"

(* The canonical post under FOR SHARE: existence, the immutable origin to
   re-verify, the author for notification resolution, and the content for
   the tombstone rule. FOR SHARE conflicts with the row lock every durable
   deletion path takes (the tombstoning UPDATE), so a post cannot be
   tombstoned between this read and the commit. *)
let lock_post_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->? Caqti_type.(t2 (t2 int int) (t2 int (option string))))
  "SELECT id, community_id, user_id, content \
   FROM posts \
   WHERE id = $1 \
   FOR SHARE"

(* The chosen destination section, locked FOR SHARE so a concurrent section
   deletion (whose DELETE takes the row exclusively) cannot commit an
   acceptance against a vanished section. The community predicate is the
   boundary: a section belonging to any other community is
   indistinguishable from a missing one. *)
let lock_section_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
  "SELECT id FROM community_sections \
   WHERE id = $1 AND community_id = $2 \
   FOR SHARE"

(* The conflict target is the partial unique active-(post, destination)
   index, written exactly as the migration declares it so PostgreSQL infers
   that index and no other. Zero returned rows means an active (pending or
   accepted) placement already holds this thread's slot in this destination
   — pre-existing or a concurrently committed winner — and the transaction
   rolls back as a whole. Historical rejected/removed/withdrawn rows fall
   outside the predicate and never conflict. Timestamps ride the database
   defaults; the pending row shape (no reviewer, remover, withdrawer,
   section, or their timestamps) is written explicitly. *)
let insert_placement_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 int int int) (t2 int (option string)))
   ->? Caqti_type.int64)
  "INSERT INTO shared_thread_placements \
     (post_id, origin_community_id, destination_community_id, \
      destination_section_id, status, requested_by_user_id, \
      reviewed_by_user_id, removed_by_user_id, withdrawn_by_user_id, \
      request_note, reviewed_at, removed_at, withdrawn_at) \
   VALUES ($1, $2, $3, NULL, 'pending', $4, NULL, NULL, NULL, $5, \
           NULL, NULL, NULL) \
   ON CONFLICT (post_id, destination_community_id) \
     WHERE status IN ('pending', 'accepted') \
   DO NOTHING \
   RETURNING id"

(* The exact subjects of one row, read WITHOUT a lock, solely to fix the
   lock order — one variant per subject boundary (destination for review,
   origin for withdrawal, either side for removal). *)
let discover_pending_for_destination_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.(t3 int int int))
  "SELECT post_id, origin_community_id, destination_community_id \
   FROM shared_thread_placements \
   WHERE id = $1 AND destination_community_id = $2 AND status = 'pending'"

let discover_pending_for_origin_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.(t3 int int int))
  "SELECT post_id, origin_community_id, destination_community_id \
   FROM shared_thread_placements \
   WHERE id = $1 AND origin_community_id = $2 AND status = 'pending'"

let discover_accepted_for_member_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int) ->? Caqti_type.(t3 int int int))
  "SELECT post_id, origin_community_id, destination_community_id \
   FROM shared_thread_placements \
   WHERE id = $1 AND status = 'accepted' \
     AND (origin_community_id = $2 OR destination_community_id = $2)"

(* The exact pending placement under its subject boundary, locked as the
   only durable row this transaction mutates. Every zero-row cause (no
   placement, another community's request, already reviewed, withdrawn, or
   removed, concurrent winner) collapses into one variant at the call
   site. *)
let lock_pending_placement_query boundary_column =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 int int))
           (t2
              (t2 (option string) (option int))
              (t2 (t4 bool bool bool bool) (t3 bool bool bool)))))
  (Printf.sprintf
     "SELECT id, post_id, origin_community_id, destination_community_id, \
             request_note, requested_by_user_id, \
             reviewed_by_user_id IS NULL, removed_by_user_id IS NULL, \
             withdrawn_by_user_id IS NULL, destination_section_id IS NULL, \
             reviewed_at IS NULL, removed_at IS NULL, withdrawn_at IS NULL \
      FROM shared_thread_placements \
      WHERE id = $1 AND %s = $2 AND status = 'pending' \
      FOR UPDATE"
     boundary_column)

(* boundary_column is one of two literals written in this file — never
   caller input — so the closed-variant discipline over dynamic SQL holds. *)
let lock_pending_for_destination_query =
  lock_pending_placement_query "destination_community_id"

let lock_pending_for_origin_query =
  lock_pending_placement_query "origin_community_id"

let lock_accepted_placement_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
   ->? Caqti_type.(
         t2
           (t2 (t2 int64 int) (t2 int int))
           (t2
              (t2 (option string) (option int))
              (t2 (t2 (option int) bool) (t3 bool bool bool)))))
  "SELECT id, post_id, origin_community_id, destination_community_id, \
          request_note, requested_by_user_id, \
          reviewed_by_user_id, removed_by_user_id IS NULL, \
          withdrawn_by_user_id IS NULL, \
          reviewed_at IS NOT NULL, removed_at IS NULL, withdrawn_at IS NULL \
   FROM shared_thread_placements \
   WHERE id = $1 AND status = 'accepted' \
     AND (origin_community_id = $2 OR destination_community_id = $2) \
   FOR UPDATE"

(* The one review mutation: the locked row, guarded again on both the
   pending status and the expected destination, so a zero count is a
   completed concurrent transition — never a second write and never a
   request addressed elsewhere. The status vocabulary comes exclusively
   from the pure domain at the call site. GREATEST clamps against the
   NOW()-before-created_at race the schema CHECKs would otherwise trip on
   same-transaction-batch fixtures. The removal and withdrawal columns are
   written NULL explicitly to keep the reviewed row shape closed; the
   section is the reviewer's accept-time choice and NULL on rejection. *)
let review_placement_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 string) (t3 int int (option int)))
   ->* Caqti_type.(t2 int64 string))
  "UPDATE shared_thread_placements \
   SET status = $2, \
       reviewed_by_user_id = $3, \
       reviewed_at = GREATEST(NOW(), created_at), \
       destination_section_id = $5, \
       removed_by_user_id = NULL, \
       removed_at = NULL, \
       withdrawn_by_user_id = NULL, \
       withdrawn_at = NULL, \
       updated_at = GREATEST(NOW(), created_at) \
   WHERE id = $1 AND status = 'pending' AND destination_community_id = $4 \
   RETURNING id, status"

(* The one withdrawal mutation, guarded on the pending status and the
   origin boundary. Review and removal columns are written NULL explicitly:
   a withdrawn row never carries a review, a removal, or a section, and the
   durable shape CHECK holds it to that. *)
let withdraw_placement_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 string) (t2 int int))
   ->* Caqti_type.(t2 int64 string))
  "UPDATE shared_thread_placements \
   SET status = $2, \
       withdrawn_by_user_id = $3, \
       withdrawn_at = GREATEST(NOW(), created_at), \
       reviewed_by_user_id = NULL, \
       reviewed_at = NULL, \
       removed_by_user_id = NULL, \
       removed_at = NULL, \
       destination_section_id = NULL, \
       updated_at = GREATEST(NOW(), created_at) \
   WHERE id = $1 AND status = 'pending' AND origin_community_id = $4 \
   RETURNING id, status"

(* The one removal mutation, guarded again on the accepted status and on
   the acting community's membership in the pair. reviewed_at is NOT NULL
   on every accepted row, and GREATEST ignores NULLs, so the clamp cannot
   produce a removed_at before either earlier timestamp. reviewed_by and
   the accepted section are never overwritten: the reviewer and the
   destination section stay behind as historical provenance beside the
   remover. *)
let remove_placement_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int64 string) (t2 int int))
   ->* Caqti_type.(t2 int64 string))
  "UPDATE shared_thread_placements \
   SET status = $2, \
       removed_by_user_id = $3, \
       removed_at = GREATEST(NOW(), created_at, reviewed_at), \
       updated_at = GREATEST(NOW(), created_at, reviewed_at) \
   WHERE id = $1 AND status = 'accepted' \
     AND (origin_community_id = $4 OR destination_community_id = $4) \
   RETURNING id, status"

(* === shared helpers === *)

(* One locked community: closed-value validation of the durable row, then
   its current connection eligibility from the single pure predicate in
   Community_connections — never a second copy of the three booleans. A
   legitimately ineligible community is not corruption; only an off-enum
   value or a mismatched id is. The section mode rides along for the
   destination's accept-time decision. *)
let with_locked_community (module C : Caqti_lwt.CONNECTION) ~rollback_to id k =
  C.find_opt lock_community_query id >>= function
  | Error _ -> rollback_to Storage_error
  | Ok None ->
      (* Which side is missing never crosses: the store cannot be used to
         probe community existence. *)
      rollback_to Community_unavailable
  | Ok
      (Some
        ((row_id, visibility_raw), (onboarding_raw, discoverable, sections)))
    -> (
      match
        ( row_id = id && row_id > 0,
          Db.community_visibility_of_string visibility_raw,
          Db.community_onboarding_state_of_string onboarding_raw )
      with
      | true, Some visibility, Ok onboarding_state ->
          k
            ~eligible:
              (Cc.connection_eligible ~visibility ~onboarding_state
                 ~discoverable)
            ~sections_enabled:sections
      | _ -> rollback_to Inconsistent_data)

(* Both communities of one placement, locked in ascending id order — the
   single global lock order every mutation here follows, whichever of the
   two is the origin. Each side's facts come back labelled by its role, so
   no caller re-derives which lock answered for which community. *)
let with_locked_pair conn ~rollback_to ~origin ~destination k =
  let first, second =
    if origin <= destination then (origin, destination)
    else (destination, origin)
  in
  with_locked_community conn ~rollback_to first
    (fun ~eligible:first_eligible ~sections_enabled:first_sections ->
      with_locked_community conn ~rollback_to second
        (fun ~eligible:second_eligible ~sections_enabled:second_sections ->
          let origin_eligible, destination_eligible, destination_sections =
            if origin = first then
              (first_eligible, second_eligible, second_sections)
            else (second_eligible, first_eligible, first_sections)
          in
          k ~origin_eligible ~destination_eligible
            ~destination_sections_enabled:destination_sections))

let with_locked_accepted_connection (module C : Caqti_lwt.CONNECTION)
    ~rollback_to ~origin ~destination k =
  C.find_opt lock_accepted_connection_query (origin, destination) >>= function
  | Error _ -> rollback_to Storage_error
  | Ok None -> rollback_to No_accepted_connection
  | Ok (Some connection_id) ->
      if positive connection_id then k ()
      else rollback_to Inconsistent_data

(* The canonical post under the held community locks. [missing] is the
   caller's collapse for a vanished post: Post_unavailable on a request,
   and the operation's own unavailable variant elsewhere — a post can only
   vanish together with its placements (ON DELETE CASCADE), so the two
   reads agree. *)
let with_locked_post (module C : Caqti_lwt.CONNECTION) ~rollback_to ~missing
    ~post_id k =
  C.find_opt lock_post_query post_id >>= function
  | Error _ -> rollback_to Storage_error
  | Ok None -> rollback_to missing
  | Ok (Some ((row_id, community_id), (author_user_id, content))) ->
      if row_id = post_id && community_id > 0 && author_user_id > 0 then
        k ~post_community:community_id ~author:author_user_id ~content
      else rollback_to Inconsistent_data

(* Recipient assembly and notification insertion for one committed
   transition, on the same open transaction as the mutation and its audit
   event and before the commit — so a notification failure rolls the
   mutation and the event back with it, and a stale or losing transition,
   which never reaches here, creates neither. Each recipient carries the
   community context of their own side; the module-side deduplication
   keeps the first listed context, so callers list origin-side recipients
   first. *)
let with_notifications (module C : Caqti_lwt.CONNECTION) ~rollback_to ~kind
    ~actor_user_id ~placement_id ~recipients k =
  let of_notification_error = function
    | Notifications.Inconsistent_data -> Inconsistent_data
    | Notifications.Storage_error -> Storage_error
  in
  Notifications.insert_many
    (module C)
    ~kind ~actor_user_id ~placement_id ~recipients
  >>= function
  | Error e -> rollback_to (of_notification_error e)
  | Ok () -> k ()

let with_top_moderators (module C : Caqti_lwt.CONNECTION) ~rollback_to
    ~community_id k =
  Notifications.community_top_moderator_ids (module C) ~community_id
  >>= function
  | Error Notifications.Inconsistent_data -> rollback_to Inconsistent_data
  | Error Notifications.Storage_error -> rollback_to Storage_error
  | Ok ids -> k ids

let with_audit (module C : Caqti_lwt.CONNECTION) ~rollback_to ~action
    ~actor_user_id ~placement_id ~post_id ~origin ~destination k =
  Audit.insert
    (module C)
    ~action ~actor_user_id ~placement_id ~post_id ~origin_community_id:origin
    ~destination_community_id:destination
  >>= function
  | Error Audit.Inconsistent_data -> rollback_to Inconsistent_data
  | Error Audit.Storage_error -> rollback_to Storage_error
  | Ok () -> k ()

(* The locked row rebuilt through the pure domain, then replayed forward to
   [target] one legal transition at a time. Returns None on any structural
   incoherence — that is Inconsistent_data at the call site, never a
   silently repaired value. *)
let reconstruct ~post ~origin ~destination ~note ~target =
  match
    Stp.create_pending ~post_id:post ~origin_community_id:origin
      ~destination_community_id:destination ~request_note:note
  with
  | Error _ -> None
  | Ok pending ->
      (* The stored note must round-trip byte-exactly: a durable note the
         canonicalizer would change is corruption, not input to repair. *)
      if Stp.request_note pending <> note then None
      else
        let rec replay value = function
          | [] -> Some value
          | action :: rest -> (
              match Stp.apply value action with
              | Error _ -> None
              | Ok next -> replay next rest)
        in
        let path =
          match target with
          | Stp.Accepted -> [ Stp.Accept ]
          | Stp.Rejected -> [ Stp.Reject ]
          | Stp.Withdrawn -> [ Stp.Withdraw ]
          | Stp.Removed -> [ Stp.Accept; Stp.Remove ]
          | Stp.Pending -> []
        in
        replay pending path

(* === request === *)

let request (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~post_id
    ~destination_community_id ~request_note =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if post_id <= 0 then Lwt.return (Error Invalid_post_id)
  else if destination_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else
    (* As in the sibling stores, every Caqti error is dropped payload-free —
       error payloads can echo SQL parameters — and rollback failure adds
       nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked origin discovery first, only to fix the lock order. The
           origin is the post's own durable community — never a caller
           value. *)
        C.find_opt discover_post_origin_query post_id
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Post_unavailable
        | Ok (Some origin) -> (
            if origin <= 0 then rollback_to Inconsistent_data
            else
              (* The pure domain validates the pair and canonicalizes the
                 note before any lock is taken; its Same_community answer is
                 the "already lives there" refusal. *)
              match
                Stp.create_pending ~post_id ~origin_community_id:origin
                  ~destination_community_id ~request_note
              with
              | Error Stp.Same_community -> rollback_to Same_community
              | Error Stp.Invalid_request_note ->
                  rollback_to Invalid_request_note
              | Error _ -> rollback_to Inconsistent_data
              | Ok placement ->
                  with_locked_pair
                    (module C)
                    ~rollback_to ~origin ~destination:destination_community_id
                    (fun ~origin_eligible ~destination_eligible
                         ~destination_sections_enabled:_ ->
                      (* The accepted connection is the standing between the
                         two communities this whole feature rides on; locked
                         FOR SHARE so a concurrent disconnect cannot slip
                         between this check and the commit. *)
                      with_locked_accepted_connection
                        (module C)
                        ~rollback_to ~origin
                        ~destination:destination_community_id
                        (fun () ->
                          with_locked_post
                            (module C)
                            ~rollback_to ~missing:Post_unavailable ~post_id
                            (fun ~post_community ~author:_ ~content ->
                              if post_community <> origin then
                                (* posts.community_id is immutable; a change
                                   between discovery and lock is corruption,
                                   not a race. *)
                                rollback_to Inconsistent_data
                              else if Stp.post_content_tombstoned content
                              then rollback_to Post_tombstoned
                              else if not origin_eligible then
                                (* Creating a placement requires both sides
                                   currently eligible, revalidated here
                                   under the held FOR SHARE locks. The
                                   origin is answered first so its own
                                   surfaces learn about their own community
                                   rather than about the target's state. *)
                                rollback_to Origin_ineligible
                              else if not destination_eligible then
                                rollback_to Destination_ineligible
                              else
                                C.find_opt insert_placement_query
                                  ( (post_id, origin,
                                     destination_community_id),
                                    (actor_user_id,
                                     Stp.request_note placement) )
                                >>= function
                                | Error _ -> rollback_to Storage_error
                                | Ok None ->
                                    (* The partial unique index refused the
                                       row: an active placement already
                                       holds this thread's slot in this
                                       destination. The conflict never
                                       surfaces as a raw SQL diagnostic. *)
                                    rollback_to Active_placement_exists
                                | Ok (Some new_id) ->
                                    if not (positive new_id) then
                                      rollback_to Inconsistent_data
                                    else
                                      with_audit
                                        (module C)
                                        ~rollback_to
                                        ~action:Audit.Placement_requested
                                        ~actor_user_id ~placement_id:new_id
                                        ~post_id ~origin
                                        ~destination:destination_community_id
                                        (fun () ->
                                          (* A request is addressed to the
                                             destination community, so its
                                             own top moderators are the ones
                                             who need to act on it, and
                                             their context is that
                                             community. *)
                                          with_top_moderators
                                            (module C)
                                            ~rollback_to
                                            ~community_id:
                                              destination_community_id
                                            (fun destination_mods ->
                                              with_notifications
                                                (module C)
                                                ~rollback_to
                                                ~kind:
                                                  Notifications
                                                  .Placement_requested
                                                ~actor_user_id
                                                ~placement_id:new_id
                                                ~recipients:
                                                  (List.map
                                                     (fun user ->
                                                       ( user,
                                                         destination_community_id
                                                       ))
                                                     destination_mods)
                                                (fun () ->
                                                  C.commit () >>= function
                                                  | Error _ ->
                                                      Lwt.return
                                                        (Error Storage_error)
                                                  | Ok () ->
                                                      Lwt.return
                                                        (Ok
                                                           {
                                                             created_id =
                                                               new_id;
                                                           })))))))))

(* === review === *)

(* One shared shape check over a locked pending row: the subjects must be
   exactly the discovered ones (all three columns are immutable, so a
   difference is durable corruption rather than a race), and the pending
   shape must be closed. *)
let pending_row_ok ~row ~placement_id ~post ~origin ~destination =
  let ( ((row_id, row_post), (row_origin, row_destination)),
        ( (_note, requested_by),
          ( (reviewed_by_null, removed_by_null, withdrawn_by_null,
             section_null),
            (reviewed_at_null, removed_at_null, withdrawn_at_null) ) ) ) =
    row
  in
  Int64.equal row_id placement_id
  && row_post = post && row_origin = origin
  && row_destination = destination
  && row_post > 0 && row_origin > 0 && row_destination > 0
  && row_origin <> row_destination
  && (match requested_by with None -> true | Some id -> id > 0)
  && reviewed_by_null && removed_by_null && withdrawn_by_null && section_null
  && reviewed_at_null && removed_at_null && withdrawn_at_null

let pending_row_note ~row =
  let (_, ((note, _), _)) = row in
  note

let pending_row_requester ~row =
  let (_, ((_, requested_by), _)) = row in
  requested_by

let review (module C : Caqti_lwt.CONNECTION) ~reviewer_user_id ~placement_id
    ~destination_community_id ~decision =
  if reviewer_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (positive placement_id) then
    Lwt.return (Error Invalid_placement_id)
  else if destination_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else if
    match decision with Accept (Some sid) -> sid <= 0 | _ -> false
  then Lwt.return (Error Invalid_destination_section)
  else
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked subject discovery first, only to fix the lock order. *)
        C.find_opt discover_pending_for_destination_query
          (placement_id, destination_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Review_unavailable
        | Ok (Some (post, origin, destination)) ->
            if
              post <= 0 || origin <= 0 || destination <= 0
              || origin = destination
              || destination <> destination_community_id
            then rollback_to Inconsistent_data
            else
              with_locked_pair
                (module C)
                ~rollback_to ~origin ~destination
                (fun ~origin_eligible ~destination_eligible
                     ~destination_sections_enabled ->
                  (* Accepting creates the destination placement, so it
                     needs the standing connection, the live canonical
                     content, current eligibility on both sides, and a
                     coherent section choice — all revalidated under the
                     held locks. Rejecting deliberately requires none of
                     that and stays available on a disconnected, ineligible,
                     or tombstoned pair, so stale work can always be
                     closed.

                     The connection lock is taken here, before the post
                     lock, to keep the single shared lock order; a missing
                     row locks nothing, so its absence is judged only in
                     the accept gates below, after the locked post has
                     answered — the refusal precedence is unchanged. *)
                  let with_connection_locked k =
                    match decision with
                    | Reject -> k ~connection:None
                    | Accept _ -> (
                        C.find_opt lock_accepted_connection_query
                          (origin, destination)
                        >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok found -> k ~connection:found)
                  in
                  with_connection_locked (fun ~connection ->
                  let with_accept_gates k =
                    match decision with
                    | Reject -> k ~section:None
                    | Accept section -> (
                        match connection with
                        | None -> rollback_to No_accepted_connection
                        | Some connection_id
                          when not (positive connection_id) ->
                            rollback_to Inconsistent_data
                        | Some _ ->
                            if not destination_eligible then
                              (* The reviewing community is answered first —
                                 that is the one its own moderators are
                                 entitled to hear about. *)
                              rollback_to Destination_ineligible
                            else if not origin_eligible then
                              rollback_to Origin_ineligible
                            else if destination_sections_enabled then
                              match section with
                              | None ->
                                  (* A sectioned destination must choose
                                     where the thread lands, exactly as its
                                     own composer must. *)
                                  rollback_to Invalid_destination_section
                              | Some sid -> (
                                  C.find_opt lock_section_query
                                    (sid, destination)
                                  >>= function
                                  | Error _ -> rollback_to Storage_error
                                  | Ok None ->
                                      (* Missing and another community's
                                         section collapse alike: the
                                         boundary is generic. *)
                                      rollback_to Invalid_destination_section
                                  | Ok (Some locked_sid) ->
                                      if locked_sid = sid then
                                        k ~section:(Some sid)
                                      else rollback_to Inconsistent_data)
                            else
                              match section with
                              | Some _ ->
                                  (* A flat destination has no section to
                                     accept into; a supplied one is refused
                                     rather than silently dropped. *)
                                  rollback_to Invalid_destination_section
                              | None -> k ~section:None)
                  in
                  (* The post is read under lock for both decisions: the
                     tombstone rule gates acceptance only, but the thread
                     author is a review-outcome recipient either way. A
                     vanished post implies vanished placements (CASCADE), so
                     the collapse is the same as a vanished placement. *)
                  with_locked_post
                    (module C)
                    ~rollback_to ~missing:Review_unavailable ~post_id:post
                    (fun ~post_community ~author ~content ->
                      if post_community <> origin then
                        rollback_to Inconsistent_data
                      else if
                        (match decision with
                        | Accept _ -> Stp.post_content_tombstoned content
                        | Reject -> false)
                      then rollback_to Post_tombstoned
                      else
                        with_accept_gates (fun ~section ->
                            C.find_opt lock_pending_for_destination_query
                              (placement_id, destination)
                            >>= function
                            | Error _ -> rollback_to Storage_error
                            | Ok None -> rollback_to Review_unavailable
                            | Ok (Some row) -> (
                                let target =
                                  match decision with
                                  | Accept _ -> Stp.Accepted
                                  | Reject -> Stp.Rejected
                                in
                                match
                                  ( pending_row_ok ~row ~placement_id ~post
                                      ~origin ~destination,
                                    reconstruct ~post ~origin ~destination
                                      ~note:(pending_row_note ~row) ~target )
                                with
                                | false, _ | _, None ->
                                    rollback_to Inconsistent_data
                                | true, Some reviewed -> (
                                    let new_status = Stp.status reviewed in
                                    let status_string =
                                      Stp.string_of_status new_status
                                    in
                                    C.collect_list review_placement_query
                                      ( (placement_id, status_string),
                                        (reviewer_user_id, destination,
                                         section) )
                                    >>= function
                                    | Error _ -> rollback_to Storage_error
                                    | Ok [] -> rollback_to Review_unavailable
                                    | Ok [ (updated_id, updated_status) ] ->
                                        if
                                          not
                                            (Int64.equal updated_id
                                               placement_id
                                            && String.equal updated_status
                                                 status_string)
                                        then rollback_to Inconsistent_data
                                        else
                                          with_audit
                                            (module C)
                                            ~rollback_to
                                            ~action:
                                              (match decision with
                                              | Accept _ ->
                                                  Audit.Placement_accepted
                                              | Reject ->
                                                  Audit.Placement_rejected)
                                            ~actor_user_id:reviewer_user_id
                                            ~placement_id ~post_id:post
                                            ~origin ~destination
                                            (fun () ->
                                              (* A review answers the origin
                                                 side: the requester and the
                                                 thread author both hear the
                                                 outcome — the author even
                                                 when an origin top mod
                                                 submitted the request — in
                                                 origin context. The
                                                 reviewing side is not
                                                 notified: it just acted. *)
                                              let origin_side =
                                                (match
                                                   pending_row_requester ~row
                                                 with
                                                | Some requester ->
                                                    [ (requester, origin) ]
                                                | None -> [])
                                                @ [ (author, origin) ]
                                              in
                                              with_notifications
                                                (module C)
                                                ~rollback_to
                                                ~kind:
                                                  (match decision with
                                                  | Accept _ ->
                                                      Notifications
                                                      .Placement_accepted
                                                  | Reject ->
                                                      Notifications
                                                      .Placement_rejected)
                                                ~actor_user_id:
                                                  reviewer_user_id
                                                ~placement_id
                                                ~recipients:origin_side
                                                (fun () ->
                                                  C.commit () >>= function
                                                  | Error _ ->
                                                      Lwt.return
                                                        (Error Storage_error)
                                                  | Ok () ->
                                                      Lwt.return
                                                        (Ok
                                                           {
                                                             reviewed_id =
                                                               placement_id;
                                                             reviewed_post =
                                                               post;
                                                             reviewed_origin =
                                                               origin;
                                                             reviewed_destination =
                                                               destination;
                                                             reviewed_result =
                                                               new_status;
                                                             reviewed_section =
                                                               section;
                                                           })))
                                    | Ok (_ :: _ :: _) ->
                                        rollback_to Inconsistent_data)))))))

(* === withdraw === *)

let withdraw (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~placement_id
    ~origin_community_id =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (positive placement_id) then
    Lwt.return (Error Invalid_placement_id)
  else if origin_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked subject discovery first, only to fix the lock order.
           Withdrawal is origin-side cleanup: it deliberately reads no
           eligibility, no connection, and no post content — a pending
           request must stay withdrawable after a disconnect, an
           eligibility change, a membership change, or a tombstoning. *)
        C.find_opt discover_pending_for_origin_query
          (placement_id, origin_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Withdrawal_unavailable
        | Ok (Some (post, origin, destination)) ->
            if
              post <= 0 || origin <= 0 || destination <= 0
              || origin = destination
              || origin <> origin_community_id
            then rollback_to Inconsistent_data
            else
              with_locked_pair
                (module C)
                ~rollback_to ~origin ~destination
                (fun ~origin_eligible:_ ~destination_eligible:_
                     ~destination_sections_enabled:_ ->
                  C.find_opt lock_pending_for_origin_query
                    (placement_id, origin)
                  >>= function
                  | Error _ -> rollback_to Storage_error
                  | Ok None -> rollback_to Withdrawal_unavailable
                  | Ok (Some row) -> (
                      match
                        ( pending_row_ok ~row ~placement_id ~post ~origin
                            ~destination,
                          reconstruct ~post ~origin ~destination
                            ~note:(pending_row_note ~row)
                            ~target:Stp.Withdrawn )
                      with
                      | false, _ | _, None -> rollback_to Inconsistent_data
                      | true, Some withdrawn -> (
                          let status_string =
                            Stp.string_of_status (Stp.status withdrawn)
                          in
                          C.collect_list withdraw_placement_query
                            ( (placement_id, status_string),
                              (actor_user_id, origin) )
                          >>= function
                          | Error _ -> rollback_to Storage_error
                          | Ok [] -> rollback_to Withdrawal_unavailable
                          | Ok [ (updated_id, updated_status) ] ->
                              if
                                not
                                  (Int64.equal updated_id placement_id
                                  && String.equal updated_status
                                       status_string)
                              then rollback_to Inconsistent_data
                              else
                                with_audit
                                  (module C)
                                  ~rollback_to
                                  ~action:Audit.Placement_withdrawn
                                  ~actor_user_id ~placement_id ~post_id:post
                                  ~origin ~destination
                                  (fun () ->
                                    (* A withdrawal answers the destination
                                       community whose queue held the
                                       request, so its own top moderators
                                       hear about it, in their own
                                       context. *)
                                    with_top_moderators
                                      (module C)
                                      ~rollback_to ~community_id:destination
                                      (fun destination_mods ->
                                        with_notifications
                                          (module C)
                                          ~rollback_to
                                          ~kind:
                                            Notifications.Placement_withdrawn
                                          ~actor_user_id ~placement_id
                                          ~recipients:
                                            (List.map
                                               (fun user ->
                                                 (user, destination))
                                               destination_mods)
                                          (fun () ->
                                            C.commit () >>= function
                                            | Error _ ->
                                                Lwt.return
                                                  (Error Storage_error)
                                            | Ok () ->
                                                Lwt.return
                                                  (Ok
                                                     {
                                                       withdrawn_id =
                                                         placement_id;
                                                       withdrawn_post = post;
                                                       withdrawn_origin =
                                                         origin;
                                                       withdrawn_destination =
                                                         destination;
                                                     }))))
                          | Ok (_ :: _ :: _) -> rollback_to Inconsistent_data
                          ))))

(* === remove === *)

let remove (module C : Caqti_lwt.CONNECTION) ~actor_user_id ~placement_id
    ~acting_community_id =
  if actor_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (positive placement_id) then
    Lwt.return (Error Invalid_placement_id)
  else if acting_community_id <= 0 then
    Lwt.return (Error Invalid_community_id)
  else
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        (* Unlocked subject discovery first, only to fix the lock order.
           Removal deliberately reads no eligibility, no connection, and no
           tombstone rule: an accepted placement must stay removable after
           either side goes private, unpublished, or undiscoverable, after
           the communities disconnect, after the canonical content is
           tombstoned, and after the destination section is deleted. *)
        C.find_opt discover_accepted_for_member_query
          (placement_id, acting_community_id)
        >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to Removal_unavailable
        | Ok (Some (post, origin, destination)) ->
            if
              post <= 0 || origin <= 0 || destination <= 0
              || origin = destination
              || (origin <> acting_community_id
                 && destination <> acting_community_id)
            then rollback_to Inconsistent_data
            else
              with_locked_pair
                (module C)
                ~rollback_to ~origin ~destination
                (fun ~origin_eligible:_ ~destination_eligible:_
                     ~destination_sections_enabled:_ ->
                  (* The post is read under lock only for the author, who is
                     a removal recipient; its content is deliberately not
                     judged. A vanished post implies vanished placements
                     (CASCADE), so the collapse is the same. *)
                  with_locked_post
                    (module C)
                    ~rollback_to ~missing:Removal_unavailable ~post_id:post
                    (fun ~post_community ~author ~content:_ ->
                      if post_community <> origin then
                        rollback_to Inconsistent_data
                      else
                        C.find_opt lock_accepted_placement_query
                          (placement_id, acting_community_id)
                        >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok None -> rollback_to Removal_unavailable
                        | Ok
                            (Some
                              ( ((row_id, row_post),
                                 (row_origin, row_destination)),
                                ( (stored_note, requested_by),
                                  ( (reviewed_by, removed_by_null),
                                    ( reviewed_at_present,
                                      removed_at_null,
                                      withdrawn_at_null ) ) ) )) -> (
                            let row_shape_ok =
                              Int64.equal row_id placement_id
                              && row_post = post && row_origin = origin
                              && row_destination = destination
                              && (match requested_by with
                                 | None -> true
                                 | Some id -> id > 0)
                              && (match reviewed_by with
                                 | None -> true
                                 | Some id -> id > 0)
                              && removed_by_null && reviewed_at_present
                              && removed_at_null && withdrawn_at_null
                            in
                            match
                              ( row_shape_ok,
                                reconstruct ~post ~origin ~destination
                                  ~note:stored_note ~target:Stp.Removed )
                            with
                            | false, _ | _, None ->
                                rollback_to Inconsistent_data
                            | true, Some removed -> (
                                let status_string =
                                  Stp.string_of_status (Stp.status removed)
                                in
                                C.collect_list remove_placement_query
                                  ( (placement_id, status_string),
                                    (actor_user_id, acting_community_id) )
                                >>= function
                                | Error _ -> rollback_to Storage_error
                                | Ok [] -> rollback_to Removal_unavailable
                                | Ok [ (updated_id, updated_status) ] ->
                                    if
                                      not
                                        (Int64.equal updated_id placement_id
                                        && String.equal updated_status
                                             status_string)
                                    then rollback_to Inconsistent_data
                                    else
                                      with_audit
                                        (module C)
                                        ~rollback_to
                                        ~action:Audit.Placement_removed
                                        ~actor_user_id ~placement_id
                                        ~post_id:post ~origin ~destination
                                        (fun () ->
                                          (* A removal detaches both sides
                                             at once, so both hear about it:
                                             origin top moderators, the
                                             original requester, and the
                                             thread author in origin
                                             context; destination top
                                             moderators in destination
                                             context. Origin-side recipients
                                             are listed first, so a user
                                             reachable through both sides is
                                             notified once, in origin
                                             context. The actor is always
                                             excluded. *)
                                          with_top_moderators
                                            (module C)
                                            ~rollback_to ~community_id:origin
                                            (fun origin_mods ->
                                              with_top_moderators
                                                (module C)
                                                ~rollback_to
                                                ~community_id:destination
                                                (fun destination_mods ->
                                                  let recipients =
                                                    List.map
                                                      (fun user ->
                                                        (user, origin))
                                                      origin_mods
                                                    @ (match requested_by with
                                                      | Some requester ->
                                                          [ (requester,
                                                             origin) ]
                                                      | None -> [])
                                                    @ [ (author, origin) ]
                                                    @ List.map
                                                        (fun user ->
                                                          (user, destination))
                                                        destination_mods
                                                  in
                                                  with_notifications
                                                    (module C)
                                                    ~rollback_to
                                                    ~kind:
                                                      Notifications
                                                      .Placement_removed
                                                    ~actor_user_id
                                                    ~placement_id ~recipients
                                                    (fun () ->
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
                                                                 removed_id =
                                                                   placement_id;
                                                                 removed_post =
                                                                   post;
                                                                 removed_origin =
                                                                   origin;
                                                                 removed_destination =
                                                                   destination;
                                                               })))))
                                | Ok (_ :: _ :: _) ->
                                    rollback_to Inconsistent_data)))))
