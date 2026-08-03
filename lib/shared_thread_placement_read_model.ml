(* The authorized per-thread Share view. Authorization is inside the SQL (the
   canonical author while still a member and unbanned, an origin top_mod, or
   a session admin claim still backed by a durable users.is_admin), so an
   unauthorized caller and a missing post produce the same Ok None; the
   tombstone rule is the pure Shared_thread_placements.post_content_tombstoned
   applied to the selected content, so it can never drift from the domain
   authority. Read-only: no locks, no writes, no logging.

   The candidate eligibility rule lives in Community_connections. The
   candidate SQL carries the same predicate so the result bound applies after
   exclusion, and every row it returns is re-checked through the pure
   predicate, which remains the authority — the same double-keel the
   connections search uses. See the .mli for the full contract. *)

open Lwt.Infix

module Stp = Shared_thread_placements
module Cc = Community_connections

type candidate = {
  candidate_id : int;
  candidate_name : string;
  candidate_slug : string;
}

let candidate_id { candidate_id; _ } = candidate_id
let candidate_name { candidate_name; _ } = candidate_name
let candidate_slug { candidate_slug; _ } = candidate_slug

type active_placement = {
  placement_id : int64;
  placement_destination_name : string;
  placement_destination_slug : string;
  placement_pending : bool;
  placement_requested_by_viewer : bool;
  placement_note : string option;
}

let placement_id { placement_id; _ } = placement_id

let placement_destination_name { placement_destination_name; _ } =
  placement_destination_name

let placement_destination_slug { placement_destination_slug; _ } =
  placement_destination_slug

let placement_pending { placement_pending; _ } = placement_pending

let placement_requested_by_viewer { placement_requested_by_viewer; _ } =
  placement_requested_by_viewer

let placement_note { placement_note; _ } = placement_note

type share_view = {
  view_post_id : int;
  view_post_title : string;
  view_origin_community_id : int;
  view_origin_community_name : string;
  view_origin_community_slug : string;
  view_origin_manager : bool;
  view_candidates : candidate list;
  view_placements : active_placement list;
}

let view_post_id { view_post_id; _ } = view_post_id
let view_post_title { view_post_title; _ } = view_post_title

let view_origin_community_id { view_origin_community_id; _ } =
  view_origin_community_id

let view_origin_community_name { view_origin_community_name; _ } =
  view_origin_community_name

let view_origin_community_slug { view_origin_community_slug; _ } =
  view_origin_community_slug

let view_origin_manager { view_origin_manager; _ } = view_origin_manager
let view_candidates { view_candidates; _ } = view_candidates
let view_placements { view_placements; _ } = view_placements

type error =
  | Invalid_user_id
  | Invalid_post_id
  | Inconsistent_data
  | Storage_error

let max_candidate_destinations = 50
let max_active_placements = 100

(* Every community is addressed at /c/:slug, so a usable slug is one
   non-empty URL path segment — the same shape the sibling read models
   require. *)
let addressable_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

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

(* === queries === *)

(* Identity, share authorization, and the manager fact in one statement.
   $2 is the viewer, $3 their session admin state — which only enables the
   durable users.is_admin check beside it, never replaces it. The author
   path additionally requires current membership and the absence of both
   ban kinds, so a departed or banned author reads exactly like a
   stranger. The tombstone rule is applied in OCaml over the selected
   content: the pure domain predicate stays the single authority. *)
let authorize_share_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int int) bool)
   ->? Caqti_type.(
         t2 (t2 (t2 int string) (t2 (option string) int)) (t3 string string bool)))
  "SELECT p.id, p.title, p.content, c.id, c.slug, c.name, \
          (EXISTS (SELECT 1 FROM community_moderators tm \
                   WHERE tm.community_id = c.id AND tm.user_id = $2 \
                     AND tm.role = 'top_mod') \
           OR ($3 AND EXISTS (SELECT 1 FROM users ua \
                              WHERE ua.id = $2 AND ua.is_admin))) \
   FROM posts p \
   JOIN communities c ON c.id = p.community_id \
   WHERE p.id = $1 \
     AND ((p.user_id = $2 \
           AND EXISTS (SELECT 1 FROM community_members cm \
                       WHERE cm.user_id = $2 AND cm.community_id = c.id) \
           AND NOT EXISTS (SELECT 1 FROM community_bans cb \
                           WHERE cb.user_id = $2 AND cb.community_id = c.id) \
           AND NOT EXISTS (SELECT 1 FROM users ub \
                           WHERE ub.id = $2 AND ub.is_banned)) \
          OR EXISTS (SELECT 1 FROM community_moderators tm2 \
                     WHERE tm2.community_id = c.id AND tm2.user_id = $2 \
                       AND tm2.role = 'top_mod') \
          OR ($3 AND EXISTS (SELECT 1 FROM users ua2 \
                             WHERE ua2.id = $2 AND ua2.is_admin)))"

(* Connectable destinations, one SQL body for both shapes. The eligibility
   predicate is spelled in SQL so the LIMIT applies after exclusion — an
   ineligible or already-holding community can never consume a result slot
   and so can never be inferred from a short list — and every returned row
   is re-checked in OCaml through the one pure predicate, which stays the
   authority. Ordered by normalized name, then id, for a stable page. The
   two instantiations differ only in the placement-exclusion clause and the
   LIMIT parameter position, so the accepted-connection eligibility rule has
   exactly one spelling. *)
let candidate_sql ~exclusion ~limit_param =
  Printf.sprintf
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
            c.discoverable \
     FROM communities c \
     WHERE c.id <> $1 \
       AND c.visibility = 'public' \
       AND c.onboarding_state = 'published' \
       AND c.discoverable \
       AND EXISTS ( \
             SELECT 1 FROM community_connections cc \
             WHERE cc.status = 'accepted' \
               AND LEAST(cc.requester_community_id, \
                         cc.recipient_community_id) = LEAST(c.id, $1) \
               AND GREATEST(cc.requester_community_id, \
                            cc.recipient_community_id) = GREATEST(c.id, $1)) \
       %s \
     ORDER BY LOWER(c.name) ASC, c.id ASC \
     LIMIT %s"
    exclusion limit_param

let candidate_row_type =
  Caqti_type.(t2 (t2 (t2 int string) (t2 string string)) (t2 string bool))

let candidates_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int int int) ->* candidate_row_type)
    (candidate_sql
       ~exclusion:
         "AND NOT EXISTS ( \
            SELECT 1 FROM shared_thread_placements sp \
            WHERE sp.post_id = $2 AND sp.destination_community_id = c.id \
              AND sp.status IN ('pending', 'accepted'))"
       ~limit_param:"$3")

(* The composer shape: the post does not exist yet, so there is no active
   placement to exclude — only the origin itself, the connection, and
   current eligibility. *)
let connected_destinations_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->* candidate_row_type)
    (candidate_sql ~exclusion:"" ~limit_param:"$2")

(* The thread's live placements, oldest first. requested_by_user_id crosses
   only long enough to compute the two viewer-relative facts; it never
   leaves this module. *)
let placements_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int)
   ->* Caqti_type.(
         t2
           (t2 (t2 int64 string) (t2 (option int) (option string)))
           (t3 int string string)))
  "SELECT sp.id, sp.status, sp.requested_by_user_id, sp.request_note, \
          d.id, d.slug, d.name \
   FROM shared_thread_placements sp \
   JOIN communities d ON d.id = sp.destination_community_id \
   WHERE sp.post_id = $1 AND sp.status IN ('pending', 'accepted') \
   ORDER BY sp.created_at ASC, sp.id ASC \
   LIMIT $2"

(* === decoding === *)

let eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable =
  match
    ( Db.community_visibility_of_string visibility_raw,
      Db.community_onboarding_state_of_string onboarding_raw )
  with
  | Some visibility, Ok onboarding_state ->
      Some (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
  | None, _ | _, Error _ -> None

(* The SQL predicate bounded the page; the pure predicate decides whether
   each surviving row is really eligible. A row that disagrees can only mean
   an off-enum durable value. *)
let decode_candidate ~origin_id
    (((id, slug), (name, visibility_raw)), (onboarding_raw, discoverable)) =
  match eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable with
  | Some true
    when id > 0 && id <> origin_id && addressable_slug slug && nonblank name
         && control_safe name ->
      Some { candidate_id = id; candidate_name = name; candidate_slug = slug }
  | Some _ | None -> None

(* The stored note must round-trip byte-exactly through the pure
   canonicalizer: a durable note the canonicalizer would change is
   corruption, not text to repair on the way out. *)
let canonical_note ~post_id ~origin_community_id ~destination_community_id note
    =
  match
    Stp.create_pending ~post_id ~origin_community_id ~destination_community_id
      ~request_note:note
  with
  | Error _ -> false
  | Ok value -> Stp.request_note value = note

(* All-or-nothing: one incoherent row fails the whole call, so no caller can
   render a partially validated list. *)
let collect_all decode rows =
  List.fold_left
    (fun acc raw ->
      match (acc, decode raw) with
      | Some decoded, Some row -> Some (row :: decoded)
      | _ -> None)
    (Some []) rows
  |> Option.map List.rev

let collect (module C : Caqti_lwt.CONNECTION) query arg =
  C.collect_list query arg >>= function
  | Error _ -> Lwt.return (Error Storage_error)
  | Ok rows -> Lwt.return (Ok rows)

(* === reads === *)

let authorize (module C : Caqti_lwt.CONNECTION) ~user_id ~session_global_admin
    ~post_id =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if post_id <= 0 then Lwt.return (Error Invalid_post_id)
  else
    C.find_opt authorize_share_query ((post_id, user_id), session_global_admin)
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    (* Missing post and every unauthorized cause alike. *)
    | Ok None -> Lwt.return (Ok None)
    | Ok
        (Some
          (((stored_post_id, title), (content, community_id)), (slug, name, manager)))
      ->
        if
          not
            (stored_post_id = post_id && community_id > 0
            && addressable_slug slug && nonblank name && control_safe name)
        then Lwt.return (Error Inconsistent_data)
        else if Stp.post_content_tombstoned content then
          (* A tombstoned thread is not shareable; it collapses with
             missing rather than confirming what the tombstone replaced. *)
          Lwt.return (Ok None)
        else Lwt.return (Ok (Some (title, community_id, slug, name, manager)))

let resolve_destination_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.int)
  "SELECT c.id FROM communities c WHERE c.slug = $1"

let resolve_destination (module C : Caqti_lwt.CONNECTION) ~slug =
  if not (addressable_slug slug) then Lwt.return (Ok None)
  else
    C.find_opt resolve_destination_query slug >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None -> Lwt.return (Ok None)
    | Ok (Some id) ->
        if id > 0 then Lwt.return (Ok (Some id))
        else Lwt.return (Error Inconsistent_data)

let viewer_may_share (module C : Caqti_lwt.CONNECTION) ~user_id
    ~session_global_admin ~post_id =
  authorize (module C) ~user_id ~session_global_admin ~post_id >>= function
  | Error _ as e -> Lwt.return e
  | Ok None -> Lwt.return (Ok false)
  | Ok (Some _) -> Lwt.return (Ok true)

let load_share_view (module C : Caqti_lwt.CONNECTION) ~user_id
    ~session_global_admin ~post_id =
  authorize (module C) ~user_id ~session_global_admin ~post_id >>= function
  | Error _ as e -> Lwt.return e
  | Ok None -> Lwt.return (Ok None)
  | Ok (Some (title, origin_id, origin_slug, origin_name, manager)) -> (
      collect
        (module C)
        candidates_query
        (origin_id, post_id, max_candidate_destinations)
      >>= function
      | Error _ as e -> Lwt.return e
      | Ok candidate_rows -> (
          collect (module C) placements_query (post_id, max_active_placements)
          >>= function
          | Error _ as e -> Lwt.return e
          | Ok placement_rows -> (
              let decode_placement
                  ( ((id, status_raw), (requested_by, note)),
                    (destination_id, destination_slug, destination_name) ) =
                match Stp.status_of_string status_raw with
                | Some ((Stp.Pending | Stp.Accepted) as status)
                  when Int64.compare id 0L > 0 && destination_id > 0
                       && destination_id <> origin_id
                       && addressable_slug destination_slug
                       && nonblank destination_name
                       && control_safe destination_name
                       && canonical_note ~post_id
                            ~origin_community_id:origin_id
                            ~destination_community_id:destination_id note ->
                    let pending = status = Stp.Pending in
                    let requested_by_viewer = requested_by = Some user_id in
                    Some
                      {
                        placement_id = id;
                        placement_destination_name = destination_name;
                        placement_destination_slug = destination_slug;
                        placement_pending = pending;
                        placement_requested_by_viewer = requested_by_viewer;
                        (* The private note crosses only to its requester or
                           an origin-side manager, and only while the
                           request is pending — the canonical author who
                           did not send it reads nothing. *)
                        placement_note =
                          (if pending && (manager || requested_by_viewer) then
                             note
                           else None);
                      }
                | Some _ | None -> None
              in
              match
                ( collect_all (decode_candidate ~origin_id) candidate_rows,
                  collect_all decode_placement placement_rows )
              with
              | Some candidates, Some placements ->
                  Lwt.return
                    (Ok
                       (Some
                          {
                            view_post_id = post_id;
                            view_post_title = title;
                            view_origin_community_id = origin_id;
                            view_origin_community_name = origin_name;
                            view_origin_community_slug = origin_slug;
                            view_origin_manager = manager;
                            view_candidates = candidates;
                            view_placements = placements;
                          }))
              | _ -> Lwt.return (Error Inconsistent_data))))

let connected_destinations (module C : Caqti_lwt.CONNECTION)
    ~origin_community_id =
  (* The id comes from a record the caller already loaded and authorized, so
     a non-positive value can only be a programming slip; no community can
     match it, and the honest answer is the empty list rather than a new
     error variant handlers would have to route. *)
  if origin_community_id <= 0 then Lwt.return (Ok [])
  else
    collect
      (module C)
      connected_destinations_query
      (origin_community_id, max_candidate_destinations)
    >>= function
    | Error _ as e -> Lwt.return e
    | Ok rows -> (
        match
          collect_all (decode_candidate ~origin_id:origin_community_id) rows
        with
        | Some candidates -> Lwt.return (Ok candidates)
        | None -> Lwt.return (Error Inconsistent_data))
