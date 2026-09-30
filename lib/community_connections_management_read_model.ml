(* The authorized community-connections management view and the target
   search behind it. Authorization is inside the SQL (a current top_mod row
   of the named community, or a session admin claim still backed by a durable
   users.is_admin), so an unauthorized caller and a missing community produce
   the same Ok None. Read-only: no locks, no writes, no logging.

   The one eligibility rule lives in Community_connections. The search SQL
   carries the same predicate so the result bound applies after exclusion,
   and every row it returns is re-checked through the pure predicate, which
   remains the authority. See the .mli for the full contract. *)

open Lwt.Infix
module Cc = Community_connections

type counterpart = {
  counterpart_id : int;
  counterpart_name : string;
  counterpart_slug : string;
}

let counterpart_id { counterpart_id; _ } = counterpart_id
let counterpart_name { counterpart_name; _ } = counterpart_name
let counterpart_slug { counterpart_slug; _ } = counterpart_slug

type entry = {
  entry_id : int64;
  entry_counterpart : counterpart;
  entry_note : string option;
}

let entry_id { entry_id; _ } = entry_id
let entry_counterpart { entry_counterpart; _ } = entry_counterpart
let entry_note { entry_note; _ } = entry_note

type view = {
  view_community_id : int;
  view_community_name : string;
  view_community_slug : string;
  view_community_eligible : bool;
  view_accepted : entry list;
  view_incoming : entry list;
  view_outgoing : entry list;
}

let view_community_id { view_community_id; _ } = view_community_id
let view_community_name { view_community_name; _ } = view_community_name
let view_community_slug { view_community_slug; _ } = view_community_slug

let view_community_eligible { view_community_eligible; _ } =
  view_community_eligible

let view_accepted { view_accepted; _ } = view_accepted
let view_incoming { view_incoming; _ } = view_incoming
let view_outgoing { view_outgoing; _ } = view_outgoing

type target = { target_id : int; target_name : string; target_slug : string }

let target_id { target_id; _ } = target_id
let target_name { target_name; _ } = target_name
let target_slug { target_slug; _ } = target_slug

type resolution = Connectable of target | Already_active | Unavailable

type error =
  | Invalid_user_id
  | Invalid_community_id
  | Invalid_community_slug
  | Inconsistent_data
  | Storage_error

let max_search_results = 20

(* The longest a community name may be (120) is the widest a meaningful
   query can be; anything longer cannot match and never reaches SQL. *)
let max_query_length = 120

(* Every community is addressed at /c/:slug, so a usable slug is one
   non-empty URL path segment. Route values are never trimmed, lowercased,
   percent-decoded, or repaired. *)
let addressable_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Display text must be non-blank and free of the byte classes that cannot
   appear in rendered text at all — the same shape the sibling stores
   validate community names with. *)
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

(* Identity, eligibility facts, and authorization in one statement: the
   community is never loaded and checked in OCaml, and the role is never
   queried afterwards. $3 is the caller's session admin state — it only
   enables the durable users.is_admin check beside it, never replaces it. *)
let load_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string int) bool)
  ->? Caqti_type.(t2 (t2 (t2 int string) (t2 string string)) (t2 string bool)))
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
     c.discoverable FROM communities c WHERE c.slug = $1 AND (EXISTS (SELECT 1 \
     FROM community_moderators m WHERE m.community_id = c.id AND m.user_id = \
     $2 AND m.role = 'top_mod') OR ($3 AND EXISTS (SELECT 1 FROM users u WHERE \
     u.id = $2 AND u.is_admin)))"

(* Accepted connections read symmetrically: the counterpart is whichever of
   the two columns is not this community. Ordered by the counterpart's name
   so the section reads as a list of communities, not of workflow rows. *)
let accepted_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 (t2 int64 int) (t2 string string)))
    "SELECT cc.id, other.id, other.slug, other.name FROM community_connections \
     cc JOIN communities other ON other.id = CASE WHEN \
     cc.requester_community_id = $1 THEN cc.recipient_community_id ELSE \
     cc.requester_community_id END WHERE (cc.requester_community_id = $1 OR \
     cc.recipient_community_id = $1) AND cc.status = 'accepted' ORDER BY \
     other.name ASC, cc.id ASC"

(* The review queue: pending requests addressed to this community, in
   arrival order, with the private note. *)
let incoming_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
  ->* Caqti_type.(t2 (t2 int64 int) (t3 string string (option string))))
    "SELECT cc.id, other.id, other.slug, other.name, cc.request_note FROM \
     community_connections cc JOIN communities other ON other.id = \
     cc.requester_community_id WHERE cc.recipient_community_id = $1 AND \
     cc.status = 'pending' ORDER BY cc.created_at ASC, cc.id ASC"

let outgoing_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
  ->* Caqti_type.(t2 (t2 int64 int) (t3 string string (option string))))
    "SELECT cc.id, other.id, other.slug, other.name, cc.request_note FROM \
     community_connections cc JOIN communities other ON other.id = \
     cc.recipient_community_id WHERE cc.requester_community_id = $1 AND \
     cc.status = 'pending' ORDER BY cc.created_at ASC, cc.id ASC"

(* Connectable targets. The eligibility predicate is spelled in SQL so the
   LIMIT applies after exclusion — an ineligible community can never consume
   a result slot and so can never be inferred from a short page — and every
   returned row is re-checked in OCaml through the one pure predicate, which
   stays the authority. The NOT EXISTS drops any community already sharing an
   active (pending or accepted) connection in either direction; rejected and
   removed history deliberately does not exclude anyone. *)
let search_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int string) int)
  ->* Caqti_type.(t2 (t2 (t2 int string) (t2 string string)) (t2 string bool)))
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
     c.discoverable FROM communities c WHERE c.id <> $1 AND c.visibility = \
     'public' AND c.onboarding_state = 'published' AND c.discoverable AND \
     (c.name ILIKE $2 ESCAPE '\\' OR c.slug ILIKE $2 ESCAPE '\\') AND NOT \
     EXISTS ( SELECT 1 FROM community_connections cc WHERE cc.status IN \
     ('pending', 'accepted') AND LEAST(cc.requester_community_id, \
     cc.recipient_community_id) = LEAST(c.id, $1) AND \
     GREATEST(cc.requester_community_id, cc.recipient_community_id) = \
     GREATEST(c.id, $1)) ORDER BY c.name ASC, c.id ASC LIMIT $3"

(* One exact target slug, with the two facts that decide its resolution:
   its eligibility columns and whether the unordered pair already holds an
   active connection. Deliberately not the substring search — an exact slug
   deserves an exact lookup, and the active-pair fact must be reported
   rather than silently excluded. *)
let resolve_target_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string)
  ->? Caqti_type.(
        t2 (t2 (t2 int string) (t2 string string)) (t3 string bool bool)))
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
     c.discoverable, EXISTS (SELECT 1 FROM community_connections cc WHERE \
     cc.status IN ('pending', 'accepted') AND LEAST(cc.requester_community_id, \
     cc.recipient_community_id) = LEAST(c.id, $1) AND \
     GREATEST(cc.requester_community_id, cc.recipient_community_id) = \
     GREATEST(c.id, $1)) FROM communities c WHERE c.slug = $2 AND c.id <> $1"

(* === decoding === *)

let eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable =
  match
    ( Community_types.community_visibility_of_string visibility_raw,
      Community_types.community_onboarding_state_of_string onboarding_raw )
  with
  | Some visibility, Ok onboarding_state ->
      Some (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
  | None, _ | _, Error _ -> None

let decode_counterpart ~id ~slug ~name =
  if id > 0 && addressable_slug slug && nonblank name && control_safe name then
    Some
      { counterpart_id = id; counterpart_name = name; counterpart_slug = slug }
  else None

(* The stored note must round-trip byte-exactly through the pure
   canonicalizer: a durable note the canonicalizer would change is
   corruption, not text to repair on the way out. The two community ids also
   go back through the pure constructor, which re-proves they are positive
   and distinct. *)
let decode_entry ~community_id ~id ~counterpart ~note =
  match
    decode_counterpart ~id:counterpart.counterpart_id
      ~slug:counterpart.counterpart_slug ~name:counterpart.counterpart_name
  with
  | None -> None
  | Some counterpart ->
      let pair_ok =
        match
          Cc.create_pending ~requester_community_id:community_id
            ~recipient_community_id:counterpart.counterpart_id
            ~request_note:note
        with
        | Error _ -> false
        | Ok value -> Cc.request_note value = note
      in
      if positive id && pair_ok then
        Some
          { entry_id = id; entry_counterpart = counterpart; entry_note = note }
      else None

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

(* === reads === *)

(* Every Caqti error is dropped payload-free: error payloads can echo SQL
   parameters. *)
let collect (module C : Caqti_lwt.CONNECTION) query arg =
  C.collect_list query arg >>= function
  | Error _ -> Lwt.return (Error Storage_error)
  | Ok rows -> Lwt.return (Ok rows)

let load_for_manager (module C : Caqti_lwt.CONNECTION) ~user_id
    ~session_global_admin ~community_slug =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (addressable_slug community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    C.find_opt load_community_query
      ((community_slug, user_id), session_global_admin)
    >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    (* Missing community and every unauthorized cause alike. *)
    | Ok None -> Lwt.return (Ok None)
    | Ok
        (Some
           ( ((community_id, stored_slug), (name, visibility_raw)),
             (onboarding_raw, discoverable) )) -> (
        match
          ( community_id > 0
            && String.equal stored_slug community_slug
            && nonblank name && control_safe name,
            eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable )
        with
        | false, _ | _, None -> Lwt.return (Error Inconsistent_data)
        | true, Some eligible -> (
            let accepted_of_row ((id, other_id), (other_slug, other_name)) =
              decode_entry ~community_id ~id
                ~counterpart:
                  {
                    counterpart_id = other_id;
                    counterpart_slug = other_slug;
                    counterpart_name = other_name;
                  }
                  (* An accepted connection is symmetric and authorless: the
                     request note stays behind in the workflow that created
                     it and is never surfaced here. *)
                ~note:None
            in
            let pending_of_row ((id, other_id), (other_slug, other_name, note))
                =
              decode_entry ~community_id ~id
                ~counterpart:
                  {
                    counterpart_id = other_id;
                    counterpart_slug = other_slug;
                    counterpart_name = other_name;
                  }
                ~note
            in
            collect (module C) accepted_query community_id >>= function
            | Error _ as e -> Lwt.return e
            | Ok accepted_rows -> (
                collect (module C) incoming_query community_id >>= function
                | Error _ as e -> Lwt.return e
                | Ok incoming_rows -> (
                    collect (module C) outgoing_query community_id >>= function
                    | Error _ as e -> Lwt.return e
                    | Ok outgoing_rows -> (
                        match
                          ( collect_all accepted_of_row accepted_rows,
                            collect_all pending_of_row incoming_rows,
                            collect_all pending_of_row outgoing_rows )
                        with
                        | Some accepted, Some incoming, Some outgoing ->
                            Lwt.return
                              (Ok
                                 (Some
                                    {
                                      view_community_id = community_id;
                                      view_community_name = name;
                                      view_community_slug = stored_slug;
                                      view_community_eligible = eligible;
                                      view_accepted = accepted;
                                      view_incoming = incoming;
                                      view_outgoing = outgoing;
                                    }))
                        | _ -> Lwt.return (Error Inconsistent_data))))))

(* ILIKE metacharacters in the caller's text are escaped, so no query can
   widen its own pattern (a bare '%' would otherwise match everything). The
   backslash is escaped first, and the SQL says ESCAPE '\' explicitly. *)
let like_pattern query =
  let buffer = Buffer.create (String.length query + 2) in
  Buffer.add_char buffer '%';
  String.iter
    (fun c ->
      (match c with '\\' | '%' | '_' -> Buffer.add_char buffer '\\' | _ -> ());
      Buffer.add_char buffer c)
    query;
  Buffer.add_char buffer '%';
  Buffer.contents buffer

let search_targets (module C : Caqti_lwt.CONNECTION) ~community_id ~query =
  let trimmed = String.trim query in
  if community_id <= 0 then Lwt.return (Error Invalid_community_id)
  else if
    trimmed = ""
    || String.length trimmed > max_query_length
    || not (control_safe trimmed)
  then
    (* No SQL at all: a blank, oversized, or control-bearing query answers
       exactly like one that matched nothing. *)
    Lwt.return (Ok [])
  else
    collect
      (module C)
      search_query
      ((community_id, like_pattern trimmed), max_search_results)
    >>= function
    | Error _ as e -> Lwt.return e
    | Ok rows -> (
        (* The SQL predicate bounded the page; the pure predicate decides
           whether each surviving row is really connectable. A row that
           disagrees can only mean an off-enum durable value. *)
        let decode
            ( ((id, slug), (name, visibility_raw)),
              (onboarding_raw, discoverable) ) =
          match
            eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable
          with
          | Some true
            when id > 0 && id <> community_id && addressable_slug slug
                 && nonblank name && control_safe name ->
              Some { target_id = id; target_name = name; target_slug = slug }
          | Some _ | None -> None
        in
        match collect_all decode rows with
        | Some targets -> Lwt.return (Ok targets)
        | None -> Lwt.return (Error Inconsistent_data))

let resolve_target (module C : Caqti_lwt.CONNECTION) ~community_id ~slug =
  if community_id <= 0 then Lwt.return (Error Invalid_community_id)
  else if not (addressable_slug slug) then Lwt.return (Ok Unavailable)
  else
    C.find_opt resolve_target_query (community_id, slug) >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    (* Missing, or the searching community itself. *)
    | Ok None -> Lwt.return (Ok Unavailable)
    | Ok
        (Some
           ( ((id, stored_slug), (name, visibility_raw)),
             (onboarding_raw, discoverable, active) )) -> (
        match eligible_of_raw ~visibility_raw ~onboarding_raw ~discoverable with
        | None -> Lwt.return (Error Inconsistent_data)
        | Some eligible ->
            if
              not
                (id > 0 && id <> community_id
                && String.equal stored_slug slug
                && nonblank name && control_safe name)
            then Lwt.return (Error Inconsistent_data)
            else if not eligible then
              (* Private, draft and undiscoverable collapse with missing. *)
              Lwt.return (Ok Unavailable)
            else if active then Lwt.return (Ok Already_active)
            else
              Lwt.return
                (Ok
                   (Connectable
                      { target_id = id; target_name = name; target_slug = slug }))
        )
