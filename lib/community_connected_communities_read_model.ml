(* The publicly visible accepted connections of one community. Read-only:
   own SQL, no locks, no writes, no network, no logging.

   The three eligibility booleans are never re-expressed here. Both queries
   return the raw durable facts — visibility, onboarding state, discoverable
   — and the single pure predicate Community_connections.connection_eligible
   decides, exactly as the creation path does, so the public reading and the
   creation rule cannot drift apart. Filtering in OCaml rather than SQL is
   what makes that possible.

   The viewed community is resolved and judged first: an ineligible one
   publishes nothing, and the counterpart query is never issued for it. See
   the .mli for the full contract. *)

open Lwt.Infix

module Cc = Community_connections

type connected_community = { name : string; slug : string }

type error =
  | Invalid_community_slug
  | Community_unavailable
  | Inconsistent_data
  | Storage_error

let community_name { name; _ } = name
let community_slug { slug; _ } = slug

(* The same single-path-segment grammar the connection surfaces use: a route
   value is never trimmed, lowercased, percent-decoded, or repaired, and a
   slug that fails this can neither be looked up nor become a link. *)
let valid_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* A display name must be non-blank and free of control bytes; the template
   escapes it, but a name carrying C0/DEL is durable corruption rather than
   something to render. *)
let valid_name value =
  String.length value > 0
  && String.exists (fun c -> c <> ' ' && c <> '\t' && c <> '\n' && c <> '\r')
       value
  && String.for_all
       (fun byte -> Char.code byte >= 0x20 && Char.code byte <> 0x7f)
       value

(* The viewed community's identity and its three eligibility facts. Its own
   visibility is deliberately not a filter in SQL: a community that exists but
   is ineligible must be distinguishable from one that no longer exists, so
   the caller's Community_unavailable stays about absence only. *)
let viewed_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.string
   ->? Caqti_type.(t2 (t2 int string) (t2 (t2 string string) bool)))
  "SELECT id, slug, visibility, onboarding_state, discoverable \
   FROM communities \
   WHERE slug = $1"

(* Every accepted counterpart of one community, from either side. The CASE is
   what makes the read symmetric: the same durable row yields the other
   community whichever side asked. Ordering is deterministic and carries no
   signal about the connection — not its age, not its direction. *)
let counterparts_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->* Caqti_type.(t2 (t2 int string) (t2 (t2 string string) (t2 string bool))))
  "SELECT other.id, other.slug, other.name, other.visibility, \
          other.onboarding_state, other.discoverable \
   FROM community_connections cc \
   JOIN communities other \
     ON other.id = CASE WHEN cc.requester_community_id = $1 \
                        THEN cc.recipient_community_id \
                        ELSE cc.requester_community_id END \
   WHERE cc.status = 'accepted' \
     AND (cc.requester_community_id = $1 OR cc.recipient_community_id = $1) \
   ORDER BY lower(other.name), other.slug, other.id"

(* One durable row's eligibility, through the single shared predicate. An
   off-enum stored value is corruption, never a quietly ineligible row. *)
let eligibility ~visibility_raw ~onboarding_raw ~discoverable =
  match
    ( Community_types.community_visibility_of_string visibility_raw,
      Community_types.community_onboarding_state_of_string onboarding_raw )
  with
  | Some visibility, Ok onboarding_state ->
      Some (Cc.connection_eligible ~visibility ~onboarding_state ~discoverable)
  | _ -> None

let load_for_community (module C : Caqti_lwt.CONNECTION) ~community_slug =
  if not (valid_slug community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    C.find_opt viewed_community_query community_slug >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None -> Lwt.return (Error Community_unavailable)
    | Ok
        (Some
          ((viewed_id, stored_slug), ((visibility_raw, onboarding_raw), discoverable)))
      -> (
        (* The stored slug must be byte-identical to the supplied one: a
           lookup that matched on anything else is corruption, not a hit. *)
        if viewed_id <= 0 || not (String.equal stored_slug community_slug) then
          Lwt.return (Error Inconsistent_data)
        else
          match eligibility ~visibility_raw ~onboarding_raw ~discoverable with
          | None -> Lwt.return (Error Inconsistent_data)
          | Some false ->
              (* Not an error and not a deletion: this community simply
                 publishes no connections while it is ineligible. Its
                 authorized management surface is unaffected. *)
              Lwt.return (Ok [])
          | Some true -> (
              C.collect_list counterparts_query viewed_id >>= function
              | Error _ -> Lwt.return (Error Storage_error)
              | Ok rows ->
                  (* Structural validation first, over every row, so a corrupt
                     counterpart fails the whole call rather than silently
                     shortening a public list. Only then does eligibility
                     filter. *)
                  let rec collect seen acc = function
                    | [] -> Ok (List.rev acc)
                    | ((other_id, other_slug), ((other_name, vis_raw), (onb_raw, disc)))
                      :: rest ->
                        if
                          other_id <= 0 || other_id = viewed_id
                          || List.mem other_id seen
                          || not (valid_slug other_slug)
                          || not (valid_name other_name)
                        then Error Inconsistent_data
                        else (
                          match
                            eligibility ~visibility_raw:vis_raw
                              ~onboarding_raw:onb_raw ~discoverable:disc
                          with
                          | None -> Error Inconsistent_data
                          | Some eligible ->
                              let acc =
                                if eligible then
                                  { name = other_name; slug = other_slug } :: acc
                                else acc
                              in
                              collect (other_id :: seen) acc rest)
                  in
                  Lwt.return (collect [] [] rows)))
