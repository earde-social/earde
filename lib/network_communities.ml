(* Pure lifecycle rules for GitHub-provisioned network communities. No IO —
   see the .mli for the invariants each function encodes. *)

type publication_mode =
  | Public
  | Unlisted

(* Exact match only: settings values come from our own forms, so anything
   off-enum (padding, case drift) is a bug or tampering — never coerced. *)
let publication_mode_of_string = function
  | "public" -> Ok Public
  | "unlisted" -> Ok Unlisted
  | s -> Error (Printf.sprintf "unknown publication mode: %S" s)

let string_of_publication_mode = function
  | Public -> "public"
  | Unlisted -> "unlisted"

type publication_configuration = {
  visibility : Community_types.community_visibility;
  indexable : bool;
  discoverable : bool;
  onboarding_state : Community_types.community_onboarding_state;
}

let configuration_for_publication = function
  | Public ->
      { visibility = Community_types.Community_public;
        indexable = true;
        discoverable = true;
        onboarding_state = Community_types.Community_published }
  | Unlisted ->
      (* Unlisted is not privacy: still public by direct URL, just kept out of
         indexing and discovery. *)
      { visibility = Community_types.Community_public;
        indexable = false;
        discoverable = false;
        onboarding_state = Community_types.Community_published }

type publication_error =
  | Not_a_network_community
  | Community_already_published

let string_of_publication_error = function
  | Not_a_network_community -> "not a network community"
  | Community_already_published -> "community already published"

let publish ~is_network_community ~onboarding_state mode =
  if not is_network_community then Error Not_a_network_community
  else
    match (onboarding_state : Community_types.community_onboarding_state) with
    | Community_types.Community_published -> Error Community_already_published
    | Community_types.Community_draft -> Ok (configuration_for_publication mode)

let visibility_change_allowed ~is_network_community ~onboarding_state
    ~requested_visibility =
  if not is_network_community then true
  else
    match (onboarding_state : Community_types.community_onboarding_state) with
    | Community_types.Community_draft -> true
    | Community_types.Community_published -> (
        (* A published network community cannot become fully private. *)
        match (requested_visibility : Community_types.community_visibility) with
        | Community_types.Community_public -> true
        | Community_types.Community_private -> false)

let lifecycle_state_valid ~is_network_community ~onboarding_state ~visibility
    ~indexable ~discoverable =
  if not is_network_community then true
  else
    match (onboarding_state : Community_types.community_onboarding_state) with
    | Community_types.Community_draft ->
        (* A draft must not leak through indexing or discovery. *)
        visibility = Community_types.Community_private
        && (not indexable) && not discoverable
    | Community_types.Community_published ->
        (* Only the two published shapes exist: fully listed or fully
           unlisted. Mixed indexable/discoverable combinations are invalid. *)
        visibility = Community_types.Community_public && indexable = discoverable
