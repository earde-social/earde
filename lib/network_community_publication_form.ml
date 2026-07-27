(* Pure strict parser for the final network-community setup submission (the
   future POST /c/:slug/publish, not registered yet). No request, session,
   database, or logging — see the .mli for the full contract.

   Why this parser delegates the identity half rather than owning it: unlike
   the provisioning form, which had no community-identity domain to delegate
   to and therefore had to establish one, a frozen canonical policy now
   exists and is already durable. Project_home_provisioning_form is what the
   provisioning store persisted, and the scoped communities_network_* CHECKs
   mirror it byte-for-byte. Re-stating those rules here would create a second
   grammar that could drift; instead the three identity fields are handed to
   that parser unchanged and only its abstract result is retained, so this
   module cannot accept an identity the provisioning form would reject, and
   vice versa.

   Error privacy: rejected values, their lengths, and the failing rule never
   travel; structural failures collapse to one shared constructor. *)

type publication_visibility =
  | Public
  | Unlisted

type t = {
  identity : Project_home_provisioning_form.t;
  visibility : publication_visibility;
}

type error =
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description
  | Invalid_publication_visibility

let ( let* ) = Result.bind

(* --- Field grammar --------------------------------------------------------

   Accumulator while walking the field list: None = not seen yet, so a second
   occurrence of any recognized field is detectable. Field names match
   byte-exactly — no case folding, no trimming. The installed Dream.form
   removes dream.csrf before application parsing, so a dream.csrf field
   reaching this parser is an unknown field like any other; recognizing or
   silently filtering it here would hide a wiring bug.

   Structure is decided for the whole list before any semantic rule runs. *)

type partial = {
  p_name : string option;
  p_slug : string option;
  p_description : string option;
  p_visibility : string option;
}

let empty_partial =
  { p_name = None; p_slug = None; p_description = None; p_visibility = None }

let structure fields =
  let rec walk fields acc =
    match fields with
    | [] -> (
        match acc with
        | { p_name = Some name; p_slug = Some slug;
            p_description = Some description; p_visibility = Some visibility } ->
            Ok (name, slug, description, visibility)
        | _ -> Error Invalid_form)
    | ("community_name", raw) :: rest -> (
        match acc.p_name with
        | None -> walk rest { acc with p_name = Some raw }
        | Some _ -> Error Invalid_form)
    | ("community_slug", raw) :: rest -> (
        match acc.p_slug with
        | None -> walk rest { acc with p_slug = Some raw }
        | Some _ -> Error Invalid_form)
    | ("community_description", raw) :: rest -> (
        match acc.p_description with
        | None -> walk rest { acc with p_description = Some raw }
        | Some _ -> Error Invalid_form)
    | ("publication_visibility", raw) :: rest -> (
        match acc.p_visibility with
        | None -> walk rest { acc with p_visibility = Some raw }
        | Some _ -> Error Invalid_form)
    | (_, _) :: _ -> Error Invalid_form
  in
  walk fields empty_partial

(* --- Identity (delegated) -------------------------------------------------

   Exactly the three fields the frozen parser expects, in its own order, with
   the raw submitted bytes. Its Invalid_form is unreachable by construction
   here — the list it receives is built, not forwarded — but is mapped rather
   than assumed away, since a silently different answer would be worse than a
   structural one. *)
let identity_of raw_name raw_slug raw_description =
  match
    Project_home_provisioning_form.of_fields
      [ ("community_name", raw_name);
        ("community_slug", raw_slug);
        ("community_description", raw_description)
      ]
  with
  | Ok identity -> Ok identity
  | Error Project_home_provisioning_form.Invalid_community_name ->
      Error Invalid_community_name
  | Error Project_home_provisioning_form.Invalid_community_slug ->
      Error Invalid_community_slug
  | Error Project_home_provisioning_form.Invalid_community_description ->
      Error Invalid_community_description
  | Error Project_home_provisioning_form.Invalid_form -> Error Invalid_form

(* --- Publication choice ---------------------------------------------------

   The frozen lifecycle grammar decides this: exact match on "public" or
   "unlisted", no trimming and no case folding. Its Error payload is a
   human-readable string and is deliberately dropped — nothing about the
   rejected value may travel. "private" is not special-cased: a network
   community has no fully private published shape, so it is simply an
   unknown value like any other. *)
let visibility_of raw =
  match Network_communities.publication_mode_of_string raw with
  | Ok Network_communities.Public -> Ok Public
  | Ok Network_communities.Unlisted -> Ok Unlisted
  | Error _ -> Error Invalid_publication_visibility

let of_fields fields =
  let* raw_name, raw_slug, raw_description, raw_visibility = structure fields in
  let* identity = identity_of raw_name raw_slug raw_description in
  let* visibility = visibility_of raw_visibility in
  Ok { identity; visibility }

let community_name t = Project_home_provisioning_form.community_name t.identity

let community_slug t = Project_home_provisioning_form.community_slug t.identity

let community_description t =
  Project_home_provisioning_form.community_description t.identity

let publication_visibility t = t.visibility
