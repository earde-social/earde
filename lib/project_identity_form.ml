(* Pure strict parser for the future project-identity form (POST /projects).
   Fields arrive already decoded by the HTTP/form layer; this module never
   touches a request, a session, or the database, and never logs form values.

   Layering is load-bearing: this parser validates only the field grammar and
   the scalar identifiers (draft id, kind, primary snapshot id). Text fields
   pass through byte-exact — trimming, UTF-8 validity, lengths, URL structure,
   reserved slugs, and empty-to-None collapsing all belong exclusively to
   Project_identity.create, and are deliberately not duplicated here.

   Anti-oracle: every rejection is the same payload-free Invalid_form, so a
   probing client cannot learn which field failed, whether a field was
   duplicated, or whether an id overflowed int64. *)

type t = {
  draft_id : int64;
  kind : Project_identity.kind;
  name : string;
  slug : string;
  description : string;
  website_url : string;
  primary_snapshot_id : int64 option;
}

type error = Invalid_form

let is_ascii_digit c = c >= '0' && c <= '9'

(* Strict positive decimal int64, identical to the repository-selection
   parser's grammar. The digits-only pre-check rejects everything
   Int64.of_string would tolerate beyond plain decimal — signs, whitespace,
   0x/0o/0b prefixes, '_' separators — leaving of_string_opt to decide exact
   int64 representability (overflow → None). Never converted through OCaml
   int, whose range is platform-dependent. Leading zeroes are fine: the
   parsed value, not the spelling, must be positive. *)
let parse_positive_int64 raw =
  if String.length raw = 0 || not (String.for_all is_ascii_digit raw) then None
  else
    match Int64.of_string_opt raw with
    | Some value when Int64.compare value 0L > 0 -> Some value
    | Some _ | None -> None

(* Accumulator while walking the field list: None = not seen yet, so a second
   occurrence of any recognized field is detectable. The primary field nests
   an option because "seen with an explicitly blank value" (Some None) and
   "not seen" (None) are different submissions — the blank spelling is the
   only way to express no primary repository. *)
type partial = {
  p_draft : int64 option;
  p_kind : Project_identity.kind option;
  p_name : string option;
  p_slug : string option;
  p_description : string option;
  p_website : string option;
  p_primary : int64 option option;
}

let empty_partial =
  {
    p_draft = None;
    p_kind = None;
    p_name = None;
    p_slug = None;
    p_description = None;
    p_website = None;
    p_primary = None;
  }

(* Field names match byte-exactly: no case folding, no trimming. The
   installed Dream.form removes dream.csrf before application parsing, so a
   dream.csrf field reaching this parser is an unknown field like any
   other — recognizing or silently filtering it here would hide a wiring
   bug. *)
let of_fields fields =
  let rec walk fields acc =
    match fields with
    | [] -> (
        match acc with
        | {
         p_draft = Some draft_id;
         p_kind = Some kind;
         p_name = Some name;
         p_slug = Some slug;
         p_description = Some description;
         p_website = Some website_url;
         p_primary = Some primary_snapshot_id;
        } ->
            Ok
              {
                draft_id;
                kind;
                name;
                slug;
                description;
                website_url;
                primary_snapshot_id;
              }
        | _ -> Error Invalid_form)
    | ("draft_id", raw) :: rest -> (
        match (acc.p_draft, parse_positive_int64 raw) with
        | None, Some id -> walk rest { acc with p_draft = Some id }
        | Some _, _ | _, None -> Error Invalid_form)
    | ("kind", raw) :: rest -> (
        (* Exactly the canonical database spellings the closed conversion
           accepts — no aliases or normalization on top of it. *)
        match (acc.p_kind, Project_identity.kind_of_string raw) with
        | None, Some kind -> walk rest { acc with p_kind = Some kind }
        | Some _, _ | _, None -> Error Invalid_form)
    | ("name", raw) :: rest -> (
        match acc.p_name with
        | None -> walk rest { acc with p_name = Some raw }
        | Some _ -> Error Invalid_form)
    | ("slug", raw) :: rest -> (
        match acc.p_slug with
        | None -> walk rest { acc with p_slug = Some raw }
        | Some _ -> Error Invalid_form)
    | ("description", raw) :: rest -> (
        match acc.p_description with
        | None -> walk rest { acc with p_description = Some raw }
        | Some _ -> Error Invalid_form)
    | ("website_url", raw) :: rest -> (
        match acc.p_website with
        | None -> walk rest { acc with p_website = Some raw }
        | Some _ -> Error Invalid_form)
    | ("primary_snapshot_id", raw) :: rest -> (
        match acc.p_primary with
        | Some _ -> Error Invalid_form
        | None -> (
            if raw = "" then walk rest { acc with p_primary = Some None }
            else
              match parse_positive_int64 raw with
              | Some id -> walk rest { acc with p_primary = Some (Some id) }
              | None -> Error Invalid_form))
    | (_, _) :: _ -> Error Invalid_form
  in
  walk fields empty_partial

let draft_id t = t.draft_id
let kind t = t.kind
let name t = t.name
let slug t = t.slug
let description t = t.description
let website_url t = t.website_url
let primary_snapshot_id t = t.primary_snapshot_id

(* The selected snapshot ids are call context, never parser state: only the
   later owner-authorized handler knows the current server-side selection,
   and the finalization store still re-reads it under the draft lock. Empty
   text fields travel as Some "" so Project_identity canonicalizes them to
   None itself; domain errors pass through untouched. *)
let create_identity t ~selected_snapshot_ids =
  Project_identity.create ~kind:t.kind ~name:t.name ~slug:t.slug
    ~description:(Some t.description) ~website_url:(Some t.website_url)
    ~selected_snapshot_ids ~primary_snapshot_id:t.primary_snapshot_id
