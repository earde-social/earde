(* Pure strict parser for the future existing-community home-request form
   (POST /projects/:slug/request-home). Fields arrive already decoded by the
   HTTP/form layer; this module never touches a request, a session, or the
   database, and never logs form values.

   Layering is load-bearing: this parser validates only the field grammar
   and the target-community identifier. The note passes through byte-exact —
   trimming, line-ending normalization, UTF-8 validity, control bytes,
   length, and empty-to-None collapsing all belong exclusively to
   Project_home_relation.create_pending, and are deliberately not
   duplicated here. The project slug is route context and must never be a
   form field — a browser-supplied slug would bypass the route-level
   authorization boundary.

   Anti-oracle: every rejection is the same payload-free Invalid_form, so a
   probing client cannot learn which field failed, whether a field was
   duplicated, or whether an id overflowed. *)

type t = {
  target_community_id : int;
  request_note : string;
}

type error = Invalid_form

let is_ascii_digit c = c >= '0' && c <= '9'

(* Strict positive decimal OCaml int, the local community id type. The
   digits-only pre-check rejects everything int_of_string_opt would tolerate
   beyond plain decimal — signs, whitespace, 0x/0o/0b prefixes, '_'
   separators — leaving of_string_opt to decide exact int representability
   (overflow → None). Never parsed through int64 and truncated. Leading
   zeroes are fine: the parsed value, not the spelling, must be positive. *)
let parse_positive_int raw =
  if String.length raw = 0 || not (String.for_all is_ascii_digit raw) then
    None
  else
    match int_of_string_opt raw with
    | Some value when value > 0 -> Some value
    | Some _ | None -> None

(* Field names match byte-exactly: no case folding, no trimming. The
   installed Dream.form removes dream.csrf before application parsing, so a
   dream.csrf field reaching this parser is an unknown field like any other
   — recognizing or silently filtering it here would hide a wiring bug. *)
let of_fields fields =
  let rec walk fields target note =
    match fields with
    | [] -> (
        match (target, note) with
        | Some target_community_id, Some request_note ->
            Ok { target_community_id; request_note }
        | _ -> Error Invalid_form)
    | ("target_community_id", raw) :: rest -> (
        match (target, parse_positive_int raw) with
        | None, Some id -> walk rest (Some id) note
        | Some _, _ | _, None -> Error Invalid_form)
    | ("request_note", raw) :: rest -> (
        match note with
        | None -> walk rest target (Some raw)
        | Some _ -> Error Invalid_form)
    | (_, _) :: _ -> Error Invalid_form
  in
  walk fields None None

let target_community_id t = t.target_community_id

let request_note t = t.request_note

(* The submitted note travels as Some even when blank: the domain
   constructor owns empty-to-None canonicalization, and its errors pass
   through untouched. *)
let create_relation t =
  Project_home_relation.create_pending ~request_note:(Some t.request_note)
