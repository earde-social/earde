(* Pure parser for the future POST /projects/new/repositories form. Fields
   arrive already decoded by the HTTP/form layer; this module never touches a
   request, a session, or the database, and never logs form values.

   Anti-oracle: every rejection is the same payload-free Invalid_form, so a
   probing client cannot learn which field failed, whether an id was
   duplicated, or whether a value overflowed int64. *)

module Int64_set = Set.Make (Int64)

type t = { draft_id : int64; selected_snapshot_ids : int64 list }

type error = Invalid_form

(* Bounded by the snapshot-store cap: a verified draft never holds more than
   2,000 repositories, so a larger submission is malformed, not large. *)
let max_repository_fields = 2000

let is_ascii_digit c = c >= '0' && c <= '9'

(* Strict positive decimal int64. The digits-only pre-check rejects
   everything Int64.of_string would tolerate beyond plain decimal — signs,
   whitespace, 0x/0o/0b prefixes, '_' separators — leaving of_string_opt to
   decide exact int64 representability (overflow → None). Never converted
   through OCaml int, whose range is platform-dependent. Leading zeroes are
   fine: the parsed value, not the spelling, must be positive. *)
let parse_positive_int64 raw =
  if String.length raw = 0 || not (String.for_all is_ascii_digit raw) then None
  else
    match Int64.of_string_opt raw with
    | Some value when Int64.compare value 0L > 0 -> Some value
    | Some _ | None -> None

let of_fields fields =
  let rec walk fields draft seen repositories_rev count =
    match fields with
    | [] -> (
        match draft with
        | Some draft_id ->
            Ok { draft_id; selected_snapshot_ids = List.rev repositories_rev }
        | None -> Error Invalid_form)
    | ("draft_id", raw) :: rest -> (
        match (draft, parse_positive_int64 raw) with
        | None, Some id -> walk rest (Some id) seen repositories_rev count
        | Some _, _ | _, None -> Error Invalid_form)
    | ("repository", raw) :: rest -> (
        match parse_positive_int64 raw with
        | Some id
          when count < max_repository_fields && not (Int64_set.mem id seen) ->
            walk rest draft (Int64_set.add id seen) (id :: repositories_rev)
              (count + 1)
        | Some _ | None -> Error Invalid_form)
    | (_, _) :: _ -> Error Invalid_form
  in
  walk fields None Int64_set.empty [] 0

let draft_id t = t.draft_id

let selected_snapshot_ids t = t.selected_snapshot_ids
