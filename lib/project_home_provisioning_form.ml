(* Pure strict parser for the initial community identity of a dedicated
   project community home (the future POST /projects/:slug/community-home).
   No request, session, database, or logging — see the .mli for the full
   contract.

   Why this parser owns semantics the sibling request-home parser delegates:
   there is no community-identity domain constructor to delegate to. Legacy
   admin community creation canonicalizes a name and slug with String.trim
   and rejects only blanks, and communities.name/slug/description carry no
   database CHECK at all, so "the current production policy" would accept a
   slug containing '/' or whitespace. The values here become a durable
   community the provisioning transaction must not have to repair, so the
   rules mirror Project_identity — the strongest canonical identity policy
   actually in production — with one deliberate strengthening: the slug must
   arrive already canonical rather than being lowercased and trimmed into
   shape, because an aliased spelling silently becoming a different URL is
   exactly the surprise a durable identity must not have.

   Error privacy: rejected values, their lengths, and the failing rule never
   travel; structural failures collapse to one shared constructor. *)

type t = {
  community_name : string;
  community_slug : string;
  community_description : string option;
}

type error =
  | Invalid_form
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description

let ( let* ) = Result.bind

(* Same limits Project_identity enforces for the permanent project identity,
   and the same ones open_source_projects checks durably. *)
let max_name_scalars = 120

let max_slug_bytes = 80

let max_description_scalars = 2000

(* --- Shared byte-level helpers (mirrors Project_identity) ---------------- *)

(* Outer trim over ASCII whitespace only: space, tab, CR, LF, FF, VT. No
   Unicode normalization, no internal rewriting — accepted values stay
   byte-identical to what was typed between the trimmed edges. Legacy
   community creation uses String.trim, which is this set minus VT; the two
   differ only on values whose remaining bytes would then be rejected as
   embedded controls anyway. *)
let is_ascii_whitespace c =
  c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'

let trim_ascii s =
  let n = String.length s in
  let start = ref 0 in
  while !start < n && is_ascii_whitespace s.[!start] do
    incr start
  done;
  let stop = ref n in
  while !stop > !start && is_ascii_whitespace s.[!stop - 1] do
    decr stop
  done;
  String.sub s !start (!stop - !start)

(* UTF-8 validity and counting come from the standard library, already the
   strictest facility installed: String.is_valid_utf_8 rejects malformed
   continuation bytes, truncated sequences, overlong encodings, UTF-16
   surrogates, and code points above U+10FFFF. Counting matches PostgreSQL
   char_length — one per Unicode scalar value, never per byte. Only ever
   called after validity is established, so every decode is exact. *)
let utf8_scalar_count s =
  let n = String.length s in
  let rec go i count =
    if i >= n then count
    else
      let decode = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length decode) (count + 1)
  in
  go 0 0

(* NUL is '\x00' and therefore covered by the control range. *)
let is_ascii_control_or_del c = c < '\x20' || c = '\x7f'

(* --- Name ---------------------------------------------------------------- *)

(* Single-line field: after the outer trim, any remaining control byte —
   including tab, CR, LF, FF, VT — or DEL is an embedded control, not
   spacing, so it rejects. *)
let validate_name raw =
  let name = trim_ascii raw in
  if
    name = ""
    || (not (String.is_valid_utf_8 name))
    || String.exists is_ascii_control_or_del name
    || utf8_scalar_count name > max_name_scalars
  then Error Invalid_community_name
  else Ok name

(* --- Slug ---------------------------------------------------------------- *)

let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

(* ^[a-z0-9]+(-[a-z0-9]+)*$ checked positionally: every byte is [a-z0-9] or a
   hyphen whose neighbours are both [a-z0-9]. ASCII-only by construction, so
   byte length is character length here. *)
let slug_structure_ok s =
  let n = String.length s in
  n >= 1
  && n <= max_slug_bytes
  && s.[0] <> '-'
  && s.[n - 1] <> '-'
  &&
  let rec ok i =
    i >= n
    ||
    if is_slug_char s.[i] then ok (i + 1)
    else s.[i] = '-' && s.[i + 1] <> '-' && ok (i + 1)
  in
  ok 0

(* Deliberately applied to the raw submission: no trim, no lowercasing, no
   repair. A slug that is not already canonical is rejected, never rewritten
   into a different URL than the one that was typed. No value is reserved —
   every community route is /c/:slug with no static sibling segment, so
   current creation reserves nothing to mirror. *)
let validate_slug raw =
  if slug_structure_ok raw then Ok raw else Error Invalid_community_slug

(* --- Description --------------------------------------------------------- *)

(* CRLF and lone CR both become LF, so stored text has one line-ending
   convention regardless of the submitting platform. Runs on raw bytes before
   any UTF-8 check — it only ever touches the ASCII CR byte, which cannot
   appear inside a multi-byte sequence. *)
let normalize_line_endings s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    (if s.[!i] = '\r' then begin
       Buffer.add_char b '\n';
       if !i + 1 < n && s.[!i + 1] = '\n' then incr i
     end
     else Buffer.add_char b s.[!i]);
    incr i
  done;
  Buffer.contents b

(* Multi-line field: LF and tab survive as content; every other control byte
   and DEL rejects. Empty and whitespace-only both collapse to None — a
   community with an empty description row has no use for one. *)
let validate_description raw =
  let text = trim_ascii (normalize_line_endings raw) in
  let is_forbidden_control c =
    is_ascii_control_or_del c && c <> '\n' && c <> '\t'
  in
  match text with
  | "" -> Ok None
  | _ ->
      if
        (not (String.is_valid_utf_8 text))
        || String.exists is_forbidden_control text
        || utf8_scalar_count text > max_description_scalars
      then Error Invalid_community_description
      else Ok (Some text)

(* --- Field grammar -------------------------------------------------------- *)

(* Accumulator while walking the field list: None = not seen yet, so a second
   occurrence of any recognized field is detectable. *)
type partial = {
  p_name : string option;
  p_slug : string option;
  p_description : string option;
}

let empty_partial = { p_name = None; p_slug = None; p_description = None }

(* Field names match byte-exactly: no case folding, no trimming. The
   installed Dream.form removes dream.csrf before application parsing, so a
   dream.csrf field reaching this parser is an unknown field like any other —
   recognizing or silently filtering it here would hide a wiring bug.

   Structure is decided for the whole list before any semantic rule runs, so
   a submission that is both structurally malformed and semantically invalid
   always answers Invalid_form and never names a field. *)
let of_fields fields =
  let rec walk fields acc =
    match fields with
    | [] -> (
        match acc with
        | { p_name = Some name; p_slug = Some slug;
            p_description = Some description } ->
            Ok (name, slug, description)
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
    | (_, _) :: _ -> Error Invalid_form
  in
  let* raw_name, raw_slug, raw_description = walk fields empty_partial in
  let* community_name = validate_name raw_name in
  let* community_slug = validate_slug raw_slug in
  let* community_description = validate_description raw_description in
  Ok { community_name; community_slug; community_description }

let community_name t = t.community_name

let community_slug t = t.community_slug

let community_description t = t.community_description
