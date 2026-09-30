(* Pure, validated project identity for the GitHub-pivot creation flow. The
   future strict form parser feeds this constructor; the future atomic
   finalization store consumes the result. This module never touches a
   request, a session, the database, or a log.

   Error privacy: every error is a payload-free nullary constructor, so no
   rejected name, slug, URL, id, length, or malformed byte can travel through
   the error channel. *)

module Int64_set = Set.Make (Int64)

type kind =
  | Project
  | Organization
  | Ecosystem
  | Foundation
  | Working_group
  | Other

(* Closed conversion: exactly the six database spellings, nothing else.
   Aliases and capitalization variants belong to no layer — the form posts
   canonical values, and anything different is malformed input. *)
let kind_of_string = function
  | "project" -> Some Project
  | "organization" -> Some Organization
  | "ecosystem" -> Some Ecosystem
  | "foundation" -> Some Foundation
  | "working_group" -> Some Working_group
  | "other" -> Some Other
  | _ -> None

let string_of_kind = function
  | Project -> "project"
  | Organization -> "organization"
  | Ecosystem -> "ecosystem"
  | Foundation -> "foundation"
  | Working_group -> "working_group"
  | Other -> "other"

(* The selected snapshot ids are constructor context only — deliberately not
   retained, so a stale copy can never masquerade as the authoritative
   selection the finalization store must re-read under the draft lock. *)
type t = {
  kind : kind;
  name : string;
  slug : string;
  description : string option;
  website_url : string option;
  primary_snapshot_id : int64 option;
}

type error =
  | Invalid_repository_selection
  | No_repositories_selected
  | Invalid_name
  | Invalid_slug
  | Reserved_slug
  | Invalid_description
  | Invalid_website_url
  | Invalid_primary_repository
  | Primary_repository_required

let ( let* ) = Result.bind

(* Mirrors the snapshot-store cap shared with the repository-selection form:
   a verified draft never holds more repositories, so a larger context is
   malformed, not large. *)
let max_selected_snapshots = 2000
let max_name_scalars = 120
let max_slug_bytes = 80
let max_description_scalars = 2000
let max_website_scalars = 2048

(* --- Shared byte-level helpers ------------------------------------------- *)

(* Outer trim over ASCII whitespace only: space, tab, CR, LF, FF, VT. No
   Unicode normalization, no internal rewriting — canonical values must stay
   byte-identical to what the user typed between the trimmed edges. *)
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

(* UTF-8 validity and counting come from the OCaml standard library (>= 4.14),
   already the strictest facility installed: String.is_valid_utf_8 rejects
   malformed continuation bytes, truncated sequences, overlong encodings,
   UTF-16 surrogates, and code points above U+10FFFF. Counting matches
   PostgreSQL char_length: one per Unicode scalar value, never per byte. Only
   ever called after validity is established, so every decode is exact. *)
let utf8_scalar_count s =
  let n = String.length s in
  let rec go i count =
    if i >= n then count
    else
      let decode = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length decode) (count + 1)
  in
  go 0 0

let is_ascii_control_or_del c = c < '\x20' || c = '\x7f'

(* --- Selected-repository context ----------------------------------------- *)

(* Structure first (positivity, uniqueness, cap), then the non-empty rule, so
   a malformed non-empty context and a malformed oversized context collapse
   to the same structural error before emptiness is even considered. The list
   is never sorted, mutated, or retained. *)
let validate_selected_context ids =
  if List.length ids > max_selected_snapshots then
    Error Invalid_repository_selection
  else
    let rec structurally_valid seen = function
      | [] -> true
      | id :: rest ->
          Int64.compare id 0L > 0
          && (not (Int64_set.mem id seen))
          && structurally_valid (Int64_set.add id seen) rest
    in
    if not (structurally_valid Int64_set.empty ids) then
      Error Invalid_repository_selection
    else if ids = [] then Error No_repositories_selected
    else Ok ()

(* --- Name ----------------------------------------------------------------- *)

(* Single-line field: after the outer trim, any remaining control byte —
   including tab, CR, LF, FF, VT — or DEL is an embedded control, not
   spacing, so it rejects. Accepted names keep their exact bytes: no
   lowercasing, no Unicode normalization. *)
let validate_name raw =
  let name = trim_ascii raw in
  if
    name = ""
    || (not (String.is_valid_utf_8 name))
    || String.exists is_ascii_control_or_del name
    || utf8_scalar_count name > max_name_scalars
  then Error Invalid_name
  else Ok name

(* --- Slug ----------------------------------------------------------------- *)

(* The only reserved value is the one exact routing conflict that exists
   today: GET /projects/new (and its /projects/new/* children) would shadow a
   project living at /projects/new. No speculative reservations — widen this
   set only when a new exact static /projects/<value> route lands. *)
let reserved_slugs = [ "new" ]
let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

(* ^[a-z0-9]+(-[a-z0-9]+)*$ checked positionally: every byte is [a-z0-9] or a
   hyphen whose neighbours are both [a-z0-9]. ASCII-only by construction, so
   byte length is character length here. *)
let slug_structure_ok s =
  let n = String.length s in
  n >= 1 && n <= max_slug_bytes
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

let validate_slug raw =
  let slug = String.lowercase_ascii (trim_ascii raw) in
  if not (slug_structure_ok slug) then Error Invalid_slug
  else if List.mem slug reserved_slugs then Error Reserved_slug
  else Ok slug

(* --- Description ---------------------------------------------------------- *)

(* CRLF and lone CR both become LF, so stored text has one line-ending
   convention regardless of the submitting platform. Runs on raw bytes before
   any UTF-8 check — it only ever touches the ASCII CR byte, which cannot
   appear inside a multi-byte sequence. *)
let normalize_line_endings s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '\r' then begin
      Buffer.add_char b '\n';
      if !i + 1 < n && s.[!i + 1] = '\n' then incr i
    end
    else Buffer.add_char b s.[!i];
    incr i
  done;
  Buffer.contents b

(* Multi-line field: LF and tab survive as content; every other control byte
   and DEL rejects. Absent, empty, and whitespace-only all collapse to None —
   the store has no use for an empty description row. *)
let validate_description = function
  | None -> Ok None
  | Some raw -> (
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
          then Error Invalid_description
          else Ok (Some text))

(* --- Website URL ---------------------------------------------------------- *)

(* Byte policy: nothing at or below 0x20 (all ASCII whitespace and controls)
   and no DEL anywhere in a URL. Multi-byte UTF-8 passes — international path
   text is allowed and left to Uri for structure. *)
let is_forbidden_url_byte c = c <= ' ' || c = '\x7f'

(* Structural validation only, via the installed Uri library: absolute,
   http/https (case-insensitive — Uri lowercases the scheme it parses), a
   non-empty host, and no userinfo of any shape (user or user:password).
   Fragments are representable by Uri and accepted. The accepted value is the
   trimmed input byte-for-byte — Uri never re-serializes it, so nothing is
   percent-recoded, reordered, or dropped. *)
let validate_website = function
  | None -> Ok None
  | Some raw -> (
      let url = trim_ascii raw in
      match url with
      | "" -> Ok None
      | _ ->
          if
            (not (String.is_valid_utf_8 url))
            || String.exists is_forbidden_url_byte url
            || utf8_scalar_count url > max_website_scalars
          then Error Invalid_website_url
          else
            let uri = Uri.of_string url in
            let scheme_ok =
              match Uri.scheme uri with
              | Some scheme -> (
                  match String.lowercase_ascii scheme with
                  | "http" | "https" -> true
                  | _ -> false)
              | None -> false
            in
            let host_ok =
              match Uri.host uri with Some host -> host <> "" | None -> false
            in
            if scheme_ok && host_ok && Uri.userinfo uri = None then
              Ok (Some url)
            else Error Invalid_website_url)

(* --- Primary repository --------------------------------------------------- *)

(* A supplied primary must be positive and present in the validated selected
   context (which is duplicate-free by now, so membership means exactly
   once). Kind Project demands an explicit primary — never inferred, even
   from a one-repository selection; the other kinds accept zero or one. The
   finalization store still re-checks membership against the locked draft. *)
let validate_primary ~kind ~selected = function
  | Some id ->
      if Int64.compare id 0L > 0 && List.exists (Int64.equal id) selected then
        Ok (Some id)
      else Error Invalid_primary_repository
  | None -> (
      match kind with
      | Project -> Error Primary_repository_required
      | Organization | Ecosystem | Foundation | Working_group | Other -> Ok None
      )

(* Deterministic order: selected-context structure, non-empty rule, name,
   slug, description, website, primary. The first failure is the whole
   answer — nothing accumulates, nothing leaks. *)
let create ~kind ~name ~slug ~description ~website_url ~selected_snapshot_ids
    ~primary_snapshot_id =
  let* () = validate_selected_context selected_snapshot_ids in
  let* name = validate_name name in
  let* slug = validate_slug slug in
  let* description = validate_description description in
  let* website_url = validate_website website_url in
  let* primary_snapshot_id =
    validate_primary ~kind ~selected:selected_snapshot_ids primary_snapshot_id
  in
  Ok { kind; name; slug; description; website_url; primary_snapshot_id }

let kind t = t.kind
let name t = t.name
let slug t = t.slug
let description t = t.description
let website_url t = t.website_url
let primary_snapshot_id t = t.primary_snapshot_id
