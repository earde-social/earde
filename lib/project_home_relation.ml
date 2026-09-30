(* Pure domain representation of a project↔community home relation for the
   GitHub-pivot flow. Shared by pending requests to adopt an existing
   community, the immediately accepted relation written while provisioning a
   dedicated community, and the later accept/reject/remove lifecycle. This
   module never touches a request, a session, the database, a clock, or a
   log; future transactional stores authorize, lock, and timestamp durable
   rows independently.

   Error privacy: every error is a payload-free nullary constructor, so no
   rejected note, length, malformed byte, status, or action can travel
   through the error channel. *)

type status = Pending | Accepted | Rejected | Removed

(* Closed conversion: exactly the four database spellings, nothing else.
   Aliases and capitalization variants belong to no layer — stores read and
   write canonical values, and anything different is corrupt data. *)
let status_of_string = function
  | "pending" -> Some Pending
  | "accepted" -> Some Accepted
  | "rejected" -> Some Rejected
  | "removed" -> Some Removed
  | _ -> None

let string_of_status = function
  | Pending -> "pending"
  | Accepted -> "accepted"
  | Rejected -> "rejected"
  | Removed -> "removed"

type action = Accept | Reject | Remove
type t = { status : status; request_note : string option }
type error = Invalid_request_note | Invalid_transition

(* Mirrors the description cap in Project_identity: private workflow text has
   no reason to outgrow the longest user-visible free-text field. *)
let max_note_scalars = 2000

(* --- Byte-level helpers (same policy as Project_identity) ----------------- *)

(* Outer trim over ASCII whitespace only: space, tab, CR, LF, FF, VT. No
   Unicode normalization, no internal rewriting — the canonical note must
   stay byte-identical to what the steward typed between the trimmed edges. *)
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

(* UTF-8 validity and counting come from the OCaml standard library (>= 4.14):
   String.is_valid_utf_8 rejects malformed continuation bytes, truncated
   sequences, overlong encodings, UTF-16 surrogates, and code points above
   U+10FFFF. Counting matches PostgreSQL char_length: one per Unicode scalar
   value, never per byte. Only ever called after validity is established, so
   every decode is exact. *)
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

(* Multi-line field: LF and tab survive as content; every other control byte
   and DEL rejects. Absent, empty, and whitespace-only all collapse to None —
   the store has no use for an empty note row. Nothing is truncated,
   Markdown/HTML-parsed, or Unicode-normalized; rendering layers must still
   HTML-escape the value. *)
let validate_note = function
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
            || utf8_scalar_count text > max_note_scalars
          then Error Invalid_request_note
          else Ok (Some text))

let create_pending ~request_note =
  match validate_note request_note with
  | Ok note -> Ok { status = Pending; request_note = note }
  | Error _ as e -> e

(* A dedicated-community home is born accepted with no note: no moderator
   review or pending request ever took place, and modeling it as
   Pending-then-Accept would fabricate a review that never happened. *)
let create_provisioned_home () = { status = Accepted; request_note = None }
let status t = t.status
let request_note t = t.request_note

(* The MVP lifecycle is a straight line: a pending request is reviewed once
   (accepted or rejected), and an accepted home can later be removed.
   Rejected and Removed are terminal — a later attempt is a new row and a new
   create_pending value, never a reopened historical one. The note is carried
   through unchanged as private history. *)
let apply t action =
  match (t.status, action) with
  | Pending, Accept -> Ok { t with status = Accepted }
  | Pending, Reject -> Ok { t with status = Rejected }
  | Accepted, Remove -> Ok { t with status = Removed }
  | Pending, Remove
  | Accepted, (Accept | Reject)
  | Rejected, (Accept | Reject | Remove)
  | Removed, (Accept | Reject | Remove) ->
      Error Invalid_transition
