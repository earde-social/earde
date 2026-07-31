(* Pure domain representation of a mutual connection between two Earde
   communities, shared by the pending request, the accepted connection, and
   the rejected/removed history. This module never touches a request, a
   session, the database, a clock, or a log; the transactional store
   authorizes, locks, and timestamps durable rows independently.

   One generic connection — no relationship kind. The requester/recipient
   split is provenance (who asked, who reviewed), not meaning: symmetry is
   expressed once here, in unordered_pair/involves/counterpart, so no
   reading surface re-derives it.

   Error privacy: every error is a payload-free nullary constructor, so no
   rejected note, length, malformed byte, community id, status, or action
   can travel through the error channel. *)

type status =
  | Pending
  | Accepted
  | Rejected
  | Removed

(* Closed conversion: exactly the four database spellings, nothing else.
   Aliases and capitalization variants belong to no layer — the store reads
   and writes canonical values, and anything different is corrupt data. *)
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

type action =
  | Accept
  | Reject
  | Remove

type t = {
  requester_community_id : int;
  recipient_community_id : int;
  status : status;
  request_note : string option;
}

type error =
  | Invalid_community_id
  | Self_connection
  | Invalid_request_note
  | Invalid_transition

(* The same cap the sibling relation domain and the durable CHECK use:
   private workflow text has no reason to outgrow the longest user-visible
   free-text field. *)
let max_note_scalars = 2000

(* --- Byte-level helpers (same policy as Project_home_relation) ----------- *)

(* Outer trim over ASCII whitespace only: space, tab, CR, LF, FF, VT. No
   Unicode normalization, no internal rewriting — the canonical note must
   stay byte-identical to what the moderator typed between the trimmed
   edges. *)
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
   convention regardless of the submitting platform. Runs on raw bytes
   before any UTF-8 check — it only ever touches the ASCII CR byte, which
   cannot appear inside a multi-byte sequence. *)
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

(* UTF-8 validity and counting come from the OCaml standard library:
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

let create_pending ~requester_community_id ~recipient_community_id
    ~request_note =
  if requester_community_id <= 0 || recipient_community_id <= 0 then
    Error Invalid_community_id
  else if requester_community_id = recipient_community_id then
    (* Distinct from Invalid_community_id: both ids are well-formed, the
       pair is not. The durable CHECK mirrors this exactly. *)
    Error Self_connection
  else
    match validate_note request_note with
    | Error _ as e -> e
    | Ok note ->
        Ok
          {
            requester_community_id;
            recipient_community_id;
            status = Pending;
            request_note = note;
          }

let requester_community_id t = t.requester_community_id

let recipient_community_id t = t.recipient_community_id

(* Ascending order — the same normalization the durable partial unique index
   applies through LEAST/GREATEST, so a connection and its mirror image
   share one identity here and in the database. *)
let unordered_pair t =
  if t.requester_community_id <= t.recipient_community_id then
    (t.requester_community_id, t.recipient_community_id)
  else (t.recipient_community_id, t.requester_community_id)

let involves t ~community_id =
  t.requester_community_id = community_id
  || t.recipient_community_id = community_id

let counterpart t ~community_id =
  if t.requester_community_id = community_id then
    Some t.recipient_community_id
  else if t.recipient_community_id = community_id then
    Some t.requester_community_id
  else None

let status t = t.status

let request_note t = t.request_note

(* The lifecycle is a straight line: a pending request is reviewed once
   (accepted or rejected), and an accepted connection can later be removed
   by either side. Rejected and Removed are terminal — a later attempt is a
   new row and a new create_pending value, in either direction, never a
   reopened historical one. Both ids and the note are carried through
   unchanged. *)
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
