(* Pure domain representation of a shared-thread placement: one canonical
   durable discussion (a posts row) placed into a second, connected
   community. The canonical thread — title, body, author, comment tree,
   origin community, origin section — is exactly the posts row and is never
   represented, copied, or transitioned here; a placement value carries only
   the destination-local lifecycle. This module never touches a request, a
   session, the database, a clock, or a log; the transactional store
   authorizes, locks, and timestamps durable rows independently.

   Error privacy: every error is a payload-free nullary constructor, so no
   rejected note, length, malformed byte, post id, community id, status, or
   action can travel through the error channel. *)

type status =
  | Pending
  | Accepted
  | Rejected
  | Removed
  | Withdrawn

(* Closed conversion: exactly the five database spellings, nothing else.
   Aliases and capitalization variants belong to no layer — the store reads
   and writes canonical values, and anything different is corrupt data. *)
let status_of_string = function
  | "pending" -> Some Pending
  | "accepted" -> Some Accepted
  | "rejected" -> Some Rejected
  | "removed" -> Some Removed
  | "withdrawn" -> Some Withdrawn
  | _ -> None

let string_of_status = function
  | Pending -> "pending"
  | Accepted -> "accepted"
  | Rejected -> "rejected"
  | Removed -> "removed"
  | Withdrawn -> "withdrawn"

type action =
  | Accept
  | Reject
  | Withdraw
  | Remove

type t = {
  post_id : int;
  origin_community_id : int;
  destination_community_id : int;
  status : status;
  request_note : string option;
}

type error =
  | Invalid_post_id
  | Invalid_community_id
  | Same_community
  | Invalid_request_note
  | Invalid_transition

(* The canonical-content tombstone rule, byte-for-byte the labels the three
   durable deletion paths write into posts.content (author soft-delete,
   admin removal, community-moderator removal) and the same closed set the
   comment-creation gate refuses to comment under. Centralized here so the
   placement store cannot drift from the comment gate's semantics: a thread
   whose canonical content has been tombstoned is not shareable and not
   acceptable. NULL content is a link post, not a tombstone. *)
let post_content_tombstoned = function
  | Some "[deleted]" | Some "[removed by admin]"
  | Some "[removed by moderator]" ->
      true
  | Some _ | None -> false

(* The same cap the sibling relation domains and the durable CHECK use:
   private workflow text has no reason to outgrow the longest user-visible
   free-text field. *)
let max_note_scalars = 2000

(* --- Byte-level helpers (same policy as Community_connections) ----------- *)

(* Deliberately a second copy of the Community_connections canonicalizer
   rather than a dependency on it: the two domains are independent, their
   error types are distinct, and the connections module keeps these helpers
   private by design. *)

(* Outer trim over ASCII whitespace only: space, tab, CR, LF, FF, VT. No
   Unicode normalization, no internal rewriting — the canonical note must
   stay byte-identical to what the requester typed between the trimmed
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

let create_pending ~post_id ~origin_community_id ~destination_community_id
    ~request_note =
  if post_id <= 0 then Error Invalid_post_id
  else if origin_community_id <= 0 || destination_community_id <= 0 then
    Error Invalid_community_id
  else if origin_community_id = destination_community_id then
    (* Distinct from Invalid_community_id: both ids are well-formed, the
       pair is not — the thread already lives in its origin community. The
       durable CHECK mirrors this exactly. *)
    Error Same_community
  else
    match validate_note request_note with
    | Error _ as e -> e
    | Ok note ->
        Ok
          {
            post_id;
            origin_community_id;
            destination_community_id;
            status = Pending;
            request_note = note;
          }

let post_id t = t.post_id

let origin_community_id t = t.origin_community_id

let destination_community_id t = t.destination_community_id

let involves t ~community_id =
  t.origin_community_id = community_id
  || t.destination_community_id = community_id

let status t = t.status

let request_note t = t.request_note

(* The lifecycle is a short tree: a pending request is closed exactly once —
   reviewed by the destination (accepted or rejected) or withdrawn by the
   origin side — and an accepted placement can later be removed by either
   side. Rejected, Removed, and Withdrawn are all terminal — a later attempt
   is a new row and a new create_pending value, never a reopened historical
   one. Withdrawal is deliberately not Remove on a pending row: the two
   cancellations answer different questions in history and the durable shape
   CHECK keeps them structurally distinct. All ids and the note are carried
   through unchanged. *)
let apply t action =
  match (t.status, action) with
  | Pending, Accept -> Ok { t with status = Accepted }
  | Pending, Reject -> Ok { t with status = Rejected }
  | Pending, Withdraw -> Ok { t with status = Withdrawn }
  | Accepted, Remove -> Ok { t with status = Removed }
  | Pending, Remove
  | Accepted, (Accept | Reject | Withdraw)
  | Rejected, (Accept | Reject | Withdraw | Remove)
  | Removed, (Accept | Reject | Withdraw | Remove)
  | Withdrawn, (Accept | Reject | Withdraw | Remove) ->
      Error Invalid_transition
