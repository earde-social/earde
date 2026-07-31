(** Pure domain representation of a mutual connection between two Earde
    communities: the requesting community's pending request, the accepted
    mutual connection, and the rejected/removed history behind it. No SQL,
    HTTP, rendering, session access, time handling, or logging happens here.

    One generic connection. There is deliberately no relationship kind,
    tier, weight, or direction-of-meaning: [requester_community_id] and
    [recipient_community_id] record only who asked and who reviewed, and an
    accepted connection is symmetric — {!unordered_pair}, {!involves}, and
    {!counterpart} are the vocabulary reading surfaces should use, so no
    caller has to re-derive symmetry from the two raw ids.

    Authorization boundary: a successfully created or transitioned value
    proves only that the two community ids are positive and distinct, the
    note is canonical and valid, the status is one of the closed values, and
    the logical transition is allowed. It proves nothing about the world —
    not that either community exists, is a network community, is published,
    is visible to anyone, that the current user moderates either side, that
    no other active connection exists between them, that the durable row is
    still in the expected status, or that the transition won a concurrent
    race. Stores must authorize and lock durable rows independently; no
    authorization booleans or roles live inside {!t}.

    Timestamps are a store concern: the pure domain decides only the logical
    status transition, and the transactional store sets [reviewed_at] and
    [removed_at] at its own transaction time.

    Error privacy: every error is payload-free — no rejected note, length,
    malformed byte, community id, status, or action can leave through the
    error channel, and no [string_of_error] or printer exists. *)

type status =
  | Pending
  | Accepted
  | Rejected
  | Removed

val status_of_string : string -> status option
(** Accepts exactly the four canonical database spellings — [pending],
    [accepted], [rejected], [removed] — and nothing else: no aliases,
    capitalization variants, padding, or abbreviations. *)

val string_of_status : status -> string
(** The exact database value; round-trips with {!status_of_string}. *)

type action =
  | Accept
  | Reject
  | Remove

type t
(** A connection-domain value: the two community ids, a closed status, and
    the canonical optional private request note. Deliberately abstract, with
    no serializer, printer, row id, or timestamp; only the accessors below
    are exposed. *)

type error =
  | Invalid_community_id
  | Self_connection
  | Invalid_request_note
  | Invalid_transition

val create_pending :
  requester_community_id:int ->
  recipient_community_id:int ->
  request_note:string option ->
  (t, error) result
(** One community requesting a mutual connection with another. On success
    the status is {!Pending} and the note is the validated canonical value.

    A non-positive community id on either side is [Invalid_community_id];
    two equal ids are [Self_connection] — a community can never connect to
    itself, and the durable unordered-pair uniqueness has no meaning for
    such a row.

    Note canonicalization, in order: CRLF then lone CR normalize to LF; the
    result is outer-trimmed over ASCII whitespace only (space, tab, CR, LF,
    FF, VT); an empty result collapses to [None]. A remaining non-empty note
    must be valid UTF-8, at most 2,000 Unicode scalar values (counted per
    scalar, never per byte), with LF and horizontal tab as the only
    permitted control bytes — no NUL, no other ASCII controls, no DEL.
    Internal spacing, line feeds, tabs, Unicode, and punctuation are
    preserved byte-for-byte; nothing is truncated, Unicode-normalized, or
    parsed as Markdown/HTML. Rendering layers must still HTML-escape the
    value. Any invalid non-empty note returns [Error Invalid_request_note].

    Success does not prove either community exists, that the requester may
    submit on behalf of the requesting community, or that the pair has no
    active connection — those checks belong to the transactional store. *)

val requester_community_id : t -> int

val recipient_community_id : t -> int

val unordered_pair : t -> int * int
(** The pair in ascending id order — the same normalization the durable
    active-pair uniqueness uses, so a value and its mirror image share one
    identity. Direction survives only in the requester/recipient
    accessors. *)

val involves : t -> community_id:int -> bool
(** Whether the community is one of the two sides, in either direction. *)

val counterpart : t -> community_id:int -> int option
(** The other side of the connection as seen from [community_id], or [None]
    when that community is not part of this connection. *)

val status : t -> status

val request_note : t -> string option
(** The canonical note: LF line endings, outer-trimmed, never [Some ""].
    Carried unchanged through every lifecycle transition as private
    history. *)

val apply : t -> action -> (t, error) result
(** The closed lifecycle. Permits exactly:

    - {!Pending} + {!Accept} → {!Accepted}
    - {!Pending} + {!Reject} → {!Rejected}
    - {!Accepted} + {!Remove} → {!Removed}

    Every other status/action pair returns [Error Invalid_transition].
    {!Rejected} and {!Removed} are terminal: no reopening, retry,
    resubmission, cancellation, suspension, expiration, replacement, or
    reactivation. A later request after a rejection or a removal is a new
    durable row and a new {!create_pending} value — from either direction —
    never a reopened historical one. Both community ids and the canonical
    note are preserved unchanged. *)
