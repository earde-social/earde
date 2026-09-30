(** Pure domain representation of a shared-thread placement: one canonical
    durable discussion (a [posts] row) placed into a second, connected
    community. No SQL, HTTP, rendering, session access, time handling, or
    logging happens here.

    The canonical thread is not modeled by this module. One [posts] row remains
    the single discussion — one title, one body, one author, one comment tree —
    and its [posts.community_id] / [posts.section_id] remain the immutable
    origin placement. A value here describes only the destination-local
    lifecycle: which community was asked to receive the thread and where that
    request stands. The destination forum section is a store concern (chosen by
    the destination reviewer at accept time) and deliberately does not appear in
    {!t}.

    Authorization boundary: a successfully created or transitioned value proves
    only that the ids are positive, origin and destination are distinct, the
    note is canonical and valid, the status is one of the closed values, and the
    logical transition is allowed. It proves nothing about the world — not that
    the post or either community exists, that the origin id matches the durable
    post row, that the two communities hold an accepted connection, that either
    is currently eligible, that the acting user may request or review anything,
    or that no other active placement exists. Stores must derive the origin from
    the locked post row, authorize, and lock durable rows independently; no
    authorization booleans or roles live inside {!t}.

    Timestamps are a store concern: the pure domain decides only the logical
    status transition, and the transactional store sets [reviewed_at],
    [removed_at], and [withdrawn_at] at its own transaction time.

    Error privacy: every error is payload-free — no rejected note, length,
    malformed byte, post id, community id, status, or action can leave through
    the error channel, and no [string_of_error] or printer exists. *)

type status = Pending | Accepted | Rejected | Removed | Withdrawn

val status_of_string : string -> status option
(** Accepts exactly the five canonical database spellings — [pending],
    [accepted], [rejected], [removed], [withdrawn] — and nothing else: no
    aliases, capitalization variants, padding, or abbreviations. An unknown
    string is [None], never a repaired or defaulted value. *)

val string_of_status : status -> string
(** The exact database value; round-trips with {!status_of_string}. *)

type action = Accept | Reject | Withdraw | Remove

type t
(** A placement-domain value: the canonical post id, the two community ids, a
    closed status, and the canonical optional private request note. Deliberately
    abstract, with no serializer, printer, row id, section, or timestamp; only
    the accessors below are exposed. *)

type error =
  | Invalid_post_id
  | Invalid_community_id
  | Same_community
  | Invalid_request_note
  | Invalid_transition

val tombstone_labels : string list
(** The closed set of durable deletion tombstone labels, exposed so SQL that
    must mirror the tombstone rule (the notification capability columns) can be
    built from this one list instead of respelling the bytes. *)

val post_content_tombstoned : string option -> bool
(** Whether a canonical post's stored [content] is one of the three durable
    deletion tombstones — [[deleted]], [[removed by admin]],
    [[removed by moderator]] — byte-for-byte the labels the deletion paths write
    and the same closed set the comment-creation gate refuses to comment under.
    A tombstoned thread is not shareable and not acceptable. [None] (a link
    post) is not a tombstone. *)

val canonical_request_note : string option -> (string option, error) result
(** Exactly the note canonicalization {!create_pending} performs — CRLF/CR to
    LF, ASCII outer-trim, empty collapses to [None], then the UTF-8 /
    control-byte / 2,000-scalar rules — exposed so a caller that must judge a
    note before any placement subject exists (deterministic form validation in
    the thread composer) cannot drift from the one domain rule. Returns the
    canonical value or [Error Invalid_request_note]; proves nothing about any
    post, community, or connection. *)

val create_pending :
  post_id:int ->
  origin_community_id:int ->
  destination_community_id:int ->
  request_note:string option ->
  (t, error) result
(** A request to place the canonical post into one connected destination
    community. On success the status is {!Pending} and the note is the validated
    canonical value.

    A non-positive post id is [Invalid_post_id]; a non-positive community id on
    either side is [Invalid_community_id]; equal origin and destination are
    [Same_community] — a thread is never "shared" into the community it already
    lives in, and the durable CHECK mirrors this exactly.

    Note canonicalization, in order: CRLF then lone CR normalize to LF; the
    result is outer-trimmed over ASCII whitespace only (space, tab, CR, LF, FF,
    VT); an empty result collapses to [None]. A remaining non-empty note must be
    valid UTF-8, at most 2,000 Unicode scalar values (counted per scalar, never
    per byte), with LF and horizontal tab as the only permitted control bytes —
    no NUL, no other ASCII controls, no DEL. Internal spacing, line feeds, tabs,
    Unicode, and punctuation are preserved byte-for-byte; nothing is truncated,
    Unicode-normalized, or parsed as Markdown/HTML. Rendering layers must still
    HTML-escape the value. Any invalid non-empty note returns
    [Error Invalid_request_note].

    Success does not prove the post exists, that [origin_community_id] is really
    the post's durable community, that the pair holds an accepted connection, or
    that no active placement exists — those checks belong to the transactional
    store, which derives the origin from the locked post row and never trusts a
    caller-supplied one. *)

val post_id : t -> int
val origin_community_id : t -> int
val destination_community_id : t -> int

val involves : t -> community_id:int -> bool
(** Whether the community is the origin or the destination of this placement. *)

val status : t -> status

val request_note : t -> string option
(** The canonical note: LF line endings, outer-trimmed, never [Some ""]. Carried
    unchanged through every lifecycle transition as private history. *)

val apply : t -> action -> (t, error) result
(** The closed lifecycle. Permits exactly:

    - {!Pending} + {!Accept} → {!Accepted}
    - {!Pending} + {!Reject} → {!Rejected}
    - {!Pending} + {!Withdraw} → {!Withdrawn}
    - {!Accepted} + {!Remove} → {!Removed}

    Every other status/action pair returns [Error Invalid_transition].
    {!Rejected}, {!Removed}, and {!Withdrawn} are all terminal: no reopening,
    retry, resubmission, cancellation, suspension, expiration, replacement, or
    reactivation. A later request after any terminal state is a new durable row
    and a new {!create_pending} value — never a reopened historical one.
    Withdrawal is deliberately not {!Remove} on a pending value: the two
    cancellations stay distinct in the vocabulary, the durable shape, and
    history. All ids and the canonical note are preserved unchanged. *)
