(** Pure domain representation of a project↔community home relation for the
    GitHub-pivot flow. Shared by three durable shapes: a steward's pending
    request to use an existing community as the project home, the immediately
    accepted relation created atomically while provisioning a dedicated
    community, and the later accept/reject/remove lifecycle over both. No SQL,
    HTTP, rendering, session access, time handling, or logging happens here.

    Authorization boundary: a successfully created or transitioned value proves
    only that the note is canonical and valid, the status is one of the closed
    values, and the logical transition is allowed. It proves nothing about the
    world — not that the current Earde user is a project steward, that the user
    moderates the target community, that the project is verified, that the
    community is eligible, that the project lacks another active relation, that
    the database row is still in the expected status, or that the transition won
    a concurrent race. Future stores must authorize and lock durable rows
    independently; no authorization booleans or roles live inside {!t}.

    Timestamps are a store concern: the pure domain decides only the logical
    status transition, and the transactional stores set [reviewed_at] and
    [removed_at] at their own transaction time.

    Error privacy: every error is payload-free — no rejected note, length,
    malformed byte, status, or action can leave through the error channel, and
    no [string_of_error] or printer exists. *)

type status = Pending | Accepted | Rejected | Removed

val status_of_string : string -> status option
(** Accepts exactly the four canonical database spellings — [pending],
    [accepted], [rejected], [removed] — and nothing else: no aliases,
    capitalization variants, padding, or abbreviations. *)

val string_of_status : status -> string
(** The exact database value; round-trips with {!status_of_string}. *)

type action = Accept | Reject | Remove

type t
(** A relation-domain value: a closed status plus the canonical optional private
    request note. Deliberately abstract, with no serializer, printer, ID, or
    timestamp; only the accessors below are exposed. *)

type error = Invalid_request_note | Invalid_transition

val create_pending : request_note:string option -> (t, error) result
(** A verified project steward requesting an existing community as the project's
    home. On success the status is {!Pending} and the note is the validated
    canonical value.

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

    Success does not prove the project exists, the caller is a current steward,
    the target community exists or is eligible, the project has no active home,
    or that the requester may submit the request — those checks belong to the
    future transactional request store. *)

val create_provisioned_home : unit -> t
(** The accepted relation created atomically when a project provisions its own
    dedicated community: status {!Accepted}, note [None]. A separate constructor
    — never [Pending] then {!Accept} — because no moderator review or pending
    request took place. It does not grant or represent a community moderation
    role. *)

val status : t -> status

val request_note : t -> string option
(** The canonical note: LF line endings, outer-trimmed, never [Some ""]. Carried
    unchanged through every lifecycle transition as private history. *)

val apply : t -> action -> (t, error) result
(** The closed MVP lifecycle. Permits exactly:

    - {!Pending} + {!Accept} → {!Accepted}
    - {!Pending} + {!Reject} → {!Rejected}
    - {!Accepted} + {!Remove} → {!Removed}

    Every other status/action pair returns [Error Invalid_transition].
    {!Rejected} and {!Removed} are terminal: no reopening, retry, resubmission,
    cancellation, suspension, expiration, replacement, or reactivation. A later
    request after rejection or removal is a new durable row and a new
    {!create_pending} value, never a reopened historical one. The canonical note
    is preserved unchanged. *)
