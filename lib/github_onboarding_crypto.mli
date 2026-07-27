(** Cryptographic material boundary for GitHub onboarding: OAuth-style
    callback state and session-binding tokens, plus the deterministic
    SHA-256 lookup hashes that are the only forms ever persisted. No IO, no
    persistence, no session access — callers wire those separately. Parse
    errors carry no payload so rejected (attacker-controlled) input can
    never leak into messages or logs. *)

type state
(** A callback state token: 32 random bytes in canonical unpadded Base64url. *)

type state_hash
(** SHA-256 of a {!state} under the state domain, as lowercase hex. Safe to
    persist: contains no raw token material. *)

type session_binding
(** A session-binding token, same format as {!state}. The raw value lives
    only in the Dream session; only its {!session_binding_hash} is
    persisted. *)

type session_binding_hash
(** SHA-256 of a {!session_binding} under the session-binding domain, as
    lowercase hex. Safe to persist: contains no raw token material. *)

type parse_error =
  | Invalid_format

val generate_state : unit -> state
(** 32 fresh random bytes ([Dream.random]), canonically Base64url-encoded. *)

val state_of_callback : string -> (state, parse_error) result
(** Accepts only the canonical unpadded Base64url encoding of exactly 32
    bytes: the input must decode, be 32 bytes long, and re-encode to the
    exact supplied string. Blank input, padding, surrounding whitespace,
    malformed Base64url, non-canonical spellings, and wrong lengths are all
    [Error Invalid_format]. *)

val state_to_string : state -> string

val hash_state : state -> state_hash
(** Deterministic: the same state always yields the same hash. Domain
    separation guarantees it differs from {!hash_session_binding} of the
    same underlying material. *)

val state_hash_to_string : state_hash -> string

val generate_session_binding : unit -> session_binding
(** Same generation scheme as {!generate_state}. *)

val session_binding_of_string : string -> (session_binding, parse_error) result
(** Same canonical-encoding rules as {!state_of_callback}. *)

val session_binding_to_string : session_binding -> string

val hash_session_binding : session_binding -> session_binding_hash
(** Deterministic, domain-separated from {!hash_state}. *)

val session_binding_hash_to_string : session_binding_hash -> string
