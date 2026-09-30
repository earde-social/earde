(** PKCE (RFC 7636) primitives for the GitHub App OAuth web flow: code verifier
    generation/parsing and S256 challenge derivation. Pure — no IO, no Lwt, no
    session or database access, and nothing here stores or logs a verifier: the
    raw verifier later lives only in the authenticated Dream session, scoped to
    its onboarding state. Parse errors carry no payload so rejected input can
    never leak into messages or logs. *)

type verifier
(** A PKCE code verifier: 32 random bytes (256 bits) in canonical unpadded
    Base64url — always 43 characters. *)

type challenge
(** The S256 code challenge derived from a {!verifier}:
    [BASE64URL_UNPADDED(SHA256(ASCII(verifier)))], also 43 characters. Safe to
    send to GitHub; contains no raw digest bytes in any other form. Always
    derived internally from a valid verifier, so it has no parser. *)

type parse_error = Invalid_format

val generate_verifier : unit -> verifier
(** 32 fresh random bytes ([Dream.random]), canonically Base64url-encoded. *)

val verifier_of_string : string -> (verifier, parse_error) result
(** Accepts only the canonical unpadded Base64url encoding of exactly 32 bytes:
    the input must decode, be 32 bytes long, and re-encode to the exact supplied
    string. Blank input, padding, surrounding whitespace, malformed Base64url,
    non-canonical spellings, and wrong lengths are all [Error Invalid_format].
    No trimming, case folding, or repair. *)

val verifier_to_string : verifier -> string

val challenge_of_verifier : verifier -> challenge
(** Standard S256: SHA-256 of the verifier {e string itself} (not its decoded
    bytes), raw 32-byte digest Base64url-encoded without padding. Deterministic,
    and deliberately without domain separation, pepper, or hex encoding — GitHub
    must derive the identical value from the verifier presented at token
    exchange. *)

val challenge_to_string : challenge -> string
