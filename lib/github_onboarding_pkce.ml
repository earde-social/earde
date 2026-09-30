(* PKCE (RFC 7636) primitives for the GitHub App OAuth web flow. The
   verifier is held in its canonical unpadded Base64url encoding of exactly
   32 random bytes, the same wire discipline as Github_onboarding_crypto
   tokens: parsing round-trips the encoding so no two spellings of one
   verifier exist. The S256 challenge is the standard
   BASE64URL_UNPADDED(SHA256(ASCII(verifier))) — deliberately no domain
   separation, pepper, or hex step, because GitHub must derive the identical
   value from the verifier we later send it. *)

type verifier = string
type challenge = string
type parse_error = Invalid_format

let verifier_bytes = 32
let generate_verifier () = Dream.to_base64url (Dream.random verifier_bytes)

(* The error is deliberately payload-free: a rejected verifier is secret (or
   attacker-controlled) material and must not leak into messages or logs. *)
let verifier_of_string input =
  match Dream.from_base64url input with
  | None -> Error Invalid_format
  | Some raw ->
      if
        String.length raw = verifier_bytes
        && String.equal (Dream.to_base64url raw) input
      then Ok input
      else Error Invalid_format

let verifier_to_string verifier = verifier

(* Hashes the ASCII verifier string itself (not its decoded bytes), then
   Base64url-encodes the raw 32-byte digest; the digest never escapes in any
   other form. *)
let challenge_of_verifier verifier =
  Dream.to_base64url Digestif.SHA256.(to_raw_string (digest_string verifier))

let challenge_to_string challenge = challenge
