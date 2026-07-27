(* Tokens are held in their canonical unpadded Base64url encoding: exactly
   32 random bytes (256 bits), so the encoded form is the single wire and
   comparison representation. Parsing round-trips the encoding to reject
   padded, whitespace-wrapped, and otherwise non-canonical spellings that
   would decode to the same bytes — lookups against stored hashes must never
   have two spellings of one token. *)

type state = string
type state_hash = string
type session_binding = string
type session_binding_hash = string

type parse_error =
  | Invalid_format

let token_bytes = 32

let generate () = Dream.to_base64url (Dream.random token_bytes)

(* The error is deliberately payload-free: rejected callback input is
   attacker-controlled and must not leak into messages or logs. *)
let parse input =
  match Dream.from_base64url input with
  | None -> Error Invalid_format
  | Some raw ->
      if
        String.length raw = token_bytes
        && String.equal (Dream.to_base64url raw) input
      then Ok input
      else Error Invalid_format

(* Domain separation keeps a state hash and a session-binding hash of the
   same underlying material distinct; the NUL separator cannot occur in
   either the domain string or the Base64url token, so the preimage is
   unambiguous. *)
let hash ~domain token =
  Digestif.SHA256.(to_hex (digest_string (domain ^ "\000" ^ token)))

let state_domain = "earde:github-onboarding:state:v1"
let session_binding_domain = "earde:github-onboarding:session-binding:v1"
let generate_state = generate
let state_of_callback = parse
let state_to_string state = state
let hash_state state = hash ~domain:state_domain state
let state_hash_to_string hash = hash
let generate_session_binding = generate
let session_binding_of_string = parse
let session_binding_to_string binding = binding
let hash_session_binding binding = hash ~domain:session_binding_domain binding
let session_binding_hash_to_string hash = hash
