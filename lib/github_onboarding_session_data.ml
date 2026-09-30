(* Private per-flow browser material for GitHub onboarding, carried in one
   dedicated encrypted cookie per onboarding state (via Dream's
   encrypted-cookie API later — never the Dream SQL-session dictionary; this
   module only supplies the plaintext value and the cookie name). Binding
   and verifier are generated independently — deriving one from the other
   would let whichever value leaks compromise both. Serialization is the
   versioned [v1.<binding>.<verifier>] string; the dot separator is
   unambiguous because canonical unpadded Base64url tokens never contain
   dots. Decoding leans on the component parsers' strict canonical
   round-trip discipline, so any decoded value re-encodes byte-for-byte to
   its input and corrupted cookie data fails closed. *)

type t = {
  session_binding : Github_onboarding_crypto.session_binding;
  verifier : Github_onboarding_pkce.verifier;
}

type parse_error = Invalid_format

let version = "v1"

(* Named by the state hash, not the raw state: cookie names travel in
   headers and must never hold a second spelling of the one-time callback
   token. *)
let cookie_name_prefix = "earde.github_onboarding.v1."

let create () =
  {
    session_binding = Github_onboarding_crypto.generate_session_binding ();
    verifier = Github_onboarding_pkce.generate_verifier ();
  }

let session_binding_hash { session_binding; _ } =
  Github_onboarding_crypto.hash_session_binding session_binding

let verifier { verifier; _ } = verifier

let code_challenge { verifier; _ } =
  Github_onboarding_pkce.challenge_of_verifier verifier

let encode { session_binding; verifier } =
  String.concat "."
    [
      version;
      Github_onboarding_crypto.session_binding_to_string session_binding;
      Github_onboarding_pkce.verifier_to_string verifier;
    ]

(* The error is deliberately payload-free: a rejected cookie value is
   secret (or corrupted) material and must not leak into messages or logs.
   The component parsers reject anything non-canonical, so the exact-three-
   component split plus version check make acceptance equivalent to "some
   [encode] output equals this input". *)
let decode input =
  match String.split_on_char '.' input with
  | [ v; binding; verifier ] when String.equal v version -> (
      match
        ( Github_onboarding_crypto.session_binding_of_string binding,
          Github_onboarding_pkce.verifier_of_string verifier )
      with
      | Ok session_binding, Ok verifier -> Ok { session_binding; verifier }
      | (Ok _ | Error _), _ -> Error Invalid_format)
  | _ -> Error Invalid_format

let cookie_name state =
  cookie_name_prefix
  ^ Github_onboarding_crypto.state_hash_to_string
      (Github_onboarding_crypto.hash_state state)
