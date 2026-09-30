(** Pure representation and serialization of the private per-flow browser
    material used during GitHub onboarding: one random session binding and one
    PKCE verifier per onboarding state. The encoded value is the plaintext for
    one dedicated encrypted HTTP cookie per flow — stored later through Dream's
    encrypted-cookie API, never inside the Dream SQL-session dictionary — and
    each flow gets independently generated material under its own cookie name,
    so multiple browser tabs can onboard concurrently without overwriting one
    another. This module does no encryption and no IO — no Lwt, no
    request/response, no cookie or database access; it only produces the
    plaintext value and the deterministic cookie name, and callers wire the
    cookie adapter separately. Parse errors carry no payload so corrupted
    (possibly attacker-influenced) cookie data can never leak into messages or
    logs. *)

type t
(** One flow's private material: a session binding and a PKCE verifier,
    generated independently of each other. The raw session binding never escapes
    this type; only its hash is exposed. *)

type parse_error = Invalid_format

val create : unit -> t
(** Freshly and independently generates a session binding
    ({!Github_onboarding_crypto.generate_session_binding}) and a PKCE verifier
    ({!Github_onboarding_pkce.generate_verifier}). Neither value is derived from
    the other or from any ambient state. *)

val session_binding_hash : t -> Github_onboarding_crypto.session_binding_hash
(** The only exposed form of the session binding — the raw token stays private
    to the serialized cookie value. *)

val verifier : t -> Github_onboarding_pkce.verifier
(** The raw verifier, needed later for the OAuth code exchange. *)

val code_challenge : t -> Github_onboarding_pkce.challenge
(** Derived deterministically via
    {!Github_onboarding_pkce.challenge_of_verifier}; never stored. *)

val encode : t -> string
(** Deterministic versioned serialization — the plaintext handed to Dream's
    encrypted-cookie API for one per-flow cookie:
    [v1.<session-binding>.<pkce-verifier>], both tokens in their canonical
    unpadded Base64url form (which cannot contain dots, so the separator is
    unambiguous). *)

val decode : string -> (t, parse_error) result
(** Accepts only the exact {!encode} output: three dot-separated components,
    version exactly [v1], and both tokens in canonical form — re-encoding a
    decoded value always equals the supplied input. No trimming or repair; blank
    input, unknown versions, missing or extra components, padded or otherwise
    non-canonical tokens are all [Error Invalid_format]. *)

val cookie_name : Github_onboarding_crypto.state -> string
(** Deterministic name of one flow's encrypted cookie:
    [earde.github_onboarding.v1.] followed by the full state hash
    ({!Github_onboarding_crypto.hash_state}) as 64 lowercase hex characters. The
    raw state never appears; distinct states get distinct names, so concurrent
    flows coexist as independent cookies. *)
