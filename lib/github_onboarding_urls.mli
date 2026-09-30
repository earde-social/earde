(** Pure construction of the URLs that start GitHub onboarding: App installation
    and explicit user authorization with PKCE.

    Both URLs target the fixed [https://github.com] host — never a configured
    one — and are built with [Uri], so reserved characters in opaque values are
    query-encoded and cannot escape their parameter. No IO, no logging, no
    session or database access; the PKCE verifier has no representation here.
    The URLs carry a raw one-time state by design, so callers must never log
    them. *)

val installation_url :
  Github_app_config.t -> state:Github_onboarding_crypto.state -> string
(** [https://github.com/apps/<slug>/installations/new?state=<state>], with
    exactly that one query parameter and no fragment or userinfo. *)

val authorization_url :
  Github_app_config.t ->
  state:Github_onboarding_crypto.state ->
  code_challenge:Github_onboarding_pkce.challenge ->
  string
(** [https://github.com/login/oauth/authorize] with exactly [client_id],
    [redirect_uri] (the validated callback URL, emitted verbatim), [state],
    [code_challenge] and [code_challenge_method=S256] — each once, and nothing
    else. *)
