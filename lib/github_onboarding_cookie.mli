(** Dream adapter for one GitHub onboarding flow's private browser material
    ({!Github_onboarding_session_data}): stores, loads, and deletes it as one
    dedicated encrypted cookie per onboarding state, entirely outside the normal
    Earde SQL-session storage.

    Fixed policy: encrypted and authenticated by Dream, [HttpOnly],
    [SameSite=Lax], 900-second [Max-Age], host-only (no [Domain]), path
    [/integrations/github] — covering exactly the registered install-return and
    authorize-callback paths. The [Secure] attribute and the [__Secure-] name
    prefix are derived solely from the validated
    {!Github_app_config.public_origin}: an https origin gets both (never
    [__Host-], whose [Path=/] requirement conflicts with the deliberately narrow
    path), and the approved loopback http development origins get neither.
    Forwarding headers are never consulted, so TLS termination at a reverse
    proxy still produces production attributes.

    Distinct states yield distinct cookie names
    ({!Github_onboarding_session_data.cookie_name}, which carries the state
    hash, never the raw state), so several onboarding flows can run in one
    browser without touching each other. No cookie names, values, or onboarding
    material are ever logged. *)

type load_error =
  | Missing  (** no cookie with this flow's browser-visible name *)
  | Invalid
      (** the cookie exists but failed authenticated decryption or strict
          plaintext decoding *)

val store :
  Github_app_config.t ->
  request:Dream.request ->
  response:Dream.response ->
  state:Github_onboarding_crypto.state ->
  Github_onboarding_session_data.t ->
  unit
(** Appends one encrypted [Set-Cookie:] header for this flow to [response], with
    every security-relevant attribute supplied explicitly. Storing one flow
    never overwrites another state's cookie. *)

val load :
  Github_app_config.t ->
  request:Dream.request ->
  state:Github_onboarding_crypto.state ->
  (Github_onboarding_session_data.t, load_error) result
(** Reads and decrypts this flow's cookie from [request], then decodes it
    strictly. [Missing] when the browser sent no cookie under the flow's
    browser-visible name; [Invalid] when one is present but cannot be decrypted
    or decoded. Never deletes anything — loading has no response to write to. *)

val drop :
  Github_app_config.t ->
  request:Dream.request ->
  response:Dream.response ->
  state:Github_onboarding_crypto.state ->
  unit
(** Appends an expiring [Set-Cookie:] header targeting exactly this flow's
    cookie, under the same name, path, and Secure/prefix policy as {!store}.
    Idempotent; other flows' cookies are untouched, and the stored value is
    never inspected. *)
