(** Cloudflare Turnstile bot-protection for public signup.

    Verification fails closed: any ambiguity — a missing/empty token, a request
    timeout, a transport error, a non-2xx response, or an unparseable body — is
    treated as failure, so no pending signup is written and no confirmation email
    is sent. The secret key is read from the environment, only ever sent to
    Cloudflare, and never logged. *)

type status =
  | Configured of string
      (** Both keys present; the [string] is the public site key, safe to render. *)
  | Disabled
      (** Keys absent and [EARDE_TURNSTILE_REQUIRED] unset; dev bypass (signup
          works without Turnstile). *)
  | Misconfigured
      (** [EARDE_TURNSTILE_REQUIRED] is set but a key is missing/empty; the caller
          must fail closed rather than serve an unprotected signup form. *)

val status : unit -> status

val parse_siteverify : string -> bool
(** Pure parser for the siteverify JSON response: [true] iff Cloudflare reported
    ["success": true]. Exposed for offline unit tests; never performs IO. *)

val verify : response:string -> bool Lwt.t
(** Verify a [cf-turnstile-response] token server-side against Cloudflare's
    siteverify endpoint. Returns [true] only on an affirmative success. Never
    raises; never logs the secret. *)
