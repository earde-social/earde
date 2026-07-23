(** Client for the GitHub App OAuth code-for-token exchange.

    This module does exactly one thing: it exchanges a GitHub OAuth
    authorization code for a GitHub App user access token at the fixed
    endpoint [https://github.com/login/oauth/access_token]. The endpoint is
    a compile-time constant — it is never derived from configuration,
    environment, headers, or caller input — so no input can redirect the
    credential-bearing request elsewhere.

    The HTTP layer is injected via {!TRANSPORT} so the exchange logic is
    testable offline; {!Cohttp_transport} is the production implementation.

    Error constructors are payload-free except for the HTTP status integer:
    no code, secret, verifier, token, response body, JSON fragment, remote
    error text, or exception text can travel inside an error. The module
    itself never logs. *)

type authorization_code
(** An opaque, byte-exact OAuth authorization code as received on the
    callback. Deliberately abstract with no accessor or printer, so the only
    thing the rest of the codebase can do with a code is hand it back to
    {!exchange}. *)

type code_error =
  | Invalid_code

val authorization_code_of_callback :
  string -> (authorization_code, code_error) result
(** Validates a raw [code] callback value. The code is treated as an opaque
    credential: any non-empty value free of ASCII whitespace, NUL, control
    bytes, and DEL is accepted and preserved byte-for-byte. No trimming or
    repair; no prefix, length, alphabet, or Base64/hex syntax is imposed.
    The future callback parser calls this after enforcing exactly one raw
    [code=] query parameter. *)

type token_set
(** Opaque token material from a successful exchange. The representation is
    abstract and there is no serializer, pretty-printer, fingerprint, or
    access to the original response.

    The accessors below exist only for the later GitHub API and persistence
    slices. Callers must never: log tokens; include them in errors; place
    them in URLs or HTML; send them to analytics; or persist them without an
    explicit encrypted-at-rest design. *)

val access_token : token_set -> string
(** The exact access-token bytes as issued by GitHub. *)

val expires_in : token_set -> int option
(** Access-token lifetime in seconds; [None] when the GitHub App is
    configured with non-expiring user tokens. *)

val refresh_token : token_set -> string option
(** The exact refresh-token bytes; [None] for non-expiring configuration. *)

val refresh_token_expires_in : token_set -> int option
(** Refresh-token lifetime in seconds; [None] for non-expiring
    configuration. The three expiration-related accessors are all [Some] or
    all [None] — partial combinations are rejected at parse time. *)

module type TRANSPORT = sig
  val post :
    uri:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (int * string, unit) result Lwt.t
  (** One POST request. [Ok (status, body)] carries the HTTP status and the
      response body; [Error ()] deliberately carries no exception or
      response content, so a failing transport cannot leak what it saw. *)
end

module Cohttp_transport : TRANSPORT
(** Production transport on [Cohttp_lwt_unix]. Makes one POST, follows no
    redirects, enforces a fixed 10-second timeout covering the request and
    the response-body read, and caps the response body at 65,536 bytes.
    DNS/connect/TLS failures, timeout, body overflow, and Cohttp or stream
    failures all collapse to [Error ()] without logging or stringifying
    anything; [Lwt.Canceled] keeps propagating. *)

type error =
  | Transport_error
  | Unexpected_http_status of int
      (** Any status other than 200. Only the integer is preserved; the
          response body and headers are dropped unread. *)
  | OAuth_rejected
      (** GitHub answered 200 with a well-formed OAuth error object. The
          remote error string is deliberately not preserved, so this client
          cannot become a remote-error oracle. *)
  | Invalid_response
      (** The 200 body was not a well-formed success or rejection object.
          No JSON or parser diagnostics are preserved. *)

val exchange :
  transport:(module TRANSPORT) ->
  config:Github_app_config.t ->
  credentials:Github_oauth_credentials.t ->
  code:authorization_code ->
  verifier:Github_onboarding_pkce.verifier ->
  (token_set, error) result Lwt.t
(** Performs the code-for-token exchange. Sends exactly the form fields
    [client_id], [client_secret], [code], [redirect_uri] (the validated
    callback URL from [config]), and [code_verifier], form-encoded in that
    order, with [Accept: application/json] and
    [Content-Type: application/x-www-form-urlencoded]. No credential ever
    appears in the URI or headers. A success requires [token_type]
    ["bearer"], an empty [scope], and either none or all of the three
    expiration-related fields. *)
