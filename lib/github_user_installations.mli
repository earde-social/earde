(** Client that checks whether a pending GitHub App installation ID is
    accessible to the user who just completed the OAuth exchange.

    This module does exactly one thing: it lists the caller's App installations
    at the fixed endpoint [https://api.github.com/user/installations] —
    authenticated with the user access token from {!Github_oauth_token_exchange}
    — and reports whether a requested installation ID appears in the returned
    list. The endpoint is a compile-time constant, never derived from
    configuration, environment, headers, token contents, or caller input, so no
    input can redirect the credential-bearing request elsewhere.

    The HTTP layer is injected via {!TRANSPORT} so the verification logic is
    testable offline; {!Cohttp_transport} is the production implementation.

    Error constructors are payload-free except for the HTTP status integer: no
    token, response body, JSON fragment, installation list, remote error text,
    or exception text can travel inside an error. The module itself never logs,
    persists nothing, and returns nothing beyond the verified installation's
    identity fields. *)

type account_type =
  | User
  | Organization
      (** The installation's target type, mapped only from the exact
          installation-level [target_type] strings ["User"] and
          ["Organization"]. [account.type] is ignored; [target_type] alone is
          authoritative. The variant is closed on purpose: any other value or
          type rejects the whole page. *)

type verified_installation
(** Proof that the requested positive installation ID appeared in a valid
    response from [GET /user/installations] authenticated with the supplied user
    access token. Deliberately abstract, with no serializer or printer; exactly
    four fields of the matching entry are retained — installation ID, account
    ID, account login, and target type. The raw JSON, account URLs, avatar data,
    permissions, repository selection, and everything about nonmatching entries
    are dropped at parse time. *)

val installation_id : verified_installation -> int64
(** The verified installation ID, exactly as requested. *)

val account_id : verified_installation -> int64
(** GitHub account ID of the installation target, validated as a positive
    integer exactly representable as [int64]. *)

val account_login : verified_installation -> string
(** GitHub account login of the installation target, preserved byte-for-byte.
    Guaranteed non-empty and free of ASCII whitespace, NUL, other ASCII control
    bytes, and DEL; no length cap, case, or username grammar is imposed beyond
    that. *)

val account_type : verified_installation -> account_type
(** Whether the installation targets a user or an organization, taken from the
    entry's [target_type] field. *)

module type TRANSPORT = sig
  val get :
    uri:Uri.t ->
    headers:(string * string) list ->
    (int * string, unit) result Lwt.t
  (** One GET request. [Ok (status, body)] carries the HTTP status and the
      response body; [Error ()] deliberately carries no exception or response
      content, so a failing transport cannot leak what it saw. *)
end

module Cohttp_transport : TRANSPORT
(** Production transport on [Cohttp_lwt_unix]. Makes one GET, follows no
    redirects, enforces a fixed 10-second timeout covering the request and the
    response-body read, reads the body incrementally, and caps it at 2,097,152
    bytes. DNS/connect/TLS failures, timeout, body overflow, and Cohttp or
    stream failures all collapse to [Error ()] without logging or stringifying
    anything; [Lwt.Canceled] keeps propagating. *)

type error =
  | Invalid_installation_id
      (** The requested ID was zero or negative; no request was made. *)
  | Transport_error
  | Unexpected_http_status of int
      (** Any status other than 200. Only the integer is preserved; the response
          body and headers are dropped unread. *)
  | Invalid_response
      (** A 200 body was not a well-formed installations page. No JSON or parser
          diagnostics are preserved, and a page with any malformed entry —
          including malformed account metadata or target type — is rejected
          whole, even when an earlier entry matched. *)
  | Installation_not_accessible
      (** The full list was searched to its definitive end and the requested ID
          was not in it. *)
  | Pagination_limit
      (** The five-page (500-installation) search limit was reached with more
          results remaining and no match — deliberately distinct from
          {!Installation_not_accessible}, which asserts a complete search. *)

val verify :
  transport:(module TRANSPORT) ->
  token_set:Github_oauth_token_exchange.token_set ->
  installation_id:int64 ->
  (verified_installation, error) result Lwt.t
(** Searches the caller's installations for [installation_id]. Requests pages
    sequentially with exactly the query [per_page=100&page=<n>], starting at
    page 1 and stopping at page 5, and exactly the headers
    [Accept: application/vnd.github+json],
    [Authorization: Bearer <user-access-token>],
    [X-GitHub-Api-Version: 2026-03-10], and
    [User-Agent: Earde-GitHub-Onboarding]. The token rides only in the
    Authorization header, and the requested ID never appears in the URI or
    headers — it is verified by searching the returned list. Stops immediately
    on success, definitive end of results, malformed response, transport
    failure, non-200 status, or the pagination limit. *)
