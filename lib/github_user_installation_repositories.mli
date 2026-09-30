(** Client that lists the public repositories accessible to a verified GitHub
    App installation on behalf of the OAuth-authorized user.

    This module does exactly one thing: it pages through the fixed endpoint
    [https://api.github.com/user/installations/<id>/repositories] —
    authenticated with the user access token from
    {!Github_oauth_token_exchange}, with the path ID taken only from an abstract
    {!Github_user_installations.verified_installation} — and returns the fully
    validated public repositories owned by that installation's account. The
    scheme and host are compile-time constants, never derived from
    configuration, environment, headers, token contents, or caller input, so no
    input can redirect the credential-bearing request elsewhere.

    The HTTP layer is the same injected {!TRANSPORT} as the installations
    client, and {!Cohttp_transport} is that module's production implementation
    re-exported rather than duplicated.

    Error constructors are payload-free except for the HTTP status integer: no
    token, response body, JSON fragment, repository metadata, installation or
    account ID, remote error text, or exception text can travel inside an error.
    The module never logs, performs no database writes or analytics, and retains
    nothing about non-public repositories. *)

type repository
(** One validated public repository. Deliberately abstract, with no serializer
    or printer; exactly the accessor fields below are retained. The raw JSON —
    including the response's own [html_url], permissions, counters, and every
    unknown field — is dropped at parse time, and non-public repositories can
    never inhabit this type. *)

type repository_set
(** The public repositories from one complete listing, in GitHub response order.
    Guaranteed non-empty: a complete scan that finds no public repository is
    reported as {!No_public_repositories} instead. *)

val repositories : repository_set -> repository list
(** The repositories in the set, preserving GitHub response order across pages.
    Always non-empty. *)

val repository_id : repository -> int64
(** GitHub repository ID, validated as a positive integer exactly representable
    as [int64]. *)

val owner_id : repository -> int64
(** GitHub account ID of the repository owner. Guaranteed equal to the verified
    installation's {!Github_user_installations.account_id} — ownership is
    checked against the stable account ID, never the renamable login. *)

val owner_login : repository -> string
(** The owner's login, preserved byte-for-byte. Non-empty and free of ASCII
    whitespace, NUL, other control bytes, DEL, and ['/']. It may differ from the
    installation's previously stored login, because GitHub logins can be
    refreshed independently of the stable account ID. *)

val name : repository -> string
(** Repository name, preserved byte-for-byte with case intact. Non-empty and
    free of ASCII whitespace, NUL, other control bytes, DEL, and ['/']. *)

val full_name : repository -> string
(** Exactly [<owner-login>/<repository-name>]; any response entry whose
    [full_name] disagrees with its own parts rejects the whole response. *)

val html_url : repository -> string
(** Canonical browser URL, constructed with [Uri] from the validated owner login
    and repository name as [https://github.com/<owner-login>/<repository-name>]
    — never taken from the response. Scheme [https], host [github.com], no
    query, fragment, or userinfo. *)

val description : repository -> string option
(** Repository description; [None] when GitHub reports JSON [null]. Accepted
    strings contain no NUL or other ASCII control bytes and are preserved
    byte-for-byte — UTF-8 and punctuation included — with no trimming. *)

val default_branch : repository -> string
(** Default branch name, snapshotted exactly as GitHub returned it — untrimmed,
    unnormalized, never URL-decoded or split. Non-empty and free of ASCII
    whitespace, NUL, other control bytes, and DEL; ['/'] is allowed, so
    slash-separated names like [release/v1] pass through byte-for-byte. No
    branch-name grammar is imposed beyond that, and this module never builds a
    URL or path from the value — a consumer that does must encode or validate it
    at that boundary. *)

val is_archived : repository -> bool
(** Whether the repository is archived. Archived public repositories are valid
    candidates and are never silently excluded. *)

module type TRANSPORT = Github_user_installations.TRANSPORT
(** The installations client's transport signature, shared so both clients are
    driven and tested through the same GET abstraction. *)

module Cohttp_transport : TRANSPORT
(** {!Github_user_installations.Cohttp_transport} itself — one GET, no redirect
    following, a fixed 10-second timeout covering request and body read, a
    bounded response body, payload-free [Error ()] on any failure, and
    propagated [Lwt.Canceled]. *)

type error =
  | Transport_error
  | Unexpected_http_status of int
      (** Any status other than 200. Only the integer is preserved; the response
          body and headers are dropped unread. *)
  | Invalid_response
      (** A 200 body was not a well-formed repositories page. This covers
          malformed JSON and fields, an entry — public or not — owned by any
          account other than the verified installation's, a [full_name]
          mismatch, and a duplicate repository ID or full name anywhere in the
          listing. No JSON or parser diagnostics are preserved, and a page with
          any malformed entry is rejected whole, never returned partially. *)
  | No_public_repositories
      (** The listing was scanned to its definitive end and contained no
          repository that is both [private = false] and [visibility = "public"].
      *)
  | Pagination_limit
      (** The twenty-page (2,000-repository) scan limit was reached with more
          results remaining — deliberately distinct from a complete result,
          which requires the definitive end of the listing. *)

val list_public :
  transport:(module TRANSPORT) ->
  token_set:Github_oauth_token_exchange.token_set ->
  installation:Github_user_installations.verified_installation ->
  (repository_set, error) result Lwt.t
(** Lists the installation's public repositories. Requests pages sequentially
    with exactly the query [per_page=100&page=<n>] in that order, starting at
    page 1 and stopping at page 20, and exactly the headers
    [Accept: application/vnd.github+json],
    [Authorization: Bearer <user-access-token>],
    [X-GitHub-Api-Version: 2026-03-10], and
    [User-Agent: Earde-GitHub-Onboarding]. The token rides only in the
    Authorization header; the installation ID appears only in the fixed path,
    taken from {!Github_user_installations.installation_id}. Every page is
    validated completely before any of its repositories count; non-public
    entries are validated just as strictly, then discarded without being
    returned, logged, or persisted. Stops immediately on transport failure,
    non-200 status, malformed response, the definitive end of the listing, or
    the pagination limit. *)
