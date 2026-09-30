(** Operator-facing classification of the one failure the GitHub onboarding
    OAuth callback can answer with. The browser keeps receiving exactly one
    opaque redirect for every cause — this module never changes a response, a
    status, or a cookie; it only names the cause for the server log, so a
    production failure is diagnosable without a debugger or a packet capture.

    Pure: no IO, no SQL, no network, and no logging of its own — the handler
    alone decides when to emit. The classification is deliberately server- side
    only, so distinguishing causes here cannot turn the endpoint into an oracle:
    nothing in this module ever reaches the browser.

    Privacy contract, enforced structurally by the type: a value can only carry
    closed variants, a GitHub installation id, a GitHub account kind, and an
    HTTP status integer. There is no constructor through which a callback state,
    authorization code, PKCE verifier, access or refresh token, client secret,
    cookie, account login, repository name, URL, description, or raw API
    response body could reach a log line. Installation ids are non-secret
    routing identifiers that the outbound API URL already puts in the client
    log, and the account kind is one of two enum values. *)

type installation = {
  installation_id : int64;
      (** The installation id GitHub verified against the user access token. *)
  account_type : Github_user_installations.account_type;
      (** Whether that installation targets a personal account or an
          organization — the one field that tells the two onboarding account
          kinds apart in a log. *)
}
(** The verified installation identity available once verification has
    succeeded. Deliberately excludes the renamable account login. *)

type t =
  | Malformed_callback
      (** The callback target failed strict state / code-XOR-error parsing. *)
  | Configuration_unavailable
      (** The GitHub App configuration failed to load. *)
  | Cookie_missing  (** No per-flow cookie for this state in this browser. *)
  | Cookie_invalid  (** The per-flow cookie was undecryptable or undecodable. *)
  | Authorization_rejected  (** The user declined authorization at GitHub. *)
  | Credentials_unavailable  (** The OAuth client credentials failed to load. *)
  | State_rejected of Github_onboarding_state_store.consume_error
      (** Single-use state consumption refused or failed. The cause is named
          only in the log; the response stays the same collapsed answer for
          every variant, including [Session_binding_mismatch], which is a
          genuine security signal an operator should be able to see. *)
  | Token_exchange_failed of {
      pending_installation_id : int64;
      error : Github_oauth_token_exchange.error;
    }
      (** The authorization-code exchange failed. The id is the one the consumed
          state carried, still unverified at this point. *)
  | Installation_verification_failed of {
      pending_installation_id : int64;
      error : Github_user_installations.error;
    }
      (** The pending installation could not be verified against the user access
          token. *)
  | Repository_listing_failed of {
      installation : installation;
      error : Github_user_installation_repositories.error;
    }
      (** The verified installation's public-repository listing failed —
          including [No_public_repositories], the fail-closed outcome when an
          installation grants access to private repositories only. *)
  | Installation_persistence_failed of {
      installation : installation;
      error : Github_installation_store.error;
    }  (** Recording the verified installation failed; nothing was committed. *)
  | Draft_persistence_failed of {
      installation : installation;
      error : Project_onboarding_draft_store.error;
    }
      (** The onboarding draft refresh failed after the installation record had
          already committed — the one intentional partial-persistence boundary.
      *)

val describe : t -> string
(** A stable, greppable, single-line label built only from the closed variants
    and integers above, in [key=value] form, e.g.
    ["stage=repository_listing reason=no_public_repositories \
     installation=149347874 account_type=organization"].

    The [stage] and [reason] vocabularies are closed and exhaustive: adding a
    cause is a compile error until it is named here. Safe to log verbatim. *)
