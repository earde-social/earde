(* Pure classification of the OAuth callback's single failure redirect, kept
   in its own module so the failure vocabulary is exhaustive by construction
   and the clients that produce these errors can keep their "this module
   never logs" contract. See the .mli for the privacy contract: the type
   admits no constructor that could carry secret or personal material. *)

type installation = {
  installation_id : int64;
  account_type : Github_user_installations.account_type;
}

type t =
  | Malformed_callback
  | Configuration_unavailable
  | Cookie_missing
  | Cookie_invalid
  | Authorization_rejected
  | Credentials_unavailable
  | State_rejected of Github_onboarding_state_store.consume_error
  | Token_exchange_failed of {
      pending_installation_id : int64;
      error : Github_oauth_token_exchange.error;
    }
  | Installation_verification_failed of {
      pending_installation_id : int64;
      error : Github_user_installations.error;
    }
  | Repository_listing_failed of {
      installation : installation;
      error : Github_user_installation_repositories.error;
    }
  | Installation_persistence_failed of {
      installation : installation;
      error : Github_installation_store.error;
    }
  | Draft_persistence_failed of {
      installation : installation;
      error : Project_onboarding_draft_store.error;
    }

(* The two account kinds print as the canonical database strings, which
   Github_onboarding owns, so a log line and a stored row never drift
   apart. *)
let account_type_label = function
  | Github_user_installations.User ->
      Github_onboarding.string_of_account_type Github_onboarding.User
  | Github_user_installations.Organization ->
      Github_onboarding.string_of_account_type Github_onboarding.Organization

(* The one remote datum any of these errors carries: an HTTP status integer,
   printed as its own field rather than glued into the reason so the reason
   vocabulary stays finite and greppable. *)
let status_field status = Printf.sprintf " status=%d" status

let consume_reason =
  let open Github_onboarding_state_store in
  function
  | State_not_found -> "state_not_found"
  | State_expired -> "state_expired"
  | State_already_consumed -> "state_already_consumed"
  | Session_binding_mismatch -> "session_binding_mismatch"
  | Flow_mismatch -> "flow_mismatch"
  | Missing_pending_installation -> "missing_pending_installation"
  | Storage_error -> "storage_error"

let exchange_reason =
  let open Github_oauth_token_exchange in
  function
  | Transport_error -> ("transport_error", "")
  | Unexpected_http_status status ->
      ("unexpected_http_status", status_field status)
  | OAuth_rejected -> ("oauth_rejected", "")
  | Invalid_response -> ("invalid_response", "")

let verification_reason =
  let open Github_user_installations in
  function
  | Invalid_installation_id -> ("invalid_installation_id", "")
  | Transport_error -> ("transport_error", "")
  | Unexpected_http_status status ->
      ("unexpected_http_status", status_field status)
  | Invalid_response -> ("invalid_response", "")
  | Installation_not_accessible -> ("installation_not_accessible", "")
  | Pagination_limit -> ("pagination_limit", "")

let listing_reason =
  let open Github_user_installation_repositories in
  function
  | Transport_error -> ("transport_error", "")
  | Unexpected_http_status status ->
      ("unexpected_http_status", status_field status)
  | Invalid_response -> ("invalid_response", "")
  | No_public_repositories -> ("no_public_repositories", "")
  | Pagination_limit -> ("pagination_limit", "")

let installation_store_reason =
  let open Github_installation_store in
  function
  | Invalid_connected_by_user_id -> "invalid_connected_by_user_id"
  | Installation_unavailable -> "installation_unavailable"
  | Storage_error -> "storage_error"

let draft_store_reason =
  let open Project_onboarding_draft_store in
  function
  | Invalid_user_id -> "invalid_user_id"
  | Installation_unavailable -> "installation_unavailable"
  | Storage_error -> "storage_error"

let stage_reason stage reason = Printf.sprintf "stage=%s reason=%s" stage reason
let pending_fields id = Printf.sprintf " installation=%Ld" id

let verified_fields { installation_id; account_type } =
  Printf.sprintf " installation=%Ld account_type=%s" installation_id
    (account_type_label account_type)

let describe = function
  | Malformed_callback -> stage_reason "callback_parse" "malformed_callback"
  | Configuration_unavailable -> stage_reason "configuration" "unavailable"
  | Cookie_missing -> stage_reason "flow_cookie" "missing"
  | Cookie_invalid -> stage_reason "flow_cookie" "invalid"
  | Authorization_rejected -> stage_reason "authorization" "rejected_at_github"
  | Credentials_unavailable -> stage_reason "credentials" "unavailable"
  | State_rejected error -> stage_reason "state" (consume_reason error)
  | Token_exchange_failed { pending_installation_id; error } ->
      let reason, status = exchange_reason error in
      stage_reason "token_exchange" reason
      ^ status
      ^ pending_fields pending_installation_id
  | Installation_verification_failed { pending_installation_id; error } ->
      let reason, status = verification_reason error in
      stage_reason "installation_verification" reason
      ^ status
      ^ pending_fields pending_installation_id
  | Repository_listing_failed { installation; error } ->
      let reason, status = listing_reason error in
      stage_reason "repository_listing" reason
      ^ status
      ^ verified_fields installation
  | Installation_persistence_failed { installation; error } ->
      stage_reason "installation_persistence" (installation_store_reason error)
      ^ verified_fields installation
  | Draft_persistence_failed { installation; error } ->
      stage_reason "draft_persistence" (draft_store_reason error)
      ^ verified_fields installation
