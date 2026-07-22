(** PostHog server-side analytics (spec: docs/features/posthog-analytics.md §3.1).

    Consent is enforced by construction: the only capture path for domain
    events is [capture_if_consented], which requires the Dream request and
    captures only when the earde_analytics_consent cookie is exactly
    "granted" and POSTHOG_ENABLED is exactly "true". The raw HTTP-capture
    function is private to the module. All entry points return [unit]
    immediately; HTTP happens asynchronously, best-effort, and can never fail
    the calling request. *)

(** Closed server-side deployment environment. Set ONLY by the
    [EARDE_DEPLOYMENT_ENVIRONMENT] variable (exact lowercase values
    ["production"] / ["staging"] / ["development"]) — never inferred from
    hostname, branch name or executable mode, and never retained as an
    arbitrary string. When [POSTHOG_ENABLED=true] the variable is REQUIRED:
    unknown, missing, blank or differently-cased values disable analytics with
    a startup diagnostic. When PostHog is disabled the variable is not needed.

    Activation rules (all fail closed):
    - production: complete PostHog configuration and [EARDE_PUBLIC_ORIGIN]
      exactly [https://earde.com];
    - staging: complete PostHog configuration and an HTTPS origin that is NOT
      a production origin (no staging hostname is hardcoded);
    - development: disabled by default; [POSTHOG_ALLOW_DEVELOPMENT=true]
      explicitly enables it with loopback-only origins and a diagnostic.

    Production and staging use SEPARATE PostHog projects — separate tokens,
    project ids and personal API keys. That project separation is the
    isolation boundary; the [deployment_environment] event property this type
    feeds is diagnostic context only, and [Preflight] verifies each
    deployment's token/project binding. *)
type deployment_environment = Production | Staging | Development

(** ["production"] / ["staging"] / ["development"] — the normalized closed
    value used in event payloads and the browser configuration. *)
val deployment_environment_to_string : deployment_environment -> string

(** Closed person-property record (§4.3). Emitted only as the [$set] object —
    on [Account_signed_up] / [Account_logged_in] payloads and in
    [sync_person_after_consent_grant] — never as ordinary event properties.
    Deliberately contains no email: the stable identity is
    ["user:<database_id>"] and email is never sent to PostHog. *)
type person_properties = {
  username : string;
  signup_date : string;  (** ISO 8601 *)
  is_admin : bool;
}

(** Closed community group-property record (§5.3), for [$groupidentify].
    For fully private communities (§13) [community_slug] and
    [community_name] are [None]: the group profile keeps only the immutable
    numeric id and the closed visibility value — no human-readable
    identifiers. *)
type community_group = {
  community_id : int;
  community_slug : string option;
  community_name : string option;
  community_visibility : string;  (** "public" / "private" *)
  created_at : string option;  (** ISO 8601 when available on the record *)
}

type response_mode = Response_json | Response_redirect

(** Closed domain-event model (§3.2/§5.1). The compiler enforces the property
    allowlist: handlers cannot attach arbitrary properties, bodies, titles,
    emails, or tokens. *)
type event =
  | Account_signed_up of { user_id : int; person : person_properties }
      (** fired only after the confirmation link creates the real users row;
          carries [$set] — refreshes person properties server-side (§4.3) *)
  | Account_logged_in of { user_id : int; person : person_properties }
      (** carries [$set] — refreshes person properties server-side (§4.3) *)
  | Community_joined of {
      user_id : int;
      community_id : int;
      community_slug : string option;
          (** [None] for fully private communities (§13) — readable
              identifiers are omitted, numeric ids and counts remain *)
      community_visibility : string;
    }
  | Community_left of { user_id : int; community_id : int }
  | Chat_message_sent of {
      user_id : int;
      community_id : int;
      community_slug : string option;
      channel_id : int;
      channel_slug : string option;
      message_id : int64;
      content_length : int;
      response_mode : response_mode;
    }
  | Forum_thread_created of {
      user_id : int;
      community_id : int;
      section_id : int option;
      post_id : int;
      content_length : int;
      has_link : bool;
      has_mention : bool;
    }
  | Forum_comment_created of {
      user_id : int;
      community_id : int;
      post_id : int;
      comment_id : int;
      parent_comment_id : int option;
      content_length : int;
      has_mention : bool;
    }
  | Conversation_promoted of {
      user_id : int;
      community_id : int;
      community_slug : string option;
      channel_id : int;
      channel_slug : string option;
      section_id : int option;
      post_id : int;
      message_id : int64;  (** seed chat message *)
      promoted_message_count : int;
      promoted_participant_count : int option;
    }
  | Account_deleted
      (** personless aggregate deletion counter (§3.3): no user_id, no person
          [$set], no group — emitted with [$process_person_profile]=false and
          the constant [account_deletion_distinct_id], never a user identity *)

(** ["user:<database_id>"] — the §4.1 authenticated distinct-ID scheme. *)
val distinct_id_of_user_id : int -> string

(** ["system:account-deletion"] — the constant non-user distinct id of the
    personless [Account_deleted] metric. Never a person: the event disables
    person processing. *)
val account_deletion_distinct_id : string

(** ["community:<database_id>"] — the §5.3 stable group key (the immutable
    numeric id, never the mutable slug). Used by server payloads and by the
    browser group attribute rendered in the layout. *)
val community_group_key : int -> string

(** PostHog group_type_index of the "community" group type — the project's
    first (and only) group type (§5.3), hence [0]. Consumed by the §13
    Groups-API cleanup client. *)
val community_group_type_index : int

(** Strictly public browser configuration: the write-only project token, the
    ingest host and the normalized closed deployment environment, nothing
    else. [None] when analytics is disabled or the required configuration is
    invalid — in that case no banner, no config attributes, and no PostHog
    request may be produced. Server-only values ([POSTHOG_PERSONAL_API_KEY],
    [POSTHOG_PROJECT_ID]) are not reachable through this interface. *)
type browser_config = {
  browser_token : string;
  browser_api_host : string;
  browser_deployment_environment : string;
}

val browser_config : unit -> browser_config option

(** Consent cookie contract (§9): plaintext (JS-readable), [Path=/],
    [SameSite=Lax], no [HttpOnly], ~180-day Max-Age, [Secure] when the
    configured public origin is https. *)
val consent_cookie_name : string

val consent_cookie_max_age : float
val consent_cookie_secure : unit -> bool

(** §9 route-specific protection for [POST /analytics/consent]: exact Origin
    match against [EARDE_PUBLIC_ORIGIN], same-origin/same-site
    [Sec-Fetch-Site], JSON-only content type, and a body of exactly
    [{"state": "granted"|"denied"}]. No Dream CSRF token and no session are
    involved — a first-time landing visitor has neither. *)
val validate_consent_request :
  content_type:string option ->
  origin:string option ->
  sec_fetch_site:string option ->
  body:string ->
  ( [ `Granted | `Denied ],
    [ `Bad_request of string | `Forbidden of string ] )
  result

(** §2.3 URL rule (reference implementation, mirrored by analytics.js):
    strips the query string and fragment, keeping origin + path only. *)
val sanitize_url_for_analytics : string -> string

(** Captures the event iff analytics is enabled and the request carries
    [earde_analytics_consent=granted]. Missing, denied, or malformed consent
    produces no side effect. Community-scoped events automatically carry
    [$groups.community = "community:<id>"]. Every payload built by this
    module additionally carries the closed [deployment_environment] value,
    added exactly once by the shared payload envelope — never by event
    constructors and never as an arbitrary string. *)
val capture_if_consented : Dream.request -> distinct_id:string -> event -> unit

(** Consent-transition person-property sync (§3.1/§9). Deliberately does not
    inspect a request cookie: at grant time the "granted" value exists only in
    the outgoing Set-Cookie response. May be called ONLY by
    [analytics_consent_handler], after [state] has been validated as exactly
    "granted", the §9 request checks have passed, and the user's properties
    have been loaded. It emits only the dedicated [$identify] payload carrying
    the closed [$set] object — it is not a generic capture bypass. *)
val sync_person_after_consent_grant :
  distinct_id:string -> person_properties -> unit

(** Server-only private Persons-API configuration (§3.3): UI host, project id,
    personal API key. [None] unless BOTH [POSTHOG_PROJECT_ID] and
    [POSTHOG_PERSONAL_API_KEY] are set (a partial configuration warns once, by
    variable name only). Independent of [POSTHOG_ENABLED] — deletion is a
    data-lifecycle duty, not collection. These values must never reach HTML,
    JavaScript, responses, or logs. *)
type deletion_api_config = {
  deletion_ui_host : string;
  deletion_project_id : string;
  deletion_api_key : string;
}

val deletion_api_config : unit -> deletion_api_config option

(** Narrow §3.3 orchestration seam, usable ONLY by the account-deletion flow:
    the consent-gated personless [Account_deleted] metric as an awaitable
    bounded transport attempt. The event is safe against the Persons-API
    deletion by CONSTRUCTION (constant system distinct id, person processing
    disabled), not by request ordering — the await merely sequences the HTTP
    calls and proves nothing about ingestion. Disabled analytics or
    absent/denied consent resolves immediately. Not a generic capture API: it
    emits exactly one closed, identity-free event shape. *)
val capture_account_deleted_sequenced : Dream.request -> unit Lwt.t

(** Consent-gated [$groupidentify] for a community (§5.3): same request-cookie
    gate as [capture_if_consented]; emits only the closed [community_group]
    record. [distinct_id] MUST be the acting authenticated user's
    ["user:<id>"] — never ["community:<id>"] or another synthetic value, which
    would create a phantom PostHog person. Call only after a successful
    create/update/join with the full authoritative record in scope. *)
val identify_community_if_consented :
  Dream.request -> distinct_id:string -> community_group -> unit

(** Server-only credential/project preflight. Verifies — without ingesting
    any event — that [POSTHOG_PROJECT_ID] exists on the configured private
    API host and that its live [api_token] equals [POSTHOG_PROJECT_TOKEN],
    via the documented endpoints
    [GET /api/organizations/] (scope [organization:read]) and
    [GET /api/organizations/:org/projects/:id/] (scope [project:read]).
    Reuses the central configuration parser (an invalid configuration fails
    before any request), uses [POSTHOG_PERSONAL_API_KEY] only server-side
    under one bounded timeout, and reports only bounded failure classes —
    never the token, the key, or any response body. Run by
    [bin/check_posthog_config.ml] before migration/restart during production
    and staging deployments. *)
module Preflight : sig
  type report = {
    report_environment : deployment_environment;
    report_project_id : string;
    report_api_host : string;
    report_ui_host : string;
    report_token_fingerprint : string;
        (** non-reversible SHA-256 prefix of the verified token *)
    report_notes : string list;  (** safe, human-readable verified facts *)
  }

  (** [Ok report] only when the configured project was positively verified.
      [Error (class, safe_detail)] on mismatch, missing project, wrong
      region, malformed response, missing scope, network failure, timeout or
      incomplete configuration. *)
  val run : unit -> (report, string * string) result Lwt.t
end

(** Test seams: pure payload builders and a capture sink that replaces the
    HTTP transport. The sink only observes payloads produced by the closed
    API above — it is not a bypass capable of arbitrary capture. *)
module For_testing : sig
  val consent_of_cookie_header :
    string option -> [ `Granted | `Denied | `Unknown ]

  val event_payload :
    api_key:string ->
    environment:deployment_environment ->
    distinct_id:string ->
    event ->
    Yojson.Safe.t

  val person_sync_payload :
    api_key:string ->
    environment:deployment_environment ->
    distinct_id:string ->
    person_properties ->
    Yojson.Safe.t

  val group_identify_payload :
    api_key:string ->
    environment:deployment_environment ->
    distinct_id:string ->
    community_group ->
    Yojson.Safe.t

  (** When set, capture dispatch calls the sink synchronously instead of
      performing HTTP. Sink exceptions are swallowed like transport errors. *)
  val set_capture_sink : (Yojson.Safe.t -> unit) -> unit

  val clear_capture_sink : unit -> unit

  (** Config overrides so tests never read the process environment or hit the
      network. The test token is a dummy value, not a real credential. *)
  val use_enabled_test_configuration : unit -> unit

  (** Deletion-client tests: enabled test configuration whose private
      Persons-API host points at a local stub. Dummy values only. *)
  val use_deletion_test_configuration :
    ui_host:string ->
    project_id:string option ->
    personal_api_key:string option ->
    unit ->
    unit

  val use_disabled_test_configuration : unit -> unit

  (** Preflight tests: a fully populated enabled configuration whose private
      API host points at a local HTTP stub. Dummy values only. *)
  val use_preflight_test_configuration :
    environment:deployment_environment ->
    ui_host:string ->
    project_id:string ->
    personal_api_key:string ->
    project_token:string ->
    unit ->
    unit

  (** Pure drive of the closed environment/activation validator on raw
      values, exactly as if they came from the process environment. Returns
      [(analytics_enabled, normalized_environment, diagnostics)]. *)
  val validate_environment_configuration :
    ?enabled:string ->
    ?environment:string ->
    ?allow_development:string ->
    ?project_token:string ->
    ?api_host:string ->
    ?ui_host:string ->
    ?project_id:string ->
    ?personal_api_key:string ->
    ?public_origin:string ->
    unit ->
    bool * string option * string list

  (** Installs the configuration the validator produced from the given raw
      values, so layout/browser-config tests exercise exactly what a given
      environment would run with. *)
  val install_validated_configuration :
    ?enabled:string ->
    ?environment:string ->
    ?allow_development:string ->
    ?project_token:string ->
    ?api_host:string ->
    ?ui_host:string ->
    ?project_id:string ->
    ?personal_api_key:string ->
    ?public_origin:string ->
    unit ->
    unit

  val clear_configuration_override : unit -> unit

  (** [(env var name, is set)] pairs for the active configuration — presence
      booleans only, never values, so secrets cannot leak through it. *)
  val config_report : unit -> (string * bool) list
end
