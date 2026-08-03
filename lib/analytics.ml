(* PostHog server-side analytics.
   Precedents: lib/turnstile.ml (env-driven optional external HTTP service) and
   lib/realtime.ml (fire-and-forget Cohttp POST with timeout). Payloads target
   the Capture API single-event endpoint POST <api_host>/i/v0/e/ with
   {api_key, event, distinct_id, properties}; $set and $groupidentify are
   event types on the same endpoint. *)

open Lwt.Infix

(* === Closed models === *)

type person_properties = {
  username : string;
  signup_date : string;
  is_admin : bool;
}

(* Fully private communities (§13) keep the numeric id and the closed
   visibility value but never expose human-readable identity: slug and name
   are None there — explicit optional fields, not empty-string sentinels. *)
type community_group = {
  community_id : int;
  community_slug : string option;
  community_name : string option;
  community_visibility : string;
  created_at : string option;
}

type response_mode = Response_json | Response_redirect

(* The moderator's closed verdict on a hosted-home request. Mirrors the
   review store's Accept/Reject decision, but stays an analytics-local type
   so this module never depends on a transactional store. *)
type review_decision = Review_accepted | Review_rejected

(* Which of the two removal routes emitted the request. Product surface only
   — deliberately NOT the authorization source: the removal store admits a
   steward, a top moderator, or a durable admin from either route, and
   exporting which of those applied would leak durable role state. *)
type removal_surface = Removal_project_route | Removal_community_route

(* The committed exposure of a published network community. Kept
   analytics-local rather than reusing
   Network_community_publication_form.publication_visibility: that module
   sits below the legacy Db macro-module in the dependency graph, and
   analytics must not pull it in. The publication handler maps the store's
   closed value onto this one exhaustively, so the two vocabularies cannot
   drift apart silently — and, like the domain type, there is deliberately
   no Private constructor to represent. *)
type publication_visibility = Published_public | Published_unlisted

type event =
  | Account_signed_up of { user_id : int; person : person_properties }
  | Account_logged_in of { user_id : int; person : person_properties }
  | Community_joined of {
      user_id : int;
      community_id : int;
      community_slug : string option;
          (* None for fully private communities (§13): ids stay, readable
             identifiers never leave the server. *)
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
  (* The ORIGIN community and the canonical post only: the closed record
     cannot carry the destination community or the private request note, and
     it is captured only after the placement store's transaction committed. *)
  | Shared_thread_request_submitted of {
      user_id : int;
      community_id : int;
      post_id : int;
    }
  | Conversation_promoted of {
      user_id : int;
      community_id : int;
      community_slug : string option;
      channel_id : int;
      channel_slug : string option;
      section_id : int option;
      post_id : int;
      message_id : int64;
      promoted_message_count : int;
      promoted_participant_count : int option;
    }
  | Account_deleted
      (* Personless aggregate deletion counter (§3.3): carries NO user_id, no
         person, no group. Person processing is disabled on the payload, so it
         can never be associated with (or recreate) the person the deletion
         job is about to remove. *)
  (* --- GitHub-anchored project and community-home funnels ---
     Every constructor below is deliberately identifier-poor: no GitHub
     installation, account or repository id, no login or namespace, no
     repository name, no project or community name or slug, no request or
     review note. The only durable identifier any of them carries is the
     acting user (already the distinct id) and, where an existing public
     accessor supplies it, the permanent Earde project id. *)
  | Github_app_install_started of { user_id : int }
      (* The authenticated, rollout- and origin-authorized start of a GitHub
         App installation, after the durable onboarding state row committed
         and the browser is about to be redirected to GitHub. *)
  | Github_app_installed of { user_id : int }
      (* The OAuth callback completed: state validated and consumed, the
         installation verified against the GitHub user, and the installation
         record plus refreshed draft committed. The verified installation id,
         account id and login stay behind in the handler. *)
  | Github_repositories_selected of { user_id : int; repository_count : int }
  | Github_project_created of {
      user_id : int;
      project_id : int64;  (* the permanent open_source_projects row id *)
      project_kind : Project_identity.kind;
      repository_count : int;
    }
  | Dedicated_home_provisioned of { user_id : int }
      (* The private setup draft, its initial role/shell, the accepted home
         relation and the audit event all committed. Deliberately NOT
         "community_published": nothing is public yet. *)
  | Network_community_published of {
      user_id : int;
      publication_visibility : publication_visibility;
          (* the committed exposure; the closed type has no Private *)
    }
  | Project_home_request_submitted of { user_id : int }
  | Project_home_request_reviewed of {
      user_id : int;  (* the reviewing moderator, not the requesting steward *)
      decision : review_decision;
    }
  | Project_home_removed of { user_id : int; removal_surface : removal_surface }

let distinct_id_of_user_id user_id = Printf.sprintf "user:%d" user_id

(* Constant non-user distinct id for the personless account_deleted metric.
   With $process_person_profile=false no person profile is ever created for
   it; the constant only pools the aggregate counter. *)
let account_deletion_distinct_id = "system:account-deletion"

(* Group keys use the immutable numeric id — slugs are mutable (§5.3). *)
let community_group_key community_id = Printf.sprintf "community:%d" community_id

(* "community" is the project's first (and only) PostHog group type (§5.3),
   so its group_type_index is 0. Used by the §13 private Groups-API cleanup. *)
let community_group_type_index = 0

(* === Closed deployment environment === *)

(* One closed server-side model of "where is this process running". It is set
   ONLY by EARDE_DEPLOYMENT_ENVIRONMENT — never inferred from hostname, branch
   name or executable mode — and never retained as an arbitrary string. *)
type deployment_environment = Production | Staging | Development

let deployment_environment_to_string = function
  | Production -> "production"
  | Staging -> "staging"
  | Development -> "development"

(* Exact closed parse: the canonical lowercase spellings only. Case variants,
   abbreviations and unknown values are rejected so the caller fails closed
   instead of guessing. *)
let deployment_environment_of_string = function
  | "production" -> Some Production
  | "staging" -> Some Staging
  | "development" -> Some Development
  | _ -> None

(* === Configuration === *)

let enabled_env = "POSTHOG_ENABLED"
let deployment_environment_env = "EARDE_DEPLOYMENT_ENVIRONMENT"
let allow_development_env = "POSTHOG_ALLOW_DEVELOPMENT"
let project_token_env = "POSTHOG_PROJECT_TOKEN"
let api_host_env = "POSTHOG_API_HOST"
let ui_host_env = "POSTHOG_UI_HOST"
let project_id_env = "POSTHOG_PROJECT_ID"
let personal_api_key_env = "POSTHOG_PERSONAL_API_KEY"
let public_origin_env = "EARDE_PUBLIC_ORIGIN"

let default_api_host = "https://eu.i.posthog.com"
let default_ui_host = "https://eu.posthog.com"

(* The one exact production browser origin. Production analytics binds to it
   exactly; staging must be a DIFFERENT https origin (no staging hostname is
   hardcoded here — none has been chosen). *)
let production_origin = "https://earde.com"
let production_www_origin = "https://www.earde.com"

type config = {
  enabled : bool;
  (* Some ⇒ the closed environment parsed exactly; [enabled] additionally
     requires it (analytics never runs without a validated environment). *)
  environment : deployment_environment option;
  project_token : string;
  api_host : string;
  (* Held for the consumers below (§3.3 deletion lifecycle, §9 consent
     endpoint, preflight). personal_api_key and project_id are server-only and
     must never be rendered into browser configuration or logged. *)
  ui_host : string;
  project_id : string option;
  personal_api_key : string option;
  public_origin : string option;
}

let strip_trailing_slash value =
  let len = String.length value in
  if len > 0 && value.[len - 1] = '/' then String.sub value 0 (len - 1)
  else value

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

let is_https url = starts_with ~prefix:"https://" url

let is_positive_int s =
  match int_of_string_opt s with Some n -> n > 0 | None -> false

(* Development opt-in accepts only loopback origins — localhost, 127.0.0.1 or
   ::1, any port — over http or https. Everything else (including the
   production origin and arbitrary remote hosts) is rejected. *)
let is_local_origin origin =
  let uri = Uri.of_string origin in
  (match Uri.scheme uri with Some "http" | Some "https" -> true | _ -> false)
  && (match Uri.host uri with
     | Some ("localhost" | "127.0.0.1" | "::1") -> true
     | _ -> false)

(* Pure fail-closed resolution of the raw environment values. Returns the
   resolved config plus startup diagnostics (variable NAMES and requirements
   only — never values). Every rule here is per-environment activation:

   - POSTHOG_ENABLED anything but exactly "true" ⇒ analytics off, and no other
     variable is required (local development keeps working with nothing set).
   - enabled ⇒ EARDE_DEPLOYMENT_ENVIRONMENT is REQUIRED and must parse through
     the closed type: unknown, missing, blank or differently-cased values
     disable analytics with a diagnostic.
   - development is additionally disabled unless POSTHOG_ALLOW_DEVELOPMENT is
     exactly "true" (an explicit, diagnosed opt-in to a NON-production
     project).
   - every enabled environment requires the complete PostHog configuration:
     project token, https api/ui hosts, positive-integer project id, personal
     API key, and an origin bound to the environment (production: exactly
     https://earde.com; staging: any OTHER https origin; development opt-in:
     loopback only).

   Production and staging are expected to point at SEPARATE PostHog projects
   (separate tokens, ids and personal keys); that project separation — not the
   deployment_environment event property — is the isolation boundary, and the
   preflight below verifies each deployment's token/project binding. *)
let validate_configuration ~enabled ~environment ~allow_development
    ~project_token ~api_host ~ui_host ~project_id ~personal_api_key
    ~public_origin =
  let norm v =
    match v with
    | None -> None
    | Some s ->
        let s = String.trim s in
        if s = "" then None else Some s
  in
  let host_or raw default =
    match norm raw with Some h -> strip_trailing_slash h | None -> default
  in
  let api_host = host_or api_host default_api_host in
  let ui_host = host_or ui_host default_ui_host in
  let project_id = norm project_id in
  let personal_api_key = norm personal_api_key in
  let public_origin = norm public_origin in
  let parsed_environment =
    Option.bind (norm environment) deployment_environment_of_string
  in
  let base ~enabled ~project_token =
    {
      enabled;
      environment = parsed_environment;
      project_token;
      api_host;
      ui_host;
      project_id;
      personal_api_key;
      public_origin;
    }
  in
  let disabled diagnostics = (base ~enabled:false ~project_token:"", diagnostics) in
  if norm enabled <> Some "true" then disabled []
  else
    match parsed_environment with
    | None ->
        disabled
          [
            Printf.sprintf
              "%s must be exactly production, staging or development when \
               %s=true; unknown, missing, blank or differently-cased values \
               disable analytics"
              deployment_environment_env enabled_env;
          ]
    | Some Development when norm allow_development <> Some "true" ->
        disabled
          [
            Printf.sprintf
              "%s=development keeps analytics disabled by default; set \
               %s=true to explicitly connect local analytics to a \
               non-production PostHog project"
              deployment_environment_env allow_development_env;
          ]
    | Some env -> (
        let diagnostics = ref [] in
        let fail msg = diagnostics := !diagnostics @ [ msg ] in
        (match norm project_token with
        | None -> fail (project_token_env ^ " is required when analytics is enabled")
        | Some _ -> ());
        if not (is_https api_host) then fail (api_host_env ^ " must use HTTPS");
        if not (is_https ui_host) then fail (ui_host_env ^ " must use HTTPS");
        (match project_id with
        | None -> fail (project_id_env ^ " is required when analytics is enabled")
        | Some id when not (is_positive_int id) ->
            fail (project_id_env ^ " must be a positive integer")
        | Some _ -> ());
        (match personal_api_key with
        | None ->
            fail (personal_api_key_env ^ " is required when analytics is enabled")
        | Some _ -> ());
        (match public_origin with
        | None -> fail (public_origin_env ^ " is required when analytics is enabled")
        | Some origin -> (
            match env with
            | Production ->
                if origin <> production_origin then
                  fail
                    (Printf.sprintf "%s must be exactly %s in production"
                       public_origin_env production_origin)
            | Staging ->
                if not (is_https origin) then
                  fail (public_origin_env ^ " must be an HTTPS origin in staging")
                else if origin = production_origin || origin = production_www_origin
                then
                  fail
                    (public_origin_env
                   ^ " must not be a production origin in staging")
            | Development ->
                if not (is_local_origin origin) then
                  fail
                    (public_origin_env
                   ^ " must be a localhost, 127.0.0.1 or ::1 origin (http or \
                      https) for the development opt-in")));
        match (!diagnostics, norm project_token) with
        | [], Some token ->
            let notices =
              match env with
              | Development ->
                  [
                    Printf.sprintf
                      "development analytics EXPLICITLY ENABLED via %s=true; \
                       events will be sent to the configured (non-production) \
                       PostHog project"
                      allow_development_env;
                  ]
              | Production | Staging -> []
            in
            (base ~enabled:true ~project_token:token, notices)
        | diags, _ -> disabled diags)

let config_from_env () =
  let config, diagnostics =
    validate_configuration
      ~enabled:(Sys.getenv_opt enabled_env)
      ~environment:(Sys.getenv_opt deployment_environment_env)
      ~allow_development:(Sys.getenv_opt allow_development_env)
      ~project_token:(Sys.getenv_opt project_token_env)
      ~api_host:(Sys.getenv_opt api_host_env)
      ~ui_host:(Sys.getenv_opt ui_host_env)
      ~project_id:(Sys.getenv_opt project_id_env)
      ~personal_api_key:(Sys.getenv_opt personal_api_key_env)
      ~public_origin:(Sys.getenv_opt public_origin_env)
  in
  List.iter
    (fun msg ->
      if config.enabled then Logs.warn (fun m -> m "PostHog analytics: %s" msg)
      else Logs.warn (fun m -> m "PostHog analytics disabled: %s" msg))
    diagnostics;
  config

(* Lazy because analytics is not wired in bin/main.ml yet; the first caller
   resolves the environment once. Tests install an override instead so they
   never depend on the process environment. *)
let env_config = lazy (config_from_env ())
let config_override : config option ref = ref None

let current_config () =
  match !config_override with
  | Some config -> config
  | None -> Lazy.force env_config

(* Enabled analytics implies a validated closed environment; any other shape
   (possible only through a hand-built test override) is treated as disabled. *)
let active_config () =
  let c = current_config () in
  match (c.enabled, c.environment) with
  | true, Some environment -> Some (c, environment)
  | _ -> None

(* === Browser configuration (strictly public values) === *)

type browser_config = {
  browser_token : string;
  browser_api_host : string;
  (* The normalized closed deployment environment — the only environment
     value the browser ever sees, so its events can carry the same
     diagnostic property as server events. *)
  browser_deployment_environment : string;
}

(* Only the public write-only project token, the ingest host and the
   normalized closed deployment environment ever reach the browser.
   personal_api_key and project_id are deliberately unreachable from here.
   None ⇒ render no banner, no config attributes, no scripts. *)
let browser_config () =
  match active_config () with
  | Some (c, environment) ->
      Some
        {
          browser_token = c.project_token;
          browser_api_host = c.api_host;
          browser_deployment_environment =
            deployment_environment_to_string environment;
        }
  | None -> None

(* === Private Persons-API configuration (server-only, §3.3) === *)

type deletion_api_config = {
  deletion_ui_host : string;
  deletion_project_id : string;
  deletion_api_key : string;
}

(* Deletion is a data-lifecycle duty, not analytics collection: it is
   available whenever the private credentials exist, independent of
   POSTHOG_ENABLED. Warn (once, by variable NAME only — never a value) when
   exactly one of the two credentials is set, since that is almost certainly a
   deployment mistake that leaves deletion jobs pending. *)
let warned_partial_deletion_config = ref false

let deletion_api_config () =
  let c = current_config () in
  match (c.project_id, c.personal_api_key) with
  | Some project_id, Some api_key ->
      Some
        {
          deletion_ui_host = c.ui_host;
          deletion_project_id = project_id;
          deletion_api_key = api_key;
        }
  | (Some _, None | None, Some _) when not !warned_partial_deletion_config ->
      warned_partial_deletion_config := true;
      Logs.warn (fun m ->
          m "only one of %s and %s is set; PostHog person deletion stays pending"
            project_id_env personal_api_key_env);
      None
  | _ -> None

(* === Consent (pure) === *)

let consent_cookie_name = "earde_analytics_consent"

(* ~180 days, in seconds, for the cookie Max-Age (§9). *)
let consent_cookie_max_age = 15552000.0

let allowed_public_origin () =
  let c = current_config () in
  if c.enabled then c.public_origin else None

(* Secure flag follows the configured public origin's scheme: https in
   production ⇒ Secure; http in local dev ⇒ not, so the cookie still works. *)
let consent_cookie_secure () =
  match allowed_public_origin () with
  | Some origin ->
      String.length origin >= 8 && String.sub origin 0 8 = "https://"
  | None -> false

(* §9 route-specific request protection for POST /analytics/consent. This
   endpoint deliberately has no Dream CSRF token (the static landing cannot
   obtain one) and requires no session; instead: exact Origin match against
   EARDE_PUBLIC_ORIGIN, same-origin/same-site Sec-Fetch-Site, JSON-only
   content type, and a body of exactly {"state": "granted"|"denied"}. *)
let validate_consent_request ~content_type ~origin ~sec_fetch_site ~body =
  let origin_ok =
    match (origin, allowed_public_origin ()) with
    | Some o, Some allowed -> String.trim o = allowed
    | _ -> false
  in
  let fetch_site_ok =
    match Option.map String.trim sec_fetch_site with
    | Some "same-origin" | Some "same-site" -> true
    | _ -> false
  in
  let is_json =
    match content_type with
    | None -> false
    | Some ct -> (
        let ct = String.lowercase_ascii (String.trim ct) in
        let mime = "application/json" in
        let ml = String.length mime in
        String.length ct >= ml
        && String.sub ct 0 ml = mime
        && (String.length ct = ml || ct.[ml] = ';'))
  in
  if not origin_ok then Error (`Forbidden "origin not allowed")
  else if not fetch_site_ok then Error (`Forbidden "fetch metadata not allowed")
  else if not is_json then Error (`Bad_request "expected application/json")
  else
    match Yojson.Safe.from_string body with
    | exception _ -> Error (`Bad_request "invalid JSON")
    | `Assoc [ ("state", `String "granted") ] -> Ok `Granted
    | `Assoc [ ("state", `String "denied") ] -> Ok `Denied
    | _ ->
        Error
          (`Bad_request "body must be exactly {\"state\":\"granted\"|\"denied\"}")

(* Reference implementation of the §2.3 URL rule, mirrored by analytics.js:
   analytics URLs carry origin + path only — never query strings (which hold
   confirmation/reset tokens and search text) and never fragments. *)
let sanitize_url_for_analytics raw =
  let uri = Uri.of_string raw in
  Uri.to_string (Uri.with_fragment (Uri.with_query uri []) None)

(* Exact parse of the raw Cookie header: the consent cookie is a plaintext,
   JS-readable cookie (§9), so no Dream cookie decryption applies. Anything
   other than the exact values "granted"/"denied" is `Unknown. *)
let consent_of_cookie_header header =
  match header with
  | None -> `Unknown
  | Some raw ->
      let rec find = function
        | [] -> `Unknown
        | part :: rest -> (
            let part = String.trim part in
            match String.index_opt part '=' with
            | Some i when String.sub part 0 i = consent_cookie_name -> (
                match
                  String.sub part (i + 1) (String.length part - i - 1)
                with
                | "granted" -> `Granted
                | "denied" -> `Denied
                | _ -> `Unknown)
            | _ -> find rest)
      in
      find (String.split_on_char ';' raw)

(* === Payload builders (pure) === *)

let json_int64 value = `Intlit (Int64.to_string value)

let response_mode_to_string = function
  | Response_json -> "json"
  | Response_redirect -> "redirect"

let opt_int name = function None -> [] | Some v -> [ (name, `Int v) ]
let opt_string name = function None -> [] | Some v -> [ (name, `String v) ]

let review_decision_to_string = function
  | Review_accepted -> "accepted"
  | Review_rejected -> "rejected"

let removal_surface_to_string = function
  | Removal_project_route -> "project"
  | Removal_community_route -> "community"

let publication_visibility_to_string = function
  | Published_public -> "public"
  | Published_unlisted -> "unlisted"

(* A draft repository snapshot is structurally bounded to 1..2,000 rows and a
   saved selection is a subset of it, so 0..2,000 is the entire legitimate
   range of a repository count. A value outside it is durable corruption, not
   a measurement: the property is omitted rather than exported as a nonsense
   number that would silently distort a funnel. *)
let max_repository_count = 2000

let bounded_count name value =
  if value >= 0 && value <= max_repository_count then [ (name, `Int value) ]
  else []

(* Internal row ids are positive by construction. A non-positive value can
   only mean the caller lost the real id, so nothing is exported for it. *)
let positive_id64 name value =
  if Int64.compare value 0L > 0 then [ (name, json_int64 value) ] else []

let event_name = function
  | Account_signed_up _ -> "account_signed_up"
  | Account_logged_in _ -> "account_logged_in"
  | Community_joined _ -> "community_joined"
  | Community_left _ -> "community_left"
  | Chat_message_sent _ -> "chat_message_sent"
  | Forum_thread_created _ -> "forum_thread_created"
  | Forum_comment_created _ -> "forum_comment_created"
  | Shared_thread_request_submitted _ -> "shared_thread_request_submitted"
  | Conversation_promoted _ -> "conversation_promoted"
  | Account_deleted -> "account_deleted"
  | Github_app_install_started _ -> "github_app_install_started"
  | Github_app_installed _ -> "github_app_installed"
  | Github_repositories_selected _ -> "github_repositories_selected"
  | Github_project_created _ -> "github_project_created"
  | Dedicated_home_provisioned _ -> "dedicated_home_provisioned"
  | Network_community_published _ -> "network_community_published"
  | Project_home_request_submitted _ -> "project_home_request_submitted"
  | Project_home_request_reviewed _ -> "project_home_request_reviewed"
  | Project_home_removed _ -> "project_home_removed"

(* Community-scoped events carry $groups.community (§5.3); identity/lifecycle
   events do not. *)
let event_community_id = function
  | Account_signed_up _ | Account_logged_in _ | Account_deleted -> None
  (* The GitHub-project and community-home lifecycle events carry no
     $groups.community: the numeric community id is simply not reachable at
     their success boundaries. The publication and provisioning stores return
     the committed community's canonical SLUG only, and the review, removal
     and request handlers work from route slugs — and a slug is deliberately
     never a group key (§5.3: keys are the immutable numeric id). Recovering
     the id would mean handler-side SQL run purely for analytics. *)
  | Github_app_install_started _ | Github_app_installed _
  | Github_repositories_selected _ | Github_project_created _
  | Dedicated_home_provisioned _ | Network_community_published _
  | Project_home_request_submitted _ | Project_home_request_reviewed _
  | Project_home_removed _ ->
      None
  | Community_joined { community_id; _ }
  | Community_left { community_id; _ }
  | Chat_message_sent { community_id; _ }
  | Forum_thread_created { community_id; _ }
  | Forum_comment_created { community_id; _ }
  (* Scoped to the ORIGIN community — the id the composer handler holds. *)
  | Shared_thread_request_submitted { community_id; _ }
  | Conversation_promoted { community_id; _ } ->
      Some community_id

(* The closed §4.3 person properties, as the $set object. Only the two
   identity events (account_signed_up, account_logged_in) and the consent
   transition may carry it; person properties never appear as ordinary
   top-level event properties. Deliberately email-free: user:<id> is the
   stable identity and no analytics need justifies ingesting email. *)
let person_set_json (p : person_properties) =
  `Assoc
    [
      ("username", `String p.username);
      ("signup_date", `String p.signup_date);
      ("is_admin", `Bool p.is_admin);
    ]

let event_properties = function
  | Account_signed_up { user_id; person } ->
      [ ("user_id", `Int user_id); ("$set", person_set_json person) ]
  | Account_logged_in { user_id; person } ->
      [ ("user_id", `Int user_id); ("$set", person_set_json person) ]
  | Community_joined { user_id; community_id; community_slug; community_visibility }
    ->
      [ ("user_id", `Int user_id); ("community_id", `Int community_id) ]
      @ opt_string "community_slug" community_slug
      @ [ ("community_visibility", `String community_visibility) ]
  | Community_left { user_id; community_id } ->
      [ ("user_id", `Int user_id); ("community_id", `Int community_id) ]
  | Chat_message_sent
      {
        user_id;
        community_id;
        community_slug;
        channel_id;
        channel_slug;
        message_id;
        content_length;
        response_mode;
      } ->
      [ ("user_id", `Int user_id); ("community_id", `Int community_id) ]
      @ opt_string "community_slug" community_slug
      @ [ ("channel_id", `Int channel_id) ]
      @ opt_string "channel_slug" channel_slug
      @ [
          ("message_id", json_int64 message_id);
          ("content_length", `Int content_length);
          ("response_mode", `String (response_mode_to_string response_mode));
        ]
  | Forum_thread_created
      { user_id; community_id; section_id; post_id; content_length; has_link;
        has_mention } ->
      [ ("user_id", `Int user_id); ("community_id", `Int community_id) ]
      @ opt_int "section_id" section_id
      @ [
          ("post_id", `Int post_id);
          ("content_length", `Int content_length);
          ("has_link", `Bool has_link);
          ("has_mention", `Bool has_mention);
        ]
  | Forum_comment_created
      { user_id; community_id; post_id; comment_id; parent_comment_id;
        content_length; has_mention } ->
      [
        ("user_id", `Int user_id);
        ("community_id", `Int community_id);
        ("post_id", `Int post_id);
        ("comment_id", `Int comment_id);
      ]
      @ opt_int "parent_comment_id" parent_comment_id
      @ [
          ("content_length", `Int content_length);
          ("has_mention", `Bool has_mention);
        ]
  | Shared_thread_request_submitted { user_id; community_id; post_id } ->
      [
        ("user_id", `Int user_id);
        ("community_id", `Int community_id);
        ("post_id", `Int post_id);
      ]
  | Conversation_promoted
      {
        user_id;
        community_id;
        community_slug;
        channel_id;
        channel_slug;
        section_id;
        post_id;
        message_id;
        promoted_message_count;
        promoted_participant_count;
      } ->
      [ ("user_id", `Int user_id); ("community_id", `Int community_id) ]
      @ opt_string "community_slug" community_slug
      @ [ ("channel_id", `Int channel_id) ]
      @ opt_string "channel_slug" channel_slug
      @ opt_int "section_id" section_id
      @ [
          ("post_id", `Int post_id);
          ("message_id", json_int64 message_id);
          ("promoted_message_count", `Int promoted_message_count);
        ]
      @ opt_int "promoted_participant_count" promoted_participant_count
  | Account_deleted ->
      (* Raw Capture API anonymous-event mechanism (verified against current
         docs, api/capture "Anonymous event capture"): person processing off,
         no other properties — an aggregate counter only. *)
      [ ("$process_person_profile", `Bool false) ]
  | Github_app_install_started { user_id } | Github_app_installed { user_id } ->
      [ ("user_id", `Int user_id) ]
  | Github_repositories_selected { user_id; repository_count } ->
      [ ("user_id", `Int user_id) ]
      @ bounded_count "repository_count" repository_count
  | Github_project_created
      { user_id; project_id; project_kind; repository_count } ->
      [ ("user_id", `Int user_id) ]
      @ positive_id64 "project_id" project_id
      @ [ ("project_kind", `String (Project_identity.string_of_kind project_kind)) ]
      @ bounded_count "repository_count" repository_count
  | Dedicated_home_provisioned { user_id }
  | Project_home_request_submitted { user_id } ->
      [ ("user_id", `Int user_id) ]
  | Network_community_published { user_id; publication_visibility } ->
      [
        ("user_id", `Int user_id);
        ( "publication_visibility",
          `String (publication_visibility_to_string publication_visibility) );
      ]
  | Project_home_request_reviewed { user_id; decision } ->
      [
        ("user_id", `Int user_id);
        ("decision", `String (review_decision_to_string decision));
      ]
  | Project_home_removed { user_id; removal_surface } ->
      [
        ("user_id", `Int user_id);
        ("removal_surface", `String (removal_surface_to_string removal_surface));
      ]

(* The one shared payload envelope. deployment_environment is appended HERE,
   exactly once, for every eligible PostHog payload (domain events,
   $groupidentify, the $identify person sync) — event constructors cannot
   carry it (closed variants), so it can never be duplicated or spoofed with
   an arbitrary string. It is diagnostic context only: the isolation boundary
   between environments is the separate PostHog project/token, not this
   property. *)
let capture_payload ~api_key ~environment ~distinct_id ~name ~properties :
    Yojson.Safe.t =
  `Assoc
    [
      ("api_key", `String api_key);
      ("event", `String name);
      ("distinct_id", `String distinct_id);
      ( "properties",
        `Assoc
          (properties
          @ [
              ( "deployment_environment",
                `String (deployment_environment_to_string environment) );
            ]) );
    ]

let event_payload ~api_key ~environment ~distinct_id event =
  let groups =
    match event_community_id event with
    | None -> []
    | Some community_id ->
        [
          ( "$groups",
            `Assoc [ ("community", `String (community_group_key community_id)) ]
          );
        ]
  in
  capture_payload ~api_key ~environment ~distinct_id ~name:(event_name event)
    ~properties:(event_properties event @ groups)

(* Consent-transition sync: a dedicated $identify payload carrying the same
   closed $set object, used only by sync_person_after_consent_grant. *)
let person_sync_payload ~api_key ~environment ~distinct_id
    (p : person_properties) =
  capture_payload ~api_key ~environment ~distinct_id ~name:"$identify"
    ~properties:[ ("$set", person_set_json p) ]

let group_identify_payload ~api_key ~environment ~distinct_id
    (g : community_group) =
  let group_set =
    [ ("community_id", `Int g.community_id) ]
    @ opt_string "community_slug" g.community_slug
    @ opt_string "community_name" g.community_name
    @ [ ("community_visibility", `String g.community_visibility) ]
    @ opt_string "created_at" g.created_at
  in
  capture_payload ~api_key ~environment ~distinct_id ~name:"$groupidentify"
    ~properties:
      [
        ("$group_type", `String "community");
        ("$group_key", `String (community_group_key g.community_id));
        ("$group_set", `Assoc group_set);
      ]

(* === Dispatch (fire-and-forget) === *)

let capture_timeout_seconds = 3.0

let capture_sink : (Yojson.Safe.t -> unit) option ref = ref None

let with_timeout seconds promise =
  Lwt.pick
    [
      promise;
      ( Lwt_unix.sleep seconds >>= fun () ->
        Logs.warn (fun m ->
            m "PostHog capture timed out after %.1fs" seconds);
        Lwt.return_unit );
    ]

let post_capture ~api_host payload =
  let uri = Uri.of_string (api_host ^ "/i/v0/e/") in
  let body = payload |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string in
  let headers =
    Cohttp.Header.init () |> fun h ->
    Cohttp.Header.add h "content-type" "application/json"
  in
  Cohttp_lwt_unix.Client.post ~headers ~body uri
  >>= fun (response, response_body) ->
  Cohttp_lwt.Body.drain_body response_body >|= fun () ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if status < 200 || status >= 300 then
    (* Log the status only: the payload embeds the project token and the
       response body is uncontrolled third-party data. *)
    Logs.warn (fun m -> m "PostHog capture failed: status=%d" status)

(* Awaitable dispatch: resolves after the sink call or the bounded transport
   attempt (timeout included). Every failure is swallowed — the returned
   promise never rejects. *)
let dispatch_await config payload =
  match !capture_sink with
  | Some sink ->
      (* Test transport. Failures are swallowed exactly like HTTP failures:
         analytics can never affect the caller. *)
      (try sink payload with _ -> ());
      Lwt.return_unit
  | None ->
      Lwt.catch
        (fun () ->
          with_timeout capture_timeout_seconds
            (post_capture ~api_host:config.api_host payload))
        (fun exn ->
          Logs.warn (fun m ->
              m "PostHog capture exception: %s" (Printexc.to_string exn));
          Lwt.return_unit)

let dispatch config payload =
  match !capture_sink with
  | Some sink -> ( try sink payload with _ -> ())
  | None -> Lwt.async (fun () -> dispatch_await config payload)

(* === Public API === *)

let capture_if_consented request ~distinct_id event =
  match active_config () with
  | None -> ()
  | Some (config, environment) -> (
      match consent_of_cookie_header (Dream.header request "Cookie") with
      | `Granted ->
          dispatch config
            (event_payload ~api_key:config.project_token ~environment
               ~distinct_id event)
      | `Denied | `Unknown -> ())

let sync_person_after_consent_grant ~distinct_id person =
  match active_config () with
  | None -> ()
  | Some (config, environment) ->
      dispatch config
        (person_sync_payload ~api_key:config.project_token ~environment
           ~distinct_id person)

(* Narrow §3.3 orchestration seam for the account-deletion flow only. The
   metric is PERSONLESS by construction — constant system distinct id,
   $process_person_profile=false, no properties — so ingestion timing relative
   to the Persons-API deletion cannot associate it with (or recreate) the
   deleted person; the await only sequences the HTTP requests, it proves
   nothing about ingestion. Same gate as capture_if_consented — disabled
   analytics or absent/denied consent resolves immediately with no side
   effect — and it can emit only the closed Account_deleted event. *)
let capture_account_deleted_sequenced request =
  match active_config () with
  | None -> Lwt.return_unit
  | Some (config, environment) -> (
      match consent_of_cookie_header (Dream.header request "Cookie") with
      | `Granted ->
          dispatch_await config
            (event_payload ~api_key:config.project_token ~environment
               ~distinct_id:account_deletion_distinct_id Account_deleted)
      | `Denied | `Unknown -> Lwt.return_unit)

(* $groupidentify shares capture_if_consented's exact gate: enabled AND the
   request's consent cookie is exactly "granted". It accepts only the closed
   community_group record — not a generic capture path — and the caller must
   supply the acting authenticated user's "user:<id>" as distinct_id (PostHog
   attributes the event to that person; a synthetic id would mint a phantom
   person). *)
let identify_community_if_consented request ~distinct_id group =
  match active_config () with
  | None -> ()
  | Some (config, environment) -> (
      match consent_of_cookie_header (Dream.header request "Cookie") with
      | `Granted ->
          dispatch config
            (group_identify_payload ~api_key:config.project_token ~environment
               ~distinct_id group)
      | `Denied | `Unknown -> ())

(* === Credential/project preflight (server-only) ==========================
   Verifies, WITHOUT ingesting any event, that the configured deployment
   really points at the PostHog project it claims to: POSTHOG_PROJECT_ID
   exists on the configured region host, and its api_token equals
   POSTHOG_PROJECT_TOKEN — i.e. the binding is checked against the live
   project metadata, never by merely comparing two variables from the same
   env file.

   Documented mechanism (verified read-only against the official PostHog
   OpenAPI schema and API reference, 2026-07):
     GET /api/organizations/                                   scope organization:read
     GET /api/organizations/:organization_id/projects/:id/     scope project:read
   The project serializer (ProjectBackwardCompat) exposes the read-only
   [api_token] field, which IS the public project token. Both requests run
   against the private-API host (POSTHOG_UI_HOST) with the personal API key —
   never the ingest host, never the capture endpoint. *)

module Preflight = struct
  type report = {
    report_environment : deployment_environment;
    report_project_id : string;
    report_api_host : string;
    report_ui_host : string;
    (* Non-reversible short fingerprint (SHA-256 prefix) of the verified
       project token, so operators can tell WHICH token a deployment carries
       without the output ever revealing it. *)
    report_token_fingerprint : string;
    report_notes : string list;
  }

  let timeout_seconds = 10.0

  let token_fingerprint token =
    String.sub Digestif.SHA256.(to_hex (digest_string token)) 0 12

  (* Known PostHog Cloud hosts, for the static ingest/private-API region
     pairing check. Unknown hosts (self-hosted, local test stubs) skip the
     static check — the live project lookup still validates them. *)
  let cloud_region host =
    if host = "https://eu.i.posthog.com" || host = "https://eu.posthog.com" then
      Some "eu"
    else if host = "https://us.i.posthog.com" || host = "https://us.posthog.com"
    then Some "us"
    else None

  let auth_header key =
    Cohttp.Header.init () |> fun h ->
    Cohttp.Header.add h "authorization" ("Bearer " ^ key)

  (* Organization ids are spliced into request paths; accept only plausible
     UUID characters so a malformed listing can never rewrite the URL. *)
  let is_safe_path_id s =
    s <> ""
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true | _ -> false)
         s

  (* Bounded failure classes only. Response bodies are parsed, never echoed;
     credentials never appear in any class or detail. *)
  let classify_error_status status =
    if status = 401 then "unauthorized"
    else if status = 403 then "missing_scope"
    else if status >= 300 && status < 400 then "unexpected_redirect"
    else Printf.sprintf "http_%d" status

  let http_get ~api_key uri =
    Cohttp_lwt_unix.Client.get ~headers:(auth_header api_key) uri
    >>= fun (response, body) ->
    Cohttp_lwt.Body.to_string body >|= fun body ->
    (Cohttp.Response.status response |> Cohttp.Code.code_of_status, body)

  let parse_organization_ids body =
    match Yojson.Safe.from_string body with
    | exception _ -> Error ("malformed_response", "organization listing did not parse")
    | `Assoc fields -> (
        match List.assoc_opt "results" fields with
        | Some (`List orgs) ->
            let ids =
              List.filter_map
                (function
                  | `Assoc o -> (
                      match List.assoc_opt "id" o with
                      | Some (`String id) when is_safe_path_id id -> Some id
                      | _ -> None)
                  | _ -> None)
                orgs
            in
            if orgs = [] then
              Error
                ( "no_organizations",
                  "the personal API key can read no organization on this host" )
            else if List.length ids <> List.length orgs then
              Error
                ( "malformed_response",
                  "organization listing contained an unusable entry" )
            else Ok ids
        | _ -> Error ("malformed_response", "organization listing did not parse"))
    | _ -> Error ("malformed_response", "organization listing did not parse")

  (* The project response must carry the configured numeric id and the
     read-only api_token; the token comparison is the actual binding proof. *)
  let parse_project_body ~project_id body =
    match Yojson.Safe.from_string body with
    | exception _ -> Error ("malformed_response", "project metadata did not parse")
    | `Assoc fields -> (
        match (List.assoc_opt "id" fields, List.assoc_opt "api_token" fields) with
        | Some (`Int id), Some (`String token)
          when string_of_int id = project_id ->
            Ok token
        | Some (`Int _), Some (`String _) ->
            Error
              ( "project_id_mismatch",
                "the project endpoint returned metadata for a different \
                 project id" )
        | _ -> Error ("malformed_response", "project metadata did not parse"))
    | _ -> Error ("malformed_response", "project metadata did not parse")

  let rec find_project ~config ~api_key ~project_id = function
    | [] ->
        Lwt.return
          (Error
             ( "project_not_found",
               Printf.sprintf
                 "project %s was not found in any organization readable by \
                  the personal API key on %s (wrong project id, wrong \
                  region, or a key for another project)"
                 project_id config.ui_host ))
    | org_id :: rest ->
        http_get ~api_key
          (Uri.of_string
             (Printf.sprintf "%s/api/organizations/%s/projects/%s/"
                config.ui_host org_id project_id))
        >>= fun (status, body) ->
        if status = 404 then find_project ~config ~api_key ~project_id rest
        else if status >= 200 && status < 300 then (
          match parse_project_body ~project_id body with
          | Error e -> Lwt.return (Error e)
          | Ok live_token ->
              if String.equal live_token config.project_token then
                Lwt.return (Ok org_id)
              else
                Lwt.return
                  (Error
                     ( "token_mismatch",
                       Printf.sprintf
                         "POSTHOG_PROJECT_TOKEN does not match the api_token \
                          of project %s (configured token fingerprint \
                          sha256:%s, live token fingerprint sha256:%s)"
                         project_id
                         (token_fingerprint config.project_token)
                         (token_fingerprint live_token) )))
        else
          Lwt.return
            (Error
               ( classify_error_status status,
                 Printf.sprintf "project retrieve answered HTTP %d" status ))

  let verify ~config ~environment ~api_key ~project_id =
    match (cloud_region config.api_host, cloud_region config.ui_host) with
    | Some a, Some b when a <> b ->
        Lwt.return
          (Error
             ( "region_mismatch",
               Printf.sprintf
                 "%s is a %s-cloud host but %s is a %s-cloud host" api_host_env
                 a ui_host_env b ))
    | api_region, _ ->
        (* limit=100 keeps the single documented listing call sufficient for
           any realistic key; pagination beyond that is reported as not-found
           rather than guessed at. *)
        http_get ~api_key
          (Uri.with_query'
             (Uri.of_string (config.ui_host ^ "/api/organizations/"))
             [ ("limit", "100") ])
        >>= fun (status, body) ->
        if status >= 200 && status < 300 then (
          match parse_organization_ids body with
          | Error e -> Lwt.return (Error e)
          | Ok org_ids -> (
              find_project ~config ~api_key ~project_id org_ids >|= function
              | Error e -> Error e
              | Ok org_id ->
                  Ok
                    {
                      report_environment = environment;
                      report_project_id = project_id;
                      report_api_host = config.api_host;
                      report_ui_host = config.ui_host;
                      report_token_fingerprint =
                        token_fingerprint config.project_token;
                      report_notes =
                        [
                          Printf.sprintf
                            "project %s found in organization %s on %s"
                            project_id org_id config.ui_host;
                          "POSTHOG_PROJECT_TOKEN matches the project's live \
                           api_token";
                        ]
                        @ (match api_region with
                          | Some region ->
                              [
                                Printf.sprintf
                                  "api/ui hosts are the matching %s-cloud pair"
                                  region;
                              ]
                          | None ->
                              [
                                "hosts are not known PostHog Cloud hosts; \
                                 static region pairing not checked";
                              ]);
                    }))
        else
          Lwt.return
            (Error
               ( classify_error_status status,
                 Printf.sprintf "organization listing answered HTTP %d" status
               ))

  (* Reuses the central configuration parser: an invalid or disabled
     configuration fails the preflight before any network request. Performs
     no event ingestion — the only requests are the two documented private
     metadata reads above, under one bounded timeout. *)
  let run () =
    match active_config () with
    | None ->
        Lwt.return
          (Error
             ( "configuration_invalid",
               "analytics is disabled by the current environment \
                configuration; fix the startup diagnostics before deploying" ))
    | Some (config, environment) -> (
        match (config.project_id, config.personal_api_key) with
        | Some project_id, Some api_key ->
            Lwt.catch
              (fun () ->
                Lwt.pick
                  [
                    ( verify ~config ~environment ~api_key ~project_id
                    >|= fun r -> `Done r );
                    ( Lwt_unix.sleep timeout_seconds >|= fun () -> `Timeout );
                  ]
                >|= function
                | `Done r -> r
                | `Timeout ->
                    Error
                      ( "timeout",
                        Printf.sprintf "no verdict within %.0fs"
                          timeout_seconds ))
              (fun _exn ->
                (* Exception text can embed hosts/URLs; a bounded class is all
                   that may be reported. *)
                Lwt.return
                  (Error
                     ( "network_failure",
                       "the private API host could not be reached" )))
        | _ ->
            Lwt.return
              (Error
                 ( "configuration_invalid",
                   project_id_env ^ " and " ^ personal_api_key_env
                   ^ " are required for the preflight" )))
end

(* === Test seams === *)

module For_testing = struct
  let consent_of_cookie_header = consent_of_cookie_header
  let event_payload = event_payload
  let person_sync_payload = person_sync_payload
  let group_identify_payload = group_identify_payload
  let set_capture_sink sink = capture_sink := Some sink
  let clear_capture_sink () = capture_sink := None

  let test_config ~enabled =
    {
      enabled;
      (* Development stands in for "an enabled non-production environment";
         the record is installed directly (not parsed), so tests exercising
         the validator use validate_environment_configuration instead. *)
      environment = (if enabled then Some Development else None);
      (* Dummy token/origin, not real values. *)
      project_token = (if enabled then "phc_test_token" else "");
      api_host = default_api_host;
      ui_host = default_ui_host;
      project_id = None;
      personal_api_key = None;
      public_origin = (if enabled then Some "http://earde.test" else None);
    }

  let use_enabled_test_configuration () =
    config_override := Some (test_config ~enabled:true)

  (* §3.3 deletion-client tests: point the private Persons API at a local HTTP
     stub. Dummy values only — never real credentials. *)
  let use_deletion_test_configuration ~ui_host ~project_id ~personal_api_key ()
      =
    config_override :=
      Some
        {
          (test_config ~enabled:true) with
          ui_host = strip_trailing_slash ui_host;
          project_id;
          personal_api_key;
        }

  let use_disabled_test_configuration () =
    config_override := Some (test_config ~enabled:false)

  (* Preflight tests: a fully populated enabled configuration whose private
     API host points at a local stub. Dummy values only. *)
  let use_preflight_test_configuration ~environment ~ui_host ~project_id
      ~personal_api_key ~project_token () =
    config_override :=
      Some
        {
          (test_config ~enabled:true) with
          environment = Some environment;
          project_token;
          ui_host = strip_trailing_slash ui_host;
          project_id = Some project_id;
          personal_api_key = Some personal_api_key;
        }

  (* Pure drive of the closed environment/activation validator on raw values,
     as if they came from the process environment. Returns
     (analytics enabled, parsed environment, diagnostics). *)
  let validate_environment_configuration ?enabled ?environment
      ?allow_development ?project_token ?api_host ?ui_host ?project_id
      ?personal_api_key ?public_origin () =
    let config, diagnostics =
      validate_configuration ~enabled ~environment ~allow_development
        ~project_token ~api_host ~ui_host ~project_id ~personal_api_key
        ~public_origin
    in
    ( config.enabled,
      Option.map deployment_environment_to_string config.environment,
      diagnostics )

  (* Installs the config the validator produced, so layout/browser-config
     tests can exercise exactly what a given environment would run with. *)
  let install_validated_configuration ?enabled ?environment ?allow_development
      ?project_token ?api_host ?ui_host ?project_id ?personal_api_key
      ?public_origin () =
    let config, _diagnostics =
      validate_configuration ~enabled ~environment ~allow_development
        ~project_token ~api_host ~ui_host ~project_id ~personal_api_key
        ~public_origin
    in
    config_override := Some config

  let clear_configuration_override () = config_override := None

  let config_report () =
    let c = current_config () in
    [
      (enabled_env, c.enabled);
      (deployment_environment_env, c.environment <> None);
      (project_token_env, c.project_token <> "");
      (api_host_env, c.api_host <> "");
      (ui_host_env, c.ui_host <> "");
      (project_id_env, c.project_id <> None);
      (personal_api_key_env, c.personal_api_key <> None);
      (public_origin_env, c.public_origin <> None);
    ]
end
