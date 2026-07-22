(* PostHog server-side analytics (spec: docs/features/posthog-analytics.md).
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

type community_group = {
  community_id : int;
  community_slug : string;
  community_name : string;
  community_visibility : string;
  created_at : string option;
}

type response_mode = Response_json | Response_redirect

type event =
  | Account_signed_up of { user_id : int; person : person_properties }
  | Account_logged_in of { user_id : int; person : person_properties }
  | Community_joined of {
      user_id : int;
      community_id : int;
      community_slug : string;
      community_visibility : string;
    }
  | Community_left of { user_id : int; community_id : int }
  | Chat_message_sent of {
      user_id : int;
      community_id : int;
      community_slug : string;
      channel_id : int;
      channel_slug : string;
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
      community_slug : string;
      channel_id : int;
      channel_slug : string;
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

let distinct_id_of_user_id user_id = Printf.sprintf "user:%d" user_id

(* Constant non-user distinct id for the personless account_deleted metric.
   With $process_person_profile=false no person profile is ever created for
   it; the constant only pools the aggregate counter. *)
let account_deletion_distinct_id = "system:account-deletion"

(* Group keys use the immutable numeric id — slugs are mutable (§5.3). *)
let community_group_key community_id = Printf.sprintf "community:%d" community_id

(* === Configuration === *)

let enabled_env = "POSTHOG_ENABLED"
let project_token_env = "POSTHOG_PROJECT_TOKEN"
let api_host_env = "POSTHOG_API_HOST"
let ui_host_env = "POSTHOG_UI_HOST"
let project_id_env = "POSTHOG_PROJECT_ID"
let personal_api_key_env = "POSTHOG_PERSONAL_API_KEY"
let public_origin_env = "EARDE_PUBLIC_ORIGIN"

let default_api_host = "https://eu.i.posthog.com"
let default_ui_host = "https://eu.posthog.com"

type config = {
  enabled : bool;
  project_token : string;
  api_host : string;
  (* Held for the later steps that consume them (§3.3 deletion lifecycle, §9
     consent endpoint). personal_api_key and project_id are server-only and
     must never be rendered into browser configuration or logged. *)
  ui_host : string;
  project_id : string option;
  personal_api_key : string option;
  public_origin : string option;
}

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | None -> None
  | Some value ->
      let value = String.trim value in
      if value = "" then None else Some value

let strip_trailing_slash value =
  let len = String.length value in
  if len > 0 && value.[len - 1] = '/' then String.sub value 0 (len - 1)
  else value

let config_from_env () =
  let enabled_flag = getenv_nonempty enabled_env = Some "true" in
  let token = getenv_nonempty project_token_env in
  let origin = getenv_nonempty public_origin_env in
  let enabled, project_token =
    match (enabled_flag, token, origin) with
    | true, Some t, Some _ -> (true, t)
    | true, _, _ ->
        (* Misconfigured: enabled without a token or without the public origin
           the §9 consent endpoint needs. Disable safely rather than break the
           product; log the variable names, never any value. *)
        Logs.warn (fun m ->
            m "%s=true but %s or %s is unset; analytics disabled" enabled_env
              project_token_env public_origin_env);
        (false, "")
    | false, _, _ -> (false, "")
  in
  let host_or env_name default =
    match getenv_nonempty env_name with
    | Some h -> strip_trailing_slash h
    | None -> default
  in
  {
    enabled;
    project_token;
    api_host = host_or api_host_env default_api_host;
    ui_host = host_or ui_host_env default_ui_host;
    project_id = getenv_nonempty project_id_env;
    personal_api_key = getenv_nonempty personal_api_key_env;
    public_origin = getenv_nonempty public_origin_env;
  }

(* Lazy because analytics is not wired in bin/main.ml yet; the first caller
   resolves the environment once. Tests install an override instead so they
   never depend on the process environment. *)
let env_config = lazy (config_from_env ())
let config_override : config option ref = ref None

let current_config () =
  match !config_override with
  | Some config -> config
  | None -> Lazy.force env_config

(* === Browser configuration (strictly public values) === *)

type browser_config = { browser_token : string; browser_api_host : string }

(* Only the public write-only project token and the ingest host ever reach the
   browser. personal_api_key and project_id are deliberately unreachable from
   here. None ⇒ render no banner, no config attributes, no scripts. *)
let browser_config () =
  let c = current_config () in
  if c.enabled then
    Some { browser_token = c.project_token; browser_api_host = c.api_host }
  else None

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

let event_name = function
  | Account_signed_up _ -> "account_signed_up"
  | Account_logged_in _ -> "account_logged_in"
  | Community_joined _ -> "community_joined"
  | Community_left _ -> "community_left"
  | Chat_message_sent _ -> "chat_message_sent"
  | Forum_thread_created _ -> "forum_thread_created"
  | Forum_comment_created _ -> "forum_comment_created"
  | Conversation_promoted _ -> "conversation_promoted"
  | Account_deleted -> "account_deleted"

(* Community-scoped events carry $groups.community (§5.3); identity/lifecycle
   events do not. *)
let event_community_id = function
  | Account_signed_up _ | Account_logged_in _ | Account_deleted -> None
  | Community_joined { community_id; _ }
  | Community_left { community_id; _ }
  | Chat_message_sent { community_id; _ }
  | Forum_thread_created { community_id; _ }
  | Forum_comment_created { community_id; _ }
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
      [
        ("user_id", `Int user_id);
        ("community_id", `Int community_id);
        ("community_slug", `String community_slug);
        ("community_visibility", `String community_visibility);
      ]
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
      [
        ("user_id", `Int user_id);
        ("community_id", `Int community_id);
        ("community_slug", `String community_slug);
        ("channel_id", `Int channel_id);
        ("channel_slug", `String channel_slug);
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
      [
        ("user_id", `Int user_id);
        ("community_id", `Int community_id);
        ("community_slug", `String community_slug);
        ("channel_id", `Int channel_id);
        ("channel_slug", `String channel_slug);
      ]
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

let capture_payload ~api_key ~distinct_id ~name ~properties : Yojson.Safe.t =
  `Assoc
    [
      ("api_key", `String api_key);
      ("event", `String name);
      ("distinct_id", `String distinct_id);
      ("properties", `Assoc properties);
    ]

let event_payload ~api_key ~distinct_id event =
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
  capture_payload ~api_key ~distinct_id ~name:(event_name event)
    ~properties:(event_properties event @ groups)

(* Consent-transition sync: a dedicated $identify payload carrying the same
   closed $set object, used only by sync_person_after_consent_grant. *)
let person_sync_payload ~api_key ~distinct_id (p : person_properties) =
  capture_payload ~api_key ~distinct_id ~name:"$identify"
    ~properties:[ ("$set", person_set_json p) ]

let group_identify_payload ~api_key ~distinct_id (g : community_group) =
  let group_set =
    [
      ("community_id", `Int g.community_id);
      ("community_slug", `String g.community_slug);
      ("community_name", `String g.community_name);
      ("community_visibility", `String g.community_visibility);
    ]
    @ (match g.created_at with
      | None -> []
      | Some created_at -> [ ("created_at", `String created_at) ])
  in
  capture_payload ~api_key ~distinct_id ~name:"$groupidentify"
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
  let config = current_config () in
  if config.enabled then
    match consent_of_cookie_header (Dream.header request "Cookie") with
    | `Granted ->
        dispatch config
          (event_payload ~api_key:config.project_token ~distinct_id event)
    | `Denied | `Unknown -> ()

let sync_person_after_consent_grant ~distinct_id person =
  let config = current_config () in
  if config.enabled then
    dispatch config
      (person_sync_payload ~api_key:config.project_token ~distinct_id person)

(* Narrow §3.3 orchestration seam for the account-deletion flow only. The
   metric is PERSONLESS by construction — constant system distinct id,
   $process_person_profile=false, no properties — so ingestion timing relative
   to the Persons-API deletion cannot associate it with (or recreate) the
   deleted person; the await only sequences the HTTP requests, it proves
   nothing about ingestion. Same gate as capture_if_consented — disabled
   analytics or absent/denied consent resolves immediately with no side
   effect — and it can emit only the closed Account_deleted event. *)
let capture_account_deleted_sequenced request =
  let config = current_config () in
  if not config.enabled then Lwt.return_unit
  else
    match consent_of_cookie_header (Dream.header request "Cookie") with
    | `Granted ->
        dispatch_await config
          (event_payload ~api_key:config.project_token
             ~distinct_id:account_deletion_distinct_id Account_deleted)
    | `Denied | `Unknown -> Lwt.return_unit

(* $groupidentify shares capture_if_consented's exact gate: enabled AND the
   request's consent cookie is exactly "granted". It accepts only the closed
   community_group record — not a generic capture path — and the caller must
   supply the acting authenticated user's "user:<id>" as distinct_id (PostHog
   attributes the event to that person; a synthetic id would mint a phantom
   person). *)
let identify_community_if_consented request ~distinct_id group =
  let config = current_config () in
  if config.enabled then
    match consent_of_cookie_header (Dream.header request "Cookie") with
    | `Granted ->
        dispatch config
          (group_identify_payload ~api_key:config.project_token ~distinct_id
             group)
    | `Denied | `Unknown -> ()

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

  let clear_configuration_override () = config_override := None

  let config_report () =
    let c = current_config () in
    [
      (enabled_env, c.enabled);
      (project_token_env, c.project_token <> "");
      (api_host_env, c.api_host <> "");
      (ui_host_env, c.ui_host <> "");
      (project_id_env, c.project_id <> None);
      (personal_api_key_env, c.personal_api_key <> None);
      (public_origin_env, c.public_origin <> None);
    ]
end
