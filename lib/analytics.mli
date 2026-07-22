(** PostHog server-side analytics (spec: docs/features/posthog-analytics.md §3.1).

    Consent is enforced by construction: the only capture path for domain
    events is [capture_if_consented], which requires the Dream request and
    captures only when the earde_analytics_consent cookie is exactly
    "granted" and POSTHOG_ENABLED is exactly "true". The raw HTTP-capture
    function is private to the module. All entry points return [unit]
    immediately; HTTP happens asynchronously, best-effort, and can never fail
    the calling request. *)

(** Closed person-property record (§4.3). Emitted only as the [$set] object —
    on [Signup_confirmed] / [Login_succeeded] payloads and in
    [sync_person_after_consent_grant] — never as ordinary event properties. *)
type person_properties = {
  username : string;
  email : string;
  signup_date : string;  (** ISO 8601 *)
  is_admin : bool;
}

(** Closed community group-property record (§5.3), for [$groupidentify]. *)
type community_group = {
  community_id : int;
  community_slug : string;
  community_name : string;
  community_visibility : string;  (** "public" / "private" *)
  created_at : string option;  (** ISO 8601 when available on the record *)
}

type response_mode = Response_json | Response_redirect

(** Closed domain-event model (§3.2/§5.1). The compiler enforces the property
    allowlist: handlers cannot attach arbitrary properties, bodies, titles,
    emails, or tokens. *)
type event =
  | Signup_confirmed of { user_id : int; person : person_properties }
      (** carries [$set] — refreshes person properties server-side (§4.3) *)
  | Login_succeeded of { user_id : int; person : person_properties }
      (** carries [$set] — refreshes person properties server-side (§4.3) *)
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
  | Post_created of {
      user_id : int;
      community_id : int;
      section_id : int option;
      post_id : int;
      content_length : int;
      has_link : bool;
      has_mention : bool;
    }
  | Comment_created of {
      user_id : int;
      community_id : int;
      post_id : int;
      comment_id : int;
      parent_comment_id : int option;
      content_length : int;
      has_mention : bool;
    }
  | Thread_promoted of {
      user_id : int;
      community_id : int;
      community_slug : string;
      channel_id : int;
      channel_slug : string;
      section_id : int option;
      post_id : int;
      message_id : int64;  (** seed chat message *)
      promoted_message_count : int;
      promoted_participant_count : int option;
    }
  | Account_deleted of { user_id : int }

(** ["user:<database_id>"] — the §4.1 authenticated distinct-ID scheme. *)
val distinct_id_of_user_id : int -> string

(** Captures the event iff analytics is enabled and the request carries
    [earde_analytics_consent=granted]. Missing, denied, or malformed consent
    produces no side effect. Community-scoped events automatically carry
    [$groups.community = "community:<id>"]. *)
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

(** Test seams: pure payload builders and a capture sink that replaces the
    HTTP transport. The sink only observes payloads produced by the closed
    API above — it is not a bypass capable of arbitrary capture. *)
module For_testing : sig
  val consent_of_cookie_header :
    string option -> [ `Granted | `Denied | `Unknown ]

  val event_payload :
    api_key:string -> distinct_id:string -> event -> Yojson.Safe.t

  val person_sync_payload :
    api_key:string -> distinct_id:string -> person_properties -> Yojson.Safe.t

  val group_identify_payload :
    api_key:string -> distinct_id:string -> community_group -> Yojson.Safe.t

  (** When set, capture dispatch calls the sink synchronously instead of
      performing HTTP. Sink exceptions are swallowed like transport errors. *)
  val set_capture_sink : (Yojson.Safe.t -> unit) -> unit

  val clear_capture_sink : unit -> unit

  (** Config overrides so tests never read the process environment or hit the
      network. The test token is a dummy value, not a real credential. *)
  val use_enabled_test_configuration : unit -> unit

  val use_disabled_test_configuration : unit -> unit
  val clear_configuration_override : unit -> unit

  (** [(env var name, is set)] pairs for the active configuration — presence
      booleans only, never values, so secrets cannot leak through it. *)
  val config_report : unit -> (string * bool) list
end
