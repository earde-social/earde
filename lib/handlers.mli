(** HTTP layer. Every route in main.ml maps 1-to-1 to a value here.
    Handlers own auth checks, session reads, and DB fan-out; rendering is
    delegated to Pages. presence_middleware and analytics_middleware are Dream
    middlewares, not handlers. *)

(** === RATE LIMITING === *)
module Rate_limit : sig
  val middleware : Dream.handler -> Dream.handler
  (** The shared per-IP, per-path limiter for sensitive POSTs. Fails closed:
      only a positive Allowed decision invokes the wrapped handler; a
      blocked request gets the Too Many Attempts page, and a lookup error,
      rejected promise or pool failure gets a generic 503 without invoking
      it. *)

  val make_middleware :
    check:
      (Dream.request -> ip:string -> endpoint:string ->
       ([ `Allowed | `Blocked ], string) result Lwt.t) ->
    cleanup:(Dream.request -> unit) ->
    Dream.handler -> Dream.handler
  (** [middleware] with its enforcement lookup and its opportunistic,
      best-effort expiry cleanup supplied — so the decision logic can be
      exercised against a failing lookup or cleanup without a database.
      [cleanup] must not block; anything it raises is logged and cannot
      change the decision. *)
end

val safe_local_redirect : ?default:string -> Dream.request -> string -> string
(** Reduce an attacker-controlled redirect target (Referer header or
    form-carried return path) to a local path+query. A path starting with '/'
    passes through; an absolute http(s) URL whose host and effective port
    match the request's Host header is reduced to its path+query. Everything
    else — protocol-relative, foreign-host, userinfo-bearing, malformed,
    backslash- or control-character-bearing values — collapses to [default]
    (itself a trusted local path, "/" when omitted). Fragments are dropped;
    the result never carries a scheme or authority. *)

(** === AUTHENTICATION === *)

val is_valid_new_username : string -> bool
(** Route-safe ASCII syntax required of NEW usernames at signup: one or more
    of [A-Za-z], [0-9], ['_'], ['-'] — no quotes, angle brackets, slashes,
    whitespace or control characters. The existing 3..30 length bound is
    checked separately and still applies. Deliberately NOT enforced against
    accounts that already exist: login, lookup and rendering never consult
    it, so no current user is locked out or renamed. Escaping at each render
    sink remains the actual XSS defence; this is the second layer. Pure. *)

val signup_page : Dream.handler
val signup_handler : Dream.handler
(** POST /signup on the process-wide auth mail dispatcher. *)

val make_signup_handler : mail:Email.message Auth_mail_dispatcher.t -> Dream.handler
(** The signup POST over an explicit dispatcher. After the closed-signup,
    honeypot, Turnstile and syntax gates, the only account-dependent answer
    is "username taken" for a handle owned by a real account (decided by the
    username alone). Every other eligible submission is admitted to [mail]
    first (a full dispatcher answers a generic 503 before any hashing or
    write), always pays one Argon2 hash, and gets the same neutral response
    whether the email is new, registered or pending, whether a reservation
    is the submitter's own or someone else's, and whether a race or storage
    error prevented the write. A confirmation job is queued only after a
    fresh pending row commits. *)
val verify_email_handler : Dream.handler
val confirm_email_handler : Dream.handler
val login_page : Dream.handler
val login_handler : Dream.handler
(** POST /login with the production Argon2 verifier. *)

val make_login_handler : verify:Login_verification.verifier -> Dream.handler
(** The login POST over an explicit verifier. A missing account and a wrong
    password get the same response after one verification each (the
    missing account against {!Login_verification.dummy_hash}); only a found
    account whose own hash verifies can log in. *)
val logout_handler : Dream.handler
val forgot_password_page : Dream.handler
val forgot_password_handler : Dream.handler
(** POST /forgot-password on the process-wide auth mail dispatcher. *)

val make_forgot_password_handler : mail:Email.message Auth_mail_dispatcher.t -> Dream.handler
(** The reset-request POST over an explicit dispatcher: admission first (a
    full dispatcher answers a generic 503 before any lookup or write), then
    the token write, then the same neutral response for every address. A
    reset job is queued only when a token row for a real account was
    written; unknown addresses get no email. *)
val reset_password_page_handler : Dream.handler
val reset_password_handler : Dream.handler

(** === CORE FEED === *)
val feed_handler : Dream.handler
val search_handler : Dream.handler

(** === COMMUNITY === *)
val new_community_page : Dream.handler
val create_community_handler : Dream.handler
val community_page_handler : Dream.handler

(** GET /c/:slug/network — the public Network page: the community's complete
    connected-projects and connected-communities lists, which the community
    home links to instead of carrying. Same view authorization as /c/:slug
    (resolve, then [can_view_community]) and the same two read models, so it
    exposes nothing that page would not; the compact Connect-a-community link
    follows the sidebar's existing top-mod-or-admin reading and authorizes
    nothing. No mutation, no management state. *)
val community_network_handler : Dream.handler
val community_section_handler : Dream.handler
val community_channel_handler : Dream.handler
val channel_messages_json_handler : Dream.handler
val realtime_token_handler : Dream.handler

(** Chat composer JSON contract (pure, exposed for tests): Accept-header
    negotiation, the shared content validation boundary, and the client-safe
    error body shape used by the fetch submission path. *)
module Chat_api : sig
  val wants_json : string option -> bool
  val max_content_length : int
  val validate_content : string -> (string, [ `Empty | `Too_long ]) result
  val error_json : code:string -> message:string -> string
  val internal_error_json : string
end

(** Canonical chat-row serialization shared by catch-up, the composer JSON
    response and (shape-wise) gateway new_msg events. Exposed for tests. *)
val chat_message_json :
  channel_id:int ->
  community_id:int ->
  ?thread_id:int ->
  Db.chat_message * string option ->
  Yojson.Safe.t

val send_message_handler : Dream.handler
(* Start thread from chat: GET renders the form, POST creates the thread + provenance. *)
val start_thread_form_handler : Dream.handler
val start_thread_create_handler : Dream.handler
val join_community_handler : Dream.handler
val leave_community_handler : Dream.handler
val community_settings_handler : Dream.handler
(** Pure lifecycle gate for the /c/:slug/settings/visibility POST, extracted for
    testing: [None] lets the update proceed; [Some message] is the user-facing
    rejection (published network communities must remain public). Delegates to
    {!Network_communities.visibility_change_allowed}. *)
val visibility_update_rejection :
  Db.community -> requested_visibility:Db.community_visibility -> string option

val update_community_visibility_handler : Dream.handler
val update_community_indexability_handler : Dream.handler
val add_member_handler : Dream.handler
val remove_member_handler : Dream.handler
val add_section_handler : Dream.handler
val update_section_handler : Dream.handler
val delete_section_handler : Dream.handler
val add_channel_handler : Dream.handler
val update_channel_handler : Dream.handler
val update_channel_indexability_handler : Dream.handler
val update_section_indexability_handler : Dream.handler
val archive_channel_handler : Dream.handler
val unarchive_channel_handler : Dream.handler
val modlog_handler : Dream.handler
val update_community_handler : Dream.handler
(* No add_mod_handler / remove_mod_handler: the unreferenced legacy /add-mod
   and /remove-mod endpoints admitted any moderator (so an ordinary mod could
   appoint moderators and unseat the Top Mod) and were removed with their
   routes. Moderator management is the manage_mods_* surface below. *)
val ban_community_user_handler : Dream.handler
val unban_community_user_handler : Dream.handler
val manage_mods_handler : Dream.handler
val manage_mods_add_handler : Dream.handler
val manage_mods_promote_handler : Dream.handler
val manage_mods_remove_handler : Dream.handler

(** === POST === *)
val new_post_page : Dream.handler
val create_post_handler : Dream.handler
val view_post_handler : Dream.handler
val view_thread_handler : Dream.handler
val delete_post_handler : Dream.handler
val mod_delete_post_handler : Dream.handler

(** === COMMENT === *)

(** Pure authorization decision for the general /delete-comment endpoint —
    author-only for non-admins; community moderation must use the scoped
    mod_delete flow. Takes no community id by design (the old hidden
    community_id form field enabled a cross-community delete) and is exposed
    for tests. *)
module Comment_delete : sig
  type decision = Admin_delete | Author_delete | Forbidden
  val decide : is_admin:bool -> requester_id:int -> owner_id:int -> decision
end

val create_comment_handler : Dream.handler
val delete_comment_handler : Dream.handler
val mod_delete_comment_handler : Dream.handler

(** === REPORTS === *)
(* GET /c/:slug/report?type=post|comment&id=<int> — SSR report form.
   POST /c/:slug/reports — create a report. Both gate login/ban/self-report server-side. *)
val report_form_handler : Dream.handler
val create_report_handler : Dream.handler
(* GET /c/:slug/reports — read-only mod queue (?status=open|dismissed|action_taken).
   PRIVATE: gated to M/TM/A like community settings, never public like the modlog. *)
val reports_queue_handler : Dream.handler
(* POST /c/:slug/reports/:report_id/dismiss | /action — resolve an open report (M/TM/A only).
   Verifies the report belongs to the slug's community; logs the report_id (not target_id) to
   the modlog. Does NOT remove content or ban authors. *)
val dismiss_report_handler : Dream.handler
val action_report_handler : Dream.handler

(** === VOTING === *)
val vote_handler : Dream.handler
val vote_comment_handler : Dream.handler
val toggle_downvotes_handler : Dream.handler

(** === USER === *)
val view_profile_handler : Dream.handler
val settings_page_handler : Dream.handler
val update_profile_handler : Dream.handler
val change_password_handler : Dream.handler
val export_data_handler : Dream.handler
val delete_account_handler : Dream.handler

(** === NOTIFICATIONS === *)
val notifications_handler : Dream.handler

(** === LEGAL / PRIVACY === *)
val privacy_page_handler : Dream.handler

(** === ADMIN === *)
val ban_user_handler : Dream.handler
val unban_user_global_handler : Dream.handler
val admin_dashboard_handler : Dream.handler
val debug_state_handler : Dream.handler

(** === ANALYTICS CONSENT (spec §9) === *)
(** JSON-only, Origin/Sec-Fetch-Site-protected, session-optional; sets the
    plaintext earde_analytics_consent cookie and, on granted with an
    authenticated session, performs the single person-property sync. *)
val analytics_consent_handler : Dream.handler

(** Controlled JSON 405 for non-POST methods on /analytics/consent. *)
val analytics_consent_method_not_allowed : Dream.handler

(** === MIDDLEWARE === *)
val presence_middleware : Dream.handler -> Dream.handler
val analytics_middleware : Dream.handler -> Dream.handler
