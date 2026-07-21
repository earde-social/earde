(** HTTP layer. Every route in main.ml maps 1-to-1 to a value here.
    Handlers own auth checks, session reads, and DB fan-out; rendering is
    delegated to Pages. analytics_middleware is a Dream middleware, not a handler. *)

(** === RATE LIMITING === *)
module Rate_limit : sig
  val middleware : Dream.handler -> Dream.handler
end

(** === AUTHENTICATION === *)
val signup_page : Dream.handler
val signup_handler : Dream.handler
val verify_email_handler : Dream.handler
val confirm_email_handler : Dream.handler
val login_page : Dream.handler
val login_handler : Dream.handler
val logout_handler : Dream.handler
val forgot_password_page : Dream.handler
val forgot_password_handler : Dream.handler
val reset_password_page_handler : Dream.handler
val reset_password_handler : Dream.handler

(** === CORE FEED === *)
val home_handler : Dream.handler
val feed_handler : Dream.handler
val search_handler : Dream.handler

(** === COMMUNITY === *)
val new_community_page : Dream.handler
val create_community_handler : Dream.handler
val community_page_handler : Dream.handler
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
val add_mod_handler : Dream.handler
val remove_mod_handler : Dream.handler
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
val unread_notifs_api : Dream.handler

(** === LEGAL / PRIVACY === *)
val privacy_page_handler : Dream.handler

(** === ADMIN === *)
val hq_dashboard_handler : Dream.handler
val ban_user_handler : Dream.handler
val unban_user_global_handler : Dream.handler
val admin_dashboard_handler : Dream.handler
val debug_state_handler : Dream.handler

(** === MIDDLEWARE === *)
val analytics_middleware : Dream.handler -> Dream.handler
