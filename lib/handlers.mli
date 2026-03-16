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
val login_page : Dream.handler
val login_handler : Dream.handler
val logout_handler : Dream.handler
val forgot_password_page : Dream.handler
val forgot_password_handler : Dream.handler
val reset_password_page_handler : Dream.handler
val reset_password_handler : Dream.handler

(** === CORE FEED === *)
val home_handler : Dream.handler
val search_handler : Dream.handler

(** === COMMUNITY === *)
val new_community_page : Dream.handler
val create_community_handler : Dream.handler
val community_page_handler : Dream.handler
val join_community_handler : Dream.handler
val leave_community_handler : Dream.handler
val community_settings_handler : Dream.handler
val update_community_handler : Dream.handler
val add_mod_handler : Dream.handler
val remove_mod_handler : Dream.handler
val ban_community_user_handler : Dream.handler
val unban_community_user_handler : Dream.handler

(** === POST === *)
val new_post_page : Dream.handler
val create_post_handler : Dream.handler
val view_post_handler : Dream.handler
val delete_post_handler : Dream.handler

(** === COMMENT === *)
val create_comment_handler : Dream.handler
val delete_comment_handler : Dream.handler

(** === VOTING === *)
val vote_handler : Dream.handler
val vote_comment_handler : Dream.handler

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
val about_page_handler : Dream.handler

(** === ADMIN === *)
val hq_dashboard_handler : Dream.handler
val ban_user_handler : Dream.handler
val unban_user_global_handler : Dream.handler
val admin_dashboard_handler : Dream.handler
val debug_state_handler : Dream.handler

(** === MIDDLEWARE === *)
val analytics_middleware : Dream.handler -> Dream.handler
