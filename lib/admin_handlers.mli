(** The global admin dashboard and site-wide bans. *)

val ban_user_handler : Dream.handler

val unban_user_global_handler : Dream.handler

val admin_dashboard_handler : Dream.handler

val debug_state_handler : Dream.handler
