(** Community moderation: reports and the report queue, community bans, the
    moderation log and moderator management. *)

val modlog_handler : Dream.handler
val ban_community_user_handler : Dream.handler
val unban_community_user_handler : Dream.handler
val report_form_handler : Dream.handler
val create_report_handler : Dream.handler
val reports_queue_handler : Dream.handler
val dismiss_report_handler : Dream.handler
val action_report_handler : Dream.handler
val manage_mods_handler : Dream.handler
val manage_mods_add_handler : Dream.handler
val manage_mods_promote_handler : Dream.handler
val manage_mods_remove_handler : Dream.handler
