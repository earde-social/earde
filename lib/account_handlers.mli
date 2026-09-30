(** The user profile, account settings, password change, data export, account
    deletion and notifications. *)

val view_profile_handler : Dream.handler
val settings_page_handler : Dream.handler
val update_profile_handler : Dream.handler
val change_password_handler : Dream.handler
val export_data_handler : Dream.handler
val delete_account_handler : Dream.handler
val notifications_handler : Dream.handler
