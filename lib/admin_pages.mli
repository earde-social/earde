(** The global admin dashboard page. *)

val looks_random_username : string -> bool

val admin_dashboard_page :
  ?user:string ->
  ?rail_communities:Community_types.community list ->
  signups_enabled:bool ->
  turnstile:[ `Configured | `Disabled | `Misconfigured ] ->
  brevo_configured:bool ->
  recent_users:Admin_store.admin_recent_user list ->
  pending:Admin_store.pending_signup_row list ->
  banned_users:User_store.user list ->
  Dream.request ->
  string
