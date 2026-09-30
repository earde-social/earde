(** Profile, settings and notifications pages. *)

val user_profile_page : ?user:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> is_globally_banned:bool -> profile_id:int -> admin_usernames:string list -> moderated_communities:Community_types.community list -> active_tab:string -> (int * int) list -> string -> string -> string option -> string option -> int -> Post_types.post list -> (int * string * string * int * string * int) list -> Community_user_stats_store.community_user_stat list -> Dream.request -> string

val settings_page : ?user:string -> ?rail_communities:Community_types.community list -> string option -> string option -> Dream.request -> string

val notifications_page : ?user:string -> ?rail_communities:Community_types.community list -> Notification_store.notification list -> Dream.request -> string
