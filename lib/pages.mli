(** Full-page HTML assembly. Each function calls Components.layout and inlines
    page-specific content. hq_dashboard_page is the only exception — it emits
    standalone HTML with no shared nav, intentionally isolated from the main shell. *)

(** === CORE FEED === *)
val index : ?user:string -> (int * int) list -> int -> string -> admin_usernames:string list -> moderated_communities:Db.community list -> Db.post list -> Db.community list -> Dream.request -> string

(** === AUTHENTICATION === *)
val signup_form : ?user:string -> Dream.request -> string
val login_form : ?user:string -> Dream.request -> string
val forgot_password_page : Dream.request -> string
val reset_password_page : token:string -> ?error:string -> Dream.request -> string

(** === COMMUNITY === *)
val new_community_form : ?user:string -> Dream.request -> string
val community_page : ?user:string -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> int -> string -> Db.community -> Db.post list -> Dream.request -> string
val community_settings_page : ?user:string -> community:Db.community -> mods:Db.user list -> banned_users:Db.user list -> Dream.request -> string

(** === POST === *)
val choose_community_page : ?user:string -> Db.community list -> string
val join_to_post_page : ?user:string -> Db.community -> Dream.request -> string
val new_post_form : ?user:string -> Db.community -> Dream.request -> string
val post_page : ?user:string -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> community:Db.community -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> (int * int) list -> Db.post -> Db.comment list -> Dream.request -> string

(** === USER === *)
val user_profile_page : ?user:string -> is_admin:bool -> is_globally_banned:bool -> profile_id:int -> admin_usernames:string list -> moderated_communities:Db.community list -> active_tab:string -> (int * int) list -> string -> string -> string option -> string option -> int -> Db.post list -> (int * string * string * int * string * int) list -> Dream.request -> string
val settings_page : ?user:string -> string option -> string option -> Dream.request -> string
val notifications_page : ?user:string -> Db.notification list -> Dream.request -> string

(** === SEARCH === *)
val search_results_page : ?user:string -> admin_usernames:string list -> (int * int) list -> int -> string -> string -> Db.community list -> (int * string * string * string option * string option) list -> Db.post list -> (int * string * string * string * int * int) list -> Dream.request -> string

(** === LEGAL / PRIVACY === *)
val privacy_page : ?user:string -> Dream.request -> string
val about_page : ?user:string -> Dream.request -> string

(** === MESSAGE PAGE === *)
val msg_page : ?user:string -> title:string -> message:string -> alert_type:string -> return_url:string -> Dream.request -> string

(** === MODERATION LOG === *)
val mod_log_page : ?user:string -> community:Db.community -> Db.mod_action list -> Dream.request -> string

(** === ADMIN === *)
val admin_dashboard_page : ?user:string -> banned_users:Db.user list -> Dream.request -> string
val hq_dashboard_page : (((int * int * int) * (int * int * int)) * (int * int * int) * (int * int * int) * (int * int * int)) -> string
