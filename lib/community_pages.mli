(** The community page, its section and overview surfaces, and their
    sidebars. *)

val community_page :
  ?user:string ->
  ?noindex:bool ->
  ?connected_projects:Html.t ->
  ?connected_communities:Html.t ->
  is_member:bool ->
  is_current_user_mod:bool ->
  is_current_user_top_mod:bool ->
  mod_usernames:string list ->
  admin_usernames:string list ->
  banned_usernames:string list ->
  user_communities:Community_types.community list ->
  moderated_communities:Community_types.community list ->
  (int * int) list ->
  int ->
  string ->
  Community_types.community ->
  Post_types.feed_item list -> Dream.request -> string

val launch_knowledge_sidebar :
  community:Community_types.community ->
  channels:Channel_store.channel list ->
  sections:Section_store.community_section list ->
  ?active_section_slug:string ->
  ?append_uncategorized:bool ->
  ?settings_active:bool ->
  ?moderation_log_active:bool ->
  ?show_visibility_note:bool -> can_manage:bool -> unit -> Html.t

val community_section_shell_page : ?user:string -> ?noindex:bool -> ?thread_count:int -> ?last_activity:string -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> section:Section_store.community_section -> user_votes:(int * int) list -> current_page:int -> sort_mode:string -> community:Community_types.community -> posts:Post_types.feed_item list -> Dream.request -> string

val community_overview_page : ?user:string -> ?noindex:bool -> ?connected_projects_count:int -> ?connected_communities_count:int -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> orphaned:(int * string option) -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> recent_posts:Post_types.feed_item list -> Community_types.community -> (Section_store.community_section * int * string option) list -> Dream.request -> string
(** [connected_projects] is the pre-rendered "Connected projects" management fragment for the
    top-mod/admin settings surface (empty for every other viewer, which also removes the panel
    and its navigation entry). *)
