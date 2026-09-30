(** Community creation and the community settings page. *)

val new_community_form : ?user:string -> ?rail_communities:Community_types.community list -> Dream.request -> string
(** The community-scoped feeds now take [Post_types.feed_item] rows: the community's
    own posts render byte-identically ([fi_shared = None]), while an accepted
    shared-thread placement row links into THIS community's thread context
    and carries its compact "Shared from" provenance and the placement's
    destination section. *)

val community_settings_page : ?user:string -> ?connected_projects:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> is_top_mod:bool -> open_reports_count:int -> community:Community_types.community -> mods:User_store.user list -> banned_users:User_store.user list -> members:User_store.user list -> sections:Section_store.community_section list -> channels:Channel_store.channel list -> Dream.request -> string
