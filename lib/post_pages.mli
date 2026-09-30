(** Post creation forms and the legacy /p/:id post page. *)

val choose_community_page : ?user:string -> ?request:Dream.request -> ?rail_communities:Community_types.community list -> Community_types.community list -> string

val join_to_post_page : ?user:string -> ?rail_communities:Community_types.community list -> Community_types.community -> Dream.request -> string

val start_thread_form : ?user:string -> ?error:string -> ?rail_communities:Community_types.community list -> channels:Channel_store.channel list -> can_manage:bool -> community:Community_types.community -> channel:Channel_store.channel -> seed_id:int64 -> candidates:(Chat_store.chat_message * string option) list -> sections:Section_store.community_section list -> default_section_id:int -> default_title:string -> default_body:string -> Dream.request -> string

val new_post_form : ?user:string -> ?preselected_section_id:int -> ?rail_communities:Community_types.community list -> ?share_candidates:(string * string) list -> Section_store.community_section list -> Community_types.community -> Dream.request -> string
(** [share_candidates] are [(slug, name)] pairs of eligible connected
    destination communities, already server-resolved by the handler. When
    non-empty the form gains the optional "Share with a connected community"
    select (blank = "Do not share") and the private request-note field; when
    empty the form is byte-identical to the pre-slice-4 composer. The markup
    grants nothing: POST /posts re-resolves the posted slug and the
    placement store revalidates everything under its own locks. *)

val post_page : ?user:string -> ?noindex:bool -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> community:Community_types.community -> user_communities:Community_types.community list -> moderated_communities:Community_types.community list -> (int * int) list -> (int * int) list -> Post_types.post -> Comment_store.comment list -> Dream.request -> string
