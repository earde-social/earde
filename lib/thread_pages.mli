(** The thread page, including shared-thread context and provenance. *)

type thread_source_view =
  | Ts_private
  | Ts_visible of (string * string) option * Thread_source_store.thread_source_msg list

type shared_thread_page_context = {
  stc_origin_name : string;
  stc_section : (string * string) option;
}

type thread_creation_notice =
  | Creation_share_requested
  | Creation_share_failed

val thread_shell_page : ?user:string -> ?noindex:bool -> ?can_share:bool -> ?can_comment:bool -> ?creation_notice:thread_creation_notice -> ?shared_context:shared_thread_page_context -> ?shared_with:(string * string) list -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> community:Community_types.community -> ?thread_source:thread_source_view -> user_post_votes:(int * int) list -> user_comment_votes:(int * int) list -> post:Post_types.post -> comments:Comment_store.comment list -> Dream.request -> string
(** [connected_projects_count] and [connected_communities_count] are how many records the
    two public connected-* read models returned for this community — the sizes of exactly
    the lists [/c/:slug/network] renders. The home carries the compact Network entry point
    only: the lists themselves live on that page, and a zero count still renders its row. *)
