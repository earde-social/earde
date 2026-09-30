(** Posts and threads: the composer, creation, the thread and legacy post pages,
    and deletion by authors and moderators. *)

val new_post_page : Dream.handler
val create_post_handler : Dream.handler
val view_post_handler : Dream.handler
val view_thread_handler : Dream.handler
val delete_post_handler : Dream.handler
val mod_delete_post_handler : Dream.handler
