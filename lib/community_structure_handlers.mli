(** Forum sections and live-chat channels of a community: creation, update,
    archiving and deletion. *)

val add_section_handler : Dream.handler
val update_section_handler : Dream.handler
val delete_section_handler : Dream.handler
val add_channel_handler : Dream.handler
val update_channel_handler : Dream.handler
val archive_channel_handler : Dream.handler
val unarchive_channel_handler : Dream.handler
