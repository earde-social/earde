(** Joining and leaving a community, and member management by moderators. *)

val join_community_handler : Dream.handler
val leave_community_handler : Dream.handler
val add_member_handler : Dream.handler
val remove_member_handler : Dream.handler
