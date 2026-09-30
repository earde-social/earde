(** Promotion of chat messages into a durable forum thread. *)

type start_perm =
  | Start_allowed
  | Start_not_member
  | Start_banned
  | Start_error of string

val start_thread_form_handler : Dream.handler
val start_thread_create_handler : Dream.handler
