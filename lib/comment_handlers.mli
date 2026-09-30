(** Comment creation and deletion by authors and moderators. *)

val mod_delete_comment_handler : Dream.handler

val create_comment_handler : Dream.handler

module Comment_delete : sig
  type decision = Admin_delete | Author_delete | Forbidden
  val decide : is_admin:bool -> requester_id:int -> owner_id:int -> decision
end

val delete_comment_handler : Dream.handler
