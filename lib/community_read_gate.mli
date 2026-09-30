(** The private-community read gate and the noindex rules for community, child
    and thread pages. *)

val can_view_community :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  admin_override:bool ->
  Community_types.community ->
  bool Lwt.t

val community_noindex : Community_types.community -> bool
val child_noindex : Community_types.community -> child_indexable:bool -> bool

val thread_noindex :
  (module Caqti_lwt.CONNECTION) ->
  Community_types.community ->
  Post_types.post ->
  bool Lwt.t

val community_not_found :
  ?user:string -> Dream.request -> Dream.response Dream.promise
