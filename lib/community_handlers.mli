(** Community creation and the public community surfaces: the community
    page, the network page and section pages, including connected projects
    and communities. *)

val new_community_page : Dream.handler

val create_community_handler : Dream.handler

val with_settings_connected_projects :
  (module Caqti_lwt.CONNECTION) ->
  ?user:string ->
  Dream.request ->
  community_slug:string ->
  authorized:bool ->
  removal_allowed:bool ->
  (string -> Dream.response Dream.promise) -> Dream.response Dream.promise

val community_page_handler : Dream.handler

val community_network_handler : Dream.handler

val community_section_handler : Dream.handler
