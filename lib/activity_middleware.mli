(** Page-view recording and presence (last activity) middleware. *)

val presence_middleware : Dream.handler -> Dream.handler

val analytics_middleware : Dream.handler -> Dream.handler
