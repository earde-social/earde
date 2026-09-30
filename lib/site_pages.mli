(** The privacy policy page and the shared message page. *)

val privacy_page : ?user:string -> Dream.request -> string

val msg_page :
  ?user:string ->
  ?auth:bool ->
  title:string ->
  message:string ->
  alert_type:string ->
  return_url:string ->
  Dream.request ->
  string
