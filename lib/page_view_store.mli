val log_page_view :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  string option ->
  string ->
  (unit, string) result Lwt.t
