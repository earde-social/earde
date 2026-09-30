(** Upload conversion: every accepted image is re-encoded by ImageMagick
    in a bounded worker pool, and every failure path removes its temporary
    files. The format and policy rules live in [Image_upload]. *)

val process_image_upload :
  db:(module Caqti_lwt.CONNECTION) ->
  ip:string ->
  purpose:Image_upload.purpose ->
  string -> (string option, string) result Lwt.t
