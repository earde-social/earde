open Caqti_request.Infix

let current_q =
  (Caqti_type.int ->! Caqti_type.int64)
    "SELECT COALESCE(\n\
    \     (SELECT generation FROM community_realtime_generations WHERE \
     community_id = $1),\n\
    \     0)"

let current (module C : Caqti_lwt.CONNECTION) ~community_id =
  Lwt.map
    (function Ok g -> Ok g | Error e -> Error (Caqti_error.show e))
    (C.find current_q community_id)

let topic ~channel_id ~generation =
  Printf.sprintf "chan:%d:%Ld" channel_id generation
