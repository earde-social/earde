open Lwt.Infix

let log_page_view_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 string (option string) string) ->. Caqti_type.unit)
  "INSERT INTO page_views (path, referer, session_hash) VALUES ($1, $2, $3)"

let log_page_view (module C: Caqti_lwt.CONNECTION) path referer session_hash =
  C.exec log_page_view_query (path, referer, session_hash) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))
