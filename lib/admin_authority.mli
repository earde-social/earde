(** Global-admin authority for the legacy handlers. The session claim is never
    trusted on its own: every decision re-reads the durable [users.is_admin]
    flag, and any doubt resolves to "not an admin". *)

type current_admin =
  | Current_admin
  | Current_non_admin
  | Current_admin_storage_error of string

val current_admin_bool :
  (module Caqti_lwt.CONNECTION) -> Dream.request -> (bool, string) result Lwt.t

val current_admin_read_override :
  (module Caqti_lwt.CONNECTION) -> Dream.request -> bool Lwt.t

val current_admin_of_request : Dream.request -> current_admin Lwt.t
