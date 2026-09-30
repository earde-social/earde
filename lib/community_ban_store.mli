val ban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val unban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_banned : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_banned_users : (module Caqti_lwt.CONNECTION) -> int -> (User_store.user list, string) result Lwt.t
