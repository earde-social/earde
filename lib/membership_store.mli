val join_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_member : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
(* true = a membership row was actually deleted; false = the user was not a
   member (no-op). Same DELETE round-trip via RETURNING. *)
val leave_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
(** List the [community_members] allow-list (username/id/email) for the settings
    member-management UI. Ordered by username. Mods/admins absent unless also members. *)
val get_community_members : (module Caqti_lwt.CONNECTION) -> int -> (User_store.user list, string) result Lwt.t
val get_user_communities : (module Caqti_lwt.CONNECTION) -> int -> (Community_types.community list, string) result Lwt.t
