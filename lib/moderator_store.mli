type moderator_entry = { user_id : int; username : string; role : string; }

(** Promotion failures classified at the database boundary. [Promotion_refused]
    carries a fixed user-facing domain message; [Promotion_storage_error]
    carries driver detail that must never reach a client. *)
type promote_error =
  | Promotion_refused of string
  | Promotion_storage_error of string

val add_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val add_top_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_moderator_role : (module Caqti_lwt.CONNECTION) -> int -> int -> (string option, string) result Lwt.t
val get_community_moderators : (module Caqti_lwt.CONNECTION) -> int -> (User_store.user list, string) result Lwt.t
val get_community_mods_with_roles : (module Caqti_lwt.CONNECTION) -> int -> (moderator_entry list, string) result Lwt.t
val remove_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_moderated_communities : (module Caqti_lwt.CONNECTION) -> int -> (Community_types.community list, string) result Lwt.t
val promote_to_top_mod : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, promote_error) result Lwt.t
val demote_inactive_mods : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
