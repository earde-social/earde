type mod_action = {
  id : int; community_id : int; moderator_id : int; moderator_username : string;
  action_type : string; target_id : int option; reason : string; created_at : string;
}

val log_action : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> int option -> string -> (unit, string) result Lwt.t
val get_modlog : (module Caqti_lwt.CONNECTION) -> int -> (mod_action list, string) result Lwt.t
