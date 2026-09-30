val get_all_communities : (module Caqti_lwt.CONNECTION) -> (Community_types.community list, string) result Lwt.t
val create_community : (module Caqti_lwt.CONNECTION) -> string -> string -> string option -> bool -> (unit, string) result Lwt.t
val get_community_by_slug : (module Caqti_lwt.CONNECTION) -> string -> (Community_types.community option, string) result Lwt.t
val get_community_by_id : (module Caqti_lwt.CONNECTION) -> int -> (Community_types.community option, string) result Lwt.t
val search_communities : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (Community_types.community list, string) result Lwt.t
(* UPDATE ... RETURNING: the authoritative updated record rides the same
   round-trip (analytics $groupidentify needs it); None = no such id, which
   was previously an indistinguishable silent no-op. *)
val update_community_details : (module Caqti_lwt.CONNECTION) -> int -> string option -> string option -> string option -> string option -> (Community_types.community option, string) result Lwt.t
val toggle_community_downvotes : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
(** Set community visibility / indexability from the settings UI. [visibility] is the
    closed variant (stringified internally); [indexable] is a bool. Touch only those columns.
    Visibility returns the authoritative updated record (UPDATE ... RETURNING) because it is a
    closed analytics group property; [None] = no such community id. *)
val update_community_visibility : (module Caqti_lwt.CONNECTION) -> int -> Community_types.community_visibility -> (Community_types.community option, string) result Lwt.t
val update_community_indexable : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
val get_allows_downvotes_for_post : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
val get_allows_downvotes_for_comment : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
