type channel = {
  id : int; community_id : int; slug : string; name : string; topic : string option;
  position : int; is_archived : bool; created_at : string; indexable : bool;
}

(* create_channel returns the generated slug — it is auto-derived from name and
   the channel view/management handlers redirect by slug. *)
val create_channel : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> int -> (string, string) result Lwt.t
val get_channels_by_community : (module Caqti_lwt.CONNECTION) -> int -> (channel list, string) result Lwt.t
val get_channel_by_slug : (module Caqti_lwt.CONNECTION) -> string -> int -> (channel option, string) result Lwt.t
val get_channel_by_id : (module Caqti_lwt.CONNECTION) -> int -> int -> (channel option, string) result Lwt.t
val set_channel_archived : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
(* update_channel channel_id community_id name topic — edits display name + topic only;
   slug is intentionally left stable so existing /ch/:slug links keep resolving. *)
val update_channel : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> string option -> (unit, string) result Lwt.t
(* update_channel_indexable channel_id community_id indexable — Slice H toggle; community-scoped. *)
val update_channel_indexable : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
