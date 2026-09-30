type community_user_stat = {
  community_name : string;
  community_slug : string;
  local_karma : int;
  local_post_count : int;
  local_comment_count : int;
  first_active_at : string option;
}

val increment_local_post_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val increment_local_comment_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_user_community_stats : (module Caqti_lwt.CONNECTION) -> int -> (community_user_stat list, string) result Lwt.t
