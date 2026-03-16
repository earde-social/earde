(** Database layer. Types are top-level for cross-module sharing. Inner modules
    (Community, User, Post, …) own their queries; the flat aliases below preserve
    call-site compatibility without a mass handler rewrite. *)

type user = { id : int; username : string; email : string; }

type post = {
  id : int; title : string; url : string option; content : string option;
  community_id : int; user_id : int; username : string; community_slug : string;
  created_at : string; score : int; comment_count : int;
}

type community = {
  id : int; slug : string; name : string; description : string option;
  rules : string option; avatar_url : string option; banner_url : string option;
}

type comment = {
  id : int; content : string; username : string; created_at : string;
  score : int; parent_id : int option; avatar_url : string option;
}

type notification = {
  id : int; user_id : int; post_id : int; message : string; is_read : bool; created_at : string;
}

(** APM helper — wraps any DB call, emits {query, execution_ms, status} JSON via Logs.info. *)
val with_query_timer : name:string -> (unit -> ('a, string) result Lwt.t) -> ('a, string) result Lwt.t

module Community : sig
  val get_all_communities : (module Caqti_lwt.CONNECTION) -> (community list, string) result Lwt.t
  val create_community : (module Caqti_lwt.CONNECTION) -> string -> string -> string option -> (unit, string) result Lwt.t
  val get_community_by_slug : (module Caqti_lwt.CONNECTION) -> string -> (community option, string) result Lwt.t
  val search_communities : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (community list, string) result Lwt.t
  val update_community_details : (module Caqti_lwt.CONNECTION) -> int -> string option -> string option -> string option -> string option -> (unit, string) result Lwt.t
end

module User : sig
  val create_user : (module Caqti_lwt.CONNECTION) -> string -> string -> string -> string -> (unit, string) result Lwt.t
  val get_user_for_login : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string * bool * bool) option, string) result Lwt.t
  val anonymize_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val get_user_public : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string option * string option) option, string) result Lwt.t
  val update_user_profile : (module Caqti_lwt.CONNECTION) -> string option -> string option -> int -> (unit, string) result Lwt.t
  val get_user_karma : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val get_user_post_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
  val get_user_comment_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
  val search_users : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string option * string option) list, string) result Lwt.t
  val get_user_by_username : (module Caqti_lwt.CONNECTION) -> string -> (user option, string) result Lwt.t
  val get_admin_usernames : (module Caqti_lwt.CONNECTION) -> (string list, string) result Lwt.t
  val is_user_admin : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
end

module Post : sig
  val create_post : (module Caqti_lwt.CONNECTION) -> string -> string option -> string option -> int -> int -> (unit, string) result Lwt.t
  val get_all_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
  val get_posts_by_community : (module Caqti_lwt.CONNECTION) -> int -> string -> int -> int -> (post list, string) result Lwt.t
  val get_post_by_id : (module Caqti_lwt.CONNECTION) -> int -> (post option, string) result Lwt.t
  val get_posts_by_user : (module Caqti_lwt.CONNECTION) -> int -> (post list, string) result Lwt.t
  val vote_post : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
  val remove_post_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val soft_delete_post : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val search_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
end

module Comment : sig
  val get_comments : (module Caqti_lwt.CONNECTION) -> int -> (comment list, string) result Lwt.t
  val create_comment : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> int option -> (unit, string) result Lwt.t
  val vote_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
  val get_comments_by_user : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * string * int * string * int) list, string) result Lwt.t
  val remove_comment_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val soft_delete_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val search_comments : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string * int * int) list, string) result Lwt.t
end

module Membership : sig
  val join_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_member : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  val leave_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val get_user_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t
end

module Moderator : sig
  val add_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  val get_community_moderators : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
  val remove_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val get_moderated_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t
end

module Ban : sig
  val ban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val unban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_banned : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  val get_banned_users : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
end

module Notification : sig
  val get_notifications : (module Caqti_lwt.CONNECTION) -> int -> (notification list, string) result Lwt.t
  val count_unread_notifs : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val mark_notifs_read : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val create_notif : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> (unit, string) result Lwt.t
  val get_post_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val get_comment_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
end

module Analytics : sig
  val log_page_view : (module Caqti_lwt.CONNECTION) -> string -> string option -> (unit, string) result Lwt.t
  val touch_user_active : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val get_kpi_dashboard : (module Caqti_lwt.CONNECTION) -> (((int * int * int) * (int * int * int) * (int * int * int) * (int * int * int)), string) result Lwt.t
end

module Security : sig
  val update_password : (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
  val verify_email : (module Caqti_lwt.CONNECTION) -> string -> (string option, string) result Lwt.t
end

module Rate_limit : sig
  val check : (module Caqti_lwt.CONNECTION) -> string -> string -> ([`Allowed | `Blocked], string) result Lwt.t
end

module Admin : sig
  val admin_delete_post : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
  val admin_delete_comment : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
  val ban_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val is_globally_banned : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
  val unban_user_global : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val get_globally_banned_users : (module Caqti_lwt.CONNECTION) -> (user list, string) result Lwt.t
end

module PasswordReset : sig
  val create_token : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
  val validate_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
  val consume_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
end

val create_user : (module Caqti_lwt.CONNECTION) -> string -> string -> string -> string -> (unit, string) result Lwt.t
val get_user_for_login : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string * bool * bool) option, string) result Lwt.t
val anonymize_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_user_public : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string option * string option) option, string) result Lwt.t
val update_user_profile : (module Caqti_lwt.CONNECTION) -> string option -> string option -> int -> (unit, string) result Lwt.t
val get_user_karma : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val verify_email : (module Caqti_lwt.CONNECTION) -> string -> (string option, string) result Lwt.t
val update_password : (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
val get_user_by_username : (module Caqti_lwt.CONNECTION) -> string -> (user option, string) result Lwt.t
val get_admin_usernames : (module Caqti_lwt.CONNECTION) -> (string list, string) result Lwt.t
val is_user_admin : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t

val get_all_communities : (module Caqti_lwt.CONNECTION) -> (community list, string) result Lwt.t
val create_community : (module Caqti_lwt.CONNECTION) -> string -> string -> string option -> (unit, string) result Lwt.t
val get_community_by_slug : (module Caqti_lwt.CONNECTION) -> string -> (community option, string) result Lwt.t
val update_community_details : (module Caqti_lwt.CONNECTION) -> int -> string option -> string option -> string option -> string option -> (unit, string) result Lwt.t
val join_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val leave_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_member : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_user_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t

val add_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_community_moderators : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
val remove_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_moderated_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t

val community_ban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val community_unban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val community_is_banned : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val community_get_banned_users : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t

val create_post : (module Caqti_lwt.CONNECTION) -> string -> string option -> string option -> int -> int -> (unit, string) result Lwt.t
val get_all_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
val get_posts_by_community : (module Caqti_lwt.CONNECTION) -> int -> string -> int -> int -> (post list, string) result Lwt.t
val get_post_by_id : (module Caqti_lwt.CONNECTION) -> int -> (post option, string) result Lwt.t
val get_posts_by_user : (module Caqti_lwt.CONNECTION) -> int -> (post list, string) result Lwt.t
val soft_delete_post : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val create_comment : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> int option -> (unit, string) result Lwt.t
val get_comments : (module Caqti_lwt.CONNECTION) -> int -> (comment list, string) result Lwt.t
val get_comments_by_user : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * string * int * string * int) list, string) result Lwt.t
val soft_delete_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val vote_post : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
val remove_post_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_user_post_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
val vote_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
val remove_comment_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_user_comment_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t

val get_notifications : (module Caqti_lwt.CONNECTION) -> int -> (notification list, string) result Lwt.t
val count_unread_notifs : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val mark_notifs_read : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val create_notif : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> (unit, string) result Lwt.t
val get_post_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val get_comment_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val log_page_view : (module Caqti_lwt.CONNECTION) -> string -> string option -> (unit, string) result Lwt.t
val touch_user_active : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_kpi_dashboard : (module Caqti_lwt.CONNECTION) -> (((int * int * int) * (int * int * int) * (int * int * int) * (int * int * int)), string) result Lwt.t

val search_communities : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (community list, string) result Lwt.t
val search_users : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string option * string option) list, string) result Lwt.t
val search_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
val search_comments : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string * int * int) list, string) result Lwt.t

val admin_delete_post : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
val admin_delete_comment : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
val mod_delete_post : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val mod_delete_comment : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val ban_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val is_globally_banned : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
val unban_user_global : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_globally_banned_users : (module Caqti_lwt.CONNECTION) -> (user list, string) result Lwt.t

val password_reset_create_token : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
val password_reset_validate_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
val password_reset_consume_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
