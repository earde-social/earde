type user = { id : int; username : string; email : string }

(* Nested row: (id, username, email, created_at), (password_hash, is_admin,
   is_banned) — created_at rides along for the analytics person $set. *)
val get_user_for_login :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  ( ((int * string * string * string) * (string * bool * bool)) option,
    string )
  result
  Lwt.t

val login_still_current :
  (module Caqti_lwt.CONNECTION) ->
  id:int ->
  hash:string ->
  (bool, string) result Lwt.t
(** Whether account [id] is still live, unbanned and has password [hash], read
    under FOR SHARE so that a concurrent deletion, ban or password change either
    finishes first and is seen, or comes after and revokes the session the
    caller already wrote. *)

val anonymize_user :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val get_user_public :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  ( (int * string * string * string option * string option) option,
    string )
  result
  Lwt.t

val get_user_avatar_url :
  (module Caqti_lwt.CONNECTION) -> int -> (string option, string) result Lwt.t
(** avatar_url by user id ([Ok None] for no avatar or no such user). Read BEFORE
    anonymization, which NULLs the column, so the deletion handler can remove
    the stored upload after commit. *)

(* Analytics person properties (username, email, created_at, is_admin) —
   the closed §4.3 set, used only by the consent-grant sync. *)
val get_user_analytics_props :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  ((string * string * string * bool) option, string) result Lwt.t

val update_user_profile :
  (module Caqti_lwt.CONNECTION) ->
  string option ->
  string option ->
  int ->
  (unit, string) result Lwt.t

val get_user_karma :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t

val get_user_post_votes :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  ((int * int) list, string) result Lwt.t

val get_user_comment_votes :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  ((int * int) list, string) result Lwt.t

val search_users :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  int ->
  int ->
  ((int * string * string * string option * string option) list, string) result
  Lwt.t

val get_user_by_username :
  (module Caqti_lwt.CONNECTION) -> string -> (user option, string) result Lwt.t

val get_admin_usernames :
  (module Caqti_lwt.CONNECTION) -> (string list, string) result Lwt.t

val is_user_admin :
  (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t

(* Presence is operational state, not analytics: last_active_at feeds
   Moderator.demote_inactive_mods and must survive analytics changes. *)
val touch_user_active :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
