(* Read-only rows for the /admin operational panels (recent users + pending signups). *)
type admin_recent_user = {
  id : int;
  username : string;
  email : string;
  created_at : string;
  is_admin : bool;
  is_banned : bool;
  post_count : int;
  comment_count : int;
  message_count : int;
}

type pending_signup_row = {
  id : int;
  username : string;
  email : string;
  created_at : string;
  expires_at : string;
  ip_address : string option;
}

val admin_delete_post :
  (module Caqti_lwt.CONNECTION) ->
  label:string ->
  int ->
  (unit, string) result Lwt.t

val admin_delete_comment :
  (module Caqti_lwt.CONNECTION) ->
  label:string ->
  int ->
  (unit, string) result Lwt.t

(* Community-scoped moderator tombstones: the mutation is bound to the route
   community and reports whether a row actually matched, so a zero-row update
   can never be mistaken for a deletion.
   mod_delete_post: Ok true = deleted; Ok false = no such post in that community.
   mod_delete_comment: Ok (Some post_id) = deleted (post_id of the parent post);
   Ok None = no such comment under that community's posts. *)
val mod_delete_post :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  int ->
  (bool, string) result Lwt.t

val mod_delete_comment :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  int ->
  (int option, string) result Lwt.t

(* GLOBAL ban. Flips users.is_banned AND revokes every Dream session of the
   banned user in one transaction, so an already-authenticated browser can
   neither keep browsing nor mint fresh realtime tokens. Fail-closed: a
   revocation failure rolls the ban back. unban does not resurrect sessions. *)
val ban_user :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val is_globally_banned :
  (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t

val unban_user_global :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val get_globally_banned_users :
  (module Caqti_lwt.CONNECTION) -> (User_store.user list, string) result Lwt.t

(* Read-only admin-dashboard reads, each bounded by [limit]. list_recent_users carries
   per-user post/comment/chat-message counts via one bounded aggregate query. *)
val list_recent_users :
  (module Caqti_lwt.CONNECTION) ->
  limit:int ->
  (admin_recent_user list, string) result Lwt.t

val list_recent_pending :
  (module Caqti_lwt.CONNECTION) ->
  limit:int ->
  (pending_signup_row list, string) result Lwt.t
