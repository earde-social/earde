type comment = {
  id : int;
  content : string;
  username : string;
  created_at : string;
  score : int;
  parent_id : int option;
  avatar_url : string option;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(* Single-query report target for a comment: the comment, its author, its post, and the
   owning community. Fields prefixed (crt_) to avoid record-field disambiguation. *)
type comment_report_target = {
  crt_comment_id : int;
  crt_content : string;
  crt_author_user_id : int;
  crt_author_username : string;
  crt_post_id : int;
  crt_post_title : string;
  crt_community_id : int;
  crt_community_slug : string;
}

val get_comments :
  (module Caqti_lwt.CONNECTION) -> int -> (comment list, string) result Lwt.t

val create_comment :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  int ->
  int ->
  int option ->
  ([ `Created of int | `Invalid_parent ], string) result Lwt.t
(** [create_comment db content post_id user_id parent_id]. A present [parent_id]
    is accepted only when that comment belongs to the SAME [post_id]; the check
    lives in the INSERT's own WHERE, so a concurrent change cannot open a gap
    between validating and writing. [`Invalid_parent] means nothing was inserted
    — the parent is missing, or lives on another post (hence possibly another
    community, possibly a private one) — and is a client error, not a storage
    failure. *)

val touch_last_activity :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

(* The community that owns a comment, via its CANONICAL parent post — the
   same binding create_comment_handler authorizes against. A shared thread's
   destination community never enters into it. [None] = no such comment. *)
val get_comment_community_id :
  (module Caqti_lwt.CONNECTION) -> int -> (int option, string) result Lwt.t

val vote_comment :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  int ->
  (unit, string) result Lwt.t

val get_comments_by_user :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  ((int * string * string * int * string * int) list, string) result Lwt.t

val remove_comment_vote :
  (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val soft_delete_comment :
  (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val search_comments :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  int ->
  int ->
  ((int * string * string * string * int * int) list, string) result Lwt.t

(* Resolve a comment + its author/post/community in one query for the report flow.
   [None] when the comment, its post, or its community no longer exists. *)
val get_comment_report_target :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (comment_report_target option, string) result Lwt.t
