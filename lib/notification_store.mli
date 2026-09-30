type notification = {
  id : int;
  user_id : int;
  post_id : int option;
  notif_type : string;
  (* NULL for the structured project-home and community-connection kinds,
     which render from the joined display fields instead of stored prose. *)
  message : string option;
  is_read : bool;
  created_at : string;
  project_name : string option;
  project_slug : string option;
  (* The notification's own community subject. For the community-connection
     kinds this is the recipient's management context — the community whose
     connections surface the rendered row links to — never the counterpart. *)
  community_name : string option;
  community_slug : string option;
  (* The other community of a connection notification, derived at read time
     relative to [community_slug] so the same stored row reads correctly from
     either direction. NULL for every other kind. *)
  counterpart_name : string option;
  counterpart_slug : string option;
  (* The shared-thread subjects, joined at read time and NULL for every other
     kind. The two [_visible] booleans are the recipient's CURRENT read
     access to each side (public, or member / moderator / durable admin),
     computed in the same bounded query: the renderer must not show a thread
     title, community name, or link its recipient could no longer reach. *)
  st_post_id : int option;
  st_post_title : string option;
  st_origin_name : string option;
  st_origin_slug : string option;
  st_destination_name : string option;
  st_destination_slug : string option;
  (* Whether the notification's own community is the placement's origin side
     — which management surface the recipient's copy should point at. *)
  st_origin_context : bool option;
  st_origin_visible : bool option;
  st_destination_visible : bool option;
  (* Whether the recipient may CURRENTLY open the origin Share page and the
     destination Shared-threads management page — the two authorized link
     targets, whose gates are stricter than read access. Mirrors of those
     pages' own SQL rules (author-while-member-unbanned / origin top_mod /
     durable admin behind the session claim; destination top_mod / that same
     durable-admin pair): a link its recipient is guaranteed to 404 on must
     never render. *)
  st_share_capable : bool option;
  st_manage_capable : bool option;
}

(* [session_admin] is the caller's session admin claim; it only enables the
   durable users.is_admin check inside the capability columns, never
   replaces it. *)
val get_notifications :
  (module Caqti_lwt.CONNECTION) ->
  session_admin:bool ->
  int ->
  (notification list, string) result Lwt.t

val count_unread_notifs :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t

val mark_notifs_read :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val create_notif :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int option ->
  string ->
  string ->
  (unit, string) result Lwt.t

val get_post_owner :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t

val get_comment_owner :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t

val get_comment_post_id :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
