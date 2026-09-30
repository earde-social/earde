(** Reusable rendering primitives: avatars and initials tiles, author
    labels, and relative and calendar dates. Escaping and URL policy live
    in {!Html}. *)

val user_avatar :
  ?alt:string ->
  img_class:string ->
  tile_class:string ->
  username:string ->
  string option ->
  Html.t
(** The avatar [<img>] when the stored URL passes {!Html.image_src_opt},
    else the user's initials tile. *)

val community_avatar :
  ?alt:string ->
  img_class:string ->
  tile_class:string ->
  name:string ->
  string option ->
  Html.t

val community_banner :
  wrap_class:string -> img_class:string -> fallback_class:string ->
  string option -> Html.t

val is_deleted_user : string -> bool
(** The tombstone username that [User_store.anonymize_user] writes. *)

val render_author :
  ?mod_usernames:string list -> ?admin_usernames:string list -> string ->
  Html.t
(** The author link, or the plain [deleted] label for a tombstone, with the
    moderator and admin badges when the name is in the given lists. *)

val time_ago : string -> string
(** A database timestamp as relative plain text ("3 hr ago"); the input
    unchanged when it does not parse. *)

val format_month_year : string -> string
(** A database timestamp as plain text month and year. *)
