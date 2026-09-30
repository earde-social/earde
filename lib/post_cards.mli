(** Post and forum-row cards with their author, moderation and sharing controls.
*)

val post_admin_actions :
  ?is_current_user_mod:bool ->
  ?admin_usernames:string list ->
  ?banned_usernames:string list ->
  csrf_token:Html.t ->
  Dream.request ->
  Post_types.post ->
  Html.t

val canonical_thread_path : string -> int -> string -> string

type feed_shared = string * Post_types.feed_shared_context

val shared_with_html : (string * string) list -> Html.t

val render_post :
  ?is_current_user_mod:bool ->
  ?mod_usernames:string list ->
  ?admin_usernames:string list ->
  ?banned_usernames:string list ->
  ?shared:feed_shared ->
  Dream.request ->
  (int * int) list ->
  Post_types.post ->
  Html.t

val extract_domain : string -> string option

val render_forum_row :
  ?is_current_user_mod:bool ->
  ?mod_usernames:string list ->
  ?admin_usernames:string list ->
  ?banned_usernames:string list ->
  ?show_context:bool ->
  ?shared:feed_shared ->
  ?shared_with:(string * string) list ->
  Dream.request ->
  (int * int) list ->
  Post_types.post ->
  Html.t
