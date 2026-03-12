(** Reusable HTML primitives. layout wraps every page; render_post and community_card
    are the only components with non-trivial state (CSRF tokens, session reads).
    All functions return raw HTML strings — no virtual DOM, no diffing overhead. *)

(** === ESCAPING === *)
val html_escape : string -> string
val safe_url : string -> string

(** === LAYOUT === *)
val layout : ?noindex:bool -> ?user:string -> ?request:Dream.request -> title:string -> string -> string

(** === HELPERS === *)
val is_deleted_user : string -> bool
val render_author : ?mod_usernames:string list -> ?admin_usernames:string list -> string -> string
val time_ago : string -> string

(** === CARDS === *)
val render_post : ?is_current_user_mod:bool -> ?mod_usernames:string list -> ?admin_usernames:string list -> ?banned_usernames:string list -> Dream.request -> (int * int) list -> Db.post -> string
val community_card : Db.community -> string
val left_sidebar : ?user:string -> moderated_communities:Db.community list -> Db.community list -> string
