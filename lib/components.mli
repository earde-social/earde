(** Reusable rendering primitives: escaping and URL policies, avatars and
    initials tiles, author labels and relative times. *)

val html_escape : string -> string

val safe_url : string -> string
(** [safe_internal_path p] gates a rooted internal path (e.g. "/c/x/t/1"):
    passes a single-leading-slash path (html-escaped) and the bare site root "/",
    rejects ""/"#"/protocol-relative "//host" (and the "/\\host" variant) → "#".
    Use for app nav targets, NOT [safe_url] (which only passes http(s) and would
    collapse every relative path to "#").

    Safe for ATTACKER-supplied values as well as server-built ones: it escapes
    what it passes and refuses anything that could leave the origin, which is
    what lets the shared message page gate its "Go back" destination once
    instead of at ~70 call sites. *)

val safe_internal_path : string -> string

val js_single_quoted_attr : string -> string

val safe_img_src : string -> string

val user_avatar : ?alt:string -> img_class:string -> tile_class:string -> username:string -> string option -> string

val community_avatar : ?alt:string -> img_class:string -> tile_class:string -> name:string -> string option -> string

val community_banner : wrap_class:string -> img_class:string -> fallback_class:string -> string option -> string

val is_deleted_user : string -> bool
(** [extract_domain url] → bare host (no scheme/www) for a link post's domain chip, or [None]. *)

val render_author : ?mod_usernames:string list -> ?admin_usernames:string list -> string -> string

val time_ago : string -> string

val format_month_year : string -> string
