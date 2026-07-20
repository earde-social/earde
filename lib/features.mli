(** Community-level feature capabilities, decided server-side. *)

val default_shared_cursor_slugs : string list
(** Communities with shared cursors when the env override is unset. *)

val slugs_of_string : string -> string list
(** Parse a comma-separated slug list: trimmed, lowercased, empties dropped. *)

val enabled_in : slugs:string list -> community_slug:string -> bool
(** Case-insensitive membership test. Pure; exposed for tests. *)

val shared_cursors_enabled : community_slug:string -> bool
(** Whether the shared-cursor UI may render for this community, from
    EARDE_SHARED_CURSOR_COMMUNITIES or the default allow-list. *)
