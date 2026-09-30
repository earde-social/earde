(** Helpers shared by the HTTP handlers: local-redirect reduction of
    attacker-controlled targets, the generic database-error message, mention
    extraction and the current user's votes. *)

val safe_local_redirect : ?default:string -> Dream.request -> string -> string
(** Reduce an attacker-controlled redirect target (Referer header or
    form-carried return path) to a local path+query. A path starting with '/'
    passes through; an absolute http(s) URL whose host and effective port
    match the request's Host header is reduced to its path+query. Everything
    else — protocol-relative, foreign-host, userinfo-bearing, malformed,
    backslash- or control-character-bearing values — collapses to [default]
    (itself a trusted local path, "/" when omitted). Fragments are dropped;
    the result never carries a scheme or authority. *)

val generic_db_error : string

val db_error_message : string -> string

val extract_mentions : string -> string list

val get_current_user_votes :
  (module Caqti_lwt.CONNECTION) -> Dream.request -> (int * int) list Lwt.t

val get_current_user_comment_votes :
  (module Caqti_lwt.CONNECTION) -> Dream.request -> (int * int) list Lwt.t
