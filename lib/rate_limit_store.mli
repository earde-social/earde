val check :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  string ->
  ([ `Allowed | `Blocked ], string) result Lwt.t

val upload_endpoint : string
(** The image-upload bucket: a constant endpoint name shared by all three
    upload-capable routes, with its own allowance ([upload_max_attempts]) over
    the same [window_seconds] window. Separate from the authentication allowance
    because an upload costs far more than a login attempt while a member
    legitimately edits several images in a row. *)

val upload_max_attempts : int

val check_upload :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  ([ `Allowed | `Blocked ], string) result Lwt.t

val window_seconds : float
(** The single enforcement window (seconds); [cleanup_after_seconds] is derived
    from it (2x), so cleanup can never remove a row a configured window still
    needs. *)

val cleanup_after_seconds : float

val cleanup_expired :
  ?now:float -> (module Caqti_lwt.CONNECTION) -> (int, string) result Lwt.t
(** Deletes one bounded batch of rate-limit rows STRICTLY older than
    [now - cleanup_after_seconds] (a row at exactly the boundary is kept) and
    returns how many were removed. The only bind parameter is a timestamp, so no
    IP address can reach a query, parameter, or error string. [?now] exists for
    tests; production callers omit it. *)
