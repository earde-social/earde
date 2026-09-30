(** The per-IP, per-path limiter for sensitive POSTs. It fails closed: only
    an explicit allow invokes the wrapped handler. *)

val temporarily_unavailable : return_url:string -> Dream.handler
(** One generic 503 for a protected action that cannot be safely processed
    right now: the limiter's storage is unavailable, or the auth-mail
    dispatcher is full. Identical for every account state and names no
    cause; its only link is [return_url]. *)

val middleware : Dream.handler -> Dream.handler
(** The shared per-IP, per-path limiter for sensitive POSTs. Fails closed:
    only a positive Allowed decision invokes the wrapped handler; a
    blocked request gets the Too Many Attempts page, and a lookup error,
    rejected promise or pool failure gets a generic 503 without invoking
    it. *)

val make_middleware :
  check:
    (Dream.request -> ip:string -> endpoint:string ->
     ([ `Allowed | `Blocked ], string) result Lwt.t) ->
  cleanup:(Dream.request -> unit) ->
  Dream.handler -> Dream.handler
(** [middleware] with its enforcement lookup and its opportunistic,
    best-effort expiry cleanup supplied — so the decision logic can be
    exercised against a failing lookup or cleanup without a database.
    [cleanup] must not block; anything it raises is logged and cannot
    change the decision. *)
