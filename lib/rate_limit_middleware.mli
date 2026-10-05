(** The per-IP, per-operation limiter for sensitive POSTs. It fails closed: only
    an explicit allow invokes the wrapped handler. *)

val temporarily_unavailable : return_url:string -> Dream.handler
(** One generic 503 for a protected action that cannot be safely processed right
    now: the limiter's storage is unavailable, or the auth-mail dispatcher is
    full. Identical for every account state and names no cause; its only link is
    [return_url]. *)

(** One constructor per logical limited operation. A route names its operation
    explicitly, so every spelling of a path that reaches the same handler —
    percent-encoded, with repeated slashes, with a query, or with different
    route parameters — counts against the same bucket, while different
    operations keep separate buckets. *)
type operation =
  | Login
  | Signup
  | Forgot_password
  | Github_installation_start
  | Project_repository_selection
  | Project_creation
  | Project_home_request
  | Project_home_provisioning
  | Project_home_accept
  | Project_home_reject
  | Project_home_removal_by_project
  | Project_home_removal_by_community
  | Network_community_publication
  | Community_connection_request
  | Community_connection_accept
  | Community_connection_reject
  | Community_connection_removal
  | Shared_thread_share_request
  | Shared_thread_accept
  | Shared_thread_reject
  | Shared_thread_withdrawal
  | Shared_thread_removal

val bucket : operation -> string
(** The fixed bucket label stored for [operation]. Never derived from the
    request. *)

val return_path : string -> string
(** The blocked and unavailable pages' return link for a request target: the
    routed path's non-empty segments, decoded and re-encoded with only
    unreserved characters left bare, with no query or fragment. Always a single
    rooted path. *)

val middleware : operation -> Dream.handler -> Dream.handler
(** The shared per-IP, per-operation limiter for sensitive POSTs. Fails closed:
    only a positive Allowed decision invokes the wrapped handler; a blocked
    request gets the Too Many Attempts page, and a lookup error, rejected
    promise or pool failure gets a generic 503 without invoking it. *)

val make_middleware :
  check:
    (Dream.request ->
    ip:string ->
    endpoint:string ->
    ([ `Allowed | `Blocked ], string) result Lwt.t) ->
  cleanup:(Dream.request -> unit) ->
  operation ->
  Dream.handler ->
  Dream.handler
(** [middleware] with its enforcement lookup and its opportunistic, best-effort
    expiry cleanup supplied — so the decision logic can be exercised against a
    failing lookup or cleanup without a database. [cleanup] must not block;
    anything it raises is logged and cannot change the decision. *)
