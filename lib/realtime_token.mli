(** Short-lived HMAC-signed tokens that admit a browser to one realtime topic.
    The payload is transparent base64url JSON (user, username, topic, the
    shared-cursors capability and expiry) followed by an HMAC-SHA256 signature
    under [REALTIME_TOKEN_SECRET]; the gateway verifies it with the same secret.
*)

val secret_env : string
(** The environment variable that holds the signing secret. *)

val default_ttl_seconds : int
(** Token lifetime; the refresh endpoint reports it to the client. *)

val create_for_topic :
  user_id:int ->
  username:string ->
  topic:string ->
  shared_cursors:bool ->
  string option
(** A token for [topic], or [None] when no secret is configured.
    [shared_cursors] is a capability claim the gateway trusts only because it is
    signed: callers derive it server-side, never from client input. *)

val decode_payload : string -> Yojson.Safe.t option
(** The token's payload as JSON, without verifying the signature. For tests that
    assert topic binding and expiry. *)

val hmac_sha256_base64url : secret:string -> string -> string
(** The signature primitive, exposed so tests can sign a tampered payload. *)

val unix_now : unit -> int
