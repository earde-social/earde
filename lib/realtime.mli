(** Best-effort publication of a committed chat message to the realtime gateway.
    The message is already durable; a failed or slow publish is logged and
    dropped, and clients catch up over HTTP. *)

val publish_body :
  topic:string ->
  channel_id:int ->
  community_id:int ->
  message_id:int64 ->
  user_id:int ->
  username:string ->
  content:string ->
  created_at:string ->
  Yojson.Safe.t
(** The gateway request body. [topic] is the access-generation topic the caller
    read after the message committed (docs/features/realtime-access.md). *)

val publish_chat_message :
  topic:string ->
  channel_id:int ->
  community_id:int ->
  message_id:int64 ->
  user_id:int ->
  username:string ->
  content:string ->
  created_at:string ->
  unit Lwt.t
(** Posts {!publish_body} to the gateway when it is configured, bounded by a
    timeout. Never fails: every error is logged and swallowed. Does nothing when
    the gateway URL or internal secret is unset. *)

val post_json :
  ?timeout:float ->
  gateway_url:string ->
  internal_secret:string ->
  Yojson.Safe.t ->
  unit Lwt.t
(** One publish over a socket this call owns: the descriptor is closed when the
    response status line arrives, on any error, or after [timeout] seconds
    (default 0.3), whichever comes first, whatever the peer does. Only [http://]
    gateway URLs are used; any other scheme is logged and skipped. Exposed so
    the descriptor bound can be tested against hostile peers. *)
