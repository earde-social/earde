(** Live chat: the channel page, message history as JSON, message sending and
    the realtime token. Messages are persisted before any live publish. *)

val community_channel_handler : Dream.handler

val chat_message_json :
  channel_id:int ->
  community_id:int ->
  ?thread_id:int ->
  Chat_store.chat_message * string option ->
  Yojson.Safe.t

module Chat_api : sig
  val wants_json : string option -> bool
  val max_content_length : int
  val validate_content : string -> (string, [ `Empty | `Too_long ]) result
  val error_json : code:string -> message:string -> string
  val internal_error_json : string
end

val channel_messages_json_handler : Dream.handler
val realtime_token_handler : Dream.handler
val send_message_handler : Dream.handler
