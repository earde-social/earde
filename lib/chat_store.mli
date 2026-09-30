(* id is int64 (BIGSERIAL column). user_id is optional on read only — nullable for
   later GDPR tombstoning — but Chat.send_message requires a non-optional user_id.
   deleted_at-set rows are still returned by archive reads (durable knowledge). *)
type chat_message = {
  id : int64;
  channel_id : int;
  user_id : int option;
  content : string;
  created_at : string;
  edited_at : string option;
  deleted_at : string option;
}

(* send_message channel_id user_id content -> the canonical persisted message
   (INSERT ... RETURNING: Postgres-assigned id and created_at included), so callers
   never need a post-insert read-back. user_id is a plain int: a new message is
   always attributed to its author. *)
val send_message :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  string ->
  (chat_message, string) result Lwt.t

val get_message_by_id :
  (module Caqti_lwt.CONNECTION) ->
  int64 ->
  (chat_message option, string) result Lwt.t

(* get_recent_messages_with_authors channel_id limit -> same as above but LEFT JOINs users
   so each message carries its author username (None when user_id is NULL). For SSR render. *)
val get_recent_messages_with_authors :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  ((chat_message * string option) list, string) result Lwt.t

(* Author-joined keyset reads for the start-thread form's nearby-message context. *)
val get_messages_before_id_with_authors :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int64 ->
  int ->
  ((chat_message * string option) list, string) result Lwt.t

val get_messages_after_id_with_authors :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int64 ->
  int ->
  ((chat_message * string option) list, string) result Lwt.t
