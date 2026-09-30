open Lwt.Infix

(* id is int64: the column is BIGSERIAL (Postgres int8) — chat out-volumes posts,
   so we map it honestly rather than risk a driver mismatch / truncation on int.
   user_id is int option on READ only (nullable for later GDPR tombstoning of
   existing rows); Chat.send_message still requires a non-optional user_id so a new
   message can never be authored anonymously by accident. deleted_at stays set-but-
   returned: soft-deleted messages remain in archive reads (durable knowledge), and
   the render layer decides whether to mask them. *)
type chat_message = {
  id : int64;
  channel_id : int;
  user_id : int option;
  content : string;
  created_at : string;
  edited_at : string option;
  deleted_at : string option;
}

(* Durable chat. Postgres is the source of truth; all chat writes funnel through here
   so a later Realtime_bus can fan out copies after the DB commit without becoming the
   source of truth. No JOIN to users: author display is a render concern and these
   queries stay index-only on (channel_id, id). search_tsv is left unmaintained until
   the search-indexing step. *)
(* 7-column message row: t2(t4, t3). id is int64 (BIGSERIAL); the three timestamps
   are ::text-cast to keep every row field string-typed, matching the other stores. *)
let chat_message_row_type =
  let open Caqti_type in
  t2
    (t4 int64 int (option int) string)
    (t3 string (option string) (option string))

let map_chat_message_row
    ((id, channel_id, user_id, content), (created_at, edited_at, deleted_at)) :
    chat_message =
  { id; channel_id; user_id; content; created_at; edited_at; deleted_at }

(* user_id is the non-optional int here (see chat_message comment); created_at,
   edited_at, deleted_at take their column defaults/NULL. RETURNING the full row
   hands the caller the canonical persisted message — Postgres-assigned id and
   created_at included — in the single insert round-trip, so the composer's JSON
   response and the realtime publish never need a read-back. *)
let send_message_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int int string) ->! chat_message_row_type)
    "INSERT INTO chat_messages (channel_id, user_id, content) VALUES ($1, $2, \
     $3) RETURNING id, channel_id, user_id, content, created_at::text, \
     edited_at::text, deleted_at::text"

let send_message (module C : Caqti_lwt.CONNECTION) channel_id user_id content =
  C.find send_message_query (channel_id, user_id, content) >>= function
  | Ok row -> Lwt.return (Ok (map_chat_message_row row))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? chat_message_row_type)
    "SELECT id, channel_id, user_id, content, created_at::text, \
     edited_at::text, deleted_at::text FROM chat_messages WHERE id = $1"

let get_message_by_id (module C : Caqti_lwt.CONNECTION) id =
  C.find_opt get_by_id_query id >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_chat_message_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* SSR initial render needs the author name per message; the index-only reads above
   deliberately skip the users JOIN (author display is a render concern, the hot
   realtime path stays on the (channel_id, id) index). This separate render-oriented
   read LEFT JOINs users so a single round-trip resolves authors — username is None
   when user_id is NULL (GDPR tombstone) and the render layer shows "[deleted]".
   8-column row: t2(t4, t4). *)
let chat_message_author_row_type =
  let open Caqti_type in
  t2
    (t4 int64 int (option int) string)
    (t4 string (option string) (option string) (option string))

let map_chat_message_author_row
    ( (id, channel_id, user_id, content),
      (created_at, edited_at, deleted_at, username) ) :
    chat_message * string option =
  ( { id; channel_id; user_id; content; created_at; edited_at; deleted_at },
    username )

let get_recent_with_authors_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, \
     m.edited_at::text, m.deleted_at::text, u.username FROM chat_messages m \
     LEFT JOIN users u ON u.id = m.user_id WHERE m.channel_id = $1 ORDER BY \
     m.id DESC LIMIT $2"

let get_recent_messages_with_authors (module C : Caqti_lwt.CONNECTION)
    channel_id limit =
  Query_timer.with_query_timer ~name:"chat_get_recent_messages_with_authors"
    (fun () ->
      C.collect_list get_recent_with_authors_query (channel_id, limit)
      >>= function
      | Ok rows ->
          Lwt.return (Ok (List.rev_map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

(* Author-joined variants of the before/after keyset reads, used by the
   "start thread from chat" form to show nearby messages with their authors
   (so the body prefill can quote "Name: > text"). Same 8-column author row and
   LEFT JOIN-users tombstone handling as get_recent_with_authors; the hot
   realtime path keeps using the index-only readers above. *)
let get_before_with_authors_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int int64 int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, \
     m.edited_at::text, m.deleted_at::text, u.username FROM chat_messages m \
     LEFT JOIN users u ON u.id = m.user_id WHERE m.channel_id = $1 AND m.id < \
     $2 ORDER BY m.id DESC LIMIT $3"

let get_messages_before_id_with_authors (module C : Caqti_lwt.CONNECTION)
    channel_id before_id limit =
  Query_timer.with_query_timer ~name:"chat_get_messages_before_id_with_authors"
    (fun () ->
      C.collect_list get_before_with_authors_query (channel_id, before_id, limit)
      >>= function
      | Ok rows ->
          Lwt.return (Ok (List.rev_map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

let get_after_with_authors_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 int int64 int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, \
     m.edited_at::text, m.deleted_at::text, u.username FROM chat_messages m \
     LEFT JOIN users u ON u.id = m.user_id WHERE m.channel_id = $1 AND m.id > \
     $2 ORDER BY m.id ASC LIMIT $3"

let get_messages_after_id_with_authors (module C : Caqti_lwt.CONNECTION)
    channel_id after_id limit =
  Query_timer.with_query_timer ~name:"chat_get_messages_after_id_with_authors"
    (fun () ->
      C.collect_list get_after_with_authors_query (channel_id, after_id, limit)
      >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))
