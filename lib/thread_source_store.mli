(* One source row of a promoted thread (thread page display). Chronologically ordered by
   the queries that return it. sm_author is "" for a tombstoned author; sm_deleted rows
   carry no author/content (masked at the SQL layer) — render "[message unavailable]".
   Fields prefixed (sm_) to avoid record-field disambiguation. *)
type thread_source_msg = {
  sm_id : int64;
  sm_author : string;
  sm_content : string;
  sm_created_at : string;
  sm_is_seed : bool;
  sm_deleted : bool;
}

(* "Start thread from chat" provenance: links a durable thread to its source chat
   messages. See thread_source_store.ml for the seed-uniqueness invariant. *)
(* The canonical thread a chat message already seeds -> (post_id, title, community_slug). *)
val get_seed_thread_for_message : (module Caqti_lwt.CONNECTION) -> int64 -> ((int * string * string) option, string) result Lwt.t
(* Every thread-source link in a channel -> (message_id, post_id, post_title, is_seed),
   non-seed references included. One bounded query; the renderer groups by message id. *)
val get_thread_links_for_channel : (module Caqti_lwt.CONNECTION) -> int -> ((int64 * int * string * bool) list, string) result Lwt.t
(* Thread attribution: (source channel (slug, name, community_id) option, source
   messages in chronological order — deleted rows included but masked). The CALLER
   decides viewer visibility: authorized readers of the returned community id see the
   conversation; everyone else gets only a neutral "promoted from a private
   conversation" notice. *)
val get_thread_source : (module Caqti_lwt.CONNECTION) -> int -> ((string * string * int) option * thread_source_msg list, string) result Lwt.t
(* Reverse navigation: promoted post -> (source channel id, post title, ids of
   surviving source messages, chronological). None when nothing usable remains. *)
val get_source_span_for_thread : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * int64 list) option, string) result Lwt.t
(* Batch chat-provenance for a page of post ids -> (post_id, channel_slug, channel_name,
   source_message_count). One bounded query (ids passed comma-joined); only posts started
   from chat appear. Empty input -> []. Lets search show "Started from #channel" with no N+1. *)
val get_thread_sources_for_posts : (module Caqti_lwt.CONNECTION) -> int list -> ((int * string * string * int) list, string) result Lwt.t
(* Atomic create of a chat-started thread: post (with promoted_from_channel_id) plus
   seed + context source rows in one transaction. Seed is forced is_seed=TRUE at
   position 0; the partial unique index rejects a racing duplicate seed. Returns the
   new post id. *)
val start_thread_from_chat :
  (module Caqti_lwt.CONNECTION) ->
  title:string -> content:string option -> section_id:int option ->
  community_id:int -> user_id:int -> channel_id:int ->
  seed_message_id:int64 -> context_message_ids:int64 list ->
  (int, string) result Lwt.t
