open Lwt.Infix

(* One source row of a promoted thread, as displayed on the thread page.
   sm_author is "" for a tombstoned author. sm_deleted rows are masked in SQL
   (author and content forced to "") so deleted chat text never leaves the DB
   layer; the render layer shows a "[message unavailable]" placeholder. *)
type thread_source_msg = {
  sm_id : int64;
  sm_author : string;
  sm_content : string;
  sm_created_at : string;
  sm_is_seed : bool;
  sm_deleted : bool;
}

(* Provenance for the "Start thread from chat" feature: links a durable forum thread
   back to the chat messages it crystallized. Kept in its own cohesive module because
   it spans posts + chat_messages + thread_source_messages; normal post creation
   (Post.create_post) is left untouched per the no-broad-refactor rule.

   TODO(P0.5 — before serious external onboarding): thread_source_messages stores
   REFERENCES only, no snapshots. Consequences today: a later edit shows the edited
   text on the thread; a soft-delete masks the row to "[message unavailable]"; user
   anonymization tombstones the author; a HARD delete CASCADEs the relation away and
   silently shrinks the conversation. The follow-up is an additive migration adding
   author/content/timestamp snapshot columns captured at promotion time — which first
   requires deciding the semantics for edits, user deletion, moderation deletion and
   hard deletion (i.e. when the snapshot may still be shown vs. must be suppressed). *)
(* The canonical thread a chat message already seeds, if any -> (post_id, title,
   community_slug). Drives the "already started -> link to it" guard and the
   "Thread ->" channel marker. Only is_seed rows count: a message merely used as
   context for another thread is still free to seed its own. *)
let seed_thread_for_message_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->? Caqti_type.(t3 int string string))
  "SELECT p.id, p.title, c.slug \
   FROM thread_source_messages t \
   JOIN posts p ON p.id = t.post_id \
   JOIN communities c ON c.id = p.community_id \
   WHERE t.message_id = $1 AND t.is_seed = TRUE LIMIT 1"

let get_seed_thread_for_message (module C : Caqti_lwt.CONNECTION) message_id =
  C.find_opt seed_thread_for_message_query message_id >>= function
  | Ok row -> Lwt.return (Ok row)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Every thread-source link in a channel -> (message_id, post_id, post_title, is_seed),
   including non-seed (context) references. One bounded query for the whole channel
   (no N+1); the renderer groups by message id. Ordered is_seed-first then most-recent
   post so the renderer's deterministic target pick is stable. A message can be both a
   seed (of its own thread) and context (of others) — both rows are returned. *)
let thread_links_for_channel_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 (t2 int64 int) (t2 string bool)))
  "SELECT t.message_id, p.id, p.title, t.is_seed \
   FROM thread_source_messages t \
   JOIN posts p ON p.id = t.post_id \
   JOIN chat_messages m ON m.id = t.message_id \
   WHERE m.channel_id = $1 \
   ORDER BY t.is_seed DESC, p.id DESC"

let get_thread_links_for_channel (module C : Caqti_lwt.CONNECTION) channel_id =
  C.collect_list thread_links_for_channel_query channel_id >>= function
  | Ok rows -> Lwt.return (Ok (List.map (fun ((mid, pid), (title, is_seed)) -> (mid, pid, title, is_seed)) rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Source channel of a promoted thread (posts.promoted_from_channel_id) ->
   (slug, name, source-channel community id). None when the post was not started from
   chat, or the channel was deleted (the FK is ON DELETE SET NULL so a durable thread
   survives channel deletion). Whether the VIEWER may see this provenance is decided by
   the handler (can_view_community on the returned community id) — access is
   authorization-driven, not indexability-driven, so members of a private community
   keep their provenance while unauthorized viewers get only a neutral notice. *)
let thread_source_channel_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.(t3 string string int))
  "SELECT c.slug, c.name, c.community_id FROM posts p \
   JOIN channels c ON c.id = p.promoted_from_channel_id \
   WHERE p.id = $1"

(* Ordered source messages for the promoted-conversation block. Chronological by
   created_at (id as tiebreak) — NOT by stored position — so the seed sits at its real
   place in the conversation; is_seed is carried only for the origin badge. Soft-deleted
   rows are returned (so the conversation doesn't silently shrink) but their author and
   content are masked to '' in SQL; the renderer shows "[message unavailable]". *)
let thread_source_messages_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 (t3 int64 string string) (t3 string bool bool)))
  "SELECT t.message_id, \
          CASE WHEN m.deleted_at IS NULL THEN COALESCE(u.username, '') ELSE '' END, \
          CASE WHEN m.deleted_at IS NULL THEN m.content ELSE '' END, \
          m.created_at::text, t.is_seed, (m.deleted_at IS NOT NULL) \
   FROM thread_source_messages t \
   JOIN chat_messages m ON m.id = t.message_id \
   LEFT JOIN users u ON u.id = m.user_id \
   WHERE t.post_id = $1 \
   ORDER BY m.created_at ASC, m.id ASC"

let map_thread_source_msg ((sm_id, sm_author, sm_content), (sm_created_at, sm_is_seed, sm_deleted)) =
  { sm_id; sm_author; sm_content; sm_created_at; sm_is_seed; sm_deleted }

let get_thread_source (module C : Caqti_lwt.CONNECTION) post_id =
  C.find_opt thread_source_channel_query post_id >>= function
  | Error err -> Lwt.return (Error (Caqti_error.show err))
  | Ok channel_row ->
      C.collect_list thread_source_messages_query post_id >>= function
      | Ok rows -> Lwt.return (Ok (channel_row, List.map map_thread_source_msg rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Reverse navigation ("View original conversation"): resolve a promoted thread to its
   source channel id, its title (for the chat-side context notice), and the ids of its
   still-available (non-deleted) source messages in chronological order. None when the
   post was never promoted, its channel is gone, or no source message survives — the
   channel page then renders normally with no highlight. *)
let source_span_for_thread_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t3 int string int64))
  "SELECT c.id, p.title, t.message_id \
   FROM posts p \
   JOIN channels c ON c.id = p.promoted_from_channel_id \
   JOIN thread_source_messages t ON t.post_id = p.id \
   JOIN chat_messages m ON m.id = t.message_id \
   WHERE p.id = $1 AND m.deleted_at IS NULL \
   ORDER BY m.created_at ASC, m.id ASC"

let get_source_span_for_thread (module C : Caqti_lwt.CONNECTION) post_id =
  C.collect_list source_span_for_thread_query post_id >>= function
  | Ok [] -> Lwt.return (Ok None)
  | Ok ((channel_id, title, _) :: _ as rows) ->
      Lwt.return (Ok (Some (channel_id, title, List.map (fun (_, _, mid) -> mid) rows)))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Batch chat-provenance for one page of post/search results:
   post_id -> (channel_slug, channel_name, source_message_count). One bounded query over
   the current page's ids — the ids are passed comma-joined and expanded with
   string_to_array(...)::int[] to sidestep Caqti array binding — so the Threads search tab
   can show "Started from #channel" without an N+1. Only rows actually started from chat
   (promoted_from_channel_id NOT NULL, channel still present) come back; callers treat a
   missing post_id as "no marker". Empty input short-circuits with no query.
   A marker is suppressed when its source channel is not publicly indexable (private
   community, community indexable=false, or channel indexable=false), so the public search
   Threads tab never names a private/non-indexable source channel. *)
let thread_sources_for_posts_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->* Caqti_type.(t4 int string string int))
  "SELECT p.id, c.slug, c.name, \
          (SELECT COUNT(*) FROM thread_source_messages t WHERE t.post_id = p.id) \
   FROM posts p \
   JOIN channels c ON c.id = p.promoted_from_channel_id \
   JOIN communities oc ON oc.id = c.community_id \
   WHERE p.promoted_from_channel_id IS NOT NULL \
     AND oc.visibility = 'public' AND oc.indexable AND c.indexable \
     AND p.id = ANY(string_to_array($1, ',')::int[])"

let get_thread_sources_for_posts (module C : Caqti_lwt.CONNECTION) post_ids =
  match post_ids with
  | [] -> Lwt.return (Ok [])
  | _ ->
      let csv = String.concat "," (List.map string_of_int post_ids) in
      C.collect_list thread_sources_for_posts_query csv >>= function
      | Ok rows -> Lwt.return (Ok rows)
      | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Dedicated insert for chat-started threads: sets promoted_from_channel_id and leaves
   url/image_url NULL. Separate from Post.create_post so the normal post path is
   untouched. RETURNING id. 6 params: (title, content, section_id), (community_id,
   user_id, channel_id). *)
let insert_promoted_post_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string (option string) (option int)) (t3 int int int)) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, section_id, community_id, user_id, promoted_from_channel_id) \
   VALUES ($1, $2, $3, $4, $5, $6) RETURNING id"

let insert_source_message_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int int64) (t2 int bool)) ->. Caqti_type.unit)
  "INSERT INTO thread_source_messages (post_id, message_id, position, is_seed) \
   VALUES ($1, $2, $3, $4)"

(* Atomic create: the post and all its source-message links land together, or not at
   all. The seed is always stored is_seed=TRUE at position 0; context messages follow
   in caller-supplied (chronological) order at positions 1.. . The partial unique index
   on (message_id) WHERE is_seed makes a racing double-submit fail here — the caller
   re-checks get_seed_thread_for_message on Error and shows the friendly "already
   started" link rather than leaking the raw conflict. Mirrors
   PasswordReset.reset_password_atomically's start/commit/rollback shape. *)
let start_thread_from_chat (module C : Caqti_lwt.CONNECTION)
    ~title ~content ~section_id ~community_id ~user_id ~channel_id
    ~seed_message_id ~context_message_ids =
  (* Defensive normalization: seed never doubles as a context row; context deduped,
     original order preserved (n <= 10, so the O(n^2) membership check is fine). *)
  let context =
    List.filter (fun id -> id <> seed_message_id) context_message_ids
    |> List.fold_left (fun acc id -> if List.mem id acc then acc else acc @ [id]) []
  in
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () ->
      let rollback e = C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e)) in
      (C.find insert_promoted_post_query
         ((title, content, section_id), (community_id, user_id, channel_id)) >>= function
       | Error e -> rollback e
       | Ok post_id ->
           (C.exec insert_source_message_query ((post_id, seed_message_id), (0, true)) >>= function
            | Error e -> rollback e
            | Ok () ->
                let rec insert_context pos = function
                  | [] ->
                      (C.commit () >>= function
                       | Error e -> Lwt.return (Error (Caqti_error.show e))
                       | Ok () -> Lwt.return (Ok post_id))
                  | id :: rest ->
                      (C.exec insert_source_message_query ((post_id, id), (pos, false)) >>= function
                       | Error e -> rollback e
                       | Ok () -> insert_context (pos + 1) rest)
                in
                insert_context 1 context))
