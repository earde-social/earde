(** Database layer. Types are top-level for cross-module sharing. Inner modules
    (Community, User, Post, …) own their queries; the flat aliases below preserve
    call-site compatibility without a mass handler rewrite. *)

type user = { id : int; username : string; email : string; }

type post = {
  id : int; title : string; url : string option; content : string option;
  community_id : int; user_id : int; username : string; community_slug : string;
  created_at : string; score : int; comment_count : int; allow_downvotes : bool;
  image_url : string option;
  section_name : string option;
  section_slug : string option;
  community_sections_enabled : bool;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(** Community access control. [Community_private] => server-side read gate (members/mods/admins
    only); [Community_public] => readable by anyone. Distinct from the [indexable] SEO flag.
    Mirrors the DB CHECK on communities.visibility; [_of_string] is partial (None off-enum). *)
type community_visibility = Community_public | Community_private

val community_visibility_to_string : community_visibility -> string
val community_visibility_of_string : string -> community_visibility option

type community = {
  id : int; slug : string; name : string; description : string option;
  rules : string option; avatar_url : string option; banner_url : string option;
  allow_downvotes : bool; sections_enabled : bool;
  visibility : community_visibility; indexable : bool;
}

type community_section = {
  section_id : int; community_id : int; name : string; slug : string; description : string option;
  position : int; default_sort : string; is_introduction_section : bool; indexable : bool;
}

type channel = {
  id : int; community_id : int; slug : string; name : string; topic : string option;
  position : int; is_archived : bool; created_at : string; indexable : bool;
}

(** === EFFECTIVE ACCESS / INDEXABILITY RULES (pure; no DB) ===
    Privacy is the stronger property and is evaluated first: a private community is always
    effectively non-indexable, regardless of any [indexable] flag. The handler supplies the
    membership/mod/admin booleans to [can_read_community] from real checks — these perform none. *)
val community_is_private : community_visibility -> bool
val effective_indexable_community : community_visibility -> community_indexable:bool -> bool
val effective_indexable_child :
  community_visibility -> community_indexable:bool -> child_indexable:bool -> bool
val can_read_community :
  community_visibility -> is_member:bool -> is_mod:bool -> is_admin:bool -> bool

(* id is int64 (BIGSERIAL column). user_id is optional on read only — nullable for
   later GDPR tombstoning — but Chat.send_message requires a non-optional user_id.
   deleted_at-set rows are still returned by archive reads (durable knowledge). *)
type chat_message = {
  id : int64; channel_id : int; user_id : int option; content : string;
  created_at : string; edited_at : string option; deleted_at : string option;
}

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

type comment = {
  id : int; content : string; username : string; created_at : string;
  score : int; parent_id : int option; avatar_url : string option;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(* Single-query report target for a comment: the comment, its author, its post, and the
   owning community. Fields prefixed (crt_) to avoid record-field disambiguation. *)
type comment_report_target = {
  crt_comment_id : int;
  crt_content : string;
  crt_author_user_id : int;
  crt_author_username : string;
  crt_post_id : int;
  crt_post_title : string;
  crt_community_id : int;
  crt_community_slug : string;
}

type notification = {
  id : int; user_id : int; post_id : int option; notif_type : string;
  message : string; is_read : bool; created_at : string;
}

type mod_action = {
  id : int; community_id : int; moderator_id : int; moderator_username : string;
  action_type : string; target_id : int option; reason : string; created_at : string;
}

type moderator_entry = { user_id : int; username : string; role : string; }

type community_user_stat = {
  community_name : string;
  community_slug : string;
  local_karma : int;
  local_post_count : int;
  local_comment_count : int;
  first_active_at : string option;
}

(* Read-only rows for the /admin operational panels (recent users + pending signups). *)
type admin_recent_user = {
  id : int;
  username : string;
  email : string;
  created_at : string;
  is_admin : bool;
  is_banned : bool;
  post_count : int;
  comment_count : int;
  message_count : int;
}

type pending_signup_row = {
  id : int;
  username : string;
  email : string;
  created_at : string;
  expires_at : string;
  ip_address : string option;
}

(** Closed variant for post sort order — the type system prevents any raw string
    from reaching dynamic SQL, making Printf.sprintf injection impossible by construction. *)
type sort_mode = Newest | Top | Hot | Active

(** Reports / mod-queue value types. Closed variants keep raw strings out of the public
    API and dynamic SQL; the DB CHECK constraints mirror them exactly. The [*_of_string]
    helpers are partial (None off-enum). *)
type report_target = Report_post | Report_comment | Report_chat_message

type report_reason =
  | Report_spam
  | Report_abuse
  | Report_off_topic
  | Report_illegal
  | Report_other

type report_status = Report_open | Report_dismissed | Report_action_taken

type report_action_kind =
  | Report_removed_content
  | Report_banned_author
  | Report_other_action

val report_target_to_string : report_target -> string
val report_target_of_string : string -> report_target option
val report_reason_to_string : report_reason -> string
val report_reason_of_string : string -> report_reason option
val report_status_to_string : report_status -> string
val report_status_of_string : string -> report_status option
val report_action_kind_to_string : report_action_kind -> string
val report_action_kind_of_string : string -> report_action_kind option

(* Denormalized mod-queue row: reporter/author usernames are JOINed in (author optional). *)
type report_row = {
  id : int;
  community_id : int;
  reporter_user_id : int;
  reporter_username : string;
  target_type : report_target;
  target_id : int64;
  target_author_user_id : int option;
  target_author_username : string option;
  reason : report_reason;
  details : string option;
  status : report_status;
  action_kind : report_action_kind option;
  resolution_note : string option;
  resolved_by_user_id : int option;
  resolved_at : string option;
  created_at : string;
}

(** APM helper — wraps any DB call, emits {query, execution_ms, status} JSON via Logs.info. *)
val with_query_timer : name:string -> (unit -> ('a, string) result Lwt.t) -> ('a, string) result Lwt.t

module Community : sig
  val get_all_communities : (module Caqti_lwt.CONNECTION) -> (community list, string) result Lwt.t
  val create_community : (module Caqti_lwt.CONNECTION) -> string -> string -> string option -> bool -> (unit, string) result Lwt.t
  val get_community_by_slug : (module Caqti_lwt.CONNECTION) -> string -> (community option, string) result Lwt.t
  val get_community_by_id : (module Caqti_lwt.CONNECTION) -> int -> (community option, string) result Lwt.t
  val search_communities : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (community list, string) result Lwt.t
  (* UPDATE ... RETURNING: the authoritative updated record rides the same
     round-trip (analytics $groupidentify needs it); None = no such id, which
     was previously an indistinguishable silent no-op. *)
  val update_community_details : (module Caqti_lwt.CONNECTION) -> int -> string option -> string option -> string option -> string option -> (community option, string) result Lwt.t
  val toggle_community_downvotes : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
  (** Slice E: set community visibility / indexability from the settings UI. [visibility] is the
      closed variant (stringified internally); [indexable] is a bool. Touch only those columns.
      Visibility returns the authoritative updated record (UPDATE ... RETURNING) because it is a
      closed analytics group property; [None] = no such community id. *)
  val update_community_visibility : (module Caqti_lwt.CONNECTION) -> int -> community_visibility -> (community option, string) result Lwt.t
  val update_community_indexable : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
  val get_allows_downvotes_for_post : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
  val get_allows_downvotes_for_comment : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
end

module User : sig
  val create_user : (module Caqti_lwt.CONNECTION) -> string -> string -> string -> string -> (unit, string) result Lwt.t
  val user_exists : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
  (* Nested row: (id, username, email, created_at), (password_hash, is_admin,
     is_banned) — created_at rides along for the analytics person $set. *)
  val get_user_for_login : (module Caqti_lwt.CONNECTION) -> string -> (((int * string * string * string) * (string * bool * bool)) option, string) result Lwt.t
  val anonymize_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val get_user_public : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string option * string option) option, string) result Lwt.t
  (* Analytics person properties (username, email, created_at, is_admin) —
     the closed §4.3 set, used only by the consent-grant sync. *)
  val get_user_analytics_props : (module Caqti_lwt.CONNECTION) -> int -> ((string * string * string * bool) option, string) result Lwt.t
  val update_user_profile : (module Caqti_lwt.CONNECTION) -> string option -> string option -> int -> (unit, string) result Lwt.t
  val get_user_karma : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val get_user_post_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
  val get_user_comment_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
  val search_users : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string option * string option) list, string) result Lwt.t
  val get_user_by_username : (module Caqti_lwt.CONNECTION) -> string -> (user option, string) result Lwt.t
  val get_admin_usernames : (module Caqti_lwt.CONNECTION) -> (string list, string) result Lwt.t
  val is_user_admin : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
end

module Post : sig
  val create_post : (module Caqti_lwt.CONNECTION) -> string -> string option -> string option -> string option -> int option -> int -> int -> (int, string) result Lwt.t
  val get_all_posts : (module Caqti_lwt.CONNECTION) -> sort_mode -> int -> int -> (post list, string) result Lwt.t
  val get_personalized_feed : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
  val get_posts_by_community : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
  val get_posts_by_section : (module Caqti_lwt.CONNECTION) -> int -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
  val get_post_by_id : (module Caqti_lwt.CONNECTION) -> int -> (post option, string) result Lwt.t
  val get_posts_by_user : (module Caqti_lwt.CONNECTION) -> int -> (post list, string) result Lwt.t
  (* Slice C/D/G profile leak-filter: (post_id, community_id, raw visibility, indexable,
     section_non_indexable) for a set of post ids, in one bounded query. Lets a profile drop
     posts/comments in private communities the viewer can't read (Slice C), public-but-non-indexable
     communities (Slice D), and non-indexable forum sections (Slice G) from public discovery.
     The 5th flag is TRUE only when the post sits in a section flagged indexable=false. Empty -> []. *)
  val get_post_communities : (module Caqti_lwt.CONNECTION) -> int list -> ((int * int * string * bool * bool) list, string) result Lwt.t
  val vote_post : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
  val remove_post_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val soft_delete_post : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val search_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
end

module Section : sig
  val create_section : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> int -> string -> bool -> (unit, string) result Lwt.t
  val get_sections_by_community : (module Caqti_lwt.CONNECTION) -> int -> (community_section list, string) result Lwt.t
  val get_section_by_id : (module Caqti_lwt.CONNECTION) -> int -> int -> (community_section option, string) result Lwt.t
  val get_section_by_slug : (module Caqti_lwt.CONNECTION) -> string -> int -> (community_section option, string) result Lwt.t
  val update_section : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> string -> (unit, string) result Lwt.t
  (* update_section_indexable section_id community_id indexable — Slice H toggle; community-scoped. *)
  val update_section_indexable : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
  val delete_section : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val get_sections_with_stats : (module Caqti_lwt.CONNECTION) -> int -> ((community_section * int * string option) list, string) result Lwt.t
  val get_orphaned_count_and_activity : (module Caqti_lwt.CONNECTION) -> int -> (int * string option, string) result Lwt.t
  val get_orphaned_posts : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
end

module Channel : sig
  (* create_channel returns the generated slug — it is auto-derived from name and
     the channel view/management handlers redirect by slug. *)
  val create_channel : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> int -> (string, string) result Lwt.t
  val get_channels_by_community : (module Caqti_lwt.CONNECTION) -> int -> (channel list, string) result Lwt.t
  val get_channel_by_slug : (module Caqti_lwt.CONNECTION) -> string -> int -> (channel option, string) result Lwt.t
  val get_channel_by_id : (module Caqti_lwt.CONNECTION) -> int -> int -> (channel option, string) result Lwt.t
  val set_channel_archived : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
  (* update_channel channel_id community_id name topic — edits display name + topic only;
     slug is intentionally left stable so existing /ch/:slug links keep resolving. *)
  val update_channel : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> string option -> (unit, string) result Lwt.t
  (* update_channel_indexable channel_id community_id indexable — Slice H toggle; community-scoped. *)
  val update_channel_indexable : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
end

module Chat : sig
  (* send_message channel_id user_id content -> the canonical persisted message
     (INSERT ... RETURNING: Postgres-assigned id and created_at included), so callers
     never need a post-insert read-back. user_id is a plain int: a new message is
     always attributed to its author. *)
  val send_message : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> (chat_message, string) result Lwt.t
  val get_message_by_id : (module Caqti_lwt.CONNECTION) -> int64 -> (chat_message option, string) result Lwt.t
  (* get_recent_messages channel_id limit -> newest `limit` messages, ascending by id. *)
  val get_recent_messages : (module Caqti_lwt.CONNECTION) -> int -> int -> (chat_message list, string) result Lwt.t
  (* get_recent_messages_with_authors channel_id limit -> same as above but LEFT JOINs users
     so each message carries its author username (None when user_id is NULL). For SSR render. *)
  val get_recent_messages_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int -> ((chat_message * string option) list, string) result Lwt.t
  (* get_messages_before_id channel_id before_id limit -> archive page older than the cursor, ascending. *)
  val get_messages_before_id : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> (chat_message list, string) result Lwt.t
  (* get_messages_after_id channel_id after_id limit -> realtime resume/replay, ascending. *)
  val get_messages_after_id : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> (chat_message list, string) result Lwt.t
  (* edit_message message_id user_id content — author-scoped via user_id. *)
  val edit_message : (module Caqti_lwt.CONNECTION) -> int64 -> int -> string -> (unit, string) result Lwt.t
  (* soft_delete_message message_id — sets deleted_at; authorization is the handler's job. *)
  val soft_delete_message : (module Caqti_lwt.CONNECTION) -> int64 -> (unit, string) result Lwt.t
  (* Author-joined keyset reads for the start-thread form's nearby-message context. *)
  val get_messages_before_id_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> ((chat_message * string option) list, string) result Lwt.t
  val get_messages_after_id_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> ((chat_message * string option) list, string) result Lwt.t
end

(* "Start thread from chat" provenance: links a durable thread to its source chat
   messages. See db.ml for the seed-uniqueness invariant. *)
module ThreadSource : sig
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
end

module Comment : sig
  val get_comments : (module Caqti_lwt.CONNECTION) -> int -> (comment list, string) result Lwt.t
  val create_comment : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> int option -> (int, string) result Lwt.t
  val touch_last_activity : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val vote_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
  val get_comments_by_user : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * string * int * string * int) list, string) result Lwt.t
  val remove_comment_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val soft_delete_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val search_comments : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string * int * int) list, string) result Lwt.t
  (* Resolve a comment + its author/post/community in one query for the report flow.
     [None] when the comment, its post, or its community no longer exists. *)
  val get_comment_report_target : (module Caqti_lwt.CONNECTION) -> int -> (comment_report_target option, string) result Lwt.t
end

module Membership : sig
  val join_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_member : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  (* true = a membership row was actually deleted; false = the user was not a
     member (no-op). Same DELETE round-trip via RETURNING. *)
  val leave_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  (** Slice F: list the [community_members] allow-list (username/id/email) for the settings
      member-management UI. Ordered by username. Mods/admins absent unless also members. *)
  val get_community_members : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
  val get_user_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t
end

module Moderator : sig
  val add_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val add_top_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  val get_moderator_role : (module Caqti_lwt.CONNECTION) -> int -> int -> (string option, string) result Lwt.t
  val get_community_moderators : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
  val get_community_mods_with_roles : (module Caqti_lwt.CONNECTION) -> int -> (moderator_entry list, string) result Lwt.t
  val remove_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val get_moderated_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t
  val promote_to_top_mod : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val demote_inactive_mods : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
end

module Mod_action : sig
  val log_action : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> int option -> string -> (unit, string) result Lwt.t
  val get_modlog : (module Caqti_lwt.CONNECTION) -> int -> (mod_action list, string) result Lwt.t
end

module Report : sig
  (* [`Created id] on insert; [`Duplicate] when an OPEN report by the same reporter for the
     same target already exists (ON CONFLICT DO NOTHING on the partial-unique index). *)
  val create_report :
    (module Caqti_lwt.CONNECTION) ->
    community_id:int -> reporter_user_id:int -> target_type:report_target ->
    target_id:int64 -> target_author_user_id:int option ->
    reason:report_reason -> details:string option ->
    ([ `Created of int | `Duplicate ], string) result Lwt.t
  val get_reports_by_community :
    (module Caqti_lwt.CONNECTION) -> int -> status:report_status -> (report_row list, string) result Lwt.t
  val get_report_by_id :
    (module Caqti_lwt.CONNECTION) -> int -> (report_row option, string) result Lwt.t
  val resolve_report :
    (module Caqti_lwt.CONNECTION) -> int -> resolver_user_id:int -> status:report_status ->
    action_kind:report_action_kind option -> note:string option -> (unit, string) result Lwt.t
  val count_open_reports : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
end

module Ban : sig
  val ban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val unban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val is_banned : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
  val get_banned_users : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
end

module Notification : sig
  val get_notifications : (module Caqti_lwt.CONNECTION) -> int -> (notification list, string) result Lwt.t
  val count_unread_notifs : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val mark_notifs_read : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val create_notif : (module Caqti_lwt.CONNECTION) -> int -> int option -> string -> string -> (unit, string) result Lwt.t
  val get_post_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val get_comment_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
  val get_comment_post_id : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
end

module Analytics : sig
  val log_page_view : (module Caqti_lwt.CONNECTION) -> string -> string option -> string -> (unit, string) result Lwt.t
  val get_kpi_dashboard : (module Caqti_lwt.CONNECTION) -> start_date:string -> end_date:string -> (((int * int * int) * (int * int)), string) result Lwt.t
  val get_dau_mau_ratio : (module Caqti_lwt.CONNECTION) -> start_date:string -> end_date:string -> (float, string) result Lwt.t
end

(* Presence is operational state, not analytics: last_active_at feeds
   Moderator.demote_inactive_mods and must survive analytics changes. *)
module Presence : sig
  val touch_user_active : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
end

module Security : sig
  val update_password : (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
  val verify_email : (module Caqti_lwt.CONNECTION) -> string -> (string option, string) result Lwt.t
end

module Rate_limit : sig
  val check : (module Caqti_lwt.CONNECTION) -> string -> string -> ([`Allowed | `Blocked], string) result Lwt.t
end

module Admin : sig
  val admin_delete_post : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
  val admin_delete_comment : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
  (* Community-scoped moderator tombstones: the mutation is bound to the route
     community and reports whether a row actually matched, so a zero-row update
     can never be mistaken for a deletion.
     mod_delete_post: Ok true = deleted; Ok false = no such post in that community.
     mod_delete_comment: Ok (Some post_id) = deleted (post_id of the parent post);
     Ok None = no such comment under that community's posts. *)
  val mod_delete_post : (module Caqti_lwt.CONNECTION) -> community_id:int -> int -> (bool, string) result Lwt.t
  val mod_delete_comment : (module Caqti_lwt.CONNECTION) -> community_id:int -> int -> (int option, string) result Lwt.t
  val ban_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val is_globally_banned : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
  val unban_user_global : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
  val get_globally_banned_users : (module Caqti_lwt.CONNECTION) -> (user list, string) result Lwt.t
  (* Read-only admin-dashboard reads, each bounded by [limit]. list_recent_users carries
     per-user post/comment/chat-message counts via one bounded aggregate query. *)
  val list_recent_users : (module Caqti_lwt.CONNECTION) -> limit:int -> (admin_recent_user list, string) result Lwt.t
  val list_recent_pending : (module Caqti_lwt.CONNECTION) -> limit:int -> (pending_signup_row list, string) result Lwt.t
end

module PasswordReset : sig
  val create_token : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
  val validate_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
  (* Ok true = updated; Ok false = token expired; Error = DB error *)
  val reset_password_atomically : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
end

module PendingSignup : sig
  val hash_token : string -> string
  (* True only if a NON-expired, unconsumed pending under a DIFFERENT email holds this username. *)
  val username_pending_elsewhere : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
  (* Clears any colliding (own-email or expired-username) pending, then inserts a fresh
     24h pending. Argon2-hash the password before calling. *)
  val upsert :
    (module Caqti_lwt.CONNECTION) ->
    username:string -> email:string -> password_hash:string -> token_hash:string ->
    ip:string option -> user_agent:string option -> (unit, string) result Lwt.t
  val sweep_expired : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
  (* `Confirmed (user_id, username, email, created_at, is_admin) = user row
     created — id/created_at/is_admin from the insert's RETURNING, so the
     caller holds the closed analytics person properties (§4.3) without a
     post-transaction lookup; `Invalid = token missing/expired/already used;
     `Conflict = username/email taken in users since signup. *)
  val confirm :
    (module Caqti_lwt.CONNECTION) -> string ->
    ([ `Confirmed of int * string * string * string * bool
     | `Invalid | `Conflict ], string) result Lwt.t
end

(* Durable PostHog person-deletion jobs (analytics spec §3.3). Job state only —
   no function here performs network IO; the Persons-API client lives in
   Posthog_deletion. *)
module PosthogDeletionJobs : sig
  val default_lease_minutes : int

  (** One transaction: lock the user row, apply exactly the [anonymize_user]
      rewrite, insert-or-adopt the pending deletion job for the immutable
      ["user:<id>"], commit both or roll back both. Returns
      [(job_id, distinct_id)]. Duplicate/concurrent calls converge on the same
      job. Never performs HTTP. *)
  val anonymize_and_enqueue :
    (module Caqti_lwt.CONNECTION) -> int -> (int * string, string) result Lwt.t

  (** Atomically claim one pending job (attempts+1, lease timestamp set in the
      same statement). [None] = completed, or lease-held by another attempt. *)
  val claim :
    (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> int ->
    (string option, string) result Lwt.t

  (** Claim up to [limit] oldest eligible pending jobs (SKIP LOCKED — safe
      under concurrent invocations), returned oldest-first as
      [(job_id, distinct_id)]. *)
  val claim_batch :
    (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> limit:int -> unit ->
    ((int * string) list, string) result Lwt.t

  val mark_completed :
    (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

  (** Store a bounded safe diagnostic class on a still-pending job. Never pass
      response bodies, URLs, tokens, or personal data. *)
  val mark_failed :
    (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t

  (** [(id, status, attempts, last_error)] for tests/inspection. *)
  val get_by_distinct_id :
    (module Caqti_lwt.CONNECTION) -> string ->
    ((int * string * int * string option) option, string) result Lwt.t
end

(** Durable §13 group-profile scrub jobs (privacy cleanup on public->private
    transitions). Same architecture as [PosthogDeletionJobs]: enqueue happens
    inside the authoritative transaction, claims are lease-based and bounded,
    and no function here performs network IO. [group_key] is always the
    immutable ["community:<id>"] — never a name or slug. *)
module PosthogGroupCleanupJobs : sig
  val default_lease_minutes : int

  (** One transaction: apply the visibility UPDATE and, when the new value is
      [Community_private], insert-or-re-arm the pending cleanup job for the
      community's group key; commit both or roll back both. Returns the
      updated community ([None] = id matched nothing) and the job id ([None]
      on ->public transitions). Never performs HTTP. *)
  val update_visibility_and_enqueue :
    (module Caqti_lwt.CONNECTION) -> int -> community_visibility ->
    (community option * int option, string) result Lwt.t

  (** Atomically claim one pending job (attempts+1, lease timestamp set in the
      same statement). [None] = completed, or lease-held by another attempt. *)
  val claim :
    (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> int ->
    (string option, string) result Lwt.t

  (** Claim up to [limit] oldest eligible pending jobs (SKIP LOCKED), returned
      oldest-first as [(job_id, group_key)]. *)
  val claim_batch :
    (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> limit:int -> unit ->
    ((int * string) list, string) result Lwt.t

  val mark_completed :
    (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

  (** Store a bounded safe diagnostic class on a still-pending job. Never pass
      response bodies, URLs, tokens, names, or slugs. *)
  val mark_failed :
    (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t

  (** [(id, status, attempts, last_error)] for tests/inspection. *)
  val get_by_group_key :
    (module Caqti_lwt.CONNECTION) -> string ->
    ((int * string * int * string option) option, string) result Lwt.t
end

module Community_user_stats : sig
  val ensure_community_user_stats : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val increment_local_post_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val increment_local_comment_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
  val update_local_karma : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
  val get_user_community_stats : (module Caqti_lwt.CONNECTION) -> int -> (community_user_stat list, string) result Lwt.t
end

val create_user : (module Caqti_lwt.CONNECTION) -> string -> string -> string -> string -> (unit, string) result Lwt.t
val user_exists : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
val get_user_for_login : (module Caqti_lwt.CONNECTION) -> string -> (((int * string * string * string) * (string * bool * bool)) option, string) result Lwt.t
val anonymize_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_user_public : (module Caqti_lwt.CONNECTION) -> string -> ((int * string * string * string option * string option) option, string) result Lwt.t
val get_user_analytics_props : (module Caqti_lwt.CONNECTION) -> int -> ((string * string * string * bool) option, string) result Lwt.t
val update_user_profile : (module Caqti_lwt.CONNECTION) -> string option -> string option -> int -> (unit, string) result Lwt.t
val get_user_karma : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val verify_email : (module Caqti_lwt.CONNECTION) -> string -> (string option, string) result Lwt.t
val update_password : (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
val get_user_by_username : (module Caqti_lwt.CONNECTION) -> string -> (user option, string) result Lwt.t
val get_admin_usernames : (module Caqti_lwt.CONNECTION) -> (string list, string) result Lwt.t
val is_user_admin : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t

val get_all_communities : (module Caqti_lwt.CONNECTION) -> (community list, string) result Lwt.t
val create_community : (module Caqti_lwt.CONNECTION) -> string -> string -> string option -> bool -> (unit, string) result Lwt.t
val get_community_by_slug : (module Caqti_lwt.CONNECTION) -> string -> (community option, string) result Lwt.t
val get_community_by_id : (module Caqti_lwt.CONNECTION) -> int -> (community option, string) result Lwt.t
val update_community_details : (module Caqti_lwt.CONNECTION) -> int -> string option -> string option -> string option -> string option -> (community option, string) result Lwt.t
val join_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val leave_community : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val is_member : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_community_members : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
val get_user_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t

val add_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val add_top_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val is_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val get_community_moderators : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t
val remove_moderator : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_moderated_communities : (module Caqti_lwt.CONNECTION) -> int -> (community list, string) result Lwt.t

val community_ban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val community_unban_user : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val community_is_banned : (module Caqti_lwt.CONNECTION) -> int -> int -> (bool, string) result Lwt.t
val community_get_banned_users : (module Caqti_lwt.CONNECTION) -> int -> (user list, string) result Lwt.t

val create_post : (module Caqti_lwt.CONNECTION) -> string -> string option -> string option -> string option -> int option -> int -> int -> (int, string) result Lwt.t
val get_all_posts : (module Caqti_lwt.CONNECTION) -> sort_mode -> int -> int -> (post list, string) result Lwt.t
val get_personalized_feed : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
val get_posts_by_community : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
val get_posts_by_section : (module Caqti_lwt.CONNECTION) -> int -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t
val get_post_by_id : (module Caqti_lwt.CONNECTION) -> int -> (post option, string) result Lwt.t
val get_posts_by_user : (module Caqti_lwt.CONNECTION) -> int -> (post list, string) result Lwt.t
val get_post_communities : (module Caqti_lwt.CONNECTION) -> int list -> ((int * int * string * bool * bool) list, string) result Lwt.t
val soft_delete_post : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val create_comment : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> int option -> (int, string) result Lwt.t
val touch_post_last_activity : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_comments : (module Caqti_lwt.CONNECTION) -> int -> (comment list, string) result Lwt.t
val get_comments_by_user : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * string * int * string * int) list, string) result Lwt.t
val soft_delete_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_comment_report_target : (module Caqti_lwt.CONNECTION) -> int -> (comment_report_target option, string) result Lwt.t

val create_section : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> int -> string -> bool -> (unit, string) result Lwt.t
val get_sections_by_community : (module Caqti_lwt.CONNECTION) -> int -> (community_section list, string) result Lwt.t
val get_section_by_id : (module Caqti_lwt.CONNECTION) -> int -> int -> (community_section option, string) result Lwt.t
val get_section_by_slug : (module Caqti_lwt.CONNECTION) -> string -> int -> (community_section option, string) result Lwt.t
val update_section : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> string -> (unit, string) result Lwt.t
val update_section_indexable : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
val delete_section : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_sections_with_stats : (module Caqti_lwt.CONNECTION) -> int -> ((community_section * int * string option) list, string) result Lwt.t
val get_orphaned_count_and_activity : (module Caqti_lwt.CONNECTION) -> int -> (int * string option, string) result Lwt.t
val get_orphaned_posts : (module Caqti_lwt.CONNECTION) -> int -> sort_mode -> int -> int -> (post list, string) result Lwt.t

val create_channel : (module Caqti_lwt.CONNECTION) -> int -> string -> string option -> int -> (string, string) result Lwt.t
val get_channels_by_community : (module Caqti_lwt.CONNECTION) -> int -> (channel list, string) result Lwt.t
val get_channel_by_slug : (module Caqti_lwt.CONNECTION) -> string -> int -> (channel option, string) result Lwt.t
val get_channel_by_id : (module Caqti_lwt.CONNECTION) -> int -> int -> (channel option, string) result Lwt.t
val set_channel_archived : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t
val update_channel : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> string option -> (unit, string) result Lwt.t
val update_channel_indexable : (module Caqti_lwt.CONNECTION) -> int -> int -> bool -> (unit, string) result Lwt.t

val send_message : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> (chat_message, string) result Lwt.t
val get_message_by_id : (module Caqti_lwt.CONNECTION) -> int64 -> (chat_message option, string) result Lwt.t
val get_recent_messages : (module Caqti_lwt.CONNECTION) -> int -> int -> (chat_message list, string) result Lwt.t
val get_recent_messages_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int -> ((chat_message * string option) list, string) result Lwt.t
val get_messages_before_id : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> (chat_message list, string) result Lwt.t
val get_messages_after_id : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> (chat_message list, string) result Lwt.t
val edit_message : (module Caqti_lwt.CONNECTION) -> int64 -> int -> string -> (unit, string) result Lwt.t
val soft_delete_message : (module Caqti_lwt.CONNECTION) -> int64 -> (unit, string) result Lwt.t
val get_messages_before_id_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> ((chat_message * string option) list, string) result Lwt.t
val get_messages_after_id_with_authors : (module Caqti_lwt.CONNECTION) -> int -> int64 -> int -> ((chat_message * string option) list, string) result Lwt.t

val get_seed_thread_for_message : (module Caqti_lwt.CONNECTION) -> int64 -> ((int * string * string) option, string) result Lwt.t
val get_thread_links_for_channel : (module Caqti_lwt.CONNECTION) -> int -> ((int64 * int * string * bool) list, string) result Lwt.t
val get_thread_source : (module Caqti_lwt.CONNECTION) -> int -> ((string * string * int) option * thread_source_msg list, string) result Lwt.t
val get_thread_sources_for_posts : (module Caqti_lwt.CONNECTION) -> int list -> ((int * string * string * int) list, string) result Lwt.t
val get_source_span_for_thread : (module Caqti_lwt.CONNECTION) -> int -> ((int * string * int64 list) option, string) result Lwt.t
val start_thread_from_chat :
  (module Caqti_lwt.CONNECTION) ->
  title:string -> content:string option -> section_id:int option ->
  community_id:int -> user_id:int -> channel_id:int ->
  seed_message_id:int64 -> context_message_ids:int64 list ->
  (int, string) result Lwt.t

val vote_post : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
val remove_post_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_user_post_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t
val vote_comment : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
val remove_comment_vote : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_user_comment_votes : (module Caqti_lwt.CONNECTION) -> int -> ((int * int) list, string) result Lwt.t

val get_notifications : (module Caqti_lwt.CONNECTION) -> int -> (notification list, string) result Lwt.t
val count_unread_notifs : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val mark_notifs_read : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val create_notif : (module Caqti_lwt.CONNECTION) -> int -> int option -> string -> string -> (unit, string) result Lwt.t
val get_post_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val get_comment_owner : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val get_comment_post_id : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val log_page_view : (module Caqti_lwt.CONNECTION) -> string -> string option -> string -> (unit, string) result Lwt.t
val get_kpi_dashboard : (module Caqti_lwt.CONNECTION) -> start_date:string -> end_date:string -> (((int * int * int) * (int * int)), string) result Lwt.t
val get_dau_mau_ratio : (module Caqti_lwt.CONNECTION) -> start_date:string -> end_date:string -> (float, string) result Lwt.t
val touch_user_active : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t

val search_communities : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (community list, string) result Lwt.t
val search_users : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string option * string option) list, string) result Lwt.t
val search_posts : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> (post list, string) result Lwt.t
val search_comments : (module Caqti_lwt.CONNECTION) -> string -> int -> int -> ((int * string * string * string * int * int) list, string) result Lwt.t

val admin_delete_post : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
val admin_delete_comment : (module Caqti_lwt.CONNECTION) -> label:string -> int -> (unit, string) result Lwt.t
(* See module Admin for the result contract. There is deliberately no ID-only
   mod deletion: community moderation must always prove route-to-target scope. *)
val mod_delete_post : (module Caqti_lwt.CONNECTION) -> community_id:int -> int -> (bool, string) result Lwt.t
val mod_delete_comment : (module Caqti_lwt.CONNECTION) -> community_id:int -> int -> (int option, string) result Lwt.t
val ban_user : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val is_globally_banned : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
val unban_user_global : (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val get_globally_banned_users : (module Caqti_lwt.CONNECTION) -> (user list, string) result Lwt.t

val promote_to_top_mod : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val get_moderator_role : (module Caqti_lwt.CONNECTION) -> int -> int -> (string option, string) result Lwt.t
val get_community_mods_with_roles : (module Caqti_lwt.CONNECTION) -> int -> (moderator_entry list, string) result Lwt.t
val demote_inactive_mods : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
val log_mod_action : (module Caqti_lwt.CONNECTION) -> int -> int -> string -> int option -> string -> (unit, string) result Lwt.t
val get_modlog : (module Caqti_lwt.CONNECTION) -> int -> (mod_action list, string) result Lwt.t

val create_report :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int -> reporter_user_id:int -> target_type:report_target ->
  target_id:int64 -> target_author_user_id:int option ->
  reason:report_reason -> details:string option ->
  ([ `Created of int | `Duplicate ], string) result Lwt.t
val get_reports_by_community :
  (module Caqti_lwt.CONNECTION) -> int -> status:report_status -> (report_row list, string) result Lwt.t
val get_report_by_id :
  (module Caqti_lwt.CONNECTION) -> int -> (report_row option, string) result Lwt.t
val resolve_report :
  (module Caqti_lwt.CONNECTION) -> int -> resolver_user_id:int -> status:report_status ->
  action_kind:report_action_kind option -> note:string option -> (unit, string) result Lwt.t
val count_open_reports : (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
val toggle_community_downvotes : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
val update_community_visibility : (module Caqti_lwt.CONNECTION) -> int -> community_visibility -> (community option, string) result Lwt.t
val update_community_indexable : (module Caqti_lwt.CONNECTION) -> int -> bool -> (unit, string) result Lwt.t
val get_allows_downvotes_for_post : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t
val get_allows_downvotes_for_comment : (module Caqti_lwt.CONNECTION) -> int -> (bool, string) result Lwt.t

val password_reset_create_token : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
val password_reset_validate_token : (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t
(* Ok true = password updated; Ok false = token expired/invalid; Error = DB error *)
val password_reset_atomically : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t

val pending_signup_hash_token : string -> string
val pending_signup_username_elsewhere : (module Caqti_lwt.CONNECTION) -> string -> string -> (bool, string) result Lwt.t
val pending_signup_upsert :
  (module Caqti_lwt.CONNECTION) ->
  username:string -> email:string -> password_hash:string -> token_hash:string ->
  ip:string option -> user_agent:string option -> (unit, string) result Lwt.t
val pending_signup_sweep_expired : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
val pending_signup_confirm :
  (module Caqti_lwt.CONNECTION) -> string ->
  ([ `Confirmed of int * string * string * string * bool
     (* new user id, username, email, created_at, is_admin *)
   | `Invalid | `Conflict ], string) result Lwt.t

val anonymize_user_and_enqueue_posthog_deletion :
  (module Caqti_lwt.CONNECTION) -> int -> (int * string, string) result Lwt.t
val claim_posthog_deletion_job :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> int ->
  (string option, string) result Lwt.t
val claim_posthog_deletion_batch :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> limit:int -> unit ->
  ((int * string) list, string) result Lwt.t
val complete_posthog_deletion_job :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val fail_posthog_deletion_job :
  (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
val get_posthog_deletion_job :
  (module Caqti_lwt.CONNECTION) -> string ->
  ((int * string * int * string option) option, string) result Lwt.t

val update_community_visibility_and_enqueue_group_cleanup :
  (module Caqti_lwt.CONNECTION) -> int -> community_visibility ->
  (community option * int option, string) result Lwt.t
val claim_posthog_group_cleanup_job :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> int ->
  (string option, string) result Lwt.t
val claim_posthog_group_cleanup_batch :
  (module Caqti_lwt.CONNECTION) -> ?lease_minutes:int -> limit:int -> unit ->
  ((int * string) list, string) result Lwt.t
val complete_posthog_group_cleanup_job :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
val fail_posthog_group_cleanup_job :
  (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
val get_posthog_group_cleanup_job :
  (module Caqti_lwt.CONNECTION) -> string ->
  ((int * string * int * string option) option, string) result Lwt.t

val ensure_community_user_stats : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val increment_local_post_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val increment_local_comment_count : (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t
val update_local_karma : (module Caqti_lwt.CONNECTION) -> int -> int -> int -> (unit, string) result Lwt.t
val get_user_community_stats : (module Caqti_lwt.CONNECTION) -> int -> (community_user_stat list, string) result Lwt.t
