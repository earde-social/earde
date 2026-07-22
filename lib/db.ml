open Lwt.Infix

(* The PostHog analytics module (lib/analytics.ml), aliased before this file's
   own inner homemade [Analytics] (page-view) module shadows the name — same
   idiom as [Components.Posthog]. Used only for the central "user:<id>"
   distinct-id derivation (§3.3). *)
module Posthog = Analytics

(* Community access control. A closed variant keeps raw strings out of the public API and dynamic
   SQL; the DB CHECK constraint (communities_visibility_check) mirrors it exactly. *_of_string is
   partial (None off-enum) so a stray value fails at the boundary instead of corrupting a row —
   see test_earde.ml for the round-trip + rejection coverage. This is ACCESS control, distinct
   from the [indexable] SEO flag below. *)
type community_visibility = Community_public | Community_private

let community_visibility_to_string = function
  | Community_public -> "public"
  | Community_private -> "private"

let community_visibility_of_string = function
  | "public" -> Some Community_public
  | "private" -> Some Community_private
  | _ -> None

(* === Effective access/indexability rules (PURE — no DB, no IO) ===
   Privacy is the strictly stronger property and is evaluated first: a private community is
   always effectively non-indexable, regardless of any [indexable] flag. These encode §1A /
   §2.8 of docs/private-access-indexability-audit.md and are unit-tested in test_earde.ml. *)

let community_is_private = function
  | Community_private -> true
  | Community_public -> false

(* Whether a community's own surfaces (home/feed/search exposure) may be indexed. *)
let effective_indexable_community visibility ~community_indexable =
  match visibility with
  | Community_private -> false
  | Community_public -> community_indexable

(* Whether a child surface (a channel archive or a forum section + its threads) may be indexed.
   Private kills it; otherwise it needs BOTH the community and the child to opt in. *)
let effective_indexable_child visibility ~community_indexable ~child_indexable =
  match visibility with
  | Community_private -> false
  | Community_public -> community_indexable && child_indexable

(* Pure read predicate: may this viewer READ the community at all? Public is readable by anyone
   (incl. logged-out / non-member); private is readable only by a member, moderator, or admin.
   The handler supplies the booleans from real DB/session checks — this helper performs none. *)
let can_read_community visibility ~is_member ~is_mod ~is_admin =
  match visibility with
  | Community_public -> true
  | Community_private -> is_member || is_mod || is_admin

type community = {
  id : int;
  slug : string;
  name : string;
  description : string option;
  rules : string option;
  avatar_url : string option;
  banner_url : string option;
  allow_downvotes : bool;
  sections_enabled : bool;
  visibility : community_visibility;
  indexable : bool;
}

type community_section = {
  section_id : int;
  community_id : int;
  name : string;
  slug : string;
  description : string option;
  position : int;
  default_sort : string;
  is_introduction_section : bool;
  indexable : bool;
}

type channel = {
  id : int;
  community_id : int;
  slug : string;
  name : string;
  topic : string option;
  position : int;
  is_archived : bool;
  created_at : string;
  indexable : bool;
}

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

type user = {
  id : int;
  username : string;
  email : string;
}

type post = {
  id : int; title : string; url : string option; content : string option;
  community_id : int; user_id : int; username : string; community_slug : string;
  created_at : string;
  score : int;
  comment_count : int;
  allow_downvotes : bool;
  image_url : string option;
  section_name : string option;
  section_slug : string option;
  community_sections_enabled : bool;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

type comment = {
  id : int;
  content : string;
  username : string;
  created_at : string;
  score : int;
  parent_id : int option;
  avatar_url : string option;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(* One-query target resolution for the report flow: a single comment with its author,
   its post, and the owning community. Joins comments→users→posts→communities so the
   report form/handler can validate community ownership, gate self-reports, and render
   context without the comment→post→community multi-hop the audit flagged as missing.
   Fields are prefixed (crt_) to avoid record-field disambiguation against post/comment. *)
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
  id : int;
  user_id : int;
  post_id : int option;
  notif_type : string;
  message : string;
  is_read : bool;
  created_at : string;
}

type mod_action = {
  id : int;
  community_id : int;
  moderator_id : int;
  moderator_username : string;
  action_type : string;
  target_id : int option;
  reason : string;
  created_at : string;
}

type moderator_entry = {
  user_id : int;
  username : string;
  role : string;
}

type community_user_stat = {
  community_name : string;
  community_slug : string;
  local_karma : int;
  local_post_count : int;
  local_comment_count : int;
  first_active_at : string option;
}

(* Read-only rows for the admin dashboard's operational panels. Kept distinct from
   [user] so the dashboard reads can carry created_at/flags/counts without bloating the
   record threaded through every handler. *)
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

(* Closed variant for post sort order. The type system enforces that no raw string
   can reach Printf.sprintf — injection is impossible by construction, not by convention. *)
type sort_mode = Newest | Top | Hot | Active

(* === REPORTS / MOD-QUEUE VALUE TYPES ===
   Closed variants so no raw, unchecked string ever reaches dynamic SQL or the public
   API; the *_to_string forms are the ONLY values written to the reports table, and the
   DB CHECK constraints mirror these enums exactly. The *_of_string helpers are partial
   (return None off-enum) so a stray value fails at the boundary instead of corrupting a
   row — see test_earde.ml for the round-trip + rejection coverage. *)
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

let report_target_to_string = function
  | Report_post -> "post"
  | Report_comment -> "comment"
  | Report_chat_message -> "chat_message"

let report_target_of_string = function
  | "post" -> Some Report_post
  | "comment" -> Some Report_comment
  | "chat_message" -> Some Report_chat_message
  | _ -> None

let report_reason_to_string = function
  | Report_spam -> "spam"
  | Report_abuse -> "abuse"
  | Report_off_topic -> "off_topic"
  | Report_illegal -> "illegal"
  | Report_other -> "other"

let report_reason_of_string = function
  | "spam" -> Some Report_spam
  | "abuse" -> Some Report_abuse
  | "off_topic" -> Some Report_off_topic
  | "illegal" -> Some Report_illegal
  | "other" -> Some Report_other
  | _ -> None

let report_status_to_string = function
  | Report_open -> "open"
  | Report_dismissed -> "dismissed"
  | Report_action_taken -> "action_taken"

let report_status_of_string = function
  | "open" -> Some Report_open
  | "dismissed" -> Some Report_dismissed
  | "action_taken" -> Some Report_action_taken
  | _ -> None

(* Report_other_action maps to the bare "other" stored value (distinct constructor name
   only to avoid clashing with report_reason's Report_other). *)
let report_action_kind_to_string = function
  | Report_removed_content -> "removed_content"
  | Report_banned_author -> "banned_author"
  | Report_other_action -> "other"

let report_action_kind_of_string = function
  | "removed_content" -> Some Report_removed_content
  | "banned_author" -> Some Report_banned_author
  | "other" -> Some Report_other_action
  | _ -> None

(* Denormalized queue row: reporter/author usernames are JOINed in (author LEFT JOIN, so
   None for a tombstoned/missing author). Enum columns decode through the *_of_string
   helpers; the DB CHECK constraints make the decode total in practice. *)
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

(* post has 20 SELECT columns; local karma stats appended as the 5th t4 group.
   Caqti tN stops at t7 so nesting is mandatory. *)
let post_row_type =
  let open Caqti_type in
  t5
    (t4 int string (option string) (option string))
    (t4 int int string string)
    (t4 string int int bool)
    (t4 (option string) (option string) (option string) bool)
    (t4 int int int (option string))

let map_post_row ((id, title, url, content), (community_id, user_id, username, community_slug), (created_at, score, comment_count, allow_downvotes), (image_url, section_name, section_slug, community_sections_enabled), (author_local_karma, author_local_post_count, author_local_comment_count, author_first_active_at)) =
  { id; title; url; content; community_id; user_id; username; community_slug; created_at; score; comment_count; allow_downvotes; image_url; section_name; section_slug; community_sections_enabled; author_local_karma; author_local_post_count; author_local_comment_count; author_first_active_at }

(* 11-column community row: t3(t4, t4, t3) stays within Caqti's per-tuple arity limit.
   visibility arrives as the raw TEXT value and decodes through community_visibility_of_string. *)
let community_row_type =
  let open Caqti_type in
  t3 (t4 int string string (option string)) (t4 (option string) (option string) (option string) bool) (t3 bool string bool)

let map_community_row ((id, slug, name, description), (rules, avatar_url, banner_url, allow_downvotes), (sections_enabled, visibility_s, indexable)) =
  (* Fail closed: an unrecognized visibility decodes to Community_private, never public —
     this is a privacy field, so a corrupt/unexpected value must err toward hiding, not
     exposing. The DB CHECK constraint makes this fallback unreachable in normal operation. *)
  { id; slug; name; description; rules; avatar_url; banner_url; allow_downvotes; sections_enabled;
    visibility = Option.value (community_visibility_of_string visibility_s) ~default:Community_private; indexable }

(* Applied selectively to hot paths — per-query instrumentation on every call
   adds two gettimeofday syscalls and a Yojson allocation per request. *)
let with_query_timer ~name f =
  let t0 = Unix.gettimeofday () in
  f () >>= fun result ->
  let ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
  let status = match result with Ok _ -> "ok" | Error _ -> "error" in
  Logs.info (fun m ->
    m "%s" (Yojson.Safe.to_string (`Assoc [
      ("query",        `String name);
      ("execution_ms", `Float ms);
      ("status",       `String status);
    ]))
  );
  Lwt.return result

module Community = struct
  let get_all_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->* community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable FROM communities"

  let get_all_communities (module C : Caqti_lwt.CONNECTION) =
    with_query_timer ~name:"get_all_communities" (fun () ->
      C.collect_list get_all_query ()
      >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_community_row rows))
      | Error err ->
          Lwt.return (Error (Caqti_error.show err))
    )

  let create_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t3 string string (option string)) bool) ->. Caqti_type.unit)
    "INSERT INTO communities (name, slug, description, sections_enabled) VALUES ($1, $2, $3, $4)"

  let create_community (module C : Caqti_lwt.CONNECTION) name slug description sections_enabled =
    C.exec create_community_query ((name, slug, description), sections_enabled)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_slug_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable FROM communities WHERE slug = $1"

  let get_community_by_slug (module C : Caqti_lwt.CONNECTION) slug =
    C.find_opt get_by_slug_query slug
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_community_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable FROM communities WHERE id = $1"

  let get_community_by_id (module C : Caqti_lwt.CONNECTION) id =
    C.find_opt get_by_id_query id
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_community_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let search_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string int int) ->* community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable FROM communities WHERE (name ILIKE $1 OR description ILIKE $1) AND visibility = 'public' AND indexable ORDER BY name ASC LIMIT $2 OFFSET $3"

  let search_communities (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_communities_query (term, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_community_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Single UPDATE covers all editable fields — no partial-update complexity;
     settings form always submits all four fields so overwrite is safe.
     RETURNING hands back the authoritative updated row in the same round-trip;
     None means the id matched no community (previously a silent no-op). *)
  let update_community_details_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t4 (option string) (option string) (option string) (option string)) int) ->? community_row_type)
    "UPDATE communities SET description = $1, rules = $2, avatar_url = $3, banner_url = $4 WHERE id = $5 RETURNING id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable"

  let update_community_details (module C : Caqti_lwt.CONNECTION) community_id description rules avatar_url banner_url =
    C.find_opt update_community_details_query ((description, rules, avatar_url, banner_url), community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_community_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let toggle_downvotes_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 bool int) ->. Caqti_type.unit)
    "UPDATE communities SET allow_downvotes = $1 WHERE id = $2"

  let toggle_community_downvotes (module C : Caqti_lwt.CONNECTION) community_id allow =
    C.exec toggle_downvotes_query (allow, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slice E: visibility/indexability writes (set from the settings UI). visibility is the
     stored TEXT mirrored by the CHECK constraint; we accept the closed [community_visibility]
     variant and stringify here so call sites never pass a raw, unvalidated string. The DB CHECK
     is the backstop. These touch only the SEO/access columns added in Slice B — no other field. *)
  (* RETURNING the authoritative updated row: visibility is a closed PostHog
     group property, so the handler needs the post-update record for its
     $groupidentify without a second lookup. None = id matched no community. *)
  let update_community_visibility_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string int) ->? community_row_type)
    "UPDATE communities SET visibility = $1 WHERE id = $2 RETURNING id, slug, name, description, rules, avatar_url, banner_url, allow_downvotes, sections_enabled, visibility, indexable"

  let update_community_visibility (module C : Caqti_lwt.CONNECTION) community_id visibility =
    C.find_opt update_community_visibility_query (community_visibility_to_string visibility, community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_community_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let update_community_indexable_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 bool int) ->. Caqti_type.unit)
    "UPDATE communities SET indexable = $1 WHERE id = $2"

  let update_community_indexable (module C : Caqti_lwt.CONNECTION) community_id indexable =
    C.exec update_community_indexable_query (indexable, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Joins posts→communities in one query so vote_handler avoids a second round-trip. *)
  let get_allows_downvotes_for_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.bool)
    "SELECT c.allow_downvotes FROM communities c JOIN posts p ON p.community_id = c.id WHERE p.id = $1"

  let get_allows_downvotes_for_post (module C : Caqti_lwt.CONNECTION) post_id =
    C.find_opt get_allows_downvotes_for_post_query post_id
    >>= function
    | Ok (Some v) -> Lwt.return (Ok v)
    | Ok None -> Lwt.return (Ok true) (* post not found: let DB constraint handle it *)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_allows_downvotes_for_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.bool)
    "SELECT c.allow_downvotes FROM communities c JOIN posts p ON p.community_id = c.id JOIN comments cm ON cm.post_id = p.id WHERE cm.id = $1"

  let get_allows_downvotes_for_comment (module C : Caqti_lwt.CONNECTION) comment_id =
    C.find_opt get_allows_downvotes_for_comment_query comment_id
    >>= function
    | Ok (Some v) -> Lwt.return (Ok v)
    | Ok None -> Lwt.return (Ok true)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module User = struct
  let create_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t4 string string string string) ->. Caqti_type.unit)
    "INSERT INTO users (username, email, password_hash, verification_token) VALUES ($1, $2, $3, $4)"

  let create_user (module C: Caqti_lwt.CONNECTION) username email password_hash verification_token =
    C.exec create_user_query (username, email, password_hash, verification_token) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Pre-insert uniqueness check avoids leaking raw Postgres constraint errors to the UI. *)
  let user_exists_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM users WHERE username = $1 OR email = $2)"

  let user_exists (module C : Caqti_lwt.CONNECTION) username email =
    C.find user_exists_query (username, email) >>= function
    | Ok exists -> Lwt.return (Ok exists)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Nested 7-column row: (id, username, email, created_at), (hash, is_admin,
     is_banned). created_at rides the same lookup so a successful login has the
     closed analytics person properties with no extra query. *)
  let get_user_for_login_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t2 (t4 int string string string) (t3 string bool bool)))
    "SELECT id, username, email, created_at::text, password_hash, is_admin, is_banned FROM users WHERE username = $1 OR email = $1"

  let get_user_for_login (module C: Caqti_lwt.CONNECTION) identifier =
    with_query_timer ~name:"get_user_for_login" (fun () ->
      C.find_opt get_user_for_login_query identifier >>= function
      | Ok res -> Lwt.return (Ok res)
      | Error e -> Lwt.return (Error (Caqti_error.show e))
    )

  (* GDPR Art. 17 (right to erasure): scrub PII from the row, preserve post/comment rows for
     thread coherence. Tombstone [deleted_N] prevents username recycling after deletion. *)
  let anonymize_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users
     SET username = '[deleted_' || id || ']',
         email = 'deleted_' || id || '@earde.local',
         password_hash = ''
     WHERE id = $1"

  let anonymize_user (module C : Caqti_lwt.CONNECTION) user_id =
    C.exec anonymize_user_query user_id
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_user_public_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t5 int string string (option string) (option string)))
    "SELECT id, username, created_at::text, bio, avatar_url FROM users WHERE username = $1"

  let get_user_public (module C : Caqti_lwt.CONNECTION) username =
    C.find_opt get_user_public_query username
    >>= function
    | Ok (Some (id, username, created_at, bio, avatar_url)) ->
        Lwt.return (Ok (Some (id, username, created_at, bio, avatar_url)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Closed analytics person-property lookup (spec §4.3): exactly the four
     allowed fields — username, email, signup date, is_admin — nothing else. *)
  let get_user_analytics_props_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.(t4 string string string bool))
    "SELECT username, email, created_at::text, is_admin FROM users WHERE id = $1"

  let get_user_analytics_props (module C : Caqti_lwt.CONNECTION) user_id =
    C.find_opt get_user_analytics_props_query user_id
    >>= function
    | Ok row -> Lwt.return (Ok row)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let update_user_profile_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 (option string) (option string) int) ->. Caqti_type.unit)
    "UPDATE users SET bio = $1, avatar_url = $2 WHERE id = $3"

  let update_user_profile (module C : Caqti_lwt.CONNECTION) bio avatar_url user_id =
    C.exec update_user_profile_query (bio, avatar_url, user_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_user_karma_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT
       COALESCE((SELECT SUM(v.direction) FROM post_votes v JOIN posts p ON v.post_id = p.id WHERE p.user_id = $1), 0) +
       COALESCE((SELECT SUM(v.direction) FROM comment_votes v JOIN comments c ON v.comment_id = c.id WHERE c.user_id = $1), 0)"

  let get_user_karma (module C : Caqti_lwt.CONNECTION) user_id =
    C.find get_user_karma_query user_id
    >>= function
    | Ok karma -> Lwt.return (Ok karma)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_user_post_votes_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t2 int int))
    "SELECT post_id, direction FROM post_votes WHERE user_id = $1"

  let get_user_post_votes (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_user_post_votes_query user_id
    >>= function
    | Ok v -> Lwt.return (Ok v)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let get_user_comment_votes_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t2 int int))
    "SELECT comment_id, direction FROM comment_votes WHERE user_id = $1"

  let get_user_comment_votes (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_user_comment_votes_query user_id
    >>= function
    | Ok v -> Lwt.return (Ok v)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let search_users_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string int int) ->* Caqti_type.(t5 int string string (option string) (option string)))
    "SELECT id, username, created_at::text, bio, avatar_url FROM users WHERE username ILIKE $1 OR bio ILIKE $1 ORDER BY username ASC LIMIT $2 OFFSET $3"

  let search_users (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_users_query (term, limit, offset) >>= function
    | Ok rows -> Lwt.return (Ok rows)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Username is the natural key for mod-promotion forms; using it here avoids
     leaking numeric IDs in URLs or hidden fields visible to the submitter. *)
  let get_user_by_username_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t3 int string string))
    "SELECT id, username, email FROM users WHERE username = $1"

  let get_user_by_username (module C : Caqti_lwt.CONNECTION) username =
    C.find_opt get_user_by_username_query username
    >>= function
    | Ok (Some (id, uname, email)) -> Lwt.return (Ok (Some { id; username = uname; email }))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Full table scan acceptable: admin set is tiny (O(10)); caching would
     over-engineer for a list that changes at most once per deployment. *)
  let get_admin_usernames_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->* Caqti_type.string)
    "SELECT username FROM users WHERE is_admin = true"

  let get_admin_usernames (module C : Caqti_lwt.CONNECTION) =
    C.collect_list get_admin_usernames_query ()
    >>= function
    | Ok rows -> Lwt.return (Ok rows)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Point-read by PK: used in mod/ban handlers to deny acting on global admins. *)
  let is_user_admin_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.bool)
    "SELECT is_admin FROM users WHERE id = $1"

  let is_user_admin (module C : Caqti_lwt.CONNECTION) user_id =
    C.find is_user_admin_query user_id
    >>= function
    | Ok b -> Lwt.return (Ok b)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Post = struct
  let create_post_query =
    let open Caqti_request.Infix in
    (* RETURNING id lets the handler fan-out @mention notifications without a
       second query — avoids a race between INSERT and SELECT MAX(id).
       section_id nullable: NULL for simple-feed posts, set for structured section posts. *)
    (Caqti_type.(t2 (t4 string (option string) (option string) (option string)) (t3 (option int) int int)) ->! Caqti_type.int)
    "INSERT INTO posts (title, url, content, image_url, section_id, community_id, user_id) VALUES ($1, $2, $3, $4, $5, $6, $7) RETURNING id"

  let create_post (module C : Caqti_lwt.CONNECTION) title url content image_url section_id community_id user_id =
    C.find create_post_query ((title, url, content, image_url), (section_id, community_id, user_id)) >>= function
    | Ok id -> Lwt.return (Ok id)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_post_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? post_row_type)
    "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
            COALESCE((SELECT SUM(direction) FROM post_votes WHERE post_id = p.id), 0) AS score,
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count,
            a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
            COALESCE(cus.local_karma, 0), COALESCE(cus.local_post_count, 0),
            COALESCE(cus.local_comment_count, 0), cus.first_active_at::text
     FROM posts p
     JOIN users u ON p.user_id = u.id
     JOIN communities a ON p.community_id = a.id
     LEFT JOIN community_sections cs ON cs.id = p.section_id
     LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
     WHERE p.id = $1"

  let get_post_by_id (module C : Caqti_lwt.CONNECTION) id =
    with_query_timer ~name:"get_post_by_id" (fun () ->
      C.find_opt get_post_by_id_query id >>= function
      | Ok (Some row) -> Lwt.return (Ok (Some (map_post_row row)))
      | Ok None -> Lwt.return (Ok None)
      | Error err -> Lwt.return (Error (Caqti_error.show err))
    )

  let get_all_posts (module C : Caqti_lwt.CONNECTION) (sort_mode : sort_mode) limit offset =
    with_query_timer ~name:"get_all_posts" (fun () ->
      (* Hot: HN gravity — (score+1)/(age_hours+2)^1.5. Exponent 1.5 decays faster than
         Reddit's 1.8, favouring freshness. The sort_mode variant guarantees that
         order_clause is always one of these four hardcoded string literals — no user
         input can reach Printf.sprintf regardless of call site.
         Public discovery surface: only public + indexable communities appear here.
         Private and public-non-indexable content is reachable only by direct URL
         (gated in Slice C) or the membership-scoped personalized feed, never globally.
         Slice G: also drop posts in a non-indexable forum section (cs left-joined on
         section_id; NULL section = root post, kept). The personalized feed deliberately
         does NOT apply this — it is a member's own following surface, not discovery. *)
      let order_clause = match sort_mode with
        | Newest -> "ORDER BY p.created_at DESC"
        | Top    -> "ORDER BY score DESC, p.created_at DESC"
        | Hot    -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
        | Active -> "ORDER BY p.last_activity_at DESC"
      in
      let query_str = Printf.sprintf
        "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
                COALESCE(SUM(v.direction), 0) as score,
                (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count,
                a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
                COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
                COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
         FROM posts p
         JOIN users u ON p.user_id = u.id
         JOIN communities a ON p.community_id = a.id
         LEFT JOIN post_votes v ON p.id = v.post_id
         LEFT JOIN community_sections cs ON cs.id = p.section_id
         LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
         WHERE a.visibility = 'public' AND a.indexable
           AND (p.section_id IS NULL OR cs.indexable)
         GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled
         %s
         LIMIT $1 OFFSET $2" order_clause
      in
      let query =
        let open Caqti_request.Infix in
        (Caqti_type.(t2 int int) ->* post_row_type) query_str
      in
      C.collect_list query (limit, offset)
      >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err))
    )

  let get_posts_by_community (module C : Caqti_lwt.CONNECTION) community_id (sort_mode : sort_mode) limit offset =
    (* Same HN gravity formula as get_all_posts; same type-driven injection safety. *)
    let order_clause = match sort_mode with
      | Newest -> "ORDER BY p.created_at DESC"
      | Top    -> "ORDER BY score DESC, p.created_at DESC"
      | Hot    -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
      | Active -> "ORDER BY p.last_activity_at DESC"
    in
    let query_str = Printf.sprintf
      "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
              COALESCE(SUM(v.direction), 0) as score,
              (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count,
              a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
              COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
              COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
       FROM posts p
       JOIN users u ON p.user_id = u.id
       JOIN communities a ON p.community_id = a.id
       LEFT JOIN post_votes v ON p.id = v.post_id
       LEFT JOIN community_sections cs ON cs.id = p.section_id
       LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
       WHERE p.community_id = $1
       GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled
       %s
       LIMIT $2 OFFSET $3" order_clause
    in
    let query =
      let open Caqti_request.Infix in
      (Caqti_type.(t3 int int int) ->* post_row_type) query_str
    in
    C.collect_list query (community_id, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Section feed: same HN gravity as get_posts_by_community but scoped to one section.
     section_id FK enforces community ownership in SQL — no extra join needed. *)
  let get_posts_by_section (module C : Caqti_lwt.CONNECTION) community_id section_id (sort_mode : sort_mode) limit offset =
    let order_clause = match sort_mode with
      | Newest -> "ORDER BY p.created_at DESC"
      | Top    -> "ORDER BY score DESC, p.created_at DESC"
      | Hot    -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
      | Active -> "ORDER BY p.last_activity_at DESC"
    in
    let query_str = Printf.sprintf
      "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
              COALESCE(SUM(v.direction), 0) as score,
              (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count,
              a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
              COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
              COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
       FROM posts p
       JOIN users u ON p.user_id = u.id
       JOIN communities a ON p.community_id = a.id
       LEFT JOIN post_votes v ON p.id = v.post_id
       LEFT JOIN community_sections cs ON cs.id = p.section_id
       LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
       WHERE p.community_id = $1 AND p.section_id = $2
       GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled
       %s
       LIMIT $3 OFFSET $4" order_clause
    in
    let query =
      let open Caqti_request.Infix in
      (Caqti_type.(t4 int int int int) ->* post_row_type) query_str
    in
    C.collect_list query (community_id, section_id, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_posts_by_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* post_row_type)
    "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
            COALESCE((SELECT SUM(direction) FROM post_votes WHERE post_id = p.id), 0) AS score,
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count,
            a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
            COALESCE(cus.local_karma, 0), COALESCE(cus.local_post_count, 0),
            COALESCE(cus.local_comment_count, 0), cus.first_active_at::text
     FROM posts p
     JOIN users u ON p.user_id = u.id
     JOIN communities a ON p.community_id = a.id
     LEFT JOIN community_sections cs ON cs.id = p.section_id
     LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
     WHERE p.user_id = $1 ORDER BY score DESC, p.created_at DESC"

  let get_posts_by_user (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_posts_by_user_query user_id >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slice C/D/G profile leak-filter: map a set of post ids to their owning community's id, raw
     visibility string, indexable flag AND a per-post "non-indexable section" flag in ONE bounded
     IN-list query (ids comma-joined, expanded via string_to_array to sidestep array binding — same
     idiom as get_thread_sources_for_posts).
     Lets the profile drop private-community posts/comments for viewers who can't read them (Slice C),
     public-but-non-indexable activity (Slice D), AND activity sitting in a non-indexable forum
     section (Slice G) from public discovery, with no N+1; comments otherwise carry no community/
     section linkage. The 5th column is TRUE only when the post is in a section flagged
     indexable=false (NULL section = root post = FALSE). Empty input short-circuits. *)
  let post_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->* Caqti_type.(t2 (t4 int int string bool) bool))
    "SELECT p.id, p.community_id, c.visibility, c.indexable, \
            (p.section_id IS NOT NULL AND NOT cs.indexable) \
     FROM posts p \
     JOIN communities c ON c.id = p.community_id \
     LEFT JOIN community_sections cs ON cs.id = p.section_id \
     WHERE p.id = ANY(string_to_array($1, ',')::int[])"

  let get_post_communities (module C : Caqti_lwt.CONNECTION) post_ids =
    match post_ids with
    | [] -> Lwt.return (Ok [])
    | _ ->
        let csv = String.concat "," (List.map string_of_int post_ids) in
        C.collect_list post_communities_query csv >>= function
        | Ok rows ->
            Lwt.return (Ok (List.map (fun ((id, cid, vis, ix), sec_excluded) -> (id, cid, vis, ix, sec_excluded)) rows))
        | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* CTE captures old direction before upsert so the delta is exact even on flip (e.g. -1→+1 = +2).
     The final INSERT upserts community_user_stats for the post AUTHOR, not the voter. *)
  let vote_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
    {|WITH old AS (
        SELECT COALESCE(direction, 0) AS d FROM post_votes WHERE user_id = $1 AND post_id = $2
      ), uv AS (
        INSERT INTO post_votes (user_id, post_id, direction) VALUES ($1, $2, $3)
        ON CONFLICT (user_id, post_id) DO UPDATE SET direction = EXCLUDED.direction
        RETURNING post_id
      )
      INSERT INTO community_user_stats (user_id, community_id, local_karma, first_active_at)
        SELECT p.user_id, p.community_id,
               ($3 - COALESCE((SELECT d FROM old), 0)),
               CURRENT_TIMESTAMP
        FROM posts p WHERE p.id = (SELECT post_id FROM uv)
      ON CONFLICT (user_id, community_id) DO UPDATE
        SET local_karma = community_user_stats.local_karma + ($3 - COALESCE((SELECT d FROM old), 0))|}

  let vote_post (module C : Caqti_lwt.CONNECTION) user_id post_id direction =
    C.exec vote_post_query (user_id, post_id, direction) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* CTE reads the old direction before delete; UPDATE adjusts the author's local karma.
     If no stats row exists yet (pre-feature vote), the UPDATE is a safe no-op. *)
  let remove_post_vote_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    {|WITH old AS (
        SELECT direction AS d FROM post_votes WHERE user_id = $1 AND post_id = $2
      ), del AS (
        DELETE FROM post_votes WHERE user_id = $1 AND post_id = $2
      )
      UPDATE community_user_stats
        SET local_karma = local_karma - COALESCE((SELECT d FROM old), 0)
      WHERE user_id = (SELECT user_id FROM posts WHERE id = $2)
        AND community_id = (SELECT community_id FROM posts WHERE id = $2)|}

  let remove_post_vote (module C : Caqti_lwt.CONNECTION) user_id post_id =
    C.exec remove_post_vote_query (user_id, post_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let soft_delete_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.int Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posts SET content = '[deleted]', url = NULL, image_url = NULL WHERE id = $1 AND user_id = $2"

  let soft_delete_post (module C : Caqti_lwt.CONNECTION) post_id user_id =
    C.exec soft_delete_post_query (post_id, user_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let search_posts_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string int int) ->* post_row_type)
    "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
            COALESCE(SUM(v.direction), 0) AS score,
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count,
            a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
            COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
            COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
     FROM posts p
     JOIN users u ON p.user_id = u.id
     JOIN communities a ON p.community_id = a.id
     LEFT JOIN post_votes v ON p.id = v.post_id
     LEFT JOIN community_sections cs ON cs.id = p.section_id
     LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
     WHERE (p.title ILIKE $1 OR p.content ILIKE $1) AND a.visibility = 'public' AND a.indexable
       AND (p.section_id IS NULL OR cs.indexable)
     GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled
     ORDER BY score DESC, p.created_at DESC LIMIT $2 OFFSET $3"

  let search_posts (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_posts_query (term, limit, offset) >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Personalized feed: identical HN gravity to get_all_posts but filtered to
     communities the user has joined. JOIN on community_members instead of a
     subquery — avoids a correlated scan per post on large datasets. *)
  let get_personalized_feed (module C : Caqti_lwt.CONNECTION) user_id (sort_mode : sort_mode) limit offset =
    with_query_timer ~name:"get_personalized_feed" (fun () ->
      (* Same type-driven injection safety as get_all_posts. *)
      let order_clause = match sort_mode with
        | Newest -> "ORDER BY p.created_at DESC"
        | Top    -> "ORDER BY score DESC, p.created_at DESC"
        | Hot    -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
        | Active -> "ORDER BY p.last_activity_at DESC"
      in
      let query_str = Printf.sprintf
        "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
                COALESCE(SUM(v.direction), 0) as score,
                (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count,
                a.allow_downvotes, p.image_url, cs.name, cs.slug, a.sections_enabled,
                COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
                COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
         FROM posts p
         JOIN users u ON p.user_id = u.id
         JOIN communities a ON p.community_id = a.id
         JOIN community_members cm ON p.community_id = cm.community_id AND cm.user_id = $1
         LEFT JOIN post_votes v ON p.id = v.post_id
         LEFT JOIN community_sections cs ON cs.id = p.section_id
         LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
         GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled
         %s
         LIMIT $2 OFFSET $3" order_clause
      in
      let query =
        let open Caqti_request.Infix in
        (Caqti_type.(t3 int int int) ->* post_row_type) query_str
      in
      C.collect_list query (user_id, limit, offset)
      >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err))
    )
end

module Comment = struct
  (* Wilson score lower bound (z=1.96, 95% CI) orders comments by quality under low vote counts.
     Raw SUM(direction) would surface polarising comments; Wilson penalises low-sample confidence.
     JOIN posts pp to get community_id for the cus LEFT JOIN — all comments here share one post_id. *)
  let get_comments_by_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 (t4 int string string string) (t3 int (option int) (option string)) (t4 int int int (option string))))
    "SELECT c.id, c.content, u.username, c.created_at::text,
            COALESCE(SUM(v.direction), 0) AS score, c.parent_id, u.avatar_url,
            COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
            COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
     FROM comments c
     JOIN users u ON c.user_id = u.id
     JOIN posts pp ON pp.id = c.post_id
     LEFT JOIN comment_votes v ON c.id = v.comment_id
     LEFT JOIN community_user_stats cus ON cus.user_id = c.user_id AND cus.community_id = pp.community_id
     WHERE c.post_id = $1
     GROUP BY c.id, u.username, u.avatar_url
     ORDER BY
       CASE
         WHEN COUNT(v.direction) = 0 THEN 0.0
         ELSE
           (
             (SUM(CASE WHEN v.direction = 1 THEN 1.0 ELSE 0.0 END) + 1.9208) / (COUNT(v.direction) + 3.8416)
             -
             1.96 * SQRT( (SUM(CASE WHEN v.direction = 1 THEN 1.0 ELSE 0.0 END) * SUM(CASE WHEN v.direction = -1 THEN 1.0 ELSE 0.0 END)) / NULLIF(COUNT(v.direction)::float, 0.0) + 0.9604 ) / (COUNT(v.direction) + 3.8416)
           )
       END DESC,
       COALESCE(SUM(v.direction), 0) DESC,
       c.created_at ASC"

  let get_comments (module C : Caqti_lwt.CONNECTION) post_id =
    C.collect_list get_comments_by_post_query post_id
    >>= function
    | Ok rows ->
        let comments = List.map (fun ((id, content, username, created_at), (score, parent_id, avatar_url), (author_local_karma, author_local_post_count, author_local_comment_count, author_first_active_at)) ->
          { id; content; username; created_at; score; parent_id; avatar_url; author_local_karma; author_local_post_count; author_local_comment_count; author_first_active_at }
        ) rows in
        Lwt.return (Ok comments)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* RETURNING id: the inserted comment id is part of the success payload so
     later consumers (e.g. analytics) never need a second lookup query. *)
  let create_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t4 string int int (option int)) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id, parent_id) VALUES ($1, $2, $3, $4) RETURNING id"

  let create_comment (module C : Caqti_lwt.CONNECTION) content post_id user_id parent_id =
    C.find create_comment_query (content, post_id, user_id, parent_id)
    >>= function
    | Ok comment_id -> Lwt.return (Ok comment_id)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Bumped on every new comment so the "active" sort reflects engagement recency, not creation time. *)
  let touch_last_activity_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posts SET last_activity_at = CURRENT_TIMESTAMP WHERE id = $1"

  let touch_last_activity (module C : Caqti_lwt.CONNECTION) post_id =
    C.exec touch_last_activity_query post_id
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Same CTE pattern as vote_post: captures old direction, upserts vote, updates author local karma.
     JOINs comments→posts to resolve the community_id for the stats row. *)
  let vote_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
    {|WITH old AS (
        SELECT COALESCE(direction, 0) AS d FROM comment_votes WHERE user_id = $1 AND comment_id = $2
      ), uv AS (
        INSERT INTO comment_votes (user_id, comment_id, direction) VALUES ($1, $2, $3)
        ON CONFLICT (user_id, comment_id) DO UPDATE SET direction = EXCLUDED.direction
        RETURNING comment_id
      )
      INSERT INTO community_user_stats (user_id, community_id, local_karma, first_active_at)
        SELECT c.user_id, p.community_id,
               ($3 - COALESCE((SELECT d FROM old), 0)),
               CURRENT_TIMESTAMP
        FROM comments c
        JOIN posts p ON p.id = c.post_id
        WHERE c.id = (SELECT comment_id FROM uv)
      ON CONFLICT (user_id, community_id) DO UPDATE
        SET local_karma = community_user_stats.local_karma + ($3 - COALESCE((SELECT d FROM old), 0))|}

  let vote_comment (module C : Caqti_lwt.CONNECTION) user_id comment_id direction =
    C.exec vote_comment_query (user_id, comment_id, direction)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* JOIN posts to surface the parent post title — avoids a second round-trip per comment
     in the profile page. GROUP BY c.id, p.id, p.title because p.title is not functionally
     dependent on c.id from Postgres's perspective (different table). *)
  let get_comments_by_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t2 (t3 int string string) (t3 int string int)))
    "SELECT c.id, c.content, c.created_at::text, c.post_id, p.title, COALESCE(SUM(v.direction), 0) AS score
     FROM comments c
     JOIN posts p ON c.post_id = p.id
     LEFT JOIN comment_votes v ON c.id = v.comment_id
     WHERE c.user_id = $1
     GROUP BY c.id, p.id, p.title
     ORDER BY c.created_at DESC"

  let get_comments_by_user (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_comments_by_user_query user_id
    >>= function
    | Ok rows ->
        let flat = List.map (fun ((id, content, created_at), (post_id, post_title, score)) ->
          (id, content, created_at, post_id, post_title, score)
        ) rows in
        Lwt.return (Ok flat)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let remove_comment_vote_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    {|WITH old AS (
        SELECT direction AS d FROM comment_votes WHERE user_id = $1 AND comment_id = $2
      ), del AS (
        DELETE FROM comment_votes WHERE user_id = $1 AND comment_id = $2
      )
      UPDATE community_user_stats
        SET local_karma = local_karma - COALESCE((SELECT d FROM old), 0)
      WHERE user_id = (SELECT user_id FROM comments WHERE id = $2)
        AND community_id = (SELECT p.community_id FROM comments c JOIN posts p ON p.id = c.post_id WHERE c.id = $2)|}

  let remove_comment_vote (module C : Caqti_lwt.CONNECTION) user_id comment_id =
    C.exec remove_comment_vote_query (user_id, comment_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let soft_delete_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.int Caqti_type.int ->. Caqti_type.unit)
    "UPDATE comments SET content = '[deleted]' WHERE id = $1 AND user_id = $2"

  let soft_delete_comment (module C : Caqti_lwt.CONNECTION) comment_id user_id =
    C.exec soft_delete_comment_query (comment_id, user_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let search_comments_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string int int) ->* Caqti_type.(t6 int string string string int int))
    "SELECT c.id, c.content, u.username, c.created_at::text, c.post_id, COALESCE(SUM(v.direction), 0) AS score
     FROM comments c
     JOIN users u ON c.user_id = u.id
     JOIN posts pp ON pp.id = c.post_id
     JOIN communities a ON a.id = pp.community_id
     LEFT JOIN community_sections cs ON cs.id = pp.section_id
     LEFT JOIN comment_votes v ON c.id = v.comment_id
     WHERE c.content ILIKE $1 AND a.visibility = 'public' AND a.indexable
       AND (pp.section_id IS NULL OR cs.indexable)
     GROUP BY c.id, u.username, c.created_at, c.post_id
     ORDER BY score DESC, c.created_at DESC LIMIT $2 OFFSET $3"

  let search_comments (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_comments_query (term, limit, offset) >>= function
    | Ok rows -> Lwt.return (Ok rows)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* 8 columns split t2(t4,t4) for Caqti arity. Inner JOINs only: a comment whose post or
     community has been hard-deleted is unreportable (None), which is the correct gate —
     there is nothing to moderate. Soft-deleted comments still resolve (content tombstone). *)
  let get_comment_report_target_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.(t2 (t4 int string int string) (t4 int string int string)))
    "SELECT c.id, c.content, c.user_id, u.username, c.post_id, p.title, p.community_id, comm.slug
     FROM comments c
     JOIN users u ON c.user_id = u.id
     JOIN posts p ON c.post_id = p.id
     JOIN communities comm ON p.community_id = comm.id
     WHERE c.id = $1"

  let get_comment_report_target (module C : Caqti_lwt.CONNECTION) comment_id =
    C.find_opt get_comment_report_target_query comment_id >>= function
    | Ok (Some ((crt_comment_id, crt_content, crt_author_user_id, crt_author_username),
                (crt_post_id, crt_post_title, crt_community_id, crt_community_slug))) ->
        Lwt.return (Ok (Some {
          crt_comment_id; crt_content; crt_author_user_id; crt_author_username;
          crt_post_id; crt_post_title; crt_community_id; crt_community_slug }))
    | Ok None -> Lwt.return (Ok None)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

module Section = struct
  (* 9-column section row: t3(t4, t4, bool) — trailing bool is the SEO indexable flag. *)
  let section_row_type =
    let open Caqti_type in
    t3 (t4 int int string string) (t4 (option string) int string bool) bool

  let map_section_row ((section_id, community_id, name, slug), (description, position, default_sort, is_introduction_section), indexable) =
    { section_id; community_id; name; slug; description; position; default_sort; is_introduction_section; indexable }

  (* lowercase, trim, spaces/dashes → single dash, strip non-alphanumeric *)
  let slugify name =
    let name = String.lowercase_ascii (String.trim name) in
    let buf = Buffer.create (String.length name) in
    let prev_dash = ref false in
    String.iter (fun c ->
      match c with
      | 'a'..'z' | '0'..'9' -> prev_dash := false; Buffer.add_char buf c
      | ' ' | '-' | '_' ->
          if not !prev_dash then Buffer.add_char buf '-';
          prev_dash := true
      | _ -> ()
    ) name;
    let s = Buffer.contents buf in
    let len = String.length s in
    if len > 0 && s.[len-1] = '-' then String.sub s 0 (len-1) else s

  let slug_exists_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int string) ->? Caqti_type.int)
    "SELECT 1 FROM community_sections WHERE community_id = $1 AND slug = $2"

  let slug_exists (module C : Caqti_lwt.CONNECTION) community_id slug =
    C.find_opt slug_exists_query (community_id, slug)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Tries base, then base-2, base-3… until a free slug is found. *)
  let rec find_unique_slug db community_id base n =
    let candidate = if n = 1 then base else Printf.sprintf "%s-%d" base n in
    slug_exists db community_id candidate >>= function
    | Ok false -> Lwt.return (Ok candidate)
    | Ok true  -> find_unique_slug db community_id base (n + 1)
    | Error e  -> Lwt.return (Error e)

  let create_section_query =
    let open Caqti_request.Infix in
    (* 7 params: t2(t4, t3) — community_id, name, slug, description, position, default_sort, is_intro *)
    (Caqti_type.(t2 (t4 int string string (option string)) (t3 int string bool)) ->. Caqti_type.unit)
    "INSERT INTO community_sections (community_id, name, slug, description, position, default_sort, is_introduction_section) VALUES ($1, $2, $3, $4, $5, $6, $7)"

  let create_section (module C : Caqti_lwt.CONNECTION) community_id name description position default_sort is_intro =
    let base = slugify name in
    let base = if base = "" then "section" else base in
    (* "uncategorized" is reserved for the virtual orphaned-posts section. *)
    if base = "uncategorized" then Lwt.return (Error "The name 'Uncategorized' is reserved. Please choose a different section name.")
    else
    find_unique_slug (module C) community_id base 1 >>= function
    | Error e -> Lwt.return (Error e)
    | Ok slug ->
        let valid_sorts = ["hot"; "new"; "top"; "active"] in
        let safe_sort = if List.mem default_sort valid_sorts then default_sort else "new" in
        C.exec create_section_query ((community_id, name, slug, description), (position, safe_sort, is_intro))
        >>= function
        | Ok () -> Lwt.return (Ok ())
        | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, is_introduction_section, indexable FROM community_sections WHERE community_id = $1 ORDER BY is_introduction_section DESC, position ASC, name ASC"

  let get_sections_by_community (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_by_community_query community_id
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_section_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Point-read by (id, community_id) — validates ownership without a JOIN.
     Used in the create_post handler to prevent posting to a foreign section. *)
  let get_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, is_introduction_section, indexable FROM community_sections WHERE id = $1 AND community_id = $2"

  let get_section_by_id (module C : Caqti_lwt.CONNECTION) section_id community_id =
    C.find_opt get_by_id_query (section_id, community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_section_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_slug_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string int) ->? section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, is_introduction_section, indexable FROM community_sections WHERE slug = $1 AND community_id = $2"

  let get_section_by_slug (module C : Caqti_lwt.CONNECTION) slug community_id =
    C.find_opt get_by_slug_query (slug, community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_section_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slug is immutable after creation — URLs must remain stable.
     Only name, description, and default_sort are editable. *)
  let update_section_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t3 string (option string) string) int) ->. Caqti_type.unit)
    "UPDATE community_sections SET name = $1, description = $2, default_sort = $3 WHERE id = $4"

  let update_section (module C : Caqti_lwt.CONNECTION) section_id name description default_sort =
    let valid_sorts = ["hot"; "new"; "top"; "active"] in
    let safe_sort = if List.mem default_sort valid_sorts then default_sort else "new" in
    C.exec update_section_query ((name, description, safe_sort), section_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slice H: section indexability toggle. Community-scoped WHERE (id AND community_id) so a
     forged section_id from another community cannot be flipped. The Slice-G resolver already
     reads this column to decide section/thread noindex + discovery exclusion. *)
  let update_section_indexable_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 bool (t2 int int)) ->. Caqti_type.unit)
    "UPDATE community_sections SET indexable = $1 WHERE id = $2 AND community_id = $3"

  let update_section_indexable (module C : Caqti_lwt.CONNECTION) section_id community_id indexable =
    C.exec update_section_indexable_query (indexable, (section_id, community_id))
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* ON DELETE SET NULL on posts.section_id releases posts back to the root feed safely. *)
  let delete_section_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_sections WHERE id = $1 AND community_id = $2"

  let delete_section (module C : Caqti_lwt.CONNECTION) section_id community_id =
    C.exec delete_section_query (section_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* 11-column stats row: t3(t4,t4,t3) = section fields(8) + indexable + post_count + last_activity.
     LEFT JOIN keeps empty sections visible in the overview — COUNT returns 0, MAX returns NULL.
     GROUP BY cs.id covers all cs.* fields (functional dependency on PK). *)
  let stats_row_type =
    let open Caqti_type in
    t3 (t4 int int string string) (t4 (option string) int string bool) (t3 bool int (option string))

  let map_stats_row ((section_id, community_id, name, slug), (description, position, default_sort, is_introduction_section), (indexable, post_count, last_activity)) =
    ({ section_id; community_id; name; slug; description; position; default_sort; is_introduction_section; indexable }, post_count, last_activity)

  let get_stats_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* stats_row_type)
    "SELECT cs.id, cs.community_id, cs.name, cs.slug, cs.description, cs.position, cs.default_sort, cs.is_introduction_section,
            cs.indexable,
            COUNT(p.id)::int AS post_count,
            MAX(p.last_activity_at)::text AS last_activity
     FROM community_sections cs
     LEFT JOIN posts p ON p.section_id = cs.id
     WHERE cs.community_id = $1
     GROUP BY cs.id
     ORDER BY cs.is_introduction_section DESC, cs.position ASC, cs.name ASC"

  let get_sections_with_stats (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_stats_query community_id
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_stats_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Orphaned posts: section_id IS NULL in a structured community, left behind by deleted sections.
     NULL::text casts avoid a pointless LEFT JOIN since section_id IS NULL always. *)
  let orphaned_count_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.(t2 int (option string)))
    "SELECT COUNT(p.id)::int, MAX(p.last_activity_at)::text FROM posts p WHERE p.community_id = $1 AND p.section_id IS NULL"

  let get_orphaned_count_and_activity (module C : Caqti_lwt.CONNECTION) community_id =
    C.find orphaned_count_query community_id >>= function
    | Ok (count, last_activity) -> Lwt.return (Ok (count, last_activity))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_orphaned_posts (module C : Caqti_lwt.CONNECTION) community_id (sort_mode : sort_mode) limit offset =
    let order_clause = match sort_mode with
      | Newest -> "ORDER BY p.created_at DESC"
      | Top    -> "ORDER BY score DESC, p.created_at DESC"
      | Hot    -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
      | Active -> "ORDER BY p.last_activity_at DESC"
    in
    let query_str = Printf.sprintf
      "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
              COALESCE(SUM(v.direction), 0) as score,
              (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count,
              a.allow_downvotes, p.image_url, NULL::text AS section_name, NULL::text AS section_slug, a.sections_enabled,
              COALESCE(MAX(cus.local_karma), 0), COALESCE(MAX(cus.local_post_count), 0),
              COALESCE(MAX(cus.local_comment_count), 0), MAX(cus.first_active_at)::text
       FROM posts p
       JOIN users u ON p.user_id = u.id
       JOIN communities a ON p.community_id = a.id
       LEFT JOIN post_votes v ON p.id = v.post_id
       LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
       WHERE p.community_id = $1 AND p.section_id IS NULL
       GROUP BY p.id, u.username, a.slug, a.allow_downvotes, a.sections_enabled
       %s
       LIMIT $2 OFFSET $3" order_clause
    in
    let query =
      let open Caqti_request.Infix in
      (Caqti_type.(t3 int int int) ->* post_row_type) query_str
    in
    C.collect_list query (community_id, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Channel = struct
  (* 8-column channel row: t2(t4, t4). created_at::text keeps every row type
     string-typed, matching the rest of db.ml. *)
  let channel_row_type =
    let open Caqti_type in
    t3 (t4 int int string string) (t4 (option string) int bool string) bool

  (* Explicit : channel annotation — `id`/`community_id`/`slug`/`name`/`position`
     are shared with other records; the literal resolves via topic/is_archived,
     but we pin it to stay robust against future field overlaps. Trailing bool is the
     SEO indexable flag for the channel's public archive surface. *)
  let map_channel_row ((id, community_id, slug, name), (topic, position, is_archived, created_at), indexable) : channel =
    { id; community_id; slug; name; topic; position; is_archived; created_at; indexable }

  (* lowercase, trim, spaces/dashes → single dash, strip non-alphanumeric.
     Self-contained copy of Section.slugify: each inner module owns its helpers,
     and channels have no reserved "uncategorized" slug to share. *)
  let slugify name =
    let name = String.lowercase_ascii (String.trim name) in
    let buf = Buffer.create (String.length name) in
    let prev_dash = ref false in
    String.iter (fun c ->
      match c with
      | 'a'..'z' | '0'..'9' -> prev_dash := false; Buffer.add_char buf c
      | ' ' | '-' | '_' ->
          if not !prev_dash then Buffer.add_char buf '-';
          prev_dash := true
      | _ -> ()
    ) name;
    let s = Buffer.contents buf in
    let len = String.length s in
    if len > 0 && s.[len-1] = '-' then String.sub s 0 (len-1) else s

  let slug_exists_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int string) ->? Caqti_type.int)
    "SELECT 1 FROM channels WHERE community_id = $1 AND slug = $2"

  let slug_exists (module C : Caqti_lwt.CONNECTION) community_id slug =
    C.find_opt slug_exists_query (community_id, slug)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Tries base, then base-2, base-3… until a free slug is found. *)
  let rec find_unique_slug db community_id base n =
    let candidate = if n = 1 then base else Printf.sprintf "%s-%d" base n in
    slug_exists db community_id candidate >>= function
    | Ok false -> Lwt.return (Ok candidate)
    | Ok true  -> find_unique_slug db community_id base (n + 1)
    | Error e  -> Lwt.return (Error e)

  let create_channel_query =
    let open Caqti_request.Infix in
    (* 5 params: community_id, slug, name, topic, position. is_archived and
       created_at take their column defaults. *)
    (Caqti_type.(t2 (t4 int string string (option string)) int) ->. Caqti_type.unit)
    "INSERT INTO channels (community_id, slug, name, topic, position) VALUES ($1, $2, $3, $4, $5)"

  (* Returns the generated slug (not unit like create_section): the slug is
     auto-derived, so the caller cannot know it otherwise, and the Step-3 handler
     needs it to redirect to /c/:slug/chat/:channel_slug. *)
  let create_channel (module C : Caqti_lwt.CONNECTION) community_id name topic position =
    let base = slugify name in
    let base = if base = "" then "channel" else base in
    find_unique_slug (module C) community_id base 1 >>= function
    | Error e -> Lwt.return (Error e)
    | Ok slug ->
        C.exec create_channel_query ((community_id, slug, name, topic), position)
        >>= function
        | Ok () -> Lwt.return (Ok slug)
        | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* channel_row_type)
    "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE community_id = $1 ORDER BY is_archived ASC, position ASC, name ASC"

  let get_channels_by_community (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_by_community_query community_id
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_channel_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_slug_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string int) ->? channel_row_type)
    "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE slug = $1 AND community_id = $2"

  let get_channel_by_slug (module C : Caqti_lwt.CONNECTION) slug community_id =
    C.find_opt get_by_slug_query (slug, community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_channel_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Point-read by (id, community_id) — validates ownership without a JOIN,
     mirroring Section.get_section_by_id. *)
  let get_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? channel_row_type)
    "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE id = $1 AND community_id = $2"

  let get_channel_by_id (module C : Caqti_lwt.CONNECTION) channel_id community_id =
    C.find_opt get_by_id_query (channel_id, community_id)
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_channel_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* set rather than one-way archive: covers archive and un-archive, and matches
     update_section's community-scoped UPDATE style. *)
  let set_archived_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t2 int int) bool) ->. Caqti_type.unit)
    "UPDATE channels SET is_archived = $3 WHERE id = $1 AND community_id = $2"

  let set_channel_archived (module C : Caqti_lwt.CONNECTION) channel_id community_id is_archived =
    C.exec set_archived_query ((channel_id, community_id), is_archived)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Display-only edit: name + topic, never slug. Slug stays stable so existing
     /c/:slug/ch/:channel_slug links and bookmarks keep resolving, and messages
     (keyed by channel_id) are unaffected. Community-scoped WHERE mirrors
     set_channel_archived / update_section. *)
  let update_channel_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t2 string (option string)) (t2 int int)) ->. Caqti_type.unit)
    "UPDATE channels SET name = $1, topic = $2 WHERE id = $3 AND community_id = $4"

  let update_channel (module C : Caqti_lwt.CONNECTION) channel_id community_id name topic =
    C.exec update_channel_query ((name, topic), (channel_id, community_id))
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slice H: channel indexability toggle. Community-scoped WHERE mirrors update_channel /
     set_channel_archived. Flips only the archive-surface noindex + provenance-safety behavior
     Slice G already wires; it does NOT make the channel private (readable by direct URL still). *)
  let update_channel_indexable_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 bool (t2 int int)) ->. Caqti_type.unit)
    "UPDATE channels SET indexable = $1 WHERE id = $2 AND community_id = $3"

  let update_channel_indexable (module C : Caqti_lwt.CONNECTION) channel_id community_id indexable =
    C.exec update_channel_indexable_query (indexable, (channel_id, community_id))
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

(* Durable chat. Postgres is the source of truth; all chat writes funnel through here
   so a later Realtime_bus can fan out copies after the DB commit without becoming the
   source of truth. No JOIN to users: author display is a render concern and these
   queries stay index-only on (channel_id, id). search_tsv is left unmaintained until
   the search-indexing step. *)
module Chat = struct
  (* 7-column message row: t2(t4, t3). id is int64 (BIGSERIAL); the three timestamps
     are ::text-cast to keep every row field string-typed, matching the rest of db.ml. *)
  let chat_message_row_type =
    let open Caqti_type in
    t2 (t4 int64 int (option int) string) (t3 string (option string) (option string))

  let map_chat_message_row
      ((id, channel_id, user_id, content), (created_at, edited_at, deleted_at)) : chat_message =
    { id; channel_id; user_id; content; created_at; edited_at; deleted_at }

  (* user_id is the non-optional int here (see chat_message comment); created_at,
     edited_at, deleted_at take their column defaults/NULL. RETURNING the full row
     hands the caller the canonical persisted message — Postgres-assigned id and
     created_at included — in the single insert round-trip, so the composer's JSON
     response and the realtime publish never need a read-back. *)
  let send_message_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int string) ->! chat_message_row_type)
    "INSERT INTO chat_messages (channel_id, user_id, content) VALUES ($1, $2, $3) \
     RETURNING id, channel_id, user_id, content, created_at::text, edited_at::text, deleted_at::text"

  let send_message (module C : Caqti_lwt.CONNECTION) channel_id user_id content =
    C.find send_message_query (channel_id, user_id, content) >>= function
    | Ok row -> Lwt.return (Ok (map_chat_message_row row))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int64 ->? chat_message_row_type)
    "SELECT id, channel_id, user_id, content, created_at::text, edited_at::text, deleted_at::text \
     FROM chat_messages WHERE id = $1"

  let get_message_by_id (module C : Caqti_lwt.CONNECTION) id =
    C.find_opt get_by_id_query id >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_chat_message_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Initial channel-view render: newest `limit` messages. Fetched id DESC (uses the
     (channel_id, id) index) then List.rev'd to ascending so the caller renders oldest
     → newest top-to-bottom. *)
  let get_recent_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->* chat_message_row_type)
    "SELECT id, channel_id, user_id, content, created_at::text, edited_at::text, deleted_at::text \
     FROM chat_messages WHERE channel_id = $1 ORDER BY id DESC LIMIT $2"

  let get_recent_messages (module C : Caqti_lwt.CONNECTION) channel_id limit =
    with_query_timer ~name:"chat_get_recent_messages" (fun () ->
      C.collect_list get_recent_query (channel_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.rev_map map_chat_message_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

  (* SSR initial render needs the author name per message; the index-only reads above
     deliberately skip the users JOIN (author display is a render concern, the hot
     realtime path stays on the (channel_id, id) index). This separate render-oriented
     read LEFT JOINs users so a single round-trip resolves authors — username is None
     when user_id is NULL (GDPR tombstone) and the render layer shows "[deleted]".
     8-column row: t2(t4, t4). *)
  let chat_message_author_row_type =
    let open Caqti_type in
    t2 (t4 int64 int (option int) string) (t4 string (option string) (option string) (option string))

  let map_chat_message_author_row
      ((id, channel_id, user_id, content), (created_at, edited_at, deleted_at, username))
      : chat_message * string option =
    ({ id; channel_id; user_id; content; created_at; edited_at; deleted_at }, username)

  let get_recent_with_authors_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, m.edited_at::text, \
            m.deleted_at::text, u.username \
     FROM chat_messages m LEFT JOIN users u ON u.id = m.user_id \
     WHERE m.channel_id = $1 ORDER BY m.id DESC LIMIT $2"

  let get_recent_messages_with_authors (module C : Caqti_lwt.CONNECTION) channel_id limit =
    with_query_timer ~name:"chat_get_recent_messages_with_authors" (fun () ->
      C.collect_list get_recent_with_authors_query (channel_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.rev_map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

  (* Archive keyset pagination (older page): messages with id < before_id, newest
     first then reversed to ascending. The caller passes the smallest id it has seen. *)
  let get_before_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int64 int) ->* chat_message_row_type)
    "SELECT id, channel_id, user_id, content, created_at::text, edited_at::text, deleted_at::text \
     FROM chat_messages WHERE channel_id = $1 AND id < $2 ORDER BY id DESC LIMIT $3"

  let get_messages_before_id (module C : Caqti_lwt.CONNECTION) channel_id before_id limit =
    with_query_timer ~name:"chat_get_messages_before_id" (fun () ->
      C.collect_list get_before_query (channel_id, before_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.rev_map map_chat_message_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

  (* Generic realtime-resume / replay read: everything newer than after_id, already
     ascending. A later realtime layer uses this to catch a reconnecting client up from
     its last-seen id — Postgres is the source of truth, the bus is best-effort. limit
     caps the replay size. *)
  let get_after_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int64 int) ->* chat_message_row_type)
    "SELECT id, channel_id, user_id, content, created_at::text, edited_at::text, deleted_at::text \
     FROM chat_messages WHERE channel_id = $1 AND id > $2 ORDER BY id ASC LIMIT $3"

  let get_messages_after_id (module C : Caqti_lwt.CONNECTION) channel_id after_id limit =
    with_query_timer ~name:"chat_get_messages_after_id" (fun () ->
      C.collect_list get_after_query (channel_id, after_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_chat_message_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

  (* Author-scoped: only the message's author edits, so the user_id guard prevents one
     user editing another's message (mirrors soft_delete_comment's ownership scoping). *)
  let edit_message_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int64 int string) ->. Caqti_type.unit)
    "UPDATE chat_messages SET content = $3, edited_at = CURRENT_TIMESTAMP WHERE id = $1 AND user_id = $2"

  let edit_message (module C : Caqti_lwt.CONNECTION) message_id user_id content =
    C.exec edit_message_query (message_id, user_id, content) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* By id only (no user_id guard): both the author and moderators delete through this
     one write path, so authorization is decided by the handler, not baked into the DB
     layer. Sets deleted_at and leaves content intact for auditability; archive reads
     still return the row and the render layer masks it. GDPR content erasure is the
     separate anonymize_user path. *)
  let soft_delete_message_query =
    let open Caqti_request.Infix in
    (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE chat_messages SET deleted_at = CURRENT_TIMESTAMP WHERE id = $1"

  let soft_delete_message (module C : Caqti_lwt.CONNECTION) message_id =
    C.exec soft_delete_message_query message_id >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Author-joined variants of the before/after keyset reads, used by the
     "start thread from chat" form to show nearby messages with their authors
     (so the body prefill can quote "Name: > text"). Same 8-column author row and
     LEFT JOIN-users tombstone handling as get_recent_with_authors; the hot
     realtime path keeps using the index-only readers above. *)
  let get_before_with_authors_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int64 int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, m.edited_at::text, \
            m.deleted_at::text, u.username \
     FROM chat_messages m LEFT JOIN users u ON u.id = m.user_id \
     WHERE m.channel_id = $1 AND m.id < $2 ORDER BY m.id DESC LIMIT $3"

  let get_messages_before_id_with_authors (module C : Caqti_lwt.CONNECTION) channel_id before_id limit =
    with_query_timer ~name:"chat_get_messages_before_id_with_authors" (fun () ->
      C.collect_list get_before_with_authors_query (channel_id, before_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.rev_map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

  let get_after_with_authors_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int64 int) ->* chat_message_author_row_type)
    "SELECT m.id, m.channel_id, m.user_id, m.content, m.created_at::text, m.edited_at::text, \
            m.deleted_at::text, u.username \
     FROM chat_messages m LEFT JOIN users u ON u.id = m.user_id \
     WHERE m.channel_id = $1 AND m.id > $2 ORDER BY m.id ASC LIMIT $3"

  let get_messages_after_id_with_authors (module C : Caqti_lwt.CONNECTION) channel_id after_id limit =
    with_query_timer ~name:"chat_get_messages_after_id_with_authors" (fun () ->
      C.collect_list get_after_with_authors_query (channel_id, after_id, limit) >>= function
      | Ok rows -> Lwt.return (Ok (List.map map_chat_message_author_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))
end

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
module ThreadSource = struct
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
     Slice G: a marker is suppressed when its source channel is not publicly indexable (private
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
end

module Membership = struct
  let join_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2) ON CONFLICT DO NOTHING"

  let join_community (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec join_community_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let is_member_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT 1 FROM community_members WHERE user_id = $1 AND community_id = $2"

  let is_member (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt is_member_query (user_id, community_id)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* DELETE ... RETURNING so the caller can tell a real membership removal
     (true) from a no-op non-member request (false) in the same round-trip —
     the analytics wiring must not report a community_left that never
     happened. (user_id, community_id) is unique, so at most one row. *)
  let leave_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "DELETE FROM community_members WHERE user_id = $1 AND community_id = $2 RETURNING community_id"

  let leave_community (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt leave_community_query (user_id, community_id)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Slice F: the allow-list for a private community, rendered as the member-management list on
     /c/:slug/settings. Mirrors Ban.get_banned_users. community_members has no timestamp column,
     so order by username. Mods/admins are NOT necessarily here (they read via their role) — this
     lists membership rows only, which is exactly what add/remove manage. *)
  let get_community_members_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 int string string))
    "SELECT u.id, u.username, u.email
     FROM users u
     JOIN community_members cm ON u.id = cm.user_id
     WHERE cm.community_id = $1
     ORDER BY u.username ASC"

  let get_community_members (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_community_members_query community_id
    >>= function
    | Ok rows ->
        let users = List.map (fun (id, username, email) -> { id; username; email }) rows in
        Lwt.return (Ok users)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_user_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* community_row_type)
    "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, a.banner_url, a.allow_downvotes, a.sections_enabled, a.visibility, a.indexable
     FROM communities a
     JOIN community_members am ON a.id = am.community_id
     WHERE am.user_id = $1
     ORDER BY a.name ASC"

  let get_user_communities (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_user_communities_query user_id
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_community_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Moderator = struct
  (* ON CONFLICT DO NOTHING: idempotent — re-promoting the same user is a no-op
     rather than an error; safe for re-runs and concurrent create_community calls. *)
  let add_moderator_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id) VALUES ($1, $2) ON CONFLICT DO NOTHING"

  let add_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec add_moderator_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Community creators need top_mod from birth — default role is 'mod', so we
     must set role explicitly. ON CONFLICT UPDATE covers the edge case where the
     user somehow already exists as a plain mod and re-creates. *)
  let add_top_moderator_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES ($1, $2, 'top_mod') ON CONFLICT (user_id, community_id) DO UPDATE SET role = 'top_mod'"

  let add_top_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec add_top_moderator_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* SELECT 1 existence check is cheaper than COUNT — we only need bool, not cardinality. *)
  let is_moderator_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT 1 FROM community_moderators WHERE user_id = $1 AND community_id = $2"

  let is_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt is_moderator_query (user_id, community_id)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Point-read by (user_id, community_id): used to gate promote/remove operations
     and to derive is_top_mod without a separate is_moderator round-trip. *)
  let get_moderator_role_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "SELECT role FROM community_moderators WHERE user_id = $1 AND community_id = $2"

  let get_moderator_role (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt get_moderator_role_query (user_id, community_id)
    >>= function
    | Ok r -> Lwt.return (Ok r)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* ORDER BY promoted_at ASC: original creator appears first, preserving
     appointment history without a separate position/rank column. *)
  let get_community_moderators_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 int string string))
    "SELECT u.id, u.username, u.email
     FROM users u
     JOIN community_moderators am ON u.id = am.user_id
     WHERE am.community_id = $1
     ORDER BY am.promoted_at ASC"

  let get_community_moderators (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_community_moderators_query community_id
    >>= function
    | Ok rows ->
        let users = List.map (fun (id, username, email) -> { id; username; email }) rows in
        Lwt.return (Ok users)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* CASE ordering: top_mod=1, mod=2, legacy_mod=3 — explicit role hierarchy for the
     manage-mods panel. promoted_at ASC breaks ties preserving appointment seniority. *)
  let get_community_mods_with_roles_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 int string string))
    "SELECT u.id, u.username, cm.role
     FROM users u
     JOIN community_moderators cm ON u.id = cm.user_id
     WHERE cm.community_id = $1
     ORDER BY CASE cm.role WHEN 'top_mod' THEN 1 WHEN 'mod' THEN 2 ELSE 3 END ASC,
              cm.promoted_at ASC"

  let get_community_mods_with_roles (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_community_mods_with_roles_query community_id
    >>= function
    | Ok rows ->
        let entries = List.map (fun (user_id, username, role) -> { user_id; username; role }) rows in
        Lwt.return (Ok entries)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Hard delete: mod removal is administrative; no tombstone needed since
     mod history is not exposed publicly (unlike user content). *)
  let remove_moderator_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_moderators WHERE user_id = $1 AND community_id = $2"

  let remove_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec remove_moderator_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* COUNT before INSERT: enforces the "Max 3 Top Mods" business rule at DB read time
     rather than via a UNIQUE constraint, because the limit is per-community cardinal —
     not a uniqueness invariant on a single column. *)
  let count_top_mods_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM community_moderators WHERE community_id = $1 AND role = 'top_mod'"

  let promote_to_top_mod_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "UPDATE community_moderators SET role = 'top_mod' WHERE user_id = $1 AND community_id = $2"

  (* Two-phase guard: first reject illegal source roles, then enforce the 3-seat cap.
     Order matters — rejecting already-top_mods avoids decrementing the cap incorrectly
     when count happens to equal the cap and the target is already counted in it. *)
  let promote_to_top_mod (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt get_moderator_role_query (user_id, community_id) >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok None -> Lwt.return (Error "User is not a moderator of this community")
    | Ok (Some "top_mod") -> Lwt.return (Error "User is already a Top Mod")
    | Ok (Some "legacy_mod") -> Lwt.return (Error "Cannot promote a legacy moderator; reinstate as mod first")
    | Ok (Some _) ->
        C.find count_top_mods_query community_id >>= function
        | Error e -> Lwt.return (Error (Caqti_error.show e))
        | Ok count ->
            if count >= 3 then Lwt.return (Error "Maximum of 3 Top Mods reached for this community")
            else
              C.exec promote_to_top_mod_query (user_id, community_id) >>= function
              | Ok () -> Lwt.return (Ok ())
              | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Single UPDATE across all communities: triggered on community page load to lazily
     enforce inactivity without a background job. UPDATE FROM ... WHERE is standard
     PostgreSQL; the 3-month threshold matches the "squatter prevention" governance spec. *)
  let demote_inactive_mods_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->. Caqti_type.unit)
    "UPDATE community_moderators cm
     SET role = 'legacy_mod'
     FROM users u
     WHERE cm.user_id = u.id
       AND cm.role IN ('top_mod', 'mod')
       AND (u.last_active_at IS NULL OR u.last_active_at < NOW() - INTERVAL '3 months')"

  let demote_inactive_mods (module C : Caqti_lwt.CONNECTION) =
    C.exec demote_inactive_mods_query ()
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Inverse of get_community_moderators: used for profile badge display.
     ORDER BY promoted_at ASC keeps creation order consistent with mod panels. *)
  let get_moderated_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* community_row_type)
    "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, a.banner_url, a.allow_downvotes, a.sections_enabled, a.visibility, a.indexable
     FROM communities a
     JOIN community_moderators am ON a.id = am.community_id
     WHERE am.user_id = $1
     ORDER BY am.promoted_at ASC"

  let get_moderated_communities (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_moderated_communities_query user_id
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_community_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Ban = struct
  (* ON CONFLICT DO NOTHING: idempotent — re-banning the same user is a no-op
     rather than an error; safe for concurrent mod actions. *)
  let ban_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_bans (user_id, community_id) VALUES ($1, $2) ON CONFLICT DO NOTHING"

  let ban_user (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec ban_user_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let unban_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_bans WHERE user_id = $1 AND community_id = $2"

  let unban_user (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec unban_user_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* SELECT 1 existence check — same pattern as is_moderator. *)
  let is_banned_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT 1 FROM community_bans WHERE user_id = $1 AND community_id = $2"

  let is_banned (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find_opt is_banned_query (user_id, community_id)
    >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* ORDER BY banned_at ASC: chronological audit trail aids mod review. *)
  let get_banned_users_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 int string string))
    "SELECT u.id, u.username, u.email
     FROM users u
     JOIN community_bans ab ON u.id = ab.user_id
     WHERE ab.community_id = $1
     ORDER BY ab.banned_at ASC"

  let get_banned_users (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_banned_users_query community_id
    >>= function
    | Ok rows ->
        let users = List.map (fun (id, username, email) -> { id; username; email }) rows in
        Lwt.return (Ok users)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Notification = struct
  let get_notifications_query =
    let open Caqti_request.Infix in
    (* Caqti arity limit: encode 7 columns as t2(t4, t3) nested tuples.
       post_id is nullable since ban notifications have no associated post. *)
    (Caqti_type.int ->* Caqti_type.(t2 (t4 int int (option int) string) (t3 string bool string)))
    "SELECT id, user_id, post_id, notif_type, message, is_read, created_at::text FROM notifications WHERE user_id = $1 ORDER BY created_at DESC LIMIT 50"

  let get_notifications (module C: Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_notifications_query user_id >>= function
    | Ok rows -> Lwt.return (Ok (List.map (fun ((id, user_id, post_id, notif_type), (message, is_read, created_at)) -> {id; user_id; post_id; notif_type; message; is_read; created_at}) rows))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let count_unread_notifs_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM notifications WHERE user_id = $1 AND is_read = FALSE"

  let count_unread_notifs (module C: Caqti_lwt.CONNECTION) user_id =
    C.find count_unread_notifs_query user_id >>= function
    | Ok c -> Lwt.return (Ok c)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let mark_notifs_read_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE notifications SET is_read = TRUE WHERE user_id = $1"

  let mark_notifs_read (module C: Caqti_lwt.CONNECTION) user_id =
    C.exec mark_notifs_read_query user_id >>= function
    | Ok () -> Lwt.return (Ok())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let create_notif_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t4 int (option int) string string) ->. Caqti_type.unit)
    "INSERT INTO notifications (user_id, post_id, notif_type, message) VALUES ($1, $2, $3, $4)"

  let create_notif (module C: Caqti_lwt.CONNECTION) user_id post_id_opt notif_type message =
    C.exec create_notif_query (user_id, post_id_opt, notif_type, message) >>= function
    | Ok () -> Lwt.return (Ok())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Notification delivery is best-effort; collapse Ok None and Error into the same
     Error path so callers can skip silently if the post/comment was deleted. *)
  let get_post_owner_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.int)
    "SELECT user_id FROM posts WHERE id = $1"

  let get_post_owner (module C: Caqti_lwt.CONNECTION) pid =
    C.find_opt get_post_owner_query pid >>= function
    | Ok (Some id) -> Lwt.return (Ok id)
    | _ -> Lwt.return (Error "not found")

  let get_comment_owner_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.int)
    "SELECT user_id FROM comments WHERE id = $1"

  let get_comment_owner (module C: Caqti_lwt.CONNECTION) cid =
    C.find_opt get_comment_owner_query cid >>= function
    | Ok (Some id) -> Lwt.return (Ok id)
    | _ -> Lwt.return (Error "not found")

  let get_comment_post_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.int)
    "SELECT post_id FROM comments WHERE id = $1"

  let get_comment_post_id (module C: Caqti_lwt.CONNECTION) cid =
    C.find_opt get_comment_post_id_query cid >>= function
    | Ok (Some id) -> Lwt.return (Ok id)
    | _ -> Lwt.return (Error "not found")
end

module Analytics = struct
  let log_page_view_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string (option string) string) ->. Caqti_type.unit)
    "INSERT INTO page_views (path, referer, session_hash) VALUES ($1, $2, $3)"

  let log_page_view (module C: Caqti_lwt.CONNECTION) path referer session_hash =
    C.exec log_page_view_query (path, referer, session_hash) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* 5 scalar columns fit in t2(t3, t2) — no arity overflow.
     end_date is inclusive: we shift it to the next day's midnight with ::date + INTERVAL '1 day'
     so that "2026-03-23" captures all events on that calendar day. *)
  let kpi_stats_type =
    let open Caqti_type in
    t2 (t3 int int int) (t2 int int)

  let get_kpi_dashboard_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->! kpi_stats_type)
    "SELECT
      (SELECT COUNT(*)::int       FROM page_views WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'),
      (SELECT COUNT(DISTINCT session_hash)::int FROM page_views WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'),
      (SELECT COUNT(*)::int       FROM users    WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'),
      (SELECT COUNT(*)::int FROM posts    WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day')
        + (SELECT COUNT(*)::int FROM comments WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'),
      (SELECT COUNT(*)::int       FROM users    WHERE last_active_at >= $1::timestamptz AND last_active_at < $2::date + INTERVAL '1 day')"

  let get_kpi_dashboard (module C: Caqti_lwt.CONNECTION) ~start_date ~end_date =
    C.find get_kpi_dashboard_query (start_date, end_date) >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* DAU/MAU: avg per-day unique contributors / total unique contributors in the range.
     posts+comments are used as the activity proxy because last_active_at tracks only the
     most recent timestamp per user, giving no per-day granularity.
     LEFT JOIN + GROUP BY collapses the N daily rows into a single AVG; COALESCE handles
     ranges with zero activity without returning NULL. *)
  let get_dau_mau_ratio_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->! Caqti_type.float)
    {sql|WITH daily_active AS (
      SELECT DATE(created_at) AS day, COUNT(DISTINCT user_id) AS dau
      FROM (
        SELECT created_at, user_id FROM posts
        WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'
        UNION ALL
        SELECT created_at, user_id FROM comments
        WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'
      ) all_activity
      GROUP BY DATE(created_at)
    ),
    mau AS (
      SELECT COUNT(DISTINCT user_id)::float AS total
      FROM (
        SELECT user_id FROM posts
        WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'
        UNION ALL
        SELECT user_id FROM comments
        WHERE created_at >= $1::timestamptz AND created_at < $2::date + INTERVAL '1 day'
      ) all_mau
    )
    SELECT CASE WHEN mau.total = 0 THEN 0.0
                ELSE COALESCE(AVG(daily_active.dau::float), 0.0) / mau.total
           END
    FROM mau
    LEFT JOIN daily_active ON true
    GROUP BY mau.total|sql}

  let get_dau_mau_ratio (module C: Caqti_lwt.CONNECTION) ~start_date ~end_date =
    C.find get_dau_mau_ratio_query (start_date, end_date) >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

(* Presence is operational state, not analytics: last_active_at is read by
   Moderator.demote_inactive_mods, so this must survive any replacement of the
   page-view analytics system. *)
module Presence = struct
  let touch_user_active_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET last_active_at = CURRENT_TIMESTAMP WHERE id = $1"

  let touch_user_active (module C: Caqti_lwt.CONNECTION) user_id =
    C.exec touch_user_active_query user_id >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

module Security = struct
  let update_password_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string int) ->. Caqti_type.unit)
    "UPDATE users SET password_hash = $1 WHERE id = $2"

  let update_password (module C: Caqti_lwt.CONNECTION) user_id new_hash =
    C.exec update_password_query (new_hash, user_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* UPDATE+RETURNING atomically consumes the token — avoids TOCTOU race of a separate
     SELECT then UPDATE, and prevents replay on concurrent verification attempts. *)
  let verify_email_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.string)
    "UPDATE users SET is_email_verified = TRUE, verification_token = NULL WHERE verification_token = $1 RETURNING username"

  let verify_email (module C: Caqti_lwt.CONNECTION) token =
    C.find_opt verify_email_query token >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

end

module Admin = struct
  (* Label is a SQL parameter so mods get "[removed by moderator]" and admins
     get "[removed by admin]" — avoids duplicating the query. *)
  let admin_delete_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.string Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posts SET content = $1, url = NULL, image_url = NULL WHERE id = $2"

  let admin_delete_post (module C: Caqti_lwt.CONNECTION) ~label post_id =
    C.exec admin_delete_post_query (label, post_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let admin_delete_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.string Caqti_type.int ->. Caqti_type.unit)
    "UPDATE comments SET content = $1 WHERE id = $2"

  let admin_delete_comment (module C : Caqti_lwt.CONNECTION) ~label comment_id =
    C.exec admin_delete_comment_query (label, comment_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Community-scoped moderator tombstones. Unlike the admin variants above, the
     mutation itself is bound to the route community — a moderator of community A
     must not be able to tombstone content in community B by forging the numeric
     id. RETURNING proves a row actually matched: C.exec reports Ok () even when
     zero rows update, which must never count as a deletion (it would produce a
     modlog entry and a notification for a mutation that never happened). *)
  let mod_delete_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.int Caqti_type.int ->? Caqti_type.int)
    "UPDATE posts SET content = '[removed by moderator]', url = NULL, image_url = NULL
     WHERE id = $1 AND community_id = $2 RETURNING id"

  let mod_delete_post (module C : Caqti_lwt.CONNECTION) ~community_id post_id =
    C.find_opt mod_delete_post_query (post_id, community_id) >>= function
    | Ok (Some _) -> Lwt.return (Ok true)
    | Ok None -> Lwt.return (Ok false)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Comment ownership is comment -> post -> community; the join enforces it
     atomically. RETURNING c.post_id doubles as the match proof and the redirect/
     notification target. *)
  let mod_delete_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.int Caqti_type.int ->? Caqti_type.int)
    "UPDATE comments AS c SET content = '[removed by moderator]'
     FROM posts AS p
     WHERE c.id = $1 AND c.post_id = p.id AND p.community_id = $2
     RETURNING c.post_id"

  let mod_delete_comment (module C : Caqti_lwt.CONNECTION) ~community_id comment_id =
    C.find_opt mod_delete_comment_query (comment_id, community_id) >>= function
    | Ok (Some post_id) -> Lwt.return (Ok (Some post_id))
    | Ok None -> Lwt.return (Ok None)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let ban_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

  let ban_user (module C: Caqti_lwt.CONNECTION) user_id =
    C.exec ban_user_query user_id >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* SELECT rather than comparing a boolean param — Caqti bool binding is driver-
     dependent; SELECT the column and let OCaml own the bool conversion. *)
  let is_globally_banned_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.bool)
    "SELECT is_banned FROM users WHERE id = $1"

  let is_globally_banned (module C: Caqti_lwt.CONNECTION) user_id =
    C.find_opt is_globally_banned_query user_id >>= function
    | Ok (Some b) -> Lwt.return (Ok b)
    | Ok None     -> Lwt.return (Ok false)
    | Error e     -> Lwt.return (Error (Caqti_error.show e))

  let unban_user_global_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = FALSE WHERE id = $1"

  let unban_user_global (module C: Caqti_lwt.CONNECTION) user_id =
    C.exec unban_user_global_query user_id >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* ORDER BY username for stable rendering in the admin dashboard. *)
  let get_globally_banned_users_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->* Caqti_type.(t3 int string string))
    "SELECT id, username, email FROM users WHERE is_banned = TRUE ORDER BY username"

  let get_globally_banned_users (module C: Caqti_lwt.CONNECTION) =
    C.collect_list get_globally_banned_users_query () >>= function
    | Ok rows -> Lwt.return (Ok (List.map (fun (id, username, email) -> { id; username; email }) rows))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Read-only operational reads for the /admin dashboard. Both are bounded by an
     explicit LIMIT so a growing users/pending_signups table can never turn the
     dashboard into a full-table scan. Per-user activity is computed as correlated
     scalar subqueries over the LIMIT-bounded outer set — no N+1 from OCaml, one query.
     A row COUNT is bigint in Postgres; ::int keeps the Caqti decode an int (per-user
     counts cannot overflow int in practice). 9 columns -> nested t3(t3,t3,t3). *)
  let list_recent_users_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 (t3 int string string) (t3 string bool bool) (t3 int int int)))
    "SELECT u.id, u.username, u.email, u.created_at::text, u.is_admin, u.is_banned, \
            (SELECT COUNT(*) FROM posts p WHERE p.user_id = u.id)::int, \
            (SELECT COUNT(*) FROM comments c WHERE c.user_id = u.id)::int, \
            (SELECT COUNT(*) FROM chat_messages m WHERE m.user_id = u.id AND m.deleted_at IS NULL)::int \
     FROM users u ORDER BY u.created_at DESC, u.id DESC LIMIT $1"

  let list_recent_users (module C : Caqti_lwt.CONNECTION) ~limit =
    C.collect_list list_recent_users_query limit >>= function
    | Ok rows ->
        let map ((id, username, email), (created_at, is_admin, is_banned),
                 (post_count, comment_count, message_count)) : admin_recent_user =
          { id; username; email; created_at; is_admin; is_banned;
            post_count; comment_count; message_count }
        in
        Lwt.return (Ok (List.map map rows))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Active (unconsumed, unexpired) pending signups only — the same liveness predicate
     used elsewhere in PendingSignup. ::text casts the tz timestamps for string decode. *)
  let list_recent_pending_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t2 (t3 int string string) (t3 string string (option string))))
    "SELECT id, username, email, created_at::text, expires_at::text, ip_address \
     FROM pending_signups WHERE consumed_at IS NULL AND expires_at > NOW() \
     ORDER BY created_at DESC LIMIT $1"

  let list_recent_pending (module C : Caqti_lwt.CONNECTION) ~limit =
    C.collect_list list_recent_pending_query limit >>= function
    | Ok rows ->
        let map ((id, username, email), (created_at, expires_at, ip_address)) : pending_signup_row =
          { id; username; email; created_at; expires_at; ip_address }
        in
        Lwt.return (Ok (List.map map rows))
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

module PasswordReset = struct
  (* DB stores only the SHA-256 hex digest of the raw token — a stolen
     password_resets row cannot be used directly; the attacker also needs the
     raw token that was emailed. Same principle as password hashing. *)
  let hash_token raw = Digestif.SHA256.(digest_string raw |> to_hex)

  (* INSERT...SELECT atomically creates the token iff the email maps to a live user.
     RETURNING distinguishes "email not found" from "inserted" without a second SELECT,
     avoiding TOCTOU between the lookup and the insert. *)
  let create_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->? Caqti_type.int)
    "INSERT INTO password_resets (token, user_id, expires_at)
     SELECT $1, id, NOW() + INTERVAL '2 hours' FROM users WHERE email = $2
     RETURNING user_id"

  let create_token (module C : Caqti_lwt.CONNECTION) email raw_token =
    let token_hash = hash_token raw_token in
    C.find_opt create_query (token_hash, email) >>= function
    | Ok res -> Lwt.return (Ok (Option.is_some res))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* expires_at > NOW() makes tokens inert after 2h with no background job required. *)
  let validate_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.int)
    "SELECT user_id FROM password_resets WHERE token = $1 AND expires_at > NOW()"

  let validate_token (module C : Caqti_lwt.CONNECTION) raw_token =
    let token_hash = hash_token raw_token in
    C.find_opt validate_query token_hash >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let consume_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.int)
    "DELETE FROM password_resets WHERE token = $1 AND expires_at > NOW() RETURNING user_id"

  (* Separate query to avoid a cross-module reference to Security.update_password_query. *)
  let update_pw_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string int) ->. Caqti_type.unit)
    "UPDATE users SET password_hash = $1 WHERE id = $2"

  (* Wraps DELETE+UPDATE in a single transaction so that if the UPDATE fails the
     token is rolled back — user retains the reset link rather than being locked out.
     Argon2 hashing must be done BEFORE calling this so no CPU work stalls the txn. *)
  let reset_password_atomically (module C : Caqti_lwt.CONNECTION) raw_token new_hash =
    C.start () >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
      (C.find_opt consume_query (hash_token raw_token) >>= function
      | Error e ->
          C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
      | Ok None ->
          (* Token not found or expired — nothing to roll back. *)
          C.rollback () >>= fun _ -> Lwt.return (Ok false)
      | Ok (Some user_id) ->
          (C.exec update_pw_query (new_hash, user_id) >>= function
          | Error e ->
              C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
          | Ok () ->
              C.commit () >>= function
              | Error e -> Lwt.return (Error (Caqti_error.show e))
              | Ok () -> Lwt.return (Ok true)))
end

(* Holds unconfirmed signups so a bot/abandoned signup never reaches the users table.
   Only a confirmed (token-clicked) pending becomes a real user. token_hash is the
   SHA-256 of the emailed token — same don't-store-the-raw-credential principle as
   password_resets and argon2 password hashing. *)
module PendingSignup = struct
  let hash_token raw = Digestif.SHA256.(digest_string raw |> to_hex)

  (* Only NON-expired, unconsumed rows are a live claim on a username. An expired
     row is a squatter the upsert clears, so it must not read as "taken" here — else
     an abandoned signup would block that username forever. Same email is allowed:
     that is the owner resubmitting, handled as a replace by [upsert]. *)
  let username_pending_elsewhere_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM pending_signups
       WHERE consumed_at IS NULL AND expires_at > NOW()
         AND LOWER(username) = LOWER($1) AND LOWER(email) <> LOWER($2))"

  let username_pending_elsewhere (module C : Caqti_lwt.CONNECTION) username email =
    C.find username_pending_elsewhere_query (username, email) >>= function
    | Ok exists -> Lwt.return (Ok exists)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* The partial unique indexes (LOWER(email)/LOWER(username) WHERE consumed_at IS NULL)
     still include expired-but-unconsumed rows, so before inserting we must clear any
     row that would collide:
       - this email's prior attempt (active OR expired) -> a clean resend/replace
       - any EXPIRED row squatting the desired username  -> frees the username index
     A DIFFERENT user's ACTIVE pending on the username is left intact; the handler
     rejects that up front via [username_pending_elsewhere]. *)
  let clear_collisions_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "DELETE FROM pending_signups
       WHERE consumed_at IS NULL
         AND ( LOWER(email) = LOWER($1)
            OR ( LOWER(username) = LOWER($2) AND expires_at <= NOW() ) )"

  (* 24h window: long enough that users who confirm later in the day still succeed,
     short enough that the table stays small. Hardcoded like password_resets' 2h. *)
  let insert_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t4 string string string string) (t2 (option string) (option string))) ->. Caqti_type.unit)
    "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at, ip_address, user_agent)
     VALUES ($1, $2, $3, $4, NOW() + INTERVAL '24 hours', $5, $6)"

  (* DELETE-then-INSERT in one transaction so a fresh signup atomically replaces any
     stale/own collision. Argon2 hashing must be done BEFORE this so no CPU work
     stalls the txn — same rule as the password-reset path. *)
  let upsert (module C : Caqti_lwt.CONNECTION) ~username ~email ~password_hash ~token_hash ~ip ~user_agent =
    C.start () >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
      (C.exec clear_collisions_query (email, username) >>= function
       | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
       | Ok () ->
         (C.exec insert_query ((username, email, password_hash, token_hash), (ip, user_agent)) >>= function
          | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
          | Ok () ->
            (C.commit () >>= function
             | Error e -> Lwt.return (Error (Caqti_error.show e))
             | Ok () -> Lwt.return (Ok ()))))

  (* Best-effort secondary cleanup only — correctness never depends on it, since
     [upsert] already removes collisions. Bounded by the expires_at index. *)
  let sweep_expired_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->. Caqti_type.unit)
    "DELETE FROM pending_signups WHERE expires_at < NOW() - INTERVAL '1 day'"

  let sweep_expired (module C : Caqti_lwt.CONNECTION) =
    C.exec sweep_expired_query () >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* FOR UPDATE locks the matched row so two concurrent confirmations of the same
     token can't both insert a user. consumed_at IS NULL excludes replays; expires_at
     > NOW() excludes expired tokens — both surface as `Invalid. *)
  let select_pending_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t4 int string string string))
    "SELECT id, username, email, password_hash FROM pending_signups
       WHERE token_hash = $1 AND consumed_at IS NULL AND expires_at > NOW()
       FOR UPDATE"

  let user_conflict_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM users WHERE username = $1 OR email = $2)"

  (* is_email_verified TRUE: clicking the link proves the address, so the new user is
     created already-verified; verification_token stays NULL (the legacy /verify column
     is irrelevant to pending-signup users). *)
  let insert_user_query =
    let open Caqti_request.Infix in
    (* created_at/is_admin come back from the same RETURNING so the caller has
       the authoritative closed person properties (analytics §4.3) without a
       post-transaction lookup. *)
    (Caqti_type.(t3 string string string ->! t3 int string bool))
    "INSERT INTO users (username, email, password_hash, is_email_verified) VALUES ($1, $2, $3, TRUE) RETURNING id, created_at::text, is_admin"

  let mark_consumed_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE pending_signups SET consumed_at = NOW() WHERE id = $1"

  (* Confirms a pending in one transaction so a token is consumed exactly once:
     find -> re-check users -> insert user -> mark consumed -> commit. Any failure
     rolls the whole thing back. Returns the new user id (from the insert's
     RETURNING, no later lookup) and the username for the success page. *)
  let confirm (module C : Caqti_lwt.CONNECTION) token_hash =
    C.start () >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
      (C.find_opt select_pending_query token_hash >>= function
       | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
       | Ok None -> C.rollback () >>= fun _ -> Lwt.return (Ok `Invalid)
       | Ok (Some (id, username, email, password_hash)) ->
         (C.find user_conflict_query (username, email) >>= function
          | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
          | Ok true -> C.rollback () >>= fun _ -> Lwt.return (Ok `Conflict)
          | Ok false ->
            (C.find insert_user_query (username, email, password_hash) >>= function
             | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
             | Ok (user_id, created_at, is_admin) ->
               (C.exec mark_consumed_query id >>= function
                | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
                | Ok () ->
                  (C.commit () >>= function
                   | Error e -> Lwt.return (Error (Caqti_error.show e))
                   | Ok () ->
                     Lwt.return
                       (Ok (`Confirmed (user_id, username, email, created_at, is_admin))))))))
end

(* Durable PostHog person-deletion jobs (analytics spec §3.3). Job state only —
   the Persons-API HTTP client lives in Posthog_deletion, and no function here
   ever performs network IO. *)
module PosthogDeletionJobs = struct
  (* Retry lease: a claimed pending job is ineligible for this long, so an
     immediate attempt and a maintenance retry (or two concurrent retries)
     cannot process the same job at once; a crash after claiming simply makes
     the job eligible again after the lease. *)
  let default_lease_minutes = 15

  let lock_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? Caqti_type.int)
    "SELECT id FROM users WHERE id = $1 FOR UPDATE"

  (* ON CONFLICT (distinct_id) DO UPDATE is a no-op rewrite that still RETURNS
     the existing row's id, so duplicate/concurrent deletions deterministically
     converge on ONE job instead of erroring or creating competitors. *)
  let enqueue_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id) VALUES ($1)
     ON CONFLICT (distinct_id) DO UPDATE SET distinct_id = EXCLUDED.distinct_id
     RETURNING id"

  (* §3.3 atomic local deletion: lock the user row, apply exactly the
     anonymize_user rewrite, enqueue (or adopt) the durable deletion job, and
     commit both together. The FOR UPDATE lock serializes concurrent deletions
     of the same account. No HTTP happens anywhere near this transaction. *)
  let anonymize_and_enqueue (module C : Caqti_lwt.CONNECTION) user_id =
    let distinct_id = Posthog.distinct_id_of_user_id user_id in
    C.start () >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
      (C.find_opt lock_user_query user_id >>= function
       | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
       | Ok _locked_row ->
         (* A missing row (already hard-deleted) keeps the old anonymize_user
            semantics — the UPDATE matches nothing — while the deletion job is
            still enqueued: PostHog may hold data regardless. *)
         (User.anonymize_user (module C) user_id >>= function
          | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
          | Ok () ->
            (C.find enqueue_query distinct_id >>= function
             | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
             | Ok job_id ->
               (C.commit () >>= function
                | Error e -> Lwt.return (Error (Caqti_error.show e))
                | Ok () -> Lwt.return (Ok (job_id, distinct_id))))))

  (* Atomic claim of one specific pending job (the immediate post-deletion
     attempt): attempts and the lease timestamp advance in the same statement.
     None = already completed, or claimed within the lease by someone else. *)
  let claim_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "UPDATE posthog_person_deletion_jobs
     SET attempts = attempts + 1, last_attempt_at = NOW()
     WHERE id = $1 AND status = 'pending'
       AND (last_attempt_at IS NULL
            OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))
     RETURNING distinct_id"

  let claim (module C : Caqti_lwt.CONNECTION) ?(lease_minutes = default_lease_minutes) job_id =
    C.find_opt claim_query (job_id, lease_minutes)
    >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Maintenance batch claim: oldest eligible pending jobs first, bounded by
     LIMIT; FOR UPDATE SKIP LOCKED keeps concurrent invocations from blocking
     on (or double-claiming) the same rows. RETURNING order is unspecified, so
     the caller re-sorts by id (BIGSERIAL follows enqueue order). *)
  (* Canonical job-queue claim: the picked set lives in a CTE (materialized —
     it contains FOR UPDATE), because a bare `WHERE id IN (SELECT ... LIMIT n
     FOR UPDATE SKIP LOCKED)` may re-evaluate the subquery during the outer
     scan and claim more than n rows. *)
  let claim_batch_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->* Caqti_type.(t2 int string))
    "WITH picked AS (
       SELECT id FROM posthog_person_deletion_jobs
       WHERE status = 'pending'
         AND (last_attempt_at IS NULL
              OR last_attempt_at < NOW() - ($2 * INTERVAL '1 minute'))
       ORDER BY created_at ASC, id ASC
       LIMIT $1
       FOR UPDATE SKIP LOCKED)
     UPDATE posthog_person_deletion_jobs j
     SET attempts = j.attempts + 1, last_attempt_at = NOW()
     FROM picked
     WHERE j.id = picked.id
     RETURNING j.id, j.distinct_id"

  let claim_batch (module C : Caqti_lwt.CONNECTION) ?(lease_minutes = default_lease_minutes) ~limit () =
    C.collect_list claim_batch_query (limit, lease_minutes)
    >>= function
    | Ok rows ->
        Lwt.return (Ok (List.sort (fun (a, _) (b, _) -> compare a b) rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let mark_completed_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs
     SET status = 'completed', completed_at = NOW(), last_error = NULL
     WHERE id = $1"

  let mark_completed (module C : Caqti_lwt.CONNECTION) job_id =
    C.exec mark_completed_query job_id
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* attempts/last_attempt_at were already advanced by the claim; a failure
     only records the bounded safe diagnostic. Length-capped as a backstop —
     callers must already pass short error classes, never response bodies. *)
  let mark_failed_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs
     SET last_error = $2
     WHERE id = $1 AND status = 'pending'"

  let mark_failed (module C : Caqti_lwt.CONNECTION) job_id error =
    let error =
      if String.length error > 120 then String.sub error 0 120 else error
    in
    C.exec mark_failed_query (job_id, error)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Test/inspection lookup: (id, status, attempts, last_error). *)
  let get_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t4 int string int (option string)))
    "SELECT id, status, attempts, last_error
     FROM posthog_person_deletion_jobs WHERE distinct_id = $1"

  let get_by_distinct_id (module C : Caqti_lwt.CONNECTION) distinct_id =
    C.find_opt get_query distinct_id
    >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Mod_action = struct
  (* 8-column result: nested t2(t4, t4) to stay within Caqti's per-tuple arity limit. *)
  let mod_action_row_type =
    let open Caqti_type in
    t2 (t4 int int int string) (t4 string (option int) string string)

  let log_action_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t2 int int) (t3 string (option int) string)) ->. Caqti_type.unit)
    "INSERT INTO mod_actions (community_id, moderator_id, action_type, target_id, reason) VALUES ($1, $2, $3, $4, $5)"

  let log_action (module C : Caqti_lwt.CONNECTION) community_id moderator_id action_type target_id reason =
    C.exec log_action_query ((community_id, moderator_id), (action_type, target_id, reason)) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* JOIN on users for the username — avoids a second query at render time; the join
     is cheap since moderator_id is indexed via the FK. *)
  let get_modlog_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* mod_action_row_type)
    "SELECT ma.id, ma.community_id, ma.moderator_id, u.username, ma.action_type, ma.target_id, ma.reason, ma.created_at::text
     FROM mod_actions ma
     JOIN users u ON ma.moderator_id = u.id
     WHERE ma.community_id = $1
     ORDER BY ma.created_at DESC
     LIMIT 100"

  let get_modlog (module C : Caqti_lwt.CONNECTION) community_id =
    C.collect_list get_modlog_query community_id >>= function
    | Ok rows ->
        let actions = List.map (fun ((id, community_id, moderator_id, moderator_username), (action_type, target_id, reason, created_at)) ->
          { id; community_id; moderator_id; moderator_username; action_type; target_id; reason; created_at }
        ) rows in
        Lwt.return (Ok actions)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

module Report = struct
  (* 16-column queue row: t4(t4, t4, t4, t4) stays within Caqti's per-tuple arity limit.
     Enum columns come back as TEXT and are decoded by map_report_row. target_id is int64
     (BIGINT). resolved_at/created_at are ::text-cast like the rest of db.ml. *)
  let report_row_type =
    let open Caqti_type in
    t4
      (t4 int int int string)
      (t4 string int64 (option int) (option string))
      (t4 string (option string) string (option string))
      (t4 (option string) (option int) (option string) string)

  let report_select =
    "SELECT r.id, r.community_id, r.reporter_user_id, reporter.username,
            r.target_type, r.target_id, r.target_author_user_id, author.username,
            r.reason, r.details, r.status, r.action_kind,
            r.resolution_note, r.resolved_by_user_id, r.resolved_at::text, r.created_at::text
     FROM reports r
     JOIN users reporter ON r.reporter_user_id = reporter.id
     LEFT JOIN users author ON r.target_author_user_id = author.id"

  (* CHECK constraints guarantee the enum strings are on-enum, so the *_of_string defaults
     below are unreachable; they exist only to keep decoding total. action_kind is NULL or
     on-enum, hence Option.bind. *)
  let map_report_row
      ((id, community_id, reporter_user_id, reporter_username),
       (target_type_s, target_id, target_author_user_id, target_author_username),
       (reason_s, details, status_s, action_kind_s),
       (resolution_note, resolved_by_user_id, resolved_at, created_at)) =
    {
      id; community_id; reporter_user_id; reporter_username;
      target_type = Option.value (report_target_of_string target_type_s) ~default:Report_post;
      target_id; target_author_user_id; target_author_username;
      reason = Option.value (report_reason_of_string reason_s) ~default:Report_other;
      details;
      status = Option.value (report_status_of_string status_s) ~default:Report_open;
      action_kind = Option.bind action_kind_s report_action_kind_of_string;
      resolution_note; resolved_by_user_id; resolved_at; created_at;
    }

  (* ON CONFLICT DO NOTHING fires against uniq_reports_open_per_reporter_target — the only
     unique index a freshly-generated SERIAL id can collide on — so a second OPEN report by
     the same reporter for the same target inserts nothing and RETURNING yields no row
     (find_opt -> Ok None -> `Duplicate). We use a bare DO NOTHING (no inference clause):
     inferring a PARTIAL index requires repeating its WHERE predicate, and the PK cannot
     collide on a generated id, so bare DO NOTHING is both correct and simpler. *)
  let create_report_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t4 int int string int64) (t3 (option int) string (option string))) ->? Caqti_type.int)
    "INSERT INTO reports
       (community_id, reporter_user_id, target_type, target_id, target_author_user_id, reason, details)
     VALUES ($1, $2, $3, $4, $5, $6, $7)
     ON CONFLICT DO NOTHING
     RETURNING id"

  let create_report (module C : Caqti_lwt.CONNECTION) ~community_id ~reporter_user_id
      ~target_type ~target_id ~target_author_user_id ~reason ~details =
    C.find_opt create_report_query
      ((community_id, reporter_user_id, report_target_to_string target_type, target_id),
       (target_author_user_id, report_reason_to_string reason, details))
    >>= function
    | Ok (Some id) -> Lwt.return (Ok (`Created id))
    | Ok None -> Lwt.return (Ok `Duplicate)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let get_reports_by_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int string) ->* report_row_type)
    (report_select ^ "
     WHERE r.community_id = $1 AND r.status = $2
     ORDER BY r.created_at DESC, r.id DESC")

  let get_reports_by_community (module C : Caqti_lwt.CONNECTION) community_id ~status =
    C.collect_list get_reports_by_community_query (community_id, report_status_to_string status)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_report_row rows))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let get_report_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? report_row_type)
    (report_select ^ " WHERE r.id = $1")

  let get_report_by_id (module C : Caqti_lwt.CONNECTION) report_id =
    C.find_opt get_report_by_id_query report_id >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_report_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let resolve_report_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t3 string (option string) (option string)) (t2 int int)) ->. Caqti_type.unit)
    "UPDATE reports
     SET status = $1, action_kind = $2, resolution_note = $3,
         resolved_by_user_id = $4, resolved_at = CURRENT_TIMESTAMP
     WHERE id = $5"

  let resolve_report (module C : Caqti_lwt.CONNECTION) report_id ~resolver_user_id ~status ~action_kind ~note =
    let action_kind_s = Option.map report_action_kind_to_string action_kind in
    C.exec resolve_report_query
      ((report_status_to_string status, action_kind_s, note), (resolver_user_id, report_id))
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let count_open_reports_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM reports WHERE community_id = $1 AND status = 'open'"

  let count_open_reports (module C : Caqti_lwt.CONNECTION) community_id =
    C.find count_open_reports_query community_id >>= function
    | Ok c -> Lwt.return (Ok c)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

(* DB-backed rate limit trades a synchronous Hashtbl lookup for a round-trip to
   Postgres; the ~1ms I/O penalty is the price of crash resilience and shared
   state across replicas — unavoidable once we move beyond a single process. *)
module Rate_limit = struct
  let max_attempts = 5

  (* Single atomic upsert: resets the window when expired, otherwise increments.
     Hardcoding 60.0 avoids a fourth bind parameter and keeps the query plan stable. *)
  let check_q =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string string float) ->! Caqti_type.int)
    {|INSERT INTO rate_limits (ip_address, endpoint, attempts, window_start)
      VALUES ($1, $2, 1, $3)
      ON CONFLICT (ip_address, endpoint) DO UPDATE
        SET attempts     = CASE WHEN rate_limits.window_start + 60.0 < EXCLUDED.window_start
                                THEN 1
                                ELSE rate_limits.attempts + 1
                           END,
            window_start = CASE WHEN rate_limits.window_start + 60.0 < EXCLUDED.window_start
                                THEN EXCLUDED.window_start
                                ELSE rate_limits.window_start
                           END
      RETURNING attempts|}

  let check (module C : Caqti_lwt.CONNECTION) ip endpoint =
    let now = Unix.gettimeofday () in
    C.find check_q (ip, endpoint, now) >>= function
    | Ok attempts ->
        if attempts > max_attempts then Lwt.return (Ok `Blocked)
        else Lwt.return (Ok `Allowed)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

module Community_user_stats = struct
  (* Upsert-only: first_active_at is set on INSERT and never overwritten — it records
     when the user first contributed, not when they last updated their stats. *)
  let ensure_q =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id) VALUES ($1, $2)
     ON CONFLICT (user_id, community_id) DO NOTHING"

  let ensure_community_user_stats (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec ensure_q (user_id, community_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let inc_post_q =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id, local_post_count)
     VALUES ($1, $2, 1)
     ON CONFLICT (user_id, community_id)
     DO UPDATE SET local_post_count = community_user_stats.local_post_count + 1"

  let increment_local_post_count (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec inc_post_q (user_id, community_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let inc_comment_q =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id, local_comment_count)
     VALUES ($1, $2, 1)
     ON CONFLICT (user_id, community_id)
     DO UPDATE SET local_comment_count = community_user_stats.local_comment_count + 1"

  let increment_local_comment_count (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec inc_comment_q (user_id, community_id) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let update_karma_q =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id, local_karma)
     VALUES ($1, $2, $3)
     ON CONFLICT (user_id, community_id)
     DO UPDATE SET local_karma = community_user_stats.local_karma + $3"

  let update_local_karma (module C : Caqti_lwt.CONNECTION) user_id community_id delta =
    C.exec update_karma_q (user_id, community_id, delta) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  let get_user_community_stats_q =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t3 (t2 string string) (t3 int int int) (option string)))
    {| SELECT c.name, c.slug,
              cus.local_karma,
              cus.local_post_count, cus.local_comment_count,
              cus.first_active_at::text
       FROM community_user_stats cus
       JOIN communities c ON c.id = cus.community_id
       WHERE cus.user_id = $1
         AND (cus.local_post_count + cus.local_comment_count > 0 OR cus.local_karma <> 0)
       ORDER BY (cus.local_post_count + cus.local_comment_count) DESC,
                cus.local_karma DESC,
                c.name ASC |}

  let get_user_community_stats (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_user_community_stats_q user_id >>= function
    | Ok rows ->
        let stats = List.map (fun ((community_name, community_slug), (local_karma, local_post_count, local_comment_count), first_active_at) ->
          { community_name; community_slug; local_karma; local_post_count; local_comment_count; first_active_at }
        ) rows in
        Lwt.return (Ok stats)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
end

(* Zero-cost aliases preserving the flat Db.f call style at handler callsites.
   Use Db.Post.f / Db.User.f etc. for explicit namespacing in new code. *)
let get_all_communities = Community.get_all_communities
let create_community = Community.create_community
let get_community_by_slug = Community.get_community_by_slug
let get_community_by_id = Community.get_community_by_id
let search_communities = Community.search_communities
let update_community_details = Community.update_community_details

let create_user = User.create_user
let user_exists = User.user_exists
let get_user_for_login = User.get_user_for_login
let anonymize_user = User.anonymize_user
let get_user_public = User.get_user_public
let get_user_analytics_props = User.get_user_analytics_props
let update_user_profile = User.update_user_profile
let get_user_karma = User.get_user_karma
let get_user_post_votes = User.get_user_post_votes
let get_user_comment_votes = User.get_user_comment_votes
let search_users = User.search_users
let get_user_by_username = User.get_user_by_username
let get_admin_usernames = User.get_admin_usernames
let is_user_admin = User.is_user_admin

let create_post = Post.create_post
let get_post_by_id = Post.get_post_by_id
let get_all_posts = Post.get_all_posts
let get_personalized_feed = Post.get_personalized_feed
let get_posts_by_community = Post.get_posts_by_community
let get_posts_by_user = Post.get_posts_by_user
let get_post_communities = Post.get_post_communities
let vote_post = Post.vote_post
let remove_post_vote = Post.remove_post_vote
let soft_delete_post = Post.soft_delete_post
let search_posts = Post.search_posts

let get_comments = Comment.get_comments
let create_comment = Comment.create_comment
let touch_post_last_activity = Comment.touch_last_activity
let vote_comment = Comment.vote_comment
let get_comments_by_user = Comment.get_comments_by_user
let remove_comment_vote = Comment.remove_comment_vote
let soft_delete_comment = Comment.soft_delete_comment
let search_comments = Comment.search_comments
let get_comment_report_target = Comment.get_comment_report_target

let create_section = Section.create_section
let get_sections_by_community = Section.get_sections_by_community
let get_section_by_id = Section.get_section_by_id
let get_section_by_slug = Section.get_section_by_slug
let update_section = Section.update_section
let update_section_indexable = Section.update_section_indexable
let delete_section = Section.delete_section
let get_sections_with_stats = Section.get_sections_with_stats
let get_orphaned_count_and_activity = Section.get_orphaned_count_and_activity
let get_orphaned_posts = Section.get_orphaned_posts
let get_posts_by_section = Post.get_posts_by_section

let create_channel = Channel.create_channel
let get_channels_by_community = Channel.get_channels_by_community
let get_channel_by_slug = Channel.get_channel_by_slug
let get_channel_by_id = Channel.get_channel_by_id
let set_channel_archived = Channel.set_channel_archived
let update_channel = Channel.update_channel
let update_channel_indexable = Channel.update_channel_indexable

let send_message = Chat.send_message
let get_message_by_id = Chat.get_message_by_id
let get_recent_messages = Chat.get_recent_messages
let get_recent_messages_with_authors = Chat.get_recent_messages_with_authors
let get_messages_before_id = Chat.get_messages_before_id
let get_messages_after_id = Chat.get_messages_after_id
let edit_message = Chat.edit_message
let soft_delete_message = Chat.soft_delete_message
let get_messages_before_id_with_authors = Chat.get_messages_before_id_with_authors
let get_messages_after_id_with_authors = Chat.get_messages_after_id_with_authors

let get_seed_thread_for_message = ThreadSource.get_seed_thread_for_message
let get_thread_links_for_channel = ThreadSource.get_thread_links_for_channel
let get_thread_source = ThreadSource.get_thread_source
let get_thread_sources_for_posts = ThreadSource.get_thread_sources_for_posts
let get_source_span_for_thread = ThreadSource.get_source_span_for_thread
let start_thread_from_chat = ThreadSource.start_thread_from_chat

let join_community = Membership.join_community
let is_member = Membership.is_member
let leave_community = Membership.leave_community
let get_community_members = Membership.get_community_members
let get_user_communities = Membership.get_user_communities

let add_moderator = Moderator.add_moderator
let add_top_moderator = Moderator.add_top_moderator
let is_moderator = Moderator.is_moderator
let get_community_moderators = Moderator.get_community_moderators
let remove_moderator = Moderator.remove_moderator
let get_moderated_communities = Moderator.get_moderated_communities

(* Prefixed to avoid collision with Admin.ban_user (global site-wide ban). *)
let community_ban_user = Ban.ban_user
let community_unban_user = Ban.unban_user
let community_is_banned = Ban.is_banned
let community_get_banned_users = Ban.get_banned_users

let get_notifications = Notification.get_notifications
let count_unread_notifs = Notification.count_unread_notifs
let mark_notifs_read = Notification.mark_notifs_read
let create_notif = Notification.create_notif
let get_post_owner = Notification.get_post_owner
let get_comment_owner = Notification.get_comment_owner
let get_comment_post_id = Notification.get_comment_post_id

let log_page_view = Analytics.log_page_view
let get_kpi_dashboard = Analytics.get_kpi_dashboard
let get_dau_mau_ratio = Analytics.get_dau_mau_ratio

let touch_user_active = Presence.touch_user_active

let update_password = Security.update_password
let verify_email = Security.verify_email

let password_reset_create_token = PasswordReset.create_token
let password_reset_validate_token = PasswordReset.validate_token
let password_reset_atomically = PasswordReset.reset_password_atomically

let pending_signup_hash_token = PendingSignup.hash_token
let pending_signup_username_elsewhere = PendingSignup.username_pending_elsewhere
let pending_signup_upsert = PendingSignup.upsert
let pending_signup_sweep_expired = PendingSignup.sweep_expired
let pending_signup_confirm = PendingSignup.confirm

let anonymize_user_and_enqueue_posthog_deletion =
  PosthogDeletionJobs.anonymize_and_enqueue
let claim_posthog_deletion_job = PosthogDeletionJobs.claim
let claim_posthog_deletion_batch = PosthogDeletionJobs.claim_batch
let complete_posthog_deletion_job = PosthogDeletionJobs.mark_completed
let fail_posthog_deletion_job = PosthogDeletionJobs.mark_failed
let get_posthog_deletion_job = PosthogDeletionJobs.get_by_distinct_id

let admin_delete_post = Admin.admin_delete_post
let admin_delete_comment = Admin.admin_delete_comment
let mod_delete_post = Admin.mod_delete_post
let mod_delete_comment = Admin.mod_delete_comment
let ban_user = Admin.ban_user
let is_globally_banned = Admin.is_globally_banned
let unban_user_global = Admin.unban_user_global
let get_globally_banned_users = Admin.get_globally_banned_users

let promote_to_top_mod = Moderator.promote_to_top_mod
let get_moderator_role = Moderator.get_moderator_role
let get_community_mods_with_roles = Moderator.get_community_mods_with_roles
let demote_inactive_mods = Moderator.demote_inactive_mods
let log_mod_action = Mod_action.log_action
let get_modlog = Mod_action.get_modlog

let create_report = Report.create_report
let get_reports_by_community = Report.get_reports_by_community
let get_report_by_id = Report.get_report_by_id
let resolve_report = Report.resolve_report
let count_open_reports = Report.count_open_reports
let toggle_community_downvotes = Community.toggle_community_downvotes
let update_community_visibility = Community.update_community_visibility
let update_community_indexable = Community.update_community_indexable
let get_allows_downvotes_for_post = Community.get_allows_downvotes_for_post
let get_allows_downvotes_for_comment = Community.get_allows_downvotes_for_comment

let ensure_community_user_stats = Community_user_stats.ensure_community_user_stats
let increment_local_post_count = Community_user_stats.increment_local_post_count
let increment_local_comment_count = Community_user_stats.increment_local_comment_count
let update_local_karma = Community_user_stats.update_local_karma
let get_user_community_stats = Community_user_stats.get_user_community_stats
