open Lwt.Infix

type user = {
  id : int;
  username : string;
  email : string;
}

(* Nested 7-column row: (id, username, email, created_at), (hash, is_admin,
   is_banned). created_at rides the same lookup so a successful login has the
   closed analytics person properties with no extra query. *)
let get_user_for_login_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t2 (t4 int string string string) (t3 string bool bool)))
  "SELECT id, username, email, created_at::text, password_hash, is_admin, is_banned FROM users WHERE username = $1 OR email = $1"

let get_user_for_login (module C: Caqti_lwt.CONNECTION) identifier =
  Query_timer.with_query_timer ~name:"get_user_for_login" (fun () ->
    C.find_opt get_user_for_login_query identifier >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
  )

(* GDPR Art. 17 (right to erasure): scrub PII from the row, preserve post/comment rows for
   thread coherence. Tombstone [deleted_N] prevents username recycling after deletion.
   bio/avatar_url are user-authored profile data and must not survive the account —
   the settings page and /privacy both promise their removal. *)
let anonymize_user_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE users
   SET username = '[deleted_' || id || ']',
       email = 'deleted_' || id || '@earde.local',
       password_hash = '',
       bio = NULL,
       avatar_url = NULL
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

(* Narrow pre-deletion lookup: the avatar_url must be read BEFORE the
   anonymize rewrite NULLs it, so the account-deletion handler can remove
   the locally stored upload after the transaction commits. *)
let get_user_avatar_url_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.(option string))
  "SELECT avatar_url FROM users WHERE id = $1"

let get_user_avatar_url (module C : Caqti_lwt.CONNECTION) user_id =
  C.find_opt get_user_avatar_url_query user_id
  >>= function
  | Ok (Some avatar_url) -> Lwt.return (Ok avatar_url)
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

(* Presence is operational state, not analytics: last_active_at is read by
   Moderator.demote_inactive_mods, so this must survive any replacement of the
   page-view analytics system. *)
let touch_user_active_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE users SET last_active_at = CURRENT_TIMESTAMP WHERE id = $1"

let touch_user_active (module C: Caqti_lwt.CONNECTION) user_id =
  C.exec touch_user_active_query user_id >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))
