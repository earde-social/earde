open Lwt.Infix

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

(* A global ban revokes every session of the banned user in the SAME
   transaction as the is_banned flip: leaving the rows behind would let an
   already-authenticated browser keep using the site — and keep minting
   fresh realtime tokens — until natural session expiry. Fail-closed: if
   revocation fails the ban rolls back and the caller sees the error, never
   a silent half-success that looks banned but stays logged in. *)
let ban_user (module C: Caqti_lwt.CONNECTION) user_id =
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () ->
    (C.exec ban_user_query user_id >>= function
    | Error e ->
        C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
        (Credential_store.delete_for_user (module C) user_id >>= function
         | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
         | Ok () ->
             C.commit () >>= function
             | Error e -> Lwt.return (Error (Caqti_error.show e))
             | Ok () -> Lwt.return (Ok ())))

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
  | Ok rows -> Lwt.return (Ok (List.map (fun (id, username, email) -> { User_store.id; username; email }) rows))
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
