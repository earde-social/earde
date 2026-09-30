open Lwt.Infix

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
      let users = List.map (fun (id, username, email) -> { User_store.id; username; email }) rows in
      Lwt.return (Ok users)
  | Error err -> Lwt.return (Error (Caqti_error.show err))
