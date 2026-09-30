open Lwt.Infix

let join_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2) ON \
     CONFLICT DO NOTHING"

let join_community (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.exec join_community_query (user_id, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let is_member_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT 1 FROM community_members WHERE user_id = $1 AND community_id = $2"

let is_member (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.find_opt is_member_query (user_id, community_id) >>= function
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
    "DELETE FROM community_members WHERE user_id = $1 AND community_id = $2 \
     RETURNING community_id"

let leave_community (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.find_opt leave_community_query (user_id, community_id) >>= function
  | Ok (Some _) -> Lwt.return (Ok true)
  | Ok None -> Lwt.return (Ok false)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* The allow-list for a private community, rendered as the member-management list on
   /c/:slug/settings. Mirrors Ban.get_banned_users. community_members has no timestamp column,
   so order by username. Mods/admins are NOT necessarily here (they read via their role) — this
   lists membership rows only, which is exactly what add/remove manage. *)
let get_community_members_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t3 int string string))
    "SELECT u.id, u.username, u.email\n\
    \   FROM users u\n\
    \   JOIN community_members cm ON u.id = cm.user_id\n\
    \   WHERE cm.community_id = $1\n\
    \   ORDER BY u.username ASC"

let get_community_members (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_community_members_query community_id >>= function
  | Ok rows ->
      let users =
        List.map
          (fun (id, username, email) -> { User_store.id; username; email })
          rows
      in
      Lwt.return (Ok users)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_user_communities_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Community_types.community_row_type)
    (* All fourteen community_row_type columns: the lifecycle trio
     (is_network_community, onboarding_state, discoverable) was added to the
     shared row type without being added here, so a user with at least one
     membership decoded past the end of the result and the driver raised
     Postgresql.Error out of Dream.sql. Zero rows never decode, which is why
     it stayed hidden. *)
    "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, \
     a.banner_url, a.allow_downvotes, a.sections_enabled, a.visibility, \
     a.indexable, a.is_network_community, a.onboarding_state, a.discoverable\n\
    \   FROM communities a\n\
    \   JOIN community_members am ON a.id = am.community_id\n\
    \   WHERE am.user_id = $1\n\
    \   ORDER BY a.name ASC"

let get_user_communities (module C : Caqti_lwt.CONNECTION) user_id =
  C.collect_list get_user_communities_query user_id >>= function
  | Ok rows -> Lwt.return (Ok (List.map Community_types.map_community_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))
