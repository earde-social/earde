open Lwt.Infix

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

(* 8-column result: nested t2(t4, t4) to stay within Caqti's per-tuple arity limit. *)
let mod_action_row_type =
  let open Caqti_type in
  t2 (t4 int int int string) (t4 string (option int) string string)

let log_action_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int int) (t3 string (option int) string))
  ->. Caqti_type.unit)
    "INSERT INTO mod_actions (community_id, moderator_id, action_type, \
     target_id, reason) VALUES ($1, $2, $3, $4, $5)"

let log_action (module C : Caqti_lwt.CONNECTION) community_id moderator_id
    action_type target_id reason =
  C.exec log_action_query
    ((community_id, moderator_id), (action_type, target_id, reason))
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

(* JOIN on users for the username — avoids a second query at render time; the join
   is cheap since moderator_id is indexed via the FK. *)
let get_modlog_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* mod_action_row_type)
    "SELECT ma.id, ma.community_id, ma.moderator_id, u.username, \
     ma.action_type, ma.target_id, ma.reason, ma.created_at::text\n\
    \   FROM mod_actions ma\n\
    \   JOIN users u ON ma.moderator_id = u.id\n\
    \   WHERE ma.community_id = $1\n\
    \   ORDER BY ma.created_at DESC\n\
    \   LIMIT 100"

let get_modlog (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_modlog_query community_id >>= function
  | Ok rows ->
      let actions =
        List.map
          (fun ( (id, community_id, moderator_id, moderator_username),
                 (action_type, target_id, reason, created_at) ) ->
            {
              id;
              community_id;
              moderator_id;
              moderator_username;
              action_type;
              target_id;
              reason;
              created_at;
            })
          rows
      in
      Lwt.return (Ok actions)
  | Error e -> Lwt.return (Error (Caqti_error.show e))
