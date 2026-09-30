open Lwt.Infix

type community_user_stat = {
  community_name : string;
  community_slug : string;
  local_karma : int;
  local_post_count : int;
  local_comment_count : int;
  first_active_at : string option;
}

let inc_post_q =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id, local_post_count)\n\
    \   VALUES ($1, $2, 1)\n\
    \   ON CONFLICT (user_id, community_id)\n\
    \   DO UPDATE SET local_post_count = community_user_stats.local_post_count \
     + 1"

let increment_local_post_count (module C : Caqti_lwt.CONNECTION) user_id
    community_id =
  C.exec inc_post_q (user_id, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let inc_comment_q =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_user_stats (user_id, community_id, \
     local_comment_count)\n\
    \   VALUES ($1, $2, 1)\n\
    \   ON CONFLICT (user_id, community_id)\n\
    \   DO UPDATE SET local_comment_count = \
     community_user_stats.local_comment_count + 1"

let increment_local_comment_count (module C : Caqti_lwt.CONNECTION) user_id
    community_id =
  C.exec inc_comment_q (user_id, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let get_user_community_stats_q =
  let open Caqti_request.Infix in
  (Caqti_type.int
  ->* Caqti_type.(t3 (t2 string string) (t3 int int int) (option string)))
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
      let stats =
        List.map
          (fun ( (community_name, community_slug),
                 (local_karma, local_post_count, local_comment_count),
                 first_active_at ) ->
            {
              community_name;
              community_slug;
              local_karma;
              local_post_count;
              local_comment_count;
              first_active_at;
            })
          rows
      in
      Lwt.return (Ok stats)
  | Error e -> Lwt.return (Error (Caqti_error.show e))
