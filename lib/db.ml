open Lwt.Infix

type community = {
  id : int;
  slug : string;
  name : string;
  description : string option;
  rules : string option;
  avatar_url : string option;
  banner_url : string option;
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
}

type comment = {
  id : int;
  content : string;
  username : string;
  created_at : string;
  score : int;
  parent_id : int option;
  avatar_url : string option;
}

type notification = {
  id : int;
  user_id : int;
  post_id : int;
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

(* post has 11 SELECT columns; Caqti tN stops at t7, so nest tuples to stay within arity. *)
let post_row_type =
  let open Caqti_type in
  t3
    (t4 int string (option string) (option string))
    (t4 int int string string)
    (t3 string int int)

let map_post_row ((id, title, url, content), (community_id, user_id, username, community_slug), (created_at, score, comment_count)) =
  { id; title; url; content; community_id; user_id; username; community_slug; created_at; score; comment_count }

(* 7-column community row: nest into t2(t4, t3) to stay within Caqti's per-tuple arity limit. *)
let community_row_type =
  let open Caqti_type in
  t2 (t4 int string string (option string)) (t3 (option string) (option string) (option string))

let map_community_row ((id, slug, name, description), (rules, avatar_url, banner_url)) =
  { id; slug; name; description; rules; avatar_url; banner_url }

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
    "SELECT id, slug, name, description, rules, avatar_url, banner_url FROM communities"

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
    (Caqti_type.(t3 string string (option string)) ->. Caqti_type.unit)
    "INSERT INTO communities (name, slug, description) VALUES ($1, $2, $3)"

  let create_community (module C : Caqti_lwt.CONNECTION) name slug description =
    C.exec create_community_query (name, slug, description)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_by_slug_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url FROM communities WHERE slug = $1"

  let get_community_by_slug (module C : Caqti_lwt.CONNECTION) slug =
    C.find_opt get_by_slug_query slug
    >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (map_community_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let search_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 string int int) ->* community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url FROM communities WHERE name ILIKE $1 OR description ILIKE $1 ORDER BY name ASC LIMIT $2 OFFSET $3"

  let search_communities (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_communities_query (term, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_community_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  (* Single UPDATE covers all editable fields — no partial-update complexity;
     settings form always submits all four fields so overwrite is safe. *)
  let update_community_details_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 (t4 (option string) (option string) (option string) (option string)) int) ->. Caqti_type.unit)
    "UPDATE communities SET description = $1, rules = $2, avatar_url = $3, banner_url = $4 WHERE id = $5"

  let update_community_details (module C : Caqti_lwt.CONNECTION) community_id description rules avatar_url banner_url =
    C.exec update_community_details_query ((description, rules, avatar_url, banner_url), community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
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

  let get_user_for_login_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.(t6 int string string string bool bool))
    "SELECT id, username, email, password_hash, is_admin, is_banned FROM users WHERE username = $1 OR email = $1"

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
    (Caqti_type.(t4 string (option string) (option string) (t2 int int)) ->. Caqti_type.unit)
    "INSERT INTO posts (title, url, content, community_id, user_id) VALUES ($1, $2, $3, $4, $5)"

  let create_post (module C : Caqti_lwt.CONNECTION) title url content community_id user_id =
    C.exec create_post_query (title, url, content, (community_id, user_id)) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_post_by_id_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->? post_row_type)
    "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
            COALESCE((SELECT SUM(direction) FROM post_votes WHERE post_id = p.id), 0) AS score,
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count
     FROM posts p JOIN users u ON p.user_id = u.id JOIN communities a ON p.community_id = a.id
     WHERE p.id = $1"

  let get_post_by_id (module C : Caqti_lwt.CONNECTION) id =
    with_query_timer ~name:"get_post_by_id" (fun () ->
      C.find_opt get_post_by_id_query id >>= function
      | Ok (Some row) -> Lwt.return (Ok (Some (map_post_row row)))
      | Ok None -> Lwt.return (Ok None)
      | Error err -> Lwt.return (Error (Caqti_error.show err))
    )

  let get_all_posts (module C : Caqti_lwt.CONNECTION) sort_mode limit offset =
    with_query_timer ~name:"get_all_posts" (fun () ->
      (* Hot: HN gravity — (score+1)/(age_hours+2)^1.5. Exponent 1.5 decays faster than
         Reddit's 1.8, favouring freshness. sort_mode is whitelist-validated by the handler
         before reaching here, so Printf.sprintf is safe against SQL injection. *)
      let order_clause = match sort_mode with
        | "new" -> "ORDER BY p.created_at DESC"
        | "top" -> "ORDER BY score DESC, p.created_at DESC"
        | _ -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
      in
      let query_str = Printf.sprintf
        "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
                COALESCE(SUM(v.direction), 0) as score,
                (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count
         FROM posts p
         JOIN users u ON p.user_id = u.id
         JOIN communities a ON p.community_id = a.id
         LEFT JOIN post_votes v ON p.id = v.post_id
         GROUP BY p.id, u.username, a.slug
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

  let get_posts_by_community (module C : Caqti_lwt.CONNECTION) community_id sort_mode limit offset =
    (* Same HN gravity formula and whitelist-validated sort_mode as get_all_posts. *)
    let order_clause = match sort_mode with
      | "new" -> "ORDER BY p.created_at DESC"
      | "top" -> "ORDER BY score DESC, p.created_at DESC"
      | _ -> "ORDER BY (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) DESC"
    in
    let query_str = Printf.sprintf
      "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
              COALESCE(SUM(v.direction), 0) as score,
              (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) as comment_count
       FROM posts p
       JOIN users u ON p.user_id = u.id
       JOIN communities a ON p.community_id = a.id
       LEFT JOIN post_votes v ON p.id = v.post_id
       WHERE p.community_id = $1
       GROUP BY p.id, u.username, a.slug
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

  let get_posts_by_user_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* post_row_type)
    "SELECT p.id, p.title, p.url, p.content, p.community_id, p.user_id, u.username, a.slug, p.created_at::text,
            COALESCE((SELECT SUM(direction) FROM post_votes WHERE post_id = p.id), 0) AS score,
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count
     FROM posts p JOIN users u ON p.user_id = u.id JOIN communities a ON p.community_id = a.id
     WHERE p.user_id = $1 ORDER BY score DESC, p.created_at DESC"

  let get_posts_by_user (module C : Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_posts_by_user_query user_id >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let vote_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
    "INSERT INTO post_votes (user_id, post_id, direction) VALUES ($1, $2, $3)
     ON CONFLICT (user_id, post_id) DO UPDATE SET direction = EXCLUDED.direction"

  let vote_post (module C : Caqti_lwt.CONNECTION) user_id post_id direction =
    C.exec vote_post_query (user_id, post_id, direction) >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let remove_post_vote_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM post_votes WHERE user_id = $1 AND post_id = $2"

  let remove_post_vote (module C : Caqti_lwt.CONNECTION) user_id post_id =
    C.exec remove_post_vote_query (user_id, post_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let soft_delete_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.t2 Caqti_type.int Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posts SET content = '[deleted]', url = NULL WHERE id = $1 AND user_id = $2"

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
            (SELECT COUNT(*) FROM comments WHERE post_id = p.id) AS comment_count
     FROM posts p
     JOIN users u ON p.user_id = u.id
     JOIN communities a ON p.community_id = a.id
     LEFT JOIN post_votes v ON p.id = v.post_id
     WHERE p.title ILIKE $1 OR p.content ILIKE $1
     GROUP BY p.id, u.username, a.slug
     ORDER BY score DESC, p.created_at DESC LIMIT $2 OFFSET $3"

  let search_posts (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_posts_query (term, limit, offset) >>= function
    | Ok rows -> Lwt.return (Ok (List.map map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
end

module Comment = struct
  (* Wilson score lower bound (z=1.96, 95% CI) orders comments by quality under low vote counts.
     Raw SUM(direction) would surface polarising comments; Wilson penalises low-sample confidence. *)
  let get_comments_by_post_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* Caqti_type.(t7 int string string string int (option int) (option string)))
    "SELECT c.id, c.content, u.username, c.created_at::text, COALESCE(SUM(v.direction), 0) AS score, c.parent_id, u.avatar_url
     FROM comments c
     JOIN users u ON c.user_id = u.id
     LEFT JOIN comment_votes v ON c.id = v.comment_id
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
        let comments = List.map (fun (id, content, username, created_at, score, parent_id, avatar_url) ->
          { id; content; username; created_at; score; parent_id; avatar_url }
        ) rows in
        Lwt.return (Ok comments)
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let create_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t4 string int int (option int)) ->. Caqti_type.unit)
    "INSERT INTO comments (content, post_id, user_id, parent_id) VALUES ($1, $2, $3, $4)"

  let create_comment (module C : Caqti_lwt.CONNECTION) content post_id user_id parent_id =
    C.exec create_comment_query (content, post_id, user_id, parent_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let vote_comment_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
    "INSERT INTO comment_votes (user_id, comment_id, direction) VALUES ($1, $2, $3)
     ON CONFLICT (user_id, comment_id) DO UPDATE SET direction = EXCLUDED.direction"

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
    "DELETE FROM comment_votes WHERE user_id = $1 AND comment_id = $2"

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
     LEFT JOIN comment_votes v ON c.id = v.comment_id
     WHERE c.content ILIKE $1
     GROUP BY c.id, u.username, c.created_at, c.post_id
     ORDER BY score DESC, c.created_at DESC LIMIT $2 OFFSET $3"

  let search_comments (module C : Caqti_lwt.CONNECTION) search_term limit offset =
    let term = "%" ^ search_term ^ "%" in
    C.collect_list search_comments_query (term, limit, offset) >>= function
    | Ok rows -> Lwt.return (Ok rows)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
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

  let leave_community_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_members WHERE user_id = $1 AND community_id = $2"

  let leave_community (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.exec leave_community_query (user_id, community_id)
    >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error err -> Lwt.return (Error (Caqti_error.show err))

  let get_user_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* community_row_type)
    "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, a.banner_url
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

  let promote_to_top_mod (module C : Caqti_lwt.CONNECTION) user_id community_id =
    C.find count_top_mods_query community_id >>= function
    | Error e -> Lwt.return (Error (Caqti_error.show e))
    | Ok count ->
        if count >= 3 then Lwt.return (Error "Maximum of 3 Top Mods reached")
        else
          C.exec promote_to_top_mod_query (user_id, community_id) >>= function
          | Ok () -> Lwt.return (Ok ())
          | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Inverse of get_community_moderators: used for profile badge display.
     ORDER BY promoted_at ASC keeps creation order consistent with mod panels. *)
  let get_moderated_communities_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->* community_row_type)
    "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, a.banner_url
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
    (Caqti_type.int ->* Caqti_type.(t6 int int int string bool string))
    "SELECT id, user_id, post_id, message, is_read, created_at::text FROM notifications WHERE user_id = $1 ORDER BY created_at DESC LIMIT 50"

  let get_notifications (module C: Caqti_lwt.CONNECTION) user_id =
    C.collect_list get_notifications_query user_id >>= function
    | Ok rows -> Lwt.return (Ok (List.map (fun (id, user_id, post_id, message, is_read, created_at) -> {id; user_id; post_id; message; is_read; created_at}) rows))
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
    (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
    "INSERT INTO notifications (user_id, post_id, message) VALUES ($1, $2, $3)"

  let create_notif (module C: Caqti_lwt.CONNECTION) user_id post_id message =
    C.exec create_notif_query (user_id, post_id, message) >>= function
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

  let touch_user_active_query =
    let open Caqti_request.Infix in
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET last_active_at = CURRENT_TIMESTAMP WHERE id = $1"

  let touch_user_active (module C: Caqti_lwt.CONNECTION) user_id =
    C.exec touch_user_active_query user_id >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* Caqti arity limit: encode 6 page-view columns as t2(t3, t3) nested inside the outer t4.
     Unique visitors use COUNT(DISTINCT session_hash) — daily-rotating hash guarantees GDPR
     pseudonymisation while still giving investor-grade UV metrics. *)
  let kpi_stats_type =
    let open Caqti_type in
    t4 (t2 (t3 int int int) (t3 int int int)) (t3 int int int) (t3 int int int) (t3 int int int)

  let get_kpi_dashboard_query =
    let open Caqti_request.Infix in
    (Caqti_type.unit ->! kpi_stats_type)
    "SELECT
      (SELECT COUNT(*)::int FROM page_views WHERE created_at > NOW() - INTERVAL '1 day'),
      (SELECT COUNT(*)::int FROM page_views WHERE created_at > NOW() - INTERVAL '7 days'),
      (SELECT COUNT(*)::int FROM page_views WHERE created_at > NOW() - INTERVAL '30 days'),
      (SELECT COUNT(DISTINCT session_hash)::int FROM page_views WHERE created_at > NOW() - INTERVAL '1 day'),
      (SELECT COUNT(DISTINCT session_hash)::int FROM page_views WHERE created_at > NOW() - INTERVAL '7 days'),
      (SELECT COUNT(DISTINCT session_hash)::int FROM page_views WHERE created_at > NOW() - INTERVAL '30 days'),

      (SELECT COUNT(*)::int FROM users WHERE created_at > NOW() - INTERVAL '1 day'),
      (SELECT COUNT(*)::int FROM users WHERE created_at > NOW() - INTERVAL '7 days'),
      (SELECT COUNT(*)::int FROM users WHERE created_at > NOW() - INTERVAL '30 days'),

      (SELECT COUNT(*)::int FROM posts WHERE created_at > NOW() - INTERVAL '1 day') + (SELECT COUNT(*)::int FROM comments WHERE created_at > NOW() - INTERVAL '1 day'),
      (SELECT COUNT(*)::int FROM posts WHERE created_at > NOW() - INTERVAL '7 days') + (SELECT COUNT(*)::int FROM comments WHERE created_at > NOW() - INTERVAL '7 days'),
      (SELECT COUNT(*)::int FROM posts WHERE created_at > NOW() - INTERVAL '30 days') + (SELECT COUNT(*)::int FROM comments WHERE created_at > NOW() - INTERVAL '30 days'),

      (SELECT COUNT(*)::int FROM users WHERE last_active_at > NOW() - INTERVAL '1 day'),
      (SELECT COUNT(*)::int FROM users WHERE last_active_at > NOW() - INTERVAL '7 days'),
      (SELECT COUNT(*)::int FROM users WHERE last_active_at > NOW() - INTERVAL '30 days')"

  let get_kpi_dashboard (module C: Caqti_lwt.CONNECTION) =
    C.find get_kpi_dashboard_query () >>= function
    | Ok res -> Lwt.return (Ok res)
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
    "UPDATE posts SET content = $1, url = NULL WHERE id = $2"

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
end

module PasswordReset = struct
  (* INSERT...SELECT atomically creates the token iff the email maps to a live user.
     RETURNING distinguishes "email not found" from "inserted" without a second SELECT,
     avoiding TOCTOU between the lookup and the insert. *)
  let create_query =
    let open Caqti_request.Infix in
    (Caqti_type.(t2 string string) ->? Caqti_type.int)
    "INSERT INTO password_resets (token, user_id, expires_at)
     SELECT $1, id, NOW() + INTERVAL '2 hours' FROM users WHERE email = $2
     RETURNING user_id"

  let create_token (module C : Caqti_lwt.CONNECTION) email token =
    C.find_opt create_query (token, email) >>= function
    | Ok res -> Lwt.return (Ok (Option.is_some res))
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* expires_at > NOW() makes tokens inert after 2h with no background job required. *)
  let validate_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.int)
    "SELECT user_id FROM password_resets WHERE token = $1 AND expires_at > NOW()"

  let validate_token (module C : Caqti_lwt.CONNECTION) token =
    C.find_opt validate_query token >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))

  (* DELETE+RETURNING atomically consumes the token — prevents replay if the
     subsequent password UPDATE fails; user must re-request a fresh reset. *)
  let consume_query =
    let open Caqti_request.Infix in
    (Caqti_type.string ->? Caqti_type.int)
    "DELETE FROM password_resets WHERE token = $1 AND expires_at > NOW() RETURNING user_id"

  let consume_token (module C : Caqti_lwt.CONNECTION) token =
    C.find_opt consume_query token >>= function
    | Ok res -> Lwt.return (Ok res)
    | Error e -> Lwt.return (Error (Caqti_error.show e))
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

(* Zero-cost aliases preserving the flat Db.f call style at handler callsites.
   Use Db.Post.f / Db.User.f etc. for explicit namespacing in new code. *)
let get_all_communities = Community.get_all_communities
let create_community = Community.create_community
let get_community_by_slug = Community.get_community_by_slug
let search_communities = Community.search_communities
let update_community_details = Community.update_community_details

let create_user = User.create_user
let get_user_for_login = User.get_user_for_login
let anonymize_user = User.anonymize_user
let get_user_public = User.get_user_public
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
let get_posts_by_community = Post.get_posts_by_community
let get_posts_by_user = Post.get_posts_by_user
let vote_post = Post.vote_post
let remove_post_vote = Post.remove_post_vote
let soft_delete_post = Post.soft_delete_post
let search_posts = Post.search_posts

let get_comments = Comment.get_comments
let create_comment = Comment.create_comment
let vote_comment = Comment.vote_comment
let get_comments_by_user = Comment.get_comments_by_user
let remove_comment_vote = Comment.remove_comment_vote
let soft_delete_comment = Comment.soft_delete_comment
let search_comments = Comment.search_comments

let join_community = Membership.join_community
let is_member = Membership.is_member
let leave_community = Membership.leave_community
let get_user_communities = Membership.get_user_communities

let add_moderator = Moderator.add_moderator
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

let log_page_view = Analytics.log_page_view
let touch_user_active = Analytics.touch_user_active
let get_kpi_dashboard = Analytics.get_kpi_dashboard

let update_password = Security.update_password
let verify_email = Security.verify_email

let password_reset_create_token = PasswordReset.create_token
let password_reset_validate_token = PasswordReset.validate_token
let password_reset_consume_token = PasswordReset.consume_token

let admin_delete_post = Admin.admin_delete_post
let admin_delete_comment = Admin.admin_delete_comment
let mod_delete_post db post_id = Admin.admin_delete_post db ~label:"[removed by moderator]" post_id
let mod_delete_comment db comment_id = Admin.admin_delete_comment db ~label:"[removed by moderator]" comment_id
let ban_user = Admin.ban_user
let is_globally_banned = Admin.is_globally_banned
let unban_user_global = Admin.unban_user_global
let get_globally_banned_users = Admin.get_globally_banned_users

let promote_to_top_mod = Moderator.promote_to_top_mod
let log_mod_action = Mod_action.log_action
let get_modlog = Mod_action.get_modlog
