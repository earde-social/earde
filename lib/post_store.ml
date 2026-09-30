open Lwt.Infix

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
  (Caqti_type.int ->? Post_types.post_row_type)
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
  Query_timer.with_query_timer ~name:"get_post_by_id" (fun () ->
    C.find_opt get_post_by_id_query id >>= function
    | Ok (Some row) -> Lwt.return (Ok (Some (Post_types.map_post_row row)))
    | Ok None -> Lwt.return (Ok None)
    | Error err -> Lwt.return (Error (Caqti_error.show err))
  )

let get_all_posts (module C : Caqti_lwt.CONNECTION) (sort_mode : Post_types.sort_mode) limit offset =
  Query_timer.with_query_timer ~name:"get_all_posts" (fun () ->
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
      (Caqti_type.(t2 int int) ->* Post_types.post_row_type) query_str
    in
    C.collect_list query (limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map Post_types.map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
  )

(* === Destination-aware community feeds (Shared Threads read side) ===
   Each community-scoped feed is ONE bounded statement: a UNION ALL of the
   community's own canonical posts and the canonical posts holding an
   accepted shared-thread placement into it, with the sort mode and
   LIMIT/OFFSET applied by the outer query to the combined set — never to
   each arm separately, and never merged in OCaml. The two arms are
   structurally disjoint (a placement's destination can never equal the
   post's own community — the schema CHECK), so no canonical post renders
   twice within one destination.

   The placement arm deliberately requires ONLY: accepted status, the
   destination binding, and a currently PUBLIC origin community.
   Connection state, discoverability, and onboarding eligibility gate
   request and acceptance in the store, not continued rendering —
   disconnecting does not silently erase an accepted shared discussion,
   while an origin turning private immediately stops the discussion
   leaking through its destinations. Destination-side privacy is the
   route's own can_view_community gate, exactly as for the community's own
   posts.

   Sort keys (created_at / score / HN hot rank / last_activity_at) are the
   canonical post's, computed per arm and ordered by the outer query, so a
   shared row competes in the destination feed exactly as at home. Each
   arm keeps its own index (posts.community_id; the destination+status
   placement index) — no unbounded placement scan. *)
let feed_sort_clause (sort_mode : Post_types.sort_mode) = match sort_mode with
  | Newest -> "ORDER BY f.sort_created DESC"
  | Top    -> "ORDER BY f.score DESC, f.sort_created DESC"
  | Hot    -> "ORDER BY f.sort_hot DESC"
  | Active -> "ORDER BY f.sort_activity DESC"

(* One arm of a combined feed. The select list is identical in both arms
   (UNION ALL discipline); only the FROM head, the arm's own WHERE
   conditions, the shared-context columns, and the GROUP BY tail differ.
   The post's section_* columns are always the ORIGIN section (cs joins
   p.section_id) — the destination section travels separately as
   shared_section_*, so no field means two different communities. *)
let feed_arm ~from_head ~where_clause ~shared_cols ~group_by_extra =
  Printf.sprintf
    "SELECT p.id AS id, p.title AS title, p.url AS url, p.content AS content,
            p.community_id AS community_id, p.user_id AS user_id,
            u.username AS username, a.slug AS community_slug,
            p.created_at::text AS created_at_text,
            COALESCE(SUM(v.direction), 0) AS score,
            (SELECT COUNT(*) FROM comments c WHERE c.post_id = p.id) AS comment_count,
            a.allow_downvotes AS allow_downvotes, p.image_url AS image_url,
            cs.name AS section_name, cs.slug AS section_slug,
            a.sections_enabled AS sections_enabled,
            COALESCE(MAX(cus.local_karma), 0) AS author_local_karma,
            COALESCE(MAX(cus.local_post_count), 0) AS author_local_post_count,
            COALESCE(MAX(cus.local_comment_count), 0) AS author_local_comment_count,
            MAX(cus.first_active_at)::text AS author_first_active,
            %s,
            p.created_at AS sort_created, p.last_activity_at AS sort_activity,
            (COALESCE(SUM(v.direction), 0) + 1.0) / POWER(EXTRACT(EPOCH FROM (NOW() - p.created_at))/3600.0 + 2.0, 1.5) AS sort_hot
     FROM %s
     JOIN users u ON p.user_id = u.id
     JOIN communities a ON p.community_id = a.id
     LEFT JOIN post_votes v ON p.id = v.post_id
     LEFT JOIN community_sections cs ON cs.id = p.section_id
     LEFT JOIN community_user_stats cus ON cus.user_id = p.user_id AND cus.community_id = p.community_id
     WHERE %s
     GROUP BY p.id, u.username, a.slug, a.allow_downvotes, cs.name, cs.slug, a.sections_enabled%s"
    shared_cols from_head where_clause group_by_extra

let own_arm ~where_clause =
  feed_arm ~from_head:"posts p" ~where_clause
    ~shared_cols:
      "FALSE AS via_placement, NULL::text AS shared_origin_name,
       NULL::text AS shared_section_name, NULL::text AS shared_section_slug"
    ~group_by_extra:""

let placement_arm ~where_clause =
  feed_arm
    ~from_head:
      "shared_thread_placements stp
       JOIN posts p ON p.id = stp.post_id
       LEFT JOIN community_sections ds ON ds.id = stp.destination_section_id"
    ~where_clause:
      ("stp.status = 'accepted' AND a.visibility = 'public' AND " ^ where_clause)
    ~shared_cols:
      "TRUE AS via_placement, a.name AS shared_origin_name,
       ds.name AS shared_section_name, ds.slug AS shared_section_slug"
    ~group_by_extra:", a.name, ds.name, ds.slug"

let combined_feed_query_str ~own_where ~placement_where ~limit_param
    ~offset_param sort_mode =
  Printf.sprintf
    "SELECT id, title, url, content, community_id, user_id, username, community_slug,
            created_at_text, score, comment_count, allow_downvotes, image_url,
            section_name, section_slug, sections_enabled,
            author_local_karma, author_local_post_count, author_local_comment_count,
            author_first_active, via_placement, shared_origin_name,
            shared_section_name, shared_section_slug
     FROM (%s
           UNION ALL
           %s) f
     %s
     LIMIT %s OFFSET %s"
    (own_arm ~where_clause:own_where)
    (placement_arm ~where_clause:placement_where)
    (feed_sort_clause sort_mode) limit_param offset_param

let get_posts_by_community (module C : Caqti_lwt.CONNECTION) community_id (sort_mode : Post_types.sort_mode) limit offset =
  (* Same HN gravity formula as get_all_posts; the sort_mode variant keeps
     every dynamic fragment a hardcoded literal (type-driven injection
     safety). *)
  let query_str =
    combined_feed_query_str
      ~own_where:"p.community_id = $1"
      ~placement_where:"stp.destination_community_id = $1"
      ~limit_param:"$2" ~offset_param:"$3" sort_mode
  in
  let query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->* Post_types.feed_item_row_type) query_str
  in
  C.collect_list query (community_id, limit, offset)
  >>= function
  | Ok rows -> Lwt.return (Ok (List.map Post_types.map_feed_item_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Section feed: the community's own posts in the section, plus canonical
   posts whose accepted placement was accepted INTO this destination
   section. section_id FK enforces community ownership of the own arm in
   SQL; the placement arm binds both the destination community and the
   destination section explicitly. *)
let get_posts_by_section (module C : Caqti_lwt.CONNECTION) community_id section_id (sort_mode : Post_types.sort_mode) limit offset =
  let query_str =
    combined_feed_query_str
      ~own_where:"p.community_id = $1 AND p.section_id = $2"
      ~placement_where:
        "stp.destination_community_id = $1 AND stp.destination_section_id = $2"
      ~limit_param:"$3" ~offset_param:"$4" sort_mode
  in
  let query =
    let open Caqti_request.Infix in
    (Caqti_type.(t4 int int int int) ->* Post_types.feed_item_row_type) query_str
  in
  C.collect_list query (community_id, section_id, limit, offset)
  >>= function
  | Ok rows -> Lwt.return (Ok (List.map Post_types.map_feed_item_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_posts_by_user_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Post_types.post_row_type)
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
  | Ok rows -> Lwt.return (Ok (List.map Post_types.map_post_row rows))
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

(* The community that OWNS a post, read from the post itself. Vote
   authorization needs this: a community id supplied by the browser would let
   a banned user point the ban check at a community they are not banned in.
   Ok None = no such post — the caller decides what that means. *)
let get_post_community_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT community_id FROM posts WHERE id = $1"

let get_post_community_id (module C : Caqti_lwt.CONNECTION) post_id =
  C.find_opt get_post_community_id_query post_id
  >>= function
  | Ok row -> Lwt.return (Ok row)
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
  (Caqti_type.(t3 string int int) ->* Post_types.post_row_type)
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
  | Ok rows -> Lwt.return (Ok (List.map Post_types.map_post_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Personalized feed: identical HN gravity to get_all_posts but filtered to
   communities the user has joined. JOIN on community_members instead of a
   subquery — avoids a correlated scan per post on large datasets. *)
let get_personalized_feed (module C : Caqti_lwt.CONNECTION) user_id (sort_mode : Post_types.sort_mode) limit offset =
  Query_timer.with_query_timer ~name:"get_personalized_feed" (fun () ->
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
      (Caqti_type.(t3 int int int) ->* Post_types.post_row_type) query_str
    in
    C.collect_list query (user_id, limit, offset)
    >>= function
    | Ok rows -> Lwt.return (Ok (List.map Post_types.map_post_row rows))
    | Error err -> Lwt.return (Error (Caqti_error.show err))
  )
