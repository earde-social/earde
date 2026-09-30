open Lwt.Infix

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
   later consumers (e.g. analytics) never need a second lookup query.

   The INSERT ... SELECT ... WHERE is the parent-binding guard. The schema's
   only constraint on parent_id is a foreign key to comments(id) — nothing
   ties the parent to the post being commented on — so a submitted
   parent_id used to be accepted from ANY post in ANY community, including
   a private one the author cannot read. That produced durable corruption
   (render_comment_tree walks down from parent_id = None, so such a row is
   stored, counted, and never displayed) and let a reply notification be
   aimed at the owner of a comment in a community the sender cannot see.

   Enforcing it in the mutation's own WHERE rather than in a preceding
   SELECT closes the TOCTOU window: the parent's post_id is read under the
   same statement that writes the row, so a concurrent change cannot land
   between the check and the insert. Zero rows inserted (find_opt returns
   None) means the parent failed the binding — the only way the predicate
   can reject — and the caller must treat it as a client error. A parent
   that does not exist at all still fails here rather than reaching the
   foreign key, so no constraint name can leak into a response. *)
let create_comment_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t4 string int int (option int)) ->? Caqti_type.int)
  {|INSERT INTO comments (content, post_id, user_id, parent_id)
    SELECT $1::text, $2::int, $3::int, $4::int
    WHERE $4::int IS NULL
       OR EXISTS (SELECT 1 FROM comments parent
                   WHERE parent.id = $4::int AND parent.post_id = $2::int)
    RETURNING id|}

let create_comment (module C : Caqti_lwt.CONNECTION) content post_id user_id parent_id =
  C.find_opt create_comment_query (content, post_id, user_id, parent_id)
  >>= function
  | Ok (Some comment_id) -> Lwt.return (Ok (`Created comment_id))
  | Ok None -> Lwt.return (Ok `Invalid_parent)
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

(* The community that owns a comment, resolved through its CANONICAL parent
   post — the same binding create_comment_handler authorizes against, and the
   same one the vote SQL below credits karma to. A shared thread is readable
   from a destination community, but the comment never leaves the origin, so
   the destination context a browser happens to be in must not decide ban
   state. Ok None = no such comment (or its post is gone). *)
let get_comment_community_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.int)
  "SELECT p.community_id FROM comments c JOIN posts p ON p.id = c.post_id WHERE c.id = $1"

let get_comment_community_id (module C : Caqti_lwt.CONNECTION) comment_id =
  C.find_opt get_comment_community_id_query comment_id
  >>= function
  | Ok row -> Lwt.return (Ok row)
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
