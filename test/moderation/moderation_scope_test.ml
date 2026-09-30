(* === Cross-community moderation scoping (security regression) ===
   A moderator of community A must not be able to tombstone content in community
   B by forging the numeric id on an A-scoped route. The enforcement lives in the
   SQL of Db.mod_delete_post / Db.mod_delete_comment (id AND community scope,
   RETURNING as match proof), so only a DB-backed test can catch a regression —
   a pure helper test cannot see a missing `AND community_id`.

   Opt-in: set EARDE_TEST_DATABASE_URL to a scratch/dev Postgres to run these;
   without it each case skips, keeping the default `dune test` DB-free. Fixture
   rows use fixed modscope_* names and are removed before and after each case, so
   reruns are idempotent even after a crash. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

(* Fixtures — fixed names so cleanup is targeted and idempotent. One request
   per statement: postgres prepared statements reject multi-command strings. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE title LIKE 'modscope %')"
    ; "DELETE FROM posts WHERE title LIKE 'modscope %'"
    ; "DELETE FROM communities WHERE slug IN ('modscope-a', 'modscope-b')"
    ; "DELETE FROM users WHERE username IN ('modscope_author', 'modscope_other')"
    ]

let q_insert_user =
  (Caqti_type.unit ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ('modscope_author', 'modscope_author@test.invalid', 'x', TRUE) RETURNING id"

(* A second, non-author user: stands in for a moderator of community A
   attacking through the general /delete-comment path. *)
let q_insert_other_user =
  (Caqti_type.unit ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ('modscope_other', 'modscope_other@test.invalid', 'x', TRUE) RETURNING id"

let q_insert_community =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO communities (slug, name) VALUES ($1, $1) RETURNING id"

let q_insert_post =
  (Caqti_type.(t4 string (option string) int int) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, image_url, community_id, user_id)
   VALUES ($1, 'modscope original post', $2, $3, $4) RETURNING id"

let q_insert_comment =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "INSERT INTO comments (content, post_id, user_id)
   VALUES ('modscope original comment', $1, $2) RETURNING id"

let q_post_state =
  (Caqti_type.int ->! Caqti_type.(t2 (option string) (option string)))
  "SELECT content, image_url FROM posts WHERE id = $1"

let q_comment_content =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT content FROM comments WHERE id = $1"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

(* Each case gets a fresh connection and a clean fixture slate; cleanup runs
   again afterwards even when an assertion fails mid-way. *)
let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f conn (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* Two communities, one author, a post (with image) in each. *)
let setup_posts (module C : Caqti_lwt.CONNECTION) =
  let* a = C.find q_insert_community "modscope-a" in
  let* a = or_fail "community a" a in
  let* b = C.find q_insert_community "modscope-b" in
  let* b = or_fail "community b" b in
  let* author = C.find q_insert_user () in
  let* author = or_fail "author" author in
  let* post_a = C.find q_insert_post ("modscope post a", Some "/static/uploads/modscope_a.webp", a, author) in
  let* post_a = or_fail "post a" post_a in
  let* post_b = C.find q_insert_post ("modscope post b", Some "/static/uploads/modscope_b.webp", b, author) in
  let* post_b = or_fail "post b" post_b in
  Lwt.return (a, b, author, post_a, post_b)

let check_post_state (module C : Caqti_lwt.CONNECTION) label post_id expected =
  let* st = C.find q_post_state post_id in
  let* st = or_fail label st in
  Alcotest.(check (pair (option string) (option string))) label expected st;
  Lwt.return_unit

(* Posts: forging a B post id on an A-scoped delete must not match — the same
   DB call serves moderators and admins on community routes, so this also pins
   the admin-on-wrong-route rejection. A genuine A deletion still works and
   reports the match that entitles the handler to exactly one modlog entry. *)
let posts_case =
  db_case "cross-community post delete refused; local delete works" (fun conn c ->
      let* (a, _b, _author, post_a, post_b) = setup_posts c in
      let* cross = Earde.Db.mod_delete_post conn ~community_id:a post_b in
      Alcotest.(check (result bool string)) "A-scoped delete of B post matches nothing" (Ok false) cross;
      let* () = check_post_state c "B post untouched, image_url intact" post_b
          (Some "modscope original post", Some "/static/uploads/modscope_b.webp") in
      let* local = Earde.Db.mod_delete_post conn ~community_id:a post_a in
      Alcotest.(check (result bool string)) "A-scoped delete of A post matches" (Ok true) local;
      check_post_state c "A post tombstoned, image_url cleared" post_a
        (Some "[removed by moderator]", None))

(* Comments: ownership is comment -> post -> community; the scoped UPDATE joins
   posts, so a comment under a B post must not match on an A-scoped call. *)
let comments_case =
  db_case "cross-community comment delete refused; local delete works" (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* (a, _b, author, post_a, post_b) = setup_posts c in
      let* comment_a = C.find q_insert_comment (post_a, author) in
      let* comment_a = or_fail "comment a" comment_a in
      let* comment_b = C.find q_insert_comment (post_b, author) in
      let* comment_b = or_fail "comment b" comment_b in
      let* cross = Earde.Db.mod_delete_comment conn ~community_id:a comment_b in
      Alcotest.(check (result (option int) string)) "A-scoped delete of B comment matches nothing" (Ok None) cross;
      let* content = C.find q_comment_content comment_b in
      let* content = or_fail "B comment state" content in
      Alcotest.(check string) "B comment untouched" "modscope original comment" content;
      let* local = Earde.Db.mod_delete_comment conn ~community_id:a comment_a in
      Alcotest.(check (result (option int) string)) "A-scoped delete of A comment matches, returns parent post"
        (Ok (Some post_a)) local;
      let* content = C.find q_comment_content comment_a in
      let* content = or_fail "A comment state" content in
      Alcotest.(check string) "A comment tombstoned" "[removed by moderator]" content;
      Lwt.return_unit)

(* /delete-comment data path: the general endpoint is author-only for
   non-admins, and its SQL is ownership-scoped (id AND user_id) — so a
   moderator of community A attacking B's comment reaches (at most)
   soft_delete_comment with a requester id that is not the owner, which must
   match nothing regardless of any community value the client sends. The
   admin path resolves the target server-side and tombstones with the admin
   label. *)
let delete_comment_case =
  db_case "delete-comment: ownership-scoped soft delete; admin path tombstones" (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* (_a, _b, author, post_a, post_b) = setup_posts c in
      let* other = C.find q_insert_other_user () in
      let* other = or_fail "other user" other in
      let* comment_b = C.find q_insert_comment (post_b, author) in
      let* comment_b = or_fail "comment b" comment_b in
      (* Non-author (e.g. a mod of A) against B's comment: zero rows match.
         soft_delete_comment reports Ok () either way — the ownership scope in
         the SQL is what this pins down. *)
      let* r = Earde.Db.soft_delete_comment conn comment_b other in
      Alcotest.(check (result unit string)) "non-author soft delete does not error" (Ok ()) r;
      let* content = C.find q_comment_content comment_b in
      let* content = or_fail "B comment state" content in
      Alcotest.(check string) "non-author soft delete matches nothing" "modscope original comment" content;
      (* The author still deletes their own comment. *)
      let* comment_a = C.find q_insert_comment (post_a, author) in
      let* comment_a = or_fail "comment a" comment_a in
      let* r = Earde.Db.soft_delete_comment conn comment_a author in
      Alcotest.(check (result unit string)) "author soft delete ok" (Ok ()) r;
      let* content = C.find q_comment_content comment_a in
      let* content = or_fail "A comment state" content in
      Alcotest.(check string) "author soft delete tombstones" "[deleted]" content;
      (* Global admin: deletes regardless of author, with the admin label. *)
      let* r = Earde.Db.admin_delete_comment conn ~label:"[removed by admin]" comment_b in
      Alcotest.(check (result unit string)) "admin delete ok" (Ok ()) r;
      let* content = C.find q_comment_content comment_b in
      let* content = or_fail "B comment after admin" content in
      Alcotest.(check string) "admin delete tombstones with admin label" "[removed by admin]" content;
      Lwt.return_unit)

let suite = [ posts_case; comments_case; delete_comment_case ]

let suites =
  [ ( "mod_delete_community_scope", suite )
  ]
