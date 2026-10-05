(* === PRE-LAUNCH SECURITY FIXES: the database-backed half ===

   Each case reproduces the ORIGINAL attack against the real handlers over
   the real routed pipeline, so a regression fails here rather than merely
   changing a rendered string. Same EARDE_TEST_DATABASE_URL opt-in gate as
   every other DB-backed suite; without it nothing here touches a database.

   Sessions are Dream.sql_sessions rather than memory_sessions, because the
   account-deletion case needs the durable dream_session rows the production
   deployment actually stores. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let contains haystack needle = Html_assert.contains haystack needle

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'sec\\_%')";
      "DELETE FROM comments WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'sec\\_%')";
      "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE title \
       LIKE 'sec %')";
      "DELETE FROM post_votes WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'sec\\_%')";
      "DELETE FROM comment_votes WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'sec\\_%')";
      "DELETE FROM posts WHERE title LIKE 'sec %'";
      "DELETE FROM community_user_stats WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'sec-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'sec-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'sec-%')";
      "DELETE FROM community_bans WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'sec-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'sec-%')";
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'sec-%')";
      "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT \
       'community:' || c.id::text FROM communities c WHERE c.slug LIKE \
       'sec-%')";
      "DELETE FROM communities WHERE slug LIKE 'sec-%'";
      "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id IN (SELECT \
       'user:' || id::text FROM users WHERE username LIKE 'sec\\_%' OR \
       username LIKE '[deleted\\_%')";
      "DELETE FROM rate_limits WHERE ip_address LIKE 'sec-%'";
      "DELETE FROM dream_session WHERE payload LIKE '%sec\\_%'";
      "DELETE FROM users WHERE username LIKE 'sec\\_%'";
    ]

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

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
             Security_fixture.ensure_uploads_dir ();
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url conn (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === fixtures === *)

let q_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@sec.invalid', 'x', TRUE) RETURNING id"

let q_community =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility) VALUES ($1, $1, $2)\n\
    \   RETURNING id"

let q_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

let q_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

let q_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)\n\
    \   VALUES ($1, 'sec body', $2, $3) RETURNING id"

let q_comment =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id) VALUES ($1, $2, $3)\n\
    \   RETURNING id"

let q_set_avatar =
  (Caqti_type.(t2 (option string) int) ->. Caqti_type.unit)
    "UPDATE users SET avatar_url = $1 WHERE id = $2"

let q_avatar =
  (Caqti_type.int ->? Caqti_type.(option string))
    "SELECT avatar_url FROM users WHERE id = $1"

let q_username =
  (Caqti_type.int ->? Caqti_type.string)
    "SELECT username FROM users WHERE id = $1"

let q_count_comments_on_post =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM comments WHERE post_id = $1"

let q_count_notifs =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM notifications WHERE user_id = $1"

let q_count_sessions =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM dream_session\n\
    \    WHERE payload::jsonb ->> 'user_id' = $1"

let q_local_comment_count =
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT local_comment_count FROM community_user_stats\n\
    \    WHERE user_id = $1 AND community_id = $2"

(* Like q_user but with a REAL argon2 hash — the password-change cases go
   through the production handler, which verifies the old password. *)
let q_user_hashed =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@sec.invalid', $2, TRUE) RETURNING id"

let q_channel =
  (Caqti_type.(t2 string int) ->! Caqti_type.int)
    "INSERT INTO channels (slug, name, community_id) VALUES ($1, $1, $2)\n\
    \   RETURNING id"

let q_password_hash =
  (Caqti_type.int ->? Caqti_type.string)
    "SELECT password_hash FROM users WHERE id = $1"

let q_is_banned =
  (Caqti_type.int ->? Caqti_type.bool)
    "SELECT is_banned FROM users WHERE id = $1"

(* === vote ban-enforcement fixtures === *)

let q_community_ban =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_bans (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

(* Sets the DURABLE global-ban flag without going through
   ban_user_handler. The production handler deliberately revokes every
   session of the banned user in the same transaction, which would leave
   nothing to test with: these cases exist precisely to prove the vote
   mutation boundary refuses on its own, so authorization never depends on
   session revocation having happened. The production invariant is asserted
   elsewhere, by global_ban_revocation_case. *)
let q_global_ban =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

(* Durable global-admin authority for an acting admin in the routed cases:
   the session claim alone no longer grants any of it. *)
let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

let q_post_vote =
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT direction FROM post_votes WHERE user_id = $1 AND post_id = $2"

let q_comment_vote =
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT direction FROM comment_votes WHERE user_id = $1 AND comment_id = $2"

(* Scores are derived, not stored: the same SUM the feed and post pages read. *)
let q_post_score =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COALESCE(SUM(direction), 0)::int FROM post_votes WHERE post_id = $1"

let q_comment_score =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COALESCE(SUM(direction), 0)::int FROM comment_votes\n\
    \    WHERE comment_id = $1"

(* local_karma IS stored — the vote SQL denormalizes it onto the AUTHOR's
   community_user_stats row, so a partial mutation would show up here even
   if the vote row itself looked untouched. *)
let q_local_karma =
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
    "SELECT local_karma FROM community_user_stats\n\
    \    WHERE user_id = $1 AND community_id = $2"

(* === the pipeline === *)

let sec_secret = "sec-test-secret-value"
let shared_pipeline = ref None

(* Set by the /session route: which identity the NEXT minted session gets —
   (user_id, optional username, admin claim). username/is_admin ride along
   because change_password_handler keys off the username session field and
   ban_user_handler off the is_admin claim, exactly like a real login. *)
let next_identity = ref (None : (int * string option * bool) option)

let pipeline_for ~url =
  match !shared_pipeline with
  | Some p -> p
  | None ->
      let p =
        Dream.sql_pool ~size:4 url
        @@ Dream.set_secret sec_secret
        (* Real SQL sessions: the account-deletion case asserts on the
           durable dream_session rows, which memory_sessions does not
           create. *)
        @@ Dream.sql_sessions
        @@ Dream.router
             [
               (* Mints a session for the requested identity and hands back
                  a CSRF token minted inside it. Stands in for POST /login
                  without needing argon2 in the test path; the session it
                  creates is the same durable row a real login creates. *)
               Dream.get "/session" (fun req ->
                   let* () =
                     match !next_identity with
                     | None -> Lwt.return_unit
                     | Some (uid, username, admin) ->
                         let* () =
                           Dream.set_session_field req "user_id"
                             (string_of_int uid)
                         in
                         let* () =
                           match username with
                           | None -> Lwt.return_unit
                           | Some u -> Dream.set_session_field req "username" u
                         in
                         if admin then
                           Dream.set_session_field req "is_admin" "true"
                         else Lwt.return_unit
                   in
                   Dream.respond (Dream.csrf_token req));
               Dream.get "/whoami" (fun req ->
                   match Dream.session_field req "user_id" with
                   | Some uid -> Dream.respond ("uid:" ^ uid)
                   | None -> Dream.respond ~status:`Unauthorized "anon");
               Dream.get "/token" (fun req ->
                   Dream.respond (Dream.csrf_token req));
               Dream.get "/settings"
                 Earde.Account_handlers.settings_page_handler;
               Dream.post "/settings"
                 Earde.Account_handlers.update_profile_handler;
               Dream.post "/delete-account"
                 Earde.Account_handlers.delete_account_handler;
               Dream.post "/comments"
                 Earde.Comment_handlers.create_comment_handler;
               Dream.post "/posts" Earde.Post_handlers.create_post_handler;
               Dream.post "/update-community"
                 Earde.Community_settings_handlers.update_community_handler;
               (* Session-revocation slice: the routed production handlers
                  for authenticated password change, global ban and
                  realtime-token refresh, mounted at their lib/app_routes.ml
                  paths. *)
               Dream.post "/settings/password"
                 Earde.Account_handlers.change_password_handler;
               Dream.post "/admin/ban/user/:id"
                 Earde.Admin_handlers.ban_user_handler;
               Dream.get "/c/:slug/ch/:channel_slug/realtime-token"
                 Earde.Chat_handlers.realtime_token_handler;
               (* Vote ban-enforcement slice: the two production vote
                  mutation boundaries, at their lib/app_routes.ml paths. *)
               Dream.post "/vote" Earde.Vote_handlers.vote_handler;
               Dream.post "/vote-comment"
                 Earde.Vote_handlers.vote_comment_handler
               (* No /add-mod and no /remove-mod: their absence from this
                  router mirrors lib/app_routes.ml, and the legacy-route case
                  asserts the real app answers 404 for them. *);
             ]
      in
      shared_pipeline := Some p;
      p

let session_cookie label response =
  match
    List.find_opt
      (fun v -> contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | None -> Alcotest.fail (label ^ ": no session cookie")
  | Some v -> (
      match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)

(* One live session for [uid], plus a CSRF token minted inside it. *)
let login ~url ?username ?(admin = false) uid =
  next_identity := Some (uid, username, admin);
  let p = pipeline_for ~url in
  let* response = p (Dream.request ~method_:`GET ~target:"/session" "") in
  let cookie = session_cookie "session" response in
  let* token = Dream.body response in
  next_identity := None;
  Lwt.return (cookie, token)

(* A fresh CSRF token inside an EXISTING session. *)
let token_in ~url ~cookie =
  let p = pipeline_for ~url in
  let* response =
    p
      (Dream.request ~method_:`GET ~target:"/token"
         ~headers:[ ("Cookie", cookie) ]
         "")
  in
  Dream.body response

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let boundary = "secboundary"

let multipart_body fields =
  String.concat ""
    (List.map
       (fun (k, v) ->
         Printf.sprintf
           "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
           boundary k v)
       fields)
  ^ Printf.sprintf "--%s--\r\n" boundary

(* A file part, so the handler's multipart parse yields real upload bytes. *)
let multipart_body_with_file ~file_field ~filename ~bytes fields =
  String.concat ""
    (List.map
       (fun (k, v) ->
         Printf.sprintf
           "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
           boundary k v)
       fields)
  ^ Printf.sprintf
      "--%s\r\n\
       Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n\
       Content-Type: image/png\r\n\
       \r\n\
       %s\r\n"
      boundary file_field filename bytes
  ^ Printf.sprintf "--%s--\r\n" boundary

let do_get ~url ~cookie ~target =
  let p = pipeline_for ~url in
  let* response =
    p (Dream.request ~method_:`GET ~target ~headers:[ ("Cookie", cookie) ] "")
  in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), response, body)

let do_post ~url ~cookie ~target ~token ?(multipart = false) ?file fields =
  let p = pipeline_for ~url in
  let all = ("dream.csrf", token) :: fields in
  let body =
    match file with
    | Some (file_field, filename, bytes) ->
        multipart_body_with_file ~file_field ~filename ~bytes all
    | None -> if multipart then multipart_body all else form_body all
  in
  let headers =
    [
      ( "Content-Type",
        if multipart || file <> None then
          "multipart/form-data; boundary=" ^ boundary
        else "application/x-www-form-urlencoded" );
      ("Cookie", cookie);
    ]
  in
  let request = Dream.request ~method_:`POST ~target ~headers "" in
  Dream.set_body request body;
  let* response = p request in
  let* rbody = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), response, rbody)

(* ------------------------------------------------------------------ *)
(* Fix 1 — arbitrary cross-user media deletion                         *)
(* ------------------------------------------------------------------ *)

(* The original P0, end to end: the attacker points their own avatar_url at
   the victim's upload through the hidden existing_avatar_url field, then
   deletes their own account so the deletion-side cleanup unlinks the
   victim's file. *)
let avatar_theft_case =
  db_case
    "cross-user media deletion: existing_avatar_url is ignored and the \
     victim's file survives the attacker's account deletion"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* victim = C.find q_user "sec_victim" in
      let* victim = or_fail "victim" victim in
      let* attacker = C.find q_user "sec_attacker" in
      let* attacker = or_fail "attacker" attacker in
      let victim_file = "earde_1753000000123_004217.webp" in
      let victim_path = Security_fixture.make_upload_file victim_file in
      let* r =
        C.exec q_set_avatar
          (Some (Security_fixture.avatar_url_of victim_file), victim)
      in
      let* () = or_fail "set victim avatar" r in
      (* The attacker starts with an avatar of their own, so the assertion
         below distinguishes "kept mine" from "took nothing". *)
      let attacker_file = "earde_1753000000999_000001.webp" in
      let attacker_path = Security_fixture.make_upload_file attacker_file in
      let* r =
        C.exec q_set_avatar
          (Some (Security_fixture.avatar_url_of attacker_file), attacker)
      in
      let* () = or_fail "set attacker avatar" r in
      Alcotest.(check bool)
        "victim file exists before" true
        (Sys.file_exists victim_path);

      let* cookie, token = login ~url attacker in
      (* Exactly the exploit request: no file part, and the victim's URL in
         the field the form used to round-trip. *)
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/settings" ~token ~multipart:true
          [
            ("bio", "sec bio");
            ("existing_avatar_url", Security_fixture.avatar_url_of victim_file);
            ("avatar_url", "");
          ]
      in
      Alcotest.(check int) "profile update accepted" 303 status;

      let* stored = C.find_opt q_avatar attacker in
      let* stored = or_fail "attacker avatar" stored in
      Alcotest.(check (option (option string)))
        "attacker keeps their OWN avatar, never the victim's"
        (Some (Some (Security_fixture.avatar_url_of attacker_file)))
        stored;

      (* And the deletion-side cleanup therefore touches only the
         attacker's own file. *)
      let* token = token_in ~url ~cookie in
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/delete-account" ~token []
      in
      Alcotest.(check int) "account deleted" 303 status;
      Alcotest.(check bool)
        "VICTIM FILE SURVIVES" true
        (Sys.file_exists victim_path);
      Alcotest.(check bool)
        "attacker's own file is cleaned up" false
        (Sys.file_exists attacker_path);
      (* The victim's row is untouched. *)
      let* still = C.find_opt q_avatar victim in
      let* still = or_fail "victim avatar" still in
      Alcotest.(check (option (option string)))
        "victim avatar unchanged"
        (Some (Some (Security_fixture.avatar_url_of victim_file)))
        still;
      Lwt.return_unit)

(* The legitimate behaviour the removed field used to provide: saving the
   profile without choosing a new file must keep the current avatar. *)
let avatar_preserved_case =
  db_case
    "profile update with no new upload preserves the caller's real current \
     avatar" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find q_user "sec_keeper" in
      let* uid = or_fail "user" uid in
      let mine = "earde_1753000111222_000042.webp" in
      let _ = Security_fixture.make_upload_file mine in
      let* r =
        C.exec q_set_avatar (Some (Security_fixture.avatar_url_of mine), uid)
      in
      let* () = or_fail "set avatar" r in
      let* cookie, token = login ~url uid in
      (* No file part and no existing_avatar_url at all — the browser now
         sends neither. *)
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/settings" ~token ~multipart:true
          [ ("bio", "sec updated bio"); ("avatar_url", "") ]
      in
      Alcotest.(check int) "accepted" 303 status;
      let* stored = C.find_opt q_avatar uid in
      let* stored = or_fail "avatar" stored in
      Alcotest.(check (option (option string)))
        "avatar preserved"
        (Some (Some (Security_fixture.avatar_url_of mine)))
        stored;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Fix 4 — account deletion revokes every session                      *)
(* ------------------------------------------------------------------ *)

let session_revocation_case =
  db_case
    "account deletion: a second live session stops authenticating and writes \
     nothing" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find q_user "sec_twosession" in
      let* uid = or_fail "user" uid in
      let* other = C.find q_user "sec_bystander" in
      let* other = or_fail "bystander" other in
      let* community = C.find q_community ("sec-two", "public") in
      let* community = or_fail "community" community in
      let* r = C.exec q_member (uid, community) in
      let* () = or_fail "member" r in
      let* post = C.find q_post ("sec twosession post", community, uid) in
      let* post = or_fail "post" post in

      (* Two independent browsers for one account. *)
      let* cookie_a, token_a = login ~url uid in
      let* cookie_b, _ = login ~url uid in
      Alcotest.(check bool) "two distinct sessions" true (cookie_a <> cookie_b);
      (* A bystander's session must survive all of this. *)
      let* cookie_c, _ = login ~url other in

      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "session count" n in
      Alcotest.(check int) "both sessions are durable rows" 2 n;

      let* status, _, _ = do_get ~url ~cookie:cookie_b ~target:"/whoami" in
      Alcotest.(check int) "session B authenticates before deletion" 200 status;

      let* status, _, _ =
        do_post ~url ~cookie:cookie_a ~target:"/delete-account" ~token:token_a
          []
      in
      Alcotest.(check int) "deleted through session A" 303 status;

      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "session count after" n in
      Alcotest.(check int) "no session row survives for that user" 0 n;

      (* Session B is now anonymous on a read... *)
      let* status, _, _ = do_get ~url ~cookie:cookie_b ~target:"/whoami" in
      Alcotest.(check int) "session B is logged out" 401 status;
      let* status, _, body = do_get ~url ~cookie:cookie_b ~target:"/settings" in
      Alcotest.(check bool)
        "settings sends B to login" true
        (status = 302 || status = 303 || contains body "login");

      (* ...and writes nothing. The token must be minted inside B, which
         now means an anonymous session — exactly what an attacker holding
         the old cookie would have. *)
      let before = C.find q_count_comments_on_post post in
      let* before = before in
      let* before = or_fail "comments before" before in
      let* token_b = token_in ~url ~cookie:cookie_b in
      let* _ =
        do_post ~url ~cookie:cookie_b ~target:"/comments" ~token:token_b
          [
            ("content", "sec after-delete comment");
            ("post_id", string_of_int post);
          ]
      in
      let* after = C.find q_count_comments_on_post post in
      let* after = or_fail "comments after" after in
      Alcotest.(check int) "no comment was written" before after;

      (* The bystander is untouched. *)
      let* status, _, _ = do_get ~url ~cookie:cookie_c ~target:"/whoami" in
      Alcotest.(check int) "another user's session is unaffected" 200 status;
      let* n = C.find q_count_sessions (string_of_int other) in
      let* n = or_fail "bystander sessions" n in
      Alcotest.(check int) "bystander session row survives" 1 n;

      (* The account really was deleted, not merely logged out. *)
      let* name = C.find_opt q_username uid in
      let* name = or_fail "username" name in
      Alcotest.(check bool)
        "account anonymized" true
        (match name with Some n -> contains n "[deleted_" | None -> false);
      Lwt.return_unit)

let password_reset_revocation_case =
  db_case "password reset revokes the user's other sessions"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find q_user "sec_resetter" in
      let* uid = or_fail "user" uid in
      let* other = C.find q_user "sec_reset_bystander" in
      let* other = or_fail "bystander" other in
      let* _ = login ~url uid in
      let* _ = login ~url uid in
      let* cookie_c, _ = login ~url other in
      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "before" n in
      Alcotest.(check int) "two sessions before the reset" 2 n;

      let token = "sec-reset-token-value" in
      let* created =
        Earde.Credential_store.create_token c "sec_resetter@sec.invalid" token
      in
      let* created = or_fail_s "create token" created in
      Alcotest.(check bool) "token created" true created;
      let* ok =
        Earde.Credential_store.reset_password_atomically c token "sec-new-hash"
      in
      let* ok = or_fail_s "reset" ok in
      Alcotest.(check bool) "reset applied" true ok;

      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "after" n in
      Alcotest.(check int) "every session of that user is gone" 0 n;
      let* status, _, _ = do_get ~url ~cookie:cookie_c ~target:"/whoami" in
      Alcotest.(check int) "another user's session is unaffected" 200 status;
      Lwt.return_unit)

let q_count_reset_tokens =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM password_resets WHERE user_id = $1"

(* Two links issued for one account: using either must kill the other, or
   an older link (one an attacker read from the mailbox, say) could set a
   new password again right after the owner recovered the account. *)
let password_reset_kills_other_links_case =
  db_case "password reset: using one link kills every other outstanding link"
    (fun ~url:_ _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find q_user "sec_twolinks" in
      let* uid = or_fail "user" uid in
      let* bystander = C.find q_user "sec_twolinks_bystander" in
      let* bystander = or_fail "bystander" bystander in
      let email = "sec_twolinks@sec.invalid" in
      let mint token =
        let* created = Earde.Credential_store.create_token c email token in
        let* created = or_fail_s ("create " ^ token) created in
        Alcotest.(check bool) ("created " ^ token) true created;
        Lwt.return_unit
      in
      let* () = mint "sec-older-link" in
      let* () = mint "sec-newer-link" in
      let* created =
        Earde.Credential_store.create_token c
          "sec_twolinks_bystander@sec.invalid" "sec-bystander-link"
      in
      let* _ = or_fail_s "bystander token" created in
      let* n = C.find q_count_reset_tokens uid in
      let* n = or_fail "tokens before" n in
      Alcotest.(check int) "two links outstanding" 2 n;
      let* ok =
        Earde.Credential_store.reset_password_atomically c "sec-newer-link"
          "sec-owner-hash"
      in
      let* ok = or_fail_s "owner reset" ok in
      Alcotest.(check bool) "owner's reset applied" true ok;
      let* valid = Earde.Credential_store.validate_token c "sec-older-link" in
      let* valid = or_fail_s "validate older" valid in
      Alcotest.(check (option int))
        "the older link no longer validates" None valid;
      let* again =
        Earde.Credential_store.reset_password_atomically c "sec-older-link"
          "sec-attacker-hash"
      in
      let* again = or_fail_s "older reset" again in
      Alcotest.(check bool) "the older link cannot reset again" false again;
      let* stored = C.find_opt q_password_hash uid in
      let* stored = or_fail "stored hash" stored in
      Alcotest.(check (option string))
        "the owner's password stands" (Some "sec-owner-hash") stored;
      let* n = C.find q_count_reset_tokens uid in
      let* n = or_fail "tokens after" n in
      Alcotest.(check int) "no link survives for the account" 0 n;
      let* n = C.find q_count_reset_tokens bystander in
      let* n = or_fail "bystander tokens" n in
      Alcotest.(check int) "another account's link is untouched" 1 n;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Session-revocation slice — authenticated password change            *)
(* ------------------------------------------------------------------ *)

let pw_old = "sec-old-password-1"
let pw_new = "sec-new-password-9"

(* Auth.hash_password pads the encoded hash to encoded_len with trailing
   NULs, which the text-typed Postgres roundtrip strips; trim them so the
   fixture compares equal to what the users table actually stores. *)
let strip_nuls s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = '\000' do
    decr n
  done;
  String.sub s 0 !n

let password_change_revocation_case =
  db_case
    "password change: every session is revoked, the changing browser is logged \
     out, and the hash really rotates" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = Earde.Auth.hash_password pw_old in
      let* hash = or_fail_s "hash fixture" hash in
      let hash = strip_nuls hash in
      let* uid = C.find q_user_hashed ("sec_pwchanger", hash) in
      let* uid = or_fail "user" uid in
      let* other = C.find q_user "sec_pw_bystander" in
      let* other = or_fail "bystander" other in

      (* Browser A performs the change; browser B stands in for a stolen
         cookie that must not survive it. *)
      let* cookie_a, token_a = login ~url ~username:"sec_pwchanger" uid in
      let* cookie_b, _ = login ~url uid in
      let* cookie_c, _ = login ~url other in
      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "sessions before" n in
      Alcotest.(check int) "two sessions before the change" 2 n;

      let* status, _, body =
        do_post ~url ~cookie:cookie_a ~target:"/settings/password"
          ~token:token_a
          [
            ("old_password", pw_old);
            ("new_password", pw_new);
            ("confirm_password", pw_new);
          ]
      in
      Alcotest.(check int) "change accepted" 200 status;
      Alcotest.(check bool)
        "re-auth copy, not an error page" true
        (contains body "Password Changed");

      (* The hash really rotated: old no longer verifies, new does. *)
      let* stored = C.find_opt q_password_hash uid in
      let* stored = or_fail "stored hash" stored in
      let stored = Option.get stored in
      Alcotest.(check bool) "hash rotated" true (stored <> hash);
      let* old_ok = Earde.Auth.verify_password ~password:pw_old ~hash:stored in
      Alcotest.(check bool)
        "old password no longer authenticates" false (old_ok = Ok true);
      let* new_ok = Earde.Auth.verify_password ~password:pw_new ~hash:stored in
      Alcotest.(check bool) "new password authenticates" true (new_ok = Ok true);

      (* BOTH previously issued sessions are dead — as durable rows and
         over HTTP, including the very browser that made the change. *)
      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "sessions after" n in
      Alcotest.(check int) "no session row survives" 0 n;
      let* status, _, _ = do_get ~url ~cookie:cookie_a ~target:"/whoami" in
      Alcotest.(check int) "the changing browser is logged out" 401 status;
      let* status, _, _ = do_get ~url ~cookie:cookie_b ~target:"/whoami" in
      Alcotest.(check int) "the other (stolen) session is logged out" 401 status;

      let* status, _, _ = do_get ~url ~cookie:cookie_c ~target:"/whoami" in
      Alcotest.(check int) "another user's session is unaffected" 200 status;
      let* n = C.find q_count_sessions (string_of_int other) in
      let* n = or_fail "bystander sessions" n in
      Alcotest.(check int) "bystander session row survives" 1 n;
      Lwt.return_unit)

let password_change_wrong_old_case =
  db_case
    "password change with the wrong current password revokes nothing and \
     leaves the hash untouched" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = Earde.Auth.hash_password pw_old in
      let* hash = or_fail_s "hash fixture" hash in
      let hash = strip_nuls hash in
      let* uid = C.find q_user_hashed ("sec_pwwrong", hash) in
      let* uid = or_fail "user" uid in
      let* cookie_a, token_a = login ~url ~username:"sec_pwwrong" uid in
      let* cookie_b, _ = login ~url uid in

      let* status, _, body =
        do_post ~url ~cookie:cookie_a ~target:"/settings/password"
          ~token:token_a
          [
            ("old_password", "sec-not-the-password");
            ("new_password", pw_new);
            ("confirm_password", pw_new);
          ]
      in
      Alcotest.(check int) "refused" 200 status;
      Alcotest.(check bool)
        "wrong-password copy" true
        (contains body "Wrong Password");

      let* n = C.find q_count_sessions (string_of_int uid) in
      let* n = or_fail "sessions" n in
      Alcotest.(check int) "both sessions survive" 2 n;
      let* status, _, _ = do_get ~url ~cookie:cookie_a ~target:"/whoami" in
      Alcotest.(check int) "changing browser still authenticated" 200 status;
      let* status, _, _ = do_get ~url ~cookie:cookie_b ~target:"/whoami" in
      Alcotest.(check int) "second session still authenticated" 200 status;
      let* stored = C.find_opt q_password_hash uid in
      let* stored = or_fail "stored hash" stored in
      Alcotest.(check (option string)) "hash unchanged" (Some hash) stored;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Session-revocation slice — global ban                               *)
(* ------------------------------------------------------------------ *)

let global_ban_revocation_case =
  db_case
    "global ban: the target's sessions are revoked and the stale session can \
     no longer mint a realtime token" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* admin = C.find q_user "sec_banadmin" in
      let* admin = or_fail "admin" admin in
      (* The route authorizes on the DURABLE users.is_admin row (the session
         claim only enables that lookup), so the acting admin must be one. *)
      let* r = C.exec q_make_admin admin in
      let* () = or_fail "durable admin" r in
      let* target = C.find q_user "sec_bantarget" in
      let* target = or_fail "target" target in
      let* other = C.find q_user "sec_ban_bystander" in
      let* other = or_fail "bystander" other in
      let* community = C.find q_community ("sec-banchat", "public") in
      let* community = or_fail "community" community in
      let* chan = C.find q_channel ("sec-banchan", community) in
      let* _ = or_fail "channel" chan in

      let* cookie_t, _ = login ~url ~username:"sec_bantarget" target in
      let* cookie_t2, _ = login ~url target in
      let* cookie_c, _ = login ~url other in
      let* n = C.find q_count_sessions (string_of_int target) in
      let* n = or_fail "before" n in
      Alcotest.(check int) "two live sessions before the ban" 2 n;

      (* The session can reach the token endpoint before the ban: anything
         but 401 proves it authenticated (200 with a configured signing
         secret, 503 without one). *)
      let token_target = "/c/sec-banchat/ch/sec-banchan/realtime-token" in
      let* status, _, _ = do_get ~url ~cookie:cookie_t ~target:token_target in
      Alcotest.(check bool)
        "token endpoint authenticates before the ban" true (status <> 401);

      (* The ban goes through the real routed admin handler. *)
      let* cookie_ad, token_ad = login ~url ~admin:true admin in
      let* status, _, _ =
        do_post ~url ~cookie:cookie_ad
          ~target:("/admin/ban/user/" ^ string_of_int target)
          ~token:token_ad []
      in
      Alcotest.(check int) "ban accepted" 303 status;
      let* banned = C.find_opt q_is_banned target in
      let* banned = or_fail "banned flag" banned in
      Alcotest.(check (option bool)) "durably banned" (Some true) banned;

      (* Every session of the banned user is dead... *)
      let* n = C.find q_count_sessions (string_of_int target) in
      let* n = or_fail "after" n in
      Alcotest.(check int) "no session row survives the ban" 0 n;
      let* status, _, _ = do_get ~url ~cookie:cookie_t ~target:"/whoami" in
      Alcotest.(check int) "session one is logged out" 401 status;
      let* status, _, _ = do_get ~url ~cookie:cookie_t2 ~target:"/whoami" in
      Alcotest.(check int) "session two is logged out" 401 status;
      (* ...so the stale cookie can no longer mint a fresh realtime token. *)
      let* status, _, _ = do_get ~url ~cookie:cookie_t ~target:token_target in
      Alcotest.(check int) "realtime token refresh refused" 401 status;

      (* Admin and bystander sessions survive. *)
      let* status, _, _ = do_get ~url ~cookie:cookie_ad ~target:"/whoami" in
      Alcotest.(check int) "the admin stays logged in" 200 status;
      let* status, _, _ = do_get ~url ~cookie:cookie_c ~target:"/whoami" in
      Alcotest.(check int) "another user's session is unaffected" 200 status;
      Lwt.return_unit)

let ban_requires_admin_case =
  db_case "global ban by a non-admin is refused and revokes nothing"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* attacker = C.find q_user "sec_bannonadmin" in
      let* attacker = or_fail "attacker" attacker in
      let* target = C.find q_user "sec_bansafe" in
      let* target = or_fail "target" target in
      let* cookie_t, _ = login ~url target in
      let* cookie_a, token_a = login ~url attacker in

      let* _status, _, body =
        do_post ~url ~cookie:cookie_a
          ~target:("/admin/ban/user/" ^ string_of_int target)
          ~token:token_a []
      in
      Alcotest.(check bool)
        "refused as non-admin" true
        (contains body "not an Admin");
      let* banned = C.find_opt q_is_banned target in
      let* banned = or_fail "banned flag" banned in
      Alcotest.(check (option bool)) "target not banned" (Some false) banned;
      let* n = C.find q_count_sessions (string_of_int target) in
      let* n = or_fail "sessions" n in
      Alcotest.(check int) "target's session survives" 1 n;
      let* status, _, _ = do_get ~url ~cookie:cookie_t ~target:"/whoami" in
      Alcotest.(check int) "target still authenticated" 200 status;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Fix 5 — parent comments are bound to the canonical post             *)
(* ------------------------------------------------------------------ *)

let parent_binding_case =
  db_case
    "comment parent binding: same-post accepted; cross-post, cross-community, \
     private and nonexistent parents refused" (fun ~url:_ _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_pauthor" in
      let* author = or_fail "author" author in
      let* alpha = C.find q_community ("sec-alpha", "public") in
      let* alpha = or_fail "alpha" alpha in
      let* beta = C.find q_community ("sec-beta", "public") in
      let* beta = or_fail "beta" beta in
      let* secret = C.find q_community ("sec-secret", "private") in
      let* secret = or_fail "secret" secret in
      let* post_a = C.find q_post ("sec alpha post", alpha, author) in
      let* post_a = or_fail "post a" post_a in
      (* A SECOND post in the SAME community: a cross-post parent must be
         refused even when no community boundary is crossed. *)
      let* post_a2 = C.find q_post ("sec alpha post two", alpha, author) in
      let* post_a2 = or_fail "post a2" post_a2 in
      let* post_b = C.find q_post ("sec beta post", beta, author) in
      let* post_b = or_fail "post b" post_b in
      let* post_s = C.find q_post ("sec secret post", secret, author) in
      let* post_s = or_fail "post s" post_s in
      let* parent_a = C.find q_comment ("sec parent a", post_a, author) in
      let* parent_a = or_fail "parent a" parent_a in
      let* parent_a2 = C.find q_comment ("sec parent a2", post_a2, author) in
      let* parent_a2 = or_fail "parent a2" parent_a2 in
      let* parent_b = C.find q_comment ("sec parent b", post_b, author) in
      let* parent_b = or_fail "parent b" parent_b in
      let* parent_s = C.find q_comment ("sec parent s", post_s, author) in
      let* parent_s = or_fail "parent s" parent_s in

      let create ?parent label =
        let* r =
          Earde.Comment_store.create_comment c ("sec reply " ^ label) post_a
            author parent
        in
        or_fail_s ("create " ^ label) r
      in
      let create_none label = create label in
      ignore create_none;
      let refused label = function
        | `Invalid_parent -> Lwt.return_unit
        | `Created id ->
            Alcotest.failf "%s: accepted, wrote comment %d" label id
      in
      let created label = function
        | `Created _ -> Lwt.return_unit
        | `Invalid_parent -> Alcotest.failf "%s: refused" label
      in

      let* r = create "toplevel" in
      let* () = created "no parent" r in
      let* r = create ~parent:parent_a "same-post" in
      let* () = created "same-post parent" r in
      let* r = create ~parent:parent_a2 "cross-post" in
      let* () = refused "cross-post parent (same community)" r in
      let* r = create ~parent:parent_b "cross-community" in
      let* () = refused "cross-community parent" r in
      let* r = create ~parent:parent_s "private" in
      let* () = refused "private-community parent" r in
      let* r = create ~parent:2147483000 "nonexistent" in
      let* () = refused "nonexistent parent" r in
      Lwt.return_unit)

(* The handler half: the refusal must land BEFORE every side effect, so a
   rejected reply leaves no comment, no notification, and no counter. *)
let parent_binding_no_side_effects_case =
  db_case
    "a refused cross-community reply writes no comment, no notification and no \
     counter" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* mallory = C.find q_user "sec_mallory" in
      let* mallory = or_fail "mallory" mallory in
      let* carol = C.find q_user "sec_carol" in
      let* carol = or_fail "carol" carol in
      let* alpha = C.find q_community ("sec-h-alpha", "public") in
      let* alpha = or_fail "alpha" alpha in
      let* secret = C.find q_community ("sec-h-secret", "private") in
      let* secret = or_fail "secret" secret in
      let* r = C.exec q_member (mallory, alpha) in
      let* () = or_fail "member" r in
      let* r = C.exec q_member (carol, secret) in
      let* () = or_fail "carol member" r in
      let* post_a = C.find q_post ("sec h alpha post", alpha, mallory) in
      let* post_a = or_fail "post a" post_a in
      let* post_s = C.find q_post ("sec h secret post", secret, carol) in
      let* post_s = or_fail "post s" post_s in
      (* Carol's comment inside the private community Mallory cannot see. *)
      let* carol_comment =
        C.find q_comment ("sec h carol secret", post_s, carol)
      in
      let* carol_comment = or_fail "carol comment" carol_comment in

      let* comments_before = C.find q_count_comments_on_post post_a in
      let* comments_before = or_fail "before" comments_before in
      let* notifs_before = C.find q_count_notifs carol in
      let* notifs_before = or_fail "notifs before" notifs_before in

      let* cookie, token = login ~url mallory in
      let* status, _, body =
        do_post ~url ~cookie ~target:"/comments" ~token
          [
            ("content", "sec h injected reply");
            ("post_id", string_of_int post_a);
            ("parent_id", string_of_int carol_comment);
          ]
      in
      Alcotest.(check int) "refused as a client error" 400 status;
      (* The refusal must not disclose the parent, its post, or its
         community — it is the anti-oracle property that keeps this from
         becoming a private-content probe. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("response does not leak " ^ needle)
            false (contains body needle))
        [
          "sec-h-secret";
          "sec h secret post";
          "sec h carol secret";
          "comments_parent_id_fkey";
          "constraint";
          "Database error";
          "INSERT";
          "Caqti";
          string_of_int carol_comment;
        ];

      let* comments_after = C.find q_count_comments_on_post post_a in
      let* comments_after = or_fail "after" comments_after in
      Alcotest.(check int) "no comment row" comments_before comments_after;
      let* notifs_after = C.find q_count_notifs carol in
      let* notifs_after = or_fail "notifs after" notifs_after in
      Alcotest.(check int)
        "no notification for the foreign parent's owner" notifs_before
        notifs_after;
      let* stats = C.find_opt q_local_comment_count (mallory, alpha) in
      let* stats = or_fail "stats" stats in
      Alcotest.(check bool)
        "no local comment counter" true
        (match stats with None -> true | Some n -> n = 0);

      (* The same request WITHOUT the forged parent still works, so the
         gate is the parent and nothing else. *)
      let* token = token_in ~url ~cookie in
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/comments" ~token
          [
            ("content", "sec h ordinary reply");
            ("post_id", string_of_int post_a);
          ]
      in
      Alcotest.(check int) "an ordinary comment still succeeds" 303 status;
      let* final = C.find q_count_comments_on_post post_a in
      let* final = or_fail "final" final in
      Alcotest.(check int)
        "exactly one comment was written" (comments_before + 1) final;
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Fix 7 — image uploads                                               *)
(* ------------------------------------------------------------------ *)

let upload_authorization_ordering_case =
  db_case
    "unauthorized upload: a non-member's post attempt runs no conversion and \
     stores no file" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* outsider = C.find q_user "sec_outsider" in
      let* outsider = or_fail "outsider" outsider in
      let* owner = C.find q_user "sec_owner" in
      let* owner = or_fail "owner" owner in
      let* community = C.find q_community ("sec-closed", "public") in
      let* community = or_fail "community" community in
      let* r = C.exec q_member (owner, community) in
      let* () = or_fail "member" r in

      let before = Security_fixture.uploads_listing () in
      let* cookie, token = login ~url outsider in
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/posts" ~token
          ~file:("image_url", "x.png", Security_fixture.real_png)
          [
            ("title", "sec outsider post");
            ("content", "sec body");
            ("community_id", string_of_int community);
            ("url", "");
          ]
      in
      Alcotest.(check bool) "refused" true (status <> 303);
      let after = Security_fixture.uploads_listing () in
      Alcotest.(check (list string))
        "static/uploads is untouched by an unauthorized upload" before after;
      Lwt.return_unit)

let upload_format_gate_case =
  db_case
    "upload: a valid PNG becomes a stored WebP; a non-image is refused before \
     any file is created" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find q_user "sec_uploader" in
      let* uid = or_fail "user" uid in
      let* cookie, _token = login ~url uid in

      (* --- refusals leave nothing behind --- *)
      let before = Security_fixture.uploads_listing () in
      let refuse ?(message = "JPEG, PNG, GIF, WebP") label bytes =
        let* token = token_in ~url ~cookie in
        let* status, _, body =
          do_post ~url ~cookie ~target:"/settings" ~token
            ~file:("avatar_url", "payload.png", bytes)
            [ ("bio", "sec bio") ]
        in
        Alcotest.(check bool) (label ^ ": not accepted") true (status <> 303);
        Alcotest.(check bool)
          (label ^ ": expected refusal message")
          true (contains body message);
        Alcotest.(check (list string))
          (label ^ ": no file created")
          before
          (Security_fixture.uploads_listing ());
        Lwt.return_unit
      in
      let* () = refuse "plain text" "hello, not an image" in
      let* () = refuse "svg" "<svg xmlns='http://www.w3.org/2000/svg'/>" in
      let* () =
        refuse "imagemagick MSL"
          "<?xml version=\"1.0\"?><image><read \
           filename=\"/etc/passwd\"/></image>"
      in
      let* () = refuse "postscript" "%!PS-Adobe-3.0\nshowpage\n" in
      let* () = refuse "truncated png" "\x89PN" in
      (* The byte cap is checked before the format gate, so this one gets
         the size message — the only refusal that is allowed to differ,
         because the uploader must be able to tell "too big" from
         "unsupported". *)
      let* () =
        refuse ~message:"5 MB limit" "oversized"
          (String.make ((5 * 1024 * 1024) + 1) 'a')
      in

      (* --- a real image succeeds and is transcoded --- *)
      let* token = token_in ~url ~cookie in
      let* status, _, _ =
        do_post ~url ~cookie ~target:"/settings" ~token
          ~file:("avatar_url", "tiny.png", Security_fixture.real_png)
          [ ("bio", "sec bio") ]
      in
      Alcotest.(check int) "valid PNG accepted" 303 status;
      let* stored = C.find_opt q_avatar uid in
      let* stored = or_fail "avatar" stored in
      (match stored with
      | Some (Some u) ->
          Alcotest.(check bool)
            "stored under the uploads prefix" true
            (contains u "/static/uploads/earde_");
          Alcotest.(check bool)
            "output is WebP, not the submitted format" true
            (Filename.check_suffix u ".webp");
          let path =
            Filename.concat Security_fixture.uploads_dir (Filename.basename u)
          in
          Alcotest.(check bool)
            "the file really exists" true (Sys.file_exists path);
          (* Transcoded, not stored verbatim. *)
          let ic = open_in_bin path in
          let len = min 12 (in_channel_length ic) in
          let head = really_input_string ic len in
          close_in ic;
          Alcotest.(check bool)
            "content is a WebP container" true (contains head "WEBP");
          Alcotest.(check bool)
            "the PNG signature is gone" false (contains head "PNG")
      | _ -> Alcotest.fail "no avatar stored after a valid upload");

      (* No stray temporaries: exactly one new file. *)
      let after = Security_fixture.uploads_listing () in
      Alcotest.(check int)
        "exactly one new stored file"
        (List.length before + 1)
        (List.length after);
      ignore token;
      Lwt.return_unit)

let upload_rate_limit_case =
  db_case "upload rate limit: a separate bucket from the auth allowance"
    (fun ~url:_ _conn c ->
      (* The limiter is exercised directly so the case does not depend on
         running a dozen real conversions. *)
      let ip = "sec-upload-ip" in
      let rec hit n acc =
        if n = 0 then Lwt.return (List.rev acc)
        else
          let* r = Earde.Rate_limit_store.check_upload c ip in
          let* r = or_fail_s "check_upload" r in
          hit (n - 1) (r :: acc)
      in
      let* results = hit (Earde.Rate_limit_store.upload_max_attempts + 2) [] in
      let allowed = List.length (List.filter (fun r -> r = `Allowed) results) in
      Alcotest.(check int)
        "allowance is spent, then blocked"
        Earde.Rate_limit_store.upload_max_attempts allowed;
      Alcotest.(check bool)
        "the tail is blocked" true
        (List.exists (fun r -> r = `Blocked) results);
      (* Distinct from the authentication bucket: spending the upload
         allowance must not lock the user out of logging in. *)
      let* login_check = Earde.Rate_limit_store.check c ip "/login" in
      let* login_check = or_fail_s "login bucket" login_check in
      Alcotest.(check bool)
        "the /login bucket is untouched" true (login_check = `Allowed);
      Alcotest.(check bool)
        "upload allowance differs from auth allowance" true
        (Earde.Rate_limit_store.upload_max_attempts <> 5
        || Earde.Rate_limit_store.upload_endpoint <> "/login");
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Fix 8 — the legacy moderator endpoints are gone                     *)
(* ------------------------------------------------------------------ *)

(* A direct HTTP request against the REAL application router, not an
   inspection of whether a link exists: an ordinary moderator holding a
   live session and a valid CSRF token must find nothing there. *)
let legacy_mod_routes_case =
  db_case
    "legacy /add-mod and /remove-mod are unroutable for an ordinary moderator"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* top = C.find q_user "sec_topmod" in
      let* top = or_fail "top" top in
      let* ordinary = C.find q_user "sec_ordinarymod" in
      let* ordinary = or_fail "ordinary" ordinary in
      let* target = C.find q_user "sec_modtarget" in
      let* target = or_fail "target" target in
      let* community = C.find q_community ("sec-mods", "public") in
      let* community = or_fail "community" community in
      let* r = C.exec q_moderator (top, community) in
      let* () = or_fail "top mod" r in
      let* r = C.exec q_moderator (ordinary, community) in
      let* () = or_fail "ordinary mod" r in

      (* The real route table, mounted as lib/app_routes.ml mounts them. *)
      let app =
        Dream.sql_pool ~size:1 url
        @@ Dream.set_secret sec_secret
        @@ Dream.memory_sessions
        @@ (fun handler request ->
          let* () =
            Dream.set_session_field request "user_id" (string_of_int ordinary)
          in
          handler request)
        @@ Dream.router
             [
               Dream.get "/mint" (fun req ->
                   Dream.respond (Dream.csrf_token req));
               Dream.post "/update-community"
                 Earde.Community_settings_handlers.update_community_handler;
               Dream.post "/c/:slug/manage-mods/add"
                 Earde.Moderation_handlers.manage_mods_add_handler;
               Dream.post "/c/:slug/manage-mods/remove"
                 Earde.Moderation_handlers.manage_mods_remove_handler;
             ]
      in
      let* mint = app (Dream.request ~method_:`GET ~target:"/mint" "") in
      let cookie = session_cookie "mint" mint in
      let* token = Dream.body mint in
      let post target fields =
        let request =
          Dream.request ~method_:`POST ~target
            ~headers:
              [
                ("Content-Type", "application/x-www-form-urlencoded");
                ("Cookie", cookie);
              ]
            ""
        in
        Dream.set_body request (form_body (("dream.csrf", token) :: fields));
        let* response = app request in
        Lwt.return (Dream.status_to_int (Dream.status response))
      in
      let* status =
        post "/add-mod"
          [
            ("community_id", string_of_int community);
            ("community_slug", "sec-mods");
            ("username", "sec_modtarget");
          ]
      in
      Alcotest.(check int) "POST /add-mod is not routed" 404 status;
      let* status =
        post "/remove-mod"
          [
            ("community_id", string_of_int community);
            ("community_slug", "sec-mods");
            ("target_user_id", string_of_int top);
          ]
      in
      Alcotest.(check int) "POST /remove-mod is not routed" 404 status;

      (* Nothing changed, and the modern surface still refuses an ordinary
         moderator on its own terms rather than by being absent. *)
      let* mods = Earde.Moderator_store.get_community_moderators c community in
      let* mods = or_fail_s "mods" mods in
      Alcotest.(check bool)
        "the Top Mod is still a moderator" true
        (List.exists (fun (u : Earde.User_store.user) -> u.id = top) mods);
      Alcotest.(check bool)
        "no new moderator was appointed" false
        (List.exists (fun (u : Earde.User_store.user) -> u.id = target) mods);
      let* status =
        post "/c/sec-mods/manage-mods/add" [ ("username", "sec_modtarget") ]
      in
      Alcotest.(check bool)
        "the modern add surface refuses an ordinary moderator" true
        (status <> 303);
      let* mods = Earde.Moderator_store.get_community_moderators c community in
      let* mods = or_fail_s "mods again" mods in
      Alcotest.(check bool)
        "still no new moderator" false
        (List.exists (fun (u : Earde.User_store.user) -> u.id = target) mods);
      Lwt.return_unit)

(* ------------------------------------------------------------------ *)
(* Fix 9 — voting is a ban-enforced mutation boundary                  *)
(* ------------------------------------------------------------------ *)

(* Every durable effect a post vote has: the vote row itself, the derived
   score, the AUTHOR's stored local_karma and their derived global karma.
   A refused vote must move none of them — asserting all four is what makes
   a partial mutation visible rather than merely a missing vote row. *)
let check_post_vote_state label c ~voter ~post ~author ~community ~vote ~score
    ~local ~karma =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* actual_vote = C.find_opt q_post_vote (voter, post) in
  let* actual_vote = or_fail "post vote row" actual_vote in
  Alcotest.(check (option int)) (label ^ ": vote row") vote actual_vote;
  let* actual_score = C.find q_post_score post in
  let* actual_score = or_fail "post score" actual_score in
  Alcotest.(check int) (label ^ ": post score") score actual_score;
  let* actual_local = C.find_opt q_local_karma (author, community) in
  let* actual_local = or_fail "local karma" actual_local in
  Alcotest.(check (option int))
    (label ^ ": author local karma")
    local actual_local;
  let* actual_karma = Earde.User_store.get_user_karma c author in
  let* actual_karma = or_fail_s "karma" actual_karma in
  Alcotest.(check int) (label ^ ": author karma") karma actual_karma;
  Lwt.return_unit

let check_comment_vote_state label c ~voter ~comment ~author ~community ~vote
    ~score ~local ~karma =
  let (module C : Caqti_lwt.CONNECTION) = c in
  let* actual_vote = C.find_opt q_comment_vote (voter, comment) in
  let* actual_vote = or_fail "comment vote row" actual_vote in
  Alcotest.(check (option int)) (label ^ ": vote row") vote actual_vote;
  let* actual_score = C.find q_comment_score comment in
  let* actual_score = or_fail "comment score" actual_score in
  Alcotest.(check int) (label ^ ": comment score") score actual_score;
  let* actual_local = C.find_opt q_local_karma (author, community) in
  let* actual_local = or_fail "local karma" actual_local in
  Alcotest.(check (option int))
    (label ^ ": author local karma")
    local actual_local;
  let* actual_karma = Earde.User_store.get_user_karma c author in
  let* actual_karma = or_fail_s "karma" actual_karma in
  Alcotest.(check int) (label ^ ": author karma") karma actual_karma;
  Lwt.return_unit

let vote ~url ~cookie ~post ~direction =
  let* token = token_in ~url ~cookie in
  do_post ~url ~cookie ~target:"/vote" ~token
    [ ("post_id", string_of_int post); ("direction", string_of_int direction) ]

let vote_comment ~url ~cookie ~comment ~direction =
  let* token = token_in ~url ~cookie in
  do_post ~url ~cookie ~target:"/vote-comment" ~token
    [
      ("comment_id", string_of_int comment);
      ("direction", string_of_int direction);
    ]

(* A — a community-banned user cannot create or change a post vote. *)
let post_vote_community_ban_case =
  db_case
    "post vote: a community ban stops the vote, the score and the author's \
     karma from moving" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_vauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_voter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-votes", "public") in
      let* community = or_fail "community" community in
      let* r = C.exec q_member (author, community) in
      let* () = or_fail "author member" r in
      let* r = C.exec q_member (voter, community) in
      let* () = or_fail "voter member" r in
      let* post = C.find q_post ("sec vote post", community, author) in
      let* post = or_fail "post" post in

      let* cookie, _ = login ~url voter in
      (* Accepted before the ban: this is the baseline every later assertion
         is measured against, and it proves the case would notice a vote
         that did go through. *)
      let* status, _, _ = vote ~url ~cookie ~post ~direction:1 in
      Alcotest.(check int) "upvote accepted before the ban" 303 status;
      let* () =
        check_post_vote_state "baseline" c ~voter ~post ~author ~community
          ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      let* r = C.exec q_community_ban (voter, community) in
      let* () = or_fail "community ban" r in

      (* Flipping to a downvote is refused... *)
      let* status, _, body = vote ~url ~cookie ~post ~direction:(-1) in
      Alcotest.(check int) "downvote refused" 403 status;
      Alcotest.(check bool)
        "refused on the community ban, by name" true
        (contains body "banned from this community");
      let* () =
        check_post_vote_state "after refused downvote" c ~voter ~post ~author
          ~community ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      (* ...and so is re-submitting the SAME direction, which the upsert
         would otherwise absorb as a no-op: the gate sits on the boundary,
         not on the delta. *)
      let* status, _, _ = vote ~url ~cookie ~post ~direction:1 in
      Alcotest.(check int) "re-upvote refused" 403 status;
      let* () =
        check_post_vote_state "after refused re-upvote" c ~voter ~post ~author
          ~community ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      (* An unbanned bystander is unaffected — the ban is about the voter,
         not about the post. *)
      let* bystander = C.find q_user "sec_vbystander" in
      let* bystander = or_fail "bystander" bystander in
      let* cookie_b, _ = login ~url bystander in
      let* status, _, _ = vote ~url ~cookie:cookie_b ~post ~direction:1 in
      Alcotest.(check int) "an unbanned voter still votes" 303 status;
      let* () =
        check_post_vote_state "bystander vote landed" c ~voter:bystander ~post
          ~author ~community ~vote:(Some 1) ~score:2 ~local:(Some 2) ~karma:2
      in
      Lwt.return_unit)

(* B — removal (direction=0) is a mutation too, so it is refused as well and
   the pre-ban vote survives. *)
let post_vote_removal_community_ban_case =
  db_case
    "post vote removal: a community-banned user cannot withdraw a vote cast \
     before the ban" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_vrauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_vrvoter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-vote-removal", "public") in
      let* community = or_fail "community" community in
      let* post = C.find q_post ("sec removal post", community, author) in
      let* post = or_fail "post" post in

      let* cookie, _ = login ~url voter in
      let* status, _, _ = vote ~url ~cookie ~post ~direction:1 in
      Alcotest.(check int) "vote cast before the ban" 303 status;
      let* () =
        check_post_vote_state "baseline" c ~voter ~post ~author ~community
          ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      let* r = C.exec q_community_ban (voter, community) in
      let* () = or_fail "community ban" r in

      let* status, _, body = vote ~url ~cookie ~post ~direction:0 in
      Alcotest.(check int) "removal refused" 403 status;
      Alcotest.(check bool)
        "refused on the community ban" true
        (contains body "banned from this community");
      let* () =
        check_post_vote_state "vote survives the refused removal" c ~voter ~post
          ~author ~community ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in
      Lwt.return_unit)

(* C — the same rule on the comment path, where the community has to be
   resolved through the comment's canonical parent post. *)
let comment_vote_community_ban_case =
  db_case
    "comment vote: a community ban stops the vote, the score and the author's \
     karma from moving" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_cvauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_cvvoter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-comment-votes", "public") in
      let* community = or_fail "community" community in
      let* post = C.find q_post ("sec comment vote post", community, author) in
      let* post = or_fail "post" post in
      let* comment = C.find q_comment ("sec comment body", post, author) in
      let* comment = or_fail "comment" comment in

      let* cookie, _ = login ~url voter in
      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:1 in
      Alcotest.(check int) "upvote accepted before the ban" 303 status;
      let* () =
        check_comment_vote_state "baseline" c ~voter ~comment ~author ~community
          ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      let* r = C.exec q_community_ban (voter, community) in
      let* () = or_fail "community ban" r in

      let* status, _, body =
        vote_comment ~url ~cookie ~comment ~direction:(-1)
      in
      Alcotest.(check int) "downvote refused" 403 status;
      Alcotest.(check bool)
        "refused on the community ban" true
        (contains body "banned from this community");
      let* () =
        check_comment_vote_state "after refused downvote" c ~voter ~comment
          ~author ~community ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in
      Lwt.return_unit)

(* D — comment vote removal under a community ban. *)
let comment_vote_removal_community_ban_case =
  db_case
    "comment vote removal: a community-banned user cannot withdraw a comment \
     vote cast before the ban" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_cvrauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_cvrvoter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-cvote-removal", "public") in
      let* community = or_fail "community" community in
      let* post = C.find q_post ("sec cvote removal post", community, author) in
      let* post = or_fail "post" post in
      let* comment = C.find q_comment ("sec cvote body", post, author) in
      let* comment = or_fail "comment" comment in

      let* cookie, _ = login ~url voter in
      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:1 in
      Alcotest.(check int) "vote cast before the ban" 303 status;
      let* () =
        check_comment_vote_state "baseline" c ~voter ~comment ~author ~community
          ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in

      let* r = C.exec q_community_ban (voter, community) in
      let* () = or_fail "community ban" r in

      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:0 in
      Alcotest.(check int) "removal refused" 403 status;
      let* () =
        check_comment_vote_state "vote survives the refused removal" c ~voter
          ~comment ~author ~community ~vote:(Some 1) ~score:1 ~local:(Some 1)
          ~karma:1
      in
      Lwt.return_unit)

(* E — defence in depth against the GLOBAL ban.

   The fixture flips users.is_banned directly and keeps the session alive.
   That is not how production bans a user — ban_user_handler revokes every
   session of the target in the same transaction, and
   global_ban_revocation_case asserts exactly that. This case deliberately
   constructs the state that revocation is supposed to make unreachable, to
   prove the vote boundary refuses on its own durable read rather than
   inheriting its safety from the session layer. *)
let vote_global_ban_defense_in_depth_case =
  db_case
    "global ban: an authenticated session that survived the ban still cannot \
     vote on a post or a comment" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_gvauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_gvvoter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-global-votes", "public") in
      let* community = or_fail "community" community in
      let* post = C.find q_post ("sec global vote post", community, author) in
      let* post = or_fail "post" post in
      let* comment = C.find q_comment ("sec global body", post, author) in
      let* comment = or_fail "comment" comment in

      let* cookie, _ = login ~url voter in
      (* No community ban anywhere: the only thing standing between this
         session and the vote is the global flag. *)
      let* r = C.exec q_global_ban voter in
      let* () = or_fail "global ban" r in
      let* banned = C.find_opt q_is_banned voter in
      let* banned = or_fail "banned flag" banned in
      Alcotest.(check (option bool)) "durably banned" (Some true) banned;
      (* The session really is still usable — otherwise this case would pass
         for the wrong reason. *)
      let* status, _, _ = do_get ~url ~cookie ~target:"/whoami" in
      Alcotest.(check int) "the stale session still authenticates" 200 status;

      let* status, _, body = vote ~url ~cookie ~post ~direction:1 in
      Alcotest.(check int) "post vote refused" 403 status;
      Alcotest.(check bool)
        "refused on the global ban, by name" true
        (contains body "permanently banned");
      let* () =
        check_post_vote_state "no post vote written" c ~voter ~post ~author
          ~community ~vote:None ~score:0 ~local:None ~karma:0
      in

      let* status, _, body = vote_comment ~url ~cookie ~comment ~direction:1 in
      Alcotest.(check int) "comment vote refused" 403 status;
      Alcotest.(check bool)
        "refused on the global ban" true
        (contains body "permanently banned");
      let* () =
        check_comment_vote_state "no comment vote written" c ~voter ~comment
          ~author ~community ~vote:None ~score:0 ~local:None ~karma:0
      in

      (* Removal is refused too, so the flag cannot be used as a one-way
         door out of an existing vote either. *)
      let* status, _, _ = vote ~url ~cookie ~post ~direction:0 in
      Alcotest.(check int) "post vote removal refused" 403 status;
      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:0 in
      Alcotest.(check int) "comment vote removal refused" 403 status;
      Lwt.return_unit)

(* F — the control: nothing about ordinary voting changed. *)
let vote_unbanned_control_case =
  db_case
    "unbanned control: add, change and remove still work on both post and \
     comment votes" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_okauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_okvoter" in
      let* voter = or_fail "voter" voter in
      let* community = C.find q_community ("sec-ok-votes", "public") in
      let* community = or_fail "community" community in
      let* post = C.find q_post ("sec ok post", community, author) in
      let* post = or_fail "post" post in
      let* comment = C.find q_comment ("sec ok body", post, author) in
      let* comment = or_fail "comment" comment in

      let* cookie, _ = login ~url voter in

      let* status, _, _ = vote ~url ~cookie ~post ~direction:1 in
      Alcotest.(check int) "post upvote accepted" 303 status;
      let* () =
        check_post_vote_state "post upvote" c ~voter ~post ~author ~community
          ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in
      (* A flip is a delta of 2, and the karma columns must follow it. *)
      let* status, _, _ = vote ~url ~cookie ~post ~direction:(-1) in
      Alcotest.(check int) "post vote changed to a downvote" 303 status;
      let* () =
        check_post_vote_state "post downvote" c ~voter ~post ~author ~community
          ~vote:(Some (-1)) ~score:(-1) ~local:(Some (-1)) ~karma:(-1)
      in
      let* status, _, _ = vote ~url ~cookie ~post ~direction:0 in
      Alcotest.(check int) "post vote removed" 303 status;
      let* () =
        check_post_vote_state "post vote removed" c ~voter ~post ~author
          ~community ~vote:None ~score:0 ~local:(Some 0) ~karma:0
      in

      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:1 in
      Alcotest.(check int) "comment upvote accepted" 303 status;
      let* () =
        check_comment_vote_state "comment upvote" c ~voter ~comment ~author
          ~community ~vote:(Some 1) ~score:1 ~local:(Some 1) ~karma:1
      in
      let* status, _, _ = vote_comment ~url ~cookie ~comment ~direction:0 in
      Alcotest.(check int) "comment vote removed" 303 status;
      let* () =
        check_comment_vote_state "comment vote removed" c ~voter ~comment
          ~author ~community ~vote:None ~score:0 ~local:(Some 0) ~karma:0
      in
      Lwt.return_unit)

(* G — the decision comes from the TARGET's community, never from anything
   the client sends. The voter is banned in beta and not in alpha; a forged
   community_id field pointing the other way changes nothing either way. *)
let vote_target_community_binding_case =
  db_case
    "vote target ownership: the ban decision follows the post's own community, \
     not a submitted community_id" (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find q_user "sec_tbauthor" in
      let* author = or_fail "author" author in
      let* voter = C.find q_user "sec_tbvoter" in
      let* voter = or_fail "voter" voter in
      let* alpha = C.find q_community ("sec-target-alpha", "public") in
      let* alpha = or_fail "alpha" alpha in
      let* beta = C.find q_community ("sec-target-beta", "public") in
      let* beta = or_fail "beta" beta in
      let* post_alpha = C.find q_post ("sec alpha target", alpha, author) in
      let* post_alpha = or_fail "alpha post" post_alpha in
      let* post_beta = C.find q_post ("sec beta target", beta, author) in
      let* post_beta = or_fail "beta post" post_beta in
      (* Banned in beta only. *)
      let* r = C.exec q_community_ban (voter, beta) in
      let* () = or_fail "beta ban" r in

      let* cookie, _ = login ~url voter in
      let forged_vote ~post ~claimed =
        let* token = token_in ~url ~cookie in
        do_post ~url ~cookie ~target:"/vote" ~token
          [
            ("post_id", string_of_int post);
            ("direction", "1");
            (* Not a field the handler reads — the point is that adding it
               cannot move the decision. *)
            ("community_id", string_of_int claimed);
          ]
      in

      (* Voting in alpha, while claiming to be in beta (where they ARE
         banned): allowed, because the post lives in alpha. *)
      let* status, _, _ = forged_vote ~post:post_alpha ~claimed:beta in
      Alcotest.(check int)
        "alpha vote accepted despite the beta claim" 303 status;
      let* () =
        check_post_vote_state "alpha vote landed" c ~voter ~post:post_alpha
          ~author ~community:alpha ~vote:(Some 1) ~score:1 ~local:(Some 1)
          ~karma:1
      in

      (* Voting in beta, while claiming to be in alpha (where they are NOT
         banned): refused, because the post lives in beta. *)
      let* status, _, body = forged_vote ~post:post_beta ~claimed:alpha in
      Alcotest.(check int)
        "beta vote refused despite the alpha claim" 403 status;
      Alcotest.(check bool)
        "refused on the community ban" true
        (contains body "banned from this community");
      let* () =
        check_post_vote_state "nothing written in beta" c ~voter ~post:post_beta
          ~author ~community:beta ~vote:None ~score:0 ~local:None ~karma:1
      in
      Lwt.return_unit)

let avatar_suite = [ avatar_theft_case; avatar_preserved_case ]

let password_change_kills_reset_links_case =
  db_case "password change: outstanding reset links die with the old password"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = Earde.Auth.hash_password pw_old in
      let* hash = or_fail_s "hash fixture" hash in
      let* uid = C.find q_user_hashed ("sec_pwlinks", strip_nuls hash) in
      let* uid = or_fail "user" uid in
      let* created =
        Earde.Credential_store.create_token c "sec_pwlinks@sec.invalid"
          "sec-pre-change-link"
      in
      let* _ = or_fail_s "create" created in
      let* cookie, token = login ~url ~username:"sec_pwlinks" uid in
      let* status, _, body =
        do_post ~url ~cookie ~target:"/settings/password" ~token
          [
            ("old_password", pw_old);
            ("new_password", pw_new);
            ("confirm_password", pw_new);
          ]
      in
      Alcotest.(check int) "change accepted" 200 status;
      Alcotest.(check bool)
        "change applied" true
        (contains body "Password Changed");
      let* again =
        Earde.Credential_store.reset_password_atomically c "sec-pre-change-link"
          "sec-attacker-hash"
      in
      let* again = or_fail_s "old link reset" again in
      Alcotest.(check bool)
        "a link issued before the change cannot reset" false again;
      let* n = C.find q_count_reset_tokens uid in
      let* n = or_fail "tokens after" n in
      Alcotest.(check int) "no link survives the change" 0 n;
      Lwt.return_unit)

let session_suite =
  [
    session_revocation_case;
    password_reset_revocation_case;
    password_reset_kills_other_links_case;
    password_change_revocation_case;
    password_change_wrong_old_case;
    password_change_kills_reset_links_case;
    global_ban_revocation_case;
    ban_requires_admin_case;
  ]

let comment_parent_suite =
  [ parent_binding_case; parent_binding_no_side_effects_case ]

let upload_suite =
  [
    upload_authorization_ordering_case;
    upload_format_gate_case;
    upload_rate_limit_case;
  ]

let legacy_route_suite = [ legacy_mod_routes_case ]

let vote_ban_suite =
  [
    post_vote_community_ban_case;
    post_vote_removal_community_ban_case;
    comment_vote_community_ban_case;
    comment_vote_removal_community_ban_case;
    vote_global_ban_defense_in_depth_case;
    vote_unbanned_control_case;
    vote_target_community_binding_case;
  ]

let suites =
  (* The gated halves: each reproduces the original attack against the
       real handlers over the real routed pipeline, on durable SQL
       sessions. *)
  [
    ("security_avatar_ownership", avatar_suite);
    ("security_session_revocation", session_suite);
    ("security_comment_parent_binding", comment_parent_suite);
    ("security_upload_hardening", upload_suite);
    ("security_legacy_mod_routes", legacy_route_suite);
    ("security_vote_ban_enforcement", vote_ban_suite);
  ]
