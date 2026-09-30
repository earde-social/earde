(* The routed application for the shared-threads HTTP cases: pipeline,
   sessions, seeded communities and threads, and the read-back queries. *)

module An = Earde.Analytics
let ( let* ) = Lwt.bind
open Caqti_request.Infix

module H = Earde.Shared_thread_placement_handlers
module Store = Earde.Shared_thread_placement_store
let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let status_of = Http_fixture.status_of

let insert_community ?name conn slug =
  Community_fixture.insert_community ?name ~network:false conn slug

(* Durable authorization fixtures reused from the sibling suites: the
   real rows the read models and the store read. *)
let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" Community_fixture.q_insert_moderator
    (user, community, "top_mod")

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let q_ban_global =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE users SET is_banned = TRUE WHERE id = $1"

let q_ban_community =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_bans (user_id, community_id) VALUES ($1, $2)"

let q_insert_post =
  (Caqti_type.(t2 string (t2 int int)) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, community_id, user_id) \
   VALUES ($1, 'sth body', $2, $3) RETURNING id"

let q_count_for_post =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM shared_thread_placements WHERE post_id = $1"

let q_unread_kinds =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*) FROM notifications \
   WHERE user_id = $1 AND is_read = FALSE \
     AND notif_type LIKE 'shared_thread_%'"

(* Dependency order, exactly as the store suite: notifications, the
   placement audit trail (which RESTRICT-protects its subjects), the
   placements, the connection fixtures' trail and rows, then posts before
   the users and communities they reference. Bans, memberships, and
   moderator rows cascade from their parents. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications \
       WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'sth_%')"
    ; "DELETE FROM notifications \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM shared_thread_placement_audit_events \
       WHERE origin_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%') \
          OR destination_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM shared_thread_placements \
       WHERE origin_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%') \
          OR destination_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM community_connection_audit_events \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM community_connections \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM posts \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM community_sections \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'sth-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'sth-%'"
    ; "DELETE FROM users WHERE username LIKE 'sth_%'"
    ]

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
               (fun () -> f ~url conn)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* One sql_pool for the whole suite (pools are never closed; see
   Ccon_http). *)
let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

let app_pipeline ?session_user_id ?session_username ?(session_admin = false)
    ~url () =
  sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ (fun handler request ->
       match session_user_id with
       | None -> handler request
       | Some uid ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           (* Real logins always set the username beside the id; cases
              that assert username-gated chrome (composer join CTA,
              report links, mod controls) opt in explicitly so every
              pre-existing case keeps its exact bytes. *)
           let* () =
             match session_username with
             | Some name -> Dream.set_session_field request "username" name
             | None -> Lwt.return_unit
           in
           let* () =
             if session_admin then
               Dream.set_session_field request "is_admin" "true"
             else Lwt.return_unit
           in
           handler request)
  @@ Dream.router
       [ Dream.get "/mint" (fun req ->
             Dream.respond
               (Dream.csrf_token req ^ "\n"
              ^ Dream.csrf_token ~valid_for:(-60.) req));
         Dream.get "/c/:slug/t/:thread/share" H.make_share_page_handler;
         Dream.post "/c/:slug/t/:thread/share" H.make_share_request_handler;
         Dream.get "/c/:slug/settings/shared-threads"
           H.make_management_page_handler;
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/accept"
           H.make_accept_handler;
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/reject"
           H.make_reject_handler;
         Dream.post
           "/c/:slug/settings/shared-threads/:placement_id/withdraw"
           H.make_withdrawal_handler;
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/remove"
           H.make_removal_handler;
         (* The surrounding surfaces the slice touches or must leave
            untouched: the canonical thread page (entry point), the
            legacy post redirect, the community/section feeds (boundary),
            the settings index (navigation), and the notification
            center. *)
         Dream.get "/c/:slug/t/:thread" Earde.Handlers.view_thread_handler;
         Dream.get "/p/:id" Earde.Handlers.view_post_handler;
         Dream.get "/c/:slug" Earde.Handlers.community_page_handler;
         Dream.get "/c/:slug/s/:section_slug"
           Earde.Handlers.community_section_handler;
         Dream.get "/c/:slug/settings"
           Earde.Handlers.community_settings_handler;
         Dream.get "/notifications" Earde.Handlers.notifications_handler;
         (* Slice 3 read-side surfaces: the comment write path, the
            origin-scoped moderation mutations the destination context
            must NOT unlock, and the deferred discovery surfaces the
            slice must leave untouched. *)
         Dream.post "/comments" Earde.Handlers.create_comment_handler;
         Dream.post "/c/:slug/posts/:id/mod_delete"
           Earde.Handlers.mod_delete_post_handler;
         Dream.post "/c/:slug/comments/:id/mod_delete"
           Earde.Handlers.mod_delete_comment_handler;
         Dream.get "/feed" Earde.Handlers.feed_handler;
         Dream.get "/search" Earde.Handlers.search_handler;
         Dream.get "/u/:username" Earde.Handlers.view_profile_handler;
         (* Slice 4: the real durable-thread composer pair. *)
         Dream.get "/new-post" Earde.Handlers.new_post_page;
         Dream.post "/posts" Earde.Handlers.create_post_handler;
       ]

let mint label pipeline =
  let* response =
    pipeline (Dream.request ~method_:`GET ~target:"/mint" "")
  in
  let cookie = Http_fixture.session_cookie label response in
  let* body = Dream.body response in
  match String.split_on_char '\n' body with
  | [ fresh; expired ] -> Lwt.return (cookie, fresh, expired)
  | _ -> Alcotest.failf "%s: unexpected mint body" label

let get ?cookie ~target pipeline = Http_fixture.do_get ?cookie ~target pipeline

let send_post ~cookie ~target ~fields pipeline =
  let headers =
    [ ("Content-Type", "application/x-www-form-urlencoded");
      ("Cookie", cookie);
    ]
  in
  let* response =
    pipeline
      (Dream.request ~method_:`POST ~target ~headers (Http_fixture.form_body fields))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* The composer posts multipart/form-data (file upload); Dream.multipart
   verifies the dream.csrf part exactly like Dream.form. consent joins the
   Cookie header only in the analytics-observation cases. *)
let send_multipart ~cookie ?(consent = false) ~target ~fields pipeline =
  let cookie =
    if consent then cookie ^ "; " ^ An.consent_cookie_name ^ "=granted"
    else cookie
  in
  let headers =
    [ ( "Content-Type",
        "multipart/form-data; boundary=" ^ Http_fixture.multipart_boundary );
      ("Cookie", cookie);
    ]
  in
  let* response =
    pipeline
      (Dream.request ~method_:`POST ~target ~headers
         (Http_fixture.multipart_body fields))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

let session ~url ~uid ?username ?(admin = false) () =
  app_pipeline ~session_user_id:uid ?session_username:username
    ~session_admin:admin ~url ()

let acting label ~url ~uid ?(admin = false) () =
  let pipeline = session ~url ~uid ~admin () in
  let* cookie, token, expired = mint label pipeline in
  Lwt.return (pipeline, cookie, token, expired)

let share_path ~slug ~post = Printf.sprintf "/c/%s/t/%d/share" slug post

let mgmt_path slug = Printf.sprintf "/c/%s/settings/shared-threads" slug

(* One author (member of the origin), a top moderator on each side, a
   connected origin/destination pair with a flat destination, and one
   canonical post by the author. *)
let fixture conn tag =
  let* author = insert_user conn ("sth_" ^ tag ^ "_author") in
  let* otop = insert_user conn ("sth_" ^ tag ^ "_otop") in
  let* dtop = insert_user conn ("sth_" ^ tag ^ "_dtop") in
  let* o =
    insert_community ~name:("Sth " ^ tag ^ " Origin") conn
      ("sth-" ^ tag ^ "-o")
  in
  let* d =
    insert_community ~name:("Sth " ^ tag ^ " Dest") conn
      ("sth-" ^ tag ^ "-d")
  in
  let* () = exec conn "flat destination" Shared_thread_fixture.q_set_sections (d, false) in
  let* () = add_member conn ~user:author ~community:o in
  let* () = add_top_mod conn ~user:otop ~community:o in
  let* () = add_top_mod conn ~user:dtop ~community:d in
  let* connection = Shared_thread_fixture.connect conn ~actor:otop o d in
  let* post =
    find conn "post fixture" q_insert_post
      ("Sth " ^ tag ^ " thread", (o, author))
  in
  Lwt.return (author, otop, dtop, o, d, post, connection)

let seed_request conn ~actor ?note ~post ~destination () =
  let* r =
    Store.request conn ~actor_user_id:actor ~post_id:post
      ~destination_community_id:destination ~request_note:note
  in
  match r with
  | Ok created -> Lwt.return (Store.created_placement_id created)
  | Error e -> Alcotest.failf "seed request: %s" (Shared_thread_fixture.error_str e)

let seed_accept conn ~reviewer ?section ~placement ~destination () =
  let* r =
    Store.review conn ~reviewer_user_id:reviewer ~placement_id:placement
      ~destination_community_id:destination
      ~decision:(Store.Accept section)
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "seed accept: %s" (Shared_thread_fixture.error_str e)

let check_location label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some expected)
    (Dream.header response "Location")

let q_restore_content =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE posts SET content = 'sth body' WHERE id = $1"

let q_insert_sectioned_post =
  (Caqti_type.(t2 string (t3 int int int)) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, community_id, user_id, section_id) \
   VALUES ($1, 'sth body', $2, $3, $4) RETURNING id"

let q_vote =
  (Caqti_type.(t3 int int int) ->. Caqti_type.unit)
  "INSERT INTO post_votes (user_id, post_id, direction) VALUES ($1, $2, $3)"

let q_age_post =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "UPDATE posts SET created_at = NOW() - make_interval(hours => $2::int), \
   last_activity_at = NOW() - make_interval(hours => $2::int) WHERE id = $1"

let q_set_activity =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "UPDATE posts SET last_activity_at = NOW() - make_interval(hours => $2::int) \
   WHERE id = $1"

let q_origin_section_of =
  (Caqti_type.int ->! Caqti_type.(option int))
  "SELECT section_id FROM posts WHERE id = $1"

let q_post_content =
  (Caqti_type.int ->! Caqti_type.(option string))
  "SELECT content FROM posts WHERE id = $1"

let q_comment_count =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM comments WHERE post_id = $1"

let q_local_comment_count =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COALESCE((SELECT local_comment_count FROM community_user_stats \
                    WHERE user_id = $1 AND community_id = $2), 0)::int"

let ok label = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: %s" label e

let feed_ids (items : Earde.Db.feed_item list) =
  List.map
    (fun (it : Earde.Db.feed_item) -> it.Earde.Db.fi_post.id)
    items

let reject_seed conn ~reviewer ~placement ~destination =
  let* r =
    Store.review conn ~reviewer_user_id:reviewer ~placement_id:placement
      ~destination_community_id:destination ~decision:Store.Reject
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "seed reject: %s" (Shared_thread_fixture.error_str e)

let withdraw_seed conn ~actor ~placement ~origin =
  let* r =
    Store.withdraw conn ~actor_user_id:actor ~placement_id:placement
      ~origin_community_id:origin
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "seed withdraw: %s" (Shared_thread_fixture.error_str e)

let remove_seed conn ~actor ~placement ~acting =
  let* r =
    Store.remove conn ~actor_user_id:actor ~placement_id:placement
      ~acting_community_id:acting
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error e -> Alcotest.failf "seed remove: %s" (Shared_thread_fixture.error_str e)

let may_comment label conn ~user ~post expected =
  let* r =
    Earde.Shared_thread_reading.viewer_may_comment conn ~user_id:user
      ~post_id:post
  in
  (match r with
   | Ok got -> Alcotest.(check bool) label expected got
   | Error _ -> Alcotest.failf "%s: storage error" label);
  Lwt.return_unit

let check_301 label expected response =
  Alcotest.(check int) (label ^ ": 301") 301 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some expected)
    (Dream.header response "Location")

let count label body needle expected =
  Alcotest.(check int) label expected (Html_assert.count_sub body needle)
