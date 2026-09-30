(* Fail-closed ban checks (A0).

   Every gate that must know a user's CURRENT durable ban state used to read
   the lookup as [Error _ -> false]: a storage failure while deciding "is this
   user banned?" was silently taken as "not banned", and the protected
   mutation went through. These cases force the ban read ITSELF to fail while
   every other query in the handler still works, and assert that the handler
   answers through the generic error boundary and writes nothing.

   Injection reuses the technique Oss_boundaries already uses for its
   forced-failure disclosure cases — a pool whose search_path points at a
   partial schema — sharpened to a single table: [shadow] mirrors every public
   relation as an auto-updatable view EXCEPT the ones named, so a handler runs
   normally until it reaches a query against an omitted table, which then
   fails with a real PostgreSQL error. Omitting `users` breaks
   Admin_store.is_globally_banned; omitting `community_bans` breaks
   Community_ban_store.is_banned. Nothing about production code is changed to make
   this possible: no failpoint, no test hook. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let contains hay needle = Html_assert.contains hay needle

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let shadow = "banfc_shadow"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP SCHEMA IF EXISTS banfc_shadow CASCADE";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'banfc\\_%')";
      "DELETE FROM reports WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'banfc-%')";
      "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE \
       community_id IN (SELECT id FROM communities WHERE slug LIKE 'banfc-%'))";
      "DELETE FROM comments WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'banfc\\_%')";
      "DELETE FROM thread_source_messages WHERE post_id IN (SELECT id FROM \
       posts WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'banfc-%'))";
      "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'banfc-%')";
      "DELETE FROM chat_messages WHERE channel_id IN (SELECT id FROM channels \
       WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'banfc-%'))";
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'banfc-%')";
      "DELETE FROM community_bans WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'banfc-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'banfc-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'banfc-%')";
      "DELETE FROM community_user_stats WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'banfc-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'banfc-%')";
      "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT \
       'community:' || c.id::text FROM communities c WHERE c.slug LIKE \
       'banfc-%')";
      "DELETE FROM communities WHERE slug LIKE 'banfc-%'";
      "DELETE FROM users WHERE username LIKE 'banfc\\_%'";
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
             Security_fixture.ensure_uploads_dir ();
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === the partial-schema injection === *)

let q_drop_shadow =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP SCHEMA IF EXISTS banfc_shadow CASCADE"

let q_create_shadow =
  (Caqti_type.unit ->. Caqti_type.unit) "CREATE SCHEMA banfc_shadow"

(* Every relation a query could name unqualified: ordinary and partitioned
   tables, views, materialized views, foreign tables. *)
let q_public_relations =
  (Caqti_type.unit ->* Caqti_type.string)
    "SELECT c.relname FROM pg_class c\n\
    \    JOIN pg_namespace n ON n.oid = c.relnamespace\n\
    \   WHERE n.nspname = 'public' AND c.relkind IN ('r', 'p', 'v', 'm', 'f')\n\
    \   ORDER BY c.relname"

let make_shadow (module C : Caqti_lwt.CONNECTION) ~omit =
  let* r = C.exec q_drop_shadow () in
  let* () = or_fail "drop shadow" r in
  let* r = C.exec q_create_shadow () in
  let* () = or_fail "create shadow" r in
  let* relations = C.collect_list q_public_relations () in
  let* relations = or_fail "public relations" relations in
  (* A typo in [omit] would silently disarm the whole case: the handler
     would then see a complete schema and pass, "proving" nothing. *)
  List.iter
    (fun t ->
      if not (List.mem t relations) then
        Alcotest.failf "omit names a relation that does not exist: %S" t)
    omit;
  Lwt_list.iter_s
    (fun t ->
      if List.mem t omit then Lwt.return_unit
      else
        let sql =
          Printf.sprintf "CREATE VIEW %s.\"%s\" AS SELECT * FROM public.\"%s\""
            shadow t t
        in
        let* r = C.exec ((Caqti_type.unit ->. Caqti_type.unit) sql) () in
        let* () = or_fail ("mirror " ^ t) r in
        Lwt.return_unit)
    relations

let with_shadow url =
  Uri.to_string
    (Uri.add_query_param' (Uri.of_string url)
       ("options", "-csearch_path=" ^ shadow))

(* Omitting `users` breaks Admin_store.is_globally_banned and nothing else the gates
   below reach first; omitting `community_bans` breaks
   Community_ban_store.is_banned only. *)
let global_read_broken = [ "users" ]
let local_read_broken = [ "community_bans" ]
let generic = "A database error occurred. Please try again later."

let db_needles =
  [
    "banfc_shadow";
    "search_path";
    "postgresql";
    "caqti";
    "relation";
    "select";
    "does not exist";
  ]

let must label body s =
  if not (contains body s) then
    Alcotest.failf "%s: expected body to contain %S" label s

let must_not label body s =
  if contains (String.lowercase_ascii body) (String.lowercase_ascii s) then
    Alcotest.failf "%s: body leaks %S" label s

(* One assertion used by every failure case: the safe generic boundary, and
   no driver detail anywhere in the body. *)
let check_safe_failure label ~expect_status response body =
  Alcotest.(check int)
    (label ^ ": status") expect_status
    (Dream.status_to_int (Dream.status response));
  must label body generic;
  List.iter (must_not label body) db_needles

(* === fixtures === *)

let q_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@banfc.invalid', 'x', TRUE) RETURNING id"

let q_community =
  (Caqti_type.string ->! Caqti_type.int)
    (* sections_enabled FALSE: a sectioned community would make create_post
     fail its section validation instead of reaching the write. *)
    "INSERT INTO communities (slug, name, visibility, sections_enabled)\n\
    \   VALUES ($1, $1, 'public', FALSE) RETURNING id"

let q_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

let q_community_ban =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_bans (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

(* Sets the durable flag directly: the production ban handler also revokes
   the user's sessions, which would hide the very gate under test. *)
let q_global_ban =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

let q_global_unban =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = FALSE WHERE id = $1"

let q_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)\n\
    \   VALUES ($1, 'banfc body', $2, $3) RETURNING id"

let q_channel =
  (Caqti_type.(t2 string int) ->! Caqti_type.int)
    "INSERT INTO channels (slug, name, community_id) VALUES ($1, $1, $2)\n\
    \   RETURNING id"

let q_chat_message =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int64)
    "INSERT INTO chat_messages (content, channel_id, user_id)\n\
    \   VALUES ($1, $2, $3) RETURNING id"

let q_count_posts =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM posts WHERE community_id = $1"

let q_count_comments =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM comments WHERE post_id = $1"

let q_count_messages =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM chat_messages WHERE channel_id = $1"

let q_count_reports =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM reports WHERE community_id = $1"

let q_count_notifs =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM notifications WHERE user_id = $1"

let count label (module C : Caqti_lwt.CONNECTION) q arg =
  let* n = C.find q arg in
  or_fail label n

(* === runners === *)

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let boundary = "banfcboundary"

let multipart_body ?file fields =
  String.concat ""
    (List.map
       (fun (k, v) ->
         Printf.sprintf
           "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
           boundary k v)
       fields)
  ^ (match file with
    | None -> ""
    | Some (field, filename, bytes) ->
        Printf.sprintf
          "--%s\r\n\
           Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n\
           Content-Type: image/png\r\n\
           \r\n\
           %s\r\n"
          boundary field filename bytes)
  ^ Printf.sprintf "--%s--\r\n" boundary

let session uid = [ ("user_id", string_of_int uid); ("username", "banfc") ]

let run_post ~url ?(multipart = false) ?file ?accept ~session:sess ~target ~form
    handler =
  let multipart = multipart || file <> None in
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) sess
    in
    let fields = ("dream.csrf", Dream.csrf_token req) :: form in
    Dream.set_body req
      (if multipart then multipart_body ?file fields else form_body fields);
    handler req
  in
  let headers =
    [
      ( "Content-Type",
        if multipart then "multipart/form-data; boundary=" ^ boundary
        else "application/x-www-form-urlencoded" );
    ]
    @ match accept with Some a -> [ ("Accept", a) ] | None -> []
  in
  let* response = pipeline (Dream.request ~method_:`POST ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

let run_get ~url ~session:sess ~target handler =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) sess
    in
    handler req
  in
  let* response = pipeline (Dream.request ~method_:`GET ~target "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* === create_post === *)

(* static/uploads is filesystem state, which the SQL cleanup in [db_case]
   cannot undo — and the control below deliberately drives the real
   conversion, so it really does store a WebP. Restore the directory to the
   listing taken before the case: remove only names this case added, and
   never a name that was already present, so a developer's own uploads
   survive. Tolerates a file that has already gone, and is idempotent, so it
   is safe to call both on the success path and from the finalizer. *)
let restore_uploads baseline =
  List.iter
    (fun name ->
      if not (List.mem name baseline) then
        try Sys.remove (Filename.concat Security_fixture.uploads_dir name)
        with Sys_error _ -> ())
    (Security_fixture.uploads_listing ())

let create_post_case =
  db_case
    "create post: a failing ban read is a storage error — no image, no post"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      (* Snapshot first, and hold it across the whole case: the finalizer
         below must be able to undo an upload made by any step, including one
         that aborted mid-assertion. *)
      let uploads_before = Security_fixture.uploads_listing () in
      Lwt.finalize
        (fun () ->
          let* uid = C.find q_user "banfc_poster" in
          let* uid = or_fail "user" uid in
          let* cid = C.find q_community "banfc-post" in
          let* cid = or_fail "community" cid in
          let* r = C.exec q_member (uid, cid) in
          let* () = or_fail "member" r in
          let form =
            [
              ("title", "banfc post");
              ("content", "banfc body");
              ("community_id", string_of_int cid);
              ("url", "");
            ]
          in
          (* A genuine PNG in the field the handler actually reads, so the
           ImageMagick conversion really would run if a gate let it. *)
          let attempt label ~url =
            run_post ~url ~session:(session uid) ~target:"/create-post"
              ~file:("image", "banfc.png", Security_fixture.real_png)
              ~form Earde.Post_handlers.create_post_handler
            |> Lwt.map (fun (response, body) -> (label, response, body))
          in

          (* A — the global-ban read fails. *)
          let* () = make_shadow (module C) ~omit:global_read_broken in
          let* label, response, body =
            attempt "global read" ~url:(with_shadow url)
          in
          check_safe_failure label ~expect_status:500 response body;
          let* n = count "posts" (module C) q_count_posts cid in
          Alcotest.(check int) "global read: no post written" 0 n;
          Alcotest.(check (list string))
            "global read: static/uploads untouched" uploads_before
            (Security_fixture.uploads_listing ());

          (* B — the global read works, the community-ban read fails. *)
          let* () = make_shadow (module C) ~omit:local_read_broken in
          let* label, response, body =
            attempt "local read" ~url:(with_shadow url)
          in
          check_safe_failure label ~expect_status:500 response body;
          let* n = count "posts" (module C) q_count_posts cid in
          Alcotest.(check int) "local read: no post written" 0 n;
          Alcotest.(check (list string))
            "local read: static/uploads untouched" uploads_before
            (Security_fixture.uploads_listing ());

          (* C — the control: on an intact schema the very same submission is
           accepted, so the two refusals above are the ban reads failing and
           not a broken fixture. *)
          let* _, response, _ = attempt "control" ~url in
          Alcotest.(check int)
            "control: post accepted" 303
            (Dream.status_to_int (Dream.status response));
          let* n = count "posts" (module C) q_count_posts cid in
          Alcotest.(check int) "control: exactly one post" 1 n;
          (* The control really did convert and store an image, so the two
           "untouched" assertions above are about the gate refusing, not about
           an upload path that never works here. *)
          Alcotest.(check int)
            "control: the image was stored"
            (List.length uploads_before + 1)
            (List.length (Security_fixture.uploads_listing ()));

          (* Teardown, asserted here rather than inside the finalizer, where a
           failure would mask whatever exception was already unwinding. The
           finalizer repeats it for the paths that never reach this line. *)
          restore_uploads uploads_before;
          Alcotest.(check (list string))
            "teardown restores static/uploads to its baseline" uploads_before
            (Security_fixture.uploads_listing ());
          Lwt.return_unit)
        (fun () ->
          restore_uploads uploads_before;
          Lwt.return_unit))

(* === create_comment === *)

let create_comment_case =
  db_case
    "create comment: a failing ban read writes no comment, notification or \
     counter" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* author = C.find q_user "banfc_cauthor" in
      let* author = or_fail "author" author in
      let* uid = C.find q_user "banfc_commenter" in
      let* uid = or_fail "commenter" uid in
      let* cid = C.find q_community "banfc-comment" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_member (uid, cid) in
      let* () = or_fail "member" r in
      let* pid = C.find q_post ("banfc comment target", cid, author) in
      let* pid = or_fail "post" pid in
      let attempt label ~url =
        run_post ~url ~session:(session uid) ~target:"/create-comment"
          ~form:[ ("content", "banfc comment"); ("post_id", string_of_int pid) ]
          Earde.Comment_handlers.create_comment_handler
        |> Lwt.map (fun (response, body) -> (label, response, body))
      in

      let* () = make_shadow (module C) ~omit:global_read_broken in
      let* label, response, body =
        attempt "global read" ~url:(with_shadow url)
      in
      check_safe_failure label ~expect_status:500 response body;
      let* n = count "comments" (module C) q_count_comments pid in
      Alcotest.(check int) "global read: no comment" 0 n;
      let* n = count "notifs" (module C) q_count_notifs author in
      Alcotest.(check int) "global read: no reply notification" 0 n;

      let* () = make_shadow (module C) ~omit:local_read_broken in
      let* label, response, body =
        attempt "local read" ~url:(with_shadow url)
      in
      check_safe_failure label ~expect_status:500 response body;
      let* n = count "comments" (module C) q_count_comments pid in
      Alcotest.(check int) "local read: no comment" 0 n;
      let* n = count "notifs" (module C) q_count_notifs author in
      Alcotest.(check int) "local read: no reply notification" 0 n;

      let* _, response, _ = attempt "control" ~url in
      Alcotest.(check int)
        "control: comment accepted" 303
        (Dream.status_to_int (Dream.status response));
      let* n = count "comments" (module C) q_count_comments pid in
      Alcotest.(check int) "control: exactly one comment" 1 n;
      Lwt.return_unit)

(* === send_message === *)

let send_message_case =
  db_case
    "chat send: a failing ban read persists no message, in either response mode"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find q_user "banfc_chatter" in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_community "banfc-chat" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_member (uid, cid) in
      let* () = or_fail "member" r in
      let* chid = C.find q_channel ("banfc-general", cid) in
      let* chid = or_fail "channel" chid in
      let attempt ?accept ~url () =
        run_post ~url ?accept ~session:(session uid) ~target:"/send-message"
          ~form:
            [
              ("community_slug", "banfc-chat");
              ("channel_slug", "banfc-general");
              ("content", "banfc hello");
            ]
          Earde.Chat_handlers.send_message_handler
      in
      let no_message label =
        let* n = count "messages" (module C) q_count_messages chid in
        Alcotest.(check int) (label ^ ": nothing persisted") 0 n;
        Lwt.return_unit
      in

      (* JSON callers keep the safe internal contract; the failure must not
         become a 200 with a fabricated message row either. *)
      let* () = make_shadow (module C) ~omit:global_read_broken in
      let* response, body =
        attempt ~accept:"application/json" ~url:(with_shadow url) ()
      in
      Alcotest.(check int)
        "json global read: 500" 500
        (Dream.status_to_int (Dream.status response));
      must "json global read" body "Something went wrong. Please try again.";
      List.iter (must_not "json global read" body) db_needles;
      let* () = no_message "json global read" in

      let* () = make_shadow (module C) ~omit:local_read_broken in
      let* response, body =
        attempt ~accept:"application/json" ~url:(with_shadow url) ()
      in
      Alcotest.(check int)
        "json local read: 500" 500
        (Dream.status_to_int (Dream.status response));
      must "json local read" body "Something went wrong. Please try again.";
      List.iter (must_not "json local read" body) db_needles;
      let* () = no_message "json local read" in

      (* The no-JS form path keeps its own generic HTML contract. *)
      let* response, body = attempt ~url:(with_shadow url) () in
      Alcotest.(check bool)
        "html local read: not a success redirect" false
        (Dream.status_to_int (Dream.status response) = 303);
      must "html local read" body generic;
      List.iter (must_not "html local read" body) db_needles;
      let* () = no_message "html local read" in

      let* response, _ = attempt ~url () in
      Alcotest.(check int)
        "control: message accepted" 303
        (Dream.status_to_int (Dream.status response));
      let* n = count "messages" (module C) q_count_messages chid in
      Alcotest.(check int) "control: exactly one message" 1 n;
      Lwt.return_unit)

(* === reports === *)

let report_router =
  Dream.router
    [
      Dream.get "/c/:slug/report" Earde.Moderation_handlers.report_form_handler;
      Dream.post "/c/:slug/reports"
        Earde.Moderation_handlers.create_report_handler;
    ]

let report_case =
  db_case "report: a failing ban read renders no form and inserts no report"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* author = C.find q_user "banfc_rauthor" in
      let* author = or_fail "author" author in
      let* uid = C.find q_user "banfc_reporter" in
      let* uid = or_fail "reporter" uid in
      let* cid = C.find q_community "banfc-report" in
      let* cid = or_fail "community" cid in
      let* pid = C.find q_post ("banfc report target", cid, author) in
      let* pid = or_fail "post" pid in
      let get_form ~url =
        run_get ~url ~session:(session uid)
          ~target:("/c/banfc-report/report?type=post&id=" ^ string_of_int pid)
          report_router
      in
      let post_report ~url =
        run_post ~url ~session:(session uid) ~target:"/c/banfc-report/reports"
          ~form:
            [
              ("target_type", "post");
              ("target_id", string_of_int pid);
              ("reason", "spam");
              ("details", "banfc details");
            ]
          report_router
      in

      let* () = make_shadow (module C) ~omit:global_read_broken in
      let* response, body = get_form ~url:(with_shadow url) in
      check_safe_failure "GET global read" ~expect_status:500 response body;
      let* response, body = post_report ~url:(with_shadow url) in
      check_safe_failure "POST global read" ~expect_status:500 response body;
      let* n = count "reports" (module C) q_count_reports cid in
      Alcotest.(check int) "global read: no report row" 0 n;

      let* () = make_shadow (module C) ~omit:local_read_broken in
      let* response, body = get_form ~url:(with_shadow url) in
      check_safe_failure "GET local read" ~expect_status:500 response body;
      let* response, body = post_report ~url:(with_shadow url) in
      check_safe_failure "POST local read" ~expect_status:500 response body;
      let* n = count "reports" (module C) q_count_reports cid in
      Alcotest.(check int) "local read: no report row" 0 n;

      (* Control: the same reporter, form and target on an intact schema. *)
      let* response, body = get_form ~url in
      Alcotest.(check int)
        "control: form renders" 200
        (Dream.status_to_int (Dream.status response));
      must "control form" body "Report";
      let* _, body = post_report ~url in
      must "control post" body "Report submitted";
      let* n = count "reports" (module C) q_count_reports cid in
      Alcotest.(check int) "control: exactly one report" 1 n;
      Lwt.return_unit)

(* === start thread === *)

let start_thread_router =
  Dream.router
    [
      Dream.get "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
        Earde.Start_thread_handlers.start_thread_form_handler;
      Dream.post "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
        Earde.Start_thread_handlers.start_thread_create_handler;
    ]

let start_thread_case =
  db_case
    "start thread: a failing global-ban read is Start_error, not permission"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find q_user "banfc_starter" in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_community "banfc-start" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_member (uid, cid) in
      let* () = or_fail "member" r in
      let* chid = C.find q_channel ("banfc-start-ch", cid) in
      let* chid = or_fail "channel" chid in
      let* mid = C.find q_chat_message ("banfc seed", chid, uid) in
      let* mid = or_fail "seed message" mid in
      let target =
        Printf.sprintf
          "/c/banfc-start/ch/banfc-start-ch/messages/%Ld/start-thread" mid
      in

      let* () = make_shadow (module C) ~omit:global_read_broken in
      let* response, body =
        run_get ~url:(with_shadow url) ~session:(session uid) ~target
          start_thread_router
      in
      check_safe_failure "GET global read" ~expect_status:500 response body;
      let* response, body =
        run_post ~url:(with_shadow url) ~session:(session uid) ~target
          ~form:[ ("title", "banfc thread"); ("content", "banfc body") ]
          start_thread_router
      in
      check_safe_failure "POST global read" ~expect_status:500 response body;
      let* n = count "posts" (module C) q_count_posts cid in
      Alcotest.(check int) "global read: no thread created" 0 n;

      (* Control: the same request on an intact schema reaches the form. *)
      let* response, _ =
        run_get ~url ~session:(session uid) ~target start_thread_router
      in
      Alcotest.(check int)
        "control: form renders" 200
        (Dream.status_to_int (Dream.status response));
      Lwt.return_unit)

(* === real bans are unchanged === *)

let real_bans_case =
  db_case "actual bans keep their existing refusals on post, comment and chat"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find q_user "banfc_banned" in
      let* uid = or_fail "user" uid in
      let* author = C.find q_user "banfc_bauthor" in
      let* author = or_fail "author" author in
      let* cid = C.find q_community "banfc-bans" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_member (uid, cid) in
      let* () = or_fail "member" r in
      let* chid = C.find q_channel ("banfc-bans-ch", cid) in
      let* chid = or_fail "channel" chid in
      let* pid = C.find q_post ("banfc ban target", cid, author) in
      let* pid = or_fail "post" pid in

      let post_attempt () =
        run_post ~url ~multipart:true ~session:(session uid)
          ~target:"/create-post"
          ~form:
            [
              ("title", "banfc banned post");
              ("content", "banfc body");
              ("community_id", string_of_int cid);
              ("url", "");
            ]
          Earde.Post_handlers.create_post_handler
      in
      let comment_attempt () =
        run_post ~url ~session:(session uid) ~target:"/create-comment"
          ~form:
            [
              ("content", "banfc banned comment"); ("post_id", string_of_int pid);
            ]
          Earde.Comment_handlers.create_comment_handler
      in
      let chat_attempt () =
        run_post ~url ~session:(session uid) ~target:"/send-message"
          ~form:
            [
              ("community_slug", "banfc-bans");
              ("channel_slug", "banfc-bans-ch");
              ("content", "banfc chat");
            ]
          Earde.Chat_handlers.send_message_handler
      in
      let refused label (response, body) needle =
        Alcotest.(check int)
          (label ^ ": 403") 403
          (Dream.status_to_int (Dream.status response));
        must label body needle;
        Lwt.return_unit
      in

      (* Community ban. *)
      let* r = C.exec q_community_ban (uid, cid) in
      let* () = or_fail "community ban" r in
      let* out = post_attempt () in
      let* () = refused "post/community ban" out "banned from posting" in
      let* out = comment_attempt () in
      let* () = refused "comment/community ban" out "banned from commenting" in
      let* out = chat_attempt () in
      let* () = refused "chat/community ban" out "banned from this community" in

      (* Global ban, on a user with no community ban left. *)
      let* r = C.exec q_global_ban uid in
      let* () = or_fail "global ban" r in
      let* out = post_attempt () in
      let* () = refused "post/global ban" out "permanently banned" in
      let* out = comment_attempt () in
      let* () = refused "comment/global ban" out "permanently banned" in
      let* out = chat_attempt () in
      let* () = refused "chat/global ban" out "permanently banned" in

      let* n = count "posts" (module C) q_count_posts cid in
      Alcotest.(check int) "no post written by a banned user" 1 n;
      let* n = count "comments" (module C) q_count_comments pid in
      Alcotest.(check int) "no comment written by a banned user" 0 n;
      let* n = count "messages" (module C) q_count_messages chid in
      Alcotest.(check int) "no message written by a banned user" 0 n;
      let* r = C.exec q_global_unban uid in
      or_fail "global unban" r)

let ban_fail_closed_suite =
  [
    create_post_case;
    create_comment_case;
    send_message_case;
    report_case;
    start_thread_case;
    real_bans_case;
  ]

let suites =
  (* Fail-closed ban checks: a ban lookup that cannot be answered from
       storage must not authorize the mutation it guards. *)
  [ ("security_ban_check_fail_closed", ban_fail_closed_suite) ]
