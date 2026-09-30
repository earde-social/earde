(* TARGET admin immunity, as opposed to the ACTOR authority Stale_admin pins.
   Two moderation mutations are forbidden when their TARGET is a durable
   global administrator. Both used to read that status through a boolean that
   folded [Error] into "not an admin", so a storage failure in the immunity
   lookup authorized exactly the mutation the immunity exists to stop. These
   cases pin both halves: the immunity still holds when the answer is yes, and
   an unanswerable lookup performs nothing at all. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let contains hay needle = Html_assert.contains hay needle

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let shadow = "tgim_shadow"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP SCHEMA IF EXISTS tgim_shadow CASCADE";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'tgim\\_%')";
      "DELETE FROM mod_actions WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE \
       community_id IN (SELECT id FROM communities WHERE slug LIKE 'tgim-%'))";
      "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'tgim-%')";
      "DELETE FROM chat_messages WHERE channel_id IN (SELECT id FROM channels \
       WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'tgim-%'))";
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'tgim-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM community_bans WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM community_user_stats WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'tgim-%')";
      "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT \
       'community:' || c.id::text FROM communities c WHERE c.slug LIKE \
       'tgim-%')";
      "DELETE FROM communities WHERE slug LIKE 'tgim-%'";
      "DELETE FROM dream_session WHERE payload LIKE '%tgim\\_%'";
      "DELETE FROM users WHERE username LIKE 'tgim\\_%'";
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

(* === fixtures === *)

let password = "tgim password"

let q_user =
  (Caqti_type.(t3 string string bool) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified, \
     is_admin)\n\
    \   VALUES ($1, $1 || '@tgim.invalid', $2, TRUE, $3) RETURNING id"

let q_community =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility, sections_enabled)\n\
    \   VALUES ($1, $1, 'public', FALSE) RETURNING id"

let q_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role)\n\
    \   VALUES ($1, $2, 'moderator') ON CONFLICT DO NOTHING"

let q_post =
  (Caqti_type.(t4 string int int (option string)) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id, image_url)\n\
    \   VALUES ($1, 'tgim body', $2, $3, $4) RETURNING id"

(* === the assertions the two mutations are judged by === *)

let q_is_community_banned =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM community_bans\n\
    \    WHERE user_id = $1 AND community_id = $2"

let q_count_mod_actions =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM mod_actions\n\
    \    WHERE community_id = $1 AND target_id = $2"

let q_count_notifs =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM notifications WHERE user_id = $1"

let q_post_state =
  (Caqti_type.int ->! Caqti_type.(t2 (option string) (option string)))
    "SELECT content, image_url FROM posts WHERE id = $1"

(* Argon2 is deliberately expensive: hash the fixture password once. *)
let shared_hash = ref None

let password_hash () =
  match !shared_hash with
  | Some h -> Lwt.return h
  | None ->
      let* hash = Earde.Auth.hash_password password in
      let* hash = or_fail_s "hash" hash in
      (* Argon2 hands back a NUL-padded buffer; the trailing bytes must not
         reach the column or the login lookup compares the wrong string. *)
      let hash =
        let n = ref (String.length hash) in
        while !n > 0 && hash.[!n - 1] = '\000' do
          decr n
        done;
        String.sub hash 0 !n
      in
      shared_hash := Some hash;
      Lwt.return hash

let make_user (module C : Caqti_lwt.CONNECTION) ~admin username =
  let* hash = password_hash () in
  let* uid = C.find q_user (username, hash, admin) in
  or_fail username uid

(* === fault injection ===

   The two target-immunity lookups both read `users`, and so does the actor
   authorization that must succeed FIRST — omitting the whole table the way
   Stale_admin does would break actor authority instead, and the resulting
   500 would prove nothing about the immunity.

   So the shadow schema mirrors every public relation as an auto-updatable
   view, including `users`, and poisons `users` in exactly one place: the
   `is_admin` column raises division_by_zero, and only for one named row.
   Reading any other user's `is_admin` still works, and reading any other
   column of the poisoned row (the username lookup selects id, username and
   email only) still works, because CASE evaluates its arms per row and only
   for a projected column.

   `(1 / (u.id - u.id))` cannot be constant-folded at plan time — it depends
   on a column — so the failure is genuinely data-dependent rather than a
   query that fails for everyone. No production failpoint exists or is
   added, and the poison lives entirely inside a schema this module drops. *)
let q_poisoned_users_columns =
  (Caqti_type.int ->! Caqti_type.string)
    "SELECT string_agg(\n\
    \            CASE WHEN a.attname = 'is_admin'\n\
    \                 THEN 'CASE WHEN u.id = ' || $1::text\n\
    \                      || ' THEN (1 / (u.id - u.id)) = 1 ELSE u.is_admin \
     END'\n\
    \                      || ' AS is_admin'\n\
    \                 ELSE 'u.' || quote_ident(a.attname) END,\n\
    \            ', ' ORDER BY a.attnum)\n\
    \     FROM pg_attribute a\n\
    \    WHERE a.attrelid = 'public.users'::regclass\n\
    \      AND a.attnum > 0 AND NOT a.attisdropped"

let make_shadow (module C : Caqti_lwt.CONNECTION) ~poison_user_id =
  let exec sql =
    let* r = C.exec ((Caqti_type.unit ->. Caqti_type.unit) sql) () in
    let* _ = or_fail "shadow" r in
    Lwt.return_unit
  in
  let* () = exec ("DROP SCHEMA IF EXISTS " ^ shadow ^ " CASCADE") in
  let* () = exec ("CREATE SCHEMA " ^ shadow) in
  let* relations =
    C.collect_list
      ((Caqti_type.unit ->* Caqti_type.string)
         "SELECT c.relname FROM pg_class c\n\
         \           JOIN pg_namespace n ON n.oid = c.relnamespace\n\
         \          WHERE n.nspname = 'public' AND c.relkind IN ('r', 'p', \
          'v', 'm', 'f')\n\
         \          ORDER BY c.relname")
      ()
  in
  let* relations = or_fail "public relations" relations in
  let* () =
    Lwt_list.iter_s
      (fun t ->
        if t = "users" then Lwt.return_unit
        else
          exec
            (Printf.sprintf
               "CREATE VIEW %s.\"%s\" AS SELECT * FROM public.\"%s\"" shadow t t))
      relations
  in
  let* columns = C.find q_poisoned_users_columns poison_user_id in
  let* columns = or_fail "users columns" columns in
  exec
    (Printf.sprintf "CREATE VIEW %s.users AS SELECT %s FROM public.users u"
       shadow columns)

let with_shadow url =
  Uri.to_string
    (Uri.add_query_param' (Uri.of_string url)
       ("options", "-csearch_path=" ^ shadow))

let generic = "A database error occurred. Please try again later."

let db_needles =
  [
    "tgim_shadow";
    "search_path";
    "postgresql";
    "caqti";
    "division by zero";
    "relation";
    "constraint";
    "select";
    "insert";
    "delete";
    "pg_";
    "is_admin";
  ]

let no_leak label body =
  List.iter
    (fun s ->
      if contains (String.lowercase_ascii body) (String.lowercase_ascii s) then
        Alcotest.failf "%s: body leaks %S" label s)
    db_needles

(* === the routed pipeline === *)

let secret = "tgim-test-secret-value"

let build_pipeline ~url pending_form =
  let with_form handler req =
    (match !pending_form with
    | None -> ()
    | Some fields ->
        pending_form := None;
        let csrf = Dream.csrf_token req in
        Dream.set_body req
          (String.concat "&"
             (List.map
                (fun (k, v) ->
                  Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
                (("dream.csrf", csrf) :: fields))));
    handler req
  in
  Dream.sql_pool ~size:4 url @@ Dream.set_secret secret @@ Dream.sql_sessions
  @@ Dream.router
       [
         Dream.post "/login" (with_form Earde.Auth_handlers.login_handler);
         Dream.post "/ban-community-user"
           (with_form Earde.Moderation_handlers.ban_community_user_handler);
         Dream.post "/delete-post"
           (with_form Earde.Post_handlers.delete_post_handler);
         Dream.get "/probe" (fun req ->
             match Dream.session_field req "user_id" with
             | Some uid ->
                 Dream.respond
                   (uid ^ "|"
                   ^
                   match Dream.session_field req "is_admin" with
                   | Some v -> v
                   | None -> "-")
             | None -> Dream.respond ~status:`Unauthorized "anon");
       ]

type client = {
  pipeline : Dream.request -> Dream.response Dream.promise;
  jar : (string * string) list ref;
  pending_form : (string * string) list option ref;
}

let make_client ~url =
  let pending_form = ref None in
  { pipeline = build_pipeline ~url pending_form; jar = ref []; pending_form }

let update_jar jar response =
  List.iter
    (fun header ->
      match String.split_on_char ';' header with
      | pair :: _ -> (
          match String.index_opt pair '=' with
          | Some i ->
              let name = String.sub pair 0 i in
              let value =
                String.sub pair (i + 1) (String.length pair - i - 1)
              in
              jar := (name, value) :: List.remove_assoc name !jar
          | None -> ())
      | [] -> ())
    (Dream.headers response "Set-Cookie")

let send client ?(method_ = `GET) ?form target =
  client.pending_form := form;
  let headers =
    (match !(client.jar) with
      | [] -> []
      | pairs ->
          [
            ( "Cookie",
              String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) pairs) );
          ])
    @
    match form with
    | Some _ -> [ ("Content-Type", "application/x-www-form-urlencoded") ]
    | None -> []
  in
  let* response =
    client.pipeline (Dream.request ~method_ ~target ~headers "")
  in
  update_jar client.jar response;
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), response, body)

(* A local community moderator with NO admin claim at all: [current_admin]
   makes no durable lookup for such a session, so the poison below cannot
   reach the actor's own authorization. *)
let logged_in_mod ~url username =
  let client = make_client ~url in
  let* status, _, _ =
    send client ~method_:`POST
      ~form:[ ("identifier", username); ("password", password) ]
      "/login"
  in
  Alcotest.(check bool) (username ^ ": login redirects") true (status / 100 = 3);
  let* _, _, probe = send client "/probe" in
  Alcotest.(check bool)
    (username ^ ": session claims no admin")
    false (contains probe "|true");
  Lwt.return client

let ban_form ~community_id ~target =
  [
    ("community_id", string_of_int community_id);
    ("target_username", target);
    ("reason", "tgim reason");
  ]

(* === 1 + 5 — the community ban keeps its immunity, and its control === *)

let ban_immunity_case =
  db_case
    "community ban: a local moderator cannot ban a durable global admin, and \
     still bans an ordinary member"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* moderator = make_user (module C) ~admin:false "tgim_immod" in
      let* target_admin = make_user (module C) ~admin:true "tgim_imadmin" in
      let* target_plain = make_user (module C) ~admin:false "tgim_implain" in
      let* cid = C.find q_community "tgim-immunity" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_moderator (moderator, cid) in
      let* () = or_fail "moderator" r in

      let* client = logged_in_mod ~url "tgim_immod" in

      (* The protected target. *)
      let* status, _, body =
        send client ~method_:`POST
          ~form:(ban_form ~community_id:cid ~target:"tgim_imadmin")
          "/ban-community-user"
      in
      Alcotest.(check bool)
        "the refusal is not a redirect" false
        (status / 100 = 3);
      Alcotest.(check bool)
        "immunity copy" true
        (contains body "cannot ban a Global Administrator");
      let* n = C.find q_is_community_banned (target_admin, cid) in
      let* n = or_fail "admin ban rows" n in
      Alcotest.(check int) "the admin was not banned" 0 n;
      let* n = C.find q_count_mod_actions (cid, target_admin) in
      let* n = or_fail "admin mod actions" n in
      Alcotest.(check int) "no mod-log row for the refused ban" 0 n;
      let* n = C.find q_count_notifs target_admin in
      let* n = or_fail "admin notifs" n in
      Alcotest.(check int) "no notification for the refused ban" 0 n;

      (* CONTROL: the same moderator, the same form, an ordinary target. *)
      let* status, _, _ =
        send client ~method_:`POST
          ~form:(ban_form ~community_id:cid ~target:"tgim_implain")
          "/ban-community-user"
      in
      Alcotest.(check bool) "the ordinary ban redirects" true (status / 100 = 3);
      let* n = C.find q_is_community_banned (target_plain, cid) in
      let* n = or_fail "plain ban rows" n in
      Alcotest.(check int) "the ordinary member really is banned" 1 n;
      let* n = C.find q_count_mod_actions (cid, target_plain) in
      let* n = or_fail "plain mod actions" n in
      Alcotest.(check int) "the ordinary ban is logged" 1 n;
      let* n = C.find q_count_notifs target_plain in
      let* n = or_fail "plain notifs" n in
      Alcotest.(check int) "the ordinary ban notifies" 1 n;
      Lwt.return_unit)

(* === 2 — the community ban's immunity lookup cannot be answered === *)

let ban_lookup_failure_case =
  db_case
    "community ban: a target-admin lookup that cannot be answered bans nobody, \
     logs nothing and notifies nobody"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* moderator = make_user (module C) ~admin:false "tgim_flmod" in
      let* target = make_user (module C) ~admin:false "tgim_fltarget" in
      let* cid = C.find q_community "tgim-banfault" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_moderator (moderator, cid) in
      let* () = or_fail "moderator" r in

      (* Minted on the intact pool, replayed against the poisoned one, so
         the session itself is real either way. *)
      let* client = logged_in_mod ~url "tgim_flmod" in
      let cookies = !(client.jar) in
      let* () = make_shadow (module C) ~poison_user_id:target in
      let broken = make_client ~url:(with_shadow url) in
      broken.jar := cookies;

      (* The moderator's OWN authorization still resolves on the poisoned
         pool — only the target's admin column raises. Anything else would
         make the 500 below prove the wrong thing. *)
      let* _, _, probe = send broken "/probe" in
      Alcotest.(check bool)
        "the session still reads back" true
        (contains probe (string_of_int moderator ^ "|"));

      let* status, _, body =
        send broken ~method_:`POST
          ~form:(ban_form ~community_id:cid ~target:"tgim_fltarget")
          "/ban-community-user"
      in
      Alcotest.(check int) "the unanswerable lookup is a generic 500" 500 status;
      Alcotest.(check bool) "generic copy" true (contains body generic);
      Alcotest.(check bool)
        "not the immunity copy" false
        (contains body "Global Administrator");
      no_leak "ban fault" body;

      let* n = C.find q_is_community_banned (target, cid) in
      let* n = or_fail "ban rows" n in
      Alcotest.(check int) "nobody was banned" 0 n;
      let* n = C.find q_count_mod_actions (cid, target) in
      let* n = or_fail "mod actions" n in
      Alcotest.(check int) "no mod-log row for the attempted ban" 0 n;
      let* n = C.find q_count_notifs target in
      let* n = or_fail "notifs" n in
      Alcotest.(check int) "no notification for the attempted ban" 0 n;

      (* CONTROL: the identical request on the intact pool succeeds, so the
         injection is the only difference between 500 and a real ban. *)
      let* status, _, _ =
        send client ~method_:`POST
          ~form:(ban_form ~community_id:cid ~target:"tgim_fltarget")
          "/ban-community-user"
      in
      Alcotest.(check bool)
        "unpoisoned, the same ban redirects" true
        (status / 100 = 3);
      let* n = C.find q_is_community_banned (target, cid) in
      let* n = or_fail "ban rows" n in
      Alcotest.(check int) "unpoisoned, the ban lands" 1 n;
      Lwt.return_unit)

(* === 3 + 5 — delete-post keeps its immunity, and its control === *)

let delete_immunity_case =
  db_case
    "delete post: a moderator cannot delete a durable global admin's post, and \
     still deletes an ordinary member's"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* moderator = make_user (module C) ~admin:false "tgim_dimod" in
      let* author_admin = make_user (module C) ~admin:true "tgim_diadmin" in
      let* author_plain = make_user (module C) ~admin:false "tgim_diplain" in
      let* cid = C.find q_community "tgim-delimmunity" in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_moderator (moderator, cid) in
      let* () = or_fail "moderator" r in
      let* admin_post =
        C.find q_post ("tgim admin post", cid, author_admin, None)
      in
      let* admin_post = or_fail "admin post" admin_post in
      let* plain_post =
        C.find q_post ("tgim plain post", cid, author_plain, None)
      in
      let* plain_post = or_fail "plain post" plain_post in

      let* client = logged_in_mod ~url "tgim_dimod" in

      let* status, _, body =
        send client ~method_:`POST
          ~form:[ ("post_id", string_of_int admin_post) ]
          "/delete-post"
      in
      Alcotest.(check int) "the admin's post is protected" 403 status;
      Alcotest.(check bool)
        "immunity copy" true
        (contains body "cannot moderate an Admin");
      let* state = C.find q_post_state admin_post in
      let* content, _ = or_fail "admin post state" state in
      Alcotest.(check (option string))
        "the admin's post is untouched" (Some "tgim body") content;

      (* CONTROL: the same moderator removes an ordinary member's post. *)
      let* status, _, _ =
        send client ~method_:`POST
          ~form:[ ("post_id", string_of_int plain_post) ]
          "/delete-post"
      in
      Alcotest.(check bool)
        "the ordinary removal redirects" true
        (status / 100 = 3);
      let* state = C.find q_post_state plain_post in
      let* content, _ = or_fail "plain post state" state in
      Alcotest.(check (option string))
        "the ordinary post really is removed" (Some "[removed by admin]")
        content;
      Lwt.return_unit)

(* === 4 — delete-post's immunity lookup cannot be answered ===

   The author's admin status is settled ABOVE the image unlink, so a failed
   lookup must leave the file on disk as well as the row in the table. *)

let delete_lookup_failure_case =
  db_case
    "delete post: an unanswerable author-admin lookup removes no row and no \
     file from disk" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      (* This case owns exactly ONE filesystem fixture: the fixed,
         tgim-namespaced upload named here, which [Sec_db.make_upload_file]
         writes to exactly [image_path]. Cleanup below removes that single
         path and nothing else — static/uploads is never enumerated and no
         prefix or before/after difference is taken — so a concurrent or
         unrelated entry in the same directory can never be caught by it. *)
      let image_name = "tgim-delfault.webp" in
      let image_path =
        Filename.concat Security_fixture.uploads_dir image_name
      in
      Lwt.finalize
        (fun () ->
          let* moderator = make_user (module C) ~admin:false "tgim_dfmod" in
          let* author = make_user (module C) ~admin:false "tgim_dfauthor" in
          let* cid = C.find q_community "tgim-delfault" in
          let* cid = or_fail "community" cid in
          let* r = C.exec q_moderator (moderator, cid) in
          let* () = or_fail "moderator" r in
          let _ = Security_fixture.make_upload_file image_name in
          let image_url = Security_fixture.avatar_url_of image_name in
          let* post =
            C.find q_post ("tgim fault post", cid, author, Some image_url)
          in
          let* post = or_fail "post" post in

          let* client = logged_in_mod ~url "tgim_dfmod" in
          let cookies = !(client.jar) in
          let* () = make_shadow (module C) ~poison_user_id:author in
          let broken = make_client ~url:(with_shadow url) in
          broken.jar := cookies;

          let* status, _, body =
            send broken ~method_:`POST
              ~form:[ ("post_id", string_of_int post) ]
              "/delete-post"
          in
          Alcotest.(check int)
            "the unanswerable lookup is a generic 500" 500 status;
          Alcotest.(check bool) "generic copy" true (contains body generic);
          Alcotest.(check bool)
            "not the immunity copy" false
            (contains body "moderate an Admin");
          no_leak "delete fault" body;

          let* state = C.find q_post_state post in
          let* content, stored_image = or_fail "post state" state in
          Alcotest.(check (option string))
            "the post is untouched" (Some "tgim body") content;
          Alcotest.(check (option string))
            "image_url is untouched" (Some image_url) stored_image;
          Alcotest.(check bool)
            "the file is still on disk" true
            (Sys.file_exists image_path);

          (* CONTROL: unpoisoned, the identical request removes both — so
             the file assertion above is about ordering, not about a code
             path that never runs. *)
          let* status, _, _ =
            send client ~method_:`POST
              ~form:[ ("post_id", string_of_int post) ]
              "/delete-post"
          in
          Alcotest.(check bool)
            "unpoisoned, the removal redirects" true
            (status / 100 = 3);
          let* state = C.find q_post_state post in
          let* content, stored_image = or_fail "post state" state in
          Alcotest.(check (option string))
            "unpoisoned, the post is removed" (Some "[removed by admin]")
            content;
          Alcotest.(check (option string))
            "unpoisoned, image_url is cleared" None stored_image;
          Alcotest.(check bool)
            "unpoisoned, the file is gone" false
            (Sys.file_exists image_path);
          Lwt.return_unit)
        (fun () ->
          (* Absence is the only tolerated outcome, and it is expected: the
             successful control request at the end of the case unlinks the
             file itself. Every other path — an assertion that fails before
             the handler runs, or the failure-path request that must leave
             the file in place — finds it here and removes it. *)
          if Sys.file_exists image_path then Sys.remove image_path;
          Lwt.return_unit))

let suite =
  [
    ban_immunity_case;
    ban_lookup_failure_case;
    delete_immunity_case;
    delete_lookup_failure_case;
  ]

let suites =
  (* Target admin immunity: where a moderation mutation is forbidden
       because its TARGET is a durable global administrator, a storage
       failure in that target lookup is an internal error — never a licence
       to perform the mutation the immunity exists to stop. Both families
       carry the moderator's ordinary-target control. *)
  [ ("security_target_admin_immunity", suite) ]
