(* Stale global-admin session boundary (02d).

   Dream's session caches [is_admin] at login and never refreshes it, so an
   operator demoted in users.is_admin kept every legacy admin power for the
   remaining life of an open session. Every case below builds that exact
   state through the REAL routes — a real POST /login writing a durable
   dream_session row, then a direct UPDATE that clears users.is_admin without
   touching the session — and replays the very same cookie against one
   authorization-boundary family per case.

   The families, not the individual textual call sites, are what these cases
   cover: an admin-only page, an admin-only mutation, admin content removal,
   the private-community read gate, the realtime token that inherits it, the
   admin-or-moderator settings gate, and an admin-or-top-mod management
   mutation. Each carries its own durable-admin control, so a case can only
   pass by distinguishing current authority from a cached claim.

   Database-gated (EARDE_TEST_DATABASE_URL). *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let contains hay needle = Html_assert.contains hay needle

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let shadow = "stad_shadow"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DROP SCHEMA IF EXISTS stad_shadow CASCADE";
      "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE \
       username LIKE 'stad\\_%')";
      "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE \
       community_id IN (SELECT id FROM communities WHERE slug LIKE 'stad-%'))";
      "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'stad-%')";
      "DELETE FROM chat_messages WHERE channel_id IN (SELECT id FROM channels \
       WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE \
       'stad-%'))";
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'stad-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stad-%')";
      "DELETE FROM community_bans WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stad-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stad-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stad-%')";
      "DELETE FROM community_user_stats WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'stad-%')";
      "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT \
       'community:' || c.id::text FROM communities c WHERE c.slug LIKE \
       'stad-%')";
      "DELETE FROM communities WHERE slug LIKE 'stad-%'";
      "DELETE FROM dream_session WHERE payload LIKE '%stad\\_%'";
      "DELETE FROM users WHERE username LIKE 'stad\\_%'";
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
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === fixtures === *)

let password = "stad password"

let q_user =
  (Caqti_type.(t3 string string bool) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified, \
     is_admin)\n\
    \   VALUES ($1, $1 || '@stad.invalid', $2, TRUE, $3) RETURNING id"

let q_set_admin =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
    "UPDATE users SET is_admin = $2 WHERE id = $1"

let q_is_admin =
  (Caqti_type.int ->! Caqti_type.bool)
    "SELECT is_admin FROM users WHERE id = $1"

let q_is_banned =
  (Caqti_type.int ->! Caqti_type.bool)
    "SELECT is_banned FROM users WHERE id = $1"

let q_community =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility, sections_enabled)\n\
    \   VALUES ($1, $1, $2, FALSE) RETURNING id"

let q_channel =
  (Caqti_type.(t2 string int) ->! Caqti_type.int)
    "INSERT INTO channels (slug, name, community_id) VALUES ($1, $1, $2)\n\
    \   RETURNING id"

let q_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)\n\
    \   ON CONFLICT DO NOTHING"

let q_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role)\n\
    \   VALUES ($1, $2, $3) ON CONFLICT DO NOTHING"

let q_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)\n\
    \   VALUES ($1, 'stad body', $2, $3) RETURNING id"

let q_post_state =
  (Caqti_type.int ->! Caqti_type.(t2 string (option string)))
    "SELECT title, content FROM posts WHERE id = $1"

let q_count_mods =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM community_moderators WHERE community_id = $1"

let q_count_sessions =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM dream_session\n\
    \    WHERE payload::jsonb ->> 'user_id' = $1"

(* Argon2 is deliberately expensive, so the fixture password is hashed once
   for the whole module rather than once per user. *)
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

(* === the routed pipeline === *)

let secret = "stad-test-secret-value"

(* Every route here is mounted at its lib/app_routes.ml path. Real SQL sessions,
   because a case's whole point is that the durable session row survives the
   demotion untouched. /probe exists only to read the session dictionary
   back; it authorizes nothing. *)
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
         Dream.get "/admin" Earde.Admin_handlers.admin_dashboard_handler;
         Dream.post "/admin/ban/user/:id"
           (with_form Earde.Admin_handlers.ban_user_handler);
         Dream.get "/debug-state" Earde.Admin_handlers.debug_state_handler;
         Dream.post "/delete-post"
           (with_form Earde.Post_handlers.delete_post_handler);
         Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
         Dream.get "/c/:slug/ch/:channel_slug/realtime-token"
           Earde.Chat_handlers.realtime_token_handler;
         Dream.get "/c/:slug/settings"
           Earde.Community_settings_handlers.community_settings_handler;
         Dream.post "/c/:slug/manage-mods/add"
           (with_form Earde.Moderation_handlers.manage_mods_add_handler);
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

(* One [client] models one browser: its own pipeline instance plus a cookie
   jar, so a cookie minted before a demotion can be replayed verbatim after
   it. *)
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

let login client username =
  send client ~method_:`POST
    ~form:[ ("identifier", username); ("password", password) ]
    "/login"

(* A logged-in browser whose session carries is_admin=true because the
   durable row really said so at login time. *)
let logged_in_admin ~url username =
  let client = make_client ~url in
  let* status, _, _ = login client username in
  Alcotest.(check bool) (username ^ ": login redirects") true (status / 100 = 3);
  let* _, _, probe = send client "/probe" in
  Alcotest.(check bool)
    (username ^ ": session claims admin")
    true (contains probe "|true");
  Lwt.return client

let demote (module C : Caqti_lwt.CONNECTION) uid =
  (* The session row is deliberately NOT touched: this is the stale state. *)
  let* r = C.exec q_set_admin (uid, false) in
  let* () = or_fail "demote" r in
  let* durable = C.find q_is_admin uid in
  let* durable = or_fail "durable" durable in
  Alcotest.(check bool) "durably demoted" false durable;
  Lwt.return_unit

let still_claims_admin client =
  let* _, _, probe = send client "/probe" in
  Alcotest.(check bool)
    "the stale session still claims admin" true (contains probe "|true");
  Lwt.return_unit

(* === A — GET /admin === *)

let admin_dashboard_case =
  db_case
    "stale admin: GET /admin is refused and returns none of its protected data"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_dashadmin" in
      (* A distinctive row the dashboard would list if it rendered. *)
      let* _victim = make_user (module C) ~admin:false "stad_dashlisted" in
      let* client = logged_in_admin ~url "stad_dashadmin" in
      let* status, _, body = send client "/admin" in
      Alcotest.(check int) "durable admin sees the dashboard" 200 status;
      Alcotest.(check bool)
        "control: the dashboard really lists users" true
        (contains body "stad_dashlisted");

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, body = send client "/admin" in
      Alcotest.(check int) "stale claim is forbidden" 403 status;
      Alcotest.(check bool) "denial copy" true (contains body "not an Admin");
      Alcotest.(check bool)
        "no listed username" false
        (contains body "stad_dashlisted");
      Alcotest.(check bool)
        "no address column" false
        (contains body "@stad.invalid");
      (* The sibling admin-only JSON route answers the same way. *)
      let* status, _, body = send client "/debug-state" in
      Alcotest.(check int) "debug-state forbidden" 403 status;
      Alcotest.(check bool) "no session echo" false (contains body "user_id");
      Lwt.return_unit)

(* === B — global ban === *)

let global_ban_case =
  db_case
    "stale admin: the global ban writes nothing and leaves the target's \
     session alive" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_banadmin" in
      let* victim = make_user (module C) ~admin:false "stad_banvictim" in
      let* control = make_user (module C) ~admin:false "stad_bancontrol" in
      (* The victim's own live browser: a real ban revokes it (02a), so its
         survival is evidence that no ban happened. *)
      let victim_browser = make_client ~url in
      let* status, _, _ = login victim_browser "stad_banvictim" in
      Alcotest.(check bool) "victim login redirects" true (status / 100 = 3);
      let* n = C.find q_count_sessions (string_of_int victim) in
      let* n = or_fail "victim sessions" n in
      Alcotest.(check int) "one live victim session" 1 n;

      let* client = logged_in_admin ~url "stad_banadmin" in
      (* Control: while the durable row still says admin, the ban works. *)
      let* status, _, _ =
        send client ~method_:`POST ~form:[]
          (Printf.sprintf "/admin/ban/user/%d" control)
      in
      Alcotest.(check bool) "durable admin ban redirects" true (status / 100 = 3);
      let* banned = C.find q_is_banned control in
      let* banned = or_fail "control banned" banned in
      Alcotest.(check bool) "control really banned" true banned;

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, body =
        send client ~method_:`POST ~form:[]
          (Printf.sprintf "/admin/ban/user/%d" victim)
      in
      Alcotest.(check int) "stale ban is refused" 200 status;
      Alcotest.(check bool) "denial copy" true (contains body "not an Admin");
      let* banned = C.find q_is_banned victim in
      let* banned = or_fail "victim banned" banned in
      Alcotest.(check bool) "victim is NOT banned" false banned;
      let* n = C.find q_count_sessions (string_of_int victim) in
      let* n = or_fail "victim sessions after" n in
      Alcotest.(check int) "victim's session survives" 1 n;
      let* status, _, _ = send victim_browser "/probe" in
      Alcotest.(check int) "victim still authenticated" 200 status;
      Lwt.return_unit)

(* === C — admin removal of another user's post === *)

let admin_delete_case =
  db_case
    "stale admin: /delete-post on a foreign post leaves the content untouched"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_deladmin" in
      let* author = make_user (module C) ~admin:false "stad_delauthor" in
      let* cid = C.find q_community ("stad-del", "public") in
      let* cid = or_fail "community" cid in
      let* control_post = C.find q_post ("stad control post", cid, author) in
      let* control_post = or_fail "control post" control_post in
      let* target_post = C.find q_post ("stad target post", cid, author) in
      let* target_post = or_fail "target post" target_post in

      let* client = logged_in_admin ~url "stad_deladmin" in
      (* Control: a durable admin's removal tombstones the row. *)
      let* status, _, _ =
        send client ~method_:`POST
          ~form:[ ("post_id", string_of_int control_post) ]
          "/delete-post"
      in
      Alcotest.(check bool)
        "durable admin removal redirects" true
        (status / 100 = 3);
      let* state = C.find q_post_state control_post in
      let* _title, content = or_fail "control state" state in
      Alcotest.(check (option string))
        "control tombstoned" (Some "[removed by admin]") content;

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, _ =
        send client ~method_:`POST
          ~form:[ ("post_id", string_of_int target_post) ]
          "/delete-post"
      in
      (* The response family matters as much as the row: /delete-post has no
         separate "not an admin" answer, it simply falls through to the
         AUTHOR-scoped soft delete, which matches nothing here and redirects
         — exactly what any ordinary logged-in user gets. Asserting 3xx is
         what stops this case passing on an unrelated 500 raised before the
         delete was ever attempted. *)
      Alcotest.(check bool)
        "stale claim gets the ordinary non-admin redirect, not an error" true
        (status / 100 = 3);
      let* state = C.find q_post_state target_post in
      let* title, content = or_fail "target state" state in
      Alcotest.(check string) "title unchanged" "stad target post" title;
      Alcotest.(check (option string))
        "body unchanged" (Some "stad body") content;
      Lwt.return_unit)

(* === D + E — the private-community read gate and the token that
       inherits it === *)

let private_read_case =
  db_case
    "stale admin: a private community stays hidden and mints no realtime token"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_readadmin" in
      let* cid = C.find q_community ("stad-secret", "private") in
      let* cid = or_fail "community" cid in
      let* _chan = C.find q_channel ("stad-secretchan", cid) in
      let* _chan = or_fail "channel" _chan in

      let* client = logged_in_admin ~url "stad_readadmin" in
      let* status, _, body = send client "/c/stad-secret" in
      Alcotest.(check int)
        "durable admin reads the private community" 200 status;
      Alcotest.(check bool)
        "control: the page really names it" true
        (contains body "stad-secret");
      let* status, _, body =
        send client "/c/stad-secret/ch/stad-secretchan/realtime-token"
      in
      Alcotest.(check bool)
        "durable admin gets a token answer" true
        (status = 200 || status = 503);
      if status = 200 then
        Alcotest.(check bool)
          "control: a token was minted" true
          (contains body "\"token\"");

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, denied = send client "/c/stad-secret" in
      Alcotest.(check int) "hidden from the stale claim" 404 status;
      Alcotest.(check bool)
        "no community name" false
        (contains denied "stad-secret");
      (* Anti-oracle: the refusal is the route's existing hidden answer, so
         it is byte-identical to a slug that never existed. *)
      let* _, _, missing = send client "/c/stad-nothing-here" in
      Alcotest.(check string)
        "hidden reads exactly like nonexistent"
        (Html_assert.without_csrf_inputs missing)
        (Html_assert.without_csrf_inputs denied);
      let* status, _, body =
        send client "/c/stad-secret/ch/stad-secretchan/realtime-token"
      in
      Alcotest.(check int) "no token for the stale claim" 404 status;
      Alcotest.(check bool)
        "no token in the body" false
        (contains body "\"token\"");
      Lwt.return_unit)

(* A stale demoted admin who is ALSO a legitimate member keeps reading the
   private community — through the membership policy, which the admin
   override never replaced. *)
let private_member_fallback_case =
  db_case "stale admin who is also a member still reads the private community"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_memberadmin" in
      let* cid = C.find q_community ("stad-memberpriv", "private") in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_member (admin, cid) in
      let* () = or_fail "membership" r in
      let* client = logged_in_admin ~url "stad_memberadmin" in
      let* () = demote (module C) admin in
      let* status, _, body = send client "/c/stad-memberpriv" in
      Alcotest.(check int) "membership still admits" 200 status;
      Alcotest.(check bool)
        "the page renders" true
        (contains body "stad-memberpriv");
      Lwt.return_unit)

(* === F — the admin-or-moderator settings gate === *)

let settings_case =
  db_case "stale admin: community settings are refused"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_setadmin" in
      let* cid = C.find q_community ("stad-settings", "public") in
      let* cid = or_fail "community" cid in
      let* banned = make_user (module C) ~admin:false "stad_setbanned" in
      let* r = C.exec q_member (banned, cid) in
      let* () = or_fail "member row" r in

      let* client = logged_in_admin ~url "stad_setadmin" in
      let* status, _, body =
        send client "/c/stad-settings/settings?panel=members"
      in
      Alcotest.(check int) "durable admin opens settings" 200 status;
      Alcotest.(check bool)
        "control: the member roster rendered" true
        (contains body "stad_setbanned");

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, body =
        send client "/c/stad-settings/settings?panel=members"
      in
      Alcotest.(check int) "stale claim is forbidden" 403 status;
      Alcotest.(check bool)
        "denial copy" true
        (contains body "You must be a moderator");
      Alcotest.(check bool)
        "no member roster" false
        (contains body "stad_setbanned");
      Lwt.return_unit)

(* === G — an admin-or-top-mod management mutation === *)

let manage_mods_case =
  db_case "stale admin: adding a moderator makes no durable change"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* admin = make_user (module C) ~admin:true "stad_modadmin" in
      let* control = make_user (module C) ~admin:false "stad_modcontrol" in
      let* _target = make_user (module C) ~admin:false "stad_modtarget" in
      let* cid = C.find q_community ("stad-mods", "public") in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_moderator (control, cid, "mod") in
      let* () = or_fail "seed mod" r in
      let* before = C.find q_count_mods cid in
      let* before = or_fail "mods before" before in

      let* client = logged_in_admin ~url "stad_modadmin" in
      (* Control: the durable admin really can appoint a moderator. *)
      let* status, _, _ =
        send client ~method_:`POST
          ~form:[ ("username", "stad_modcontrol") ]
          "/c/stad-mods/manage-mods/add"
      in
      Alcotest.(check bool) "durable admin add redirects" true (status / 100 = 3);

      let* () = demote (module C) admin in
      let* () = still_claims_admin client in
      let* status, _, body =
        send client ~method_:`POST
          ~form:[ ("username", "stad_modtarget") ]
          "/c/stad-mods/manage-mods/add"
      in
      Alcotest.(check int) "stale claim is forbidden" 403 status;
      Alcotest.(check bool)
        "denial copy" true
        (contains body "Only Top Mods and Admins");
      let* after = C.find q_count_mods cid in
      let* after = or_fail "mods after" after in
      Alcotest.(check int) "no moderator row added" before after;
      Lwt.return_unit)

(* === stale FALSE after promotion: documented, and it requires re-login === *)

let promotion_requires_relogin_case =
  db_case
    "promoted admin with an old non-admin session stays refused until a fresh \
     login" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = make_user (module C) ~admin:false "stad_promoted" in
      let client = make_client ~url in
      let* status, _, _ = login client "stad_promoted" in
      Alcotest.(check bool) "login redirects" true (status / 100 = 3);
      let* status, _, _ = send client "/admin" in
      Alcotest.(check int) "not an admin yet" 403 status;
      (* Durable promotion, session untouched. The resolver deliberately
         short-circuits a claim that is not "true", so this session keeps no
         admin authority — that is the price of ordinary traffic making no
         admin query at all, and re-login is the documented remedy. *)
      let* r = C.exec q_set_admin (uid, true) in
      let* () = or_fail "promote" r in
      let* status, _, _ = send client "/admin" in
      Alcotest.(check int) "old session is still refused" 403 status;
      let* status, _, _ = login client "stad_promoted" in
      Alcotest.(check bool) "re-login redirects" true (status / 100 = 3);
      let* status, _, _ = send client "/admin" in
      Alcotest.(check int) "granted after re-login" 200 status;
      Lwt.return_unit)

(* === the current-admin lookup itself failing === *)

(* Same partial-schema injection Ban_fail_closed uses: [shadow] mirrors
   every public relation as an auto-updatable view EXCEPT the ones named, so
   a handler runs normally until it reaches a query against an omitted
   table. Omitting `users` breaks exactly the durable admin lookup while the
   session store, the community and the membership rows all still resolve.
   No production failpoint exists or is added. *)
let make_shadow (module C : Caqti_lwt.CONNECTION) ~omit =
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
  List.iter
    (fun t ->
      if not (List.mem t relations) then
        Alcotest.failf "omit names a relation that does not exist: %S" t)
    omit;
  Lwt_list.iter_s
    (fun t ->
      if List.mem t omit then Lwt.return_unit
      else
        exec
          (Printf.sprintf "CREATE VIEW %s.\"%s\" AS SELECT * FROM public.\"%s\""
             shadow t t))
    relations

let with_shadow url =
  Uri.to_string
    (Uri.add_query_param' (Uri.of_string url)
       ("options", "-csearch_path=" ^ shadow))

let generic = "A database error occurred. Please try again later."

(* Driver/schema vocabulary only: "does not exist" is deliberately absent,
   because the community 404 the anti-oracle assertion depends on says
   exactly that in its own product copy. *)
let db_needles =
  [
    "stad_shadow";
    "search_path";
    "postgresql";
    "caqti";
    "relation";
    "select ";
    "pg_";
  ]

let no_leak label body =
  List.iter
    (fun s ->
      if contains (String.lowercase_ascii body) (String.lowercase_ascii s) then
        Alcotest.failf "%s: body leaks %S" label s)
    db_needles

let storage_failure_case =
  db_case
    "a current-admin lookup that cannot be answered grants nothing and opens \
     no existence oracle" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* _admin = make_user (module C) ~admin:true "stad_failadmin" in
      let* victim = make_user (module C) ~admin:false "stad_failvictim" in
      let* cid = C.find q_community ("stad-failpriv", "private") in
      let* _cid = or_fail "community" cid in

      (* The session is minted on the intact pool; the replay happens
         against the broken one, so the cookie is real either way. *)
      let* client = logged_in_admin ~url "stad_failadmin" in
      let cookies = !(client.jar) in
      let* () = make_shadow (module C) ~omit:[ "users" ] in
      let broken = make_client ~url:(with_shadow url) in
      broken.jar := cookies;

      (* The admin-only mutation refuses generically and bans nobody. *)
      let* status, _, body =
        send broken ~method_:`POST ~form:[]
          (Printf.sprintf "/admin/ban/user/%d" victim)
      in
      Alcotest.(check int) "ban answers the generic failure" 500 status;
      Alcotest.(check bool) "generic copy" true (contains body generic);
      no_leak "ban" body;
      let* banned = C.find q_is_banned victim in
      let* banned = or_fail "victim banned" banned in
      Alcotest.(check bool) "nothing was banned" false banned;

      (* The private read keeps the route's existing hidden answer: no new
         response shape, so no way to tell the community apart from one that
         never existed. *)
      let* status, _, denied = send broken "/c/stad-failpriv" in
      Alcotest.(check int) "private read stays a 404" 404 status;
      Alcotest.(check bool)
        "no community name" false
        (contains denied "stad-failpriv");
      no_leak "private read" denied;
      let* _, _, missing = send broken "/c/stad-nothing-here" in
      Alcotest.(check string)
        "still byte-identical to nonexistent"
        (Html_assert.without_csrf_inputs missing)
        (Html_assert.without_csrf_inputs denied);
      Lwt.return_unit)

let suite =
  [
    admin_dashboard_case;
    global_ban_case;
    admin_delete_case;
    private_read_case;
    private_member_fallback_case;
    settings_case;
    manage_mods_case;
    promotion_requires_relogin_case;
    storage_failure_case;
  ]

let suites =
  (* Stale global-admin sessions: a cached is_admin claim, replayed after
       the durable users.is_admin row was cleared, no longer grants an
       admin-only page, an admin-only mutation, admin content removal, the
       private-community read gate (or the realtime token that inherits
       it), the admin-or-moderator settings gate, or an admin-or-top-mod
       management mutation. Each family carries its durable-admin control;
       stale-false-after-promotion is pinned as requiring re-login, and a
       failing current-admin lookup grants nothing and opens no existence
       oracle. *)
  [ ("security_stale_admin_boundary", suite) ]
