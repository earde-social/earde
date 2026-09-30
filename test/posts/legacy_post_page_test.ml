(* === Legacy /p/:id post fallback (pass 20A) ================================
   The last user-reachable Components.layout `Site document — Post_pages.post_page,
   view_post_handler's safe fallback for the pathological unmappable post
   (community_slug = "") — moves onto Components.launch_app_page under
   body.launch-legacy-post, with the warm-card fragment (post card, recursive
   comment tree, every vote/comment/delete/mod_delete/ban/join form, the
   mod/admin/ban dialogs and the toggleComment script) pinned byte-for-byte.
   Renderer pins (DB-free, real session middleware for the CSRF tag): wrapper
   identity and local assets only, one behavior script and one notification
   fetch for members, the guest share-only script and zero notification
   fetches for guests, the pinned form/dialog/id contracts across viewer
   states (guest / member / author / moderator / admin / banned target /
   tombstoned), the launch rail fed only by handler-supplied joined
   communities, and the h1 title escaping regression (the one interpolation
   this renderer emitted raw). Route pins (database-gated, the real handler):
   the untouched canonical 301 (status + Location + a launch-free redirect
   document byte-identical across viewer states), missing/invalid-id 404s,
   the private-community anti-enumeration gate running BEFORE any fallback
   chrome, and the real fallback render for both viewer states. *)

let case name f = Alcotest.test_case name `Quick f

let ( let* ) = Lwt.bind

(* The handler's synthesized minimal record for the unmappable post (its
   community_res is Ok None when the slug is empty), reproduced here for
   direct renderer calls. *)
let fallback_community : Earde.Community_types.community =
  { id = 9107; slug = ""; name = ""; description = None; rules = None;
    avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = false; visibility = Earde.Community_types.Community_public;
    indexable = true; is_network_community = false;
    onboarding_state = Earde.Community_types.Community_published; discoverable = true }

let rail_community : Earde.Community_types.community =
  { fallback_community with id = 9108; slug = "qa-lgp-rail";
    name = "qa-lgp-rail" }

let make_post ?(id = 9107) ?(title = "Qa fallback discussion")
    ?(content = Some "Qa fallback body") ?url ?image_url
    ?(username = "qa_lgp_author") () : Earde.Post_types.post =
  { id; title; url; content; community_id = 9107; user_id = 42; username;
    community_slug = ""; created_at = "2026-07-30 12:00:00"; score = 3;
    comment_count = 1; allow_downvotes = true; image_url;
    section_name = None; section_slug = None;
    community_sections_enabled = false; author_local_karma = 5;
    author_local_post_count = 2; author_local_comment_count = 3;
    author_first_active_at = None }

let make_comment ?(id = 501) ?(content = "Qa fallback comment")
    ?(username = "qa_lgp_commenter") ?parent_id () : Earde.Comment_store.comment =
  { id; content; username; created_at = "2026-07-30 12:05:00"; score = 1;
    parent_id; avatar_url = None; author_local_karma = 1;
    author_local_post_count = 0; author_local_comment_count = 1;
    author_first_active_at = None }

let render ?user ?(session = []) ?(noindex = false) ?(is_member = false)
    ?(is_current_user_mod = false) ?(mod_usernames = [])
    ?(admin_usernames = []) ?(banned_usernames = [])
    ?(user_communities = []) ?(post = make_post ()) ?(comments = []) () =
  let rendered = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    rendered :=
      Some
        (Earde.Post_pages.post_page ?user ~noindex ~is_member
           ~is_current_user_mod ~mod_usernames ~admin_usernames
           ~banned_usernames ~community:fallback_community ~user_communities
           ~moderated_communities:[] [] [] post comments req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/p/9107" "")));
  match !rendered with
  | Some s -> s
  | None -> Alcotest.fail "renderer did not run"

let member_session name = [ ("user_id", "43"); ("username", name) ]

(* 1: anonymous document — launch wrapper, local assets only, the guest
   share-only script and nothing authenticated. *)
let guest_wrapper_case =
  case "guest: launch wrapper, local assets, share-only script, zero \
        notification fetches" (fun () ->
      let page = render ~comments:[ make_comment () ] () in
      Html_assert.must page "<body class='launch-legacy-post'>";
      Html_assert.must page "<title>Qa fallback discussion - Earde</title>";
      Html_assert.must page "<link rel='stylesheet' href='/static/css/earde.css'>";
      Html_assert.must page
        "<link rel='stylesheet' href='/static/css/mobile-gate.css'>";
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "fonts.gstatic";
      Html_assert.must_not page "shell.css";
      Html_assert.must_not page "create.css";
      Html_assert.must_not page "auth.css";
      Html_assert.must_not page "Nunito";
      (* Guest interaction surface: the byte-pinned Share control works via
         the share-only script; nothing else runs. *)
      Alcotest.(check int) "exactly one copyPostLink definition" 1
        (Html_assert.occurrences page "function copyPostLink");
      Html_assert.must page "onclick='copyPostLink(\"/p/9107\", this)'";
      Alcotest.(check int) "zero notification fetches" 0
        (Html_assert.occurrences page "/api/unread-notifs");
      Alcotest.(check int) "no notif badge markup" 0
        (Html_assert.occurrences page "notif-badge");
      Alcotest.(check int) "no confirm modal" 0
        (Html_assert.occurrences page "function confirmModal");
      Alcotest.(check int) "no vote handler wiring" 0
        (Html_assert.occurrences page "form[action='/vote']");
      (* Anonymous gates preserved: vote arrows are /login links, the join
         box keeps its exact redirect_to, readable content stays. *)
      Html_assert.must page "<a href='/login'";
      Html_assert.must page "<input type='hidden' name='redirect_to' value='/p/9107'>";
      Html_assert.must page "Qa fallback body";
      Html_assert.must page "Qa fallback comment";
      Html_assert.must page "ph-mask";
      (* Cartographic chrome + neutral copy; noindex only when asked. *)
      Html_assert.must page "class='topbar'";
      Html_assert.must page "class='rail'";
      Html_assert.must_not page "noindex";
      Html_assert.must page "toggleComment";
      Html_assert.must_not page "Legacy";
      Html_assert.must_not page "orphaned";
      let flagged = render ~noindex:true () in
      Html_assert.must flagged "<meta name='robots' content='noindex'>")

(* 2: member document — the shared behavior script exactly once (single
   copyPostLink source), one notification fetch, real vote forms, the rail
   fed by handler-supplied joined communities only. *)
let member_scripts_case =
  case "member: one behavior script, one notification fetch, joined rail"
    (fun () ->
      let page =
        render ~user:"qa_lgp_member"
          ~session:(member_session "qa_lgp_member")
          ~user_communities:[ rail_community ]
          ~comments:[ make_comment () ] ()
      in
      Alcotest.(check int) "exactly one copyPostLink definition" 1
        (Html_assert.occurrences page "function copyPostLink");
      Alcotest.(check int) "no notification fetch" 0
        (Html_assert.occurrences page "fetch('/api/unread-notifs')");
      Alcotest.(check int) "one confirm modal definition" 1
        (Html_assert.occurrences page "function confirmModal");
      (* Rendered with no request, so no count and no badge element. *)
      Html_assert.must_not page "id='notif-badge'";
      Html_assert.must page "<form action='/vote' method='POST' class='m-0 p-0 flex'>";
      Html_assert.must page "<form action='/vote-comment' method='POST' class='m-0 p-0'>";
      Html_assert.must page "name=\"dream.csrf\"";
      (* The launch rail carries the real joined community and nothing
         invented — no current-community tile, no fallback entry. *)
      Html_assert.must page "href='/c/qa-lgp-rail/ch/general'";
      Alcotest.(check int) "one community tile" 1
        (Html_assert.occurrences page "rail__item--community"))

(* 3: the pinned fragment across author/member state — every form action,
   method, hidden id and dialog id byte-intact. *)
let fragment_contract_case =
  case "author member: composer, reply, vote, delete and join contracts \
        byte-intact" (fun () ->
      let comments =
        [ make_comment (); make_comment ~id:502 ~parent_id:501
            ~content:"Qa nested reply" () ]
      in
      let page =
        render ~user:"qa_lgp_author"
          ~session:(member_session "qa_lgp_author")
          ~is_member:true ~comments ()
      in
      (* Top-level composer + per-comment reply forms (member-only). *)
      Html_assert.must page "<form action='/comments' method='POST' class='mt-0'>";
      Html_assert.must page "<input type='hidden' name='post_id' value='9107'>";
      Html_assert.must page
        "<form id='reply-form-501' action='/comments' method='POST' \
         class='hidden w-full mt-3 mb-2'>";
      Html_assert.must page "<input type='hidden' name='parent_id' value='501'>";
      (* Vote forms keep the optimistic-vote DOM contract. *)
      Html_assert.must page "<input type='hidden' name='comment_id' value='501'>";
      Html_assert.must page "<input type='hidden' name='direction' value='1'>";
      (* Own-post personal delete (Rule A) with its confirm hook. *)
      Html_assert.must page "<form action='/delete-post' method='POST' class='inline \
                    m-0 p-0 ml-3' onsubmit=\"confirmModal(event, 'Do you \
                    really want to delete this post? This action cannot be \
                    undone.')\">";
      (* Member documents drop the join box; comment tree ids stay. *)
      Html_assert.must_not page "name='redirect_to'";
      Html_assert.must page "id='comment-content-501'";
      Html_assert.must page "id='comment-children-501'";
      Html_assert.must page "Qa nested reply";
      (* No nested forms: every <form is matched by exactly one </form>. *)
      Alcotest.(check int) "form open/close balance"
        (Html_assert.occurrences page "<form") (Html_assert.occurrences page "</form>"))

(* 4: moderation permissions unchanged — mod dialogs (with the historical
   empty-slug action paths), admin fallback, banned badge, admin-target
   immunity. *)
let moderation_case =
  case "moderator and admin controls unchanged (dialogs, ban, immunity)"
    (fun () ->
      let comments = [ make_comment () ] in
      let mod_page =
        render ~user:"qa_lgp_mod" ~session:(member_session "qa_lgp_mod")
          ~is_current_user_mod:true ~comments ()
      in
      Html_assert.must mod_page "Mod Remove";
      Html_assert.must mod_page "id='mod-modal-9107'";
      (* The unmappable post's historical action path is preserved verbatim
         (empty community slug and all). *)
      Html_assert.must mod_page "action='/c//posts/9107/mod_delete'";
      Html_assert.must mod_page "action='/c//comments/501/mod_delete'";
      Html_assert.must mod_page "<form action='/ban-community-user' method='POST'";
      Html_assert.must mod_page
        "<input type='hidden' name='target_username' value='qa_lgp_author'>";
      Html_assert.must mod_page "<input type='hidden' name='community_id' value='9107'>";
      Html_assert.must_not mod_page "Admin Remove";
      let admin_page =
        render ~user:"qa_lgp_admin"
          ~session:(("is_admin", "true") :: member_session "qa_lgp_admin")
          ~comments ()
      in
      Html_assert.must admin_page "Admin Remove";
      Html_assert.must admin_page "Admin Ban";
      Html_assert.must_not admin_page "Mod Remove";
      (* Already-banned target shows the badge, not a second hammer. *)
      let banned_page =
        render ~user:"qa_lgp_mod" ~session:(member_session "qa_lgp_mod")
          ~is_current_user_mod:true
          ~banned_usernames:[ "qa_lgp_author"; "qa_lgp_commenter" ]
          ~comments ()
      in
      Html_assert.must banned_page "🚫 Banned";
      Html_assert.must_not banned_page "Mod Ban";
      (* Admin-authored content is immune to mod removal. *)
      let immune_page =
        render ~user:"qa_lgp_mod" ~session:(member_session "qa_lgp_mod")
          ~is_current_user_mod:true
          ~admin_usernames:[ "qa_lgp_author"; "qa_lgp_commenter" ]
          ~comments ()
      in
      Html_assert.must_not immune_page "Mod Remove")

(* 5: tombstoned content keeps its quiet state — no action buttons. *)
let tombstone_case =
  case "deleted post and comment tombstones drop every action control"
    (fun () ->
      let page =
        render ~user:"qa_lgp_author"
          ~session:(member_session "qa_lgp_author")
          ~post:(make_post ~content:(Some "[deleted]") ())
          ~comments:[ make_comment ~content:"[removed by moderator]" () ] ()
      in
      Html_assert.must_not page "action='/delete-post'";
      Html_assert.must_not page "action='/delete-comment'";
      Html_assert.must_not page "Mod Remove")

(* 6: the h1 escaping regression — the one interpolation this renderer
   emitted raw. Every user-controlled field stays escaped text. *)
let escaping_case =
  case "post title (h1 + <title>) and comment content stay escaped text"
    (fun () ->
      let page =
        render
          ~post:
            (make_post
               ~title:"Qa <script>alert(1)</script> & \"q\" 'tick'"
               ~content:(Some "body <img src=x onerror=alert(2)>") ())
          ~comments:
            [ make_comment ~content:"<b>bold</b> & <i>sneaky</i>" () ] ()
      in
      Html_assert.must page
        "Qa &lt;script&gt;alert(1)&lt;/script&gt; &amp; &quot;q&quot; \
         &#39;tick&#39;";
      Html_assert.must page
        "<title>Qa &lt;script&gt;alert(1)&lt;/script&gt; &amp; \
         &quot;q&quot; &#39;tick&#39; - Earde</title>";
      Html_assert.must_not page "<script>alert(1)";
      Html_assert.must page "body &lt;img src=x onerror=alert(2)&gt;";
      Html_assert.must_not page "<img src=x";
      Html_assert.must page "&lt;b&gt;bold&lt;/b&gt; &amp; &lt;i&gt;sneaky&lt;/i&gt;";
      Html_assert.must_not page "<b>bold</b>")

let renderer_suite =
  [ guest_wrapper_case; member_scripts_case; fragment_contract_case;
    moderation_case; tombstone_case; escaping_case ]

(* --- Route suite: the real GET /p/:id handler over a real database. --- *)

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE title LIKE 'Legpost %')"
    ; "DELETE FROM posts WHERE title LIKE 'Legpost %'"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug = 'legpost-c' OR name LIKE 'legpost %')"
    ; "DELETE FROM communities WHERE slug = 'legpost-c' OR name LIKE 'legpost %'"
    ; "DELETE FROM users WHERE username LIKE 'legpost_%'"
    ]

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_user =
  (Caqti_type.(t2 string bool) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified, is_admin)
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE, $2) RETURNING id"

let q_insert_community =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name) VALUES ($1, $2) RETURNING id"

let q_make_private =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET visibility = 'private' WHERE id = $1"

let q_insert_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)
     VALUES ($1, $1 || ' body', $2, $3) RETURNING id"

let q_insert_comment =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id)
     VALUES ($1, $2, $3) RETURNING id"

type fx = {
  author : int;
  admin : int;
  canonical_post : int;
  fallback_post : int;
  fallback_community : int;
}

(* One mappable community/post pair (the 301 arm) and one unmappable pair:
   a community row whose slug is the empty string — the only durable state
   that reaches the fallback renderer (the app cannot create it; it stands
   in for historical/corrupt rows). *)
let make_fixtures (module C : Caqti_lwt.CONNECTION) =
  let* author = C.find q_insert_user ("legpost_author", false) in
  let* author = or_fail "author" author in
  let* admin = C.find q_insert_user ("legpost_admin", true) in
  let* admin = or_fail "admin" admin in
  let* canonical_c = C.find q_insert_community ("legpost-c", "legpost canonical") in
  let* canonical_c = or_fail "canonical community" canonical_c in
  let* fallback_c = C.find q_insert_community ("", "legpost fallback") in
  let* fallback_c = or_fail "fallback community" fallback_c in
  let* canonical_post =
    C.find q_insert_post ("Legpost canonical post", canonical_c, author)
  in
  let* canonical_post = or_fail "canonical post" canonical_post in
  let* fallback_post =
    C.find q_insert_post ("Legpost fallback post", fallback_c, author)
  in
  let* fallback_post = or_fail "fallback post" fallback_post in
  let* comment =
    C.find q_insert_comment ("Legpost fallback comment", fallback_post, author)
  in
  let* _ = or_fail "comment" comment in
  Lwt.return
    { author; admin; canonical_post; fallback_post;
      fallback_community = fallback_c }

let router =
  Dream.router [ Dream.get "/p/:id" Earde.Post_handlers.view_post_handler ]

let run ~url ?(session = []) target =
  let pipeline =
    Dream.sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s
        (fun (k, v) -> Dream.set_session_field req k v)
        session
    in
    router req
  in
  let request = Dream.request ~method_:`GET ~target "" in
  let* response = pipeline request in
  let* body = Dream.body response in
  let location = Dream.header response "Location" in
  Lwt.return (Dream.status_to_int (Dream.status response), location, body)

let session_of uid name =
  [ ("user_id", string_of_int uid); ("username", name) ]

(* 1: the canonical arm is untouched — exact 301 + Location, no launch
   document, no notification wiring, and a redirect response byte-identical
   across anonymous and authenticated viewers. *)
let redirect_case =
  db_case "canonical post: exact 301 + Location, launch-free, viewer-\
           independent" (fun ~url c ->
      let* fx = make_fixtures c in
      let target = "/p/" ^ string_of_int fx.canonical_post in
      let* status, location, body = run ~url target in
      Alcotest.(check int) "301" 301 status;
      Alcotest.(check (option string)) "Location"
        (Some
           (Earde.Post_cards.canonical_thread_path "legpost-c"
              fx.canonical_post "Legpost canonical post"))
        location;
      Html_assert.must_not body "launch-legacy-post";
      Html_assert.must_not body "unread-notifs";
      Html_assert.must_not body "earde.css";
      Html_assert.must_not body "<form";
      let* status2, location2, body2 =
        run ~url ~session:(session_of fx.author "legpost_author") target
      in
      Alcotest.(check int) "authenticated 301" 301 status2;
      Alcotest.(check (option string)) "authenticated Location" location
        location2;
      Alcotest.(check string) "redirect body byte-identical across viewers"
        body body2;
      Lwt.return_unit)

(* 2: the real fallback render, both viewer states. *)
let fallback_case =
  db_case "fallback post: launch document, guest zero / member one \
           notification fetch" (fun ~url c ->
      let* fx = make_fixtures c in
      let target = "/p/" ^ string_of_int fx.fallback_post in
      let* status, _, body = run ~url target in
      Alcotest.(check int) "anonymous 200" 200 status;
      Html_assert.must body "<body class='launch-legacy-post'>";
      Html_assert.must body "<link rel='stylesheet' href='/static/css/earde.css'>";
      Html_assert.must body
        "<link rel='stylesheet' href='/static/css/mobile-gate.css'>";
      Html_assert.must_not body "tailwind";
      Html_assert.must_not body "fonts.googleapis";
      Html_assert.must body "Legpost fallback post";
      Html_assert.must body "Legpost fallback comment";
      Alcotest.(check int) "guest: one copyPostLink" 1
        (Html_assert.occurrences body "function copyPostLink");
      Alcotest.(check int) "guest: zero notification fetches" 0
        (Html_assert.occurrences body "/api/unread-notifs");
      Html_assert.must body "<a href='/login'";
      let* status2, _, body2 =
        run ~url ~session:(session_of fx.author "legpost_author") target
      in
      Alcotest.(check int) "member 200" 200 status2;
      Alcotest.(check int) "member: no notification fetch" 0
        (Html_assert.occurrences body2 "fetch('/api/unread-notifs')");
      Alcotest.(check int) "member: one copyPostLink" 1
        (Html_assert.occurrences body2 "function copyPostLink");
      Html_assert.must body2 "<form action='/vote' method='POST'";
      Html_assert.must body2 "name=\"dream.csrf\"";
      (* Author is not a member: the join box keeps its exact redirect. *)
      Html_assert.must body2
        ("<input type='hidden' name='redirect_to' value='/p/"
        ^ string_of_int fx.fallback_post ^ "'>");
      Lwt.return_unit)

(* 3: missing / invalid ids keep their 404 message pages — no launch post
   document, no notification wiring. *)
let missing_case =
  db_case "missing and invalid ids: unchanged 404 message pages"
    (fun ~url c ->
      let* _fx = make_fixtures c in
      let* status, _, body = run ~url "/p/999999999" in
      Alcotest.(check int) "missing 404" 404 status;
      Html_assert.must body "This post does not exist or has been deleted.";
      Html_assert.must_not body "launch-legacy-post";
      Html_assert.must_not body "unread-notifs";
      let* status2, _, body2 = run ~url "/p/not-a-number" in
      Alcotest.(check int) "invalid 404" 404 status2;
      Html_assert.must body2 "Invalid post ID.";
      Html_assert.must_not body2 "launch-legacy-post";
      Lwt.return_unit)

(* 4: the private gate still runs BEFORE anything renders — outsiders get
   the canonical community 404 with zero fallback chrome or content leak;
   a global admin still reaches the fallback document. *)
let private_gate_case =
  db_case "private fallback community: anti-enumeration 404 first, admin \
           still renders" (fun ~url c ->
      let* fx = make_fixtures c in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* r = C.exec q_make_private fx.fallback_community in
      let* () = or_fail "make private" r in
      let target = "/p/" ^ string_of_int fx.fallback_post in
      let* status, _, body = run ~url target in
      Alcotest.(check int) "outsider 404" 404 status;
      Html_assert.must body "This community does not exist.";
      Html_assert.must_not body "Legpost fallback post";
      Html_assert.must_not body "launch-legacy-post";
      Html_assert.must_not body "unread-notifs";
      let* status2, _, body2 =
        run ~url
          ~session:
            (("is_admin", "true") :: session_of fx.admin "legpost_admin")
          target
      in
      Alcotest.(check int) "admin 200" 200 status2;
      Html_assert.must body2 "<body class='launch-legacy-post'>";
      Html_assert.must body2 "Legpost fallback post";
      Lwt.return_unit)

let route_suite =
  [ redirect_case; fallback_case; missing_case; private_gate_case ]

let suites =
    (* Legacy /p/:id post fallback (pass 20A): the last user-reachable
       Components.layout document moves onto launch_app_page under
       body.launch-legacy-post with the warm-card fragment pinned
       byte-for-byte — renderer contracts (assets, scripts, forms,
       dialogs, permissions, tombstones, the h1 escaping regression) are
       DB-free; the untouched canonical 301, the 404 arms, the private
       anti-enumeration gate order and the real fallback render are
       database-gated. *)
  [ ("legacy_post_fallback_renderer", renderer_suite)
  ; ("legacy_post_fallback_route", route_suite)
  ]
