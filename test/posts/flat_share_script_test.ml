(* === Flat-community guest Share script (pass 15A follow-up) ===
   The byte-pinned render_post rows show the Share control to every viewer,
   but only member documents carry the full launch behavior script (which
   defines copyPostLink). The flat community home therefore ships the
   guest-only share script — the SAME single copyPostLink source — so the
   control works anonymously without any notification fetch or
   authenticated-only behavior. These cases exercise the real
   community_page_handler through the production pipeline shape. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'fshr-%')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'fshr-%')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'fshr-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'fshr-%'"
    ; "DELETE FROM users WHERE username LIKE 'fshr_%'"
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
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

(* sections_enabled drives the /c/:slug branch under test: FALSE = the
   converted flat feed, TRUE = the (stable) structured overview. *)
let q_insert_community =
  (Caqti_type.(t3 string bool string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, sections_enabled, visibility)
     VALUES ($1, $1, $2, $3) RETURNING id"

let q_insert_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)
     VALUES ($1, $1 || ' body', $2, $3) RETURNING id"

let q_add_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

type fx = {
  author : int;
  outsider : int;
  post_a : int;
  post_b : int;
}

let make_fixtures (module C : Caqti_lwt.CONNECTION) =
  let user name =
    let* id = C.find q_insert_user name in
    or_fail name id
  in
  let* author = user "fshr_author" in
  let* outsider = user "fshr_outsider" in
  let* flat = C.find q_insert_community ("fshr-flat", false, "public") in
  let* flat = or_fail "flat community" flat in
  let* () =
    let* r = C.exec q_add_member (author, flat) in
    or_fail "member" r
  in
  let* post_a = C.find q_insert_post ("Fshr share target A", flat, author) in
  let* post_a = or_fail "post a" post_a in
  let* post_b = C.find q_insert_post ("Fshr share target B", flat, author) in
  let* post_b = or_fail "post b" post_b in
  Lwt.return { author; outsider; post_a; post_b }

(* The real /c/:slug handler, plus a probe route that renders the SAME
   shared render_post fragment the flat page must splice byte-identically.
   Anonymous rows emit no CSRF material, so the probe's bytes are
   request-independent and must appear verbatim in the page document. *)
let router =
  Dream.router
    [ Dream.get "/c/:slug" Earde.Handlers.community_page_handler
    ; Dream.get "/feed" Earde.Handlers.feed_handler
    ; Dream.get "/search" Earde.Handlers.search_handler
    ; Dream.get "/u/:username" Earde.Handlers.view_profile_handler
    ; Dream.get "/probe/rows/:slug" (fun req ->
          Dream.sql req (fun db ->
              let slug = Dream.param req "slug" in
              let* community = Earde.Db.get_community_by_slug db slug in
              match community with
              | Ok (Some community) ->
                  let* posts =
                    Earde.Db.get_posts_by_community db community.Earde.Db.id
                      Earde.Db.Hot 20 0
                  in
                  (match posts with
                   | Ok posts ->
                       (* The probe pins the community's OWN rows: a
                          fi_shared = None item must splice byte-identically
                          through the plain render_post call. *)
                       Dream.respond
                         (String.concat "\n"
                            (List.map
                               (fun (item : Earde.Db.feed_item) ->
                                 Earde.Components.render_post req []
                                   item.Earde.Db.fi_post)
                               posts))
                   | Error _ -> Dream.respond ~status:`Internal_Server_Error "")
              | _ -> Dream.respond ~status:`Not_Found ""))
    ]

let run ~url ?(session = []) target =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
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
  Lwt.return (Dream.status_to_int (Dream.status response), body)

let session_of uid name =
  [ ("user_id", string_of_int uid); ("username", name) ]

let share_onclick post_id =
  Printf.sprintf "onclick='copyPostLink(\"/p/%d\", this)'" post_id

(* 1: an anonymous public flat page carries exactly one working
   copyPostLink definition, wired to the canonical /p/:id path, with no
   notification fetch and none of the authenticated-only behavior. *)
let guest_case =
  db_case "anonymous flat page: one copyPostLink, no notif fetch, no auth \
           behavior" (fun ~url c ->
      let* fx = make_fixtures c in
      let* status, body = run ~url "/c/fshr-flat" in
      Alcotest.(check int) "200" 200 status;
      Alcotest.(check int) "exactly one copyPostLink definition" 1
        (Html_assert.occurrences body "function copyPostLink");
      Alcotest.(check int) "share button wired to post A" 1
        (Html_assert.occurrences body (share_onclick fx.post_a));
      Alcotest.(check int) "share button wired to post B" 1
        (Html_assert.occurrences body (share_onclick fx.post_b));
      Alcotest.(check int) "no notification fetch" 0
        (Html_assert.occurrences body "/api/unread-notifs");
      Alcotest.(check int) "no notif badge markup or init" 0
        (Html_assert.occurrences body "notif-badge");
      Alcotest.(check int) "no authenticated confirm modal" 0
        (Html_assert.occurrences body "function confirmModal");
      Alcotest.(check int) "no vote handler wiring" 0
        (Html_assert.occurrences body "form[action='/vote']");
      Lwt.return_unit)

(* 2: the authenticated page still carries the full behavior script — and
   still exactly one copyPostLink definition (no duplicate from the guest
   script). *)
let member_case =
  db_case "authenticated flat page: full behavior script, still exactly one \
           copyPostLink" (fun ~url c ->
      let* fx = make_fixtures c in
      let session = session_of fx.author "fshr_author" in
      let* status, body = run ~url ~session "/c/fshr-flat" in
      Alcotest.(check int) "200" 200 status;
      Alcotest.(check int) "exactly one copyPostLink definition" 1
        (Html_assert.occurrences body "function copyPostLink");
      Alcotest.(check int) "no notification fetch (badge is server-rendered)" 0
        (Html_assert.occurrences body "/api/unread-notifs");
      Alcotest.(check int) "confirm modal present" 1
        (Html_assert.occurrences body "function confirmModal");
      Alcotest.(check int) "share button wired" 1
        (Html_assert.occurrences body (share_onclick fx.post_a));
      Lwt.return_unit)

(* 3: the spliced feed rows are the untouched shared renderer's bytes: the
   probe route renders Components.render_post for the same posts under the
   same anonymous viewer state, and that fragment appears verbatim. *)
let row_bytes_case =
  db_case "anonymous flat rows are byte-identical to the shared render_post \
           output" (fun ~url c ->
      let* _fx = make_fixtures c in
      let* pstatus, fragment = run ~url "/probe/rows/fshr-flat" in
      Alcotest.(check int) "probe 200" 200 pstatus;
      Alcotest.(check bool) "probe rendered rows" true
        (String.length fragment > 0);
      let* status, body = run ~url "/c/fshr-flat" in
      Alcotest.(check int) "page 200" 200 status;
      Alcotest.(check bool) "row fragment spliced verbatim" true
        (Html_assert.index_from body fragment 0 <> None);
      Lwt.return_unit)

(* 4: the guest share script is flat-route-only — the structured overview
   and the other anonymous launch documents stay script-free. *)
let siblings_case =
  db_case "structured overview, feed, search and profile stay free of the \
           share script" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* _fx = make_fixtures c in
      let* structured =
        C.find q_insert_community ("fshr-struct", true, "public")
      in
      let* _structured = or_fail "structured community" structured in
      Lwt_list.iter_s
        (fun (label, target) ->
          let* status, body = run ~url target in
          Alcotest.(check int) (label ^ ": 200") 200 status;
          Alcotest.(check int) (label ^ ": no copyPostLink") 0
            (Html_assert.occurrences body "function copyPostLink");
          Alcotest.(check int) (label ^ ": no notif fetch") 0
            (Html_assert.occurrences body "/api/unread-notifs");
          Lwt.return_unit)
        [ ("structured overview", "/c/fshr-struct"); ("feed", "/feed");
          ("search", "/search?q=fshr"); ("profile", "/u/fshr_author") ])

(* 5: private flat communities keep the canonical anti-enumeration 404 —
   byte-identical to a missing slug, with no share script and no title
   leak. *)
let private_case =
  db_case "private flat community: outsider gets the canonical 404, no \
           script, no leak" (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* fx = make_fixtures c in
      let* priv =
        C.find q_insert_community ("fshr-priv", false, "private")
      in
      let* priv = or_fail "private community" priv in
      let* secret =
        C.find q_insert_post ("FSHR-PRIV-SECRET title", priv, fx.author)
      in
      let* _secret = or_fail "secret post" secret in
      let outsider = session_of fx.outsider "fshr_outsider" in
      let* s1, denied = run ~url ~session:outsider "/c/fshr-priv" in
      let* s2, missing = run ~url ~session:outsider "/c/fshr-missing" in
      Alcotest.(check (list int)) "both 404" [ 404; 404 ] [ s1; s2 ];
      Alcotest.(check bool) "denied == missing byte-identical" true
        (String.equal denied missing);
      Alcotest.(check bool) "generic copy" true
        (Html_assert.contains denied "This community does not exist.");
      (* The 404 is the legacy msg_page, whose layout script has always
         carried its own copyPostLink — the byte-equality above pins that
         the guest share script changed nothing about this response. *)
      Alcotest.(check bool) "no secret title" false
        (Html_assert.contains denied "FSHR-PRIV-SECRET");
      let* s3, anon = run ~url "/c/fshr-priv" in
      Alcotest.(check int) "anonymous 404" 404 s3;
      Alcotest.(check bool) "anonymous no secret" false
        (Html_assert.contains anon "FSHR-PRIV-SECRET");
      Lwt.return_unit)

let suite =
  [ guest_case; member_case; row_bytes_case; siblings_case; private_case ]

let suites =
    (* Flat-community guest Share: the byte-pinned rows' Share control
       works for anonymous viewers via the guest-only share script (one
       shared copyPostLink source, no notification fetch, no
       authenticated-only behavior), the authenticated document still holds
       exactly one definition, rows stay byte-identical to the shared
       renderer, sibling documents stay script-free, and private flat
       communities keep the canonical anti-enumeration 404.
       Database-gated. *)
  [ ("flat_community_share_script", suite)
  ]
