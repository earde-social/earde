(* === PUBLIC PAGINATION BOUND ===
   Offset pages cost the database every skipped row, so the anonymous page
   number is bounded at Public_pagination.max_page. The parser is checked
   DB-free; the handlers are run with no SQL pool at all, so an out-of-range
   page can only answer if it is refused before any database work, while an
   in-range page reaches the database boundary. The gated cases serve the
   deepest permitted page through the production router. *)

let ( let* ) = Lwt.bind

let parse_case =
  Alcotest.test_case "page parsing: bounded, malformed falls back, no overflow"
    `Quick (fun () ->
      let show = function
        | Ok n -> string_of_int n
        | Error `Out_of_range -> "out of range"
      in
      List.iter
        (fun (raw, expected) ->
          Alcotest.(check string)
            (Option.value raw ~default:"<absent>")
            expected
            (show (Earde.Public_pagination.parse raw)))
        [
          (None, "1");
          (Some "", "1");
          (Some "0", "1");
          (Some "1", "1");
          (Some " 7 ", "7");
          (Some "999", "999");
          (Some "1000", "1000");
          (Some "0001000", "1000");
          (Some "1001", "out of range");
          (Some "5000", "out of range");
          (Some "4611686018427387904", "out of range");
          (Some "99999999999999999999999999999999999999", "out of range");
          (Some (String.make 10_000 '9'), "out of range");
          (Some "-5", "1");
          (Some "+5", "1");
          (Some "0x3e8", "1");
          (Some "1e3", "1");
          (Some "1_000", "1");
          (Some "abc", "1");
          (Some "10 01", "1");
        ];
      Alcotest.(check int)
        "deepest offset" 19_980
        (Earde.Public_pagination.offset 1000))

(* No pool: Dream.sql raises, which gate_run reports as `Db_boundary. *)
let routes =
  Dream.router
    [
      Dream.get "/feed" Earde.Public_handlers.feed_handler;
      Dream.get "/search" Earde.Public_handlers.search_handler;
      Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
      Dream.get "/c/:slug/s/:section_slug"
        Earde.Community_handlers.community_section_handler;
    ]

let refused_before_db_case =
  Alcotest.test_case "an out-of-range page is refused before any database work"
    `Quick (fun () ->
      List.iter
        (fun target ->
          match
            Http_fixture.gate_run ~method_:`GET ~target
              (Dream.memory_sessions routes)
          with
          | `Db_boundary -> Alcotest.failf "%s reached the database" target
          | `Response response ->
              Alcotest.(check int)
                (target ^ ": 400") 400
                (Http_fixture.status_of response);
              let body = Lwt_main.run (Dream.body response) in
              Alcotest.(check bool)
                (target ^ ": says why") true
                (Html_assert.contains body "Page out of range"))
        [
          "/feed?page=1001";
          "/feed?sort=top&page=99999999999999999999999";
          "/search?q=word&page=1001";
          "/search?q=word&t=comments&page=5000";
          "/c/anything?page=1001";
          "/c/anything/s/general?page=4611686018427387904";
        ];
      List.iter
        (fun target ->
          match
            Http_fixture.gate_run ~method_:`GET ~target
              (Dream.memory_sessions routes)
          with
          | `Db_boundary -> ()
          | `Response response ->
              Alcotest.failf "%s answered %d without the database" target
                (Http_fixture.status_of response))
        [
          "/feed?page=1000";
          "/feed?page=abc";
          "/search?q=word&page=1000";
          "/c/anything?page=1000";
          "/c/anything/s/general?page=-3";
        ];
      (* The empty-query search prompt never reads the page number and stays
         DB-free for anonymous viewers, as before. *)
      match
        Http_fixture.gate_run ~method_:`GET ~target:"/search?page=5000"
          (Dream.memory_sessions routes)
      with
      | `Db_boundary -> Alcotest.fail "empty search prompt reached the database"
      | `Response response ->
          Alcotest.(check int)
            "empty prompt" 200
            (Http_fixture.status_of response))

(* === gated: the deepest permitted page still serves === *)

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM comments WHERE post_id IN (SELECT p.id FROM posts p JOIN \
       communities c ON c.id = p.community_id WHERE c.slug = 'ppg-home')";
      "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities \
       WHERE slug = 'ppg-home')";
      "DELETE FROM communities WHERE slug = 'ppg-home'";
      "DELETE FROM users WHERE username = 'ppg_author'";
    ]

let q_seed =
  (Caqti_type.unit ->. Caqti_type.unit)
    "WITH u AS (INSERT INTO users (username, email, password_hash, \
     is_email_verified) VALUES ('ppg_author', 'ppg@ppg.invalid', 'x', TRUE) \
     RETURNING id), c AS (INSERT INTO communities (slug, name, visibility) \
     VALUES ('ppg-home', 'ppg-home', 'public') RETURNING id), p AS (INSERT \
     INTO posts (title, content, community_id, user_id) SELECT 'ppg post ' || \
     g, 'ppgword body', c.id, u.id FROM generate_series(1, 45) g, c, u \
     RETURNING id, user_id) INSERT INTO comments (post_id, user_id, content) \
     SELECT p.id, p.user_id, 'ppgword comment' FROM p"

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let gated_case =
  Alcotest.test_case
    "page 1000 and the last real page serve through the production router"
    `Quick (fun () ->
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
                   or_fail "cleanup" r)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () ->
                 let* r = C.exec q_seed () in
                 let* () = or_fail "seed" r in
                 let app = App_fixture.app ~url ~client:"ppg-client" in
                 let b = App_fixture.browser () in
                 Lwt_list.iter_s
                   (fun (target, needle, present) ->
                     let* status, _, body = App_fixture.get app b target in
                     Alcotest.(check int) (target ^ ": status") 200 status;
                     Alcotest.(check bool)
                       (target ^ ": " ^ needle)
                       present
                       (Html_assert.contains body needle);
                     Lwt.return_unit)
                   [
                     ("/c/ppg-home?sort=new", "ppg post", true);
                     ("/c/ppg-home?sort=new&page=3", "ppg post", true);
                     ("/c/ppg-home?page=1000", "ppg-home", true);
                     ("/feed?page=1000", "ppg post", false);
                     ("/search?q=ppgword&page=3", "ppg post", true);
                     ("/search?q=ppgword&page=1000", "ppg post", false);
                     ("/search?q=ppgword&t=comments", "ppgword comment", true);
                     ( "/search?q=ppgword&t=comments&page=1000",
                       "ppgword comment",
                       false );
                   ])
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let suites =
  [
    ("public_pagination", [ parse_case; refused_before_db_case ]);
    ("public_pagination_db", [ gated_case ]);
  ]
