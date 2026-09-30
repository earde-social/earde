(* Correctness-boundary invariants: /search links must percent-encode their
   q/t values so hostile queries round-trip without altering URL structure;
   Location and Content-Disposition headers must be built only from
   database-authoritative or numeric values, never from submitted fields;
   and a database failure must render a stable generic message — Caqti error
   strings (driver detail, connection metadata, SQL text) stay server-side.
   The URL suite is DB-free; the gated suites run the real handlers behind
   sql_pool + memory_sessions, with database failures forced through pools
   whose connections resolve no (or only some) unqualified table names. *)

let ( let* ) = Lwt.bind

let contains hay needle =
  let ln = String.length needle and lh = String.length hay in
  let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i + 1)) in
  ln > 0 && go 0

(* === DB-free: /search link URL encoding === *)

(* Exact inverse of Components.html_escape (its five entities only), so a
   parsed href can be fed to Uri the way a browser would after entity
   decoding. *)
let html_unescape s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  let starts p =
    !i + String.length p <= n && String.sub s !i (String.length p) = p
  in
  while !i < n do
    if starts "&amp;" then (Buffer.add_char b '&'; i := !i + 5)
    else if starts "&lt;" then (Buffer.add_char b '<'; i := !i + 4)
    else if starts "&gt;" then (Buffer.add_char b '>'; i := !i + 4)
    else if starts "&quot;" then (Buffer.add_char b '"'; i := !i + 6)
    else if starts "&#39;" then (Buffer.add_char b '\''; i := !i + 5)
    else (Buffer.add_char b s.[!i]; incr i)
  done;
  Buffer.contents b

(* Every href='...' attribute value pointing at /search. *)
let search_hrefs body =
  let needle = "href='" in
  let nlen = String.length needle in
  let acc = ref [] in
  let rec scan from =
    if from + nlen > String.length body then ()
    else if String.sub body from nlen = needle then (
      let start = from + nlen in
      match String.index_from_opt body start '\'' with
      | None -> ()
      | Some stop ->
          let href = String.sub body start (stop - start) in
          if String.length href >= 8 && String.sub href 0 8 = "/search?"
          then acc := href :: !acc;
          scan (stop + 1))
    else scan (from + 1)
  in
  scan 0;
  List.rev !acc

let render_search ~q ~tab ~page =
  let pipeline =
    Dream.memory_sessions @@ fun req ->
    Dream.html
      (Earde.Pages.search_results_page ~admin_usernames:[] [] page tab q [] [] []
         [] req)
  in
  let* response =
    pipeline (Dream.request ~method_:`GET ~target:"/search" "")
  in
  Dream.body response

let parse_href href = Uri.of_string (html_unescape href)

let pager_links hrefs =
  List.filter
    (fun h -> Uri.get_query_param (parse_href h) "page" <> None)
    hrefs

(* Covers &, =, #, both quote kinds, spaces, '+', backslash and non-ASCII
   (Latin + CJK). Semantic assertion: the URL, parsed and decoded the way a
   navigation would, must yield the original value back. *)
let hostile_q = "a&b=c#d 'e' \"f\" +g\\h \xc3\xa0\xe6\xbc\xa2"

let closed_tabs = [ "posts"; "communities"; "comments"; "people" ]

let url_q_case =
  Alcotest.test_case
    "hostile query round-trips through every /search link" `Quick
    (fun () ->
      Lwt_main.run
        (let* body = render_search ~q:hostile_q ~tab:"posts" ~page:2 in
         let hrefs = search_hrefs body in
         (* four tab links + the Prev pager link *)
         Alcotest.(check int) "search link count" 5 (List.length hrefs);
         List.iter
           (fun href ->
             let u = parse_href href in
             (match Uri.get_query_param u "q" with
              | Some v ->
                  Alcotest.(check string) "q round-trips" hostile_q v
              | None -> Alcotest.failf "no q parameter in %s" href);
             (match Uri.get_query_param u "t" with
              | Some t ->
                  Alcotest.(check bool) "t stays closed" true
                    (List.mem t closed_tabs)
              | None -> Alcotest.failf "no t parameter in %s" href);
             Alcotest.(check bool) "no fragment" true
               (Uri.fragment u = None))
           hrefs;
         (match pager_links hrefs with
          | [ h ] ->
              Alcotest.(check (option string)) "prev page intact"
                (Some "1")
                (Uri.get_query_param (parse_href h) "page")
          | l ->
              Alcotest.failf "expected exactly one pager link, got %d"
                (List.length l));
         Lwt.return_unit))

let hostile_t = "posts&admin=1#x 'y' \"z\""

let url_t_case =
  Alcotest.test_case
    "hostile tab value cannot smuggle parameters or truncate the URL"
    `Quick
    (fun () ->
      Lwt_main.run
        (let* body = render_search ~q:"hello world" ~tab:hostile_t ~page:2 in
         match pager_links (search_hrefs body) with
         | [ h ] ->
             let u = parse_href h in
             Alcotest.(check (option string)) "t round-trips"
               (Some hostile_t)
               (Uri.get_query_param u "t");
             Alcotest.(check (option string)) "q round-trips"
               (Some "hello world")
               (Uri.get_query_param u "q");
             Alcotest.(check (option string)) "no smuggled parameter" None
               (Uri.get_query_param u "admin");
             Alcotest.(check bool) "no fragment" true (Uri.fragment u = None);
             Lwt.return_unit
         | l ->
             Alcotest.failf "expected exactly one pager link, got %d"
               (List.length l)))

let url_suite = [ url_q_case; url_t_case ]

(* === Database-gated: header sinks and error disclosure === *)

open Caqti_request.Infix

(* Cleanup uses the exact fixed test identities: in SQL LIKE, '_' is a
   single-character wildcard, so 'osshard_%' would also match unrelated
   usernames such as 'osshardX...'. The only users these tests create are
   'osshard_admin' and the ban target 'osshard_target'. *)
let test_usernames = "('osshard_admin', 'osshard_target')"

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DROP SCHEMA IF EXISTS osshard_half CASCADE"
    ; "DROP SCHEMA IF EXISTS osshard_nosessions CASCADE"
    ; "DROP SCHEMA IF EXISTS osshard_frozen CASCADE"
    ; "DELETE FROM dream_session WHERE id LIKE 'osshard-%'"
    ; "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username IN "
      ^ test_usernames ^ ")"
    ; "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'osshard-%')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'osshard-%')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'osshard-%')"
    ; "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'osshard-%')"
    ; "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'osshard-%')"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || c.id::text FROM communities c WHERE c.slug LIKE 'osshard-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'osshard-%'"
    ; "DELETE FROM users WHERE username IN " ^ test_usernames
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
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let q_insert_community =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO communities (slug, name) VALUES ($1, $1) RETURNING id"

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let boundary = "osshardboundary"

let multipart_body fields =
  String.concat ""
    (List.map
       (fun (k, v) ->
         Printf.sprintf
           "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
           boundary k v)
       fields)
  ^ Printf.sprintf "--%s--\r\n" boundary

(* POST runner: presets session fields and injects a fresh dream.csrf into
   the (urlencoded or multipart) body, so Dream.form/Dream.multipart CSRF
   validation passes and the handler's own logic is what gets exercised. *)
let run_post ~url ?(multipart = false) ~session ~target ~form handler =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    let csrf = Dream.csrf_token req in
    let fields = ("dream.csrf", csrf) :: form in
    Dream.set_body req
      (if multipart then multipart_body fields else form_body fields);
    handler req
  in
  let headers =
    [ ( "Content-Type",
        if multipart then "multipart/form-data; boundary=" ^ boundary
        else "application/x-www-form-urlencoded" ) ]
  in
  pipeline (Dream.request ~method_:`POST ~target ~headers "")

let run_get ~url ~session ~target handler =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    handler req
  in
  pipeline (Dream.request ~method_:`GET ~target "")

(* The admin overrides these routes take are decided on the DURABLE
   users.is_admin row; the session claim only enables the lookup. *)
let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE users SET is_admin = TRUE WHERE id = $1"

let insert_admin (module C : Caqti_lwt.CONNECTION) username =
  let* uid = C.find q_insert_user username in
  let* uid = or_fail "user" uid in
  let* r = C.exec q_make_admin uid in
  let* () = or_fail "durable admin" r in
  Lwt.return uid

let admin_session uid =
  [ ("user_id", string_of_int uid); ("username", "osshard_admin");
    ("is_admin", "true") ]

let status_int r = Dream.status_to_int (Dream.status r)

(* CR, LF, a full header-injection payload, quotes, slashes, backslash,
   protocol-relative and absolute external URLs. *)
let hostile_slugs =
  [ "evil\rX: 1"; "evil\nX: 1"; "evil\r\nX-Evil: injected"; "\"quoted\"";
    "sla/sh"; "back\\slash"; "//example.com"; "https://example.com" ]

(* No hostile fragment may appear in ANY response header, and no header
   value may contain a raw CR/LF (which would mint an additional header). *)
let assert_clean_headers label response =
  List.iter
    (fun (name, value) ->
      if
        contains value "example.com" || contains value "X-Evil"
        || String.exists (fun c -> c = '\r' || c = '\n') value
      then
        Alcotest.failf "%s: hostile content in header %s: %S" label name
          value)
    (Dream.all_headers response)

let unban_location_case =
  db_case "unban: Location comes from the database slug, never the form"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* cid = C.find q_insert_community "osshard-real" in
      let* cid = or_fail "community" cid in
      Lwt_list.iter_s
        (fun slug ->
          let* response =
            run_post ~url ~session:(admin_session uid)
              ~target:"/unban-community-user"
              ~form:
                [ ("community_id", string_of_int cid)
                ; ("community_slug", slug)
                ; ("target_user_id", string_of_int uid) ]
              Earde.Handlers.unban_community_user_handler
          in
          Alcotest.(check int) "303" 303 (status_int response);
          Alcotest.(check (option string)) "authoritative Location"
            (Some "/c/osshard-real/settings?panel=bans")
            (Dream.header response "Location");
          assert_clean_headers "unban" response;
          Lwt.return_unit)
        hostile_slugs)

let update_community_location_case =
  db_case
    "update-community: redirect and return URLs use the database slug"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* cid = C.find q_insert_community "osshard-real" in
      let* cid = or_fail "community" cid in
      Lwt_list.iter_s
        (fun slug ->
          let* response =
            run_post ~url ~multipart:true ~session:(admin_session uid)
              ~target:"/update-community"
              ~form:
                [ ("community_id", string_of_int cid)
                ; ("community_slug", slug)
                ; ("description", "osshard description")
                ; ("rules", "")
                ; ("avatar_url", "")
                ; ("banner_url", "")
                ; ("existing_avatar_url", "")
                ; ("existing_banner_url", "") ]
              Earde.Handlers.update_community_handler
          in
          Alcotest.(check int) "303" 303 (status_int response);
          Alcotest.(check (option string)) "authoritative Location"
            (Some "/c/osshard-real/settings")
            (Dream.header response "Location");
          assert_clean_headers "update-community" response;
          Lwt.return_unit)
        hostile_slugs)

let update_missing_community_case =
  db_case
    "update-community: a nonexistent id falls back to / without echoing"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* response =
        run_post ~url ~multipart:true ~session:(admin_session uid)
          ~target:"/update-community"
          ~form:
            [ ("community_id", "999999999")
            ; ("community_slug", "//example.com")
            ; ("description", "osshard description")
            ; ("rules", "")
            ; ("avatar_url", "")
            ; ("banner_url", "")
            ; ("existing_avatar_url", "")
            ; ("existing_banner_url", "") ]
          Earde.Handlers.update_community_handler
      in
      Alcotest.(check int) "303" 303 (status_int response);
      Alcotest.(check (option string)) "root Location" (Some "/")
        (Dream.header response "Location");
      assert_clean_headers "update-community missing" response;
      Lwt.return_unit)

let export_disposition_case =
  db_case "export: Content-Disposition filename derives from the user id"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* response =
        run_get ~url ~session:(admin_session uid) ~target:"/export-data"
          Earde.Handlers.export_data_handler
      in
      Alcotest.(check int) "200" 200 (status_int response);
      Alcotest.(check (option string)) "conservative ASCII filename"
        (Some
           (Printf.sprintf
              "attachment; filename=\"earde_export_user_%d.json\"" uid))
        (Dream.header response "Content-Disposition");
      Lwt.return_unit)

let header_suite =
  [ unban_location_case; update_community_location_case;
    update_missing_community_case; export_disposition_case ]

(* --- forced database failures must stay payload-free --- *)

(* A pool whose connections resolve only the tables of [schema]: with the
   nonexistent 'osshard_void' every store call fails; with 'osshard_half'
   (a schema exposing only a communities view) the community lookup
   succeeds and the *next* query fails, reaching deeper error arms. Either
   way the failure is a real PostgreSQL error carrying SQL text and the
   schema name — none of which may reach the body. *)
let with_search_path url schema =
  Uri.to_string
    (Uri.add_query_param' (Uri.of_string url)
       ("options", "-csearch_path=" ^ schema))

let poison url = with_search_path url "osshard_void"

let q_half_schema =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DROP SCHEMA IF EXISTS osshard_half CASCADE"
    ; "CREATE SCHEMA osshard_half"
    ; "CREATE VIEW osshard_half.communities AS SELECT * FROM public.communities"
      (* users rides along so the handlers' CURRENT-admin lookup succeeds and
         the failure stays where each case wants it — the promote role/write
         and the community-ban write. Without it every admin-gated handler
         would fail at the authority lookup instead, and these cases would
         stop covering the arms they name. *)
    ; "CREATE VIEW osshard_half.users AS SELECT * FROM public.users"
    ]

let make_half_schema (module C : Caqti_lwt.CONNECTION) =
  Lwt_list.iter_s
    (fun q ->
      let* r = C.exec q () in
      let* _ = or_fail "half schema" r in
      Lwt.return_unit)
    q_half_schema

let generic = "A database error occurred. Please try again later."

let db_needles =
  [ "osshard_void"; "osshard_half"; "search_path"; "postgresql"; "caqti";
    "relation"; "constraint"; "select"; "insert" ]

let must body s =
  if not (contains body s) then
    Alcotest.failf "expected body to contain %S" s

(* Case-insensitive: driver error text varies in casing ("SELECT" in SQL,
   "relation" in PostgreSQL messages, "Caqti"/"caqti" in module paths). *)
let must_not body s =
  if contains (String.lowercase_ascii body) (String.lowercase_ascii s) then
    Alcotest.failf "body leaks %S" s

let verify_email_disclosure_case =
  db_case "verify-email: a database failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* response =
        run_get ~url:(poison url) ~session:[]
          ~target:"/verify-email?token=osshard-token"
          Earde.Handlers.verify_email_handler
      in
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let add_section_disclosure_case =
  db_case "add-section: a database failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* response =
        run_post ~url:(poison url) ~session:(admin_session uid)
          ~target:"/c/osshard-real/add-section"
          ~form:
            [ ("name", "General"); ("description", "");
              ("default_sort", "hot"); ("position", "1") ]
          (Dream.router
             [ Dream.post "/c/:slug/add-section"
                 Earde.Handlers.add_section_handler ])
      in
      Alcotest.(check int) "500" 500 (status_int response);
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let update_community_disclosure_case =
  db_case
    "update-community: a database failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* response =
        run_post ~url:(poison url) ~multipart:true
          ~session:(admin_session uid) ~target:"/update-community"
          ~form:
            [ ("community_id", "1")
            ; ("community_slug", "osshard-x")
            ; ("description", "osshard description")
            ; ("rules", "")
            ; ("avatar_url", "")
            ; ("banner_url", "")
            ; ("existing_avatar_url", "")
            ; ("existing_banner_url", "") ]
          Earde.Handlers.update_community_handler
      in
      Alcotest.(check int) "500" 500 (status_int response);
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let global_ban_disclosure_case =
  db_case "global ban: a database failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* response =
        run_post ~url:(poison url) ~session:(admin_session 42)
          ~target:"/admin/ban/user/1" ~form:[]
          (Dream.router
             [ Dream.post "/admin/ban/user/:id"
                 Earde.Handlers.ban_user_handler ])
      in
      (* The current-admin lookup is the first query this route makes, so a
         pool where nothing resolves fails there — one query earlier than the
         ban write it used to fail at. This case now covers the
         AUTHORIZATION-lookup failure; the ban write's own failure is covered
         separately by global_ban_mutation_failure_case. Either way: the
         generic message, no driver text, and no ban performed. *)
      Alcotest.(check int) "500" 500 (status_int response);
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let global_unban_disclosure_case =
  db_case
    "global unban: a database failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* response =
        run_post ~url:(poison url) ~session:(admin_session 42)
          ~target:"/admin/unban/user/1" ~form:[]
          (Dream.router
             [ Dream.post "/admin/unban/user/:id"
                 Earde.Handlers.unban_user_global_handler ])
      in
      (* Authorization-lookup failure, as above; the unban write's own
         failure is covered by global_unban_mutation_failure_case. *)
      Alcotest.(check int) "500" 500 (status_int response);
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

(* --- authorized mutations whose OWN storage fails ---

   The two cases above break the FIRST query these admin routes make — the
   durable current-admin lookup — so what they prove is that the
   authorization boundary fails closed. They can no longer reach the ban and
   unban writes, so the mutation boundary needs its own injection: a schema
   where the admin lookup SUCCEEDS and the intended write is the only thing
   that fails.

   Each schema is written out by hand rather than mirroring every public
   relation, because these two routes touch one table each (plus
   dream_session), and naming exactly what is present is what makes the
   failure point unambiguous. *)

let make_schema (module C : Caqti_lwt.CONNECTION) statements =
  Lwt_list.iter_s
    (fun sql ->
      let* r = C.exec ((Caqti_type.unit ->. Caqti_type.unit) sql) () in
      let* () = or_fail "schema" r in
      Lwt.return_unit)
    statements

(* [users] is a plain SELECT * view and therefore auto-updatable: the
   current-admin SELECT and Admin_store.ban_user's own "UPDATE users SET is_banned"
   both succeed. What is missing is dream_session, so the session revocation
   inside the ban transaction fails and the ban must roll back. *)
let q_no_sessions_schema =
  [ "DROP SCHEMA IF EXISTS osshard_nosessions CASCADE"
  ; "CREATE SCHEMA osshard_nosessions"
  ; "CREATE VIEW osshard_nosessions.users AS SELECT * FROM public.users"
  ]

(* SELECT DISTINCT is the narrowest way to make a view non-auto-updatable:
   reading is_admin still works, every UPDATE against it is refused. *)
let q_frozen_users_schema =
  [ "DROP SCHEMA IF EXISTS osshard_frozen CASCADE"
  ; "CREATE SCHEMA osshard_frozen"
  ; "CREATE VIEW osshard_frozen.users AS SELECT DISTINCT * FROM public.users"
  ]

let q_set_banned =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
  "UPDATE users SET is_banned = $2 WHERE id = $1"

let q_is_banned =
  (Caqti_type.int ->! Caqti_type.bool)
  "SELECT is_banned FROM users WHERE id = $1"

(* A durable session row for the target, written directly. Admin_store.ban_user
   deletes exactly the rows whose payload names this user_id, so the row
   surviving is what proves the rolled-back ban revoked nothing either. *)
let q_insert_session =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
  "INSERT INTO dream_session (id, label, expires_at, payload)
   VALUES ($1, $1, 9999999999, '{\"user_id\":\"' || $2 || '\"}')"

let q_count_sessions =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM dream_session
    WHERE payload::jsonb ->> 'user_id' = $1"

let global_ban_mutation_failure_case =
  db_case
    "global ban: a failed session revocation rolls the ban back and answers \
     generically"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* target = C.find q_insert_user "osshard_target" in
      let* target = or_fail "target" target in
      let* r =
        C.exec q_insert_session
          ("osshard-target-session", string_of_int target)
      in
      let* () = or_fail "target session" r in
      let* () = make_schema (module C) q_no_sessions_schema in
      let* response =
        run_post
          ~url:(with_search_path url "osshard_nosessions")
          ~session:(admin_session uid)
          ~target:(Printf.sprintf "/admin/ban/user/%d" target)
          ~form:[]
          (Dream.router
             [ Dream.post "/admin/ban/user/:id"
                 Earde.Handlers.ban_user_handler ])
      in
      (* Neither the refusal of a failed authority check nor a redirect: the
         durable admin lookup succeeded, the ban was attempted, and its own
         write failed. The route renders that at 200 — its shape before 02d
         and after it. *)
      Alcotest.(check int) "200" 200 (status_int response);
      let* body = Dream.body response in
      must body generic;
      must_not body "not an Admin";
      List.iter (must_not body) db_needles;
      let* banned = C.find q_is_banned target in
      let* banned = or_fail "target banned" banned in
      Alcotest.(check bool) "no durable ban survives the rollback" false
        banned;
      let* n = C.find q_count_sessions (string_of_int target) in
      let* n = or_fail "target sessions" n in
      Alcotest.(check int) "the target's session survives" 1 n;
      Lwt.return_unit)

let global_unban_mutation_failure_case =
  db_case
    "global unban: a failed unban write leaves the target banned and answers \
     generically"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* target = C.find q_insert_user "osshard_target" in
      let* target = or_fail "target" target in
      let* r = C.exec q_set_banned (target, true) in
      let* () = or_fail "pre-ban" r in
      let* () = make_schema (module C) q_frozen_users_schema in
      let* response =
        run_post
          ~url:(with_search_path url "osshard_frozen")
          ~session:(admin_session uid)
          ~target:(Printf.sprintf "/admin/unban/user/%d" target)
          ~form:[]
          (Dream.router
             [ Dream.post "/admin/unban/user/:id"
                 Earde.Handlers.unban_user_global_handler ])
      in
      Alcotest.(check int) "200" 200 (status_int response);
      Alcotest.(check (option string)) "no Location" None
        (Dream.header response "Location");
      let* body = Dream.body response in
      must body generic;
      must_not body "not an Admin";
      List.iter (must_not body) db_needles;
      let* still = C.find q_is_banned target in
      let* still = or_fail "target still banned" still in
      Alcotest.(check bool) "the target stays banned" true still;
      Lwt.return_unit)

let unban_lookup_failure_case =
  db_case
    "community unban: a failed authoritative lookup errors, never redirects"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* response =
        run_post ~url:(poison url) ~session:(admin_session 42)
          ~target:"/unban-community-user"
          ~form:
            [ ("community_id", "1"); ("community_slug", "//example.com");
              ("target_user_id", "1") ]
          Earde.Handlers.unban_community_user_handler
      in
      Alcotest.(check int) "500" 500 (status_int response);
      Alcotest.(check (option string)) "no Location" None
        (Dream.header response "Location");
      assert_clean_headers "unban lookup failure" response;
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let unban_mutation_failure_case =
  db_case
    "community unban: a failed unban write errors, never fakes success"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* cid = C.find q_insert_community "osshard-real" in
      let* cid = or_fail "community" cid in
      (* Half-visible schema: the authoritative community loads, then the
         DELETE against community_bans fails with a real driver error. *)
      let* () = make_half_schema (module C) in
      let* response =
        run_post
          ~url:(with_search_path url "osshard_half")
          ~session:(admin_session uid) ~target:"/unban-community-user"
          ~form:
            [ ("community_id", string_of_int cid)
            ; ("community_slug", "//example.com")
            ; ("target_user_id", string_of_int uid) ]
          Earde.Handlers.unban_community_user_handler
      in
      Alcotest.(check int) "500" 500 (status_int response);
      Alcotest.(check (option string)) "no Location" None
        (Dream.header response "Location");
      assert_clean_headers "unban mutation failure" response;
      let* body = Dream.body response in
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let disclosure_suite =
  [ verify_email_disclosure_case; add_section_disclosure_case;
    update_community_disclosure_case; global_ban_disclosure_case;
    global_unban_disclosure_case; global_ban_mutation_failure_case;
    global_unban_mutation_failure_case; unban_lookup_failure_case;
    unban_mutation_failure_case ]

(* --- typed Top Mod promotion error contract --- *)

let q_break_path = (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO ''"

let q_reset_path = (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

let promote_typed_contract_case =
  db_case
    "promote_to_top_mod classifies storage vs domain failures at the boundary"
    (fun ~url:_ (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* cid = C.find q_insert_community "osshard-real" in
      let* cid = or_fail "community" cid in
      (* An emptied search_path makes the role SELECT itself fail: the
         result must be the storage constructor, never a domain refusal. *)
      let* broke = C.exec q_break_path () in
      let* () = Lwt.map ignore (or_fail "break search_path" broke) in
      let* storage = Earde.Moderator_store.promote_to_top_mod (module C) uid cid in
      let* reset = C.exec q_reset_path () in
      let* () = Lwt.map ignore (or_fail "reset search_path" reset) in
      (match storage with
       | Error (Earde.Moderator_store.Promotion_storage_error s) ->
           Alcotest.(check bool) "storage detail retained for the log" true
             (String.length s > 0)
       | Error (Earde.Moderator_store.Promotion_refused m) ->
           Alcotest.failf "storage failure classified as domain: %s" m
       | Ok () -> Alcotest.fail "succeeded on a broken connection");
      (* A non-moderator target is a domain refusal with the fixed friendly
         message, never a storage error. *)
      let* refused = Earde.Moderator_store.promote_to_top_mod (module C) uid cid in
      (match refused with
       | Error (Earde.Moderator_store.Promotion_refused m) ->
           Alcotest.(check string) "friendly domain message"
             "User is not a moderator of this community" m
       | Error (Earde.Moderator_store.Promotion_storage_error s) ->
           Alcotest.failf "domain refusal classified as storage: %s" s
       | Ok () -> Alcotest.fail "promoted a non-moderator");
      Lwt.return_unit)

let promote_router =
  Dream.router
    [ Dream.post "/c/:slug/manage-mods/promote"
        Earde.Handlers.manage_mods_promote_handler ]

let promote_storage_disclosure_case =
  db_case "promote: a storage failure renders only the generic message"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* _cid = C.find q_insert_community "osshard-real" in
      let* _cid = or_fail "community" _cid in
      (* Half-visible schema: the current-admin and community lookups
         succeed, the promote role query fails — exercising the handler's
         storage arm. *)
      let* () = make_half_schema (module C) in
      let* response =
        run_post
          ~url:(with_search_path url "osshard_half")
          ~session:(admin_session uid)
          ~target:"/c/osshard-real/manage-mods/promote"
          ~form:[ ("target_user_id", "1") ]
          promote_router
      in
      Alcotest.(check int) "200" 200 (status_int response);
      let* body = Dream.body response in
      must body "Promotion Failed";
      must body generic;
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let promote_domain_message_case =
  db_case "promote: friendly domain refusals stay user-visible"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "osshard_admin" in
      let* _cid = C.find q_insert_community "osshard-real" in
      let* _cid = or_fail "community" _cid in
      let* response =
        run_post ~url ~session:(admin_session uid)
          ~target:"/c/osshard-real/manage-mods/promote"
          ~form:[ ("target_user_id", string_of_int uid) ]
          promote_router
      in
      Alcotest.(check int) "200" 200 (status_int response);
      let* body = Dream.body response in
      must body "User is not a moderator of this community";
      List.iter (must_not body) db_needles;
      Lwt.return_unit)

let promotion_suite =
  [ promote_typed_contract_case; promote_storage_disclosure_case;
    promote_domain_message_case ]

let suites =
    (* Correctness boundaries: /search link URL encoding (DB-free),
       untrusted header values, forced-database-failure disclosure, and the
       typed promotion error contract (gated). *)
  [ ("boundary_search_url_encoding", url_suite)
  ; ("boundary_untrusted_headers", header_suite)
  ; ("boundary_db_error_disclosure", disclosure_suite)
  ; ("boundary_promotion_errors", promotion_suite)
  ]
