(* The authenticated top bar's unread-notification badge, end to end.

   Manual testing found the badge in three mutually inconsistent states on
   the same account: absent, present showing "0", and present showing "1".
   All three came from one arrangement: the badge was ALWAYS server-rendered
   as a literal <span ...>0</span> carrying a `hidden` class, revealed by a
   per-page fetch of /api/unread-notifs. Nothing hid it globally — every
   launch page class had to repeat its own `#notif-badge.hidden` rule, so a
   scope that forgot one (launch-community-connections) showed the hard-coded
   zero — and the endpoint answered "0" when its query failed, which the
   browser could not tell from a real zero.

   Now there is one durable query, resolved once per request by
   Notification_badge.middleware, and one renderer shared by every document
   that draws a top bar. The element exists only when there is something to
   show, so these cases assert absence as strictly as presence. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let contains haystack needle = Html_assert.occurs haystack ~needle

let status_of = Http_fixture.status_of

let add_top_mod conn ~user ~community =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, "top_mod")

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications \
       WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'nbdg_%')"
    ; "DELETE FROM community_connection_audit_events \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'nbdg-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'nbdg-%')"
    ; "DELETE FROM community_connections \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'nbdg-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'nbdg-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'nbdg-%'"
    ; "DELETE FROM users WHERE username LIKE 'nbdg_%'"
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

(* Ordinary prose notifications: the legacy row shape, which is all this
   suite needs — the badge counts rows, not kinds. *)
let q_seed_notification =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
  "INSERT INTO notifications (user_id, notif_type, message) \
   VALUES ($1, 'mention', $2)"

let q_seed_many =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO notifications (user_id, notif_type, message) \
   SELECT $1, 'mention', 'Nbdg bulk ' || g FROM generate_series(1, $2) AS g"

let q_unread =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM notifications \
   WHERE user_id = $1 AND is_read = FALSE"

let seed conn user label =
  exec conn "seed notification" q_seed_notification (user, label)

let unread conn label user expected =
  let* n = find conn (label ^ ": unread") q_unread user in
  Alcotest.(check int) (label ^ ": durable unread count") expected n;
  Lwt.return_unit

(* --- The real routed application, over the real pool --- *)

let gck_secret = "nbdg-test-secret"

let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

(* A document with a top bar and nothing else, so a case can observe the
   badge without the page under it also needing the database. *)
let badge_probe request =
  Dream.html
    (Earde.Page_shell.launch_app_page ~request ~user:"nbdg_probe"
       ~page_class:"launch-feed" ~title:"probe" ~content:Earde.Html.empty ())

let routes =
  Dream.router
    [ Dream.get "/probe" badge_probe
    (* The same document behind the two shapes the middleware must skip: a
       .json catch-up path (with and without its query string) and a
       realtime-token refresh. The probe renders a badge whenever a count
       was stashed, so a badge here means the query ran. *)
    ; Dream.get "/probe.json" badge_probe
    ; Dream.get "/probe/realtime-token" badge_probe
    ; Dream.get "/static/probe" badge_probe
    ; Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler
    ; Dream.get "/c/:slug/settings/connections"
        Earde.Community_connections_handlers.make_connections_page_handler
    ; Dream.get "/notifications" Earde.Account_handlers.notifications_handler
    ]

let session_layer session handler request =
  match session with
  | None -> handler request
  | Some (uid, username) ->
      let* () =
        Dream.set_session_field request "user_id" (string_of_int uid)
      in
      let* () = Dream.set_session_field request "username" username in
      handler request

(* The badge middleware sits exactly where bin/main.ml mounts it: inside
   the pool and the session store, immediately outside the router. *)
let pipeline ?session ~url () =
  sql_pool url @@ Dream.set_secret gck_secret @@ Dream.memory_sessions
  @@ session_layer session
  @@ Earde.Notification_badge.middleware
  @@ routes

let get ~pipeline target =
  let* response = pipeline (Dream.request ~method_:`GET ~target "") in
  let* body = Dream.body response in
  Lwt.return (status_of response, body)

let badge_markup n =
  Printf.sprintf "<span id='notif-badge' class='bell__count'>%s</span>" n

(* Absence is checked on the element and on its class, so neither a
   hard-coded zero nor an empty badge can slip through. *)
let check_badge label body expected =
  Alcotest.(check bool)
    (label ^ ": the bell itself is unconditional")
    true (contains body "class='bell'");
  match expected with
  | None ->
      Alcotest.(check bool)
        (label ^ ": no badge element") false (contains body "notif-badge");
      Alcotest.(check bool)
        (label ^ ": no badge class") false (contains body "bell__count")
  | Some n ->
      Alcotest.(check bool)
        (label ^ ": badge reads " ^ n)
        true
        (contains body (badge_markup n))

(* --- 0. no production source can hard-code a badge ---------------------- *)

let sources_case =
  Alcotest.test_case
    "the badge markup exists in exactly one production source" `Quick
    (fun () ->
      (* dune test runs against _build, where the ppx leaves a .pp.ml
         beside each preprocessed source; they are the same code twice. *)
      let sources =
        List.filter
          (fun (path, _) -> not (Filename.check_suffix path ".pp.ml"))
          Source_census.production_sources
      in
      let owners =
        List.filter (fun (_, body) -> contains body "bell__count") sources
        |> List.map fst
      in
      Alcotest.(check (list string))
        "only Notification_badge renders the badge"
        [ "lib/notification_badge.ml" ] owners;
      List.iter
        (fun (path, body) ->
          List.iter
            (fun needle ->
              if contains body needle then
                Alcotest.failf "%s still contains %S" path needle)
            [ "bell__count hidden"; "fetch('/api/unread-notifs')" ])
        sources)

(* --- 1. zero renders nothing; one renders "1" -------------------------- *)

let zero_then_one_case =
  db_case
    "zero unread renders the bell with no badge and no zero; one unread \
     renders 1" (fun ~url conn ->
      let* user = insert_user conn "nbdg_zo" in
      let* _cid = insert_community ~name:"Nbdg Zero" conn "nbdg-zero" in
      let p = pipeline ~session:(user, "nbdg_zo") ~url () in
      let* status, body = get ~pipeline:p "/c/nbdg-zero" in
      Alcotest.(check int) "community page 200" 200 status;
      check_badge "zero" body None;
      let* () = seed conn user "Nbdg first" in
      let* status, body = get ~pipeline:p "/c/nbdg-zero" in
      Alcotest.(check int) "community page 200 again" 200 status;
      check_badge "one" body (Some "1");
      Lwt.return_unit)

(* --- 1b. only document requests pay for the count ----------------------- *)

(* The live-chat page re-reads messages.json after every burst and refreshes
   its realtime token on a timer, so a count attached to those would ride the
   chat polling loop instead of page loads. Each target below renders the
   same probe document: a badge means the middleware counted, its absence
   means it skipped. *)
let request_scope_case =
  db_case
    "only document requests incur the unread count; json, realtime-token \
     and asset paths do not" (fun ~url conn ->
      let* user = insert_user conn "nbdg_scope" in
      let* () = seed conn user "Nbdg scope" in
      let p = pipeline ~session:(user, "nbdg_scope") ~url () in
      (* The control: an ordinary document does count. *)
      let* status, body = get ~pipeline:p "/probe" in
      Alcotest.(check int) "document 200" 200 status;
      check_badge "document" body (Some "1");
      let* () =
        Lwt_list.iter_s
          (fun (label, target) ->
            let* status, body = get ~pipeline:p target in
            Alcotest.(check int) (label ^ ": 200") 200 status;
            check_badge label body None;
            Lwt.return_unit)
          [ ("catch-up json", "/probe.json")
          ; ("catch-up json with query", "/probe.json?after_id=12")
          ; ("realtime token", "/probe/realtime-token")
          ; ("asset", "/static/probe")
          ]
      in
      (* An anonymous document never counts either. *)
      let anon = pipeline ~url () in
      let* status, body = get ~pipeline:anon "/probe" in
      Alcotest.(check int) "anonymous 200" 200 status;
      check_badge "anonymous" body None;
      Lwt.return_unit)

(* --- 2. one count, every page ------------------------------------------ *)

let parity_case =
  db_case
    "a positive count is identical on an ordinary community page and on \
     the connections management page" (fun ~url conn ->
      let* top = insert_user conn "nbdg_par" in
      let* cid = insert_community ~name:"Nbdg Parity" conn "nbdg-par" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = seed conn top "Nbdg a" in
      let* () = seed conn top "Nbdg b" in
      let* () = seed conn top "Nbdg c" in
      let p = pipeline ~session:(top, "nbdg_par") ~url () in
      let* home_status, home = get ~pipeline:p "/c/nbdg-par" in
      Alcotest.(check int) "community home 200" 200 home_status;
      let* mgmt_status, mgmt =
        get ~pipeline:p "/c/nbdg-par/settings/connections"
      in
      Alcotest.(check int) "connections page 200" 200 mgmt_status;
      check_badge "community home" home (Some "3");
      check_badge "connections management" mgmt (Some "3");
      (* Neither page renders a second, disagreeing badge. *)
      Alcotest.(check int) "home: one badge" 1
        (Html_assert.count_sub home "id='notif-badge'");
      Alcotest.(check int) "connections: one badge" 1
        (Html_assert.count_sub mgmt "id='notif-badge'");
      Lwt.return_unit)

(* --- 3. read semantics ------------------------------------------------- *)

let read_semantics_case =
  db_case
    "unrelated pages never mark notifications read; the notifications page \
     marks the whole mailbox read" (fun ~url conn ->
      let* top = insert_user conn "nbdg_read" in
      let* cid = insert_community ~name:"Nbdg Read" conn "nbdg-read" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = seed conn top "Nbdg r1" in
      let* () = seed conn top "Nbdg r2" in
      let p = pipeline ~session:(top, "nbdg_read") ~url () in
      (* Three ordinary authenticated pages, none of which is the
         notification centre. *)
      let* () =
        Lwt_list.iter_s
          (fun target ->
            let* status, body = get ~pipeline:p target in
            Alcotest.(check int) (target ^ ": 200") 200 status;
            check_badge target body (Some "2");
            unread conn target top 2)
          [ "/c/nbdg-read"; "/c/nbdg-read/settings/connections"; "/probe" ]
      in
      (* The one established view action, and only it, persists read
         state — for the whole mailbox, exactly as before. *)
      let* status, _ = get ~pipeline:p "/notifications" in
      Alcotest.(check int) "notifications 200" 200 status;
      let* () = unread conn "after the notification centre" top 0 in
      let* _status, body = get ~pipeline:p "/probe" in
      check_badge "after the notification centre" body None;
      Lwt.return_unit)

(* --- 4. a failed count is not a zero ----------------------------------- *)

let failure_case =
  db_case "a failed count renders no badge, never a fabricated zero"
    (fun ~url conn ->
      let* user = insert_user conn "nbdg_fail" in
      let* () = seed conn user "Nbdg f1" in
      let* () = seed conn user "Nbdg f2" in
      (* Healthy first, so the difference is the failure and nothing
         else. *)
      let ok = pipeline ~session:(user, "nbdg_fail") ~url () in
      let* _status, body = get ~pipeline:ok "/probe" in
      check_badge "healthy" body (Some "2");
      (* A pool whose connections resolve no unqualified table names: the
         count query fails for real. *)
      let poisoned_url =
        Uri.to_string
          (Uri.add_query_param' (Uri.of_string url)
             ("options", "-csearch_path=nbdg_void"))
      in
      let broken =
        Dream.sql_pool ~size:1 poisoned_url
        @@ Dream.set_secret gck_secret @@ Dream.memory_sessions
        @@ session_layer (Some (user, "nbdg_fail"))
        @@ Earde.Notification_badge.middleware
        @@ routes
      in
      let* status, body = get ~pipeline:broken "/probe" in
      (* The page itself still renders: the badge fails soft. *)
      Alcotest.(check int) "page still 200" 200 status;
      check_badge "failed count" body None;
      (* And nothing about the failure reaches the document. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("failed count: no " ^ needle)
            false (contains body needle))
        [ "nbdg_void"; "search_path"; "Caqti"; "PostgreSQL"; "relation" ];
      Lwt.return_unit)

(* --- 5. a large mailbox cannot widen the top bar ----------------------- *)

let cap_case =
  db_case "a count past the display cap renders 99+" (fun ~url conn ->
      let* user = insert_user conn "nbdg_cap" in
      let* () = exec conn "bulk seed" q_seed_many (user, 150) in
      let* () = unread conn "seeded" user 150 in
      let p = pipeline ~session:(user, "nbdg_cap") ~url () in
      let* _status, body = get ~pipeline:p "/probe" in
      check_badge "capped" body (Some "99+");
      Alcotest.(check bool) "the real count is not rendered" false
        (contains body (badge_markup "150"));
      Lwt.return_unit)

(* --- 6. a connection notification counts, and still links home --------- *)

let connection_notification_case =
  db_case
    "a community-connection notification is counted by the badge and links \
     to the recipient's own management context" (fun ~url conn ->
      let* requester_top = insert_user conn "nbdg_cn_req" in
      let* recipient_top = insert_user conn "nbdg_cn_rec" in
      let* a = insert_community ~name:"Nbdg Conn A" conn "nbdg-cn-a" in
      let* b = insert_community ~name:"Nbdg Conn B" conn "nbdg-cn-b" in
      let* () = add_top_mod conn ~user:requester_top ~community:a in
      let* () = add_top_mod conn ~user:recipient_top ~community:b in
      let* _id =
        Http_fixture.request_direct conn ~actor:requester_top ~requester:a
          ~recipient:b ()
      in
      (* The recipient's top mod: one unread, counted. The actor: none. *)
      let recipient = pipeline ~session:(recipient_top, "nbdg_cn_rec") ~url () in
      let* _status, body = get ~pipeline:recipient "/probe" in
      check_badge "recipient" body (Some "1");
      let actor = pipeline ~session:(requester_top, "nbdg_cn_req") ~url () in
      let* _status, body = get ~pipeline:actor "/probe" in
      check_badge "actor" body None;
      (* And the notification still points at the recipient's own
         connections page, not the requester's. *)
      let* status, page = get ~pipeline:recipient "/notifications" in
      Alcotest.(check int) "notifications 200" 200 status;
      Alcotest.(check bool) "links to the recipient's management context"
        true
        (contains page "href='/c/nbdg-cn-b/settings/connections'");
      Alcotest.(check bool) "not the requester's" false
        (contains page "href='/c/nbdg-cn-a/settings/connections'");
      Lwt.return_unit)

let suite =
  [ sources_case; zero_then_one_case; request_scope_case; parity_case
  ; read_semantics_case; failure_case; cap_case
  ; connection_notification_case ]

let suites =
  [ ("notification_badge", suite)
  ]
