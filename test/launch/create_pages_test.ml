module Phrp = Earde.Project_home_review_pages

(* === Final create-page callers (pass 19) ==================================
   The last two live Components.create_page documents move onto launch
   wrappers, with everything else preserved:
   1. GET /new-community (Community_settings_pages.new_community_form) → launch_app_page under
      body.launch-new-community. Renderer pins (DB-free, real session
      middleware for the CSRF tag): wrapper identity, local assets only
      (earde.css + mobile-gate.css, no Tailwind / Google Fonts / shell.css /
      create.css), the byte-preserved POST /communities form contract, one
      behavior script and one notification fetch, and a launch rail fed only
      by handler-supplied joined communities. Handler pins (DB-free — the
      denial paths never reach SQL): the pre-existing global-admin gate.
      Gated: a real global admin receives the form with their real joined
      rail; a real top moderator without the admin session flag keeps the
      /bring redirect; the POST authorization / validation / duplicate /
      CSRF / success contracts are unchanged (handler untouched).
   2. The moderator review queue's degraded document
      (Project_home_review_pages, shell:None — the durable community record
      could not be re-read for chrome) → launch_message_page: the neutral
      body class, no chrome, no scripts, no notification wiring, viewer
      independence, and the identical feature fragment (the pending-request
      list bytes match the launch community document for the same state). *)

let case name f = Alcotest.test_case name `Quick f
let ( let* ) = Lwt.bind

(* --- /new-community renderer --- *)

let render_form ?user ?(rail_communities = []) () =
  let rendered = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    rendered :=
      Some
        (Earde.Community_settings_pages.new_community_form ?user
           ~rail_communities req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/new-community" "")));
  match !rendered with
  | Some html -> html
  | None -> Alcotest.fail "renderer did not run"

let form_marker =
  "<form action='/communities' method='POST' class='create-form' \
   id='new-community-form'>"

let wrapper_case =
  case
    "authorized document: launch wrapper, local assets only, one script, one \
     notification fetch, real rail" (fun () ->
      let page =
        render_form ~user:"ncl_admin"
          ~rail_communities:[ Launch_fixture.nav_test_community ]
          ()
      in
      Html_assert.must page "<body class='launch-new-community'>";
      Html_assert.must page "<title>New Community - Earde</title>";
      Html_assert.must page
        "<link rel='stylesheet' href='/static/css/earde.css'>";
      Html_assert.must page
        "<link rel='stylesheet' href='/static/css/mobile-gate.css'>";
      Alcotest.(check int)
        "exactly two stylesheets" 2
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "shell.css";
      Html_assert.must_not page "create.css";
      Alcotest.(check int)
        "no notification fetch (badge is server-rendered)" 0
        (Html_assert.occurrences page "/api/unread-notifs");
      Alcotest.(check int)
        "exactly one behavior script" 1
        (Html_assert.occurrences page "function confirmModal");
      (* The dark rail: Feed (the only active marker), the real joined
         community, and the + Connect tile — nothing invented, no
         current-page tile for this utility. *)
      Alcotest.(check int)
        "one active rail marker" 1
        (Html_assert.occurrences page "rail__marker");
      Html_assert.must page "href='/c/ocaml/ch/general'";
      Html_assert.must page "rail__item--add' href='/bring'";
      (* Factual administrative context precedes the create-shell marker;
         the page never sells itself as the onboarding flow. *)
      Html_assert.order page "launch-newcomm-context"
        "<div class='create-shell'>";
      Html_assert.must page "Administrator utility";
      Html_assert.must_not page "/projects/new")

let form_contract_case =
  case "the POST /communities form contract is byte-intact" (fun () ->
      let page = render_form ~user:"ncl_admin" () in
      Html_assert.must page form_marker;
      Alcotest.(check int)
        "one creation form" 1
        (Html_assert.occurrences page "action='/communities'");
      Alcotest.(check int)
        "one framework CSRF field" 1
        (Html_assert.occurrences page "dream.csrf");
      Html_assert.must page
        "<input type='hidden' name='section_count' id='section_count' \
         value='0'>";
      Html_assert.must page
        "<input type='hidden' name='channel_count' id='channel_count' \
         value='0'>";
      Html_assert.must page
        "<input type='text' name='name' required class='create-input' \
         placeholder='e.g., Italian Cuisine'>";
      Html_assert.must page
        "<input type='text' name='slug' required class='create-input' \
         placeholder='italian-cuisine'>";
      Html_assert.must page
        "<textarea name='description' class='create-textarea' \
         placeholder='What is this community about?'></textarea>";
      Html_assert.must page "<div class='create-shell'>";
      Html_assert.must page "Start a community";
      Html_assert.must page "<span class='create-chip-name'>general</span>";
      Html_assert.must page "<span class='create-chip-name'>General</span>";
      Html_assert.must page
        "<button type='submit' class='create-btn create-btn--block'>Create \
         community</button>";
      Html_assert.must_not page "enctype";
      Alcotest.(check int)
        "single form id" 1
        (Html_assert.occurrences page "id='new-community-form'");
      (* The row-management scripts the server-side count loop depends on. *)
      Html_assert.must page "function addChannel()";
      Html_assert.must page "function addSection()";
      Html_assert.must page "function renumberChannels()";
      Html_assert.must page "function renumberSections()")

let viewerless_render_case =
  case "viewer-less render carries no script or notification wiring" (fun () ->
      let page = render_form () in
      Html_assert.must page "<body class='launch-new-community'>";
      Html_assert.must_not page "/api/unread-notifs";
      Html_assert.must_not page "confirmModal")

let renderer_suite =
  [ wrapper_case; form_contract_case; viewerless_render_case ]

(* --- /new-community handler: DB-free denial paths (the /bring and
   /login redirects run before any SQL — no pool is installed, so a
   regression that queried before the gate would raise, not pass). --- *)

let handler_router =
  Dream.router
    [ Dream.get "/new-community" Earde.Community_handlers.new_community_page ]

let run_get ?(session = []) target =
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    handler_router req
  in
  Lwt_main.run
    (let* response = pipeline (Dream.request ~method_:`GET ~target "") in
     let* body = Dream.body response in
     Lwt.return
       ( Dream.status_to_int (Dream.status response),
         Dream.header response "Location",
         body ))

let is_redirect s = s = 301 || s = 302 || s = 303 || s = 307 || s = 308

let check_denied_clean label body =
  List.iter
    (fun marker ->
      Alcotest.(check bool)
        (label ^ ": no " ^ marker)
        false
        (Html_assert.contains body marker))
    [
      form_marker;
      "create-shell";
      "launch-new-community";
      "unread-notifs";
      "rail__item";
      "Start a community";
      "dream.csrf";
    ]

let anonymous_gate_case =
  case "anonymous GET keeps the /bring redirect and renders nothing" (fun () ->
      let status, location, body = run_get "/new-community" in
      Alcotest.(check bool) "redirects" true (is_redirect status);
      Alcotest.(check (option string)) "location" (Some "/bring") location;
      check_denied_clean "anonymous" body)

(* Community moderators carry no is_admin session field, so this case is
   also the moderator gate; the gated case below re-proves it against a
   real top_mod row. *)
let ordinary_user_gate_case =
  case "ordinary authenticated GET keeps the /bring redirect" (fun () ->
      let status, location, body =
        run_get
          ~session:[ ("user_id", "424242"); ("username", "ncl_user") ]
          "/new-community"
      in
      Alcotest.(check bool) "redirects" true (is_redirect status);
      Alcotest.(check (option string)) "location" (Some "/bring") location;
      check_denied_clean "ordinary user" body)

(* A session field that claims admin but carries no user id names nobody, so
   there is no durable row to confirm it against and no lookup to make: the
   claim is refused outright, on the ordinary-user path. It used to reach the
   admin branch on the strength of the claim alone and only then fall out to
   /login for want of a user id — the same denial, but reached by granting
   admin first. Both then and now, nothing of the form is rendered. *)
let stale_admin_gate_case =
  case "admin flag without a user id is refused as an ordinary visitor"
    (fun () ->
      let status, location, body =
        run_get ~session:[ ("is_admin", "true") ] "/new-community"
      in
      Alcotest.(check bool) "redirects" true (is_redirect status);
      Alcotest.(check (option string)) "location" (Some "/bring") location;
      check_denied_clean "stale admin" body)

let gate_suite =
  [ anonymous_gate_case; ordinary_user_gate_case; stale_admin_gate_case ]

(* --- Gated: real rows, real pool, real handlers --- *)

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'ncl-%')";
      "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'ncl-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'ncl-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'ncl-%')";
      "DELETE FROM communities WHERE slug LIKE 'ncl-%'";
      "DELETE FROM users WHERE username LIKE 'ncl_%'";
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
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \     VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let q_insert_community =
  (Caqti_type.(t4 string string bool string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, sections_enabled, visibility)\n\
    \     VALUES ($1, $2, $3, $4) RETURNING id"

let q_add_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_add_top_mod =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES \
     ($1, $2, 'top_mod')"

(* Legacy creation authorizes on the DURABLE users.is_admin row; the session
   claim only decides whether that lookup is worth making. An admin fixture
   must therefore really be an admin. *)
let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

let insert_admin (module C : Caqti_lwt.CONNECTION) username =
  let* uid = C.find q_insert_user username in
  let* uid = or_fail "admin" uid in
  let* r = C.exec q_make_admin uid in
  let* () = or_fail "durable admin" r in
  Lwt.return uid

let q_community_id =
  (Caqti_type.string ->? Caqti_type.int)
    "SELECT id FROM communities WHERE slug = $1"

let q_count_sections =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM community_sections WHERE community_id = $1"

let q_count_channels =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM channels WHERE community_id = $1"

let q_has_top_mod =
  (Caqti_type.(t2 int int) ->! Caqti_type.bool)
    "SELECT EXISTS(SELECT 1 FROM community_moderators WHERE user_id = $1 AND \
     community_id = $2 AND role = 'top_mod')"

let q_has_member =
  (Caqti_type.(t2 int int) ->! Caqti_type.bool)
    "SELECT EXISTS(SELECT 1 FROM community_members WHERE user_id = $1 AND \
     community_id = $2)"

let run_get_db ~url ?(session = []) target =
  let pipeline =
    Dream.sql_pool url
    @@ Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    handler_router req
  in
  let* response = pipeline (Dream.request ~method_:`GET ~target "") in
  let* body = Dream.body response in
  Lwt.return
    ( Dream.status_to_int (Dream.status response),
      Dream.header response "Location",
      body )

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

(* POST /communities through the real handler; the CSRF token is minted
   inside the same request/session, exactly like the analytics harness. *)
let run_post ~url ?(session = []) ?(with_csrf = true) form =
  let pipeline =
    Dream.sql_pool url
    @@ Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Lwt_list.iter_s (fun (k, v) -> Dream.set_session_field req k v) session
    in
    let fields =
      if with_csrf then ("dream.csrf", Dream.csrf_token req) :: form else form
    in
    Dream.set_body req (form_body fields);
    Earde.Community_handlers.create_community_handler req
  in
  let request =
    Dream.request ~method_:`POST ~target:"/communities"
      ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
      ""
  in
  let* response = pipeline request in
  let* body = Dream.body response in
  Lwt.return
    ( Dream.status_to_int (Dream.status response),
      Dream.header response "Location",
      body )

let admin_session uid name =
  [ ("user_id", string_of_int uid); ("username", name); ("is_admin", "true") ]

let plain_session uid name =
  [ ("user_id", string_of_int uid); ("username", name) ]

let admin_get_case =
  db_case "global admin gets the real form with the real joined rail"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "ncl_admin" in
      let* cid =
        C.find q_insert_community ("ncl-joined", "NCL Joined", true, "public")
      in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_add_member (uid, cid) in
      let* () = or_fail "membership" r in
      let* status, _, body =
        run_get_db ~url
          ~session:(admin_session uid "ncl_admin")
          "/new-community"
      in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check bool)
        "launch wrapper" true
        (Html_assert.contains body "<body class='launch-new-community'>");
      Alcotest.(check bool)
        "real form" true
        (Html_assert.contains body form_marker);
      Alcotest.(check bool)
        "joined rail tile" true
        (Html_assert.contains body "href='/c/ncl-joined/ch/general'");
      Alcotest.(check int)
        "no notification fetch (badge is server-rendered)" 0
        (Html_assert.occurrences body "/api/unread-notifs");
      Alcotest.(check bool)
        "no shell.css" false
        (Html_assert.contains body "shell.css");
      Alcotest.(check bool)
        "no create.css" false
        (Html_assert.contains body "create.css");
      Alcotest.(check bool)
        "no tailwind" false
        (Html_assert.contains body "tailwind");
      Lwt.return_unit)

let top_mod_get_case =
  db_case "real top moderator without the admin flag keeps the /bring redirect"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find q_insert_user "ncl_topmod" in
      let* uid = or_fail "top mod" uid in
      let* cid =
        C.find q_insert_community ("ncl-modded", "NCL Modded", true, "public")
      in
      let* cid = or_fail "community" cid in
      let* r = C.exec q_add_top_mod (uid, cid) in
      let* () = or_fail "top mod row" r in
      let* r = C.exec q_add_member (uid, cid) in
      let* () = or_fail "membership" r in
      let* status, location, body =
        run_get_db ~url
          ~session:(plain_session uid "ncl_topmod")
          "/new-community"
      in
      Alcotest.(check bool) "redirects" true (is_redirect status);
      Alcotest.(check (option string)) "location" (Some "/bring") location;
      check_denied_clean "top moderator" body;
      Lwt.return_unit)

let post_forbidden_case =
  db_case "non-admin POST is rejected outright with no rows created"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = C.find q_insert_user "ncl_pleb" in
      let* uid = or_fail "user" uid in
      let* status, _, body =
        run_post ~url
          ~session:(plain_session uid "ncl_pleb")
          [
            ("name", "NCL Forged");
            ("slug", "ncl-forged");
            ("section_count", "0");
          ]
      in
      Alcotest.(check int) "403" 403 status;
      Alcotest.(check bool)
        "denial copy" true
        (Html_assert.contains body
           "Forbidden: community creation is restricted to Earde \
            administrators.");
      let* row = C.find_opt q_community_id "ncl-forged" in
      let* row = or_fail "lookup" row in
      Alcotest.(check bool) "no community row" true (row = None);
      Lwt.return_unit)

let post_validation_case =
  db_case "missing name keeps the Validation Error message page"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "ncl_admin2" in
      let* _status, _, body =
        run_post ~url
          ~session:(admin_session uid "ncl_admin2")
          [ ("name", ""); ("slug", "ncl-noname"); ("section_count", "0") ]
      in
      Alcotest.(check bool)
        "validation copy" true
        (Html_assert.contains body "Community name and URL slug are required.");
      Alcotest.(check bool)
        "shared message wrapper" true
        (Html_assert.contains body "launch-message-page");
      let* row = C.find_opt q_community_id "ncl-noname" in
      let* row = or_fail "lookup" row in
      Alcotest.(check bool) "no community row" true (row = None);
      Lwt.return_unit)

let post_missing_csrf_case =
  db_case "admin POST without the framework token keeps the Form Error answer"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "ncl_admin3" in
      let* status, _, body =
        run_post ~url ~with_csrf:false
          ~session:(admin_session uid "ncl_admin3")
          [
            ("name", "NCL Stale"); ("slug", "ncl-stale"); ("section_count", "0");
          ]
      in
      Alcotest.(check int) "400" 400 status;
      Alcotest.(check bool)
        "form error copy" true
        (Html_assert.contains body "Your form submission was invalid.");
      let* row = C.find_opt q_community_id "ncl-stale" in
      let* row = or_fail "lookup" row in
      Alcotest.(check bool) "no community row" true (row = None);
      Lwt.return_unit)

let post_success_case =
  db_case
    "successful creation keeps its database state, redirect and duplicate-slug \
     answer" (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = insert_admin (module C) "ncl_admin4" in
      let session = admin_session uid "ncl_admin4" in
      let* status, location, _ =
        run_post ~url ~session
          [
            ("name", "NCL Created");
            ("slug", "ncl-created");
            ("description", "A pass-19 fixture");
            ("section_count", "1");
            ("section_name_1", "Docs");
            ("section_sort_1", "top");
            ("channel_count", "1");
            ("channel_name_1", "lab");
          ]
      in
      Alcotest.(check bool) "redirects" true (is_redirect status);
      Alcotest.(check (option string))
        "canonical destination" (Some "/c/ncl-created") location;
      let* cid = C.find_opt q_community_id "ncl-created" in
      let* cid = or_fail "created" cid in
      let cid =
        match cid with
        | Some id -> id
        | None -> Alcotest.fail "community row missing"
      in
      let* sections = C.find q_count_sections cid in
      let* sections = or_fail "sections" sections in
      Alcotest.(check int) "General + Docs" 2 sections;
      let* channels = C.find q_count_channels cid in
      let* channels = or_fail "channels" channels in
      Alcotest.(check int) "general + lab" 2 channels;
      let* top_mod = C.find q_has_top_mod (uid, cid) in
      let* top_mod = or_fail "top mod" top_mod in
      Alcotest.(check bool) "creator is top_mod" true top_mod;
      let* joined = C.find q_has_member (uid, cid) in
      let* joined = or_fail "member" joined in
      Alcotest.(check bool) "creator joined" true joined;
      (* The pre-existing duplicate-slug answer is unchanged. *)
      let* dup_status, _, dup_body =
        run_post ~url ~session
          [
            ("name", "NCL Created Again");
            ("slug", "ncl-created");
            ("section_count", "0");
          ]
      in
      Alcotest.(check int) "duplicate 500" 500 dup_status;
      Alcotest.(check bool)
        "duplicate copy" true
        (Html_assert.contains dup_body
           "Could not create community. The URL slug may already be taken.");
      Lwt.return_unit)

let db_suite =
  [
    admin_get_case;
    top_mod_get_case;
    post_forbidden_case;
    post_validation_case;
    post_missing_csrf_case;
    post_success_case;
  ]

(* --- The review queue's degraded document --- *)

let degraded_render ?user ?(state = Home_review_fixture.phrp_state ()) () =
  Phrp.project_home_review_page ?user ~state ~feedback:None ()

let launch_shell : Phrp.launch_shell =
  {
    Phrp.community_record = Launch_fixture.nav_test_community;
    rail_communities = [ Launch_fixture.nav_test_community ];
    sidebar = Earde.Html.static "<div class='ncl-sidebar-mark'></div>";
  }

let launch_render ?user ?(state = Home_review_fixture.phrp_state ()) () =
  Phrp.project_home_review_page ?user ~shell:launch_shell ~state ~feedback:None
    ()

let degraded_wrapper_case =
  case
    "degraded document: neutral message wrapper, no chrome, no scripts, no \
     notification wiring" (fun () ->
      let page = degraded_render () in
      Html_assert.must page "<body class='launch-message-page'>";
      Html_assert.must page "<title>Project home requests - Earde</title>";
      Html_assert.must page "<meta name='robots' content='noindex'>";
      Html_assert.must page
        "<link rel='stylesheet' href='/static/css/earde.css'>";
      Alcotest.(check int)
        "exactly one stylesheet" 1
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "shell.css";
      Html_assert.must_not page "create.css";
      Html_assert.must_not page "unread-notifs";
      Html_assert.must_not page "confirmModal";
      Html_assert.must_not page "copyPostLink";
      Html_assert.must_not page "rail__item";
      Html_assert.must_not page "topbar";
      (* The real fragment survives inside the create-shell marker. *)
      let frag = Html_assert.panel_fragment page in
      Html_assert.must frag "Project home requests";
      Html_assert.must frag "Review projects requesting this community";
      Html_assert.must frag "Widget Kit")

let degraded_viewer_independence_case =
  case "degraded document ignores ?user (no viewer-derived chrome)" (fun () ->
      Alcotest.(check string)
        "anonymous = named viewer" (degraded_render ())
        (degraded_render ~user:"ncl_reviewer" ()))

(* The pending-request list — every phrv-* element and form — is byte
   identical between the degraded and the launch community documents for
   the same state. *)
let degraded_fragment_parity_case =
  case "request-list bytes match the launch community document" (fun () ->
      let slice html =
        let start = "<ul class='phrv-request-list'>" in
        match Html_assert.index_from html start 0 with
        | None -> Alcotest.fail "request list missing"
        | Some s -> (
            match Html_assert.index_from html "</ul>" s with
            | None -> Alcotest.fail "unterminated request list"
            | Some e -> String.sub html s (e - s))
      in
      Alcotest.(check string)
        "identical request list"
        (slice (launch_render ()))
        (slice (degraded_render ())))

let launch_branch_unchanged_case =
  case "normal branch still renders the launch community document" (fun () ->
      let page = launch_render ~user:"ncl_reviewer" () in
      Html_assert.must page "<body class='launch-project-home-review'>";
      (* The queue now renders inside the shared settings shell: the
         header band, one grouped settings index with exactly one active
         item (Project home requests), and the panel column. *)
      Html_assert.must page "cm-wrap cm-wrap--settings";
      Html_assert.must page "<nav class='cm-index'>";
      Html_assert.must page
        "cm-index-link cm-index-link--active' \
         href='/c/ocaml/project-home-requests'";
      Html_assert.must page "<div class='ncl-sidebar-mark'></div>";
      Html_assert.must_not page "launch-review-context";
      Html_assert.must_not page "launch-message-page")

let degraded_suite =
  [
    degraded_wrapper_case;
    degraded_viewer_independence_case;
    degraded_fragment_parity_case;
    launch_branch_unchanged_case;
  ]

let suites =
  (* Final create-page callers (pass 19): the /new-community renderer
       and admin-gate contracts on the launch app chrome, the unchanged
       POST /communities authorization / validation / CSRF / success
       contracts (database-gated), and the review queue's degraded
       document on the neutral launch message wrapper with its feature
       fragment byte-matched against the launch community document. *)
  [
    ("new_community_launch_renderer", renderer_suite);
    ("new_community_admin_gates", gate_suite);
    ("new_community_handlers_db", db_suite);
    ("project_home_review_degraded_page", degraded_suite);
  ]
