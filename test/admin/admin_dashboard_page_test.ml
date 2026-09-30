(* === Global admin dashboard on the launch shell (pass 18A) =================
   DB-free renderer pins for Pages.admin_dashboard_page: wrapper identity
   (body.launch-global-admin, local assets only, noindex), the preserved
   unban form contract (route, method, CSRF, confirm hook), escaping of the
   rendered usernames/emails, the replay-masking tables, and the
   single-notification-fetch contract. Authorization itself (200 admin / 403
   everyone else, no queries on denial) is pinned end-to-end by the gated
   login_session_replacement suite. *)

let case name f = Alcotest.test_case name `Quick f

let ( let* ) = Lwt.bind

let mk_recent ?(id = 1) ?(username = "alice") ?(email = "alice@example.com")
    ?(created_at = "2026-01-01 00:00:00") ?(is_admin = false)
    ?(is_banned = false) ?(post_count = 0) ?(comment_count = 0)
    ?(message_count = 0) () : Earde.Db.admin_recent_user =
  { id; username; email; created_at; is_admin; is_banned; post_count;
    comment_count; message_count }

let mk_pending ?(id = 1) ?(username = "penny")
    ?(email = "penny@example.com") ?(created_at = "2026-01-01 00:00:00")
    ?(expires_at = "2026-01-02 00:00:00") ?ip_address () :
    Earde.Db.pending_signup_row =
  { id; username; email; created_at; expires_at; ip_address }

let mk_banned ~id ~username ~email : Earde.Db.user = { id; username; email }

(* Renders the real page through real session middleware (the unban forms
   embed a CSRF tag); the session carries the admin identity the topbar
   user menu reads. The handler's own gate is not re-tested here. *)
let render ?(recent_users = []) ?(pending = []) ?(banned_users = [])
    ?(signups_enabled = true) ?(turnstile = `Configured)
    ?(brevo_configured = true) () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           let* () = Dream.set_session_field req "user_id" "1" in
           let* () = Dream.set_session_field req "username" "qa-admin" in
           let* () = Dream.set_session_field req "is_admin" "true" in
           rendered :=
             Earde.Pages.admin_dashboard_page ~user:"qa-admin"
               ~signups_enabled ~turnstile ~brevo_configured ~recent_users
               ~pending ~banned_users req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/admin" ""))
  in
  !rendered

let wrapper_case =
  case "wrapper: launch document, local assets only, noindex" (fun () ->
      let page = render () in
      Html_assert.must page "<body class='launch-global-admin'>";
      Html_assert.must page "<title>Admin Dashboard - Earde</title>";
      Html_assert.must page "<meta name='robots' content='noindex'>";
      Alcotest.(check int) "exactly one earde.css" 1
        (Html_assert.occurrences page "href='/static/css/earde.css'");
      Alcotest.(check int) "exactly one mobile-gate.css" 1
        (Html_assert.occurrences page "mobile-gate.css");
      Alcotest.(check int) "exactly two stylesheets" 2
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "admin.css";
      Html_assert.must_not page "shell.css";
      Html_assert.must_not page "href='#'";
      (* Serif head; KPI monitoring moved to PostHog — no dashboard link. *)
      Html_assert.must page "<h1 class='page__title'>Administration</h1>";
      Html_assert.must_not page "earde-hq-dashboard";
      (* No count fetch and no badge: the badge is server-rendered from
         the request's unread count, and this document is rendered without
         one. One shared behavior script (one confirmModal definition). *)
      Alcotest.(check int) "no notification fetch" 0
        (Html_assert.occurrences page "fetch('/api/unread-notifs')");
      Alcotest.(check int) "no notif badge" 0
        (Html_assert.occurrences page "id='notif-badge'");
      Alcotest.(check int) "one confirmModal definition" 1
        (Html_assert.occurrences page "function confirmModal");
      (* Admin session: the user menu links back to /admin exactly once. *)
      Alcotest.(check int) "one /admin menu link" 1
        (Html_assert.occurrences page "href='/admin'"))

let empty_states_case =
  case "empty dashboard keeps the three real empty states" (fun () ->
      let page = render () in
      Html_assert.must page "No users yet.";
      Html_assert.must page "No active pending signups.";
      Html_assert.must page "No users are currently globally banned.";
      (* All three data tables stay replay-masked. *)
      Alcotest.(check int) "three masked tables" 3
        (Html_assert.occurrences page "<table class='admin-table ph-no-capture'>"))

(* The one action form on the dashboard: POST /admin/unban/user/:id with a
   framework CSRF field and the existing confirm hook — byte contract. *)
let unban_form_case =
  case "unban forms keep route, method, CSRF and confirm hook" (fun () ->
      let page =
        render
          ~banned_users:
            [ mk_banned ~id:41 ~username:"marge" ~email:"marge@example.com"
            ; mk_banned ~id:42 ~username:"o'brien"
                ~email:"obrien@example.com"
            ] ()
      in
      Html_assert.must page
        "<form class='admin-act-form' action='/admin/unban/user/41' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/marge?')\">";
      (* The apostrophe is BACKSLASHED before it is entity-encoded. The
         previous expectation here was the bare "u/o&#39;brien", which is
         what the vulnerability looked like: the HTML parser decodes the
         entity before JavaScript parses the attribute, so an unescaped
         &#39; closes the string literal and everything after it in the
         username becomes executable. *)
      Html_assert.must page
        "<form class='admin-act-form' action='/admin/unban/user/42' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/o\\&#39;brien?')\">";
      Alcotest.(check int) "exactly two unban forms" 2
        (Html_assert.occurrences page "action='/admin/unban/user/");
      Alcotest.(check int) "one Unban button per form" 2
        (Html_assert.occurrences page
           "<button type='submit' class='admin-btn-unban'>Unban</button>");
      (* CSRF census: the unban forms are the only token-carrying forms on
         the page (search is GET; the menu logout form has never carried
         one). *)
      Alcotest.(check int) "exactly two CSRF fields" 2
        (List.length
           (List.filter Html_assert.is_csrf_input (Html_assert.input_tags page))))

(* Usernames and emails reach three different tables; all render through
   html_escape in both text and attribute contexts. *)
let escaping_case =
  case "hostile usernames and emails stay escaped everywhere" (fun () ->
      let u = "ban<script>me" and e = "evil&<x>\"@qa" in
      let page =
        render
          ~recent_users:[ mk_recent ~id:9 ~username:u ~email:e () ]
          ~pending:[ mk_pending ~id:5 ~username:u ~email:e () ]
          ~banned_users:[ mk_banned ~id:7 ~username:u ~email:e ] ()
      in
      Html_assert.must_not page "ban<script>me";
      Html_assert.must_not page "evil&<x>";
      Html_assert.must page "ban&lt;script&gt;me";
      Html_assert.must page "evil&amp;&lt;x&gt;&quot;@qa";
      (* Profile links build from the escaped username too. *)
      Html_assert.must page "href='/u/ban&lt;script&gt;me'";
      (* The recent-users table shows no email column — the address must
         appear exactly twice (pending + banned). *)
      Alcotest.(check int) "email in pending and banned only" 2
        (Html_assert.occurrences page "evil&amp;&lt;x&gt;&quot;@qa"))

(* Real config/status values render; no invented metrics appear. *)
let status_ledger_case =
  case "status ledger maps real config states onto the chips" (fun () ->
      let page =
        render ~signups_enabled:false ~turnstile:`Misconfigured
          ~brevo_configured:false ()
      in
      Html_assert.must page "admin-stat-val--off'>closed";
      Html_assert.must page "admin-stat-val--bad'>misconfigured";
      Html_assert.must page "admin-stat-val--warn'>not configured";
      let page2 = render () in
      Html_assert.must page2 "admin-stat-val--ok'>enabled";
      Html_assert.must page2 "admin-stat-val--ok'>required &amp; configured";
      Html_assert.must page2 "admin-stat-val--ok'>configured")

let suite =
  [ wrapper_case; empty_states_case; unban_form_case; escaping_case;
    status_ledger_case ]

let suites =
  [ ("admin_launch_page", suite)
  ]
