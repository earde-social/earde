(* DREAM_SECRET env var required in production (256-bit key via
   `Dream.to_base64url (Dream.random 32)`). Falls back to ephemeral
   random secret in dev — sessions won't survive restarts. *)
let () =
  (* Fail fast on missing DATABASE_URL: a hardcoded fallback would either ship
     real credentials in source or train operators to ignore the env contract. *)
  let db_url = match Sys.getenv_opt "DATABASE_URL" with
    | Some url -> url
    | None ->
      prerr_endline "FATAL: DATABASE_URL environment variable is required.";
      exit 1
  in
  (* Pool sized for launch-day surges; tunable per-deploy without recompile.
     Hard-fail on garbage input rather than silently falling back — a typo
     here would quietly cap us at 32 under load and be invisible in logs. *)
  let db_pool_size = match Sys.getenv_opt "DB_POOL_SIZE" with
    | None -> 32
    | Some s ->
      (match int_of_string_opt (String.trim s) with
       | Some n when n > 0 -> n
       | _ ->
         prerr_endline "FATAL: DB_POOL_SIZE must be a positive integer.";
         exit 1)
  in
  let secret_middleware = match Sys.getenv_opt "DREAM_SECRET" with
    | Some s -> Dream.set_secret s
    | None -> Fun.id
  in
  let interface = match Sys.getenv_opt "HOST" with
    | Some h -> h
    | None -> "localhost"
  in
  (* Dream alpha7 has no Dream.proxy. We trust X-Forwarded-For from Nginx so
     Dream.client returns the real user IP for the rate limiter. *)
  let proxy handler request =
    (match Dream.header request "X-Forwarded-For" with
     | Some v ->
       let ip = String.trim (List.nth (String.split_on_char ',' v) 0) in
       Dream.set_client request ip
     | None -> ());
    handler request
  in
  (* Private per-request slot holding the ORIGINAL (unredacted) target. Redaction
     stashes it here before overwriting the target, and restore_token_target_middleware
     puts it back for the route handlers. Without this, token=[REDACTED] reaches
     Dream.query and silently breaks every token GET route (/verify, /reset-password,
     /confirm-email). *)
  let original_target_field : string Dream.field = Dream.new_field ~name:"earde.raw_target" () in
  (* Mutates the request target before Dream.logger and analytics_middleware read it,
     replacing token=<value> with token=[REDACTED] so raw tokens never appear in access
     logs or page_views. Dream.set_target is internal; we reach it via dream-pure's
     Message module, which is the same mutable record Dream.target reads. *)
  let redact_token_middleware handler request =
    let target = Dream.target request in
    let needle = "token=" in
    let nlen = String.length needle in
    let tlen = String.length target in
    let buf = Buffer.create tlen in
    let i = ref 0 in
    while !i < tlen do
      if !i + nlen <= tlen && String.sub target !i nlen = needle then begin
        Buffer.add_string buf needle;
        Buffer.add_string buf "[REDACTED]";
        i := !i + nlen;
        while !i < tlen && target.[!i] <> '&' do incr i done
      end else begin
        Buffer.add_char buf target.[!i];
        incr i
      end
    done;
    let redacted = Buffer.contents buf in
    if redacted <> target then begin
      (* Keep the real target so restore_token_target_middleware can hand the
         unredacted token to the route handler after logging/analytics ran. *)
      Dream.set_field request original_target_field target;
      Dream_pure.Message.set_target request redacted
    end;
    handler request
  in
  (* Runs AFTER Dream.logger and analytics_middleware (both must see the redacted
     target) but BEFORE the router, so only the route handler gets the real token
     back via Dream.query. No-op for requests that had no token to redact. *)
  let restore_token_target_middleware handler request =
    (match Dream.field request original_target_field with
     | Some original -> Dream_pure.Message.set_target request original
     | None -> ());
    handler request
  in
  Dream.run ~interface ~port:8080
  @@ proxy
  @@ redact_token_middleware
  @@ Dream.logger
  @@ Dream.sql_pool ~size:db_pool_size db_url
  @@ secret_middleware
  (* sql_sessions trades ~1ms per-request DB round-trip for crash-safe session
     persistence. memory_sessions is zero-latency but loses all sessions on
     every systemd restart, forcing mass re-login. *)
  @@ Dream.sql_sessions
  @@ Earde.Handlers.analytics_middleware
  @@ restore_token_target_middleware
  @@ Dream.router [
    (* / now redirects to the new global Feed. home_handler is kept (still in
       handlers.mli) so / can become a real landing page later — hence a
       temporary redirect, not a 301. *)
    Dream.get "/" (fun request -> Dream.redirect request "/feed");
    (* Legacy /all is superseded by /feed. Redirect (not 404) to preserve old
       bookmarks. Temporary redirect to match the existing /-> /feed style above;
       the repo has no permanent-redirect (301/308) pattern. *)
    Dream.get "/all" (fun request -> Dream.redirect request "/feed");
    Dream.get "/feed" Earde.Handlers.feed_handler;
    Dream.get "/new-community" Earde.Handlers.new_community_page;
    Dream.post "/communities" Earde.Handlers.create_community_handler;
    Dream.post "/join" Earde.Handlers.join_community_handler;
    Dream.post "/leave" Earde.Handlers.leave_community_handler;
    Dream.get "/c/:slug" Earde.Handlers.community_page_handler;
    Dream.get "/c/:slug/s/:section_slug" Earde.Handlers.community_section_handler;
    Dream.get "/c/:slug/ch/:channel_slug" Earde.Handlers.community_channel_handler;
    Dream.get "/c/:slug/ch/:channel_slug/messages.json" Earde.Handlers.channel_messages_json_handler;
    Dream.get "/c/:slug/ch/:channel_slug/realtime-token" Earde.Handlers.realtime_token_handler;
    Dream.get "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread" Earde.Handlers.start_thread_form_handler;
    Dream.post "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread" Earde.Handlers.start_thread_create_handler;
    Dream.get "/c/:slug/t/:thread" Earde.Handlers.view_thread_handler;
    Dream.post "/messages" Earde.Handlers.send_message_handler;
    Dream.get "/c/:slug/settings" Earde.Handlers.community_settings_handler;
    Dream.get "/c/:slug/modlog" Earde.Handlers.modlog_handler;
    (* Reports: singular GET form + plural POST create (Slice B) + plural GET mod queue
       (read-only). Distinct literal segments from settings/modlog/manage-mods, so no router
       shadowing; the queue GET and create POST share the /reports path and split by method. *)
    Dream.get "/c/:slug/report" Earde.Handlers.report_form_handler;
    Dream.get "/c/:slug/reports" Earde.Handlers.reports_queue_handler;
    Dream.post "/c/:slug/reports" Earde.Handlers.create_report_handler;
    (* Resolution POSTs sit on a longer path (.../reports/:report_id/{dismiss,action}) than the
       create POST (.../reports), so they don't shadow it; M/TM/A gated in the handler. *)
    Dream.post "/c/:slug/reports/:report_id/dismiss" Earde.Handlers.dismiss_report_handler;
    Dream.post "/c/:slug/reports/:report_id/action" Earde.Handlers.action_report_handler;
    Dream.get "/c/:slug/manage-mods" Earde.Handlers.manage_mods_handler;
    Dream.post "/c/:slug/toggle_downvotes" Earde.Handlers.toggle_downvotes_handler;
    (* Slice E: TM/A-only visibility + discovery controls. Distinct literal sub-segments under
       /settings/, so no shadowing of the /c/:slug/settings GET. *)
    Dream.post "/c/:slug/settings/visibility" Earde.Handlers.update_community_visibility_handler;
    Dream.post "/c/:slug/settings/indexability" Earde.Handlers.update_community_indexability_handler;
    (* Slice F: TM/A-only member allow-list management (private communities). Distinct
       /settings/members/* sub-segments, so no shadowing of the settings GET or the Slice E POSTs. *)
    Dream.post "/c/:slug/settings/members/add" Earde.Handlers.add_member_handler;
    Dream.post "/c/:slug/settings/members/remove" Earde.Handlers.remove_member_handler;
    Dream.post "/c/:slug/manage-mods/add" Earde.Handlers.manage_mods_add_handler;
    Dream.post "/c/:slug/manage-mods/promote" Earde.Handlers.manage_mods_promote_handler;
    Dream.post "/c/:slug/manage-mods/remove" Earde.Handlers.manage_mods_remove_handler;
    Dream.post "/c/:slug/sections/add" Earde.Handlers.add_section_handler;
    Dream.post "/c/:slug/sections/:section_id/update" Earde.Handlers.update_section_handler;
    (* Slice H: TM/A-only section indexability toggle (gated in-handler). *)
    Dream.post "/c/:slug/sections/:section_id/indexability" Earde.Handlers.update_section_indexability_handler;
    Dream.post "/c/:slug/sections/:section_id/delete" Earde.Handlers.delete_section_handler;
    Dream.post "/c/:slug/channels/add" Earde.Handlers.add_channel_handler;
    Dream.post "/c/:slug/channels/:channel_id/update" Earde.Handlers.update_channel_handler;
    (* Slice H: TM/A-only channel indexability toggle (gated in-handler). *)
    Dream.post "/c/:slug/channels/:channel_id/indexability" Earde.Handlers.update_channel_indexability_handler;
    Dream.post "/c/:slug/channels/:channel_id/archive" Earde.Handlers.archive_channel_handler;
    Dream.post "/c/:slug/channels/:channel_id/unarchive" Earde.Handlers.unarchive_channel_handler;
    Dream.post "/update-community" Earde.Handlers.update_community_handler;
    Dream.post "/add-mod" Earde.Handlers.add_mod_handler;
    Dream.post "/remove-mod" Earde.Handlers.remove_mod_handler;
    Dream.post "/ban-community-user" Earde.Handlers.ban_community_user_handler;
    Dream.post "/unban-community-user" Earde.Handlers.unban_community_user_handler;
    Dream.get "/new-post" Earde.Handlers.new_post_page;
    Dream.post "/posts" Earde.Handlers.create_post_handler;
    Dream.get "/p/:id" Earde.Handlers.view_post_handler;
    Dream.post "/comments" Earde.Handlers.create_comment_handler;
    Dream.post "/vote" Earde.Handlers.vote_handler;
    Dream.post "/vote-comment" Earde.Handlers.vote_comment_handler;
    Dream.get "/u/:username" Earde.Handlers.view_profile_handler;
    Dream.get "/search" Earde.Handlers.search_handler;
    Dream.get "/settings" Earde.Handlers.settings_page_handler;
    Dream.post "/settings" Earde.Handlers.update_profile_handler;
    Dream.get "/notifications" Earde.Handlers.notifications_handler;
    Dream.get "/api/unread-notifs" Earde.Handlers.unread_notifs_api;
    Dream.post "/delete-account" Earde.Handlers.delete_account_handler;
    Dream.post "/delete-post" Earde.Handlers.delete_post_handler;
    Dream.post "/c/:slug/posts/:id/mod_delete" Earde.Handlers.mod_delete_post_handler;
    Dream.post "/delete-comment" Earde.Handlers.delete_comment_handler;
    Dream.post "/c/:slug/comments/:id/mod_delete" Earde.Handlers.mod_delete_comment_handler;
    Dream.get  "/admin" Earde.Handlers.admin_dashboard_handler;
    Dream.post "/admin/ban/user/:id" Earde.Handlers.ban_user_handler;
    Dream.post "/admin/unban/user/:id" Earde.Handlers.unban_user_global_handler;
    Dream.get "/privacy" Earde.Handlers.privacy_page_handler;
    Dream.get "/signup" Earde.Handlers.signup_page;
    Dream.post "/signup" (Earde.Handlers.Rate_limit.middleware Earde.Handlers.signup_handler);
    Dream.get "/verify" Earde.Handlers.verify_email_handler;
    Dream.get "/confirm-email" Earde.Handlers.confirm_email_handler;
    Dream.get "/login" Earde.Handlers.login_page;
    Dream.post "/login" (Earde.Handlers.Rate_limit.middleware Earde.Handlers.login_handler);
    Dream.post "/logout" Earde.Handlers.logout_handler;
    Dream.post "/settings/password" Earde.Handlers.change_password_handler;
    Dream.get "/forgot-password" Earde.Handlers.forgot_password_page;
    Dream.post "/forgot-password" (Earde.Handlers.Rate_limit.middleware Earde.Handlers.forgot_password_handler);
    Dream.get "/reset-password" Earde.Handlers.reset_password_page_handler;
    Dream.post "/reset-password" Earde.Handlers.reset_password_handler;
    Dream.get "/export-data" Earde.Handlers.export_data_handler;
    Dream.get "/earde-hq-dashboard" Earde.Handlers.hq_dashboard_handler;
    Dream.get "/_debug/state" Earde.Handlers.debug_state_handler;
    Dream.get "/static/**" (Dream.static "static");
  ]
