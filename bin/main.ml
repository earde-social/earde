(* DREAM_SECRET env var required in production (256-bit key via
   `Dream.to_base64url (Dream.random 32)`). Falls back to ephemeral
   random secret in dev — sessions won't survive restarts. *)
let () =
  let db_url = match Sys.getenv_opt "DATABASE_URL" with
    | Some url -> url
    | None -> "postgresql://tolwiz:tolwiz@localhost:5432/earde"
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
  Dream.run ~interface ~port:8080
  @@ proxy
  @@ Dream.logger
  @@ Dream.sql_pool db_url
  @@ secret_middleware
  (* sql_sessions trades ~1ms per-request DB round-trip for crash-safe session
     persistence. memory_sessions is zero-latency but loses all sessions on
     every systemd restart, forcing mass re-login. *)
  @@ Dream.sql_sessions
  @@ Earde.Handlers.analytics_middleware
  @@ Dream.router [
    Dream.get "/" Earde.Handlers.home_handler;
    Dream.get "/all" Earde.Handlers.global_feed_handler;
    Dream.get "/new-community" Earde.Handlers.new_community_page;
    Dream.post "/communities" Earde.Handlers.create_community_handler;
    Dream.post "/join" Earde.Handlers.join_community_handler;
    Dream.post "/leave" Earde.Handlers.leave_community_handler;
    Dream.get "/c/:slug" Earde.Handlers.community_page_handler;
    Dream.get "/c/:slug/settings" Earde.Handlers.community_settings_handler;
    Dream.get "/c/:slug/modlog" Earde.Handlers.modlog_handler;
    Dream.get "/c/:slug/manage-mods" Earde.Handlers.manage_mods_handler;
    Dream.post "/c/:slug/toggle_downvotes" Earde.Handlers.toggle_downvotes_handler;
    Dream.post "/c/:slug/manage-mods/add" Earde.Handlers.manage_mods_add_handler;
    Dream.post "/c/:slug/manage-mods/promote" Earde.Handlers.manage_mods_promote_handler;
    Dream.post "/c/:slug/manage-mods/remove" Earde.Handlers.manage_mods_remove_handler;
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
    Dream.get "/about" Earde.Handlers.about_page_handler;
    Dream.get "/signup" Earde.Handlers.signup_page;
    Dream.post "/signup" (Earde.Handlers.Rate_limit.middleware Earde.Handlers.signup_handler);
    Dream.get "/verify" Earde.Handlers.verify_email_handler;
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
  ]
