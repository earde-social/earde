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
  Dream.run ~interface ~port:8080
  @@ proxy
  (* Replaces sensitive query parameter values (token=, state=, code=) with
     [REDACTED] before Dream.logger and analytics_middleware read the target,
     so those secrets never appear in access logs or page_views. The original
     target is stashed in a field private to Request_target_redaction and put
     back by its restore_middleware below, just before the router — without
     that, [REDACTED] would reach Dream.query and silently break every
     sensitive-parameter GET route (/verify, /reset-password, /confirm-email). *)
  @@ Earde.Request_target_redaction.redact_middleware
  @@ Dream.logger
  @@ Dream.sql_pool ~size:db_pool_size db_url
  @@ secret_middleware
  (* sql_sessions trades ~1ms per-request DB round-trip for crash-safe session
     persistence. memory_sessions is zero-latency but loses all sessions on
     every systemd restart, forcing mass re-login. *)
  @@ Dream.sql_sessions
  (* Inside sql_pool + sql_sessions: needs Dream.sql and the session's user_id.
     Separate from analytics_middleware so removing page-view analytics later
     cannot take last_active_at (moderator auto-demotion input) down with it. *)
  @@ Earde.Handlers.presence_middleware
  @@ Earde.Handlers.analytics_middleware
  (* Runs AFTER Dream.logger and analytics_middleware (both must see the
     redacted target) but BEFORE the router, so only the route handler gets
     the real sensitive query parameters back via Dream.query. No-op for
     requests that had nothing to redact. *)
  @@ Earde.Request_target_redaction.restore_middleware
  @@ Dream.router [
    (* / redirects to the global Feed. Temporary (302), not a 301, so / can
       become a real landing page later without a cached permanent redirect
       standing in the way. *)
    Dream.get "/" (fun request -> Dream.redirect request "/feed");
    (* Legacy /all is superseded by /feed. Redirect (not 404) to preserve old
       bookmarks. Temporary redirect to match the existing /-> /feed style above;
       the repo has no permanent-redirect (301/308) pattern. *)
    Dream.get "/all" (fun request -> Dream.redirect request "/feed");
    Dream.get "/feed" Earde.Handlers.feed_handler;
    (* Entry and return page for GitHub onboarding: offers the start action
       when the viewer passes the onboarding policy and shows the one-time
       connected/failed callback feedback. Informational GET, deliberately
       not rate-limited; the mode is re-read per request (uncached, matching
       the other onboarding routes below). *)
    Dream.get "/bring" (fun request ->
        Earde.Github_onboarding_handlers.make_bring_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    (* Starts GitHub App installation: rate-limited like the other sensitive
       POSTs. The mode is re-read per request via the existing
       Project_onboarding API (uncached, matching /bring) and the validated
       GitHub App configuration is loaded per request from the environment.
       No GET variant and no callback routes in this slice. *)
    Dream.post "/integrations/github/install/start"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Github_onboarding_handlers.make_start_installation_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* GitHub App setup return: GET only, and deliberately NOT wrapped in
       Rate_limit.middleware — malformed or cookieless requests are rejected
       before any database access, a valid encrypted per-flow cookie is
       required before the single attach UPDATE, and the limiter's blocked
       page is rendered HTML, while this state-bearing callback URL must only
       ever answer with a clean redirect away. Request-target redaction keeps
       the state out of Dream logging and analytics. *)
    Dream.get "/integrations/github/install/return"
      (fun request ->
        Earde.Github_onboarding_handlers.make_setup_return_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          ~load_config:Earde.Github_app_config.from_env
          request);
    (* Final OAuth authorization callback: GET only and, like the setup
       return, deliberately NOT wrapped in Rate_limit.middleware — the
       limiter's blocked page is rendered HTML, while this code/state-bearing
       callback URL must only ever answer a clean redirect away, and
       request-target redaction already keeps code and state out of Dream
       logging and analytics. Secret credentials and the three GitHub
       transports are injected here so the handler stays testable offline. *)
    Dream.get "/integrations/github/authorize/callback"
      (fun request ->
        Earde.Github_onboarding_handlers.make_oauth_callback_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          ~load_config:Earde.Github_app_config.from_env
          ~load_credentials:Earde.Github_oauth_credentials.from_env
          ~exchange_transport:
            (module Earde.Github_oauth_token_exchange.Cohttp_transport)
          ~installations_transport:
            (module Earde.Github_user_installations.Cohttp_transport)
          ~repositories_transport:
            (module Earde.Github_user_installation_repositories
                    .Cohttp_transport)
          request);
    (* Project setup over verified GitHub drafts. The GET is informational
       and deliberately not rate-limited (matching /bring); the
       selection-replacing POST reuses the same sensitive-POST rate limit as
       the other authenticated mutations above. Mode is re-read per request
       (uncached, matching the onboarding routes), and the POST loads the
       validated GitHub App configuration per request only to enforce the
       same public-origin policy as the installation start — no GitHub
       credential is used and no outbound HTTP occurs. *)
    Dream.get "/projects/new" (fun request ->
        Earde.Project_setup_handlers.make_new_project_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    Dream.post "/projects/new/repositories"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_setup_handlers.make_repository_selection_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* Permanent project creation and its PRG destination. The POST shares
       the sensitive-POST rate limit and per-request configuration load of
       the selection POST above (origin policy only — no GitHub credential,
       no outbound HTTP); the owner-only GET is informational and
       deliberately not rate-limited, matching the other project-setup
       GETs. *)
    Dream.post "/projects"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_creation_handlers.make_project_creation_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    Dream.get "/projects/:slug/setup" (fun request ->
        Earde.Project_creation_handlers.make_project_home_setup_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    (* Existing-community home request for a verified project. The
       steward-only GET is informational and deliberately not rate-limited,
       matching the other project-setup GETs; the request-creating POST
       shares the sensitive-POST rate limit and per-request configuration
       load of the project POSTs above (origin policy only — no GitHub
       credential, no outbound HTTP). *)
    Dream.get "/projects/:slug/request-home" (fun request ->
        Earde.Project_home_request_handlers.make_project_home_choice_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    Dream.post "/projects/:slug/request-home"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_request_handlers
           .make_project_home_request_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* Dedicated-community-home creation for a verified project. The
       steward-only GET is informational and deliberately not rate-limited,
       matching the other project-setup GETs; the provisioning POST is the
       exact action that page's single form emits and shares the
       sensitive-POST rate limit and per-request configuration load of the
       project POSTs above (origin policy only — no GitHub credential, no
       outbound HTTP). There is no alias and no second GET: a committed
       provision returns the browser to the new community's existing
       settings route. *)
    Dream.get "/projects/:slug/community-home/new" (fun request ->
        Earde.Project_home_provisioning_handlers
        .make_project_home_provisioning_page_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    Dream.post "/projects/:slug/community-home"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_provisioning_handlers
           .make_project_home_provisioning_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* Moderator review of pending project-home requests for a community.
       The queue GET is informational and deliberately not rate-limited,
       matching the other private settings GETs; the accept/reject POSTs
       share the sensitive-POST rate limit and per-request configuration
       load of the project POSTs above (origin policy only — no GitHub
       credential, no outbound HTTP). Mode is re-read per request (uncached,
       matching the onboarding routes). Authorization is decided in the read
       model and review store SQL, not here. *)
    Dream.get "/c/:slug/project-home-requests" (fun request ->
        Earde.Project_home_review_handlers
        .make_project_home_review_queue_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    Dream.post "/c/:slug/projects/:project_slug/accept"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_review_handlers.make_project_home_accept_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    Dream.post "/c/:slug/projects/:project_slug/reject"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_review_handlers.make_project_home_reject_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* Removal of an accepted project home, from either authorized surface.
       Both POSTs call the same transactional removal store with the same
       two slugs and share the sensitive-POST rate limit and per-request
       configuration load of the project POSTs above (origin policy only —
       no GitHub credential, no outbound HTTP); they differ only in where a
       completed or already-completed removal returns the browser. There is
       no GET counterpart and no alias: each surface's own existing route
       is the confirmation and the destination. Authorization is decided in
       the removal store's SQL, not here and not by the route shape. *)
    Dream.post "/projects/:project_slug/community-home/:community_slug/remove"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_removal_handlers
           .make_project_side_home_removal_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    Dream.post "/c/:community_slug/projects/:project_slug/remove-home"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Project_home_removal_handlers
           .make_community_side_home_removal_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
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
    (* Final setup and publication surface of a provisioned network
       community. Both are distinct literal segments from
       settings/modlog/reports, so no router shadowing. The GET is
       informational and deliberately not rate-limited, matching the other
       setup GETs; the publication POST is exactly the action that page's
       single form emits and shares the sensitive-POST rate limit and
       per-request configuration load of the project POSTs above (origin
       policy only — no GitHub credential, no outbound HTTP). Mode is re-read
       per request (uncached, matching the onboarding routes) and
       authorization is decided in the read model's and the publication
       store's SQL, not here and not by the route shape. There is no alias
       and no second GET: a committed publication returns the browser to the
       community's own now-public home. *)
    Dream.get "/c/:slug/setup" (fun request ->
        Earde.Network_community_publication_handlers
        .make_network_community_publication_page_handler
          ~mode:(Earde.Project_onboarding.mode_from_env ())
          request);
    Dream.post "/c/:slug/publish"
      (Earde.Handlers.Rate_limit.middleware (fun request ->
           Earde.Network_community_publication_handlers
           .make_network_community_publication_handler
             ~mode:(Earde.Project_onboarding.mode_from_env ())
             ~load_config:Earde.Github_app_config.from_env
             request));
    (* Community-connections management: the authorized surface where a
       community's top moderators (or a durable global admin) see their
       connected communities, review incoming requests, watch outgoing ones,
       and send new ones. Every route is community-scoped under
       /c/:slug/settings/connections — distinct longer paths than the
       /c/:slug/settings GET and the Slice E/F settings POSTs, so nothing
       shadows anything. The two GETs are informational and deliberately not
       rate-limited, matching the other private settings GETs; the four
       mutations share the sensitive-POST rate limit. Authorization is
       decided in the read model's SQL and the subject binding inside the
       store's guarded mutations — never by the route shape, and never by a
       form field. *)
    Dream.get "/c/:slug/settings/connections"
      Earde.Community_connections_handlers.make_connections_page_handler;
    Dream.get "/c/:slug/settings/connections/new"
      Earde.Community_connections_handlers.make_connections_search_handler;
    Dream.post "/c/:slug/settings/connections/request"
      (Earde.Handlers.Rate_limit.middleware
         Earde.Community_connections_handlers.make_connection_request_handler);
    Dream.post "/c/:slug/settings/connections/:id/accept"
      (Earde.Handlers.Rate_limit.middleware
         Earde.Community_connections_handlers.make_connection_accept_handler);
    Dream.post "/c/:slug/settings/connections/:id/reject"
      (Earde.Handlers.Rate_limit.middleware
         Earde.Community_connections_handlers.make_connection_reject_handler);
    Dream.post "/c/:slug/settings/connections/:id/remove"
      (Earde.Handlers.Rate_limit.middleware
         Earde.Community_connections_handlers.make_connection_removal_handler);
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
    (* §9 consent endpoint: JSON-only + Origin/Sec-Fetch-Site protection
       instead of the form-CSRF used by other state-changing routes; the POST
       route matches first, every other method falls to the controlled 405.
       Inside sql_pool (person-property lookup) and sql_sessions (reads the
       authenticated user on granted). *)
    Dream.post "/analytics/consent" Earde.Handlers.analytics_consent_handler;
    Dream.any "/analytics/consent" Earde.Handlers.analytics_consent_method_not_allowed;
    Dream.get "/export-data" Earde.Handlers.export_data_handler;
    (* There is deliberately no KPI-dashboard route: PostHog is the
       authoritative analytics product, and /earde-hq-dashboard was removed
       without a replacement page and without a redirect. *)
    Dream.get "/_debug/state" Earde.Handlers.debug_state_handler;
    Dream.get "/static/**" (Dream.static "static");
  ]
