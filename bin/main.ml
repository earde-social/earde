(* DREAM_SECRET env var required in production (256-bit key via
   `Dream.to_base64url (Dream.random 32)`). Falls back to ephemeral
   random secret in dev — sessions won't survive restarts. *)
let () =
  (* Fail fast on missing DATABASE_URL: a hardcoded fallback would either ship
     real credentials in source or train operators to ignore the env contract. *)
  let db_url =
    match Sys.getenv_opt "DATABASE_URL" with
    | Some url -> url
    | None ->
        prerr_endline "FATAL: DATABASE_URL environment variable is required.";
        exit 1
  in
  (* Pool sized for launch-day surges; tunable per-deploy without recompile.
     Hard-fail on garbage input rather than silently falling back — a typo
     here would quietly cap us at 32 under load and be invisible in logs. *)
  let db_pool_size =
    match Sys.getenv_opt "DB_POOL_SIZE" with
    | None -> 32
    | Some s -> (
        match int_of_string_opt (String.trim s) with
        | Some n when n > 0 -> n
        | _ ->
            prerr_endline "FATAL: DB_POOL_SIZE must be a positive integer.";
            exit 1)
  in
  let secret_middleware =
    match Sys.getenv_opt "DREAM_SECRET" with
    | Some s -> Dream.set_secret s
    | None -> Fun.id
  in
  let interface =
    match Sys.getenv_opt "HOST" with Some h -> h | None -> "localhost"
  in
  (* Dream alpha7 has no Dream.proxy, so the forwarded-header trust boundary
     is ours to draw. It is drawn once, here, and every consumer of
     Dream.client (the rate limiter, the stored signup IP, the admin view)
     inherits it.

     This used to take the LEFTMOST X-Forwarded-For value unconditionally.
     That value is the one segment of the header a client fully controls, so
     rotating it minted an unlimited supply of rate-limit buckets; and with
     no header at all Dream.client kept its "address:port" form, whose
     ephemeral port minted a fresh bucket per connection. Client_address
     fixes both: forwarded headers count only when the immediate peer is a
     configured trusted proxy, only the rightmost entry (the address that
     proxy itself observed) is believed, and the result is always a bare
     normalized address.

     Resolved once per process — the trusted set is deployment topology, not
     per-request state. *)
  let trusted_proxies = Earde.Client_address.trusted_proxies_from_env () in
  let proxy handler request =
    Dream.set_client request
      (Earde.Client_address.client_ip ~trusted_proxies
         ~peer:(Dream.client request)
         ~forwarded_for:(Dream.header request "X-Forwarded-For"));
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
  (* OUTSIDE sql_sessions so it sees the session's own Set-Cookie. Dream
     infers Secure from its TLS listener flag, which is false behind a
     TLS-terminating nginx, and neither sql_sessions nor any exported setter
     can override it — so the attribute is added to the outgoing header
     instead, decided by EARDE_PUBLIC_ORIGIN (server-side configuration, not
     a forwarded header). No-op on an http origin, so local development is
     unchanged. See session_cookie_policy.mli. *)
  @@ Earde.Session_cookie_policy.middleware
  (* sql_sessions trades ~1ms per-request DB round-trip for crash-safe session
     persistence. memory_sessions is zero-latency but loses all sessions on
     every systemd restart, forcing mass re-login. *)
  @@ Dream.sql_sessions
  (* Inside sql_pool + sql_sessions: needs Dream.sql and the session's user_id.
     Separate from analytics_middleware so removing page-view analytics later
     cannot take last_active_at (moderator auto-demotion input) down with it. *)
  @@ Earde.Activity_middleware.presence_middleware
  @@ Earde.Activity_middleware.analytics_middleware
  (* Inside sql_pool + sql_sessions, like presence: resolves the signed-in
     user's unread-notification count once and stashes it on the request, so
     every authenticated document renders its top-bar badge from the same
     durable query instead of each page fetching its own answer. Best-effort
     — a failed count leaves the field unset and the badge simply absent. *)
  @@ Earde.Notification_badge.middleware
  (* Runs AFTER Dream.logger and analytics_middleware (both must see the
     redacted target) but BEFORE the router, so only the route handler gets
     the real sensitive query parameters back via Dream.query. No-op for
     requests that had nothing to redact. *)
  @@ Earde.Request_target_redaction.restore_middleware
  @@ Dream.router
       [
         (* / redirects to the global Feed. Temporary (302), not a 301, so / can
       become a real landing page later without a cached permanent redirect
       standing in the way. *)
         Dream.get "/" (fun request -> Dream.redirect request "/feed");
         (* Legacy /all is superseded by /feed. Redirect (not 404) to preserve old
       bookmarks. Temporary redirect to match the existing /-> /feed style above;
       the repo has no permanent-redirect (301/308) pattern. *)
         Dream.get "/all" (fun request -> Dream.redirect request "/feed");
         Dream.get "/feed" Earde.Public_handlers.feed_handler;
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
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Github_onboarding_handlers.make_start_installation_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         (* GitHub App setup return: GET only, and deliberately NOT wrapped in
       Rate_limit.middleware — malformed or cookieless requests are rejected
       before any database access, a valid encrypted per-flow cookie is
       required before the single attach UPDATE, and the limiter's blocked
       page is rendered HTML, while this state-bearing callback URL must only
       ever answer with a clean redirect away. Request-target redaction keeps
       the state out of Dream logging and analytics. *)
         Dream.get "/integrations/github/install/return" (fun request ->
             Earde.Github_onboarding_handlers.make_setup_return_handler
               ~mode:(Earde.Project_onboarding.mode_from_env ())
               ~load_config:Earde.Github_app_config.from_env request);
         (* Final OAuth authorization callback: GET only and, like the setup
       return, deliberately NOT wrapped in Rate_limit.middleware — the
       limiter's blocked page is rendered HTML, while this code/state-bearing
       callback URL must only ever answer a clean redirect away, and
       request-target redaction already keeps code and state out of Dream
       logging and analytics. Secret credentials and the three GitHub
       transports are injected here so the handler stays testable offline. *)
         Dream.get "/integrations/github/authorize/callback" (fun request ->
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
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_setup_handlers.make_repository_selection_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         (* Permanent project creation and its PRG destination. The POST shares
       the sensitive-POST rate limit and per-request configuration load of
       the selection POST above (origin policy only — no GitHub credential,
       no outbound HTTP); the owner-only GET is informational and
       deliberately not rate-limited, matching the other project-setup
       GETs. *)
         Dream.post "/projects"
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_creation_handlers.make_project_creation_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
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
             Earde.Project_home_request_handlers
             .make_project_home_choice_handler
               ~mode:(Earde.Project_onboarding.mode_from_env ())
               request);
         Dream.post "/projects/:slug/request-home"
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_request_handlers
                .make_project_home_request_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
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
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_provisioning_handlers
                .make_project_home_provisioning_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
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
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_review_handlers
                .make_project_home_accept_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         Dream.post "/c/:slug/projects/:project_slug/reject"
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_review_handlers
                .make_project_home_reject_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         (* Removal of an accepted project home, from either authorized surface.
       Both POSTs call the same transactional removal store with the same
       two slugs and share the sensitive-POST rate limit and per-request
       configuration load of the project POSTs above (origin policy only —
       no GitHub credential, no outbound HTTP); they differ only in where a
       completed or already-completed removal returns the browser. There is
       no GET counterpart and no alias: each surface's own existing route
       is the confirmation and the destination. Authorization is decided in
       the removal store's SQL, not here and not by the route shape. *)
         Dream.post
           "/projects/:project_slug/community-home/:community_slug/remove"
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_removal_handlers
                .make_project_side_home_removal_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         Dream.post "/c/:community_slug/projects/:project_slug/remove-home"
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Project_home_removal_handlers
                .make_community_side_home_removal_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         Dream.get "/new-community" Earde.Community_handlers.new_community_page;
         Dream.post "/communities"
           Earde.Community_handlers.create_community_handler;
         Dream.post "/join" Earde.Membership_handlers.join_community_handler;
         Dream.post "/leave" Earde.Membership_handlers.leave_community_handler;
         Dream.get "/c/:slug" Earde.Community_handlers.community_page_handler;
         Dream.get "/c/:slug/s/:section_slug"
           Earde.Community_handlers.community_section_handler;
         Dream.get "/c/:slug/ch/:channel_slug"
           Earde.Chat_handlers.community_channel_handler;
         Dream.get "/c/:slug/ch/:channel_slug/messages.json"
           Earde.Chat_handlers.channel_messages_json_handler;
         Dream.get "/c/:slug/ch/:channel_slug/realtime-token"
           Earde.Chat_handlers.realtime_token_handler;
         Dream.get "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
           Earde.Start_thread_handlers.start_thread_form_handler;
         Dream.post
           "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
           Earde.Start_thread_handlers.start_thread_create_handler;
         Dream.get "/c/:slug/t/:thread" Earde.Post_handlers.view_thread_handler;
         Dream.post "/messages" Earde.Chat_handlers.send_message_handler;
         Dream.get "/c/:slug/settings"
           Earde.Community_settings_handlers.community_settings_handler;
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
           (Earde.Rate_limit_middleware.middleware (fun request ->
                Earde.Network_community_publication_handlers
                .make_network_community_publication_handler
                  ~mode:(Earde.Project_onboarding.mode_from_env ())
                  ~load_config:Earde.Github_app_config.from_env request));
         (* Community-connections management: the authorized surface where a
       community's top moderators (or a durable global admin) see their
       connected communities, review incoming requests, watch outgoing ones,
       and send new ones. Every route is community-scoped under
       /c/:slug/settings/connections — distinct longer paths than the
       /c/:slug/settings GET and the visibility/member settings POSTs, so nothing
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
           (Earde.Rate_limit_middleware.middleware
              Earde.Community_connections_handlers
              .make_connection_request_handler);
         Dream.post "/c/:slug/settings/connections/:id/accept"
           (Earde.Rate_limit_middleware.middleware
              Earde.Community_connections_handlers
              .make_connection_accept_handler);
         Dream.post "/c/:slug/settings/connections/:id/reject"
           (Earde.Rate_limit_middleware.middleware
              Earde.Community_connections_handlers
              .make_connection_reject_handler);
         Dream.post "/c/:slug/settings/connections/:id/remove"
           (Earde.Rate_limit_middleware.middleware
              Earde.Community_connections_handlers
              .make_connection_removal_handler);
         (* Shared-threads workflow: the per-thread Share page (longer literal
       path than the /c/:slug/t/:thread GET, so no shadowing) and the
       community-scoped management surface under
       /c/:slug/settings/shared-threads — a distinct literal segment from
       the /c/:slug/settings GET, the visibility/member settings POSTs, and the
       /settings/connections family, so nothing shadows anything. The two
       GETs are informational and deliberately not rate-limited, matching
       the other private settings GETs; the five mutations share the
       sensitive-POST rate limit. Authorization is decided in the read
       models' SQL and the subject binding inside the Slice 1 store's
       guarded mutations — never by the route shape, and never by a form
       field. *)
         Dream.get "/c/:slug/t/:thread/share"
           Earde.Shared_thread_placement_handlers.make_share_page_handler;
         Dream.post "/c/:slug/t/:thread/share"
           (Earde.Rate_limit_middleware.middleware
              Earde.Shared_thread_placement_handlers.make_share_request_handler);
         Dream.get "/c/:slug/settings/shared-threads"
           Earde.Shared_thread_placement_handlers.make_management_page_handler;
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/accept"
           (Earde.Rate_limit_middleware.middleware
              Earde.Shared_thread_placement_handlers.make_accept_handler);
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/reject"
           (Earde.Rate_limit_middleware.middleware
              Earde.Shared_thread_placement_handlers.make_reject_handler);
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/withdraw"
           (Earde.Rate_limit_middleware.middleware
              Earde.Shared_thread_placement_handlers.make_withdrawal_handler);
         Dream.post "/c/:slug/settings/shared-threads/:placement_id/remove"
           (Earde.Rate_limit_middleware.middleware
              Earde.Shared_thread_placement_handlers.make_removal_handler);
         (* Public Network page: the community's connected projects and connected
       communities in full, which the community home now links to instead of
       listing. A distinct literal segment from settings/modlog/reports and
       from the /settings/connections management paths, so nothing shadows
       anything. Read-only and deliberately public — its access decision is
       the community's own can_view_community, exactly like /c/:slug. *)
         Dream.get "/c/:slug/network"
           Earde.Community_handlers.community_network_handler;
         Dream.get "/c/:slug/modlog" Earde.Moderation_handlers.modlog_handler;
         (* Reports: singular GET form + plural POST create + plural GET mod queue
       (read-only). Distinct literal segments from settings/modlog/manage-mods, so no router
       shadowing; the queue GET and create POST share the /reports path and split by method. *)
         Dream.get "/c/:slug/report"
           Earde.Moderation_handlers.report_form_handler;
         Dream.get "/c/:slug/reports"
           Earde.Moderation_handlers.reports_queue_handler;
         Dream.post "/c/:slug/reports"
           Earde.Moderation_handlers.create_report_handler;
         (* Resolution POSTs sit on a longer path (.../reports/:report_id/{dismiss,action}) than the
       create POST (.../reports), so they don't shadow it; M/TM/A gated in the handler. *)
         Dream.post "/c/:slug/reports/:report_id/dismiss"
           Earde.Moderation_handlers.dismiss_report_handler;
         Dream.post "/c/:slug/reports/:report_id/action"
           Earde.Moderation_handlers.action_report_handler;
         Dream.get "/c/:slug/manage-mods"
           Earde.Moderation_handlers.manage_mods_handler;
         Dream.post "/c/:slug/toggle_downvotes"
           Earde.Community_settings_handlers.toggle_downvotes_handler;
         (* TM/A-only visibility + discovery controls. Distinct literal sub-segments under
       /settings/, so no shadowing of the /c/:slug/settings GET. *)
         Dream.post "/c/:slug/settings/visibility"
           Earde.Community_settings_handlers.update_community_visibility_handler;
         Dream.post "/c/:slug/settings/indexability"
           Earde.Community_settings_handlers
           .update_community_indexability_handler;
         (* TM/A-only member allow-list management (private communities). Distinct
       /settings/members/* sub-segments, so no shadowing of the settings GET or the visibility POSTs. *)
         Dream.post "/c/:slug/settings/members/add"
           Earde.Membership_handlers.add_member_handler;
         Dream.post "/c/:slug/settings/members/remove"
           Earde.Membership_handlers.remove_member_handler;
         Dream.post "/c/:slug/manage-mods/add"
           Earde.Moderation_handlers.manage_mods_add_handler;
         Dream.post "/c/:slug/manage-mods/promote"
           Earde.Moderation_handlers.manage_mods_promote_handler;
         Dream.post "/c/:slug/manage-mods/remove"
           Earde.Moderation_handlers.manage_mods_remove_handler;
         Dream.post "/c/:slug/sections/add"
           Earde.Community_structure_handlers.add_section_handler;
         Dream.post "/c/:slug/sections/:section_id/update"
           Earde.Community_structure_handlers.update_section_handler;
         (* TM/A-only section indexability toggle (gated in-handler). *)
         Dream.post "/c/:slug/sections/:section_id/indexability"
           Earde.Community_settings_handlers.update_section_indexability_handler;
         Dream.post "/c/:slug/sections/:section_id/delete"
           Earde.Community_structure_handlers.delete_section_handler;
         Dream.post "/c/:slug/channels/add"
           Earde.Community_structure_handlers.add_channel_handler;
         Dream.post "/c/:slug/channels/:channel_id/update"
           Earde.Community_structure_handlers.update_channel_handler;
         (* TM/A-only channel indexability toggle (gated in-handler). *)
         Dream.post "/c/:slug/channels/:channel_id/indexability"
           Earde.Community_settings_handlers.update_channel_indexability_handler;
         Dream.post "/c/:slug/channels/:channel_id/archive"
           Earde.Community_structure_handlers.archive_channel_handler;
         Dream.post "/c/:slug/channels/:channel_id/unarchive"
           Earde.Community_structure_handlers.unarchive_channel_handler;
         Dream.post "/update-community"
           Earde.Community_settings_handlers.update_community_handler;
         (* There are deliberately no /add-mod and /remove-mod routes. They were
       an unreferenced legacy pair — no form, link, script or test emitted
       them — whose authorization was strictly weaker than the surface that
       replaced them: they admitted ANY moderator of the community, so an
       ordinary mod could appoint other moderators and remove a Top Mod. The
       live surface is /c/:slug/manage-mods/{add,promote,remove}, which
       requires top_mod (or a durable admin) and protects top_mod targets.
       Both handlers were deleted with the routes; nothing references them. *)
         Dream.post "/ban-community-user"
           Earde.Moderation_handlers.ban_community_user_handler;
         Dream.post "/unban-community-user"
           Earde.Moderation_handlers.unban_community_user_handler;
         Dream.get "/new-post" Earde.Post_handlers.new_post_page;
         Dream.post "/posts" Earde.Post_handlers.create_post_handler;
         Dream.get "/p/:id" Earde.Post_handlers.view_post_handler;
         Dream.post "/comments" Earde.Comment_handlers.create_comment_handler;
         Dream.post "/vote" Earde.Vote_handlers.vote_handler;
         Dream.post "/vote-comment" Earde.Vote_handlers.vote_comment_handler;
         Dream.get "/u/:username" Earde.Account_handlers.view_profile_handler;
         Dream.get "/search" Earde.Public_handlers.search_handler;
         Dream.get "/settings" Earde.Account_handlers.settings_page_handler;
         Dream.post "/settings" Earde.Account_handlers.update_profile_handler;
         Dream.get "/notifications" Earde.Account_handlers.notifications_handler;
         Dream.post "/delete-account"
           Earde.Account_handlers.delete_account_handler;
         Dream.post "/delete-post" Earde.Post_handlers.delete_post_handler;
         Dream.post "/c/:slug/posts/:id/mod_delete"
           Earde.Post_handlers.mod_delete_post_handler;
         Dream.post "/delete-comment"
           Earde.Comment_handlers.delete_comment_handler;
         Dream.post "/c/:slug/comments/:id/mod_delete"
           Earde.Comment_handlers.mod_delete_comment_handler;
         Dream.get "/admin" Earde.Admin_handlers.admin_dashboard_handler;
         Dream.post "/admin/ban/user/:id" Earde.Admin_handlers.ban_user_handler;
         Dream.post "/admin/unban/user/:id"
           Earde.Admin_handlers.unban_user_global_handler;
         Dream.get "/privacy" Earde.Public_handlers.privacy_page_handler;
         Dream.get "/signup" Earde.Auth_handlers.signup_page;
         Dream.post "/signup"
           (Earde.Rate_limit_middleware.middleware
              Earde.Auth_handlers.signup_handler);
         Dream.get "/verify" Earde.Auth_handlers.verify_email_handler;
         Dream.get "/confirm-email" Earde.Auth_handlers.confirm_email_handler;
         Dream.get "/login" Earde.Auth_handlers.login_page;
         Dream.post "/login"
           (Earde.Rate_limit_middleware.middleware
              Earde.Auth_handlers.login_handler);
         Dream.post "/logout" Earde.Auth_handlers.logout_handler;
         Dream.post "/settings/password"
           Earde.Account_handlers.change_password_handler;
         Dream.get "/forgot-password" Earde.Auth_handlers.forgot_password_page;
         Dream.post "/forgot-password"
           (Earde.Rate_limit_middleware.middleware
              Earde.Auth_handlers.forgot_password_handler);
         Dream.get "/reset-password"
           Earde.Auth_handlers.reset_password_page_handler;
         Dream.post "/reset-password" Earde.Auth_handlers.reset_password_handler;
         (* §9 consent endpoint: JSON-only + Origin/Sec-Fetch-Site protection
       instead of the form-CSRF used by other state-changing routes; the POST
       route matches first, every other method falls to the controlled 405.
       Inside sql_pool (person-property lookup) and sql_sessions (reads the
       authenticated user on granted). *)
         Dream.post "/analytics/consent"
           Earde.Analytics_handlers.analytics_consent_handler;
         Dream.any "/analytics/consent"
           Earde.Analytics_handlers.analytics_consent_method_not_allowed;
         Dream.get "/export-data" Earde.Account_handlers.export_data_handler;
         (* There is deliberately no KPI-dashboard route: PostHog is the
       authoritative analytics product, and /earde-hq-dashboard was removed
       without a replacement page and without a redirect. *)
         Dream.get "/_debug/state" Earde.Admin_handlers.debug_state_handler;
         Dream.get "/static/**" (Dream.static "static");
       ]
