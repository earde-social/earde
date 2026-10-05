(* The application's route table. It lives in the library, not in
   bin/main.ml, so routed regression tests drive exactly the routes, the
   rate-limit wrapping and the handlers that production serves; main.ml keeps
   the process configuration and the middleware stack around it. *)
let router =
  Dream.router
    [
      (* / redirects to the global Feed. Temporary (302), not a 301, so / can
       become a real landing page later without a cached permanent redirect
       standing in the way. *)
      Dream.get "/" (fun request -> Dream.redirect request "/feed");
      (* Legacy /all is superseded by /feed. Redirect (not 404) to preserve old
       bookmarks. Temporary redirect to match the existing /-> /feed style above;
       the repo has no permanent-redirect (301/308) pattern. *)
      Dream.get "/all" (fun request -> Dream.redirect request "/feed");
      Dream.get "/feed" Public_handlers.feed_handler;
      (* Entry and return page for GitHub onboarding: offers the start action
       when the viewer passes the onboarding policy and shows the one-time
       connected/failed callback feedback. Informational GET, deliberately
       not rate-limited; the mode is re-read per request (uncached, matching
       the other onboarding routes below). *)
      Dream.get "/bring" (fun request ->
          Github_onboarding_handlers.make_bring_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      (* Starts GitHub App installation: rate-limited like the other sensitive
       POSTs. The mode is re-read per request via the existing
       Project_onboarding API (uncached, matching /bring) and the validated
       GitHub App configuration is loaded per request from the environment.
       No GET variant and no callback routes in this slice. *)
      Dream.post "/integrations/github/install/start"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Github_installation_start (fun request ->
             Github_onboarding_handlers.make_start_installation_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      (* GitHub App setup return: GET only, and deliberately NOT wrapped in
       Rate_limit.middleware — malformed or cookieless requests are rejected
       before any database access, a valid encrypted per-flow cookie is
       required before the single attach UPDATE, and the limiter's blocked
       page is rendered HTML, while this state-bearing callback URL must only
       ever answer with a clean redirect away. Request-target redaction keeps
       the state out of Dream logging and analytics. *)
      Dream.get "/integrations/github/install/return" (fun request ->
          Github_onboarding_handlers.make_setup_return_handler
            ~mode:(Project_onboarding.mode_from_env ())
            ~load_config:Github_app_config.from_env request);
      (* Final OAuth authorization callback: GET only and, like the setup
       return, deliberately NOT wrapped in Rate_limit.middleware — the
       limiter's blocked page is rendered HTML, while this code/state-bearing
       callback URL must only ever answer a clean redirect away, and
       request-target redaction already keeps code and state out of Dream
       logging and analytics. Secret credentials and the three GitHub
       transports are injected here so the handler stays testable offline. *)
      Dream.get "/integrations/github/authorize/callback" (fun request ->
          Github_onboarding_handlers.make_oauth_callback_handler
            ~mode:(Project_onboarding.mode_from_env ())
            ~load_config:Github_app_config.from_env
            ~load_credentials:Github_oauth_credentials.from_env
            ~exchange_transport:
              (module Github_oauth_token_exchange.Cohttp_transport)
            ~installations_transport:
              (module Github_user_installations.Cohttp_transport)
            ~repositories_transport:
              (module Github_user_installation_repositories.Cohttp_transport)
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
          Project_setup_handlers.make_new_project_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      Dream.post "/projects/new/repositories"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_repository_selection (fun request ->
             Project_setup_handlers.make_repository_selection_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      (* Permanent project creation and its PRG destination. The POST shares
       the sensitive-POST rate limit and per-request configuration load of
       the selection POST above (origin policy only — no GitHub credential,
       no outbound HTTP); the owner-only GET is informational and
       deliberately not rate-limited, matching the other project-setup
       GETs. *)
      Dream.post "/projects"
        (Rate_limit_middleware.middleware Rate_limit_middleware.Project_creation
           (fun request ->
             Project_creation_handlers.make_project_creation_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      Dream.get "/projects/:slug/setup" (fun request ->
          Project_creation_handlers.make_project_home_setup_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      (* Existing-community home request for a verified project. The
       steward-only GET is informational and deliberately not rate-limited,
       matching the other project-setup GETs; the request-creating POST
       shares the sensitive-POST rate limit and per-request configuration
       load of the project POSTs above (origin policy only — no GitHub
       credential, no outbound HTTP). *)
      Dream.get "/projects/:slug/request-home" (fun request ->
          Project_home_request_handlers.make_project_home_choice_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      Dream.post "/projects/:slug/request-home"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_request (fun request ->
             Project_home_request_handlers.make_project_home_request_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
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
          Project_home_provisioning_handlers
          .make_project_home_provisioning_page_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      Dream.post "/projects/:slug/community-home"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_provisioning (fun request ->
             Project_home_provisioning_handlers
             .make_project_home_provisioning_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      (* Moderator review of pending project-home requests for a community.
       The queue GET is informational and deliberately not rate-limited,
       matching the other private settings GETs; the accept/reject POSTs
       share the sensitive-POST rate limit and per-request configuration
       load of the project POSTs above (origin policy only — no GitHub
       credential, no outbound HTTP). Mode is re-read per request (uncached,
       matching the onboarding routes). Authorization is decided in the read
       model and review store SQL, not here. *)
      Dream.get "/c/:slug/project-home-requests" (fun request ->
          Project_home_review_handlers.make_project_home_review_queue_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      Dream.post "/c/:slug/projects/:project_slug/accept"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_accept (fun request ->
             Project_home_review_handlers.make_project_home_accept_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      Dream.post "/c/:slug/projects/:project_slug/reject"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_reject (fun request ->
             Project_home_review_handlers.make_project_home_reject_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
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
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_removal_by_project (fun request ->
             Project_home_removal_handlers
             .make_project_side_home_removal_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      Dream.post "/c/:community_slug/projects/:project_slug/remove-home"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Project_home_removal_by_community
           (fun request ->
             Project_home_removal_handlers
             .make_community_side_home_removal_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
      Dream.get "/new-community" Community_handlers.new_community_page;
      Dream.post "/communities" Community_handlers.create_community_handler;
      Dream.post "/join" Membership_handlers.join_community_handler;
      Dream.post "/leave" Membership_handlers.leave_community_handler;
      Dream.get "/c/:slug" Community_handlers.community_page_handler;
      Dream.get "/c/:slug/s/:section_slug"
        Community_handlers.community_section_handler;
      Dream.get "/c/:slug/ch/:channel_slug"
        Chat_handlers.community_channel_handler;
      Dream.get "/c/:slug/ch/:channel_slug/messages.json"
        Chat_handlers.channel_messages_json_handler;
      Dream.get "/c/:slug/ch/:channel_slug/realtime-token"
        Chat_handlers.realtime_token_handler;
      Dream.get "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
        Start_thread_handlers.start_thread_form_handler;
      Dream.post "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
        Start_thread_handlers.start_thread_create_handler;
      Dream.get "/c/:slug/t/:thread" Post_handlers.view_thread_handler;
      Dream.post "/messages" Chat_handlers.send_message_handler;
      Dream.get "/c/:slug/settings"
        Community_settings_handlers.community_settings_handler;
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
          Network_community_publication_handlers
          .make_network_community_publication_page_handler
            ~mode:(Project_onboarding.mode_from_env ())
            request);
      Dream.post "/c/:slug/publish"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Network_community_publication (fun request ->
             Network_community_publication_handlers
             .make_network_community_publication_handler
               ~mode:(Project_onboarding.mode_from_env ())
               ~load_config:Github_app_config.from_env request));
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
        Community_connections_handlers.make_connections_page_handler;
      Dream.get "/c/:slug/settings/connections/new"
        Community_connections_handlers.make_connections_search_handler;
      Dream.post "/c/:slug/settings/connections/request"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Community_connection_request
           Community_connections_handlers.make_connection_request_handler);
      Dream.post "/c/:slug/settings/connections/:id/accept"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Community_connection_accept
           Community_connections_handlers.make_connection_accept_handler);
      Dream.post "/c/:slug/settings/connections/:id/reject"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Community_connection_reject
           Community_connections_handlers.make_connection_reject_handler);
      Dream.post "/c/:slug/settings/connections/:id/remove"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Community_connection_removal
           Community_connections_handlers.make_connection_removal_handler);
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
        Shared_thread_placement_handlers.make_share_page_handler;
      Dream.post "/c/:slug/t/:thread/share"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Shared_thread_share_request
           Shared_thread_placement_handlers.make_share_request_handler);
      Dream.get "/c/:slug/settings/shared-threads"
        Shared_thread_placement_handlers.make_management_page_handler;
      Dream.post "/c/:slug/settings/shared-threads/:placement_id/accept"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Shared_thread_accept
           Shared_thread_placement_handlers.make_accept_handler);
      Dream.post "/c/:slug/settings/shared-threads/:placement_id/reject"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Shared_thread_reject
           Shared_thread_placement_handlers.make_reject_handler);
      Dream.post "/c/:slug/settings/shared-threads/:placement_id/withdraw"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Shared_thread_withdrawal
           Shared_thread_placement_handlers.make_withdrawal_handler);
      Dream.post "/c/:slug/settings/shared-threads/:placement_id/remove"
        (Rate_limit_middleware.middleware
           Rate_limit_middleware.Shared_thread_removal
           Shared_thread_placement_handlers.make_removal_handler);
      (* Public Network page: the community's connected projects and connected
       communities in full, which the community home now links to instead of
       listing. A distinct literal segment from settings/modlog/reports and
       from the /settings/connections management paths, so nothing shadows
       anything. Read-only and deliberately public — its access decision is
       the community's own can_view_community, exactly like /c/:slug. *)
      Dream.get "/c/:slug/network" Community_handlers.community_network_handler;
      Dream.get "/c/:slug/modlog" Moderation_handlers.modlog_handler;
      (* Reports: singular GET form + plural POST create + plural GET mod queue
       (read-only). Distinct literal segments from settings/modlog/manage-mods, so no router
       shadowing; the queue GET and create POST share the /reports path and split by method. *)
      Dream.get "/c/:slug/report" Moderation_handlers.report_form_handler;
      Dream.get "/c/:slug/reports" Moderation_handlers.reports_queue_handler;
      Dream.post "/c/:slug/reports" Moderation_handlers.create_report_handler;
      (* Resolution POSTs sit on a longer path (.../reports/:report_id/{dismiss,action}) than the
       create POST (.../reports), so they don't shadow it; M/TM/A gated in the handler. *)
      Dream.post "/c/:slug/reports/:report_id/dismiss"
        Moderation_handlers.dismiss_report_handler;
      Dream.post "/c/:slug/reports/:report_id/action"
        Moderation_handlers.action_report_handler;
      Dream.get "/c/:slug/manage-mods" Moderation_handlers.manage_mods_handler;
      Dream.post "/c/:slug/toggle_downvotes"
        Community_settings_handlers.toggle_downvotes_handler;
      (* TM/A-only visibility + discovery controls. Distinct literal sub-segments under
       /settings/, so no shadowing of the /c/:slug/settings GET. *)
      Dream.post "/c/:slug/settings/visibility"
        Community_settings_handlers.update_community_visibility_handler;
      Dream.post "/c/:slug/settings/indexability"
        Community_settings_handlers.update_community_indexability_handler;
      (* TM/A-only member allow-list management (private communities). Distinct
       /settings/members/* sub-segments, so no shadowing of the settings GET or the visibility POSTs. *)
      Dream.post "/c/:slug/settings/members/add"
        Membership_handlers.add_member_handler;
      Dream.post "/c/:slug/settings/members/remove"
        Membership_handlers.remove_member_handler;
      Dream.post "/c/:slug/manage-mods/add"
        Moderation_handlers.manage_mods_add_handler;
      Dream.post "/c/:slug/manage-mods/promote"
        Moderation_handlers.manage_mods_promote_handler;
      Dream.post "/c/:slug/manage-mods/remove"
        Moderation_handlers.manage_mods_remove_handler;
      Dream.post "/c/:slug/sections/add"
        Community_structure_handlers.add_section_handler;
      Dream.post "/c/:slug/sections/:section_id/update"
        Community_structure_handlers.update_section_handler;
      (* TM/A-only section indexability toggle (gated in-handler). *)
      Dream.post "/c/:slug/sections/:section_id/indexability"
        Community_settings_handlers.update_section_indexability_handler;
      Dream.post "/c/:slug/sections/:section_id/delete"
        Community_structure_handlers.delete_section_handler;
      Dream.post "/c/:slug/channels/add"
        Community_structure_handlers.add_channel_handler;
      Dream.post "/c/:slug/channels/:channel_id/update"
        Community_structure_handlers.update_channel_handler;
      (* TM/A-only channel indexability toggle (gated in-handler). *)
      Dream.post "/c/:slug/channels/:channel_id/indexability"
        Community_settings_handlers.update_channel_indexability_handler;
      Dream.post "/c/:slug/channels/:channel_id/archive"
        Community_structure_handlers.archive_channel_handler;
      Dream.post "/c/:slug/channels/:channel_id/unarchive"
        Community_structure_handlers.unarchive_channel_handler;
      Dream.post "/update-community"
        Community_settings_handlers.update_community_handler;
      (* There are deliberately no /add-mod and /remove-mod routes. They were
       an unreferenced legacy pair — no form, link, script or test emitted
       them — whose authorization was strictly weaker than the surface that
       replaced them: they admitted ANY moderator of the community, so an
       ordinary mod could appoint other moderators and remove a Top Mod. The
       live surface is /c/:slug/manage-mods/{add,promote,remove}, which
       requires top_mod (or a durable admin) and protects top_mod targets.
       Both handlers were deleted with the routes; nothing references them. *)
      Dream.post "/ban-community-user"
        Moderation_handlers.ban_community_user_handler;
      Dream.post "/unban-community-user"
        Moderation_handlers.unban_community_user_handler;
      Dream.get "/new-post" Post_handlers.new_post_page;
      Dream.post "/posts" Post_handlers.create_post_handler;
      Dream.get "/p/:id" Post_handlers.view_post_handler;
      Dream.post "/comments" Comment_handlers.create_comment_handler;
      Dream.post "/vote" Vote_handlers.vote_handler;
      Dream.post "/vote-comment" Vote_handlers.vote_comment_handler;
      Dream.get "/u/:username" Account_handlers.view_profile_handler;
      Dream.get "/search" Public_handlers.search_handler;
      Dream.get "/settings" Account_handlers.settings_page_handler;
      Dream.post "/settings" Account_handlers.update_profile_handler;
      Dream.get "/notifications" Account_handlers.notifications_handler;
      Dream.post "/delete-account" Account_handlers.delete_account_handler;
      Dream.post "/delete-post" Post_handlers.delete_post_handler;
      Dream.post "/c/:slug/posts/:id/mod_delete"
        Post_handlers.mod_delete_post_handler;
      Dream.post "/delete-comment" Comment_handlers.delete_comment_handler;
      Dream.post "/c/:slug/comments/:id/mod_delete"
        Comment_handlers.mod_delete_comment_handler;
      Dream.get "/admin" Admin_handlers.admin_dashboard_handler;
      Dream.post "/admin/ban/user/:id" Admin_handlers.ban_user_handler;
      Dream.post "/admin/unban/user/:id"
        Admin_handlers.unban_user_global_handler;
      Dream.get "/privacy" Public_handlers.privacy_page_handler;
      Dream.get "/signup" Auth_handlers.signup_page;
      Dream.post "/signup"
        (Rate_limit_middleware.middleware Rate_limit_middleware.Signup
           Auth_handlers.signup_handler);
      Dream.get "/verify" Auth_handlers.verify_email_handler;
      Dream.get "/confirm-email" Auth_handlers.confirm_email_handler;
      Dream.get "/login" Auth_handlers.login_page;
      Dream.post "/login"
        (Rate_limit_middleware.middleware Rate_limit_middleware.Login
           Auth_handlers.login_handler);
      Dream.post "/logout" Auth_handlers.logout_handler;
      Dream.post "/settings/password" Account_handlers.change_password_handler;
      Dream.get "/forgot-password" Auth_handlers.forgot_password_page;
      Dream.post "/forgot-password"
        (Rate_limit_middleware.middleware Rate_limit_middleware.Forgot_password
           Auth_handlers.forgot_password_handler);
      Dream.get "/reset-password" Auth_handlers.reset_password_page_handler;
      Dream.post "/reset-password" Auth_handlers.reset_password_handler;
      (* §9 consent endpoint: JSON-only + Origin/Sec-Fetch-Site protection
       instead of the form-CSRF used by other state-changing routes; the POST
       route matches first, every other method falls to the controlled 405.
       Inside sql_pool (person-property lookup) and sql_sessions (reads the
       authenticated user on granted). *)
      Dream.post "/analytics/consent"
        Analytics_handlers.analytics_consent_handler;
      Dream.any "/analytics/consent"
        Analytics_handlers.analytics_consent_method_not_allowed;
      Dream.get "/export-data" Account_handlers.export_data_handler;
      (* There is deliberately no KPI-dashboard route: PostHog is the
       authoritative analytics product, and /earde-hq-dashboard was removed
       without a replacement page and without a redirect. *)
      Dream.get "/_debug/state" Admin_handlers.debug_state_handler;
      Dream.get "/static/**" (Dream.static "static");
    ]
