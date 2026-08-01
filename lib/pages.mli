(** Full-page HTML assembly. Every function renders through one of the
    Cartographic Civic launch document wrappers in [Components] and inlines
    page-specific content. *)

(** === AUTHENTICATION === *)
val signup_form : ?user:string -> ?error:string -> ?turnstile_site_key:string -> Dream.request -> string
val login_form : ?user:string -> Dream.request -> string
val forgot_password_page : Dream.request -> string
val reset_password_page : token:string -> ?error:string -> Dream.request -> string

(** === COMMUNITY === *)
(** Global-admin-only legacy community creation form (GET /new-community), on
    the Cartographic Civic launch app chrome. [rail_communities] is the
    viewer's joined communities for the dark rail, loaded by the handler only
    AFTER the global-admin gate; defaults to [] so pure renders need no DB. *)
val new_community_form : ?user:string -> ?rail_communities:Db.community list -> Dream.request -> string
val community_page : ?user:string -> ?noindex:bool -> ?connected_projects:string -> ?connected_communities:string -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> int -> string -> Db.community -> Db.post list -> Dream.request -> string
val community_section_shell_page : ?user:string -> ?noindex:bool -> ?thread_count:int -> ?last_activity:string -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Db.community list -> channels:Db.channel list -> sections:Db.community_section list -> section:Db.community_section -> user_votes:(int * int) list -> current_page:int -> sort_mode:string -> community:Db.community -> posts:Db.post list -> Dream.request -> string

(** [/feed] — global Feed surface. [scope] is "following" | "all"; logged-out callers must pass
    [~scope:"all"] [~is_logged_in:false] (no toggle, no personalized feed). [sort_mode] is the
    string form (hot/new/top/active). [rail_communities] drives both the icon rail and the
    Following mini-list. *)
val feed_page : ?user:string -> scope:string -> sort_mode:string -> is_logged_in:bool -> admin_usernames:string list -> rail_communities:Db.community list -> user_votes:(int * int) list -> current_page:int -> Db.post list -> Dream.request -> string
(** [source_focus] = (promoted post id, post title, chronological highlight message ids):
    renders the reverse-navigation state — SSR context notice, anchor/highlight data
    attributes, canonical link to the clean channel URL. Omitted → normal channel page. *)
val community_channel_shell_page : ?user:string -> ?realtime_token:string -> ?noindex:bool -> is_member:bool -> ?can_start:bool -> ?thread_links:(int64 * int * string * bool) list -> ?source_focus:(int * string * int64 list) -> rail_communities:Db.community list -> channels:Db.channel list -> sections:Db.community_section list -> channel:Db.channel -> messages:(Db.chat_message * string option) list -> community:Db.community -> Dream.request -> string
(** Viewer-scoped promoted-thread provenance, decided by the handler: [Ts_visible] renders
    the structured source conversation (channel (slug, name) if it still exists, plus the
    chronological source rows); [Ts_private] renders only a neutral notice with no channel
    name, authors, content, or timestamps. *)
type thread_source_view =
  | Ts_private
  | Ts_visible of (string * string) option * Db.thread_source_msg list
(** Canonical thread view inside the persistent shell (/c/:slug/t/:post_id-:post_slug).
    Shell-styled comments/composer; mod/admin/ban dialogs preserve post_page behavior verbatim.
    Preserves the optimistic-vote DOM contract and all comment/vote/mod/delete routes & CSRF. *)
val thread_shell_page : ?user:string -> ?noindex:bool -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Db.community list -> channels:Db.channel list -> sections:Db.community_section list -> community:Db.community -> ?thread_source:thread_source_view -> user_post_votes:(int * int) list -> user_comment_votes:(int * int) list -> post:Db.post -> comments:Db.comment list -> Dream.request -> string
(** [connected_projects_count] and [connected_communities_count] are how many records the
    two public connected-* read models returned for this community — the sizes of exactly
    the lists [/c/:slug/network] renders. The home carries the compact Network entry point
    only: the lists themselves live on that page, and a zero count still renders its row. *)
val community_overview_page : ?user:string -> ?noindex:bool -> ?connected_projects_count:int -> ?connected_communities_count:int -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> orphaned:(int * string option) -> rail_communities:Db.community list -> channels:Db.channel list -> recent_posts:Db.post list -> Db.community -> (Db.community_section * int * string option) list -> Dream.request -> string
(** [connected_projects] is the pre-rendered "Connected projects" management fragment for the
    top-mod/admin settings surface (empty for every other viewer, which also removes the panel
    and its navigation entry). *)
val community_settings_page : ?user:string -> ?connected_projects:string -> ?rail_communities:Db.community list -> is_admin:bool -> is_top_mod:bool -> open_reports_count:int -> community:Db.community -> mods:Db.user list -> banned_users:Db.user list -> members:Db.user list -> sections:Db.community_section list -> channels:Db.channel list -> Dream.request -> string

(** The shared launch community sidebar (identity head, Overview, factual
    visibility marker, Live channels, Knowledge sections, Network links) in
    the exact grammar the converted /c/:slug routes render. Exposed for the
    project-home review handler, which supplies the prebuilt sidebar to its
    page module rather than duplicating this renderer. [home_requests_active]
    renders the Home requests entry active — callers must have re-proved the
    top-mod/admin gate first; [show_visibility_note:false] suppresses the
    private-community marker on the one surface that never names an
    ineligibility reason; [moderation_log_active] marks the always-present
    Moderation log entry current (modlog route only — the entry renders for
    every viewer regardless); [reports_active] renders the Reports entry
    active (report-queue route only — callers must have re-proved the M/TM/A
    gate first); [manage_moderators_active] renders the Manage moderators
    entry active (manage-mods route only — callers must have re-proved the
    TM/A gate first). Defaults preserve every existing route's output. *)
val launch_knowledge_sidebar : community:Db.community -> channels:Db.channel list -> sections:Db.community_section list -> ?active_section_slug:string -> ?append_uncategorized:bool -> ?settings_active:bool -> ?home_requests_active:bool -> ?connections_active:bool -> ?moderation_log_active:bool -> ?reports_active:bool -> ?manage_moderators_active:bool -> ?show_visibility_note:bool -> can_manage:bool -> unit -> string

(** Moderator roster + role actions (TM/A only — gated in the handler; the
    POST handlers re-check every role/hierarchy rule server-side, this only
    renders). [rail_communities]/[channels]/[sections] feed the launch
    shell's global rail and shared community sidebar (pass 14C) — loaded by
    the handler only after authorization. *)
val manage_mods_page : ?user:string -> ?rail_communities:Db.community list -> is_admin:bool -> current_user_role:string option -> channels:Db.channel list -> sections:Db.community_section list -> community:Db.community -> mods:Db.moderator_entry list -> Dream.request -> string

(** === POST === *)

(** The three GET /new-post states on the launch shell (pass 15B). The
    create-* fragments (form action/method/enctype, every field name and
    hidden input, the Dream CSRF tag and the .create-shell marker) are
    unchanged; only the outer document is the shared launch app chrome.
    [rail_communities] is the viewer's joined communities as the handler
    loaded them — after the community authorization gate on the
    community-bound states, so an unauthorized request never loads rail
    data. The chooser's [request] feeds the shared analytics assets and the
    admin session flag; the community-bound states also carry their
    (id, visibility) analytics pair exactly as before. *)
val choose_community_page : ?user:string -> ?request:Dream.request -> ?rail_communities:Db.community list -> Db.community list -> string
val join_to_post_page : ?user:string -> ?rail_communities:Db.community list -> Db.community -> Dream.request -> string
val new_post_form : ?user:string -> ?preselected_section_id:int -> ?rail_communities:Db.community list -> Db.community_section list -> Db.community -> Dream.request -> string

(** SSR report form (no JS). [target_title] is shown as a trimmed, escaped context excerpt;
    [return_url] is the Cancel target. The handler re-validates everything — this only renders.
    Launch chrome (pass 14D): [rail_communities]/[channels]/[sections] feed the shared
    community shell, loaded by the handler only after every access gate passed; [can_manage]
    is the handler's real admin-or-moderator check and only gates the sidebar Settings link. *)
val report_form_page : ?user:string -> ?rail_communities:Db.community list -> channels:Db.channel list -> sections:Db.community_section list -> can_manage:bool -> community:Db.community -> target_type:Db.report_target -> target_id:int -> target_title:string -> return_url:string -> Dream.request -> string

(** Read-only mod queue (M/TM/A only — gated in the handler). [previews] maps a report id to
    its (context_url, excerpt), built by the handler's bounded per-row lookup; rows absent from
    it render a safe "Target unavailable or deleted" cell. Renders nothing mutable.
    [rail_communities]/[channels]/[sections] feed the launch shell's global rail and shared
    community sidebar (pass 14B) — loaded by the handler only after authorization. *)
val reports_queue_page : ?user:string -> ?rail_communities:Db.community list -> channels:Db.channel list -> sections:Db.community_section list -> community:Db.community -> status:Db.report_status -> reports:Db.report_row list -> previews:(int * (string * string)) list -> Dream.request -> string

(** "Start thread from chat" pure helpers (title prefill, checkbox-id parsing, server-side
    selection guard, provenance summary, reverse-navigation parsing). Pure — unit-tested
    without a DB. *)
module Start_thread : sig
  val derive_title : string -> string
  val parse_selected_ids : (string * string) list -> int64 list
  (* Channel-row marker for a chat message, from all its thread-source links. *)
  type msg_marker =
    | Mk_seed of int * string
    | Mk_referenced of int * string * int
    | Mk_no_link
  val classify_message_links : (int * string * bool) list -> msg_marker
  val normalize_selection : seed:int64 -> max_total:int -> valid:int64 list -> int64 list -> int64 list
  (* ?source_thread= strict positive-int parse; anything else is "no focus". *)
  val parse_source_thread : string option -> int option
  (* Comma-joined ids for data-source-highlight-ids (attribute-safe by construction). *)
  val highlight_ids_attr : int64 list -> string
  (* Compact "YYYY-MM-DD" / "YYYY-MM-DD HH:MM" truncations of a Postgres timestamp text. *)
  val date_of_ts : string -> string
  val minute_of_ts : string -> string
  (* Provenance metadata for the promoted-conversation block, derived from persisted
     source rows only (never from the editable introduction). *)
  type source_summary = {
    ss_available : int;
    ss_unavailable : int;
    ss_participants : int;
    ss_date_range : string;
  }
  val summarize_source : Db.thread_source_msg list -> source_summary
end
(** GET form to start a durable thread from a seed chat message + nearby context.
    Launch chrome (pass 16A): [rail_communities]/[channels]/[sections] feed the shared
    launch shell's global rail and community sidebar; [can_manage] is the handler's real
    admin-or-moderator check and only picks the sidebar Settings visibility. *)
val start_thread_form : ?user:string -> ?error:string -> ?rail_communities:Db.community list -> channels:Db.channel list -> can_manage:bool -> community:Db.community -> channel:Db.channel -> seed_id:int64 -> candidates:(Db.chat_message * string option) list -> sections:Db.community_section list -> default_section_id:int -> default_title:string -> default_body:string -> Dream.request -> string
val post_page : ?user:string -> ?noindex:bool -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> community:Db.community -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> (int * int) list -> Db.post -> Db.comment list -> Dream.request -> string

(** === USER === *)
val user_profile_page : ?user:string -> ?rail_communities:Db.community list -> is_admin:bool -> is_globally_banned:bool -> profile_id:int -> admin_usernames:string list -> moderated_communities:Db.community list -> active_tab:string -> (int * int) list -> string -> string -> string option -> string option -> int -> Db.post list -> (int * string * string * int * string * int) list -> Db.community_user_stat list -> Dream.request -> string
val settings_page : ?user:string -> ?rail_communities:Db.community list -> string option -> string option -> Dream.request -> string
val notifications_page : ?user:string -> ?rail_communities:Db.community list -> Db.notification list -> Dream.request -> string

(** === SEARCH === *)
val search_results_page : ?user:string -> admin_usernames:string list -> ?chat_sources:(int * string * string * int) list -> ?rail_communities:Db.community list -> (int * int) list -> int -> string -> string -> Db.community list -> (int * string * string * string option * string option) list -> Db.post list -> (int * string * string * string * int * int) list -> Dream.request -> string

(** === LEGAL / PRIVACY === *)
val privacy_page : ?user:string -> Dream.request -> string

(** === MESSAGE PAGE === *)
(** One chrome-free launch message document for every caller. [auth] is
    accepted for signature compatibility with the ~400 existing call sites and
    no longer varies the output — the anti-enumeration pins require byte
    identity between the account-lifecycle and general outcomes. *)
val msg_page : ?user:string -> ?auth:bool -> title:string -> message:string -> alert_type:string -> return_url:string -> Dream.request -> string

(** === MODERATION LOG === *)
(** [rail_communities], [channels] and [sections] feed the launch chrome
    (global rail + shared community sidebar) — supplied by the handler only
    after the private-community authorization decision. [can_access_settings]
    keeps its existing job (back-link target) and additionally gates the
    sidebar's Settings entry, mirroring the sibling launch routes. *)
val mod_log_page : ?user:string -> ?noindex:bool -> ?rail_communities:Db.community list -> can_access_settings:bool -> channels:Db.channel list -> sections:Db.community_section list -> community:Db.community -> Db.mod_action list -> Dream.request -> string

(** === ADMIN === *)
(** Display-only heuristic: [true] when a username looks bot-generated (digit-heavy,
    vowel-poor, long consonant runs). Pure; only paints a dashboard chip. Exposed for
    unit testing. *)
val looks_random_username : string -> bool
val admin_dashboard_page :
  ?user:string ->
  ?rail_communities:Db.community list ->
  signups_enabled:bool ->
  turnstile:[ `Configured | `Disabled | `Misconfigured ] ->
  brevo_configured:bool ->
  recent_users:Db.admin_recent_user list ->
  pending:Db.pending_signup_row list ->
  banned_users:Db.user list ->
  Dream.request -> string
