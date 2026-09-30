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
val new_community_form : ?user:string -> ?rail_communities:Community_types.community list -> Dream.request -> string
(** The community-scoped feeds now take [Post_types.feed_item] rows: the community's
    own posts render byte-identically ([fi_shared = None]), while an accepted
    shared-thread placement row links into THIS community's thread context
    and carries its compact "Shared from" provenance and the placement's
    destination section. *)
val community_page : ?user:string -> ?noindex:bool -> ?connected_projects:string -> ?connected_communities:string -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> user_communities:Community_types.community list -> moderated_communities:Community_types.community list -> (int * int) list -> int -> string -> Community_types.community -> Post_types.feed_item list -> Dream.request -> string
val community_section_shell_page : ?user:string -> ?noindex:bool -> ?thread_count:int -> ?last_activity:string -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> section:Section_store.community_section -> user_votes:(int * int) list -> current_page:int -> sort_mode:string -> community:Community_types.community -> posts:Post_types.feed_item list -> Dream.request -> string

(** [/feed] — global Feed surface. [scope] is "following" | "all"; logged-out callers must pass
    [~scope:"all"] [~is_logged_in:false] (no toggle, no personalized feed). [sort_mode] is the
    string form (hot/new/top/active). [rail_communities] drives both the icon rail and the
    Following mini-list. [shared_destinations] (default []) maps a canonical post id to its
    currently publicly renderable accepted shared-thread destinations as [(slug, name)] pairs,
    already restricted and ordered by {!Shared_thread_reading.public_destinations_for_posts};
    it only appends the "Shared with" provenance span to the matching cards — it can never
    add, drop, or reorder the rows the feed query selected. *)
val feed_page : ?user:string -> scope:string -> sort_mode:string -> is_logged_in:bool -> admin_usernames:string list -> rail_communities:Community_types.community list -> user_votes:(int * int) list -> current_page:int -> ?shared_destinations:(int * (string * string) list) list -> Post_types.post list -> Dream.request -> string
(** [source_focus] = (promoted post id, post title, chronological highlight message ids):
    renders the reverse-navigation state — SSR context notice, anchor/highlight data
    attributes, canonical link to the clean channel URL. Omitted → normal channel page. *)
val community_channel_shell_page : ?user:string -> ?realtime_token:string -> ?noindex:bool -> is_member:bool -> ?can_start:bool -> ?thread_links:(int64 * int * string * bool) list -> ?source_focus:(int * string * int64 list) -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> channel:Channel_store.channel -> messages:(Chat_store.chat_message * string option) list -> community:Community_types.community -> Dream.request -> string
(** Viewer-scoped promoted-thread provenance, decided by the handler: [Ts_visible] renders
    the structured source conversation (channel (slug, name) if it still exists, plus the
    chronological source rows); [Ts_private] renders only a neutral notice with no channel
    name, authors, content, or timestamps. *)
type thread_source_view =
  | Ts_private
  | Ts_visible of (string * string) option * Thread_source_store.thread_source_msg list

(** An accepted DESTINATION-context rendering of a canonical thread, fully
    resolved and authorized by the handler (accepted placement, destination
    binding, currently-public origin, viewer admitted by the destination's
    existing access rule). [stc_section] is the placement's own destination
    section — the effective local section context; [None] renders the
    destination's flat/uncategorized context. [stc_origin_name] feeds the
    visible "Shared from" label (safe to name and link: the context only
    exists while the origin is public). Nothing else about the placement —
    actors, notes, ids, lifecycle vocabulary — reaches the page. *)
type shared_thread_page_context = {
  stc_origin_name : string;
  stc_section : (string * string) option;
}

(** The closed post-creation notices the composer's redirect can land on the
    canonical origin thread: sharing requested, or thread created but the
    sharing request could not be sent. Fixed copy only — the page can render
    no other text for them, no failure cause, and no destination name. The
    handler resolves the value from its own closed query vocabulary and only
    for the origin rendering; an unknown query value maps to nothing. *)
type thread_creation_notice =
  | Creation_share_requested
  | Creation_share_failed

(** Canonical thread view inside the persistent shell (/c/:slug/t/:post_id-:post_slug).
    Shell-styled comments/composer; mod/admin/ban dialogs preserve post_page behavior verbatim.
    Preserves the optimistic-vote DOM contract and all comment/vote/mod/delete routes & CSRF.
    [can_comment] is the handler's one SQL participation capability
    ({!Shared_thread_reading.viewer_may_comment}) and gates the composer and
    reply controls — the POST enforces the same rule, so this render gate can
    never grant what the server refuses. [shared_context] switches the page
    into a destination-context rendering: destination shell, destination
    section context, "Shared from" provenance, a hidden server-revalidated
    [context_community] field on the comment forms — while the canonical
    [<link rel=canonical>] keeps pointing at the immutable origin URL
    (callers must also pass [~noindex:true] for destination contexts). The
    moderator-facing arguments ([is_current_user_mod], [mod_usernames],
    [banned_usernames]) are ALWAYS the canonical origin community's:
    destination standing grants no canonical-content controls.
    [shared_with] (default []) is the ORIGIN rendering's own provenance —
    the post's currently publicly renderable accepted destinations as
    [(slug, name)] pairs from
    {!Shared_thread_reading.public_destinations_for_posts} — rendered as the
    "Shared with" label in the thread meta. A destination-context rendering
    ([shared_context = Some _]) ignores it entirely, so the two provenance
    directions can never appear on one page. *)
val thread_shell_page : ?user:string -> ?noindex:bool -> ?can_share:bool -> ?can_comment:bool -> ?creation_notice:thread_creation_notice -> ?shared_context:shared_thread_page_context -> ?shared_with:(string * string) list -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> community:Community_types.community -> ?thread_source:thread_source_view -> user_post_votes:(int * int) list -> user_comment_votes:(int * int) list -> post:Post_types.post -> comments:Comment_store.comment list -> Dream.request -> string
(** [connected_projects_count] and [connected_communities_count] are how many records the
    two public connected-* read models returned for this community — the sizes of exactly
    the lists [/c/:slug/network] renders. The home carries the compact Network entry point
    only: the lists themselves live on that page, and a zero count still renders its row. *)
val community_overview_page : ?user:string -> ?noindex:bool -> ?connected_projects_count:int -> ?connected_communities_count:int -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> orphaned:(int * string option) -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> recent_posts:Post_types.feed_item list -> Community_types.community -> (Section_store.community_section * int * string option) list -> Dream.request -> string
(** [connected_projects] is the pre-rendered "Connected projects" management fragment for the
    top-mod/admin settings surface (empty for every other viewer, which also removes the panel
    and its navigation entry). *)
val community_settings_page : ?user:string -> ?connected_projects:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> is_top_mod:bool -> open_reports_count:int -> community:Community_types.community -> mods:User_store.user list -> banned_users:User_store.user list -> members:User_store.user list -> sections:Section_store.community_section list -> channels:Channel_store.channel list -> Dream.request -> string

(** The shared launch community sidebar (identity head, Overview, factual
    visibility marker, Live channels, Knowledge sections, Network links) in
    the exact grammar the converted /c/:slug routes render. Exposed for the
    project-home review handler, which supplies the prebuilt sidebar to its
    page module rather than duplicating this renderer. [settings_active]
    marks the Settings entry current — the settings hub AND every management
    surface on the shared settings shell (Connections, Shared threads,
    Project home requests, Manage moderators, Reports) pass it, so Settings
    is the one active community-level item inside settings and the internal
    settings navigation distinguishes the surfaces;
    [show_visibility_note:false] suppresses the private-community marker on
    the surfaces that never name an ineligibility reason;
    [moderation_log_active] marks the always-present Moderation log entry
    current (modlog route only — the entry renders for every viewer
    regardless). Defaults preserve every existing route's output. *)
val launch_knowledge_sidebar : community:Community_types.community -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> ?active_section_slug:string -> ?append_uncategorized:bool -> ?settings_active:bool -> ?moderation_log_active:bool -> ?show_visibility_note:bool -> can_manage:bool -> unit -> string

(** Moderator roster + role actions (TM/A only — gated in the handler; the
    POST handlers re-check every role/hierarchy rule server-side, this only
    renders). [rail_communities]/[channels]/[sections] feed the launch
    shell's global rail and shared community sidebar (pass 14C) — loaded by
    the handler only after authorization. *)
val manage_mods_page : ?user:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> current_user_role:string option -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> community:Community_types.community -> mods:Moderator_store.moderator_entry list -> Dream.request -> string

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
val choose_community_page : ?user:string -> ?request:Dream.request -> ?rail_communities:Community_types.community list -> Community_types.community list -> string
val join_to_post_page : ?user:string -> ?rail_communities:Community_types.community list -> Community_types.community -> Dream.request -> string
val new_post_form : ?user:string -> ?preselected_section_id:int -> ?rail_communities:Community_types.community list -> ?share_candidates:(string * string) list -> Section_store.community_section list -> Community_types.community -> Dream.request -> string
(** [share_candidates] are [(slug, name)] pairs of eligible connected
    destination communities, already server-resolved by the handler. When
    non-empty the form gains the optional "Share with a connected community"
    select (blank = "Do not share") and the private request-note field; when
    empty the form is byte-identical to the pre-slice-4 composer. The markup
    grants nothing: POST /posts re-resolves the posted slug and the
    placement store revalidates everything under its own locks. *)

(** SSR report form (no JS). [target_title] is shown as a trimmed, escaped context excerpt;
    [return_url] is the Cancel target. The handler re-validates everything — this only renders.
    Launch chrome (pass 14D): [rail_communities]/[channels]/[sections] feed the shared
    community shell, loaded by the handler only after every access gate passed; [can_manage]
    is the handler's real admin-or-moderator check and only gates the sidebar Settings link. *)
val report_form_page : ?user:string -> ?rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> can_manage:bool -> community:Community_types.community -> target_type:Report_store.report_target -> target_id:int -> target_title:string -> return_url:string -> Dream.request -> string

(** Read-only mod queue (M/TM/A only — gated in the handler). [previews] maps a report id to
    its (context_url, excerpt), built by the handler's bounded per-row lookup; rows absent from
    it render a safe "Target unavailable or deleted" cell. Renders nothing mutable.
    [rail_communities]/[channels]/[sections] feed the launch shell's global rail and shared
    community sidebar (pass 14B) — loaded by the handler only after authorization. *)
val reports_queue_page : ?user:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> is_top_mod:bool -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> community:Community_types.community -> status:Report_store.report_status -> reports:Report_store.report_row list -> previews:(int * (string * string)) list -> Dream.request -> string

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
  val summarize_source : Thread_source_store.thread_source_msg list -> source_summary
end
(** GET form to start a durable thread from a seed chat message + nearby context.
    Launch chrome (pass 16A): [rail_communities]/[channels]/[sections] feed the shared
    launch shell's global rail and community sidebar; [can_manage] is the handler's real
    admin-or-moderator check and only picks the sidebar Settings visibility. *)
val start_thread_form : ?user:string -> ?error:string -> ?rail_communities:Community_types.community list -> channels:Channel_store.channel list -> can_manage:bool -> community:Community_types.community -> channel:Channel_store.channel -> seed_id:int64 -> candidates:(Chat_store.chat_message * string option) list -> sections:Section_store.community_section list -> default_section_id:int -> default_title:string -> default_body:string -> Dream.request -> string
val post_page : ?user:string -> ?noindex:bool -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> community:Community_types.community -> user_communities:Community_types.community list -> moderated_communities:Community_types.community list -> (int * int) list -> (int * int) list -> Post_types.post -> Comment_store.comment list -> Dream.request -> string

(** === USER === *)
val user_profile_page : ?user:string -> ?rail_communities:Community_types.community list -> is_admin:bool -> is_globally_banned:bool -> profile_id:int -> admin_usernames:string list -> moderated_communities:Community_types.community list -> active_tab:string -> (int * int) list -> string -> string -> string option -> string option -> int -> Post_types.post list -> (int * string * string * int * string * int) list -> Community_user_stats_store.community_user_stat list -> Dream.request -> string
val settings_page : ?user:string -> ?rail_communities:Community_types.community list -> string option -> string option -> Dream.request -> string
val notifications_page : ?user:string -> ?rail_communities:Community_types.community list -> Notification_store.notification list -> Dream.request -> string

(** === SEARCH === *)
val search_results_page : ?user:string -> admin_usernames:string list -> ?chat_sources:(int * string * string * int) list -> ?rail_communities:Community_types.community list -> (int * int) list -> int -> string -> string -> Community_types.community list -> (int * string * string * string option * string option) list -> Post_types.post list -> (int * string * string * string * int * int) list -> Dream.request -> string

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
val mod_log_page : ?user:string -> ?noindex:bool -> ?rail_communities:Community_types.community list -> can_access_settings:bool -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> community:Community_types.community -> Mod_log_store.mod_action list -> Dream.request -> string

(** === ADMIN === *)
(** Display-only heuristic: [true] when a username looks bot-generated (digit-heavy,
    vowel-poor, long consonant runs). Pure; only paints a dashboard chip. Exposed for
    unit testing. *)
val looks_random_username : string -> bool
val admin_dashboard_page :
  ?user:string ->
  ?rail_communities:Community_types.community list ->
  signups_enabled:bool ->
  turnstile:[ `Configured | `Disabled | `Misconfigured ] ->
  brevo_configured:bool ->
  recent_users:Admin_store.admin_recent_user list ->
  pending:Admin_store.pending_signup_row list ->
  banned_users:User_store.user list ->
  Dream.request -> string
