(** Full-page HTML assembly. Each function calls Components.layout and inlines
    page-specific content. hq_dashboard_page is the only exception — it emits
    standalone HTML with no shared nav, intentionally isolated from the main shell. *)

(** === CORE FEED === *)
val index : ?user:string -> (int * int) list -> int -> string -> feed_type:string -> admin_usernames:string list -> moderated_communities:Db.community list -> Db.post list -> Db.community list -> Dream.request -> string

(** === AUTHENTICATION === *)
val signup_form : ?user:string -> ?error:string -> ?turnstile_site_key:string -> Dream.request -> string
val login_form : ?user:string -> Dream.request -> string
val forgot_password_page : Dream.request -> string
val reset_password_page : token:string -> ?error:string -> Dream.request -> string

(** === COMMUNITY === *)
val new_community_form : ?user:string -> Dream.request -> string
val community_page : ?user:string -> ?noindex:bool -> ?connected_projects:string -> ?section:Db.community_section -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> int -> string -> Db.community -> Db.post list -> Dream.request -> string
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
val community_overview_page : ?user:string -> ?noindex:bool -> ?connected_projects:string -> is_member:bool -> is_current_user_mod:bool -> is_current_user_top_mod:bool -> mod_usernames:string list -> orphaned:(int * string option) -> rail_communities:Db.community list -> channels:Db.channel list -> recent_posts:Db.post list -> Db.community -> (Db.community_section * int * string option) list -> Dream.request -> string
(** [connected_projects] is the pre-rendered "Connected projects" management fragment for the
    top-mod/admin settings surface (empty for every other viewer, which also removes the panel
    and its navigation entry). *)
val community_settings_page : ?user:string -> ?connected_projects:string -> is_admin:bool -> is_top_mod:bool -> open_reports_count:int -> community:Db.community -> mods:Db.user list -> banned_users:Db.user list -> members:Db.user list -> sections:Db.community_section list -> channels:Db.channel list -> Dream.request -> string
val manage_mods_page : ?user:string -> is_admin:bool -> current_user_role:string option -> community:Db.community -> mods:Db.moderator_entry list -> Dream.request -> string

(** === POST === *)
val choose_community_page : ?user:string -> Db.community list -> string
val join_to_post_page : ?user:string -> Db.community -> Dream.request -> string
val new_post_form : ?user:string -> ?preselected_section_id:int -> Db.community_section list -> Db.community -> Dream.request -> string

(** SSR report form (no JS). [target_title] is shown as a trimmed, escaped context excerpt;
    [return_url] is the Cancel target. The handler re-validates everything — this only renders. *)
val report_form_page : ?user:string -> community:Db.community -> target_type:Db.report_target -> target_id:int -> target_title:string -> return_url:string -> Dream.request -> string

(** Read-only mod queue (M/TM/A only — gated in the handler). [previews] maps a report id to
    its (context_url, excerpt), built by the handler's bounded per-row lookup; rows absent from
    it render a safe "Target unavailable or deleted" cell. Renders nothing mutable. *)
val reports_queue_page : ?user:string -> community:Db.community -> status:Db.report_status -> reports:Db.report_row list -> previews:(int * (string * string)) list -> Dream.request -> string

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
(** GET form to start a durable thread from a seed chat message + nearby context. *)
val start_thread_form : ?user:string -> ?error:string -> community:Db.community -> channel:Db.channel -> seed_id:int64 -> candidates:(Db.chat_message * string option) list -> sections:Db.community_section list -> default_section_id:int -> default_title:string -> default_body:string -> Dream.request -> string
val post_page : ?user:string -> ?noindex:bool -> is_member:bool -> is_current_user_mod:bool -> mod_usernames:string list -> admin_usernames:string list -> banned_usernames:string list -> community:Db.community -> user_communities:Db.community list -> moderated_communities:Db.community list -> (int * int) list -> (int * int) list -> Db.post -> Db.comment list -> Dream.request -> string

(** === USER === *)
val user_profile_page : ?user:string -> is_admin:bool -> is_globally_banned:bool -> profile_id:int -> admin_usernames:string list -> moderated_communities:Db.community list -> active_tab:string -> (int * int) list -> string -> string -> string option -> string option -> int -> Db.post list -> (int * string * string * int * string * int) list -> Db.community_user_stat list -> Dream.request -> string
val settings_page : ?user:string -> string option -> string option -> Dream.request -> string
val notifications_page : ?user:string -> Db.notification list -> Dream.request -> string

(** === SEARCH === *)
val search_results_page : ?user:string -> admin_usernames:string list -> ?chat_sources:(int * string * string * int) list -> (int * int) list -> int -> string -> string -> Db.community list -> (int * string * string * string option * string option) list -> Db.post list -> (int * string * string * string * int * int) list -> Dream.request -> string

(** === LEGAL / PRIVACY === *)
val privacy_page : ?user:string -> Dream.request -> string

(** === MESSAGE PAGE === *)
(** [auth:true] renders the focused auth panel (auth.css) for account-lifecycle
    flows; the default keeps the warm `Site card for every other caller. *)
val msg_page : ?user:string -> ?auth:bool -> title:string -> message:string -> alert_type:string -> return_url:string -> Dream.request -> string

(** === MODERATION LOG === *)
val mod_log_page : ?user:string -> ?noindex:bool -> can_access_settings:bool -> community:Db.community -> Db.mod_action list -> Dream.request -> string

(** === ADMIN === *)
(** Display-only heuristic: [true] when a username looks bot-generated (digit-heavy,
    vowel-poor, long consonant runs). Pure; only paints a dashboard chip. Exposed for
    unit testing. *)
val looks_random_username : string -> bool
val admin_dashboard_page :
  ?user:string ->
  signups_enabled:bool ->
  turnstile:[ `Configured | `Disabled | `Misconfigured ] ->
  brevo_configured:bool ->
  recent_users:Db.admin_recent_user list ->
  pending:Db.pending_signup_row list ->
  banned_users:Db.user list ->
  Dream.request -> string
val hq_dashboard_page : ((int * int * int) * (int * int)) -> dau_mau_ratio:float -> start_date:string -> end_date:string -> string
