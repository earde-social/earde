(** The live-chat channel page and the pure helpers behind starting a
    thread from chat. *)

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
    Launch chrome: [rail_communities]/[channels]/[sections] feed the shared
    launch shell's global rail and community sidebar; [can_manage] is the handler's real
    admin-or-moderator check and only picks the sidebar Settings visibility. *)

val community_channel_shell_page : ?user:string -> ?realtime_token:string -> ?noindex:bool -> is_member:bool -> ?can_start:bool -> ?thread_links:(int64 * int * string * bool) list -> ?source_focus:(int * string * int64 list) -> rail_communities:Community_types.community list -> channels:Channel_store.channel list -> sections:Section_store.community_section list -> channel:Channel_store.channel -> messages:(Chat_store.chat_message * string option) list -> community:Community_types.community -> Dream.request -> string
(** Viewer-scoped promoted-thread provenance, decided by the handler: [Ts_visible] renders
    the structured source conversation (channel (slug, name) if it still exists, plus the
    chronological source rows); [Ts_private] renders only a neutral notice with no channel
    name, authors, content, or timestamps. *)
