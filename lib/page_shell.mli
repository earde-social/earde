(** The launch document shells (entry, auth, message, app and onboarding
    documents), their top bar and footer, the shared page scripts, and the
    analytics assets and replay guard. *)

val mobile_gate_css_link : string

val mobile_desktop_gate : string

val analytics_assets :
  ?request:Dream.request ->
  ?analytics_community:int * Community_types.community_visibility ->
  unit -> string * string

val launch_connect_cta : string

val launch_footer : string

val topbar_anon_actions : string

type entry_topbar =
  | Entry_connect_cta
  | Entry_viewer of string option

val launch_entry_page : ?noindex:bool -> ?request:Dream.request -> ?topbar:entry_topbar -> ?desktop_only:bool -> page_class:string -> title:string -> content:string -> unit -> string

val launch_auth_page : ?noindex:bool -> ?request:Dream.request -> page_class:string -> title:string -> content:string -> unit -> string

val launch_message_page : ?noindex:bool -> ?request:Dream.request -> title:string -> content:string -> unit -> string

val launch_tile_color : string -> string

val launch_share_script : string

val launch_behavior_script : string

val launch_app_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Community_types.community list -> ?analytics_community:(int * Community_types.community_visibility) -> ?aside:string -> page_class:string -> title:string -> content:string -> unit -> string

val launch_onboarding_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?stepper:string -> page_class:string -> title:string -> content:string -> unit -> string

val private_replay_guard : community:Community_types.community -> string -> string
