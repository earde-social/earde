(** The launch document shells (entry, auth, message, app and onboarding
    documents), their top bar and footer, the shared page scripts, and the
    analytics assets and replay guard. Every [launch_*_page] returns the
    serialized document; everything that goes into one is {!Html.t}. *)

val mobile_gate_css_link : Html.t

val mobile_desktop_gate : Html.t

val analytics_assets :
  ?request:Dream.request ->
  ?analytics_community:int * Community_types.community_visibility ->
  unit -> Html.t * Html.t
(** The analytics script and the consent banner, or nothing when analytics
    is not enabled with a valid public configuration. *)

val launch_connect_cta : Html.t

val launch_footer : Html.t

val topbar_anon_actions : Html.t

type entry_topbar =
  | Entry_connect_cta
  | Entry_viewer of string option

val launch_entry_page :
  ?noindex:bool -> ?request:Dream.request -> ?topbar:entry_topbar ->
  ?desktop_only:bool -> page_class:string -> title:string -> content:Html.t ->
  unit -> string

val launch_auth_page :
  ?noindex:bool -> ?request:Dream.request -> page_class:string ->
  title:string -> content:Html.t -> unit -> string

val launch_message_page :
  ?noindex:bool -> ?request:Dream.request -> title:string -> content:Html.t ->
  unit -> string

val launch_tile_color : string -> string
(** A stable palette colour for a community slug, as a CSS colour value. *)

val launch_share_script : Html.t

val launch_behavior_script : Html.t

val launch_app_page :
  ?noindex:bool -> ?request:Dream.request -> ?user:string ->
  ?rail_communities:Community_types.community list ->
  ?analytics_community:int * Community_types.community_visibility ->
  ?aside:Html.t -> page_class:string -> title:string -> content:Html.t ->
  unit -> string

val launch_onboarding_page :
  ?noindex:bool -> ?request:Dream.request -> ?user:string ->
  ?stepper:Html.t -> page_class:string -> title:string -> content:Html.t ->
  unit -> string

val private_replay_guard : community:Community_types.community -> Html.t -> Html.t
(** Wraps a private community's content in the session-replay exclusion
    class; other content is returned unchanged. *)
