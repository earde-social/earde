(** The community document shell and its top bar. Both builders return the
    serialized document. *)

val launch_community_page :
  ?noindex:bool -> ?request:Dream.request -> ?user:string ->
  ?rail_communities:Community_types.community list -> ?head_extra:Html.t ->
  community:Community_types.community -> sidebar:Html.t -> page_class:string ->
  title:string -> content:Html.t -> unit -> string

val launch_community_surface_page :
  ?noindex:bool -> ?request:Dream.request -> ?user:string ->
  ?rail_communities:Community_types.community list -> ?head_extra:Html.t ->
  ?aside:Html.t -> community:Community_types.community -> sidebar:Html.t ->
  page_class:string -> title:string -> main_el:Html.t -> unit -> string
