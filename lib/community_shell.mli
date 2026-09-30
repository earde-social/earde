(** The community document shell and its top bar. *)

val launch_community_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Community_types.community list -> ?head_extra:string -> community:Community_types.community -> sidebar:string -> page_class:string -> title:string -> content:string -> unit -> string

val launch_community_surface_page : ?noindex:bool -> ?request:Dream.request -> ?user:string -> ?rail_communities:Community_types.community list -> ?head_extra:string -> ?aside:string -> community:Community_types.community -> sidebar:string -> page_class:string -> title:string -> main_el:string -> unit -> string
