(** The one Cartographic Civic community-settings shell: shared header band,
    grouped internal settings navigation, and panel wrapper for every
    authorized settings/management surface. Pure rendering — no session,
    SQL, or authority decisions; callers pass the authority booleans their
    route already proved, and every mutation route keeps reauthorizing. *)

(** Closed vocabulary of internal settings destinations; exactly one is
    active per rendered surface. *)
type item =
  | Setup_publish
  | Profile
  | Visibility
  | Connected_projects
  | Home_requests
  | Connections
  | Shared_threads
  | Channels
  | Members
  | Manage_moderators
  | Moderation
  | Bans

(** Whether the "Complete setup and publish" affordance is worth showing: an
    unpublished network setup draft, an authorized (top-mod/admin) viewer and
    a canonical slug. The setup surface independently reauthorizes. *)
val can_complete_setup : community:Db.community -> authorized:bool -> bool

(** The shared settings header band (community identity + the canonical
    back-to-community link). [slug] is the raw canonical slug; escaping is
    internal. *)
val header : slug:string -> string

(** The grouped internal settings navigation (Community / Network /
    Structure / People). [network_manager] is the surface's own
    top-mod/admin reading and gates the Network group and Manage moderators;
    groups without visible entries render nothing. *)
val nav :
  slug:string ->
  active:item ->
  can_complete_setup:bool ->
  network_manager:bool ->
  unit ->
  string

(** Header + nav + panel column, wrapped in the cm-wrap--settings scope the
    shared shell CSS keys on. *)
val wrap :
  slug:string ->
  active:item ->
  can_complete_setup:bool ->
  network_manager:bool ->
  panel:string ->
  unit ->
  string
