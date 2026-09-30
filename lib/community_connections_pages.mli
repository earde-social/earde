(** The community-connections management surface: the three-section
    management page and the two-step "Connect a community" flow (search, then
    a confirmation page carrying the optional private note).

    Pure rendering over handler-supplied view models: no Caqti, no read-model
    dependency, no session access, no authority decision of its own. Every
    caller-controlled string is escaped at the template boundary, and a value
    that would carry a non-addressable slug into an action path degrades to
    plain text rather than becoming actionable.

    Language rules: an accepted connection is symmetric, so it is rendered as
    a connected community and never as something granted by, approved by, or
    belonging to a person. No requester, reviewer, or remover username
    appears anywhere on these pages, and there is no relationship label,
    tier, or kind. The private request note renders only inside the
    authorized pending sections, labelled as private, escaped, and never
    parsed as Markdown or HTML. *)

type community = {
  name : string;
  slug : string;
  eligible : bool;
      (** Whether this community may currently create or accept connections.
          An ineligible community still sees its history and keeps its reject
          and remove controls. *)
}

type counterpart = { counterpart_name : string; counterpart_slug : string }

type accepted = { accepted_id : string; accepted_with : counterpart }
(** [accepted_id] is the connection's row id as a decimal string — an opaque
    record locator in an action path, never a grant of authority: the handler
    re-resolves and re-authorizes it server-side. *)

type pending = {
  pending_id : string;
  pending_with : counterpart;
  pending_note : string option;
}

type state = {
  community : community;
  accepted : accepted list;
  incoming : pending list;
  outgoing : pending list;
}

type target = { target_name : string; target_slug : string }

(** What a completed or refused action leaves on the reloaded page. Each is
    one stable outcome; none names a community's visibility, a moderator, or
    an internal lifecycle detail. *)
type feedback =
  | Stale_form
  | Already_connected
  | Review_unavailable
  | Removal_unavailable
  | Target_unavailable
  | Source_ineligible
  | Note_invalid
  | Action_failed

val management_page :
  ?user:string ->
  ?request:Dream.request ->
  ?shell:Community_types.community *
         Community_types.community list * Html.t ->
  state:state -> feedback:feedback option -> unit -> string
(** The management page: connected communities, incoming requests, outgoing
    requests, and the entry point to the search flow. [shell] is the
    Cartographic Civic launch chrome (the durable community record, the
    viewer's rail communities, and the prebuilt sidebar); without it the page
    falls back to the chrome-free launch message document, exactly as the
    sibling review surface does. *)

val target_search_page :
  ?user:string ->
  ?request:Dream.request ->
  ?shell:Community_types.community *
         Community_types.community list * Html.t ->
  community:community ->
  query:string ->
  results:target list ->
  searched:bool -> feedback:feedback option -> unit -> string
(** Step one: the server-rendered target search. [searched] distinguishes "no
    query yet" from "this query matched nothing", so an empty page never
    implies that a community was withheld. No result count is rendered. Each
    result links to the confirmation step; there is deliberately no note
    field on a result row. *)

val confirm_page :
  ?user:string ->
  ?request:Dream.request ->
  ?shell:Community_types.community *
         Community_types.community list * Html.t ->
  community:community ->
  target:target -> note:string -> feedback:feedback option -> unit -> string
(** Step two: the single confirmation form carrying the chosen target and the
    one optional private note, posting to the request route. [note] is
    redisplayed verbatim (escaped) after a refused submission so nothing the
    moderator typed is silently lost. *)
