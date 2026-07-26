(** Owner-authorized read model behind the dedicated-community-home creation
    entry for one permanent verified project — the data for the steward-only
    GET [/projects/:slug/community-home/new]. Read-only: this feature module
    owns its SQL, performs no network IO, no logging, takes no locks, and
    never writes.

    Authorization is decided entirely in SQL: the view is loaded only when
    the supplied Earde user is one of the project's [project_stewards] and
    the project is currently [verified]. Creator, onboarding-draft
    provenance, installation connector, community membership, and session
    admin are never consulted — matching the sibling permanent-project setup
    and home-choice read models exactly, neither of which grants a durable
    global admin an owner-flow bypass.

    The same statement also excludes any project that already has an active
    [home] relation (status [pending] or [accepted]): a project may not start
    creating a dedicated home while a request is outstanding or a home is
    connected.

    Nothing internal crosses: no permanent project id, source draft id,
    steward id, installation id, GitHub account or repository id, active
    relation id, timestamps, serializers, or printers. The closed error
    variant is payload-free — Caqti/PostgreSQL details (which can echo SQL
    parameters) are dropped, never returned or logged.

    Concurrency: reading takes no locks, so an active relation may be created
    after the read completes, and the suggested slug may be taken between
    this GET and the eventual POST. Nothing here queries for slug
    availability: a GET-time answer would be a race the user could only lose
    later, and the future provisioning store is authoritative for both. *)

type project
(** The project's public identity for the creation page. Deliberately
    abstract; only the accessors below are exposed. *)

type view
(** One authorized load: the project plus the suggested initial community
    identity derived from it. *)

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_project_slug
      (** The supplied route value does not already have the permanent
          canonical shape (1–80 ASCII characters matching
          [^[a-z0-9]+(-[a-z0-9]+)*$]); rejected before any SQL runs. Route
          values are never lowercased, trimmed, or repaired. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: a non-positive id, a
          stored slug that is malformed or differs from the supplied
          canonical one, a verification status other than [verified], a name
          that is blank, untrimmed, over-long, control-bearing, or not valid
          UTF-8, an off-enum project kind, a blank or non-addressable
          namespace login, or a description that is blank, over-long,
          control-bearing, or not valid UTF-8. Payload-free on purpose; never
          a partial view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped,
          never returned or logged: they can echo SQL parameters. *)

val load_for_steward :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  project_slug:string ->
  (view option, error) result Lwt.t
(** The creation view for the one verified project with this canonical slug,
    only when [user_id] is one of its stewards and the project has no active
    home relation. Slug, stewardship, [verified] status, and the
    active-relation exclusion are decided together inside the SQL — never
    loaded and checked afterward.

    [Ok None] deliberately collapses every unavailable state — nonexistent
    slug, another user's project, a creator without stewardship, removed
    stewardship, a stale or revoked project, an outstanding pending request,
    an accepted community home, and an active relation created concurrently
    and observed by this read — so probing slugs cannot become an ownership
    or state oracle. Historical [rejected] and [removed] relations never
    suppress the view. *)

val project : view -> project

val project_name : project -> string

val project_slug : project -> string
(** The canonical persisted slug, byte-identical to the accepted route
    value. *)

val project_description : project -> string option

val project_kind : project -> Project_identity.kind

val project_namespace_login : project -> string
(** The verified GitHub namespace login as persisted at finalization time —
    display metadata, never an authorization source. *)

val suggested_community_name : view -> string
(** The project's validated name. It already satisfies the creation form's
    name policy, so the page can prefill it unchanged. *)

val suggested_community_slug : view -> string
(** The project's canonical slug when it also satisfies the community
    creation-slug policy, and [""] otherwise — never a repaired, derived, or
    suffixed value. Today the two grammars coincide
    ([^[a-z0-9]+(-[a-z0-9]+)*$] within 80 characters, nothing reserved), so
    the empty answer is defensive rather than reachable through the durable
    model; it exists so tightening the community grammar later degrades to
    "the steward types a slug" instead of to a wrong suggestion.

    Availability is deliberately not consulted: the slug may be taken by the
    time the form is submitted, and only the provisioning transaction can
    answer that without racing. *)

val suggested_community_description : view -> string option
(** The project's validated description, unchanged. *)
