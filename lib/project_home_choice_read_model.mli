(** Owner-authorized read model behind the existing-community home choice for
    one permanent verified project — the data for the future steward-only choice
    page. Read-only: this feature module owns its SQL, performs no network IO,
    no logging, takes no locks, and never writes.

    Authorization is decided entirely in SQL: the view is loaded only when the
    supplied Earde user is one of the project's [project_stewards] and the
    project is currently [verified]. Creator, namespace, draft, and installation
    provenance are never consulted.

    The view is either the project's one active [home] relation (status
    [pending] or [accepted]) with its target community's public identity — in
    which case the eligible list is empty, so the page cannot solicit a second
    request — or the deterministic list of currently eligible target
    communities. An active target stays visible even when its lifecycle later
    became ineligible: the relation is historical workflow state.

    Nothing internal crosses: no permanent project id, relation id, requester or
    reviewer, private request note, community creator, membership or moderation
    data, timestamps, GitHub or installation ids, serializers, or printers. The
    closed error variant is payload-free — Caqti/PostgreSQL details (which can
    echo SQL parameters) are dropped, never returned or logged.

    Concurrency: reading takes no locks, so a concurrent request may be created
    after the read completes. The transactional request store remains
    authoritative and returns [Active_home_exists] for the loser. *)

type project
(** The project's public identity for the choice page. Deliberately abstract;
    only the accessors below are exposed. *)

type community
(** One target community's public identity. Abstract; the creator, membership,
    moderation, and counts never cross. *)

type active_relation
(** The project's one active home relation: its workflow status plus the target
    community's identity. The private request note never crosses. *)

type view
(** One authorized load: the project, at most one active relation, and the
    eligible targets (empty whenever an active relation exists). *)

(** Presentation visibility of a target community. There is no database
    [unlisted] value: [Public] means exactly the fully listed published network
    shape ([is_network_community], [onboarding_state = 'published'],
    [visibility = 'public'], [indexable = discoverable = TRUE]) and [Unlisted]
    exactly the fully unlisted one (same lifecycle,
    [indexable = discoverable = FALSE]) — never anything else.

    [Currently_unavailable] appears only on an active relation's target whose
    lifecycle later became individually valid but ineligible
    (legacy/non-network, setup draft, private, or valid combinations of those);
    the reason is deliberately collapsed so no lifecycle detail crosses. It
    never appears in {!eligible_communities}. Unavailability is not corruption:
    an unknown enum value or a mixed [indexable]/[discoverable] pair is
    [Inconsistent_data], never [Currently_unavailable]. *)
type visibility = Public | Unlisted | Currently_unavailable

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_project_slug
      (** The supplied route value does not already have the permanent canonical
          shape (1–80 ASCII characters matching [^[a-z0-9]+(-[a-z0-9]+)*$]);
          rejected before any SQL runs. Route values are never lowercased,
          trimmed, or repaired. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: a malformed stored
          project slug or namespace login, a second active home row past the
          partial unique index, an off-enum status or lifecycle value, a mixed
          indexable/discoverable pair on any returned community row (eligible or
          active target — flags that contradict each other are corruption,
          unlike a lifecycle that merely became ineligible), a non-positive id,
          a blank community name, a non-addressable community slug, or a
          control-unsafe description. Payload-free on purpose; never a partial
          view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped, never
          returned or logged: they can echo SQL parameters. *)

val load_for_steward :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  project_slug:string ->
  (view option, error) result Lwt.t
(** The choice view for the one verified project with this canonical slug, only
    when [user_id] is one of its stewards. Slug, stewardship, and [verified]
    status are authorized together inside the SQL — never loaded and checked
    afterward. [Ok None] deliberately collapses every unavailable state —
    nonexistent slug, another user's project, a creator without stewardship,
    removed stewardship, and a stale or revoked project — so probing slugs
    cannot become an ownership or state oracle.

    Eligible targets are exactly the current durable predicate — network
    community, [onboarding_state = 'published'], [visibility = 'public'] —
    ordered deterministically by [lower(name) ASC, slug ASC, id ASC]. Stewards
    need no membership or moderation in a target. Historical
    [rejected]/[removed] relations never suppress the list. *)

val project : view -> project
val active_relation : view -> active_relation option

val eligible_communities : view -> community list
(** Empty whenever {!active_relation} is [Some _]: the page must not permit
    another request while one is active. *)

val project_name : project -> string

val project_slug : project -> string
(** The canonical persisted slug, byte-identical to the accepted route value. *)

val project_namespace_login : project -> string
(** The verified GitHub namespace login as persisted at finalization time —
    display metadata, never an authorization source. *)

val community_id : community -> int
(** The local community row id — the value the request form submits back; a
    database identifier, never a bearer credential. The future POST handler must
    still revalidate it through the transactional request store. *)

val community_name : community -> string
val community_slug : community -> string
val community_description : community -> string option
val community_visibility : community -> visibility

val active_relation_status : active_relation -> Project_home_relation.status
(** Only {!Project_home_relation.Pending} or {!Project_home_relation.Accepted}
    can appear: anything else on an active row is [Inconsistent_data] at load
    time. *)

val active_relation_community : active_relation -> community
(** The target's public identity, loaded without any eligibility predicate so
    the relation stays visible after later lifecycle drift; a target that became
    ineligible carries {!Currently_unavailable} rather than a false
    [Public]/[Unlisted] label. *)

val active_relation_removal_allowed : active_relation -> bool
(** Whether the transactional {!Project_home_removal_store} could detach this
    home at all, derived from the target's durable lifecycle. It is [false] for
    exactly the unpublished dedicated-community setup draft, whose provisioned
    home is part of the draft's structural integrity and is durably unremovable
    by every authority until the community is published; [true] for a published
    network community and for every ordinary accepted home, including targets
    that later drifted to {!Currently_unavailable} — a home must stay separable
    precisely when its community's lifecycle has drifted.

    Only meaningful for the accepted state; a pending relation carries no
    removal control on any surface. The raw lifecycle never crosses, so a
    suppressed control names no reason. This is presentation permission only: it
    authorizes nothing, and the store independently re-decides every POST — a
    surface that renders no form grants nothing, and one that renders a form
    grants nothing either. *)
