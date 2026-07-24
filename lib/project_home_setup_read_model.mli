(** Owner-authorized read model over one permanent verified open-source
    project, keyed by its canonical slug — the data behind the permanent
    [GET /projects/:slug/setup] destination. Read-only: this feature module
    owns its SQL, performs no network IO, no logging, and never writes.

    Authorization is decided entirely in SQL: the project is loaded only
    when the supplied Earde user is one of its [project_stewards] and the
    project is currently [verified]. [created_by_user_id],
    [source_onboarding_draft_id], and
    [github_installations.connected_by_user_id] are provenance and are
    never consulted — steward rows are the only permanent authorization
    source.

    Nothing credential-shaped or provenance-shaped can pass through here:
    no tokens of any kind, draft ids, creator or steward user ids, local
    installation record ids, GitHub account or repository ids, timestamps,
    or raw rows are exposed, and the closed error variant is payload-free —
    Caqti/PostgreSQL details (which can echo SQL parameters) are
    deliberately dropped, never returned or logged. *)

type project
(** One permanent verified project in full: identity, verified namespace,
    and the complete permanent repository list, read in one statement so
    they can never disagree. Deliberately abstract, with no serializer or
    printer; only the accessors below are exposed. *)

type repository
(** One validated permanent repository row. Deliberately abstract; only the
    display accessors below are retained — the GitHub repository id never
    crosses. *)

val project_id : project -> int64
(** The local project row id — a database identifier, never a bearer
    credential, and never rendered by the permanent setup page. *)

val name : project -> string
val slug : project -> string
(** The canonical persisted slug, byte-identical to the accepted route
    value. *)

val description : project -> string option
val website_url : project -> string option
val kind : project -> Project_identity.kind

val namespace_login : project -> string
(** The verified GitHub namespace login as persisted at finalization time —
    display metadata, never an authorization source. *)

val namespace_type :
  project ->
  Github_user_installations.account_type

val repositories : project -> repository list
(** The complete permanent repository set, ordered by [position ASC] with
    positions contiguous from 1. Never empty. *)

val position : repository -> int
(** Display order only, contiguous from 1 within one project. *)

val full_name : repository -> string
val html_url : repository -> string
(** Exactly [https://github.com/<owner>/<name>], revalidated by structured
    reconstruction before being returned. *)

val repository_description : repository -> string option
(** Named apart from the project accessor because one OCaml signature
    cannot hold two values called [description]. *)

val default_branch : repository -> string
(** Persisted byte-for-byte; may contain ['/']. Any consumer that embeds it
    in a URL or command must encode it at that boundary. *)

val is_primary : repository -> bool
val is_archived : repository -> bool

type error =
  | Invalid_user_id  (** [user_id <= 0]; rejected before any SQL runs. *)
  | Invalid_slug
      (** The supplied route value does not already have the permanent
          canonical shape (1–80 ASCII characters matching
          [^[a-z0-9]+(-[a-z0-9]+)*$]); rejected before any SQL runs. Route
          values are never lowercased, trimmed, or repaired. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant the finalization
          store promises: repository counts, contiguous positions,
          canonical names or URLs, byte validity, primary rules,
          duplicates, an off-enum kind or namespace type, or a blank
          namespace login. Payload-free on purpose; never a partial
          view. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped,
          never returned or logged: they can echo SQL parameters. *)

val load_for_steward :
  (module Caqti_lwt.CONNECTION) ->
  user_id:int ->
  slug:string ->
  (project option, error) result Lwt.t
(** The one verified project with this canonical slug, only when [user_id]
    is one of its stewards. Slug, stewardship, and [verified] status are
    authorized together inside the SQL — never loaded by slug and checked
    afterward — and project plus repositories come from one joined
    statement, so the view is one PostgreSQL snapshot.

    [Ok None] deliberately collapses every unavailable state — nonexistent
    slug, another user's project, a project this user does not steward,
    and a stale or revoked project — so probing slugs cannot become an
    ownership or state oracle. The single exception: a corrupted
    zero-repository project that IS stewarded by [user_id] and otherwise
    verified returns [Error Inconsistent_data] (never an empty view); any
    other user still receives [Ok None] for that same slug. *)
