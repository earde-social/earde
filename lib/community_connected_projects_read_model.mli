(** The accepted project-home relations of one community — the data behind
    the "Connected projects" section of the existing community page. Read-only:
    this feature module owns its SQL, performs no network or GitHub IO, no
    logging, takes no locks, and never writes.

    This module decides nothing about who may see the community. The existing
    community route ({!Community_handlers.community_page_handler}) completes its own
    lookup and {!Community_read_gate.can_view_community} authorization first, and only
    then reads here — so the section can never become a side channel for a
    private or draft community. What this module does own is durable
    identity: the supplied slug must still resolve to exactly one community
    row whose stored identity is coherent.

    An accepted home relation is a factual, GitHub-verified connection. It is
    deliberately not an endorsement: it does not mean GitHub or Earde endorses
    the project, that the project's stewards moderate the community, or that
    the community controls the repository. Nothing in this module's vocabulary
    implies otherwise.

    Nothing internal crosses: no relation id, project id, community id,
    requester or reviewer identity, request note, timestamps, steward rows,
    moderator or member data, GitHub installation/account/repository external
    ids, serializers, printers, or [string_of_error]. The closed error variant
    is payload-free — Caqti/PostgreSQL details (which can echo SQL parameters)
    are dropped, never returned or logged. *)

type repository
(** One permanent project repository's public identity. Abstract; the database
    row id, GitHub external repository id, position, description, and default
    branch never cross. *)

type project
(** One project whose accepted [home] relation names the target community.
    Abstract; the project id, steward rows, provenance, and the whole
    relation record never cross. *)

(** The project's current GitHub verification state. All three are returned:
    an accepted home stays part of the community's history while verification
    drifts, and the page says plainly which state it is in rather than quietly
    dropping the project. *)
type verification =
  | Verified
  | Stale
  | Revoked

type error =
  | Invalid_community_slug
      (** The supplied slug is not a single non-empty URL path segment (no
          ASCII whitespace, controls, DEL, or ['/']); rejected before any SQL.
          Route values are never trimmed, lowercased, percent-decoded, or
          repaired. *)
  | Community_unavailable
      (** No community row carries this exact slug any more. Private, draft,
          unlisted, and legacy communities are *not* this error — the caller's
          existing authorization owns whether the page renders at all. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: a non-positive
          community id, a stored slug that is not byte-identical to the
          supplied one, a blank or control-bearing community name, an off-enum
          community visibility or onboarding state, a non-positive relation or
          project id, an off-enum project verification status or kind, a
          non-canonical stored project slug, a blank project name, a
          non-addressable namespace login, a website URL outside the permanent
          project grammar, a duplicate or non-adjacent project group, more than
          one accepted relation for one project, or a missing or malformed
          repository set. Payload-free on purpose; never a partial list. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped, never
          returned or logged: they can echo SQL parameters. *)

val load_for_community :
  (module Caqti_lwt.CONNECTION) ->
  community_slug:string ->
  (project list, error) result Lwt.t
(** The projects whose accepted [home] relation names the community with this
    exact slug.

    Selection is exactly [relation_type = 'home' AND status = 'accepted'] for
    that community: pending, rejected, and removed relations never appear.
    Provenance is deliberately not filtered — an automatically provisioned
    accepted home legitimately has no [requested_by_user_id] and no
    [reviewed_by_user_id], and appears exactly like a moderator-reviewed one.
    The distinction between the two is not exposed. Request notes are not
    loaded.

    The community's own lifecycle is never a filter here: a relation accepted
    while the community was public stays attached if the community later goes
    unlisted, private, draft, or legacy. Whether such a page renders at all is
    the caller's existing authorization decision, not this module's.

    Projects come back in a deterministic order — [lower(name) ASC], then
    [slug ASC], then the internal id — and each project's repositories stay in
    stored position order. Nothing is ordered by stars, GitHub or Earde
    activity, member count, acceptance time, requester, or reviewer.

    Reading is informational: no locks, no transaction beyond the normal
    statement scope, no writes, and no network calls. An empty result is
    [Ok []], never an error. *)

val repository_full_name : repository -> string
val repository_html_url : repository -> string
(** Exactly [https://github.com/<owner>/<name>], revalidated by structured
    reconstruction from the stored full name before being returned. *)

val repository_is_primary : repository -> bool
val repository_is_archived : repository -> bool
val project_name : project -> string

val project_slug : project -> string
(** The canonical persisted project slug. Exposed as identity only — no public
    project route exists yet, so no caller may build one from it. *)

val project_kind : project -> Project_identity.kind
val project_namespace_login : project -> string
val project_verification : project -> verification

val project_website_url : project -> string option
(** The stored project website, byte-for-byte, only when it still satisfies
    the permanent-project website rules {!Project_identity} enforces at
    creation. No new URL grammar is defined here. *)

val project_repositories : project -> repository list
(** The project's permanent repositories in stored position order — at least
    one, at most 2,000, positions exactly [1..n], distinct full names, and the
    primary-count rule for the project kind, all revalidated before the project
    is returned. *)
