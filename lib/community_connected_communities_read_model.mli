(** The publicly visible accepted connections of one community — the data behind
    the "Connected communities" block of the existing community pages.
    Read-only: this feature module owns its SQL, performs no network IO, no
    logging, takes no locks, and never writes.

    {b Public visibility is decided here, and it is decided twice.} Unlike
    {!Community_connected_projects_read_model}, whose selection is purely the
    accepted relation, this module applies
    {!Community_connections.connection_eligible} — public, published,
    discoverable — at read time to {i both} sides:

    - to the community being viewed, so an ineligible community publishes no
      connections at all and the caller receives [Ok []];
    - to each counterpart, so a counterpart that has since gone private,
      unpublished, or undiscoverable is simply absent.

    Nothing is deleted or mutated to achieve that. The accepted row stays
    exactly as it is: it remains visible on both communities' authorized
    management surfaces, disappears from the public block while either side is
    ineligible, and reappears if eligibility returns. The predicate is the same
    one the creation path enforces, so the two readings cannot drift.

    Reading is symmetric. An accepted connection belongs to both communities
    equally, so the same durable row is returned from either side with the other
    community as the counterpart.

    This module decides nothing about who may see the {i viewed} community — the
    existing community route completes its own lookup and
    {!Community_read_gate.can_view_community} decision first, and only then
    reads here, so the block can never become a side channel for a page the
    viewer was not already entitled to. What this module does own is durable
    identity: the supplied slug must still resolve to exactly one community row
    whose stored identity is coherent.

    Nothing internal crosses: no connection id, community id, status, request
    note, direction, requester/reviewer/remover identity, timestamps, audit
    rows, moderator or member data, serializers, printers, or [string_of_error].
    The closed error variant is payload-free — Caqti and PostgreSQL details
    (which can echo SQL parameters) are dropped, never returned or logged. *)

type connected_community
(** One publicly visible counterpart's public identity. Abstract; the row id,
    visibility, onboarding state, discovery flag, and every property of the
    connection itself never cross. *)

type error =
  | Invalid_community_slug
      (** The supplied slug is not a single non-empty URL path segment (no ASCII
          whitespace, controls, DEL, or ['/']); rejected before any SQL. Route
          values are never trimmed, lowercased, percent-decoded, or repaired. *)
  | Community_unavailable
      (** No community row carries this exact slug any more. An ineligible
          community is {i not} this error — it is [Ok []]; whether the page
          renders at all is the caller's existing authorization decision. *)
  | Inconsistent_data
      (** A durable row violated a structural invariant: a non-positive
          community id, a stored slug that is not byte-identical to the supplied
          one, a counterpart slug outside the single-path-segment grammar, a
          blank or control-bearing community name, an off-enum visibility or
          onboarding state, a counterpart equal to the viewed community, or the
          same counterpart returned twice. Payload-free on purpose; never a
          partial list. *)
  | Storage_error
      (** Any Caqti/PostgreSQL failure. Raw database errors are dropped, never
          returned or logged: they can echo SQL parameters. *)

val load_for_community :
  (module Caqti_lwt.CONNECTION) ->
  community_slug:string ->
  (connected_community list, error) result Lwt.t
(** The publicly visible counterparts of the community with this exact slug.

    Selection is exactly [status = 'accepted'] for that community from either
    side, then the eligibility filter described above. Pending, rejected, and
    removed rows never appear, in any state of either community. Request notes
    are not loaded, and no query here can distinguish which side requested.

    Counterparts come back in a deterministic order — [lower(name) ASC], then
    [slug ASC], then the internal id — never by connection age, direction,
    member count, or activity.

    Reading is informational: no locks, no transaction beyond the normal
    statement scope, no writes, and no network calls. An empty result — no
    accepted connection, an ineligible viewer, or every counterpart currently
    ineligible — is [Ok []], never an error. *)

val community_name : connected_community -> string

val community_slug : connected_community -> string
(** The stored slug, revalidated as a single URL path segment before the row is
    returned, so callers may build [/c/<slug>] from it directly. *)
