(** Atomic publication of one provisioned network-community setup draft — the
    transactional store behind the future POST [/c/:slug/publish], which this
    slice deliberately does not register. In a single explicit database
    transaction the draft's identity (name, final canonical slug, optional
    description) and its lifecycle (published, public, and either fully listed
    or fully unlisted) are updated together, so no partial publication or
    partial identity change can ever survive: on any failure the community
    remains byte-identical under its original slug as a private draft, with its
    accepted project-home relation, membership, moderation roles, General
    section, general channel, and all content untouched.

    Route context versus form content: [current_community_slug] is the slug the
    route authorized and must address the draft as it exists now; the requested
    final identity comes only from the already-parsed abstract
    {!Network_community_publication_form.t}, read through its public accessors —
    never from parallel raw strings. The final lifecycle comes only from the
    pure {!Network_communities.publish} transition over the locked draft state;
    nothing here re-derives visibility, onboarding state, indexability, or
    discoverability.

    Locking protocol: after bounded non-locking candidate resolution (the route
    is keyed by the community slug, but every project-home mutation store locks
    project-first), rows are locked in the shared global order — permanent
    project, community, the actor's target-community top_mod row, the actor's
    durable users row, then the exact accepted provisioned home relation —
    always all of them, in full, even after one authorization source qualifies,
    so no sibling store can deadlock against this one.

    Authorization is durable-only: a current [top_mod] of the target community
    or [users.is_admin = TRUE]. Session claims, ordinary membership,
    ['mod']/['legacy_mod'] roles, project stewardship, and creation provenance
    never authorize, and which authority was present (or absent) is never
    revealed — every unauthorized or no-longer-publishable state collapses into
    {!Draft_unavailable}.

    Error privacy: all errors are nullary; no slug, identity byte, constraint
    name, SQLSTATE, or database diagnostic travels through them, no
    [string_of_error] or printer exists, and nothing is logged. *)

type published_community
(** One committed publication: the final canonical community slug and the
    selected exposure, nothing else. Deliberately abstract with no serializer or
    printer; only the two accessors below exist. A value is only ever
    constructed after the transaction has committed, so it can never represent a
    private, draft, mixed, or failed state. *)

val community_slug : published_community -> string
(** The final canonical slug the community now lives under — the parsed form
    value, byte-identical, whether or not it changed. *)

val publication_visibility :
  published_community ->
  Network_community_publication_form.publication_visibility
(** The selected exposure as parsed: [Public] (indexable and discoverable) or
    [Unlisted] (reachable by direct URL only). *)

type error =
  | Invalid_user_id  (** Non-positive actor id; rejected before any SQL. *)
  | Invalid_community_slug
      (** The supplied current route slug is not a single non-empty URL path
          segment (whitespace, control bytes, DEL, or '/'); rejected before any
          SQL. Nothing is trimmed, decoded, or repaired. *)
  | Draft_unavailable
      (** The draft cannot be published by this actor right now: missing,
          renamed, legacy, already published, authorization absent or
          concurrently revoked, or the accepted home relation was removed first.
          One indistinguishable answer for all of them. *)
  | Community_slug_unavailable
      (** Only a confirmed conflict: another community owns the requested final
          canonical slug ([communities_slug_key] is the arbiter). The losing
          draft is left byte-identical under its original slug. *)
  | Inconsistent_data
      (** Malformed or contradictory durable state, or an impossible pure-domain
          outcome, discovered before or after the update. *)
  | Storage_error  (** Unexpected database failure, payload-free. *)

val publish :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  current_community_slug:string ->
  publication:Network_community_publication_form.t ->
  (published_community, error) result Lwt.t
(** Atomically publishes the draft addressed by [current_community_slug] with
    the identity and exposure carried by [publication].

    Success requires, under the locks described above: a permanent project in
    any known verification state (verified, stale, and revoked all publish —
    losing verification never strands a draft), the community in exactly the
    private-draft network lifecycle with a canonical identity, durable
    publication authority, the exact provisioned accepted home relation (NULL
    requester/reviewer/note, reviewed and not removed), and a complete draft
    shell (a member, a top moderator, the canonical General section and active
    general channel, exactly one active home). The one guarded UPDATE writes
    identity and lifecycle simultaneously and is verified afterwards: the
    returned row byte-equals the parsed identity and the pure publication
    configuration, exactly one community owns the final slug, and bounded
    re-reads prove membership, moderation, shell, relation, project, and content
    untouched. Only then does the transaction commit.

    A unique-key failure on the slug change is classified structurally
    (savepoint, rollback to it, then a bounded probe for the row owning the
    requested slug) as {!Community_slug_unavailable}; every unclassified
    database failure is {!Storage_error}. Publishing under an unchanged slug
    never needs conflict handling. *)
