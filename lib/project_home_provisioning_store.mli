(** Atomic provisioning of a dedicated community home for a verified project:
    one transaction creates the private network-community setup draft, the
    creating steward's initial membership and top-moderator role, the minimum
    community shell (the default General forum section and the default general
    live channel every community carries), and the immediately accepted
    project-home relation. Either the complete coherent draft commits or nothing
    durable survives — no partial community, membership, moderator, shell row,
    or relation can outlive a failure. No HTTP, session, rendering, GitHub, or
    logging concern lives here.

    Lock discipline is the shared project-first order every sibling home store
    follows: the permanent project row is locked first and is the global
    serialization point, then the current steward authorization row, then any
    active home relation rows. The new community does not exist before this
    transaction inserts it, so no community lock precedes project authorization;
    once inserted, the transaction owns the row. Final arbitration never rests
    on the cooperative locks alone: [communities_slug_key] arbitrates the
    requested slug and the partial unique active-home index arbitrates the
    accepted relation, both through ON CONFLICT DO NOTHING.

    The created community is a private setup draft — network marker set,
    onboarding state draft, visibility private, neither indexable nor
    discoverable — readable under the existing private-community authorization
    only because the actor is both a member and the top moderator. The accepted
    relation carries no fabricated review provenance: requester, reviewer, and
    note are all NULL; only [reviewed_at] records when acceptance happened.
    Nothing here is official, GitHub-approved, or GitHub-endorsed, and no
    moderation role is granted anywhere but in the new community itself.

    Error privacy: every error is payload-free, no SQL diagnostic or constraint
    name travels, nothing is logged, and no [string_of_error] or printer exists.
    Project and stewardship unavailability collapse into one error so the store
    cannot be used to probe projects. *)

type provisioned_home
(** One successfully committed provisioning result. Deliberately abstract and
    deliberately narrow: only the created community's canonical slug and the
    resulting relation status are representable — no database ids, timestamps,
    or serializers. *)

val community_slug : provisioned_home -> string
(** The created community's slug, byte-identical to the canonical slug the
    parsed identity carried in. *)

val resulting_status : provisioned_home -> Project_home_relation.status
(** Always {!Project_home_relation.Accepted} on the only path that constructs a
    value; exposed so callers assert rather than assume. *)

type error =
  | Invalid_user_id  (** Non-positive actor id, rejected before any SQL. *)
  | Invalid_project_slug
      (** The supplied project slug is not already canonical under the permanent
          grammar (1–80 characters, [^[a-z0-9]+(-[a-z0-9]+)*$]). Nothing is
          trimmed, lowercased, or repaired; rejected before any SQL. *)
  | Project_unavailable
      (** Every unavailable or unauthorized project shape: missing, another
          steward's, creator without stewardship, removed stewardship, stale or
          revoked verification, a durable admin who is not a steward.
          Deliberately indistinguishable. *)
  | Community_slug_unavailable
      (** The requested canonical community slug is already taken — whether by a
          legacy community, a network community, or a concurrent winner — as
          arbitrated by [communities_slug_key]. *)
  | Active_home_exists
      (** The project already holds a pending or accepted home relation,
          pre-existing or concurrently committed. Rejected and removed history
          never blocks. *)
  | Inconsistent_data
      (** Malformed durable rows or an impossible internal outcome — corruption
          is reported, never repaired or carried forward. *)
  | Storage_error  (** Unexpected database failure, payload dropped. *)

val provision :
  (module Caqti_lwt.CONNECTION) ->
  actor_user_id:int ->
  project_slug:string ->
  identity:Project_home_provisioning_form.t ->
  (provisioned_home, error) result Lwt.t
(** Provisions the dedicated home inside one explicit transaction on the
    supplied connection. The identity arrives only as the abstract parsed form
    value — the store never accepts, repairs, or re-derives raw identity
    strings, so what was parsed is exactly what is persisted (the new scoped
    database constraints are the final defense).

    Authorization is decided inside the transaction: the project must be
    verified and slug-byte-exact, and the actor must hold a current steward row
    — [created_by_user_id], session admin flags, onboarding provenance, and
    community roles never authorize. On success the transaction has created
    exactly one private draft network community with the parsed identity, the
    actor's membership and [top_mod] role, the default General section and
    general channel, and one accepted home relation with NULL
    requester/reviewer/note and a coherent [reviewed_at]; the complete durable
    result is revalidated with bounded reads before commit, and any mismatch
    rolls everything back as {!Inconsistent_data}. *)
