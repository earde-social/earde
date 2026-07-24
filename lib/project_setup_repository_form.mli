(** Pure parser for the future repository-selection form
    ([POST /projects/new/repositories]). It consumes fields already decoded
    by the HTTP/form layer (the established Dream form API) — never a raw
    body, and never percent-decoding of its own.

    The grammar is closed: exactly one [draft_id] plus zero to 2,000
    [repository] fields, every value a strict positive decimal [int64], no
    unknown fields, no duplicate repository ids. The submit button carries
    no [name], so it never enters the field set.

    Error privacy: every malformed input collapses to the payload-free
    [Invalid_form] — which field failed, the supplied value, duplication,
    and overflow stay indistinguishable, and nothing here logs form
    values. *)

type t
(** A validated submission. Deliberately abstract, with no serializer or
    printer; only the accessors below are exposed. *)

type error = Invalid_form

val of_fields : (string * string) list -> (t, error) result

val draft_id : t -> int64
(** The submitted draft row id. Not a bearer credential — the later handler
    must re-authorize it against the current user through the owning
    draft. *)

val selected_snapshot_ids : t -> int64 list
(** The submitted local snapshot ids in exact form order — never sorted,
    never deduplicated (duplicates reject the whole form instead). Local
    snapshot ids are used deliberately: a GitHub refresh recreates the
    snapshot rows, so a stale browser form names ids that no longer exist
    and can be rejected safely. May be empty — an empty selection is a
    valid submission. *)
