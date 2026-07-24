(** Pure strict parser for the future existing-community home-request form
    (POST /projects/:slug/request-home). No IO: fields arrive already
    decoded by the HTTP/form layer, and nothing here touches a request, a
    session, the database, or a log.

    Recognized grammar — byte-exact names, each required exactly once, in
    any order:

    - [target_community_id]: non-empty ASCII decimal digits parsing to a
      positive OCaml [int] (leading zeroes tolerated; signs, whitespace,
      decimals, hex, separators, zero, and overflow all reject);
    - [request_note]: preserved byte-for-byte, blank included — trimming,
      normalization, UTF-8/control/length rules, and empty-to-None
      collapsing belong exclusively to
      {!Project_home_relation.create_pending}.

    Missing, duplicate, and unknown fields (including case or whitespace
    variants of the recognized names, and a [dream.csrf] field reaching the
    pure parser) reject the whole submission. The project slug is route
    context, never a form field, so no slug accessor exists here.

    Error privacy: the one error is nullary — no field name, value, or
    length can travel through it, and no [string_of_error] or printer
    exists. *)

type t
(** One structurally valid submission. Deliberately abstract, with no
    serializer or printer; only the accessors below are exposed. *)

type error = Invalid_form

val of_fields : (string * string) list -> (t, error) result

val target_community_id : t -> int
(** A local community id as submitted — never a bearer credential. The
    future POST handler must still reauthorize the project and revalidate
    the target through the transactional request store. *)

val request_note : t -> string
(** The exact Dream-decoded submission, possibly blank; canonicalization is
    the domain's job. *)

val create_relation :
  t ->
  (Project_home_relation.t, Project_home_relation.error) result
(** Delegates to {!Project_home_relation.create_pending} with the submitted
    note (blank canonicalizes to [None] there); domain errors pass through
    unchanged. Success proves nothing about the world — stewardship,
    project verification, community eligibility, and active-home uniqueness
    all belong to the transactional request store. *)
