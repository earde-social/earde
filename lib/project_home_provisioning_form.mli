(** Pure strict parser for the initial community identity submitted when a
    project steward creates a dedicated community home
    (POST /projects/:slug/community-home, not registered yet). Fields arrive
    already decoded by the HTTP/form layer; this module never touches a
    request, a session, or the database, and never logs form values.

    Unlike the sibling request-home parser, this one owns the full semantic
    policy for the three values it returns: the future atomic provisioning
    store must never have to invent, repair, or guess a durable community
    identity, so a value that reaches the store is already exactly what will
    be persisted. The project slug is route context and must never be a form
    field — a browser-supplied slug would bypass the route-level
    authorization boundary.

    Anti-oracle: structural rejections are all the same payload-free
    {!Invalid_form}, so a probing client cannot learn which field was
    missing, duplicated, or unknown. The three semantic errors name only the
    field, never the value, its length, or the rule that failed. No error
    carries a payload and no [string_of_error] or printer exists. *)

type t
(** One accepted, canonical initial community identity. Deliberately
    abstract, with no serializer or printer; only the accessors below are
    exposed.

    A successful value guarantees local canonical validity only. It proves
    nothing about the world: the slug may already be taken, the project may
    have gained an active home relation, stewardship may have been revoked.
    The future provisioning transaction must revalidate all of that. *)

type error =
  | Invalid_form
      (** Structural failure: a missing, duplicated, unknown, case-variant,
          or whitespace-padded field name — including a [dream.csrf] field
          reaching this parser, which Dream strips before application
          parsing, so its presence is a wiring bug rather than user input.
          Deliberately does not say which field. *)
  | Invalid_community_name
  | Invalid_community_slug
  | Invalid_community_description

val of_fields : (string * string) list -> (t, error) result
(** Parses exactly the three application fields [community_name],
    [community_slug], and [community_description] — each required exactly
    once, matched byte-exactly with no case folding or trimming, in any
    order — then validates them in the fixed order name, slug, description
    and returns the first failure.

    Canonicalization mirrors {!Project_identity}, the strongest canonical
    identity policy in production, since legacy community creation
    canonicalizes nothing beyond a trim:

    - [community_name] is outer-trimmed over ASCII whitespace (space, tab,
      CR, LF, FF, VT) and must then be non-empty, valid UTF-8, free of every
      ASCII control byte (including NUL) and DEL, and at most 120 Unicode
      scalars. The accepted bytes are otherwise untouched: no lowercasing,
      no Unicode normalization, no truncation.

    - [community_slug] must already be canonical. It is matched against
      [^[a-z0-9]+(-[a-z0-9]+)*$] within 80 characters exactly as submitted:
      nothing is trimmed, lowercased, transliterated, repaired, derived, or
      suffixed, so an aliased spelling is rejected rather than silently
      rewritten. No slug is reserved — no static [/c/<value>] route exists
      today, so current community creation reserves nothing either. Durable
      uniqueness is not checked here; the future provisioning store
      arbitrates that race.

    - [community_description] has CRLF and lone CR normalized to LF, is
      outer-trimmed over ASCII whitespace, and collapses to [None] when the
      result is empty — so a structurally required but blank field is
      accepted as "no description". A non-empty description must be valid
      UTF-8, at most 2,000 Unicode scalars, and free of every ASCII control
      byte and DEL except LF and tab, which survive as content. Nothing is
      truncated, and no Markdown or HTML processing happens here. *)

val community_name : t -> string
(** The canonical trimmed name, bytes otherwise exactly as submitted. *)

val community_slug : t -> string
(** The canonical slug, byte-identical to the submitted value. *)

val community_description : t -> string option
(** The canonical description: LF line endings, outer-trimmed, never
    [Some ""]. *)
