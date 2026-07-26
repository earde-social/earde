(** Pure strict parser for the final setup submission of a provisioned
    network community — the review-and-publish form behind the future
    POST [/c/:slug/publish], which this slice deliberately does not register.
    Fields arrive already decoded by the HTTP/form layer; this module never
    touches a request, a session, or the database, and never logs form
    values.

    The community identity half is not re-specified here. It is delegated
    byte-for-byte to {!Project_home_provisioning_form}, the frozen canonical
    community-identity policy the provisioning store already persists and the
    scoped [communities_network_*] database CHECKs already enforce, so a
    community created through provisioning and then edited here can never
    drift into a second, subtly different grammar. The only value this module
    owns is the publication choice.

    The current community slug is route context and must never be a form
    field: a browser-supplied current slug would bypass the route-level
    authorization boundary. The [community_slug] field below is the
    {i requested} canonical slug the publisher wants the community to keep or
    move to — never the identity the route authorized.

    Anti-oracle: structural rejections are all the same payload-free
    {!Invalid_form}, so a probing client cannot learn which field was
    missing, duplicated, or unknown. The semantic errors name only the field,
    never the value, its length, or the rule that failed. No error carries a
    payload and no [string_of_error] or printer exists. *)

(** How the publisher chose to expose the community. Exactly the two shapes
    network-community publication supports — there is deliberately no
    [Private] constructor, because a published network community is always
    reachable ([visibility = 'public']); [Unlisted] only withholds indexing
    and discovery. Private rooms and restricted sections remain available
    inside either choice. *)
type publication_visibility =
  | Public
  | Unlisted

type t
(** One accepted, canonical publication submission: the community identity to
    persist plus the chosen exposure. Deliberately abstract, with no
    serializer or printer; only the accessors below are exposed.

    A successful value guarantees local canonical validity only. It proves
    nothing about the world: the requested slug may already be taken, the
    community may have been published or removed, the publisher's top-mod
    role may have been revoked, the accepted home relation may be gone. The
    future atomic publication transaction must revalidate all of that. *)

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
  | Invalid_publication_visibility

val of_fields : (string * string) list -> (t, error) result
(** Parses exactly the four application fields [community_name],
    [community_slug], [community_description], and [publication_visibility] —
    each required exactly once, matched byte-exactly with no case folding or
    trimming, in any order. The complete field structure is decided before
    any semantic rule runs, so a submission that is both structurally
    malformed and semantically invalid always answers {!Invalid_form} and
    never names a field.

    Semantics are then validated in the fixed order name, slug, description,
    publication visibility, and the first failure is returned:

    - the three identity fields are handed verbatim to
      {!Project_home_provisioning_form.of_fields}, whose canonicalization
      (ASCII outer trim, UTF-8 validity, control/DEL rejection, the 120- and
      2,000-scalar limits, the already-canonical
      [^\[a-z0-9\]+(-\[a-z0-9\]+)*$] slug within 80 characters, CR/LF
      normalization, and the empty-description collapse to [None]) applies
      unchanged. Its three semantic errors map one-to-one onto the matching
      constructors above. Nothing is trimmed, lowercased, transliterated,
      repaired, derived, or suffixed here.

    - [publication_visibility] is matched exactly against ["public"] and
      ["unlisted"] through {!Network_communities.publication_mode_of_string},
      the frozen lifecycle grammar. Nothing is trimmed or case-folded, so
      ["Public"], [" public"], [""], and every unknown or future value are
      rejected. ["private"] is rejected like any other unknown value: a
      network community has no fully private published shape, so there is no
      constructor to map it to. *)

val community_name : t -> string
(** The canonical trimmed name, bytes otherwise exactly as submitted. *)

val community_slug : t -> string
(** The canonical requested slug, byte-identical to the submitted value. *)

val community_description : t -> string option
(** The canonical description: LF line endings, outer-trimmed, never
    [Some ""]. *)

val publication_visibility : t -> publication_visibility
