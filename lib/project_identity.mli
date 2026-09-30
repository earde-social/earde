(** Pure, validated, canonical project identity for the GitHub-pivot creation
    flow. The future strict form module parses field names and calls {!create};
    the future atomic finalization store consumes the resulting value. No SQL,
    HTTP, rendering, session access, or logging happens here.

    A successful value guarantees only local canonical validity: the kind is
    closed, the name/description/website satisfy the database constraints, the
    slug is canonical lowercase ASCII and unreserved, the construction context
    held 1–2,000 unique positive snapshot ids, and the primary rule for the kind
    holds. It proves nothing about the world: the draft may be gone, ownership
    may have changed, the selection may have been replaced, the slug may be
    taken, repositories may be claimed elsewhere. The finalization transaction
    must re-read and revalidate all of that while holding the draft lock.

    Error privacy: every error is payload-free — no rejected value, length, or
    malformed byte can leave through the error channel, and no [string_of_error]
    or printer exists. *)

type kind =
  | Project
  | Organization
  | Ecosystem
  | Foundation
  | Working_group
  | Other

val kind_of_string : string -> kind option
(** Accepts exactly the six canonical database spellings — [project],
    [organization], [ecosystem], [foundation], [working_group], [other] — and
    nothing else: no aliases, capitalization variants, spaces, or hyphenated
    alternatives. *)

val string_of_kind : kind -> string
(** The exact database value; round-trips with {!kind_of_string}. *)

type t
(** A validated identity. Deliberately abstract, with no serializer or printer;
    only the accessors below are exposed. The selected snapshot ids are
    constructor context only and are never retained — the finalization store
    re-reads the authoritative selection under the draft lock. *)

type error =
  | Invalid_repository_selection
  | No_repositories_selected
  | Invalid_name
  | Invalid_slug
  | Reserved_slug
  | Invalid_description
  | Invalid_website_url
  | Invalid_primary_repository
  | Primary_repository_required

val create :
  kind:kind ->
  name:string ->
  slug:string ->
  description:string option ->
  website_url:string option ->
  selected_snapshot_ids:int64 list ->
  primary_snapshot_id:int64 option ->
  (t, error) result
(** Validates in a fixed order — selected-context structure, non-empty
    selection, name, slug, description, website, primary rule — and returns the
    first failure.

    Canonicalization: user-visible text is outer-trimmed over ASCII whitespace
    only (space, tab, CR, LF, FF, VT) with no Unicode normalization and no
    internal rewriting. The name keeps its exact bytes (1–120 Unicode scalars,
    single-line). The slug is additionally ASCII-lowercased and must match
    [^[a-z0-9]+(-[a-z0-9]+)*$] within 80 characters; the canonical value [new]
    is reserved (it is the one exact [/projects/new] routing conflict). The
    description has CRLF and lone CR normalized to LF, collapses to [None] when
    empty after trimming, and otherwise allows up to 2,000 scalars with LF and
    tab as the only permitted control bytes. The website collapses to [None]
    when empty after trimming, and otherwise must be an absolute http(s) URL
    (scheme case-insensitive) with a non-empty host, no userinfo, no whitespace
    or control bytes, within 2,048 scalars; fragments are accepted, and the
    accepted value is preserved byte-for-byte — never re-serialized by the URI
    parser.

    Selected context: 1–2,000 unique positive [int64] ids, order preserved and
    unretained. A supplied primary must be positive and among them; kind
    [Project] requires an explicit primary (never inferred, even from a
    one-repository selection), while every other kind accepts zero or one. *)

val kind : t -> kind

val name : t -> string
(** Canonical trimmed name, bytes otherwise untouched. *)

val slug : t -> string
(** Canonical lowercase-ASCII slug. *)

val description : t -> string option
(** Canonical description: LF line endings, outer-trimmed, never [Some ""]. *)

val website_url : t -> string option
(** The accepted URL, byte-for-byte as supplied between the trimmed edges. *)

val primary_snapshot_id : t -> int64 option
(** Present for kind [Project]; optional otherwise. Membership in the current
    draft selection must be revalidated by the finalization store. *)
