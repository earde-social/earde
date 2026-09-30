(** Pure strict parser for the future project-identity form ([POST /projects]).
    It consumes fields already decoded by the HTTP/form layer (the established
    Dream form API) — never a raw body, and never percent-decoding of its own.

    The grammar is closed: exactly one occurrence of each recognized field —
    [draft_id], [kind], [name], [slug], [description], [website_url],
    [primary_snapshot_id] — byte-exact names, in any order. Missing, duplicate,
    unknown, case-variant, and whitespace-variant field names all reject; the
    installed [Dream.form] strips [dream.csrf] before application parsing, so it
    is not recognized or filtered here.

    Layering: this parser validates only field grammar and scalar identifiers.
    Text-field content rules — trimming, UTF-8 validity, lengths, URL structure,
    reserved slugs, empty-to-[None] collapsing — belong exclusively to
    {!Project_identity.create} and are not duplicated here.

    Error privacy: every malformed input collapses to the payload-free
    [Invalid_form] — which field failed, the supplied value, duplication, and
    overflow stay indistinguishable, and nothing here logs form values. *)

type t
(** A structurally valid submission. Deliberately abstract, with no serializer
    or printer; only the accessors below are exposed. *)

type error = Invalid_form

val of_fields : (string * string) list -> (t, error) result

val draft_id : t -> int64
(** The submitted draft row id: strict positive decimal [int64] (leading zeroes
    accepted, never converted through OCaml [int]). Not a bearer credential —
    the later handler must re-authorize it against the current user through the
    owning draft. *)

val kind : t -> Project_identity.kind
(** Parsed via {!Project_identity.kind_of_string}: exactly the six canonical
    spellings, nothing else. *)

val name : t -> string
(** The submitted name, byte-exact. This and the other string accessors exist so
    a later handler can re-render a failed submission without reparsing the
    body; they are submitted application values and must never be logged. *)

val slug : t -> string
(** The submitted slug, byte-exact — not canonicalized here. *)

val description : t -> string
(** The submitted description, byte-exact; may be [""]. *)

val website_url : t -> string
(** The submitted website value, byte-exact; may be [""]. *)

val primary_snapshot_id : t -> int64 option
(** [None] iff the field was submitted with an explicitly blank value; otherwise
    a strict positive decimal [int64]. Membership in the current selection is
    checked by {!create_identity}, not here. *)

val create_identity :
  t ->
  selected_snapshot_ids:int64 list ->
  (Project_identity.t, Project_identity.error) result
(** Calls {!Project_identity.create} with the parsed fields and the supplied
    authoritative selected snapshot ids (derived server-side from the
    owner-authorized draft read model — never from the browser). Empty
    description and website strings travel as [Some ""] for the domain to
    collapse to [None]. Domain errors return exactly as produced; the selected
    ids are call context only and are never retained in [t] — the finalization
    store still re-reads the current selection under the draft lock. *)
