(** Safe HTML fragments for server-side rendering.

    A value of type [t] is markup that is safe to emit where it is placed: every
    piece of text in it was escaped, and every URL passed the policy of its
    context. Code that renders a page builds [t] values and serializes the
    document once, with {!to_string}.

    There are exactly three ways to introduce markup:

    - {!template} and {!static}, whose markup argument must be a string literal
      written in this codebase (a source census enforces it);
    - {!trusted}, the named escape hatch for markup that is not a literal but
      was produced by code of this repository for a documented reason. Every
      call site says why.

    Everything else enters through a constructor that decides escaping by
    construction: {!text} for character data and quoted attribute values, {!int}
    and {!int64} for numbers, {!external_url}, {!internal_path} and {!image_src}
    for URLs by context. None of them returns its input unchanged. Values that
    an inline script needs travel in data attributes as text; no user data is
    ever written into script source. *)

type t

val empty : t

val text : string -> t
(** Escapes the five characters ampersand, less-than, greater-than, double quote
    and apostrophe, so the result is safe as element content and as a single- or
    double-quoted attribute value. It does not make a value safe as a URL,
    inside a script, or as an unquoted attribute. *)

val int : int -> t
val int64 : int64 -> t

val external_url : string -> t
(** A user-supplied link target: [http://] or [https://] only (case
    insensitive), escaped for a quoted attribute. Anything else, including
    [javascript:] and [data:], renders as the inert ["#"]. *)

val external_url_opt : string -> t option
(** {!external_url}, with [None] where it would render ["#"]. *)

val internal_path : string -> t
(** A rooted path inside this site: ["/"] itself, or a value starting with a
    single slash not followed by a slash or a backslash, so it can never become
    a protocol-relative or backslash-normalized off-site URL. Escaped for a
    quoted attribute; anything else renders as ["#"]. *)

val internal_path_opt : string -> t option
(** {!internal_path}, with [None] where it would render ["#"]. *)

val image_src : string -> t
(** An image source: a rooted local path (uploads) or an http(s) URL, after
    trimming. A candidate containing a quote, angle bracket, backtick,
    whitespace or control character is refused rather than escaped, as are
    [javascript:], [data:] and protocol-relative values: they render as ["#"].
*)

val image_src_opt : string -> t option
(** {!image_src}, with [None] where it would render ["#"]. *)

val template : string -> t list -> t
(** [template markup holes] substitutes [holes], in order, for the occurrences
    of ["%s"] in [markup]; every other character of [markup], ["%"] included, is
    copied as is. [markup] must be a string literal. Raises [Invalid_argument]
    when the number of holes differs from the length of [holes], which is a
    programming error. *)

val static : string -> t
(** A literal piece of markup with no holes. The argument must be a string
    literal. *)

val trusted : string -> t
(** Markup that is not a literal but is safe for a reason the call site states:
    a script or stylesheet body shipped with the application, or markup produced
    by an escaping renderer outside this module. The name is deliberately
    searchable; new uses need a comment. *)

val concat : t list -> t

val join : t -> t list -> t
(** [join sep items] puts [sep] between consecutive items. *)

module Infix : sig
  val ( ++ ) : t -> t -> t
  (** Concatenation. *)
end

val is_empty : t -> bool

val to_string : t -> string
(** The serialized markup, for the response body or an attribute value built by
    another [t]. *)
