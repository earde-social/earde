(** Pure policy for the image-upload pipeline: which byte payloads are accepted,
    and the exact argument vector ImageMagick is invoked with.

    Everything here is a total function over strings — no IO, no process
    execution, no database — so the whole accept/reject decision and the command
    construction are unit-tested without ImageMagick installed and without a
    hostile payload ever reaching a decoder. The Lwt side (writing the temporary
    file, running the process, moving the result into [static/uploads]) lives in
    [Image_processing.process_image_upload], which calls into this module for
    every decision. *)

(** The surface an upload is for. Only the resize geometry differs; the accepted
    formats, the byte cap and the resource limits are identical everywhere, so
    no surface can be a weaker way in. *)
type purpose =
  | Profile_avatar
  | Community_avatar
  | Community_banner
  | Post_image

(** The formats the upload UI promises ("PNG or JPG", [accept='image/*']), plus
    the two other lossless/animated web formats the previous pipeline already
    accepted in practice. Deliberately no SVG: it is a markup document, and
    serving one back would be a stored-XSS vector even though the pipeline
    re-encodes. *)
type format = Jpeg | Png | Gif | Webp

val max_bytes : int
(** Hard cap on the submitted byte length (5 MiB), unchanged from the previous
    pipeline. Checked before anything is written to disk. *)

val too_large_message : string
(** The user-facing message for a payload over [max_bytes]. Byte-identical to
    the message the previous pipeline produced, so existing copy is unchanged.
*)

val rejected_message : string
(** The user-facing message for a payload that is not a supported image.
    Deliberately identical for "not an image at all", "unsupported format", and
    "decoder failed": the uploader learns which formats are accepted and nothing
    about why a specific payload was refused. *)

val rate_limited_message : string
(** The user-facing message when the upload bucket is exhausted. *)

val detect_format : string -> format option
(** The accept decision, made from the payload's own leading bytes — never from
    the multipart [Content-Type], never from the submitted filename, both of
    which are attacker-controlled. Returns [None] for an empty payload, a
    truncated header, and every unsupported or non-image format. Recognises
    exactly: JPEG ([FF D8 FF]), PNG (the 8-byte signature), GIF
    ([GIF87a]/[GIF89a]) and WebP ([RIFF]…[WEBP]). *)

val coder : format -> string
(** The explicit ImageMagick coder prefix for a detected format, e.g. ["PNG:"].
    Prefixing the input path with it pins the decoder to the format the magic
    bytes proved, so ImageMagick never sniffs content and can never be steered
    into the [URL:], [MSL:], [MSVG:], [EPHEMERAL:], [PS:] or [PDF:] coders — the
    delegate families behind the ImageTragick class of bugs — by a crafted
    payload. *)

val resize_geometry : purpose -> string
(** The ImageMagick geometry for a surface, e.g. ["512x512>"]. The ['>'] suffix
    means shrink-only. Compile-time constants, never user input. *)

val resource_limits : string list
(** The [-limit] pairs applied to every invocation, bounding memory, on-disk map
    and cache, pixel area, per-image width and height, and wall-clock decode
    time. Sized for the 5 MiB byte cap: a payload that only decodes into
    something enormous fails inside these limits instead of consuming the host.
    Not a substitute for the deployment's [policy.xml], which is the only place
    delegate coders can be disabled globally. *)

val convert_argv :
  binary:string ->
  format:format ->
  purpose:purpose ->
  input:string ->
  output:string ->
  string array
(** The complete argv for one conversion — passed to [Lwt_process] as an
    argument vector, so there is no shell, no word splitting, and no
    metacharacter in any path or geometry can affect execution. Input and output
    are both coder-qualified ([PNG:in], [webp:out]) so neither end is resolved
    by content sniffing or by file extension. *)
