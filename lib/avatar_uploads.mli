(** Locally stored avatar uploads: strict mapping from a stored avatar_url to
    the one on-disk file the upload pipeline could have written, and its safe
    removal during account deletion.

    The upload pipeline ([Handlers.process_image_upload]) only ever mints
    [/static/uploads/earde_<digits>_<digits>.webp], so only that exact shape
    is accepted here — everything else (external URLs, legacy values, bundled
    [/static/images/...] assets, traversal attempts, encoded separators) maps
    to [None] and never reaches the filesystem. The accepted basename
    alphabet is the fixed prefix/suffix plus digits and underscores, so a
    validated path cannot contain a path separator or a [..] segment by
    construction. *)

(** A new upload basename without the [.webp] suffix:
    [earde_<now_ms>_<32 digits>]. The digits are drawn from [random], which
    must be a cryptographic byte source ([Dream.random] in production): the
    served URL is the only protection of an upload from a private community,
    so it must not be guessable. The result always has the pipeline shape
    {!local_file_of_url} accepts. *)
val fresh_basename : now_ms:int64 -> random:(int -> string) -> string

(** [Some "static/uploads/<basename>"] iff the url is exactly a
    pipeline-shaped upload reference; [None] otherwise. Pure. *)
val local_file_of_url : string -> string option

(** Best-effort removal of a validated upload path. [`Removed] on unlink,
    [`Absent] when the file was already gone (success for cleanup purposes),
    [`Failed] when the file demonstrably still exists after a filesystem
    error. Never raises. *)
val remove_local_file : string -> [ `Removed | `Absent | `Failed ]

(** The account-deletion composition: validate the stored avatar_url and
    remove its local file. Anything that is not a pipeline upload —
    [None], external URLs, unexpected values — is [`Not_local] and touches
    nothing. *)
val cleanup_deleted_account_avatar :
  string option -> [ `Removed | `Absent | `Failed | `Not_local ]
