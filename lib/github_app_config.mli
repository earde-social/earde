(** Validated public configuration for the GitHub App installation and
    user-authorization flow.

    Covers only the public surface needed to render and start installation: the
    public origin, app slug, OAuth client id and the two registered return URLs.
    Server credentials (GITHUB_APP_ID, GITHUB_APP_CLIENT_SECRET,
    GITHUB_APP_PRIVATE_KEY_PATH) are deliberately not read here.

    Validation guarantees that both registered URLs sit on the configured public
    origin with their exact registered paths and carry no userinfo, query or
    fragment — so OAuth callbacks can only target our own origin and the future
    redirect_uri can be emitted exactly as stored. *)

type t

val public_origin : t -> string
(** Canonical origin, e.g. ["https://earde.com"] — lowercase scheme and host, no
    trailing slash, no default port. *)

val app_slug : t -> string
val client_id : t -> string

val setup_url : t -> string
(** Canonical absolute URL on the public origin with path
    [/integrations/github/install/return]; no query or fragment. *)

val callback_url : t -> string
(** Canonical absolute URL on the public origin with path
    [/integrations/github/authorize/callback]; no query or fragment. *)

type field = Public_origin | App_slug | Client_id | Setup_url | Callback_url

type error =
  | Missing of field  (** the variable is not set at all *)
  | Invalid of field  (** blank, malformed, or violates the field's rules *)
  | Origin_mismatch of field
      (** a registered URL is not on the public origin *)
  | Unexpected_path of field
      (** a URL's path differs from the registered one *)

val string_of_field : field -> string
(** The environment variable name for the field. *)

val string_of_error : error -> string
(** Stable diagnostic naming the field and reason only — never the supplied
    value. *)

val of_values :
  public_origin:string option ->
  app_slug:string option ->
  client_id:string option ->
  setup_url:string option ->
  callback_url:string option ->
  (t, error) result
(** Pure validation of raw values. Surrounding whitespace is trimmed before
    validation; stored values are canonical. *)

val from_env : unit -> (t, error) result
(** Reads EARDE_PUBLIC_ORIGIN, GITHUB_APP_SLUG, GITHUB_APP_CLIENT_ID,
    GITHUB_APP_SETUP_URL and GITHUB_APP_CALLBACK_URL, then delegates to
    {!of_values}. No caching, no logging. *)
