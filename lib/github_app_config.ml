(* Validated public configuration for the GitHub App installation and
   user-authorization flow. Public surface only: the app slug, OAuth client id
   and the two registered return URLs. Server credentials (App ID, client
   secret, private key) belong to later server-to-server slices and are
   deliberately not read here — rendering or starting an installation must not
   require them.

   Everything below [from_env] is pure. Errors carry the offending field but
   never the supplied value, so they are safe to log verbatim. *)

type t = {
  public_origin : string;
  app_slug : string;
  client_id : string;
  setup_url : string;
  callback_url : string;
}

type field = Public_origin | App_slug | Client_id | Setup_url | Callback_url

type error =
  | Missing of field
  | Invalid of field
  | Origin_mismatch of field
  | Unexpected_path of field

let public_origin_env = "EARDE_PUBLIC_ORIGIN"
let app_slug_env = "GITHUB_APP_SLUG"
let client_id_env = "GITHUB_APP_CLIENT_ID"
let setup_url_env = "GITHUB_APP_SETUP_URL"
let callback_url_env = "GITHUB_APP_CALLBACK_URL"
let setup_path = "/integrations/github/install/return"
let callback_path = "/integrations/github/authorize/callback"

let string_of_field = function
  | Public_origin -> public_origin_env
  | App_slug -> app_slug_env
  | Client_id -> client_id_env
  | Setup_url -> setup_url_env
  | Callback_url -> callback_url_env

let string_of_error = function
  | Missing f -> string_of_field f ^ " is not set"
  | Invalid f -> string_of_field f ^ " is invalid"
  | Origin_mismatch f ->
      string_of_field f ^ " does not match the " ^ public_origin_env
      ^ " origin"
  | Unexpected_path f -> string_of_field f ^ " has an unexpected path"

let public_origin t = t.public_origin
let app_slug t = t.app_slug
let client_id t = t.client_id
let setup_url t = t.setup_url
let callback_url t = t.callback_url

let ( let* ) = Result.bind

(* Unset and set-but-blank are distinct operator mistakes; keep them
   distinguishable in diagnostics. *)
let required field value =
  match value with
  | None -> Error (Missing field)
  | Some raw ->
      let trimmed = String.trim raw in
      if trimmed = "" then Error (Invalid field) else Ok trimmed

(* An HTTP origin, normalized for comparison: lowercase scheme and host, port
   as written (the scheme default is applied only when comparing). *)
type origin = { scheme : string; host : string; port : int option }

let default_port = function "https" -> 443 | _ -> 80
let effective_port o = match o.port with Some p -> p | None -> default_port o.scheme

let same_origin a b =
  String.equal a.scheme b.scheme
  && String.equal a.host b.host
  && effective_port a = effective_port b

let is_loopback_host = function
  | "localhost" | "127.0.0.1" | "::1" -> true
  | _ -> false

(* Canonical form: no trailing slash, no default port, IPv6 hosts
   re-bracketed (Uri.host strips the brackets). *)
let origin_to_string o =
  let host = if String.contains o.host ':' then "[" ^ o.host ^ "]" else o.host in
  let port =
    match o.port with
    | Some p when p <> default_port o.scheme -> ":" ^ string_of_int p
    | Some _ | None -> ""
  in
  o.scheme ^ "://" ^ host ^ port

(* Shared shape for the origin and both registered URLs: absolute http(s),
   a real host, and none of the components an attacker or typo could smuggle
   a redirect through — userinfo, query (verbatim_query also catches a bare
   "?") or fragment. Field-specific rules layer on top. *)
let parse_http_url field raw =
  if String.exists (fun c -> c <= ' ' || c = '\x7f') raw then
    Error (Invalid field)
  else
    let uri = Uri.of_string raw in
    let* scheme =
      match Option.map String.lowercase_ascii (Uri.scheme uri) with
      | Some (("http" | "https") as s) -> Ok s
      | Some _ | None -> Error (Invalid field)
    in
    let* host =
      match Uri.host uri with
      | Some h when h <> "" -> Ok (String.lowercase_ascii h)
      | Some _ | None -> Error (Invalid field)
    in
    let* () =
      match (Uri.userinfo uri, Uri.verbatim_query uri, Uri.fragment uri) with
      | None, None, None -> Ok ()
      | _ -> Error (Invalid field)
    in
    Ok ({ scheme; host; port = Uri.port uri }, Uri.path uri)

(* Plain http is acceptable only on loopback, so a production typo like
   http://earde.com cannot downgrade the OAuth callbacks. *)
let validate_public_origin raw =
  let field = Public_origin in
  let* origin, path = parse_http_url field raw in
  let* () =
    match path with "" | "/" -> Ok () | _ -> Error (Unexpected_path field)
  in
  if String.equal origin.scheme "https" || is_loopback_host origin.host then
    Ok origin
  else Error (Invalid field)

(* A registered URL must sit on our own origin (scheme, host and effective
   port all equal) and use exactly its registered path, so the future OAuth
   redirect_uri can be emitted verbatim and never points off-site. *)
let validate_registered_url field ~public ~path:expected raw =
  let* origin, path = parse_http_url field raw in
  let* () = if same_origin origin public then Ok () else Error (Origin_mismatch field) in
  if String.equal path expected then Ok (origin_to_string public ^ expected)
  else Error (Unexpected_path field)

let validate_app_slug raw =
  let is_edge c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  let is_inner c = is_edge c || c = '-' in
  let len = String.length raw in
  if
    len > 0
    && is_edge raw.[0]
    && is_edge raw.[len - 1]
    && String.for_all is_inner raw
  then Ok raw
  else Error (Invalid App_slug)

(* Opaque GitHub identifier: no prefix or length assumptions, but whitespace
   and control characters can only be paste damage. *)
let validate_client_id raw =
  if String.exists (fun c -> c <= ' ' || c = '\x7f') raw then
    Error (Invalid Client_id)
  else Ok raw

let of_values ~public_origin ~app_slug ~client_id ~setup_url ~callback_url =
  let* raw_origin = required Public_origin public_origin in
  let* public = validate_public_origin raw_origin in
  let* app_slug =
    let* raw = required App_slug app_slug in
    validate_app_slug raw
  in
  let* client_id =
    let* raw = required Client_id client_id in
    validate_client_id raw
  in
  let* setup_url =
    let* raw = required Setup_url setup_url in
    validate_registered_url Setup_url ~public ~path:setup_path raw
  in
  let* callback_url =
    let* raw = required Callback_url callback_url in
    validate_registered_url Callback_url ~public ~path:callback_path raw
  in
  Ok
    {
      public_origin = origin_to_string public;
      app_slug;
      client_id;
      setup_url;
      callback_url;
    }

let from_env () =
  of_values
    ~public_origin:(Sys.getenv_opt public_origin_env)
    ~app_slug:(Sys.getenv_opt app_slug_env)
    ~client_id:(Sys.getenv_opt client_id_env)
    ~setup_url:(Sys.getenv_opt setup_url_env)
    ~callback_url:(Sys.getenv_opt callback_url_env)
