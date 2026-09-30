(* Server-only secret credential for the GitHub App OAuth code-for-token
   exchange. Kept apart from Github_app_config so public browser-facing
   configuration and secret material never share a module.

   Everything below [from_env] is pure. Errors carry the offending field but
   never the supplied value or anything derived from it, so they are safe to
   log verbatim. *)

type t = { client_secret : string }
type field = Client_secret
type error = Missing of field | Invalid of field

let client_secret_env = "GITHUB_APP_CLIENT_SECRET"
let string_of_field = function Client_secret -> client_secret_env

let string_of_error = function
  | Missing f -> string_of_field f ^ " is not set"
  | Invalid f -> string_of_field f ^ " is invalid"

let client_secret t = t.client_secret

(* Opaque GitHub credential: no prefix, length or alphabet is imposed, but
   whitespace and control bytes (including DEL) can only be paste damage.
   The value is never trimmed or repaired — a damaged secret is rejected so
   the operator fixes the source rather than shipping a silently altered
   credential. *)
let of_values ~client_secret =
  match client_secret with
  | None -> Error (Missing Client_secret)
  | Some raw ->
      if raw = "" || String.exists (fun c -> c <= ' ' || c = '\x7f') raw then
        Error (Invalid Client_secret)
      else Ok { client_secret = raw }

let from_env () = of_values ~client_secret:(Sys.getenv_opt client_secret_env)
