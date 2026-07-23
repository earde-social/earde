(* Configuration model for the future GitHub onboarding feature.

   This slice is deliberately just the closed mode and its parser: no routes,
   handlers, or persistence read it yet. Parsing fails closed — anything that
   is not an exact canonical value (after trimming surrounding whitespace)
   means Off, and the raw environment value is never logged so a typo'd or
   accidentally-secret value cannot leak. *)

type mode =
  | Off
  | Admins
  | Public

let env_var = "EARDE_GITHUB_ONBOARDING_ENABLED"

let mode_of_string raw =
  match raw with
  | None -> Off
  | Some raw -> (
      (* Trim only; canonical values are exact and lowercase. Arbitrary values
         are not normalized, so "PUBLIC" or "Admin" fail closed to Off. *)
      match String.trim raw with
      | "off" -> Off
      | "admins" -> Admins
      | "public" -> Public
      | _ -> Off)

(* Not cached: callers decide their own caching later, and re-reading keeps
   deployment/configuration tests predictable. *)
let mode_from_env () = mode_of_string (Sys.getenv_opt env_var)

let mode_to_string = function
  | Off -> "off"
  | Admins -> "admins"
  | Public -> "public"

let onboarding_available mode ~is_admin =
  match mode with
  | Off -> false
  | Admins -> is_admin
  | Public -> true
