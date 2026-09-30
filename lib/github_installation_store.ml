(* Persistence of verified GitHub App installations. The only GitHub-derived
   input is the abstract proof value from Github_user_installations, whose
   constructor already validated the identity fields, so nothing is trimmed,
   repaired, or re-checked here. One atomic upsert carries the whole
   contract — no read-before-write — and no token of any kind can reach this
   module; see the .mli for the full privacy contract. *)

open Lwt.Infix

type error =
  | Invalid_connected_by_user_id
  | Installation_unavailable
  | Storage_error

(* Pure precheck, before any SQL: installations are only ever recorded on
   behalf of an authenticated user with a real id. *)
let valid_connected_by_user_id user_id = user_id > 0

(* The two closed account-type variants map one-to-one; going through
   Github_onboarding keeps the canonical database strings defined in exactly
   one place, and never derives them from arbitrary input. *)
let domain_account_type = function
  | Github_user_installations.User -> Github_onboarding.User
  | Github_user_installations.Organization -> Github_onboarding.Organization

(* One atomic upsert: a fresh installation id inserts an active row; a
   conflicting id re-activates and login-refreshes the existing row only
   when it is non-revoked and carries the same account identity. The
   conflict arm deliberately never touches github_account_id,
   github_account_type, connected_by_user_id, created_at, or revoked_at —
   the installation belongs to the GitHub account, so provenance stays with
   whoever first connected it (including a NULL left by a deleted Earde
   user). A revoked row is terminal and an identity mismatch is never
   repaired, so both leave the row untouched and yield zero rows, which the
   caller cannot tell apart. github_installation_id is UNIQUE, so at most
   one row can ever match and concurrent writers serialize on the conflict
   arbitration rather than on any application lock. *)
let record_verified_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t4 int64 int64 string string) int) ->? Caqti_type.bool)
    "INSERT INTO github_installations (github_installation_id, \
     github_account_id, github_account_login, github_account_type, \
     connected_by_user_id, status) VALUES ($1, $2, $3, $4, $5, 'active') ON \
     CONFLICT (github_installation_id) DO UPDATE SET github_account_login = \
     EXCLUDED.github_account_login, status = 'active', updated_at = NOW() \
     WHERE github_installations.status <> 'revoked' AND \
     github_installations.github_account_id = EXCLUDED.github_account_id AND \
     github_installations.github_account_type = EXCLUDED.github_account_type \
     RETURNING TRUE"

let record_verified (module C : Caqti_lwt.CONNECTION) ~connected_by_user_id
    verified =
  if not (valid_connected_by_user_id connected_by_user_id) then
    Lwt.return (Error Invalid_connected_by_user_id)
  else
    let account_type =
      Github_onboarding.string_of_account_type
        (domain_account_type (Github_user_installations.account_type verified))
    in
    C.find_opt record_verified_query
      ( ( Github_user_installations.installation_id verified,
          Github_user_installations.account_id verified,
          Github_user_installations.account_login verified,
          account_type ),
        connected_by_user_id )
    >>= function
    | Ok (Some _) -> Lwt.return (Ok ())
    | Ok None ->
        (* Zero rows means the conflict arm refused the existing row —
           revoked, or a different account identity behind the same
           installation id. Both collapse into one payload-free error. *)
        Lwt.return (Error Installation_unavailable)
    | Error _ ->
        (* Caqti error payloads can echo SQL parameters; dropping them keeps
           installation identity out of callers and logs. *)
        Lwt.return (Error Storage_error)
