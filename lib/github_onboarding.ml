(* Pure GitHub installation / onboarding domain rules. No IO — see the .mli
   for what each rule encodes. *)

type account_type = User | Organization

(* Exact match only, as in Network_communities: these values come from our
   own schema enums, so anything off-enum (padding, case drift) is a bug or
   tampering — never coerced. *)
let account_type_of_string = function
  | "user" -> Ok User
  | "organization" -> Ok Organization
  | s -> Error (Printf.sprintf "unknown github account type: %S" s)

let string_of_account_type = function
  | User -> "user"
  | Organization -> "organization"

type installation_status = Active | Revoked | Inaccessible

let installation_status_of_string = function
  | "active" -> Ok Active
  | "revoked" -> Ok Revoked
  | "inaccessible" -> Ok Inaccessible
  | s -> Error (Printf.sprintf "unknown installation status: %S" s)

let string_of_installation_status = function
  | Active -> "active"
  | Revoked -> "revoked"
  | Inaccessible -> "inaccessible"

let installation_transition_allowed ~from_ ~to_ =
  match (from_, to_) with
  (* Revoked is terminal: a reinstall is a new installation row. *)
  | Revoked, Revoked -> true
  | Revoked, (Active | Inaccessible) -> false
  | (Active | Inaccessible), _ -> true

type flow = Project_onboarding

let flow_of_string = function
  | "project_onboarding" -> Ok Project_onboarding
  | s -> Error (Printf.sprintf "unknown onboarding flow: %S" s)

let string_of_flow = function Project_onboarding -> "project_onboarding"

let revoked_at_allowed ~status ~has_revoked_at =
  match status with
  (* The schema lets a newly marked revoked row still lack revoked_at. *)
  | Revoked -> true
  | Active | Inaccessible -> not has_revoked_at
