(** Pure domain types for the GitHub installation and onboarding-state
    schema. Closed variants and transition rules only — no IO, no
    persistence, and deliberately no token/state/crypto values: those
    boundaries are designed with the cryptographic state lifecycle, not
    here. Error strings are stable and never carry secrets. *)

(** The GitHub account kind an installation is attached to. *)
type account_type =
  | User
  | Organization

val account_type_of_string : string -> (account_type, string) result
(** Exact-match parse of ["user"] / ["organization"]. No trimming or case
    folding — anything else (blank, padded, differently cased, unknown) is
    an [Error]. *)

val string_of_account_type : account_type -> string

(** Whether Earde can still act through an installation. [Inaccessible]
    covers temporary loss of access (e.g. suspension); [Revoked] is
    terminal. *)
type installation_status =
  | Active
  | Revoked
  | Inaccessible

val installation_status_of_string :
  string -> (installation_status, string) result
(** Exact-match parse of ["active"] / ["revoked"] / ["inaccessible"], same
    rules as {!account_type_of_string}. *)

val string_of_installation_status : installation_status -> string

val installation_transition_allowed :
  from_:installation_status -> to_:installation_status -> bool
(** [Active] and [Inaccessible] may move to any status (including
    recovering [Inaccessible] -> [Active]). [Revoked] is terminal: a later
    GitHub reinstall is a new installation row, never a reactivation. Every
    self-transition is allowed. *)

(** Which onboarding flow a pending state belongs to. *)
type flow =
  | Project_onboarding

val flow_of_string : string -> (flow, string) result
(** Exact-match parse of ["project_onboarding"], same rules as
    {!account_type_of_string}. No generic fallback. *)

val string_of_flow : flow -> string

val revoked_at_allowed :
  status:installation_status -> has_revoked_at:bool -> bool
(** Mirrors the schema contract on [revoked_at]: forbidden on non-revoked
    statuses, while a [Revoked] row may have it or (when newly marked) still
    lack it. Pure — no timestamps or clock logic here. *)
