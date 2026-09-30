(** Configuration model for the future GitHub onboarding feature.

    Only the closed mode and its parser live here; no route or handler reads it
    yet. Parsing fails closed to [Off] and never logs the raw value. *)

type mode = Off | Admins | Public

val mode_of_string : string option -> mode
(** Parse the configuration value. [None], empty/whitespace-only, and unknown or
    differently-cased values are [Off]; surrounding whitespace around the exact
    canonical values "off" / "admins" / "public" is accepted. *)

val mode_from_env : unit -> mode
(** Read EARDE_GITHUB_ONBOARDING_ENABLED and delegate to [mode_of_string]. Not
    cached; callers decide caching later. *)

val mode_to_string : mode -> string
(** Canonical lowercase representation of a mode. *)

val onboarding_available : mode -> is_admin:bool -> bool
(** Whether GitHub onboarding is available: [Off] for no one, [Admins] for
    global admins only, [Public] for everyone. *)

val can_use_legacy_community_creation : is_admin:bool -> bool
(** Whether the legacy generic community-creation flow (GET /new-community, POST
    /communities) is available: global admins only. Independent of [mode] —
    [Public] onboarding never reopens arbitrary community creation. *)

type legacy_creation_decision =
  | Show_form  (** Proceed into the existing creation flow unchanged. *)
  | Redirect_to_bring  (** GET by a non-admin (or anonymous) visitor. *)
  | Forbid  (** POST by a non-admin (or anonymous) visitor: controlled 403. *)

val legacy_creation_get_decision : is_admin:bool -> legacy_creation_decision
(** [Show_form] for global admins, [Redirect_to_bring] otherwise. *)

val legacy_creation_post_decision : is_admin:bool -> legacy_creation_decision
(** [Show_form] for global admins, [Forbid] otherwise. *)
