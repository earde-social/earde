(** Pure lifecycle invariants for GitHub-provisioned network communities.

    This module owns only the domain rules — publication modes, the
    visibility/indexing configuration each mode implies, and the state
    invariants that keep a draft from leaking and a published community from
    going fully private. No IO, no persistence; handlers wire these rules to the
    database separately. Legacy (non-network) communities are deliberately
    outside its enforcement. *)

(** How a network community is exposed once published. [Unlisted] is not a
    privacy state: the community stays [Community_types.Community_public] and
    reachable by direct URL — it is only kept out of external indexing and
    Earde's own discovery surfaces. *)
type publication_mode = Public | Unlisted

val publication_mode_of_string : string -> (publication_mode, string) result
(** Exact-match parse of ["public"] / ["unlisted"]. No trimming or case folding
    — anything else (blank, padded, differently cased, unknown) is an [Error].
*)

val string_of_publication_mode : publication_mode -> string

type publication_configuration = {
  visibility : Community_types.community_visibility;
  indexable : bool;
  discoverable : bool;
  onboarding_state : Community_types.community_onboarding_state;
}
(** The database fields a publication writes, as one closed record so a mode can
    never be applied partially. *)

val configuration_for_publication :
  publication_mode -> publication_configuration
(** [Public] => public + indexable + discoverable; [Unlisted] => public but
    neither indexable nor discoverable. Both land in [Community_published]. *)

type publication_error = Not_a_network_community | Community_already_published

val string_of_publication_error : publication_error -> string
(** Stable, non-sensitive messages for tests and future handler rendering. *)

val publish :
  is_network_community:bool ->
  onboarding_state:Community_types.community_onboarding_state ->
  publication_mode ->
  (publication_configuration, publication_error) result
(** Pure publication decision: only a network community still in
    [Community_draft] may publish, yielding the exact configuration for the
    selected mode. Nothing is written here. *)

val visibility_change_allowed :
  is_network_community:bool ->
  onboarding_state:Community_types.community_onboarding_state ->
  requested_visibility:Community_types.community_visibility ->
  bool
(** The one lifecycle invariant on visibility changes: a published network
    community cannot become fully private. Legacy communities and setup drafts
    (which may stay private while configured) are unrestricted here — ordinary
    authorization remains elsewhere. *)

val lifecycle_state_valid :
  is_network_community:bool ->
  onboarding_state:Community_types.community_onboarding_state ->
  visibility:Community_types.community_visibility ->
  indexable:bool ->
  discoverable:bool ->
  bool
(** Whole-state validity. Legacy communities always pass. A network draft must
    be private and neither indexable nor discoverable (a draft must not leak
    through indexing or discovery). A published network community must be public
    in exactly one of the two supported shapes: fully listed (indexable +
    discoverable) or fully unlisted (neither) — mixed combinations are invalid.
*)
