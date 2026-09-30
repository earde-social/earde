(** Community access control. [Community_private] => server-side read gate (members/mods/admins
    only); [Community_public] => readable by anyone. Distinct from the [indexable] SEO flag.
    Mirrors the DB CHECK on communities.visibility; [_of_string] is partial (None off-enum). *)
type community_visibility = Community_public | Community_private

val community_visibility_to_string : community_visibility -> string

val community_visibility_of_string : string -> community_visibility option

(** Network-community setup lifecycle (storage foundation only — no enforcement yet).
    [Community_draft] = a provisioned community still being configured;
    [Community_published] = live. Mirrors the DB CHECK on communities.onboarding_state.
    [_of_string] returns [Error] for off-enum values — never silently published. *)
type community_onboarding_state =
  | Community_draft
  | Community_published

val string_of_community_onboarding_state : community_onboarding_state -> string

val community_onboarding_state_of_string :
  string -> (community_onboarding_state, string) result

type community = {
  id : int; slug : string; name : string; description : string option;
  rules : string option; avatar_url : string option; banner_url : string option;
  allow_downvotes : bool; sections_enabled : bool;
  visibility : community_visibility; indexable : bool;
  (* Lifecycle foundation: [discoverable] = Earde's own discovery surfaces (distinct
     from external-SEO [indexable]). No query behavior attached yet. *)
  is_network_community : bool;
  onboarding_state : community_onboarding_state;
  discoverable : bool;
}

(** === EFFECTIVE ACCESS / INDEXABILITY RULES (pure; no DB) ===
    Privacy is the stronger property and is evaluated first: a private community is always
    effectively non-indexable, regardless of any [indexable] flag. The handler supplies the
    membership/mod/admin booleans to [can_read_community] from real checks — these perform none. *)
val community_is_private : community_visibility -> bool

val effective_indexable_community : community_visibility -> community_indexable:bool -> bool

val effective_indexable_child :
  community_visibility -> community_indexable:bool -> child_indexable:bool -> bool

val can_read_community :
  community_visibility -> is_member:bool -> is_mod:bool -> is_admin:bool -> bool

(** The 14 SELECT columns every community query returns, in order: (id,
    slug, name, description), (rules, avatar_url, banner_url,
    allow_downvotes), (sections_enabled, visibility, indexable),
    (is_network_community, onboarding_state, discoverable). *)
type community_row =
  (int * string * string * string option)
  * (string option * string option * string option * bool)
  * (bool * string * bool)
  * (bool * string * bool)

val community_row_type : community_row Caqti_type.t

(** Decodes one row. An unrecognized visibility decodes to
    [Community_private] (fail closed); an off-enum onboarding state raises,
    never silently reading as draft or published. *)
val map_community_row : community_row -> community
