(* Community access control. A closed variant keeps raw strings out of the public API and dynamic
   SQL; the DB CHECK constraint (communities_visibility_check) mirrors it exactly. *_of_string is
   partial (None off-enum) so a stray value fails at the boundary instead of corrupting a row —
   see test_earde.ml for the round-trip + rejection coverage. This is ACCESS control, distinct
   from the [indexable] SEO flag below. *)
type community_visibility = Community_public | Community_private

let community_visibility_to_string = function
  | Community_public -> "public"
  | Community_private -> "private"

let community_visibility_of_string = function
  | "public" -> Some Community_public
  | "private" -> Some Community_private
  | _ -> None

(* Network-community setup lifecycle (storage foundation only — no enforcement yet).
   Draft = a provisioned community still being configured; Published = live. Closed
   variant mirrors the DB CHECK (communities_onboarding_state_check) exactly.
   _of_string returns an explicit Error for off-enum values — a corrupt state must
   surface at the boundary, never silently read as published. *)
type community_onboarding_state =
  | Community_draft
  | Community_published

let string_of_community_onboarding_state = function
  | Community_draft -> "draft"
  | Community_published -> "published"

let community_onboarding_state_of_string = function
  | "draft" -> Ok Community_draft
  | "published" -> Ok Community_published
  | s -> Error (Printf.sprintf "unknown community onboarding state: %S" s)

(* === Effective access/indexability rules (PURE — no DB, no IO) ===
   Privacy is the strictly stronger property and is evaluated first: a private community is
   always effectively non-indexable, regardless of any [indexable] flag. These rules are
   unit-tested in test_earde.ml. *)

let community_is_private = function
  | Community_private -> true
  | Community_public -> false

(* Whether a community's own surfaces (home/feed/search exposure) may be indexed. *)
let effective_indexable_community visibility ~community_indexable =
  match visibility with
  | Community_private -> false
  | Community_public -> community_indexable

(* Whether a child surface (a channel archive or a forum section + its threads) may be indexed.
   Private kills it; otherwise it needs BOTH the community and the child to opt in. *)
let effective_indexable_child visibility ~community_indexable ~child_indexable =
  match visibility with
  | Community_private -> false
  | Community_public -> community_indexable && child_indexable

(* Pure read predicate: may this viewer READ the community at all? Public is readable by anyone
   (incl. logged-out / non-member); private is readable only by a member, moderator, or admin.
   The handler supplies the booleans from real DB/session checks — this helper performs none. *)
let can_read_community visibility ~is_member ~is_mod ~is_admin =
  match visibility with
  | Community_public -> true
  | Community_private -> is_member || is_mod || is_admin

type community = {
  id : int;
  slug : string;
  name : string;
  description : string option;
  rules : string option;
  avatar_url : string option;
  banner_url : string option;
  allow_downvotes : bool;
  sections_enabled : bool;
  visibility : community_visibility;
  indexable : bool;
  (* Network-community lifecycle foundation. discoverable = may appear in Earde's own
     discovery surfaces (distinct from [indexable], which is external SEO). None of
     these drive query behavior yet — enforcement lands in later slices. *)
  is_network_community : bool;
  onboarding_state : community_onboarding_state;
  discoverable : bool;
}

(* 14-column community row: t4(t4, t4, t3, t3) stays within Caqti's per-tuple arity limit.
   visibility and onboarding_state arrive as raw TEXT and decode through their closed variants. *)
type community_row =
  (int * string * string * string option)
  * (string option * string option * string option * bool)
  * (bool * string * bool)
  * (bool * string * bool)

let community_row_type : community_row Caqti_type.t =
  let open Caqti_type in
  t4 (t4 int string string (option string)) (t4 (option string) (option string) (option string) bool) (t3 bool string bool) (t3 bool string bool)

(* DB-decode boundary (private): an off-enum onboarding_state raises rather than
   decoding to ANY valid lifecycle state — a corrupt value must fail loudly, not be
   read as draft or published. The DB CHECK constraint makes this unreachable in
   normal operation. *)
let community_onboarding_state_of_string_exn s =
  match community_onboarding_state_of_string s with
  | Ok v -> v
  | Error msg -> failwith msg

let map_community_row ((id, slug, name, description), (rules, avatar_url, banner_url, allow_downvotes), (sections_enabled, visibility_s, indexable), (is_network_community, onboarding_state_s, discoverable)) =
  (* Fail closed: an unrecognized visibility decodes to Community_private, never public —
     this is a privacy field, so a corrupt/unexpected value must err toward hiding, not
     exposing; the DB CHECK constraint makes the fallback unreachable in normal operation.
     onboarding_state has no such fallback: an off-enum value raises (see above). *)
  { id; slug; name; description; rules; avatar_url; banner_url; allow_downvotes; sections_enabled;
    visibility = Option.value (community_visibility_of_string visibility_s) ~default:Community_private; indexable;
    is_network_community;
    onboarding_state = community_onboarding_state_of_string_exn onboarding_state_s;
    discoverable }
