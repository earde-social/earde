open Lwt.Infix

type moderator_entry = {
  user_id : int;
  username : string;
  role : string;
}

(* Promotion failures are classified at this boundary, where each error site
   knows its own provenance: [Promotion_refused] carries a fixed user-facing
   domain message, [Promotion_storage_error] carries driver detail that must
   stay server-side. Callers can then route the two without inspecting
   strings. *)
type promote_error =
  | Promotion_refused of string
  | Promotion_storage_error of string

(* ON CONFLICT DO NOTHING: idempotent — re-promoting the same user is a no-op
   rather than an error; safe for re-runs and concurrent create_community calls. *)
let add_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_moderators (user_id, community_id) VALUES ($1, $2) ON CONFLICT DO NOTHING"

let add_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.exec add_moderator_query (user_id, community_id)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Community creators need top_mod from birth — default role is 'mod', so we
   must set role explicitly. ON CONFLICT UPDATE covers the edge case where the
   user somehow already exists as a plain mod and re-creates. *)
let add_top_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_moderators (user_id, community_id, role) VALUES ($1, $2, 'top_mod') ON CONFLICT (user_id, community_id) DO UPDATE SET role = 'top_mod'"

let add_top_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.exec add_top_moderator_query (user_id, community_id)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* SELECT 1 existence check is cheaper than COUNT — we only need bool, not cardinality. *)
let is_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.int)
  "SELECT 1 FROM community_moderators WHERE user_id = $1 AND community_id = $2"

let is_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.find_opt is_moderator_query (user_id, community_id)
  >>= function
  | Ok (Some _) -> Lwt.return (Ok true)
  | Ok None -> Lwt.return (Ok false)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Point-read by (user_id, community_id): used to gate promote/remove operations
   and to derive is_top_mod without a separate is_moderator round-trip. *)
let get_moderator_role_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
  "SELECT role FROM community_moderators WHERE user_id = $1 AND community_id = $2"

let get_moderator_role (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.find_opt get_moderator_role_query (user_id, community_id)
  >>= function
  | Ok r -> Lwt.return (Ok r)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* ORDER BY promoted_at ASC: original creator appears first, preserving
   appointment history without a separate position/rank column. *)
let get_community_moderators_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t3 int string string))
  "SELECT u.id, u.username, u.email
   FROM users u
   JOIN community_moderators am ON u.id = am.user_id
   WHERE am.community_id = $1
   ORDER BY am.promoted_at ASC"

let get_community_moderators (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_community_moderators_query community_id
  >>= function
  | Ok rows ->
      let users = List.map (fun (id, username, email) -> { User_store.id; username; email }) rows in
      Lwt.return (Ok users)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* CASE ordering: top_mod=1, mod=2, legacy_mod=3 — explicit role hierarchy for the
   manage-mods panel. promoted_at ASC breaks ties preserving appointment seniority. *)
let get_community_mods_with_roles_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t3 int string string))
  "SELECT u.id, u.username, cm.role
   FROM users u
   JOIN community_moderators cm ON u.id = cm.user_id
   WHERE cm.community_id = $1
   ORDER BY CASE cm.role WHEN 'top_mod' THEN 1 WHEN 'mod' THEN 2 ELSE 3 END ASC,
            cm.promoted_at ASC"

let get_community_mods_with_roles (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_community_mods_with_roles_query community_id
  >>= function
  | Ok rows ->
      let entries = List.map (fun (user_id, username, role) -> { user_id; username; role }) rows in
      Lwt.return (Ok entries)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Hard delete: mod removal is administrative; no tombstone needed since
   mod history is not exposed publicly (unlike user content). *)
let remove_moderator_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "DELETE FROM community_moderators WHERE user_id = $1 AND community_id = $2"

let remove_moderator (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.exec remove_moderator_query (user_id, community_id)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* COUNT before INSERT: enforces the "Max 3 Top Mods" business rule at DB read time
   rather than via a UNIQUE constraint, because the limit is per-community cardinal —
   not a uniqueness invariant on a single column. *)
let count_top_mods_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM community_moderators WHERE community_id = $1 AND role = 'top_mod'"

let promote_to_top_mod_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "UPDATE community_moderators SET role = 'top_mod' WHERE user_id = $1 AND community_id = $2"

(* Two-phase guard: first reject illegal source roles, then enforce the 3-seat cap.
   Order matters — rejecting already-top_mods avoids decrementing the cap incorrectly
   when count happens to equal the cap and the target is already counted in it. *)
let promote_to_top_mod (module C : Caqti_lwt.CONNECTION) user_id community_id =
  C.find_opt get_moderator_role_query (user_id, community_id) >>= function
  | Error e -> Lwt.return (Error (Promotion_storage_error (Caqti_error.show e)))
  | Ok None -> Lwt.return (Error (Promotion_refused "User is not a moderator of this community"))
  | Ok (Some "top_mod") -> Lwt.return (Error (Promotion_refused "User is already a Top Mod"))
  | Ok (Some "legacy_mod") -> Lwt.return (Error (Promotion_refused "Cannot promote a legacy moderator; reinstate as mod first"))
  | Ok (Some _) ->
      C.find count_top_mods_query community_id >>= function
      | Error e -> Lwt.return (Error (Promotion_storage_error (Caqti_error.show e)))
      | Ok count ->
          if count >= 3 then Lwt.return (Error (Promotion_refused "Maximum of 3 Top Mods reached for this community"))
          else
            C.exec promote_to_top_mod_query (user_id, community_id) >>= function
            | Ok () -> Lwt.return (Ok ())
            | Error e -> Lwt.return (Error (Promotion_storage_error (Caqti_error.show e)))

(* Single UPDATE across all communities: triggered on community page load to lazily
   enforce inactivity without a background job. UPDATE FROM ... WHERE is standard
   PostgreSQL; the 3-month threshold matches the "squatter prevention" governance spec. *)
let demote_inactive_mods_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
  "UPDATE community_moderators cm
   SET role = 'legacy_mod'
   FROM users u
   WHERE cm.user_id = u.id
     AND cm.role IN ('top_mod', 'mod')
     AND (u.last_active_at IS NULL OR u.last_active_at < NOW() - INTERVAL '3 months')"

let demote_inactive_mods (module C : Caqti_lwt.CONNECTION) =
  C.exec demote_inactive_mods_query ()
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Inverse of get_community_moderators: used for profile badge display.
   ORDER BY promoted_at ASC keeps creation order consistent with mod panels. *)
let get_moderated_communities_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Community_types.community_row_type)
  (* Same fourteen-column contract as get_user_communities_query above, and
     the same latent decode overrun before it was completed. *)
  "SELECT a.id, a.slug, a.name, a.description, a.rules, a.avatar_url, a.banner_url, a.allow_downvotes, a.sections_enabled, a.visibility, a.indexable, a.is_network_community, a.onboarding_state, a.discoverable
   FROM communities a
   JOIN community_moderators am ON a.id = am.community_id
   WHERE am.user_id = $1
   ORDER BY am.promoted_at ASC"

let get_moderated_communities (module C : Caqti_lwt.CONNECTION) user_id =
  C.collect_list get_moderated_communities_query user_id
  >>= function
  | Ok rows -> Lwt.return (Ok (List.map Community_types.map_community_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))
