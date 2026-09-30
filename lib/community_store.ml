open Lwt.Infix

let get_all_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->* Community_types.community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, \
     allow_downvotes, sections_enabled, visibility, indexable, \
     is_network_community, onboarding_state, discoverable FROM communities"

let get_all_communities (module C : Caqti_lwt.CONNECTION) =
  Query_timer.with_query_timer ~name:"get_all_communities" (fun () ->
      C.collect_list get_all_query () >>= function
      | Ok rows ->
          Lwt.return (Ok (List.map Community_types.map_community_row rows))
      | Error err -> Lwt.return (Error (Caqti_error.show err)))

let create_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string string (option string)) bool) ->. Caqti_type.unit)
    "INSERT INTO communities (name, slug, description, sections_enabled) \
     VALUES ($1, $2, $3, $4)"

let create_community (module C : Caqti_lwt.CONNECTION) name slug description
    sections_enabled =
  C.exec create_community_query ((name, slug, description), sections_enabled)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_slug_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Community_types.community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, \
     allow_downvotes, sections_enabled, visibility, indexable, \
     is_network_community, onboarding_state, discoverable FROM communities \
     WHERE slug = $1"

let get_community_by_slug (module C : Caqti_lwt.CONNECTION) slug =
  C.find_opt get_by_slug_query slug >>= function
  | Ok (Some row) ->
      Lwt.return (Ok (Some (Community_types.map_community_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Community_types.community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, \
     allow_downvotes, sections_enabled, visibility, indexable, \
     is_network_community, onboarding_state, discoverable FROM communities \
     WHERE id = $1"

let get_community_by_id (module C : Caqti_lwt.CONNECTION) id =
  C.find_opt get_by_id_query id >>= function
  | Ok (Some row) ->
      Lwt.return (Ok (Some (Community_types.map_community_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let search_communities_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t3 string int int) ->* Community_types.community_row_type)
    "SELECT id, slug, name, description, rules, avatar_url, banner_url, \
     allow_downvotes, sections_enabled, visibility, indexable, \
     is_network_community, onboarding_state, discoverable FROM communities \
     WHERE (name ILIKE $1 OR description ILIKE $1) AND visibility = 'public' \
     AND indexable ORDER BY name ASC LIMIT $2 OFFSET $3"

let search_communities (module C : Caqti_lwt.CONNECTION) search_term limit
    offset =
  let term = "%" ^ search_term ^ "%" in
  C.collect_list search_communities_query (term, limit, offset) >>= function
  | Ok rows -> Lwt.return (Ok (List.map Community_types.map_community_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Single UPDATE covers all editable fields — no partial-update complexity;
   settings form always submits all four fields so overwrite is safe.
   RETURNING hands back the authoritative updated row in the same round-trip;
   None means the id matched no community (previously a silent no-op). *)
let update_community_details_query =
  let open Caqti_request.Infix in
  (Caqti_type.(
     t2 (t4 (option string) (option string) (option string) (option string)) int)
  ->? Community_types.community_row_type)
    "UPDATE communities SET description = $1, rules = $2, avatar_url = $3, \
     banner_url = $4 WHERE id = $5 RETURNING id, slug, name, description, \
     rules, avatar_url, banner_url, allow_downvotes, sections_enabled, \
     visibility, indexable, is_network_community, onboarding_state, \
     discoverable"

let update_community_details (module C : Caqti_lwt.CONNECTION) community_id
    description rules avatar_url banner_url =
  C.find_opt update_community_details_query
    ((description, rules, avatar_url, banner_url), community_id)
  >>= function
  | Ok (Some row) ->
      Lwt.return (Ok (Some (Community_types.map_community_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let toggle_downvotes_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 bool int) ->. Caqti_type.unit)
    "UPDATE communities SET allow_downvotes = $1 WHERE id = $2"

let toggle_community_downvotes (module C : Caqti_lwt.CONNECTION) community_id
    allow =
  C.exec toggle_downvotes_query (allow, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Visibility/indexability writes (set from the settings UI). visibility is the
   stored TEXT mirrored by the CHECK constraint; we accept the closed [community_visibility]
   variant and stringify here so call sites never pass a raw, unvalidated string. The DB CHECK
   is the backstop. These touch only the SEO/access columns — no other field. *)
(* RETURNING the authoritative updated row: visibility is a closed PostHog
   group property, so the handler needs the post-update record for its
   $groupidentify without a second lookup. None = id matched no community. *)
let update_community_visibility_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->? Community_types.community_row_type)
    "UPDATE communities SET visibility = $1 WHERE id = $2 RETURNING id, slug, \
     name, description, rules, avatar_url, banner_url, allow_downvotes, \
     sections_enabled, visibility, indexable, is_network_community, \
     onboarding_state, discoverable"

let update_community_visibility (module C : Caqti_lwt.CONNECTION) community_id
    visibility =
  C.find_opt update_community_visibility_query
    (Community_types.community_visibility_to_string visibility, community_id)
  >>= function
  | Ok (Some row) ->
      Lwt.return (Ok (Some (Community_types.map_community_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let update_community_indexable_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 bool int) ->. Caqti_type.unit)
    "UPDATE communities SET indexable = $1 WHERE id = $2"

let update_community_indexable (module C : Caqti_lwt.CONNECTION) community_id
    indexable =
  C.exec update_community_indexable_query (indexable, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Joins posts→communities in one query so vote_handler avoids a second round-trip. *)
let get_allows_downvotes_for_post_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.bool)
    "SELECT c.allow_downvotes FROM communities c JOIN posts p ON \
     p.community_id = c.id WHERE p.id = $1"

let get_allows_downvotes_for_post (module C : Caqti_lwt.CONNECTION) post_id =
  C.find_opt get_allows_downvotes_for_post_query post_id >>= function
  | Ok (Some v) -> Lwt.return (Ok v)
  | Ok None ->
      Lwt.return (Ok true) (* post not found: let DB constraint handle it *)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_allows_downvotes_for_comment_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? Caqti_type.bool)
    "SELECT c.allow_downvotes FROM communities c JOIN posts p ON \
     p.community_id = c.id JOIN comments cm ON cm.post_id = p.id WHERE cm.id = \
     $1"

let get_allows_downvotes_for_comment (module C : Caqti_lwt.CONNECTION)
    comment_id =
  C.find_opt get_allows_downvotes_for_comment_query comment_id >>= function
  | Ok (Some v) -> Lwt.return (Ok v)
  | Ok None -> Lwt.return (Ok true)
  | Error err -> Lwt.return (Error (Caqti_error.show err))
