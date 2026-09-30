open Lwt.Infix

type community_section = {
  section_id : int;
  community_id : int;
  name : string;
  slug : string;
  description : string option;
  position : int;
  default_sort : string;
  is_introduction_section : bool;
  indexable : bool;
}

(* 9-column section row: t3(t4, t4, bool) — trailing bool is the SEO indexable flag. *)
let section_row_type =
  let open Caqti_type in
  t3 (t4 int int string string) (t4 (option string) int string bool) bool

let map_section_row
    ( (section_id, community_id, name, slug),
      (description, position, default_sort, is_introduction_section),
      indexable ) =
  {
    section_id;
    community_id;
    name;
    slug;
    description;
    position;
    default_sort;
    is_introduction_section;
    indexable;
  }

(* lowercase, trim, spaces/dashes → single dash, strip non-alphanumeric *)
let slugify name =
  let name = String.lowercase_ascii (String.trim name) in
  let buf = Buffer.create (String.length name) in
  let prev_dash = ref false in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' ->
          prev_dash := false;
          Buffer.add_char buf c
      | ' ' | '-' | '_' ->
          if not !prev_dash then Buffer.add_char buf '-';
          prev_dash := true
      | _ -> ())
    name;
  let s = Buffer.contents buf in
  let len = String.length s in
  if len > 0 && s.[len - 1] = '-' then String.sub s 0 (len - 1) else s

let slug_exists_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string) ->? Caqti_type.int)
    "SELECT 1 FROM community_sections WHERE community_id = $1 AND slug = $2"

let slug_exists (module C : Caqti_lwt.CONNECTION) community_id slug =
  C.find_opt slug_exists_query (community_id, slug) >>= function
  | Ok (Some _) -> Lwt.return (Ok true)
  | Ok None -> Lwt.return (Ok false)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Tries base, then base-2, base-3… until a free slug is found. *)
let rec find_unique_slug db community_id base n =
  let candidate = if n = 1 then base else Printf.sprintf "%s-%d" base n in
  slug_exists db community_id candidate >>= function
  | Ok false -> Lwt.return (Ok candidate)
  | Ok true -> find_unique_slug db community_id base (n + 1)
  | Error e -> Lwt.return (Error e)

let create_section_query =
  let open Caqti_request.Infix in
  (* 7 params: t2(t4, t3) — community_id, name, slug, description, position, default_sort, is_intro *)
  (Caqti_type.(t2 (t4 int string string (option string)) (t3 int string bool))
  ->. Caqti_type.unit)
    "INSERT INTO community_sections (community_id, name, slug, description, \
     position, default_sort, is_introduction_section) VALUES ($1, $2, $3, $4, \
     $5, $6, $7)"

let create_section (module C : Caqti_lwt.CONNECTION) community_id name
    description position default_sort is_intro =
  let base = slugify name in
  let base = if base = "" then "section" else base in
  (* "uncategorized" is reserved for the virtual orphaned-posts section. *)
  if base = "uncategorized" then
    Lwt.return
      (Error
         "The name 'Uncategorized' is reserved. Please choose a different \
          section name.")
  else
    find_unique_slug (module C) community_id base 1 >>= function
    | Error e -> Lwt.return (Error e)
    | Ok slug -> (
        let valid_sorts = [ "hot"; "new"; "top"; "active" ] in
        let safe_sort =
          if List.mem default_sort valid_sorts then default_sort else "new"
        in
        C.exec create_section_query
          ( (community_id, name, slug, description),
            (position, safe_sort, is_intro) )
        >>= function
        | Ok () -> Lwt.return (Ok ())
        | Error err -> Lwt.return (Error (Caqti_error.show err)))

let get_by_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, \
     is_introduction_section, indexable FROM community_sections WHERE \
     community_id = $1 ORDER BY is_introduction_section DESC, position ASC, \
     name ASC"

let get_sections_by_community (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_by_community_query community_id >>= function
  | Ok rows -> Lwt.return (Ok (List.map map_section_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Point-read by (id, community_id) — validates ownership without a JOIN.
   Used in the create_post handler to prevent posting to a foreign section. *)
let get_by_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, \
     is_introduction_section, indexable FROM community_sections WHERE id = $1 \
     AND community_id = $2"

let get_section_by_id (module C : Caqti_lwt.CONNECTION) section_id community_id
    =
  C.find_opt get_by_id_query (section_id, community_id) >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_section_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_slug_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->? section_row_type)
    "SELECT id, community_id, name, slug, description, position, default_sort, \
     is_introduction_section, indexable FROM community_sections WHERE slug = \
     $1 AND community_id = $2"

let get_section_by_slug (module C : Caqti_lwt.CONNECTION) slug community_id =
  C.find_opt get_by_slug_query (slug, community_id) >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_section_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Slug is immutable after creation — URLs must remain stable.
   Only name, description, and default_sort are editable. *)
let update_section_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string (option string) string) int) ->. Caqti_type.unit)
    "UPDATE community_sections SET name = $1, description = $2, default_sort = \
     $3 WHERE id = $4"

let update_section (module C : Caqti_lwt.CONNECTION) section_id name description
    default_sort =
  let valid_sorts = [ "hot"; "new"; "top"; "active" ] in
  let safe_sort =
    if List.mem default_sort valid_sorts then default_sort else "new"
  in
  C.exec update_section_query ((name, description, safe_sort), section_id)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Section indexability toggle. Community-scoped WHERE (id AND community_id) so a
   forged section_id from another community cannot be flipped. The Slice-G resolver already
   reads this column to decide section/thread noindex + discovery exclusion. *)
let update_section_indexable_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 bool (t2 int int)) ->. Caqti_type.unit)
    "UPDATE community_sections SET indexable = $1 WHERE id = $2 AND \
     community_id = $3"

let update_section_indexable (module C : Caqti_lwt.CONNECTION) section_id
    community_id indexable =
  C.exec update_section_indexable_query (indexable, (section_id, community_id))
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* ON DELETE SET NULL on posts.section_id releases posts back to the root feed safely. *)
let delete_section_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_sections WHERE id = $1 AND community_id = $2"

let delete_section (module C : Caqti_lwt.CONNECTION) section_id community_id =
  C.exec delete_section_query (section_id, community_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* 11-column stats row: t3(t4,t4,t3) = section fields(8) + indexable + post_count + last_activity.
   LEFT JOIN keeps empty sections visible in the overview — COUNT returns 0, MAX returns NULL.
   GROUP BY cs.id covers all cs.* fields (functional dependency on PK). *)
let stats_row_type =
  let open Caqti_type in
  t3 (t4 int int string string)
    (t4 (option string) int string bool)
    (t3 bool int (option string))

let map_stats_row
    ( (section_id, community_id, name, slug),
      (description, position, default_sort, is_introduction_section),
      (indexable, post_count, last_activity) ) =
  ( {
      section_id;
      community_id;
      name;
      slug;
      description;
      position;
      default_sort;
      is_introduction_section;
      indexable;
    },
    post_count,
    last_activity )

(* Section statistics count what the section feed renders: the section's
   own posts plus accepted shared-thread placements into it (public origin
   only — a private origin's placements vanish from the feed, so they must
   vanish from the counts too). Scalar subqueries per section row replace
   the old single LEFT JOIN because the two sources would cross-multiply
   under one GROUP BY; the statement count is unchanged (one per page).
   GREATEST ignores NULL arms, so an empty side never masks the other. *)
let get_stats_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* stats_row_type)
    "SELECT cs.id, cs.community_id, cs.name, cs.slug, cs.description, \
     cs.position, cs.default_sort, cs.is_introduction_section,\n\
    \          cs.indexable,\n\
    \          ((SELECT COUNT(*) FROM posts p WHERE p.section_id = cs.id)\n\
    \           + (SELECT COUNT(*) FROM shared_thread_placements stp\n\
    \                JOIN communities oc ON oc.id = stp.origin_community_id\n\
    \               WHERE stp.destination_section_id = cs.id\n\
    \                 AND stp.destination_community_id = cs.community_id\n\
    \                 AND stp.status = 'accepted'\n\
    \                 AND oc.visibility = 'public'))::int AS post_count,\n\
    \          GREATEST(\n\
    \            (SELECT MAX(p.last_activity_at) FROM posts p WHERE \
     p.section_id = cs.id),\n\
    \            (SELECT MAX(sp.last_activity_at)\n\
    \               FROM shared_thread_placements stp\n\
    \               JOIN posts sp ON sp.id = stp.post_id\n\
    \               JOIN communities oc ON oc.id = stp.origin_community_id\n\
    \              WHERE stp.destination_section_id = cs.id\n\
    \                AND stp.destination_community_id = cs.community_id\n\
    \                AND stp.status = 'accepted'\n\
    \                AND oc.visibility = 'public'))::text AS last_activity\n\
    \   FROM community_sections cs\n\
    \   WHERE cs.community_id = $1\n\
    \   ORDER BY cs.is_introduction_section DESC, cs.position ASC, cs.name ASC"

let get_sections_with_stats (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_stats_query community_id >>= function
  | Ok rows -> Lwt.return (Ok (List.map map_stats_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Orphaned/uncategorized statistics: the community's own sectionless posts
   plus accepted NULL-section placements (public origin only), matching
   get_orphaned_posts row for row — the count also gates the virtual
   Uncategorized page's 404. *)
let orphaned_count_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->! Caqti_type.(t2 int (option string)))
    "SELECT ((SELECT COUNT(*) FROM posts p WHERE p.community_id = $1 AND \
     p.section_id IS NULL)\n\
    \           + (SELECT COUNT(*) FROM shared_thread_placements stp\n\
    \                JOIN communities oc ON oc.id = stp.origin_community_id\n\
    \               WHERE stp.destination_community_id = $1\n\
    \                 AND stp.destination_section_id IS NULL\n\
    \                 AND stp.status = 'accepted'\n\
    \                 AND oc.visibility = 'public'))::int,\n\
    \          GREATEST(\n\
    \            (SELECT MAX(p.last_activity_at) FROM posts p WHERE \
     p.community_id = $1 AND p.section_id IS NULL),\n\
    \            (SELECT MAX(sp.last_activity_at)\n\
    \               FROM shared_thread_placements stp\n\
    \               JOIN posts sp ON sp.id = stp.post_id\n\
    \               JOIN communities oc ON oc.id = stp.origin_community_id\n\
    \              WHERE stp.destination_community_id = $1\n\
    \                AND stp.destination_section_id IS NULL\n\
    \                AND stp.status = 'accepted'\n\
    \                AND oc.visibility = 'public'))::text"

let get_orphaned_count_and_activity (module C : Caqti_lwt.CONNECTION)
    community_id =
  C.find orphaned_count_query community_id >>= function
  | Ok (count, last_activity) -> Lwt.return (Ok (count, last_activity))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Uncategorized/orphaned feed: the community's own sectionless posts, plus
   accepted placements whose destination_section_id IS NULL — sectionless
   (flat) destinations and placements released by a destination-section
   deletion (ON DELETE SET NULL) alike. Shares Post_store.combined_feed_query_str
   so ordering, pagination, and the placement arm's public-origin rule are
   the one copy. The own arm's origin-section columns come back NULL
   naturally (p.section_id IS NULL), matching the old NULL::text shape. *)
let get_orphaned_posts (module C : Caqti_lwt.CONNECTION) community_id
    (sort_mode : Post_types.sort_mode) limit offset =
  let query_str =
    Post_store.combined_feed_query_str
      ~own_where:"p.community_id = $1 AND p.section_id IS NULL"
      ~placement_where:
        "stp.destination_community_id = $1 AND stp.destination_section_id IS \
         NULL"
      ~limit_param:"$2" ~offset_param:"$3" sort_mode
  in
  let query =
    let open Caqti_request.Infix in
    (Caqti_type.(t3 int int int) ->* Post_types.feed_item_row_type) query_str
  in
  C.collect_list query (community_id, limit, offset) >>= function
  | Ok rows -> Lwt.return (Ok (List.map Post_types.map_feed_item_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))
