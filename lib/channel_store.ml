open Lwt.Infix

type channel = {
  id : int;
  community_id : int;
  slug : string;
  name : string;
  topic : string option;
  position : int;
  is_archived : bool;
  created_at : string;
  indexable : bool;
}

(* 8-column channel row: t2(t4, t4). created_at::text keeps every row type
   string-typed, matching the other stores. *)
let channel_row_type =
  let open Caqti_type in
  t3 (t4 int int string string) (t4 (option string) int bool string) bool

(* Explicit : channel annotation — `id`/`community_id`/`slug`/`name`/`position`
   are shared with other records; the literal resolves via topic/is_archived,
   but we pin it to stay robust against future field overlaps. Trailing bool is the
   SEO indexable flag for the channel's public archive surface. *)
let map_channel_row ((id, community_id, slug, name), (topic, position, is_archived, created_at), indexable) : channel =
  { id; community_id; slug; name; topic; position; is_archived; created_at; indexable }

(* lowercase, trim, spaces/dashes → single dash, strip non-alphanumeric.
   Self-contained copy of Section.slugify: each inner module owns its helpers,
   and channels have no reserved "uncategorized" slug to share. *)
let slugify name =
  let name = String.lowercase_ascii (String.trim name) in
  let buf = Buffer.create (String.length name) in
  let prev_dash = ref false in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | '0'..'9' -> prev_dash := false; Buffer.add_char buf c
    | ' ' | '-' | '_' ->
        if not !prev_dash then Buffer.add_char buf '-';
        prev_dash := true
    | _ -> ()
  ) name;
  let s = Buffer.contents buf in
  let len = String.length s in
  if len > 0 && s.[len-1] = '-' then String.sub s 0 (len-1) else s

let slug_exists_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string) ->? Caqti_type.int)
  "SELECT 1 FROM channels WHERE community_id = $1 AND slug = $2"

let slug_exists (module C : Caqti_lwt.CONNECTION) community_id slug =
  C.find_opt slug_exists_query (community_id, slug)
  >>= function
  | Ok (Some _) -> Lwt.return (Ok true)
  | Ok None -> Lwt.return (Ok false)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Tries base, then base-2, base-3… until a free slug is found. *)
let rec find_unique_slug db community_id base n =
  let candidate = if n = 1 then base else Printf.sprintf "%s-%d" base n in
  slug_exists db community_id candidate >>= function
  | Ok false -> Lwt.return (Ok candidate)
  | Ok true  -> find_unique_slug db community_id base (n + 1)
  | Error e  -> Lwt.return (Error e)

let create_channel_query =
  let open Caqti_request.Infix in
  (* 5 params: community_id, slug, name, topic, position. is_archived and
     created_at take their column defaults. *)
  (Caqti_type.(t2 (t4 int string string (option string)) int) ->. Caqti_type.unit)
  "INSERT INTO channels (community_id, slug, name, topic, position) VALUES ($1, $2, $3, $4, $5)"

(* Returns the generated slug (not unit like create_section): the slug is
   auto-derived, so the caller cannot know it otherwise, and the Step-3 handler
   needs it to redirect to /c/:slug/chat/:channel_slug. *)
let create_channel (module C : Caqti_lwt.CONNECTION) community_id name topic position =
  let base = slugify name in
  let base = if base = "" then "channel" else base in
  find_unique_slug (module C) community_id base 1 >>= function
  | Error e -> Lwt.return (Error e)
  | Ok slug ->
      C.exec create_channel_query ((community_id, slug, name, topic), position)
      >>= function
      | Ok () -> Lwt.return (Ok slug)
      | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* channel_row_type)
  "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE community_id = $1 ORDER BY is_archived ASC, position ASC, name ASC"

let get_channels_by_community (module C : Caqti_lwt.CONNECTION) community_id =
  C.collect_list get_by_community_query community_id
  >>= function
  | Ok rows -> Lwt.return (Ok (List.map map_channel_row rows))
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let get_by_slug_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->? channel_row_type)
  "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE slug = $1 AND community_id = $2"

let get_channel_by_slug (module C : Caqti_lwt.CONNECTION) slug community_id =
  C.find_opt get_by_slug_query (slug, community_id)
  >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_channel_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Point-read by (id, community_id) — validates ownership without a JOIN,
   mirroring Section.get_section_by_id. *)
let get_by_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int) ->? channel_row_type)
  "SELECT id, community_id, slug, name, topic, position, is_archived, created_at::text, indexable FROM channels WHERE id = $1 AND community_id = $2"

let get_channel_by_id (module C : Caqti_lwt.CONNECTION) channel_id community_id =
  C.find_opt get_by_id_query (channel_id, community_id)
  >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_channel_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* set rather than one-way archive: covers archive and un-archive, and matches
   update_section's community-scoped UPDATE style. *)
let set_archived_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 int int) bool) ->. Caqti_type.unit)
  "UPDATE channels SET is_archived = $3 WHERE id = $1 AND community_id = $2"

let set_channel_archived (module C : Caqti_lwt.CONNECTION) channel_id community_id is_archived =
  C.exec set_archived_query ((channel_id, community_id), is_archived)
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Display-only edit: name + topic, never slug. Slug stays stable so existing
   /c/:slug/ch/:channel_slug links and bookmarks keep resolving, and messages
   (keyed by channel_id) are unaffected. Community-scoped WHERE mirrors
   set_channel_archived / update_section. *)
let update_channel_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t2 string (option string)) (t2 int int)) ->. Caqti_type.unit)
  "UPDATE channels SET name = $1, topic = $2 WHERE id = $3 AND community_id = $4"

let update_channel (module C : Caqti_lwt.CONNECTION) channel_id community_id name topic =
  C.exec update_channel_query ((name, topic), (channel_id, community_id))
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

(* Slice H: channel indexability toggle. Community-scoped WHERE mirrors update_channel /
   set_channel_archived. Flips only the archive-surface noindex + provenance-safety behavior
   Slice G already wires; it does NOT make the channel private (readable by direct URL still). *)
let update_channel_indexable_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 bool (t2 int int)) ->. Caqti_type.unit)
  "UPDATE channels SET indexable = $1 WHERE id = $2 AND community_id = $3"

let update_channel_indexable (module C : Caqti_lwt.CONNECTION) channel_id community_id indexable =
  C.exec update_channel_indexable_query (indexable, (channel_id, community_id))
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))
