type post = {
  id : int;
  title : string;
  url : string option;
  content : string option;
  community_id : int;
  user_id : int;
  username : string;
  community_slug : string;
  created_at : string;
  score : int;
  comment_count : int;
  allow_downvotes : bool;
  image_url : string option;
  section_name : string option;
  section_slug : string option;
  community_sections_enabled : bool;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(* Closed variant for post sort order. The type system enforces that no raw string
   can reach Printf.sprintf — injection is impossible by construction, not by convention. *)
type sort_mode = Newest | Top | Hot | Active

(* post has 20 SELECT columns; local karma stats appended as the 5th t4 group.
   Caqti tN stops at t7 so nesting is mandatory. *)
type post_row =
  (int * string * string option * string option)
  * (int * int * string * string)
  * (string * int * int * bool)
  * (string option * string option * string option * bool)
  * (int * int * int * string option)

let post_row_type : post_row Caqti_type.t =
  let open Caqti_type in
  t5
    (t4 int string (option string) (option string))
    (t4 int int string string) (t4 string int int bool)
    (t4 (option string) (option string) (option string) bool)
    (t4 int int int (option string))

let map_post_row
    ( (id, title, url, content),
      (community_id, user_id, username, community_slug),
      (created_at, score, comment_count, allow_downvotes),
      (image_url, section_name, section_slug, community_sections_enabled),
      ( author_local_karma,
        author_local_post_count,
        author_local_comment_count,
        author_first_active_at ) ) =
  {
    id;
    title;
    url;
    content;
    community_id;
    user_id;
    username;
    community_slug;
    created_at;
    score;
    comment_count;
    allow_downvotes;
    image_url;
    section_name;
    section_slug;
    community_sections_enabled;
    author_local_karma;
    author_local_post_count;
    author_local_comment_count;
    author_first_active_at;
  }

(* A community-scoped feed row wraps the canonical post rather than extending
   it: [post] keeps its immutable origin identity (community_slug and the
   section_* fields are always the origin's), and everything a destination
   surface must render differently rides in [fi_shared]. One renderer can
   therefore never read a single field as the origin while another reads it
   as the destination. [fi_shared = None] is the community's own post, and
   the row renders byte-identically to the pre-shared-threads feed. *)
type feed_shared_context = {
  fs_origin_name : string;
  fs_section_name : string option;
  fs_section_slug : string option;
}

type feed_item = { fi_post : post; fi_shared : feed_shared_context option }

type feed_item_row =
  post_row * (bool * string option * string option * string option)

let feed_item_row_type : feed_item_row Caqti_type.t =
  let open Caqti_type in
  t2 post_row_type (t4 bool (option string) (option string) (option string))

let map_feed_item_row (post_row, (via_placement, origin_name, ds_name, ds_slug))
    =
  {
    fi_post = map_post_row post_row;
    fi_shared =
      (if via_placement then
         (* origin_name is a.name on the placement arm and therefore never
            NULL in practice; the default only guards a corrupt row. *)
         Some
           {
             fs_origin_name = Option.value origin_name ~default:"";
             fs_section_name = ds_name;
             fs_section_slug = ds_slug;
           }
       else None);
  }
