type post = {
  id : int; title : string; url : string option; content : string option;
  community_id : int; user_id : int; username : string; community_slug : string;
  created_at : string; score : int; comment_count : int; allow_downvotes : bool;
  image_url : string option;
  section_name : string option;
  section_slug : string option;
  community_sections_enabled : bool;
  author_local_karma : int;
  author_local_post_count : int;
  author_local_comment_count : int;
  author_first_active_at : string option;
}

(** One row of a community-scoped feed (community page, section feed,
    uncategorized feed, Recent durable knowledge). [fi_post] is the one
    canonical post and keeps its immutable origin identity —
    [post.community_slug] and the [post.section_*] fields are always the
    ORIGIN's. [fi_shared = Some _] means the row reached this feed through an
    accepted shared-thread placement into the requested community: the
    provenance name for the "Shared from" label and the placement's own
    destination section (the effective local section context) travel here,
    never inside [post], so no renderer can read one field as two different
    communities. [fi_shared = None] rows render byte-identically to the
    pre-shared-threads feeds. *)
type feed_shared_context = {
  fs_origin_name : string;
  fs_section_name : string option;
  fs_section_slug : string option;
}

type feed_item = {
  fi_post : post;
  fi_shared : feed_shared_context option;
}

(** Closed variant for post sort order — the type system prevents any raw string
    from reaching dynamic SQL, making Printf.sprintf injection impossible by construction. *)
type sort_mode = Newest | Top | Hot | Active

(** The 20 SELECT columns of a post row, local karma stats last. *)
type post_row =
  (int * string * string option * string option)
  * (int * int * string * string)
  * (string * int * int * bool)
  * (string option * string option * string option * bool)
  * (int * int * int * string option)

val post_row_type : post_row Caqti_type.t
val map_post_row : post_row -> post

(** A post row plus the placement arm of a community-scoped feed:
    (via_placement, origin name, destination section name and slug). *)
type feed_item_row =
  post_row * (bool * string option * string option * string option)

val feed_item_row_type : feed_item_row Caqti_type.t
val map_feed_item_row : feed_item_row -> feed_item
