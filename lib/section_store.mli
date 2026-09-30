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

val create_section :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  string ->
  string option ->
  int ->
  string ->
  bool ->
  (unit, string) result Lwt.t

val get_sections_by_community :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (community_section list, string) result Lwt.t

val get_section_by_id :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  (community_section option, string) result Lwt.t

val get_section_by_slug :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  int ->
  (community_section option, string) result Lwt.t

val update_section :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  string ->
  string option ->
  string ->
  (unit, string) result Lwt.t

(* Update_section_indexable section_id community_id indexable — per-section toggle; community-scoped. *)
val update_section_indexable :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  bool ->
  (unit, string) result Lwt.t

val delete_section :
  (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val get_sections_with_stats :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  ((community_section * int * string option) list, string) result Lwt.t

val get_orphaned_count_and_activity :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (int * string option, string) result Lwt.t

(* Destination-aware like the community/section feeds: own sectionless
   posts plus accepted NULL-section placements (sectionless destinations
   and section deletions via ON DELETE SET NULL). The count query below
   matches it row for row and gates the virtual Uncategorized page. *)
val get_orphaned_posts :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  Post_types.sort_mode ->
  int ->
  int ->
  (Post_types.feed_item list, string) result Lwt.t
