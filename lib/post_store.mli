val create_post :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  string option ->
  string option ->
  string option ->
  int option ->
  int ->
  int ->
  (int, string) result Lwt.t

val get_all_posts :
  (module Caqti_lwt.CONNECTION) ->
  Post_types.sort_mode ->
  int ->
  int ->
  (Post_types.post list, string) result Lwt.t

val get_personalized_feed :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  Post_types.sort_mode ->
  int ->
  int ->
  (Post_types.post list, string) result Lwt.t

(* Community-scoped feeds are destination-aware (Shared Threads read side):
   one bounded UNION ALL statement returns the community's own posts AND
   the canonical posts holding an accepted placement into it (public
   origin only), with the sort mode and LIMIT/OFFSET applied to the
   combined set. Current connection state, discoverability, and
   onboarding eligibility deliberately do NOT gate the placement arm —
   they gate request/acceptance, not continued rendering. *)
val get_posts_by_community :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  Post_types.sort_mode ->
  int ->
  int ->
  (Post_types.feed_item list, string) result Lwt.t

val get_posts_by_section :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  Post_types.sort_mode ->
  int ->
  int ->
  (Post_types.feed_item list, string) result Lwt.t

val get_post_by_id :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (Post_types.post option, string) result Lwt.t

val get_posts_by_user :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (Post_types.post list, string) result Lwt.t

(* Profile leak-filter: (post_id, community_id, raw visibility, indexable,
   section_non_indexable) for a set of post ids, in one bounded query. Lets a profile drop
   posts/comments in private communities the viewer can't read, public-but-non-indexable
   communities, and non-indexable forum sections from public discovery.
   The 5th flag is TRUE only when the post sits in a section flagged indexable=false. Empty -> []. *)
val get_post_communities :
  (module Caqti_lwt.CONNECTION) ->
  int list ->
  ((int * int * string * bool * bool) list, string) result Lwt.t

(* The community that owns a post, derived from the post itself — the
   authoritative input to the vote ban gate, which must never trust a
   community id from the request. [None] = no such post. *)
val get_post_community_id :
  (module Caqti_lwt.CONNECTION) -> int -> (int option, string) result Lwt.t

val vote_post :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  int ->
  int ->
  (unit, string) result Lwt.t

val remove_post_vote :
  (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val soft_delete_post :
  (module Caqti_lwt.CONNECTION) -> int -> int -> (unit, string) result Lwt.t

val search_posts :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  int ->
  int ->
  (Post_types.post list, string) result Lwt.t

val combined_feed_query_str :
  own_where:string ->
  placement_where:string ->
  limit_param:string ->
  offset_param:string ->
  Post_types.sort_mode ->
  string
(** The destination-aware community feed statement: the community's own posts
    [UNION ALL] the canonical posts holding an accepted placement into it
    (public origin only), with [sort_mode] and the LIMIT/OFFSET parameters
    applied to the combined set. [own_where] and [placement_where] are trusted
    SQL fragments with positional parameters; shared by the section feeds. *)
