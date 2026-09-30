(** The global feed and search results pages. *)

val feed_page :
  ?user:string ->
  scope:string ->
  sort_mode:string ->
  is_logged_in:bool ->
  admin_usernames:string list ->
  rail_communities:Community_types.community list ->
  user_votes:(int * int) list ->
  current_page:int ->
  ?shared_destinations:(int * (string * string) list) list ->
  Post_types.post list ->
  Dream.request ->
  string
(** [source_focus] = (promoted post id, post title, chronological highlight
    message ids): renders the reverse-navigation state — SSR context notice,
    anchor/highlight data attributes, canonical link to the clean channel URL.
    Omitted → normal channel page. *)

val search_results_page :
  ?user:string ->
  admin_usernames:string list ->
  ?chat_sources:(int * string * string * int) list ->
  ?rail_communities:Community_types.community list ->
  (int * int) list ->
  int ->
  string ->
  string ->
  Community_types.community list ->
  (int * string * string * string option * string option) list ->
  Post_types.post list ->
  (int * string * string * string * int * int) list ->
  Dream.request ->
  string
