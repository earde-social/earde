(** Page numbers for the public, offset-paginated listings: /feed, /search, a
    community home and a community section. *)

val max_page : int
(** The deepest page served (1000). *)

val page_size : int
(** Rows per page (20). *)

val parse : string option -> (int, [ `Out_of_range ]) result
(** The [page] query value. Absent, empty, zero or not a plain decimal number:
    page 1. A plain decimal number above {!max_page}, however long:
    [`Out_of_range]. Never raises and never overflows. *)

val offset : int -> int
(** The row offset of a page returned by {!parse}. *)

val out_of_range :
  ?user:string -> return_url:string -> Dream.request -> Dream.response Lwt.t
(** The 400 answer for [`Out_of_range], rendered without any database work. *)
