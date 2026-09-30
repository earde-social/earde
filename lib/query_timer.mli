(** APM helper — wraps any DB call, emits {query, execution_ms, status} JSON via Logs.info. *)
val with_query_timer : name:string -> (unit -> ('a, string) result Lwt.t) -> ('a, string) result Lwt.t
