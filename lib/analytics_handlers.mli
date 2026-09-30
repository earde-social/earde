(** The analytics consent endpoint and the helpers other handlers use to capture
    events after their SQL has committed. *)

val analytics_consent_handler : Dream.handler
val analytics_consent_method_not_allowed : Dream.handler

val with_analytics_after_sql :
  (((unit -> unit) -> unit) -> 'a Lwt.t) -> 'a Lwt.t

val analytics_public_string : Community_types.community -> 'a -> 'a option
val community_group_of : Community_types.community -> Analytics.community_group

val attempt_posthog_group_cleanup_job :
  Dream.request -> job_id:int -> unit Lwt.t
