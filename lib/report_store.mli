(** Reports / mod-queue value types. Closed variants keep raw strings out of the
    public API and dynamic SQL; the DB CHECK constraints mirror them exactly.
    The [*_of_string] helpers are partial (None off-enum). *)
type report_target = Report_post | Report_comment | Report_chat_message

type report_reason =
  | Report_spam
  | Report_abuse
  | Report_off_topic
  | Report_illegal
  | Report_other

type report_status = Report_open | Report_dismissed | Report_action_taken

type report_action_kind =
  | Report_removed_content
  | Report_banned_author
  | Report_other_action

val report_target_to_string : report_target -> string
val report_target_of_string : string -> report_target option
val report_reason_to_string : report_reason -> string
val report_reason_of_string : string -> report_reason option
val report_status_to_string : report_status -> string
val report_status_of_string : string -> report_status option
val report_action_kind_to_string : report_action_kind -> string
val report_action_kind_of_string : string -> report_action_kind option

(* Denormalized mod-queue row: reporter/author usernames are JOINed in (author optional). *)
type report_row = {
  id : int;
  community_id : int;
  reporter_user_id : int;
  reporter_username : string;
  target_type : report_target;
  target_id : int64;
  target_author_user_id : int option;
  target_author_username : string option;
  reason : report_reason;
  details : string option;
  status : report_status;
  action_kind : report_action_kind option;
  resolution_note : string option;
  resolved_by_user_id : int option;
  resolved_at : string option;
  created_at : string;
}

(* [`Created id] on insert; [`Duplicate] when an OPEN report by the same reporter for the
   same target already exists (ON CONFLICT DO NOTHING on the partial-unique index). *)
val create_report :
  (module Caqti_lwt.CONNECTION) ->
  community_id:int ->
  reporter_user_id:int ->
  target_type:report_target ->
  target_id:int64 ->
  target_author_user_id:int option ->
  reason:report_reason ->
  details:string option ->
  ([ `Created of int | `Duplicate ], string) result Lwt.t

val get_reports_by_community :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  status:report_status ->
  (report_row list, string) result Lwt.t

val get_report_by_id :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  (report_row option, string) result Lwt.t

val resolve_report :
  (module Caqti_lwt.CONNECTION) ->
  int ->
  resolver_user_id:int ->
  status:report_status ->
  action_kind:report_action_kind option ->
  note:string option ->
  (unit, string) result Lwt.t

val count_open_reports :
  (module Caqti_lwt.CONNECTION) -> int -> (int, string) result Lwt.t
