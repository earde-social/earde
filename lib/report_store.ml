open Lwt.Infix

(* === REPORTS / MOD-QUEUE VALUE TYPES ===
   Closed variants so no raw, unchecked string ever reaches dynamic SQL or the public
   API; the *_to_string forms are the ONLY values written to the reports table, and the
   DB CHECK constraints mirror these enums exactly. The *_of_string helpers are partial
   (return None off-enum) so a stray value fails at the boundary instead of corrupting a
   row — see test_earde.ml for the round-trip + rejection coverage. *)
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

let report_target_to_string = function
  | Report_post -> "post"
  | Report_comment -> "comment"
  | Report_chat_message -> "chat_message"

let report_target_of_string = function
  | "post" -> Some Report_post
  | "comment" -> Some Report_comment
  | "chat_message" -> Some Report_chat_message
  | _ -> None

let report_reason_to_string = function
  | Report_spam -> "spam"
  | Report_abuse -> "abuse"
  | Report_off_topic -> "off_topic"
  | Report_illegal -> "illegal"
  | Report_other -> "other"

let report_reason_of_string = function
  | "spam" -> Some Report_spam
  | "abuse" -> Some Report_abuse
  | "off_topic" -> Some Report_off_topic
  | "illegal" -> Some Report_illegal
  | "other" -> Some Report_other
  | _ -> None

let report_status_to_string = function
  | Report_open -> "open"
  | Report_dismissed -> "dismissed"
  | Report_action_taken -> "action_taken"

let report_status_of_string = function
  | "open" -> Some Report_open
  | "dismissed" -> Some Report_dismissed
  | "action_taken" -> Some Report_action_taken
  | _ -> None

(* Report_other_action maps to the bare "other" stored value (distinct constructor name
   only to avoid clashing with report_reason's Report_other). *)
let report_action_kind_to_string = function
  | Report_removed_content -> "removed_content"
  | Report_banned_author -> "banned_author"
  | Report_other_action -> "other"

let report_action_kind_of_string = function
  | "removed_content" -> Some Report_removed_content
  | "banned_author" -> Some Report_banned_author
  | "other" -> Some Report_other_action
  | _ -> None

(* Denormalized queue row: reporter/author usernames are JOINed in (author LEFT JOIN, so
   None for a tombstoned/missing author). Enum columns decode through the *_of_string
   helpers; the DB CHECK constraints make the decode total in practice. *)
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

(* 16-column queue row: t4(t4, t4, t4, t4) stays within Caqti's per-tuple arity limit.
   Enum columns come back as TEXT and are decoded by map_report_row. target_id is int64
   (BIGINT). resolved_at/created_at are ::text-cast like the other stores. *)
let report_row_type =
  let open Caqti_type in
  t4
    (t4 int int int string)
    (t4 string int64 (option int) (option string))
    (t4 string (option string) string (option string))
    (t4 (option string) (option int) (option string) string)

let report_select =
  "SELECT r.id, r.community_id, r.reporter_user_id, reporter.username,
          r.target_type, r.target_id, r.target_author_user_id, author.username,
          r.reason, r.details, r.status, r.action_kind,
          r.resolution_note, r.resolved_by_user_id, r.resolved_at::text, r.created_at::text
   FROM reports r
   JOIN users reporter ON r.reporter_user_id = reporter.id
   LEFT JOIN users author ON r.target_author_user_id = author.id"

(* CHECK constraints guarantee the enum strings are on-enum, so the *_of_string defaults
   below are unreachable; they exist only to keep decoding total. action_kind is NULL or
   on-enum, hence Option.bind. *)
let map_report_row
    ((id, community_id, reporter_user_id, reporter_username),
     (target_type_s, target_id, target_author_user_id, target_author_username),
     (reason_s, details, status_s, action_kind_s),
     (resolution_note, resolved_by_user_id, resolved_at, created_at)) =
  {
    id; community_id; reporter_user_id; reporter_username;
    target_type = Option.value (report_target_of_string target_type_s) ~default:Report_post;
    target_id; target_author_user_id; target_author_username;
    reason = Option.value (report_reason_of_string reason_s) ~default:Report_other;
    details;
    status = Option.value (report_status_of_string status_s) ~default:Report_open;
    action_kind = Option.bind action_kind_s report_action_kind_of_string;
    resolution_note; resolved_by_user_id; resolved_at; created_at;
  }

(* ON CONFLICT DO NOTHING fires against uniq_reports_open_per_reporter_target — the only
   unique index a freshly-generated SERIAL id can collide on — so a second OPEN report by
   the same reporter for the same target inserts nothing and RETURNING yields no row
   (find_opt -> Ok None -> `Duplicate). We use a bare DO NOTHING (no inference clause):
   inferring a PARTIAL index requires repeating its WHERE predicate, and the PK cannot
   collide on a generated id, so bare DO NOTHING is both correct and simpler. *)
let create_report_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t4 int int string int64) (t3 (option int) string (option string))) ->? Caqti_type.int)
  "INSERT INTO reports
     (community_id, reporter_user_id, target_type, target_id, target_author_user_id, reason, details)
   VALUES ($1, $2, $3, $4, $5, $6, $7)
   ON CONFLICT DO NOTHING
   RETURNING id"

let create_report (module C : Caqti_lwt.CONNECTION) ~community_id ~reporter_user_id
    ~target_type ~target_id ~target_author_user_id ~reason ~details =
  C.find_opt create_report_query
    ((community_id, reporter_user_id, report_target_to_string target_type, target_id),
     (target_author_user_id, report_reason_to_string reason, details))
  >>= function
  | Ok (Some id) -> Lwt.return (Ok (`Created id))
  | Ok None -> Lwt.return (Ok `Duplicate)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let get_reports_by_community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int string) ->* report_row_type)
  (report_select ^ "
   WHERE r.community_id = $1 AND r.status = $2
   ORDER BY r.created_at DESC, r.id DESC")

let get_reports_by_community (module C : Caqti_lwt.CONNECTION) community_id ~status =
  C.collect_list get_reports_by_community_query (community_id, report_status_to_string status)
  >>= function
  | Ok rows -> Lwt.return (Ok (List.map map_report_row rows))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let get_report_by_id_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->? report_row_type)
  (report_select ^ " WHERE r.id = $1")

let get_report_by_id (module C : Caqti_lwt.CONNECTION) report_id =
  C.find_opt get_report_by_id_query report_id >>= function
  | Ok (Some row) -> Lwt.return (Ok (Some (map_report_row row)))
  | Ok None -> Lwt.return (Ok None)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let resolve_report_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string (option string) (option string)) (t2 int int)) ->. Caqti_type.unit)
  "UPDATE reports
   SET status = $1, action_kind = $2, resolution_note = $3,
       resolved_by_user_id = $4, resolved_at = CURRENT_TIMESTAMP
   WHERE id = $5"

let resolve_report (module C : Caqti_lwt.CONNECTION) report_id ~resolver_user_id ~status ~action_kind ~note =
  let action_kind_s = Option.map report_action_kind_to_string action_kind in
  C.exec resolve_report_query
    ((report_status_to_string status, action_kind_s, note), (resolver_user_id, report_id))
  >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let count_open_reports_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM reports WHERE community_id = $1 AND status = 'open'"

let count_open_reports (module C : Caqti_lwt.CONNECTION) community_id =
  C.find count_open_reports_query community_id >>= function
  | Ok c -> Lwt.return (Ok c)
  | Error e -> Lwt.return (Error (Caqti_error.show e))
