(* PostHog person-deletion worker (analytics spec §3.3). Private REST API on
   POSTHOG_UI_HOST — never the ingest host, never the public project token.
   Endpoints verified read-only against current PostHog docs
   (privacy/data-storage#data-deletion, api/persons):
     GET    /api/projects/:project_id/persons/?distinct_id=<exact id>
     DELETE /api/projects/:project_id/persons/:uuid/?delete_events=true
   both with "Authorization: Bearer <personal API key>". *)

open Lwt.Infix

let default_batch_limit = 25

(* One bounded window per attempt; retries belong to the durable job table,
   not to the HTTP layer. *)
let attempt_timeout_seconds = 10.0

let with_timeout promise =
  Lwt.pick
    [
      (promise >|= fun r -> `Done r);
      (Lwt_unix.sleep attempt_timeout_seconds >|= fun () -> `Timeout);
    ]

(* Bounded diagnostics only. The exception text (which could embed hosts or
   URLs) is deliberately dropped — a class name is all that may be persisted
   or logged. *)
let classify_exn = function
  | Unix.Unix_error _ | Failure _ -> "network_error"
  | _ -> "network_error"

let auth_header key =
  Cohttp.Header.init () |> fun h ->
  Cohttp.Header.add h "authorization" ("Bearer " ^ key)

(* The person UUID is spliced into the DELETE path; accept only plausible
   UUID characters so a malformed lookup response can never rewrite the URL. *)
let is_safe_uuid uuid =
  uuid <> ""
  && String.for_all
       (function
         | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' | '-' -> true | _ -> false)
       uuid

(* Lookup outcome: `Absent (no matching person — already-deleted or
   never-consented users), `Person uuid (exactly one match), or a bounded
   error. More than one result must NOT delete an arbitrary person. *)
let parse_lookup_body body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error "malformed_lookup_response"
  | json -> (
      match json with
      | `Assoc fields -> (
          match List.assoc_opt "results" fields with
          | Some (`List []) -> Ok `Absent
          | Some (`List [ `Assoc person ]) -> (
              match List.assoc_opt "id" person with
              | Some (`String uuid) when is_safe_uuid uuid -> Ok (`Person uuid)
              | _ -> Error "malformed_lookup_response")
          | Some (`List (_ :: _ :: _)) -> Error "ambiguous_person_match"
          | _ -> Error "malformed_lookup_response")
      | _ -> Error "malformed_lookup_response")

let lookup_person ~config ~distinct_id =
  let uri =
    Uri.with_query'
      (Uri.of_string
         (Printf.sprintf "%s/api/projects/%s/persons/"
            config.Analytics.deletion_ui_host
            config.Analytics.deletion_project_id))
      [ ("distinct_id", distinct_id) ]
  in
  Cohttp_lwt_unix.Client.get
    ~headers:(auth_header config.Analytics.deletion_api_key)
    uri
  >>= fun (response, body) ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if status >= 200 && status < 300 then
    Cohttp_lwt.Body.to_string body >|= parse_lookup_body
  else
    Cohttp_lwt.Body.drain_body body >|= fun () ->
    Error (Printf.sprintf "lookup_http_%d" status)

let delete_person ~config ~uuid =
  let uri =
    Uri.with_query'
      (Uri.of_string
         (Printf.sprintf "%s/api/projects/%s/persons/%s/"
            config.Analytics.deletion_ui_host
            config.Analytics.deletion_project_id uuid))
      [ ("delete_events", "true") ]
  in
  Cohttp_lwt_unix.Client.delete
    ~headers:(auth_header config.Analytics.deletion_api_key)
    uri
  >>= fun (response, body) ->
  Cohttp_lwt.Body.drain_body body >|= fun () ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  (* Any documented success (2xx — the docs show 202/204 acceptance) marks the
     job completed; everything else stays pending. *)
  if status >= 200 && status < 300 then Ok ()
  else Error (Printf.sprintf "delete_http_%d" status)

let attempt_person_deletion ~distinct_id =
  match Analytics.deletion_api_config () with
  | None -> Lwt.return (Error "missing_configuration")
  | Some config ->
      Lwt.catch
        (fun () ->
          with_timeout
            ( lookup_person ~config ~distinct_id >>= function
              | Error e -> Lwt.return (Error e)
              | Ok `Absent ->
                  (* Nothing to delete: completed (covers never-consented
                     users and idempotent re-runs). *)
                  Lwt.return (Ok ())
              | Ok (`Person uuid) -> delete_person ~config ~uuid )
          >|= function
          | `Done result -> result
          | `Timeout -> Error "timeout")
        (fun exn -> Lwt.return (Error (classify_exn exn)))

(* === §13 group-profile scrub (private Groups API) ========================
   When a community turns fully private, its previously sent human-readable
   group properties must actually be REMOVED from the PostHog group profile —
   $groupidentify has no unset operation, so mere omission would leave the
   old values behind. The documented mechanism (verified against current
   PostHog docs api/groups and the served implementation):
     GET  /api/projects/:id/groups/find/?group_type_index&group_key
     POST /api/projects/:id/groups/delete_property/?group_type_index&group_key
          with JSON body {"$unset": "<property name>"}
   delete_property returns 400 when the property is absent, so each attempt
   reads the group first and deletes only what is still present — that makes
   re-runs idempotent. Only the closed property KEYS below and the numeric
   "community:<id>" group key ever appear in requests, jobs, or logs. *)

let group_cleanup_targets = [ "community_name"; "community_slug" ]

let group_query ~group_key =
  [
    ("group_type_index", string_of_int Analytics.community_group_type_index);
    ("group_key", group_key);
  ]

(* Which scrub targets the group still carries. Any other shape than the
   documented serializer output is a bounded parse error. *)
let parse_group_find_body body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error "malformed_group_response"
  | `Assoc fields -> (
      match List.assoc_opt "group_properties" fields with
      | Some (`Assoc props) ->
          Ok
            (List.filter
               (fun key -> List.mem key group_cleanup_targets)
               (List.map fst props))
      | Some `Null | None -> Ok []
      | Some _ -> Error "malformed_group_response")
  | _ -> Error "malformed_group_response"

let find_group ~config ~group_key =
  let uri =
    Uri.with_query'
      (Uri.of_string
         (Printf.sprintf "%s/api/projects/%s/groups/find/"
            config.Analytics.deletion_ui_host
            config.Analytics.deletion_project_id))
      (group_query ~group_key)
  in
  Cohttp_lwt_unix.Client.get
    ~headers:(auth_header config.Analytics.deletion_api_key)
    uri
  >>= fun (response, body) ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if status = 404 then
    (* PostHog never saw this group (analytics disabled, always-private, or
       nothing identified yet): there is nothing stored to remove. *)
    Cohttp_lwt.Body.drain_body body >|= fun () -> Ok `Absent
  else if status >= 200 && status < 300 then
    Cohttp_lwt.Body.to_string body >|= fun b ->
    Result.map (fun props -> `Props props) (parse_group_find_body b)
  else
    Cohttp_lwt.Body.drain_body body >|= fun () ->
    Error (Printf.sprintf "group_lookup_http_%d" status)

let delete_group_property ~config ~group_key ~property =
  let uri =
    Uri.with_query'
      (Uri.of_string
         (Printf.sprintf "%s/api/projects/%s/groups/delete_property/"
            config.Analytics.deletion_ui_host
            config.Analytics.deletion_project_id))
      (group_query ~group_key)
  in
  let headers =
    Cohttp.Header.add
      (auth_header config.Analytics.deletion_api_key)
      "content-type" "application/json"
  in
  let body =
    Cohttp_lwt.Body.of_string
      (Yojson.Safe.to_string (`Assoc [ ("$unset", `String property) ]))
  in
  Cohttp_lwt_unix.Client.post ~headers ~body uri
  >>= fun (response, response_body) ->
  Cohttp_lwt.Body.drain_body response_body >|= fun () ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if status >= 200 && status < 300 then Ok ()
  else Error (Printf.sprintf "group_delete_http_%d" status)

(* One bounded, idempotent scrub attempt: read the group, delete whichever
   scrub targets remain, sequentially. Absent group or no remaining targets
   completes immediately; the first failure leaves the job pending for the
   durable retry path. *)
let attempt_group_cleanup ~group_key =
  match Analytics.deletion_api_config () with
  | None -> Lwt.return (Error "missing_configuration")
  | Some config ->
      Lwt.catch
        (fun () ->
          with_timeout
            ( find_group ~config ~group_key >>= function
              | Error e -> Lwt.return (Error e)
              | Ok `Absent -> Lwt.return (Ok ())
              | Ok (`Props props) ->
                  Lwt_list.fold_left_s
                    (fun acc property ->
                      match acc with
                      | Error _ as e -> Lwt.return e
                      | Ok () ->
                          delete_group_property ~config ~group_key ~property)
                    (Ok ()) props )
          >|= function
          | `Done result -> result
          | `Timeout -> Error "timeout")
        (fun exn -> Lwt.return (Error (classify_exn exn)))

(* === Claimed-job orchestration (shared by both job kinds) ================ *)

let run_claimed ~log_label ~attempt ~mark_completed ~mark_failed =
  let%lwt outcome = attempt () in
  let mark label op =
    Lwt.catch
      (fun () ->
        let%lwt marked = op () in
        (match marked with
        | Ok () -> ()
        | Error e ->
            Logs.warn (fun m -> m "%s %s mark failed: %s" log_label label e));
        Lwt.return_unit)
      (fun exn ->
        Logs.warn (fun m ->
            m "%s %s mark raised: %s" log_label label (Printexc.to_string exn));
        Lwt.return_unit)
  in
  match outcome with
  | Ok () ->
      let%lwt () = mark "completed" mark_completed in
      Lwt.return `Completed
  | Error class_ ->
      let%lwt () = mark "failed" (fun () -> mark_failed class_) in
      Lwt.return (`Left_pending class_)

let process_claimed_job ~mark_completed ~mark_failed ~distinct_id =
  run_claimed ~log_label:"posthog deletion"
    ~attempt:(fun () -> attempt_person_deletion ~distinct_id)
    ~mark_completed ~mark_failed

let process_claimed_group_job ~mark_completed ~mark_failed ~group_key =
  run_claimed ~log_label:"posthog group cleanup"
    ~attempt:(fun () -> attempt_group_cleanup ~group_key)
    ~mark_completed ~mark_failed

type batch_summary = { claimed : int; completed : int; left_pending : int }

let run_batch ~run_one ~claim ~mark_completed ~mark_failed () =
  let%lwt claimed = claim () in
  match claimed with
  | Error e -> Lwt.return (Error e)
  | Ok jobs ->
      let%lwt outcomes =
        (* Sequential: one bounded HTTP attempt at a time, each mark its own
           short DB call — never a connection held across HTTP. *)
        Lwt_list.map_s
          (fun (job_id, payload) ->
            run_one
              ~mark_completed:(fun () -> mark_completed job_id)
              ~mark_failed:(fun err -> mark_failed job_id err)
              payload)
          jobs
      in
      let completed =
        List.length (List.filter (fun o -> o = `Completed) outcomes)
      in
      Lwt.return
        (Ok
           {
             claimed = List.length jobs;
             completed;
             left_pending = List.length jobs - completed;
           })

let process_batch ~claim ~mark_completed ~mark_failed () =
  run_batch
    ~run_one:(fun ~mark_completed ~mark_failed distinct_id ->
      process_claimed_job ~mark_completed ~mark_failed ~distinct_id)
    ~claim ~mark_completed ~mark_failed ()

let process_group_batch ~claim ~mark_completed ~mark_failed () =
  run_batch
    ~run_one:(fun ~mark_completed ~mark_failed group_key ->
      process_claimed_group_job ~mark_completed ~mark_failed ~group_key)
    ~claim ~mark_completed ~mark_failed ()
