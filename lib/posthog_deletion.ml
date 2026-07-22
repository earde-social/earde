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
       (function 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' | '-' -> true | _ -> false)
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
            config.Analytics.deletion_ui_host config.Analytics.deletion_project_id))
      [ ("distinct_id", distinct_id) ]
  in
  Cohttp_lwt_unix.Client.get ~headers:(auth_header config.Analytics.deletion_api_key) uri
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
            config.Analytics.deletion_ui_host config.Analytics.deletion_project_id
            uuid))
      [ ("delete_events", "true") ]
  in
  Cohttp_lwt_unix.Client.delete
    ~headers:(auth_header config.Analytics.deletion_api_key) uri
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

let process_claimed_job ~mark_completed ~mark_failed ~distinct_id =
  let%lwt outcome = attempt_person_deletion ~distinct_id in
  let mark label op =
    Lwt.catch
      (fun () ->
        let%lwt marked = op () in
        (match marked with
        | Ok () -> ()
        | Error e ->
            Logs.warn (fun m -> m "posthog deletion %s mark failed: %s" label e));
        Lwt.return_unit)
      (fun exn ->
        Logs.warn (fun m ->
            m "posthog deletion %s mark raised: %s" label
              (Printexc.to_string exn));
        Lwt.return_unit)
  in
  match outcome with
  | Ok () ->
      let%lwt () = mark "completed" mark_completed in
      Lwt.return `Completed
  | Error class_ ->
      let%lwt () = mark "failed" (fun () -> mark_failed class_) in
      Lwt.return (`Left_pending class_)

type batch_summary = { claimed : int; completed : int; left_pending : int }

let process_batch ~claim ~mark_completed ~mark_failed () =
  let%lwt claimed = claim () in
  match claimed with
  | Error e -> Lwt.return (Error e)
  | Ok jobs ->
      let%lwt outcomes =
        (* Sequential: one bounded HTTP attempt at a time, each mark its own
           short DB call — never a connection held across HTTP. *)
        Lwt_list.map_s
          (fun (job_id, distinct_id) ->
            process_claimed_job
              ~mark_completed:(fun () -> mark_completed job_id)
              ~mark_failed:(fun err -> mark_failed job_id err)
              ~distinct_id)
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
