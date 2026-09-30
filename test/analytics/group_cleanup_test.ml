module AnT = Earde.Analytics.For_testing
let ( let* ) = Lwt.bind

(* --- §13: durable private-community group-profile scrub ------------------ *)

(* When a community turns fully private, its previously sent community_name /
   community_slug must actually be REMOVED from the PostHog group profile via
   the documented private Groups API (find + delete_property {"$unset": …}).
   Stub-only cases run always; handler/job cases sit behind the DB gate. *)

open Caqti_request.Infix

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

let group_find_path = "/api/projects/42/groups/find/"

let group_delete_path = "/api/projects/42/groups/delete_property/"

(* Human-readable stand-ins that must NEVER appear in requests (beyond the
   find response we serve), job rows, or error strings. *)
let secret_name = "Secret Club"

let secret_slug = "secret-club"

let group_props_body props =
  Printf.sprintf {|{"group_type_index": 0, "group_key": "k", "group_properties": {%s}}|}
    (String.concat ", "
       (List.map (fun (k, v) -> Printf.sprintf "%S: %S" k v) props))

let unset_of_body body =
  match Yojson.Safe.from_string body with
  | exception _ -> None
  | `Assoc l -> (
      match List.assoc_opt "$unset" l with
      | Some (`String k) -> Some k
      | _ -> None)
  | _ -> None

(* Stateful stub: find serves the current [props]; a successful
   delete_property removes the named key, mirroring real PostHog (which
   400s on an absent key — exactly why the client re-reads before
   deleting). *)
let groups_handler ?(find_status = 200) ?(delete_status = 200) props
    (req : Posthog_persons_stub.req) =
  if req.auth <> Some ("Bearer " ^ Posthog_persons_stub.deletion_test_key) then
    (401, {|{"type":"authentication_error"}|})
  else if req.meth = "GET" && req.path = group_find_path then
    if find_status <> 200 then (find_status, {|{"detail":"not found"}|})
    else (200, group_props_body !props)
  else if req.meth = "POST" && req.path = group_delete_path then (
    match unset_of_body req.body with
    | Some key when delete_status = 200 && List.mem_assoc key !props ->
        props := List.remove_assoc key !props;
        (200, "{}")
    | Some key when delete_status <> 200 ->
        ignore key;
        (delete_status, "")
    | _ -> (400, {|{"attr":"$unset"}|}))
  else (404, {|{"detail":"not found"}|})

let with_groups_stub handler f =
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* base_url, seen, stop = Posthog_persons_stub.start handler in
     AnT.use_deletion_test_configuration ~ui_host:base_url
       ~project_id:(Some "42") ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
     Lwt.finalize
       (fun () -> f ~seen)
       (fun () ->
         AnT.clear_configuration_override ();
         stop ();
         Lwt.return_unit))

let no_leak_in_requests (seen : Posthog_persons_stub.req list) =
  List.iter
    (fun (r : Posthog_persons_stub.req) ->
      let surface =
        r.path ^ " " ^ r.body ^ " "
        ^ String.concat " " (List.concat_map snd r.query)
      in
      if Html_assert.contains surface secret_name || Html_assert.contains surface secret_slug then
        Alcotest.failf "request leaked a community name/slug: %s" r.path)
    seen

let full_props () =
  ref
    [ ("community_id", "9"); ("community_name", secret_name);
      ("community_slug", secret_slug); ("community_visibility", "private")
    ]

let scrub_case =
  Analytics_fixture.an_case "attempt: deletes exactly name+slug via $unset; rerun is a no-op"
    (fun () ->
      let props = full_props () in
      with_groups_stub (groups_handler props) (fun ~seen ->
          let ( let* ) = Lwt.bind in
          let* first =
            Earde.Posthog_deletion.attempt_group_cleanup
              ~group_key:"community:9"
          in
          Alcotest.(check (result unit string)) "first run" (Ok ()) first;
          let deletes =
            List.filter
              (fun (r : Posthog_persons_stub.req) -> r.meth = "POST")
              !seen
          in
          Alcotest.(check (slist (option string) compare))
            "exactly the two closed scrub targets"
            [ Some "community_name"; Some "community_slug" ]
            (List.map (fun (r : Posthog_persons_stub.req) -> unset_of_body r.body) deletes);
          List.iter
            (fun (r : Posthog_persons_stub.req) ->
              Alcotest.(check (option (list string)))
                "delete addressed by numeric key" (Some [ "community:9" ])
                (List.assoc_opt "group_key" r.query);
              Alcotest.(check (option (list string)))
                "community group type index" (Some [ "0" ])
                (List.assoc_opt "group_type_index" r.query))
            deletes;
          (* Non-target properties survive; the group key itself stays. *)
          Alcotest.(check (list string)) "untouched non-targets"
            [ "community_id"; "community_visibility" ]
            (List.map fst !props);
          (* Idempotent rerun: nothing left to delete → find only. *)
          let before = List.length !seen in
          let* second =
            Earde.Posthog_deletion.attempt_group_cleanup
              ~group_key:"community:9"
          in
          Alcotest.(check (result unit string)) "rerun" (Ok ()) second;
          let extra =
            List.filteri (fun i _ -> i >= before) !seen
          in
          Alcotest.(check (list string)) "rerun performs find only"
            [ "GET" ]
            (List.map (fun (r : Posthog_persons_stub.req) -> r.meth) extra);
          no_leak_in_requests deletes;
          Lwt.return_unit))

let absent_case =
  Analytics_fixture.an_case "attempt: group PostHog never saw (find 404) completes" (fun () ->
      let props = full_props () in
      with_groups_stub (groups_handler ~find_status:404 props) (fun ~seen ->
          let ( let* ) = Lwt.bind in
          let* r =
            Earde.Posthog_deletion.attempt_group_cleanup
              ~group_key:"community:9"
          in
          Alcotest.(check (result unit string)) "absent completes" (Ok ()) r;
          Alcotest.(check (list string)) "no delete attempted" [ "GET" ]
            (List.map (fun (q : Posthog_persons_stub.req) -> q.meth) !seen);
          Lwt.return_unit))

let failure_case =
  Analytics_fixture.an_case "attempt: delete failure is a bounded class, never a name/slug"
    (fun () ->
      let props = full_props () in
      with_groups_stub (groups_handler ~delete_status:500 props)
        (fun ~seen ->
          let ( let* ) = Lwt.bind in
          let* r =
            Earde.Posthog_deletion.attempt_group_cleanup
              ~group_key:"community:9"
          in
          (match r with
          | Error cls ->
              Alcotest.(check string) "closed class" "group_delete_http_500"
                cls
          | Ok () -> Alcotest.fail "expected failure");
          ignore seen;
          Lwt.return_unit))

let missing_config_case =
  Analytics_fixture.an_case "attempt: no private credentials -> bounded missing_configuration"
    (fun () ->
      AnT.use_enabled_test_configuration ();
      Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
          Alcotest.(check (result unit string))
            "missing configuration"
            (Error "missing_configuration")
            (Lwt_main.run
               (Earde.Posthog_deletion.attempt_group_cleanup
                  ~group_key:"community:9"))))

(* ---- DB-gated: transaction coupling, durability, retry, restore ------- *)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || c.id::text FROM communities c WHERE c.slug LIKE 'grpclean-%')"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key LIKE 'community:99912%'"
    ; "DELETE FROM communities WHERE slug LIKE 'grpclean-%'"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE NOT EXISTS (SELECT 1 FROM communities c WHERE 'community:' || c.id::text = posthog_group_cleanup_jobs.group_key)"
    ; "DELETE FROM users WHERE username LIKE 'grpclean_%'"
    ]

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let ( let* ) = Lwt.bind in
             let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* _ = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url conn (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_community =
  (Caqti_type.(t3 string string string) ->! Caqti_type.int)
  "INSERT INTO communities (slug, name, visibility) VALUES ($1, $2, $3) RETURNING id"

let q_visibility_of =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT visibility FROM communities WHERE id = $1"

let q_insert_fake_job =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO posthog_group_cleanup_jobs (group_key) VALUES ($1) RETURNING id"

(* Runs update_community_visibility_handler like Step-7 runs the deletion
   handler: caller-chosen analytics configuration (so the async cleanup can
   reach a stub), completion awaited through [done_pred]. *)
let run_visibility ~url ~configure ~uid ~slug ~value ~done_pred () =
  let ( let* ) = Lwt.bind in
  let payloads = ref [] in
  configure ();
  AnT.set_capture_sink (fun p -> payloads := !payloads @ [ p ]);
  Lwt.finalize
    (fun () ->
      let router =
        Dream.router
          [ Dream.post "/c/:slug/settings/visibility"
              Earde.Handlers.update_community_visibility_handler
          ]
      in
      let pipeline =
        Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
        let* () = Dream.set_session_field req "user_id" (string_of_int uid) in
        let* () = Dream.set_session_field req "username" "grpclean_admin" in
        let* () = Dream.set_session_field req "is_admin" "true" in
        let csrf = Dream.csrf_token req in
        Dream.set_body req
          (Http_fixture.encoded_form_body
             [ ("dream.csrf", csrf); ("visibility", value) ]);
        router req
      in
      let request =
        Dream.request ~method_:`POST
          ~target:("/c/" ^ slug ^ "/settings/visibility")
          ~headers:
            ([ ("Content-Type", "application/x-www-form-urlencoded") ]
            @ Analytics_fixture.consent_header (Some "granted"))
          ""
      in
      let* response = pipeline request in
      let* () = Analytics_fixture.wait_until ~label:"group cleanup chain" done_pred in
      Lwt.return (Dream.status_to_int (Dream.status response), !payloads))
    (fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ();
      Lwt.return_unit)

let job_state conn key =
  let ( let* ) = Lwt.bind in
  let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
  or_fail_s "job state" job

let job_completed conn key () =
  let ( let* ) = Lwt.bind in
  let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
  match job with
  | Ok (Some (_, "completed", _, _)) -> Lwt.return true
  | _ -> Lwt.return false

let job_has_error conn key () =
  let ( let* ) = Lwt.bind in
  let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
  match job with
  | Ok (Some (_, _, _, Some _)) -> Lwt.return true
  | _ -> Lwt.return false

let handler_flow_case =
  db_case "handler: public->private commits change + durable job; scrub runs"
    (fun ~url conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("grpclean_admin", "x") in
      let* uid = or_fail "user" uid in
      (* The visibility route authorizes on the DURABLE users.is_admin
         row; the session claim only enables that lookup. *)
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      let* cid =
        C.find q_insert_community ("grpclean-flow", secret_name, "public")
      in
      let* cid = or_fail "community" cid in
      let key = "community:" ^ string_of_int cid in
      let props =
        ref
          [ ("community_id", string_of_int cid);
            ("community_name", secret_name);
            ("community_slug", "grpclean-flow")
          ]
      in
      let* base_url, seen, stop = Posthog_persons_stub.start (groups_handler props) in
      Lwt.finalize
        (fun () ->
          let* status, payloads =
            run_visibility ~url
              ~configure:(fun () ->
                AnT.use_deletion_test_configuration ~ui_host:base_url
                  ~project_id:(Some "42")
                  ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ())
              ~uid ~slug:"grpclean-flow" ~value:"private"
              ~done_pred:(job_completed conn key) ()
          in
          Alcotest.(check bool) "redirects" true (status / 100 = 3);
          let* visibility = C.find q_visibility_of cid in
          let* visibility = or_fail "visibility" visibility in
          Alcotest.(check string) "visibility committed" "private" visibility;
          let* state = job_state conn key in
          (match state with
          | Some (_, s, attempts, last_error) ->
              Alcotest.(check string) "job completed" "completed" s;
              Alcotest.(check bool) "attempted" true (attempts >= 1);
              Alcotest.(check (option string)) "no error" None last_error
          | None -> Alcotest.fail "job row missing");
          (* Both stored identifiers really were unset over the API. *)
          Alcotest.(check (list string)) "profile scrubbed"
            [ "community_id" ]
            (List.map fst !props);
          no_leak_in_requests
            (List.filter (fun (r : Posthog_persons_stub.req) -> r.meth = "POST") !seen);
          (* The consent-gated $groupidentify that rode along is already
             the private shape. *)
          (match
             List.find_opt
               (fun p -> Analytics_fixture.event_of p = "$groupidentify")
               payloads
           with
          | Some gi ->
              Alcotest.(check (slist string compare)) "private group set"
                [ "community_id"; "community_visibility" ]
                (List.map fst (Analytics_fixture.group_set_of gi))
          | None -> Alcotest.fail "no $groupidentify captured");
          Lwt.return_unit)
        (fun () ->
          stop ();
          Lwt.return_unit))

let handler_failure_case =
  db_case "handler: PostHog failure keeps the committed change, job pending"
    (fun ~url conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("grpclean_admin2", "x") in
      let* uid = or_fail "user" uid in
      (* The visibility route authorizes on the DURABLE users.is_admin
         row; the session claim only enables that lookup. *)
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      let* cid =
        C.find q_insert_community ("grpclean-down", secret_name, "public")
      in
      let* cid = or_fail "community" cid in
      let key = "community:" ^ string_of_int cid in
      let props =
        ref
          [ ("community_name", secret_name);
            ("community_slug", "grpclean-down")
          ]
      in
      let* base_url, _seen, stop =
        Posthog_persons_stub.start (groups_handler ~delete_status:500 props)
      in
      Lwt.finalize
        (fun () ->
          let* status, _payloads =
            run_visibility ~url
              ~configure:(fun () ->
                AnT.use_deletion_test_configuration ~ui_host:base_url
                  ~project_id:(Some "42")
                  ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ())
              ~uid ~slug:"grpclean-down" ~value:"private"
              ~done_pred:(job_has_error conn key) ()
          in
          Alcotest.(check bool) "product response preserved" true
            (status / 100 = 3);
          (* The transient PostHog failure did NOT roll anything back. *)
          let* visibility = C.find q_visibility_of cid in
          let* visibility = or_fail "visibility" visibility in
          Alcotest.(check string) "visibility still private" "private"
            visibility;
          let* state = job_state conn key in
          (match state with
          | Some (_, s, attempts, Some err) ->
              Alcotest.(check string) "durably pending" "pending" s;
              Alcotest.(check bool) "attempted" true (attempts >= 1);
              Alcotest.(check string) "bounded class only"
                "group_delete_http_500" err;
              if Html_assert.contains err secret_name || Html_assert.contains err "grpclean-down"
              then Alcotest.fail "error leaked a name/slug"
          | _ -> Alcotest.fail "expected pending job with error");
          Lwt.return_unit)
        (fun () ->
          stop ();
          Lwt.return_unit))

let enqueue_semantics_case =
  db_case "enqueue: converges while pending, re-arms after completion"
    (fun ~url:_ conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* cid =
        C.find q_insert_community ("grpclean-rearm", secret_name, "public")
      in
      let* cid = or_fail "community" cid in
      let key = "community:" ^ string_of_int cid in
      let* r =
        Earde.Db.update_community_visibility_and_enqueue_group_cleanup conn
          cid Earde.Db.Community_private
      in
      let* updated, job1 = or_fail_s "first transition" r in
      Alcotest.(check bool) "community returned" true (updated <> None);
      let job1 = Option.get job1 in
      (* Duplicate transition converges on the SAME pending job. *)
      let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
          conn cid Earde.Db.Community_private
      in
      let* _, job2 = or_fail_s "duplicate transition" r in
      Alcotest.(check int) "same job" job1 (Option.get job2);
      (* ->public enqueues nothing (restore rides $groupidentify). *)
      let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
          conn cid Earde.Db.Community_public
      in
      let* _, job3 = or_fail_s "back to public" r in
      Alcotest.(check bool) "no job on ->public" true (job3 = None);
      (* Complete, then a NEW ->private transition re-arms it. *)
      let* m = Earde.Db.complete_posthog_group_cleanup_job conn job1 in
      let* () = or_fail_s "complete" m in
      let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
          conn cid Earde.Db.Community_private
      in
      let* _, job4 = or_fail_s "re-arm" r in
      Alcotest.(check int) "same row re-armed" job1 (Option.get job4);
      let* state = job_state conn key in
      (match state with
      | Some (_, s, attempts, last_error) ->
          Alcotest.(check string) "pending again" "pending" s;
          Alcotest.(check int) "counters reset" 0 attempts;
          Alcotest.(check (option string)) "diagnostics reset" None last_error
      | None -> Alcotest.fail "job row missing");
      Lwt.return_unit)

let restore_case =
  db_case "handler: private->public restores name+slug, enqueues nothing"
    (fun ~url conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("grpclean_admin3", "x") in
      let* uid = or_fail "user" uid in
      (* The visibility route authorizes on the DURABLE users.is_admin
         row; the session claim only enables that lookup. *)
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      let* cid =
        C.find q_insert_community ("grpclean-back", secret_name, "private")
      in
      let* cid = or_fail "community" cid in
      let key = "community:" ^ string_of_int cid in
      let* status, payloads =
        run_visibility ~url
          ~configure:(fun () -> AnT.use_enabled_test_configuration ())
          ~uid ~slug:"grpclean-back" ~value:"public"
          ~done_pred:(fun () -> Lwt.return true)
          ()
      in
      Alcotest.(check bool) "redirects" true (status / 100 = 3);
      (match
         List.find_opt (fun p -> Analytics_fixture.event_of p = "$groupidentify") payloads
       with
      | Some gi ->
          let set = Analytics_fixture.group_set_of gi in
          Alcotest.(check (slist string compare))
            "public shape restored"
            [ "community_id"; "community_slug"; "community_name";
              "community_visibility" ]
            (List.map fst set);
          Alcotest.(check (option string)) "name restored"
            (Some secret_name)
            (match List.assoc_opt "community_name" set with
             | Some (`String v) -> Some v
             | _ -> None)
      | None -> Alcotest.fail "no $groupidentify captured");
      let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
      let* job = or_fail_s "job lookup" job in
      Alcotest.(check bool) "no cleanup job for ->public" true (job = None);
      Lwt.return_unit)

let batch_case =
  db_case "batch: bounded claim + shared maintenance worker completes"
    (fun ~url:_ conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* j1 = C.find q_insert_fake_job "community:999121" in
      let* j1 = or_fail "job1" j1 in
      let* j2 = C.find q_insert_fake_job "community:999122" in
      let* j2 = or_fail "job2" j2 in
      let* j3 = C.find q_insert_fake_job "community:999123" in
      let* j3 = or_fail "job3" j3 in
      let* claimed =
        Earde.Db.claim_posthog_group_cleanup_batch conn ~limit:2 ()
      in
      let* claimed = or_fail_s "bounded claim" claimed in
      Alcotest.(check (list int)) "bounded, oldest first" [ j1; j2 ]
        (List.map fst claimed);
      (* The third job processes through the SAME worker; a 404 find (group
         never existed) completes it. *)
      let props = ref [] in
      let handler = groups_handler ~find_status:404 props in
      let* base_url, _seen, stop = Posthog_persons_stub.start handler in
      AnT.use_deletion_test_configuration ~ui_host:base_url
        ~project_id:(Some "42") ~personal_api_key:(Some Posthog_persons_stub.deletion_test_key) ();
      Lwt.finalize
        (fun () ->
          let* summary =
            Earde.Posthog_deletion.process_group_batch
              ~claim:(fun () ->
                Earde.Db.claim_posthog_group_cleanup_batch conn
                  ~lease_minutes:0 ~limit:10 ())
              ~mark_completed:(fun job_id ->
                Earde.Db.complete_posthog_group_cleanup_job conn job_id)
              ~mark_failed:(fun job_id err ->
                Earde.Db.fail_posthog_group_cleanup_job conn job_id err)
              ()
          in
          let* summary = or_fail_s "batch" summary in
          Alcotest.(check int) "all pending processed" 3
            summary.Earde.Posthog_deletion.claimed;
          Alcotest.(check int) "all completed" 3
            summary.Earde.Posthog_deletion.completed;
          let* state = job_state conn "community:999123" in
          (match state with
          | Some (id, s, _, _) ->
              Alcotest.(check int) "third job" j3 id;
              Alcotest.(check string) "completed" "completed" s
          | None -> Alcotest.fail "job row missing");
          Lwt.return_unit)
        (fun () ->
          AnT.clear_configuration_override ();
          stop ();
          Lwt.return_unit))

let suite =
  [ scrub_case; absent_case; failure_case; missing_config_case
  ; handler_flow_case; handler_failure_case; enqueue_semantics_case
  ; restore_case; batch_case
  ]

let suites =
  [ ( "posthog_group_cleanup", suite )
  ]
