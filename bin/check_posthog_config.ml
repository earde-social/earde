(* PostHog configuration preflight (analytics spec: deployment environments).
   Verifies — without ingesting any event — that this deployment's analytics
   configuration points at the PostHog project it claims to: the environment
   parses through the closed model, the activation rules hold, POSTHOG_PROJECT_ID
   exists on the configured region host, and its live api_token equals
   POSTHOG_PROJECT_TOKEN. Run BEFORE migration/restart during production and
   staging deployments:

     POSTHOG_ENABLED=... EARDE_DEPLOYMENT_ENVIRONMENT=... dune exec bin/check_posthog_config.exe

   Exit 0 only when the configured project is verified; nonzero otherwise.
   Output never contains the project token, the personal API key, or any
   response body — only ids, normalized hosts, a bounded failure class and a
   non-reversible token fingerprint. *)

let () =
  (* Surface the central parser's startup diagnostics on stderr so an invalid
     configuration explains itself. *)
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs.format_reporter ());
  match Lwt_main.run (Earde.Analytics.Preflight.run ()) with
  | Ok report ->
      let open Earde.Analytics.Preflight in
      Printf.printf "PostHog configuration preflight: VERIFIED\n";
      Printf.printf "  deployment environment : %s\n"
        (Earde.Analytics.deployment_environment_to_string
           report.report_environment);
      Printf.printf "  project id             : %s\n" report.report_project_id;
      Printf.printf "  api host               : %s\n" report.report_api_host;
      Printf.printf "  ui host                : %s\n" report.report_ui_host;
      Printf.printf "  project token          : sha256:%s (fingerprint only)\n"
        report.report_token_fingerprint;
      List.iter (fun note -> Printf.printf "  - %s\n" note) report.report_notes;
      exit 0
  | Error (failure_class, detail) ->
      Printf.eprintf "PostHog configuration preflight: FAILED (%s)\n"
        failure_class;
      Printf.eprintf "  %s\n" detail;
      exit 1
