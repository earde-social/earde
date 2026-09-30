module GTE = Earde.Github_oauth_token_exchange
module GUI = Earde.Github_user_installations
module GUR = Earde.Github_user_installation_repositories

(* === Callback failure diagnostics (Github_onboarding_diagnostics) ===
   The callback answers one opaque redirect for every cause, so the server
   log is the only place a production failure can be identified. Every
   classification must produce its exact stable label — operators grep
   these — and the type must remain structurally incapable of carrying
   anything secret: the cases below pin both. *)

module D = Earde.Github_onboarding_diagnostics
module GOS = Earde.Github_onboarding_state_store
module GIS = Earde.Github_installation_store
module PODS = Earde.Project_onboarding_draft_store

let case = Case.quick
let user_context = { D.installation_id = 149256567L; account_type = GUI.User }

let organization_context =
  { D.installation_id = 149347874L; account_type = GUI.Organization }

let check label expected reason =
  Alcotest.(check string) label expected (D.describe reason)

let stage_cases =
  case "every stage produces its exact label" (fun () ->
      List.iter
        (fun (expected, reason) -> check expected expected reason)
        [
          ( "stage=callback_parse reason=malformed_callback",
            D.Malformed_callback );
          ("stage=configuration reason=unavailable", D.Configuration_unavailable);
          ("stage=flow_cookie reason=missing", D.Cookie_missing);
          ("stage=flow_cookie reason=invalid", D.Cookie_invalid);
          ( "stage=authorization reason=rejected_at_github",
            D.Authorization_rejected );
          ("stage=credentials reason=unavailable", D.Credentials_unavailable);
        ])

let state_cases =
  case "state consumption causes stay distinguishable in the log" (fun () ->
      List.iter
        (fun (expected, error) ->
          check expected
            ("stage=state reason=" ^ expected)
            (D.State_rejected error))
        [
          ("state_not_found", GOS.State_not_found);
          ("state_expired", GOS.State_expired);
          ("state_already_consumed", GOS.State_already_consumed);
          ("session_binding_mismatch", GOS.Session_binding_mismatch);
          ("flow_mismatch", GOS.Flow_mismatch);
          ("missing_pending_installation", GOS.Missing_pending_installation);
          ("storage_error", GOS.Storage_error);
        ])

let exchange_case =
  case "token exchange carries the pending id and any HTTP status" (fun () ->
      let reason error =
        D.Token_exchange_failed { pending_installation_id = 149347874L; error }
      in
      check "transport"
        "stage=token_exchange reason=transport_error installation=149347874"
        (reason GTE.Transport_error);
      check "status"
        "stage=token_exchange reason=unexpected_http_status status=502 \
         installation=149347874"
        (reason (GTE.Unexpected_http_status 502));
      check "rejected"
        "stage=token_exchange reason=oauth_rejected installation=149347874"
        (reason GTE.OAuth_rejected);
      check "invalid"
        "stage=token_exchange reason=invalid_response installation=149347874"
        (reason GTE.Invalid_response))

let verification_case =
  case "verification carries the still-unverified pending id" (fun () ->
      let reason error =
        D.Installation_verification_failed
          { pending_installation_id = 149347874L; error }
      in
      check "not accessible"
        "stage=installation_verification reason=installation_not_accessible \
         installation=149347874"
        (reason GUI.Installation_not_accessible);
      check "status"
        "stage=installation_verification reason=unexpected_http_status \
         status=403 installation=149347874"
        (reason (GUI.Unexpected_http_status 403));
      check "invalid id"
        "stage=installation_verification reason=invalid_installation_id \
         installation=149347874"
        (reason GUI.Invalid_installation_id);
      check "transport"
        "stage=installation_verification reason=transport_error \
         installation=149347874"
        (reason GUI.Transport_error);
      check "invalid response"
        "stage=installation_verification reason=invalid_response \
         installation=149347874"
        (reason GUI.Invalid_response);
      check "pagination"
        "stage=installation_verification reason=pagination_limit \
         installation=149347874"
        (reason GUI.Pagination_limit))

(* The exact line the private-repository-only organization installation
   produces: the class of failure plus the two identifiers needed to tell
   which installation and which account kind it was. *)
let listing_case =
  case "repository listing names the account kind, not the repository"
    (fun () ->
      let reason ?(installation = organization_context) error =
        D.Repository_listing_failed { installation; error }
      in
      check "no public repositories"
        "stage=repository_listing reason=no_public_repositories \
         installation=149347874 account_type=organization"
        (reason GUR.No_public_repositories);
      check "personal account prints the other kind"
        "stage=repository_listing reason=no_public_repositories \
         installation=149256567 account_type=user"
        (reason ~installation:user_context GUR.No_public_repositories);
      check "status"
        "stage=repository_listing reason=unexpected_http_status status=404 \
         installation=149347874 account_type=organization"
        (reason (GUR.Unexpected_http_status 404));
      check "transport"
        "stage=repository_listing reason=transport_error \
         installation=149347874 account_type=organization"
        (reason GUR.Transport_error);
      check "invalid"
        "stage=repository_listing reason=invalid_response \
         installation=149347874 account_type=organization"
        (reason GUR.Invalid_response);
      check "pagination"
        "stage=repository_listing reason=pagination_limit \
         installation=149347874 account_type=organization"
        (reason GUR.Pagination_limit))

let persistence_case =
  case "both persistence steps are distinguishable" (fun () ->
      check "installation store"
        "stage=installation_persistence reason=installation_unavailable \
         installation=149347874 account_type=organization"
        (D.Installation_persistence_failed
           {
             installation = organization_context;
             error = GIS.Installation_unavailable;
           });
      check "installation storage"
        "stage=installation_persistence reason=storage_error \
         installation=149347874 account_type=organization"
        (D.Installation_persistence_failed
           { installation = organization_context; error = GIS.Storage_error });
      check "installation user id"
        "stage=installation_persistence reason=invalid_connected_by_user_id \
         installation=149347874 account_type=organization"
        (D.Installation_persistence_failed
           {
             installation = organization_context;
             error = GIS.Invalid_connected_by_user_id;
           });
      check "draft store"
        "stage=draft_persistence reason=storage_error installation=149347874 \
         account_type=organization"
        (D.Draft_persistence_failed
           { installation = organization_context; error = PODS.Storage_error });
      check "draft installation"
        "stage=draft_persistence reason=installation_unavailable \
         installation=149347874 account_type=organization"
        (D.Draft_persistence_failed
           {
             installation = organization_context;
             error = PODS.Installation_unavailable;
           });
      check "draft user id"
        "stage=draft_persistence reason=invalid_user_id installation=149347874 \
         account_type=organization"
        (D.Draft_persistence_failed
           { installation = organization_context; error = PODS.Invalid_user_id }))

(* Whatever the cause, a label is one line of [key=value] pairs drawn
   from a closed vocabulary plus integers — never free text that could
   have come from a request, a cookie, or a GitHub response. *)
let shape_case =
  case "every label is one safe key=value line" (fun () ->
      List.iter
        (fun reason ->
          let label = D.describe reason in
          Alcotest.(check bool)
            "single line" false
            (String.exists (fun c -> c = '\n' || c = '\r') label);
          Alcotest.(check bool)
            "safe alphabet" true
            (String.for_all
               (function
                 | 'a' .. 'z' | '0' .. '9' | '_' | '=' | ' ' -> true
                 | _ -> false)
               label);
          Alcotest.(check bool)
            "starts with the stage key" true
            (String.length label > 6 && String.sub label 0 6 = "stage="))
        [
          D.Malformed_callback;
          D.Configuration_unavailable;
          D.Cookie_missing;
          D.Cookie_invalid;
          D.Authorization_rejected;
          D.Credentials_unavailable;
          D.State_rejected GOS.Session_binding_mismatch;
          D.Token_exchange_failed
            {
              pending_installation_id = 1L;
              error = GTE.Unexpected_http_status 500;
            };
          D.Installation_verification_failed
            {
              pending_installation_id = 1L;
              error = GUI.Installation_not_accessible;
            };
          D.Repository_listing_failed
            {
              installation = organization_context;
              error = GUR.No_public_repositories;
            };
          D.Installation_persistence_failed
            { installation = user_context; error = GIS.Storage_error };
          D.Draft_persistence_failed
            { installation = user_context; error = PODS.Storage_error };
        ])

let suite =
  [
    stage_cases;
    state_cases;
    exchange_case;
    verification_case;
    listing_case;
    persistence_case;
    shape_case;
  ]

let suites =
  (* The callback's single opaque redirect is only diagnosable through
       the server log: every failure class must produce its exact stable
       label, and no label may carry anything but closed vocabulary and
       integers (see Ghd). *)
  [ ("github_callback_failure_diagnostics", suite) ]
