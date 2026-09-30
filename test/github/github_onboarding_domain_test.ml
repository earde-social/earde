module GO = Earde.Github_onboarding

(* === GitHub onboarding domain: pure closed types (no DB, no IO) ===
   Exact-match parsing mirrors the schema enums; Revoked is a terminal
   installation status; revoked_at coherence mirrors the schema contract
   (forbidden on non-revoked rows, optional on revoked ones). *)

let go_account_str = function
  | GO.User -> "User"
  | GO.Organization -> "Organization"

let go_account_ok name expected input =
  Case.quick name (fun () ->
      match GO.account_type_of_string input with
      | Ok a ->
          Alcotest.(check string)
            "parsed account type" (go_account_str expected) (go_account_str a)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_account_err name input =
  Case.quick name (fun () ->
      match GO.account_type_of_string input with
      | Ok a -> Alcotest.failf "expected Error, got Ok %s" (go_account_str a)
      | Error _ -> ())

let go_status_str = function
  | GO.Active -> "Active"
  | GO.Revoked -> "Revoked"
  | GO.Inaccessible -> "Inaccessible"

let go_status_ok name expected input =
  Case.quick name (fun () ->
      match GO.installation_status_of_string input with
      | Ok s ->
          Alcotest.(check string)
            "parsed status" (go_status_str expected) (go_status_str s)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_status_err name input =
  Case.quick name (fun () ->
      match GO.installation_status_of_string input with
      | Ok s -> Alcotest.failf "expected Error, got Ok %s" (go_status_str s)
      | Error _ -> ())

let go_flow_ok name input =
  Case.quick name (fun () ->
      match GO.flow_of_string input with
      | Ok GO.Project_onboarding -> ()
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_flow_err name input =
  Case.quick name (fun () ->
      match GO.flow_of_string input with
      | Ok GO.Project_onboarding ->
          Alcotest.fail "expected Error, got Ok Project_onboarding"
      | Error _ -> ())

let go_transition name expected ~from_ ~to_ =
  Case.quick name (fun () ->
      Alcotest.(check bool)
        "allowed" expected
        (GO.installation_transition_allowed ~from_ ~to_))

let go_revoked_at name expected ~status ~has_revoked_at =
  Case.quick name (fun () ->
      Alcotest.(check bool)
        "allowed" expected
        (GO.revoked_at_allowed ~status ~has_revoked_at))

let suites =
  (* GitHub account type strings are exact-match: no trimming, no case
       folding — off-enum values are explicit errors. *)
  [
    ( "github_account_type",
      [
        go_account_ok "user parses" GO.User "user";
        go_account_ok "organization parses" GO.Organization "organization";
        go_account_err "blank rejected" "";
        go_account_err "unknown rejected" "bot";
        go_account_err "capitalized User rejected" "User";
        go_account_err "capitalized Organization rejected" "Organization";
        go_account_err "leading whitespace rejected" " user";
        go_account_err "trailing whitespace rejected" "organization ";
        Case.quick "User serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "user"
              (GO.string_of_account_type GO.User));
        Case.quick "Organization serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "organization"
              (GO.string_of_account_type GO.Organization));
      ] )
    (* Installation status parsing follows the same exact-match rules. *);
    ( "github_installation_status",
      [
        go_status_ok "active parses" GO.Active "active";
        go_status_ok "revoked parses" GO.Revoked "revoked";
        go_status_ok "inaccessible parses" GO.Inaccessible "inaccessible";
        go_status_err "blank rejected" "";
        go_status_err "unknown rejected" "suspended";
        go_status_err "capitalized rejected" "Active";
        go_status_err "uppercase rejected" "REVOKED";
        go_status_err "leading whitespace rejected" " active";
        go_status_err "trailing whitespace rejected" "inaccessible ";
        Case.quick "Active serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "active"
              (GO.string_of_installation_status GO.Active));
        Case.quick "Revoked serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "revoked"
              (GO.string_of_installation_status GO.Revoked));
        Case.quick "Inaccessible serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "inaccessible"
              (GO.string_of_installation_status GO.Inaccessible));
      ] )
    (* Full 3x3 matrix: Active and Inaccessible move freely (including the
       Inaccessible -> Active recovery); Revoked is terminal. *);
    ( "github_installation_transitions",
      [
        go_transition "active -> active" true ~from_:GO.Active ~to_:GO.Active;
        go_transition "active -> inaccessible" true ~from_:GO.Active
          ~to_:GO.Inaccessible;
        go_transition "active -> revoked" true ~from_:GO.Active ~to_:GO.Revoked;
        go_transition "inaccessible -> active recovers" true
          ~from_:GO.Inaccessible ~to_:GO.Active;
        go_transition "inaccessible -> inaccessible" true ~from_:GO.Inaccessible
          ~to_:GO.Inaccessible;
        go_transition "inaccessible -> revoked" true ~from_:GO.Inaccessible
          ~to_:GO.Revoked;
        go_transition "revoked -> active forbidden" false ~from_:GO.Revoked
          ~to_:GO.Active;
        go_transition "revoked -> inaccessible forbidden" false
          ~from_:GO.Revoked ~to_:GO.Inaccessible;
        go_transition "revoked -> revoked" true ~from_:GO.Revoked
          ~to_:GO.Revoked;
      ] )
    (* Onboarding flow: single closed value, exact-match, no fallback. *);
    ( "github_onboarding_flow",
      [
        go_flow_ok "project_onboarding parses" "project_onboarding";
        go_flow_err "blank rejected" "";
        go_flow_err "unknown rejected" "repo_onboarding";
        go_flow_err "case variant rejected" "Project_onboarding";
        go_flow_err "leading whitespace rejected" " project_onboarding";
        go_flow_err "trailing whitespace rejected" "project_onboarding ";
        Case.quick "Project_onboarding serializes" (fun () ->
            Alcotest.(check string)
              "canonical" "project_onboarding"
              (GO.string_of_flow GO.Project_onboarding));
      ] )
    (* Full status x has_revoked_at matrix: revoked_at is forbidden on
       non-revoked rows; a revoked row may have it or still lack it. *);
    ( "github_revoked_at_coherence",
      [
        go_revoked_at "active without revoked_at" true ~status:GO.Active
          ~has_revoked_at:false;
        go_revoked_at "active with revoked_at forbidden" false ~status:GO.Active
          ~has_revoked_at:true;
        go_revoked_at "inaccessible without revoked_at" true
          ~status:GO.Inaccessible ~has_revoked_at:false;
        go_revoked_at "inaccessible with revoked_at forbidden" false
          ~status:GO.Inaccessible ~has_revoked_at:true;
        go_revoked_at "revoked without revoked_at (newly marked)" true
          ~status:GO.Revoked ~has_revoked_at:false;
        go_revoked_at "revoked with revoked_at" true ~status:GO.Revoked
          ~has_revoked_at:true;
      ] );
  ]
