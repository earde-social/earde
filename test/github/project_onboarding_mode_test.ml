module Ob = Earde.Project_onboarding

(* GitHub onboarding configuration mode — pure parsing/serialization/policy.
   The parser is tested directly on string options; the process environment is
   never mutated. *)

let ob_mode_str = function
  | Ob.Off -> "off"
  | Ob.Admins -> "admins"
  | Ob.Public -> "public"

let check_ob_parse name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string)
        name (ob_mode_str expected)
        (ob_mode_str (Ob.mode_of_string raw)))

let check_ob_string name expected mode =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (Ob.mode_to_string mode))

let check_ob_avail name expected mode ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        name expected
        (Ob.onboarding_available mode ~is_admin))

(* Legacy community-creation gate — pure request decisions, exercised directly
   so no Dream server or session middleware is needed. Anonymous visitors and
   authenticated non-admins both carry is_admin:false (the session flag is only
   ever set to "true" for authenticated global admins). *)
let ob_decision_str = function
  | Ob.Show_form -> "show_form"
  | Ob.Redirect_to_bring -> "redirect_to_bring"
  | Ob.Forbid -> "forbid"

let check_ob_legacy name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool)
        name expected
        (Ob.can_use_legacy_community_creation ~is_admin))

let check_ob_legacy_get name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string)
        name (ob_decision_str expected)
        (ob_decision_str (Ob.legacy_creation_get_decision ~is_admin)))

let check_ob_legacy_post name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string)
        name (ob_decision_str expected)
        (ob_decision_str (Ob.legacy_creation_post_decision ~is_admin)))

let suites =
  (* GitHub onboarding mode parsing: exact canonical values after trimming,
       everything else fails closed to Off. *)
  [
    ( "project_onboarding_parse",
      [
        check_ob_parse "none" Ob.Off None;
        check_ob_parse "empty string" Ob.Off (Some "");
        check_ob_parse "whitespace only" Ob.Off (Some "   \t ");
        check_ob_parse "off" Ob.Off (Some "off");
        check_ob_parse "admins" Ob.Admins (Some "admins");
        check_ob_parse "public" Ob.Public (Some "public");
        check_ob_parse "trims surrounding whitespace" Ob.Admins
          (Some "  admins ");
        check_ob_parse "trims around public" Ob.Public (Some "\tpublic\n");
        check_ob_parse "uppercase PUBLIC rejected" Ob.Off (Some "PUBLIC");
        check_ob_parse "mixed case Admin rejected" Ob.Off (Some "Admin");
        check_ob_parse "unknown value" Ob.Off (Some "on");
        check_ob_parse "unknown word" Ob.Off (Some "everyone");
      ] );
    ( "project_onboarding_to_string",
      [
        check_ob_string "off" "off" Ob.Off;
        check_ob_string "admins" "admins" Ob.Admins;
        check_ob_string "public" "public" Ob.Public;
      ] );
    ( "project_onboarding_available",
      [
        check_ob_avail "off rejects admin" false Ob.Off ~is_admin:true;
        check_ob_avail "off rejects non-admin" false Ob.Off ~is_admin:false;
        check_ob_avail "admins accepts admin" true Ob.Admins ~is_admin:true;
        check_ob_avail "admins rejects non-admin" false Ob.Admins
          ~is_admin:false;
        check_ob_avail "public accepts admin" true Ob.Public ~is_admin:true;
        check_ob_avail "public accepts non-admin" true Ob.Public ~is_admin:false;
      ] )
    (* Legacy generic community creation is global-admin only, and the
       policy is independent of the GitHub onboarding mode: Public must
       never reopen arbitrary community creation. *);
    ( "legacy_community_creation",
      [
        check_ob_legacy "admin allowed" true ~is_admin:true;
        check_ob_legacy "non-admin denied" false ~is_admin:false;
        check_ob_legacy_get "admin GET shows form" Ob.Show_form ~is_admin:true;
        check_ob_legacy_get "authenticated non-admin GET resolves to /bring"
          Ob.Redirect_to_bring ~is_admin:false;
        check_ob_legacy_get "anonymous GET resolves to /bring"
          Ob.Redirect_to_bring ~is_admin:false;
        check_ob_legacy_post "admin POST proceeds" Ob.Show_form ~is_admin:true;
        check_ob_legacy_post "authenticated non-admin POST forbidden" Ob.Forbid
          ~is_admin:false;
        check_ob_legacy_post "anonymous POST forbidden" Ob.Forbid
          ~is_admin:false;
        Alcotest.test_case
          "Public onboarding mode does not reopen legacy creation" `Quick
          (fun () ->
            Alcotest.(check bool)
              "onboarding itself is open to non-admins" true
              (Ob.onboarding_available Ob.Public ~is_admin:false);
            Alcotest.(check bool)
              "legacy creation still denied" false
              (Ob.can_use_legacy_community_creation ~is_admin:false);
            Alcotest.(check string)
              "legacy POST still forbidden"
              (ob_decision_str Ob.Forbid)
              (ob_decision_str
                 (Ob.legacy_creation_post_decision ~is_admin:false)));
      ] );
  ]
