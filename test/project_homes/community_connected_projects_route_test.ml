module Phr = Earde.Project_home_relation

(* === Connected projects on the real community page (GET /c/:slug) ===
   End-to-end coverage of the integration itself: the section appears inside
   the page the existing route already serves, for exactly the visitors that
   route already authorizes, and never for anyone else. The real handler runs
   behind the real production pipeline shape — sql_pool + secret + memory
   sessions + the real router path — so :slug, Community_store.get_community_by_slug and
   can_view_community behave exactly as in bin/main; no synthetic parallel
   handler exists. Database-gated with its own reserved
   external-installation-id range 947000001..947000999 (hence account ids
   947100001..947100999, which also scope the permanent-project cleanup),
   ccph_% usernames, and ccph-% community slugs so no suite shares fixtures.
   Credential and privacy assertions are boolean, so no fixture byte reaches
   test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Fin = Earde.Project_finalization_store

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let insert_community = Community_fixture.insert_community
let status_of = Http_fixture.status_of

(* Distinctive credential-shaped fixtures. None may appear in the page, its
   headers, or its cookies. *)
let private_note = "ccph private note gho_CCPH_ACCESS_TOKEN_SECRET"
let refresh_token_marker = "ghr_CCPH_REFRESH_TOKEN"
let pkce_verifier_marker = "CCPH_PKCE_VERIFIER_VALUE"
let oauth_state_marker = "CCPH_OAUTH_STATE_VALUE"
let client_secret_marker = "CCPH_CLIENT_SECRET_VALUE"
let authorization_code_marker = "CCPH_AUTHORIZATION_CODE"
let session_binding_marker = "CCPH_SESSION_BINDING"

let credential_markers =
  [
    ("access token / private note", private_note);
    ("refresh token", refresh_token_marker);
    ("PKCE verifier", pkce_verifier_marker);
    ("OAuth state", oauth_state_marker);
    ("client secret", client_secret_marker);
    ("authorization code", authorization_code_marker);
    ("session binding", session_binding_marker);
    ("external installation id", "947000001");
    ("external account id", "947100001");
    ("external repository id", "947400001");
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 947100001 \
       AND 947100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       947100001 AND 947100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 947000001 AND 947000999)";
      "DELETE FROM communities WHERE slug LIKE 'ccph-%'";
      "DELETE FROM users WHERE username LIKE 'ccph_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       947000001 AND 947000999";
    ]

let q_insert_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES \
     ($1, $2, $3)"

let q_insert_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_provision_accepted =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at) VALUES ($1, $2, 'home', 'accepted', NOW())"

let q_insert_removed =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at, removed_at) VALUES ($1, $2, 'home', 'removed', \
     NOW(), NOW())"

let q_legacy_community =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET sections_enabled = FALSE WHERE id = $1"

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
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
               (fun () -> f ~url conn)
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun ~url conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () ->
          Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f ~url conn))

let section_marker = "<section class='ccp-section'>"

let check_has_section label body =
  Alcotest.(check bool)
    (label ^ ": section present")
    true
    (Html_assert.contains body section_marker);
  Alcotest.(check bool)
    (label ^ ": heading") true
    (Html_assert.contains body "Connected projects");
  Alcotest.(check bool)
    (label ^ ": supporting copy")
    true
    (Html_assert.contains body
       "Open-source projects that use this community as their Earde home.")

let check_no_section label body =
  Alcotest.(check bool)
    (label ^ ": no section") false
    (Html_assert.contains body section_marker);
  Alcotest.(check bool)
    (label ^ ": no heading") false
    (Html_assert.contains body "Connected projects")

(* The community home carries no list either way now: only the compact
   Network panel, whose row is labelled "Connected projects" — so absence is
   judged on the fragment's own markup, never on that label. *)
let check_no_list label body =
  Alcotest.(check bool)
    (label ^ ": no fragment") false
    (Html_assert.contains body section_marker);
  Alcotest.(check bool)
    (label ^ ": no fragment heading")
    false
    (Html_assert.contains body "<h2 class='ccp-title'>");
  Alcotest.(check bool)
    (label ^ ": no supporting copy")
    false
    (Html_assert.contains body
       "Open-source projects that use this community as their Earde home.")

(* The Network page names both destinations whether or not either holds
   anything, so nothing connected is a quiet line inside the block rather
   than a missing block — the opposite of the community page's contract. *)
let check_empty_section label body =
  Alcotest.(check bool)
    (label ^ ": block still framed")
    true
    (Html_assert.contains body section_marker);
  Alcotest.(check bool)
    (label ^ ": quiet empty line")
    true
    (Html_assert.contains body "No connected projects yet.");
  Alcotest.(check bool)
    (label ^ ": no project list")
    false
    (Html_assert.contains body "<ul class='ccp-projects'>")

(* Nothing in this feature may imply endorsement or officiality. *)
let check_no_officiality label body =
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no " ^ needle)
        false
        (Html_assert.contains body needle))
    [
      "Official project";
      "Official community";
      "Official home";
      "GitHub-approved";
      "GitHub-endorsed";
    ]

(* Connected projects must never cause analytics to initialize on a page
   that would not otherwise carry it. The section itself contributes no
   script (proven structurally in the DB-free renderer suite); here the
   assertion is end-to-end — a private or draft page that DOES list
   connected projects still carries no analytics bootstrap, group
   attribute, or consent block. *)
let check_no_analytics_init label body =
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no " ^ needle)
        false
        (Html_assert.contains body needle))
    [
      "analytics.js";
      "data-analytics-group";
      "analytics-consent";
      "data-analytics-private-community";
    ]

(* Boolean privacy sweep over the page, its headers, and its cookies. *)
let check_no_credentials label response body =
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  List.iter
    (fun (what, marker) ->
      Alcotest.(check bool)
        (label ^ ": body carries no " ^ what)
        false
        (Html_assert.contains body marker);
      Alcotest.(check bool)
        (label ^ ": headers carry no " ^ what)
        false
        (Html_assert.contains headers marker))
    credential_markers

let check_ok label response =
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response)

(* The route's single generic unavailable response, used for both a missing
   community and a denied private read. *)
let check_unavailable label response body =
  Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
  Alcotest.(check bool)
    (label ^ ": generic copy") true
    (Html_assert.contains body "This community does not exist.");
  check_no_section label body

let check_generic_500 label response body =
  Alcotest.(check int) (label ^ ": 500") 500 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": non-cacheable")
    (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check bool)
    (label ^ ": generic copy") true
    (Html_assert.contains body "Something went wrong on our side.");
  check_no_section label body;
  (* No SQL diagnostics of any kind. *)
  List.iter
    (fun needle ->
      Alcotest.(check bool)
        (label ^ ": no " ^ needle)
        false
        (Html_assert.contains body needle))
    [
      "community_projects";
      "open_source_projects";
      "Caqti";
      "PostgreSQL";
      "SELECT";
      "relation \"";
    ]

(* === fixtures === *)

let make_project ?kind ?name ?website ?(repos = [ "alpha" ]) conn ~user ~ext_id
    ~slug =
  let base = Int64.add ext_id 400000L in
  let* _inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        List.mapi
          (fun i n ->
            Project_fixture.repo ~account_id
              ~id:(Int64.add base (Int64.of_int i))
              n)
          repos)
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let primary = List.nth ids 0 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary ids
  in
  let identity =
    Project_fixture.identity_exn ?kind ?name ~slug ?website ~selected:ids
      ~primary ()
  in
  let* created =
    Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity
  in
  Lwt.return (Fin.project_id created)

let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" q_insert_moderator (user, community, "top_mod")

let add_member conn ~user ~community =
  exec conn "member fixture" q_insert_member (user, community)

let request_pending label conn ~user ~slug ~community =
  let relation =
    Home_request_fixture.phr_expect_ok
      (Phr.create_pending ~request_note:(Some private_note))
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: request fixture failed" label

let review label conn ~reviewer ~slug ~community_slug ~decision =
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community_slug ~decision
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: review fixture failed" label

let accept_reviewed label conn ~owner ~reviewer ~slug ~cid ~community_slug =
  let* () = request_pending label conn ~user:owner ~slug ~community:cid in
  review label conn ~reviewer ~slug ~community_slug ~decision:Rvs.Accept

(* === cases === *)

let empty_case =
  db_case
    "network page: a public community with no accepted project home keeps the \
     block and says so quietly; the home carries neither" (fun ~url conn ->
      let* _cid = insert_community conn "ccph-empty" in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-empty" ()
      in
      check_ok "anonymous" response;
      check_empty_section "anonymous" body;
      (* The page itself renders around it. *)
      Alcotest.(check bool)
        "community still renders" true
        (Html_assert.contains body "ccph-empty");
      (* The community home has no list at all any more — only the compact
         entry point with its count. *)
      let* home_response, home_body =
        Connected_projects_fixture.visit ~path:"" ~url ~slug:"ccph-empty" ()
      in
      check_ok "home" home_response;
      check_no_list "home" home_body;
      Alcotest.(check bool)
        "home links the network page" true
        (Html_assert.contains home_body "href='/c/ccph-empty/network#projects'");
      Lwt.return_unit)

let anonymous_case =
  db_case
    "community page: one accepted home is visible to an anonymous visitor, \
     with factual copy and no GitHub login requirement" (fun ~url conn ->
      let* owner = insert_user conn "ccph_owner" in
      let* moderator = insert_user conn "ccph_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000001L ~slug:"ccph-alpha"
          ~name:"Ccph Alpha" ~website:"https://ccph-alpha.example/"
      in
      let* cid = insert_community conn "ccph-pub" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "accept" conn ~owner ~reviewer:moderator
          ~slug:"ccph-alpha" ~cid ~community_slug:"ccph-pub"
      in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-pub" ()
      in
      check_ok "anonymous" response;
      check_has_section "anonymous" body;
      Alcotest.(check bool)
        "project name" true
        (Html_assert.contains body "Ccph Alpha");
      Alcotest.(check bool)
        "verification copy" true
        (Html_assert.contains body "Verified through GitHub");
      Alcotest.(check bool)
        "provenance copy" true
        (Html_assert.contains body "Project connected through GitHub");
      Alcotest.(check bool)
        "repository linked" true
        (Html_assert.contains body "https://github.com/pfin-owner/alpha");
      (* No GitHub authorization is demanded of an ordinary visitor. *)
      Alcotest.(check bool)
        "no GitHub sign-in prompt" false
        (Html_assert.contains body "github.com/login/oauth");
      Alcotest.(check bool)
        "no owner-only setup link" false
        (Html_assert.contains body "/projects/ccph-alpha/setup");
      check_no_officiality "anonymous" body;
      check_no_credentials "anonymous" response body;
      Lwt.return_unit)

(* The home's compact entry point, over the same durable relation: the count
   is the size of exactly the list the Network page renders, and no part of
   that list reaches the home. *)
let home_entry_point_case =
  db_case
    "community home: the connected projects are one counted link to the \
     network page, never a list" (fun ~url conn ->
      let* owner = insert_user conn "ccph_hep_owner" in
      let* moderator = insert_user conn "ccph_hep_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000005L ~slug:"ccph-hep"
          ~name:"Ccph Entry Point" ~website:"https://ccph-hep.example/"
      in
      let* cid = insert_community conn "ccph-hep" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "hep" conn ~owner ~reviewer:moderator ~slug:"ccph-hep"
          ~cid ~community_slug:"ccph-hep"
      in
      let* response, body =
        Connected_projects_fixture.visit ~path:"" ~url ~slug:"ccph-hep" ()
      in
      check_ok "home" response;
      check_no_list "home" body;
      Alcotest.(check bool)
        "no project identity on the home" false
        (Html_assert.contains body "Ccph Entry Point");
      Alcotest.(check bool)
        "counted row" true
        (Html_assert.contains body
           "<span class='launch-net-label'>Connected projects</span><span \
            class='launch-net-count'>1</span>");
      Alcotest.(check bool)
        "row links the network page" true
        (Html_assert.contains body "href='/c/ccph-hep/network#projects'");
      (* And the list itself is one click away, complete. *)
      let* net_response, net_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-hep" ()
      in
      check_ok "network" net_response;
      check_has_section "network" net_body;
      Alcotest.(check bool)
        "project identity on the network page" true
        (Html_assert.contains net_body "Ccph Entry Point");
      Lwt.return_unit)

let ordering_case =
  db_case
    "community page: multiple accepted homes keep the read model's \
     deterministic order" (fun ~url conn ->
      let* owner = insert_user conn "ccph_ord_owner" in
      let* moderator = insert_user conn "ccph_ord_mod" in
      let* cid = insert_community conn "ccph-ord" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* _ =
        make_project conn ~user:owner ~ext_id:947000010L ~slug:"ccph-ord-z"
          ~name:"zulu tool"
      in
      let* _ =
        make_project conn ~user:owner ~ext_id:947000011L ~slug:"ccph-ord-a"
          ~name:"Alpha tool"
      in
      let* () =
        accept_reviewed "z" conn ~owner ~reviewer:moderator ~slug:"ccph-ord-z"
          ~cid ~community_slug:"ccph-ord"
      in
      let* () =
        accept_reviewed "a" conn ~owner ~reviewer:moderator ~slug:"ccph-ord-a"
          ~cid ~community_slug:"ccph-ord"
      in
      let* _response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-ord" ()
      in
      let idx needle =
        match Html_assert.index_from body needle 0 with
        | Some i -> i
        | None -> Alcotest.failf "missing %s" needle
      in
      Alcotest.(check bool)
        "lower(name) order preserved through rendering" true
        (idx "Alpha tool" < idx "zulu tool");
      Lwt.return_unit)

let excluded_case =
  db_case "community page: pending, rejected and removed relations never appear"
    (fun ~url conn ->
      let* owner = insert_user conn "ccph_ex_owner" in
      let* moderator = insert_user conn "ccph_ex_mod" in
      let* _pending =
        make_project conn ~user:owner ~ext_id:947000020L ~slug:"ccph-pend"
          ~name:"Ccph Pending"
      in
      let* _rejected =
        make_project conn ~user:owner ~ext_id:947000021L ~slug:"ccph-rej"
          ~name:"Ccph Rejected"
      in
      let* removed =
        make_project conn ~user:owner ~ext_id:947000022L ~slug:"ccph-rem"
          ~name:"Ccph Removed"
      in
      let* cid = insert_community conn "ccph-ex" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        request_pending "pending" conn ~user:owner ~slug:"ccph-pend"
          ~community:cid
      in
      let* () =
        request_pending "reject seed" conn ~user:owner ~slug:"ccph-rej"
          ~community:cid
      in
      let* () =
        review "reject" conn ~reviewer:moderator ~slug:"ccph-rej"
          ~community_slug:"ccph-ex" ~decision:Rvs.Reject
      in
      let* () = exec conn "removed row" q_insert_removed (removed, cid) in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-ex" ()
      in
      check_ok "closed states" response;
      check_empty_section "only closed states" body;
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("absent: " ^ needle) false
            (Html_assert.contains body needle))
        [ "Ccph Pending"; "Ccph Rejected"; "Ccph Removed" ];
      check_no_credentials "closed states" response body;
      Lwt.return_unit)

let drifted_verification_case =
  db_case
    "community page: stale and revoked projects stay visible with their exact \
     copy" (fun ~url conn ->
      let* owner = insert_user conn "ccph_ver_owner" in
      let* moderator = insert_user conn "ccph_ver_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:947000030L ~slug:"ccph-ver"
          ~name:"Ccph Drifting"
      in
      let* cid = insert_community conn "ccph-ver" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "ver" conn ~owner ~reviewer:moderator ~slug:"ccph-ver"
          ~cid ~community_slug:"ccph-ver"
      in
      let* () =
        exec conn "stale" Connected_projects_fixture.q_set_verification
          (project, "stale")
      in
      let* _r, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-ver" ()
      in
      check_has_section "stale" body;
      Alcotest.(check bool)
        "stale copy" true
        (Html_assert.contains body "Verification stale");
      Alcotest.(check bool)
        "still listed" true
        (Html_assert.contains body "Ccph Drifting");
      let* () =
        exec conn "revoked" Connected_projects_fixture.q_set_verification
          (project, "revoked")
      in
      let* _r, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-ver" ()
      in
      check_has_section "revoked" body;
      Alcotest.(check bool)
        "revoked copy" true
        (Html_assert.contains body "Verification revoked");
      check_no_officiality "revoked" body;
      Lwt.return_unit)

let provisioned_case =
  db_case
    "community page: a provisioned accepted home (no requester, no reviewer) \
     is visible like any other" (fun ~url conn ->
      let* owner = insert_user conn "ccph_prov_owner" in
      let* project =
        make_project conn ~user:owner ~ext_id:947000040L ~slug:"ccph-prov"
          ~name:"Ccph Provisioned"
      in
      let* cid = insert_community conn "ccph-prov" in
      let* () = exec conn "provision" q_provision_accepted (project, cid) in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-prov" ()
      in
      check_ok "provisioned" response;
      check_has_section "provisioned" body;
      Alcotest.(check bool)
        "project listed" true
        (Html_assert.contains body "Ccph Provisioned");
      (* Provenance is not exposed, so nothing distinguishes it from a
         reviewed home. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("no provenance: " ^ needle)
            false
            (Html_assert.contains body needle))
        [ "Requested by"; "Reviewed by"; "provisioned"; "Provisioned home" ];
      Lwt.return_unit)

let legacy_branch_case =
  db_case
    "community page: the simple-feed branch still renders the section inline, \
     and its network page renders it too" (fun ~url conn ->
      let* owner = insert_user conn "ccph_leg_owner" in
      let* moderator = insert_user conn "ccph_leg_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000050L ~slug:"ccph-leg"
          ~name:"Ccph Legacy"
      in
      let* cid = insert_community conn "ccph-leg" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "leg" conn ~owner ~reviewer:moderator ~slug:"ccph-leg"
          ~cid ~community_slug:"ccph-leg"
      in
      let* () = exec conn "simple feed" q_legacy_community cid in
      let* response, body =
        Connected_projects_fixture.visit ~path:"" ~url ~slug:"ccph-leg" ()
      in
      check_ok "simple feed" response;
      check_has_section "simple feed" body;
      Alcotest.(check bool)
        "project listed" true
        (Html_assert.contains body "Ccph Legacy");
      (* The Network page exists for a flat community as well. *)
      let* net_response, net_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-leg" ()
      in
      check_ok "simple feed network" net_response;
      check_has_section "simple feed network" net_body;
      Lwt.return_unit)

let unlisted_case =
  db_case
    "community page: an unlisted community is unchanged and still shows its \
     connected projects" (fun ~url conn ->
      let* owner = insert_user conn "ccph_unl_owner" in
      let* moderator = insert_user conn "ccph_unl_mod" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000060L ~slug:"ccph-unl"
          ~name:"Ccph Unlisted"
      in
      let* cid = insert_community conn "ccph-unl" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "unl" conn ~owner ~reviewer:moderator ~slug:"ccph-unl"
          ~cid ~community_slug:"ccph-unl"
      in
      let* () = exec conn "unlisted" Community_fixture.q_make_unlisted cid in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-unl" ()
      in
      check_ok "unlisted" response;
      check_has_section "unlisted" body;
      (* Unchanged indexing policy: an unlisted community stays noindex, and
         this feature adds no discovery surface. *)
      Alcotest.(check bool)
        "still noindex" true
        (Html_assert.contains body "noindex");
      Lwt.return_unit)

let private_case =
  db_case_lifecycle_relaxed
    "community page: only visitors the route already authorizes see a private \
     community and its connected projects" (fun ~url conn ->
      let* owner = insert_user conn "ccph_priv_owner" in
      let* moderator = insert_user conn "ccph_priv_mod" in
      let* member = insert_user conn "ccph_priv_member" in
      let* stranger = insert_user conn "ccph_priv_stranger" in
      let* admin = insert_user conn "ccph_priv_admin" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000070L ~slug:"ccph-priv"
          ~name:"Ccph Private"
      in
      let* cid = insert_community conn "ccph-priv" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "priv" conn ~owner ~reviewer:moderator ~slug:"ccph-priv"
          ~cid ~community_slug:"ccph-priv"
      in
      let* () = add_member conn ~user:member ~community:cid in
      let* () =
        exec conn "make admin" Connected_projects_fixture.q_set_admin
          (admin, true)
      in
      (* Only after acceptance does the community go private — the drift the
         feature must tolerate. *)
      let* () = exec conn "private" Community_fixture.q_make_private cid in
      (* Authorized: member, moderator, admin. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, uid, is_admin) ->
            let* response, body =
              Connected_projects_fixture.visit ~url ~session_user_id:uid
                ~session_admin:is_admin ~slug:"ccph-priv" ()
            in
            check_ok label response;
            check_has_section label body;
            Alcotest.(check bool)
              (label ^ ": project listed")
              true
              (Html_assert.contains body "Ccph Private");
            (* Connected projects exist on this private page, and it still
               initializes no analytics of any kind. *)
            check_no_analytics_init label body;
            check_no_credentials label response body;
            Lwt.return_unit)
          [
            ("member", member, false);
            ("moderator", moderator, false);
            ("admin", admin, true);
          ]
      in
      (* Unauthorized: anonymous and a signed-in stranger get the route's
         existing generic response, byte-identical to a missing community. *)
      let* anon_response, anon_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-priv" ()
      in
      check_unavailable "anonymous" anon_response anon_body;
      let* str_response, str_body =
        Connected_projects_fixture.visit ~url ~session_user_id:stranger
          ~slug:"ccph-priv" ()
      in
      check_unavailable "stranger" str_response str_body;
      let* miss_response, miss_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-missing" ()
      in
      check_unavailable "missing" miss_response miss_body;
      Alcotest.(check bool)
        "denied private is indistinguishable from missing" true
        (String.equal str_body miss_body);
      Alcotest.(check bool)
        "no project name leaks to the denied visitor" false
        (Html_assert.contains str_body "Ccph Private");
      Lwt.return_unit)

let draft_case =
  db_case
    "community page: a setup-draft community stays authorized exactly as before"
    (fun ~url conn ->
      let* owner = insert_user conn "ccph_dr_owner" in
      let* moderator = insert_user conn "ccph_dr_mod" in
      let* stranger = insert_user conn "ccph_dr_stranger" in
      let* _project =
        make_project conn ~user:owner ~ext_id:947000080L ~slug:"ccph-dr"
          ~name:"Ccph Draft"
      in
      let* cid = insert_community conn "ccph-dr" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "dr" conn ~owner ~reviewer:moderator ~slug:"ccph-dr"
          ~cid ~community_slug:"ccph-dr"
      in
      let* () = exec conn "draft" Community_fixture.q_make_draft_state cid in
      (* The draft's own moderator keeps access, and sees the section. *)
      let* mod_response, mod_body =
        Connected_projects_fixture.visit ~url ~session_user_id:moderator
          ~slug:"ccph-dr" ()
      in
      check_ok "draft moderator" mod_response;
      check_has_section "draft moderator" mod_body;
      check_no_analytics_init "draft moderator" mod_body;
      (* An unrelated visitor and an anonymous one are still denied. *)
      let* str_response, str_body =
        Connected_projects_fixture.visit ~url ~session_user_id:stranger
          ~slug:"ccph-dr" ()
      in
      check_unavailable "draft stranger" str_response str_body;
      let* anon_response, anon_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-dr" ()
      in
      check_unavailable "draft anonymous" anon_response anon_body;
      Lwt.return_unit)

let missing_case =
  db_case "community page: a missing community is unchanged" (fun ~url _conn ->
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-nope" ()
      in
      check_unavailable "missing" response body;
      Lwt.return_unit)

let inconsistency_case =
  db_case
    "community page: durable inconsistency in a connected project is one \
     generic non-cacheable 500, never a partial page" (fun ~url conn ->
      let* owner = insert_user conn "ccph_inc_owner" in
      let* moderator = insert_user conn "ccph_inc_mod" in
      let* project =
        make_project conn ~user:owner ~ext_id:947000090L ~slug:"ccph-inc"
          ~name:"Ccph Inconsistent"
      in
      let* cid = insert_community conn "ccph-inc" in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () =
        accept_reviewed "inc" conn ~owner ~reviewer:moderator ~slug:"ccph-inc"
          ~cid ~community_slug:"ccph-inc"
      in
      let* ok_response, ok_body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-inc" ()
      in
      check_ok "before corruption" ok_response;
      check_has_section "before corruption" ok_body;
      let* () =
        exec conn "corrupt login" Connected_projects_fixture.q_corrupt_login
          project
      in
      let* response, body =
        Connected_projects_fixture.visit ~url ~slug:"ccph-inc" ()
      in
      check_generic_500 "corrupted" response body;
      (* The community's own content is not rendered around the failure. *)
      Alcotest.(check bool)
        "no partial community page" false
        (Html_assert.contains body "Ccph Inconsistent");
      check_no_credentials "corrupted" response body;
      Lwt.return_unit)

let storage_failure_case =
  db_case
    "community page: a connected-projects storage failure is one generic \
     non-cacheable 500" (fun ~url conn ->
      let* _cid = insert_community conn "ccph-store" in
      let* () =
        exec conn "hide relations" Connected_projects_fixture.q_hide_relations
          ()
      in
      Lwt.finalize
        (fun () ->
          let* response, body =
            Connected_projects_fixture.visit ~url ~slug:"ccph-store" ()
          in
          check_generic_500 "storage failure" response body;
          Lwt.return_unit)
        (fun () ->
          exec conn "restore relations"
            Connected_projects_fixture.q_show_relations ()))

let suite =
  [
    empty_case;
    anonymous_case;
    home_entry_point_case;
    ordering_case;
    excluded_case;
    drifted_verification_case;
    provisioned_case;
    legacy_branch_case;
    unlisted_case;
    private_case;
    draft_case;
    missing_case;
    inconsistency_case;
    storage_failure_case;
  ]

let suites =
  (* The section inside the real GET /c/:slug page: visibility follows the
       route's existing authorization exactly, closed relations never
       appear, ordering survives rendering, and read-model failures become
       one generic non-cacheable 500. Database-gated. *)
  [ ("community_connected_projects_page", suite) ]
