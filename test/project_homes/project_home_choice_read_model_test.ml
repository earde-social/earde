module Phr = Earde.Project_home_relation

(* === Project home choice read model (Project_home_choice_read_model) ===
   Owner-authorized view behind the existing-community home choice: the
   active relation or the eligible target list, driven over verified
   permanent projects built through the real draft/selection/finalization
   chain and pending relations written by the real request store.
   Database-gated (EARDE_TEST_DATABASE_URL, same opt-in as Mod_scope) with
   its own reserved external-installation-id range 944400001..944400999
   (hence account ids 944500001..944500999, which also scope the
   permanent-project cleanup), phcv_% usernames, and phcv-% community
   slugs so no suite shares fixtures. The eligible-list predicate is
   global, so exact assertions filter to phcv-% slugs — except under an
   active relation, where the list is exactly empty by contract. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rm = Earde.Project_home_choice_read_model

module Rq = Earde.Project_home_request_store

let error_str : Rm.error -> string = function
  | Rm.Invalid_user_id -> "Invalid_user_id"
  | Rm.Invalid_project_slug -> "Invalid_project_slug"
  | Rm.Inconsistent_data -> "Inconsistent_data"
  | Rm.Storage_error -> "Storage_error"

let vis_str : Rm.visibility -> string = function
  | Rm.Public -> "public"
  | Rm.Unlisted -> "unlisted"
  | Rm.Currently_unavailable -> "currently_unavailable"

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let make_project = Home_request_fixture.make_project

(* Same dependency order as the sibling suites; the LIKE pattern also
   catches deliberately corrupted phcv- slugs. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 944500001 AND 944500999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 944500001 AND 944500999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 944400001 AND 944400999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phcv-%'"
    ; "DELETE FROM users WHERE username LIKE 'phcv_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 944400001 AND 944400999"
    ]

let q_corrupt_description =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET description = 'phcv' || chr(1) || 'corrupt' \
   WHERE id = $1"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way, and the
   connection is disconnected deterministically. *)
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
               (fun () -> f conn)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () -> Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f conn))

(* === call helpers === *)

let load conn ~user ~slug =
  Rm.load_for_steward conn ~user_id:user ~project_slug:slug

let load_view label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok (Some view) -> Lwt.return view
  | Ok None -> Alcotest.failf "%s: unexpectedly absent" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_none label conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Alcotest.failf "%s: unexpectedly present" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn ~user ~slug =
  let* r = load conn ~user ~slug in
  match r with
  | Ok None ->
      Alcotest.failf "%s: expected %s, got Ok None" label
        (error_str expected)
  | Ok (Some _) ->
      Alcotest.failf "%s: expected %s, got Ok Some" label
        (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let phcv_only communities =
  List.filter
    (fun c ->
      let slug = Rm.community_slug c in
      String.length slug >= 5 && String.sub slug 0 5 = "phcv-")
    communities

let active_of label view =
  match Rm.active_relation view with
  | Some relation -> relation
  | None -> Alcotest.failf "%s: expected an active relation" label

let check_no_active label view =
  Alcotest.(check bool) label true
    (match Rm.active_relation view with None -> true | Some _ -> false)

(* === pure input validation === *)

let pure_inputs_case =
  db_case "choice: invalid inputs rejected before any SQL" (fun _conn ->
      (* A deliberately unusable connection: pure validation must return
         without touching it — were any SQL attempted, the driver would
         raise on the finished connection and fail the test. *)
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let expect label e ~user ~slug = load_expect label e dead ~user ~slug in
      let* () = expect "user id 0" Rm.Invalid_user_id ~user:0 ~slug:"phcv-a" in
      let* () =
        expect "negative user id" Rm.Invalid_user_id ~user:(-7)
          ~slug:"phcv-a"
      in
      let* () =
        expect "user checked before slug" Rm.Invalid_user_id ~user:0
          ~slug:"NOT A SLUG"
      in
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            expect "invalid project slug" Rm.Invalid_project_slug ~user:1
              ~slug:bad)
          [ ""
          ; "Phcv-Upper"
          ; "phcv slug"
          ; " phcv-a"
          ; "phcv-a "
          ; "phcv_a"
          ; "phcv/a"
          ; "-phcv"
          ; "phcv-"
          ; "phcv--a"
          ; String.make 81 'a'
          ]
      in
      Lwt.return_unit)

(* A live connection whose unqualified table names stop resolving: a real
   PostgreSQL request failure, collapsed payload-free. The path is
   restored before the shared cleanup runs on the same connection. *)
let q_hide_tables =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO phcv_void"

let q_restore_tables =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

let storage_case =
  db_case "choice: database failure collapses to the payload-free error"
    (fun conn ->
      let* () = exec conn "hide tables" q_hide_tables () in
      Lwt.finalize
        (fun () ->
          load_expect "storage error" Rm.Storage_error conn ~user:1
            ~slug:"phcv-a")
        (fun () -> exec conn "restore tables" q_restore_tables ()))

(* === steward authorization === *)

let steward_view_case =
  db_case "choice: steward load; stewardship alone authorizes" (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* b = insert_user conn "phcv_b" in
      let* inst, project =
        make_project conn ~user:a ~ext_id:944400001L ~slug:"phcv-view"
      in
      let* view = load_view "owner load" conn ~user:a ~slug:"phcv-view" in
      let p = Rm.project view in
      Alcotest.(check string) "project name" "Pfin Fixture Project"
        (Rm.project_name p);
      Alcotest.(check string) "canonical slug" "phcv-view"
        (Rm.project_slug p);
      Alcotest.(check string) "namespace login" "pfin-owner"
        (Rm.project_namespace_login p);
      check_no_active "no active relation" view;
      Alcotest.(check int) "no fixture-eligible targets" 0
        (List.length (phcv_only (Rm.eligible_communities view)));
      (* A second steward is equally authorized — membership or
         moderation in any community is never consulted. *)
      let* () =
        exec conn "add steward" Home_request_fixture.q_insert_steward (project, b, inst)
      in
      let* view_b =
        load_view "second steward" conn ~user:b ~slug:"phcv-view"
      in
      Alcotest.(check string) "same project" "phcv-view"
        (Rm.project_slug (Rm.project view_b));
      Lwt.return_unit)

let collapse_case =
  db_case "choice: every unavailable-project cause collapses to absence"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* b = insert_user conn "phcv_b" in
      let* _, project =
        make_project conn ~user:a ~ext_id:944400002L ~slug:"phcv-auth"
      in
      let* () = load_none "missing project" conn ~user:a ~slug:"phcv-absent" in
      let* () = load_none "foreign project" conn ~user:b ~slug:"phcv-auth" in
      let* () =
        exec conn "mark stale" Home_request_fixture.q_set_verification (project, "stale")
      in
      let* () = load_none "stale project" conn ~user:a ~slug:"phcv-auth" in
      let* () =
        exec conn "mark revoked" Home_request_fixture.q_set_verification (project, "revoked")
      in
      let* () = load_none "revoked project" conn ~user:a ~slug:"phcv-auth" in
      let* () =
        exec conn "restore verified" Home_request_fixture.q_set_verification
          (project, "verified")
      in
      let* () = exec conn "drop steward" Home_request_fixture.q_delete_steward (project, a) in
      load_none "creator without stewardship" conn ~user:a ~slug:"phcv-auth")

(* === eligible list === *)

let eligible_case =
  db_case_lifecycle_relaxed "choice: eligible predicate, mapping, and deterministic order"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400003L ~slug:"phcv-elig"
      in
      let* unl =
        Community_fixture.insert_community conn "phcv-elig-unl" ~name:"phcv apple"
          ~indexable:false ~discoverable:false
      in
      let* same_a =
        Community_fixture.insert_community conn "phcv-elig-same-a" ~name:"Phcv Same"
      in
      let* same_b =
        Community_fixture.insert_community conn "phcv-elig-same-b" ~name:"Phcv Same"
      in
      let* pub =
        Community_fixture.insert_community conn "phcv-elig-pub" ~name:"Phcv Zebra"
          ~description:"Multi\nline\twith \xe2\x98\x95"
      in
      let* _legacy =
        Community_fixture.insert_community conn "phcv-elig-legacy" ~network:false
      in
      let* _draft =
        Community_fixture.insert_community conn "phcv-elig-draft" ~onboarding:"draft"
          ~visibility:"private" ~indexable:false ~discoverable:false
      in
      let* _priv =
        Community_fixture.insert_community conn "phcv-elig-priv" ~visibility:"private"
          ~indexable:false ~discoverable:false
      in
      let* view = load_view "eligible load" conn ~user:a ~slug:"phcv-elig" in
      check_no_active "no active relation" view;
      let eligible = phcv_only (Rm.eligible_communities view) in
      (* lower(name) ASC, then slug ASC for the tied pair; the excluded
         legacy, draft, and private slugs prove the predicate. *)
      Alcotest.(check (list string)) "deterministic order"
        [ "phcv-elig-unl"; "phcv-elig-same-a"; "phcv-elig-same-b";
          "phcv-elig-pub" ]
        (List.map Rm.community_slug eligible);
      Alcotest.(check (list int)) "exact local ids"
        [ unl; same_a; same_b; pub ]
        (List.map Rm.community_id eligible);
      let by_slug slug =
        List.find (fun c -> Rm.community_slug c = slug) eligible
      in
      let pub_c = by_slug "phcv-elig-pub" in
      Alcotest.(check string) "public name" "Phcv Zebra"
        (Rm.community_name pub_c);
      Alcotest.(check string) "fully listed maps to Public" "public"
        (vis_str (Rm.community_visibility pub_c));
      Alcotest.(check (option string)) "description byte-exact"
        (Some "Multi\nline\twith \xe2\x98\x95")
        (Rm.community_description pub_c);
      let unl_c = by_slug "phcv-elig-unl" in
      Alcotest.(check string) "fully unlisted maps to Unlisted" "unlisted"
        (vis_str (Rm.community_visibility unl_c));
      Alcotest.(check (option string)) "absent description stays None" None
        (Rm.community_description unl_c);
      Lwt.return_unit)

let mixed_flags_case =
  db_case_lifecycle_relaxed "choice: mixed publication flags on an eligible row are corrupt"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400004L ~slug:"phcv-mix"
      in
      let* home = Community_fixture.insert_community conn "phcv-mix-home" in
      let* () = exec conn "mix flags" Home_request_fixture.q_mix_community_flags home in
      let* () =
        load_expect "mixed flags" Rm.Inconsistent_data conn ~user:a
          ~slug:"phcv-mix"
      in
      (* The same durable rules bind an eligible row's identity. The
         scoped identity constraints are dropped for this probe alone
         and restored under Lwt.finalize once the row is canonical
         again. *)
      let* () = exec conn "restore flags" Community_fixture.q_make_unlisted home in
      let* () = Network_community_constraints.drop conn in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "corrupt slug" Home_request_fixture.q_corrupt_community_slug
              (home, "phcv-mix bad slug")
          in
          load_expect "corrupt eligible slug" Rm.Inconsistent_data conn
            ~user:a ~slug:"phcv-mix")
        (fun () ->
          let* () =
            exec conn "recanonicalize" Network_community_constraints.q_recanonicalize
              (home, "phcv-mix-home", "phcv-mix-home")
          in
          Network_community_constraints.restore conn))

(* === active relation === *)

let active_pending_case =
  db_case "choice: active pending relation suppresses the eligible list"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400005L ~slug:"phcv-pend"
      in
      let* home =
        Community_fixture.insert_community conn "phcv-pend-home" ~name:"Phcv Pending Home"
      in
      let* _other = Community_fixture.insert_community conn "phcv-pend-other" in
      let* _rid =
        Community_fixture.request_pending "pending fixture" conn ~user:a ~slug:"phcv-pend"
          ~community:home ~note:"phcv secret note" ()
      in
      let* view = load_view "pending load" conn ~user:a ~slug:"phcv-pend" in
      let relation = active_of "pending" view in
      Alcotest.(check bool) "status pending" true
        (Rm.active_relation_status relation = Phr.Pending);
      let c = Rm.active_relation_community relation in
      Alcotest.(check int) "target id" home (Rm.community_id c);
      Alcotest.(check string) "target slug" "phcv-pend-home"
        (Rm.community_slug c);
      Alcotest.(check string) "target name" "Phcv Pending Home"
        (Rm.community_name c);
      Alcotest.(check string) "fully eligible target maps Public" "public"
        (vis_str (Rm.community_visibility c));
      (* The private note crosses nowhere: no accessor exists, and no
         exposed field carries it. *)
      List.iter
        (fun v ->
          Alcotest.(check bool) "no note leakage" false
            (Html_assert.contains v "phcv secret note"))
        [ Rm.community_name c; Rm.community_slug c;
          (match Rm.community_description c with Some d -> d | None -> "")
        ];
      Alcotest.(check int) "eligible list exactly empty" 0
        (List.length (Rm.eligible_communities view));
      Lwt.return_unit)

let active_accepted_case =
  db_case "choice: accepted home is the active relation" (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400006L ~slug:"phcv-acc"
      in
      let* home = Community_fixture.insert_community conn "phcv-acc-home" in
      let* rid =
        Community_fixture.request_pending "accepted fixture" conn ~user:a ~slug:"phcv-acc"
          ~community:home ()
      in
      let* () = exec conn "accept" Home_request_fixture.q_mark_accepted rid in
      let* view = load_view "accepted load" conn ~user:a ~slug:"phcv-acc" in
      let relation = active_of "accepted" view in
      Alcotest.(check bool) "status accepted" true
        (Rm.active_relation_status relation = Phr.Accepted);
      Alcotest.(check string) "target slug" "phcv-acc-home"
        (Rm.community_slug (Rm.active_relation_community relation));
      Alcotest.(check string) "fully eligible target maps Public" "public"
        (vis_str
           (Rm.community_visibility
              (Rm.active_relation_community relation)));
      Alcotest.(check int) "eligible list exactly empty" 0
        (List.length (Rm.eligible_communities view));
      Lwt.return_unit)

let historical_case =
  db_case "choice: historical rejected/removed rows never suppress the list"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400007L ~slug:"phcv-hist"
      in
      let* home = Community_fixture.insert_community conn "phcv-hist-home" in
      let* rid =
        Community_fixture.request_pending "first request" conn ~user:a ~slug:"phcv-hist"
          ~community:home ()
      in
      let* () = exec conn "reject" Home_request_fixture.q_mark_rejected rid in
      let* view = load_view "after rejection" conn ~user:a ~slug:"phcv-hist" in
      check_no_active "rejection leaves no active relation" view;
      Alcotest.(check (list string)) "target eligible again"
        [ "phcv-hist-home" ]
        (List.map Rm.community_slug
           (phcv_only (Rm.eligible_communities view)));
      let* rid2 =
        Community_fixture.request_pending "second request" conn ~user:a ~slug:"phcv-hist"
          ~community:home ()
      in
      let* () = exec conn "accept" Home_request_fixture.q_mark_accepted rid2 in
      let* () = exec conn "remove" Home_request_fixture.q_mark_removed rid2 in
      let* view2 = load_view "after removal" conn ~user:a ~slug:"phcv-hist" in
      check_no_active "removal leaves no active relation" view2;
      Alcotest.(check (list string)) "target eligible after removal"
        [ "phcv-hist-home" ]
        (List.map Rm.community_slug
           (phcv_only (Rm.eligible_communities view2)));
      Lwt.return_unit)

let drift_case =
  db_case_lifecycle_relaxed "choice: drifted active targets map exactly, reasons collapsed"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      (* Four projects, one active target each: the still-eligible
         unlisted shape keeps its exact label; private, draft, and
         legacy drift all collapse to the same unavailable state. *)
      let* () =
        Lwt_list.iter_s
          (fun (ext_id, slug) ->
            let* _ = make_project conn ~user:a ~ext_id ~slug in
            Lwt.return_unit)
          [ (944400008L, "phcv-drift-unl")
          ; (944400009L, "phcv-drift-priv")
          ; (944400011L, "phcv-drift-draft")
          ; (944400012L, "phcv-drift-leg")
          ]
      in
      let* unlisted_home =
        Community_fixture.insert_community conn "phcv-drift-unl-home"
          ~name:"Phcv Drift Alpha"
      in
      let* private_home =
        Community_fixture.insert_community conn "phcv-drift-priv-home"
          ~name:"Phcv Drift Beta"
      in
      let* draft_home =
        Community_fixture.insert_community conn "phcv-drift-draft-home"
          ~name:"Phcv Drift Gamma"
      in
      let* legacy_home =
        Community_fixture.insert_community conn "phcv-drift-leg-home"
          ~name:"Phcv Drift Delta"
      in
      let* () =
        Lwt_list.iter_s
          (fun (slug, community) ->
            let* _ =
              Community_fixture.request_pending "drift fixture" conn ~user:a ~slug
                ~community ()
            in
            Lwt.return_unit)
          [ ("phcv-drift-unl", unlisted_home)
          ; ("phcv-drift-priv", private_home)
          ; ("phcv-drift-draft", draft_home)
          ; ("phcv-drift-leg", legacy_home)
          ]
      in
      let* () = exec conn "make unlisted" Community_fixture.q_make_unlisted unlisted_home in
      let* () = exec conn "make private" Community_fixture.q_make_private private_home in
      let* () = exec conn "make draft" Community_fixture.q_make_draft_state draft_home in
      let* () = exec conn "make legacy" Community_fixture.q_make_legacy legacy_home in
      Lwt_list.iter_s
        (fun (label, slug, home_slug, expected) ->
          let* view = load_view label conn ~user:a ~slug in
          let relation = active_of label view in
          let c = Rm.active_relation_community relation in
          Alcotest.(check string) (label ^ ": identity survives") home_slug
            (Rm.community_slug c);
          Alcotest.(check string) (label ^ ": exact mapping") expected
            (vis_str (Rm.community_visibility c));
          (* The reason never crosses: no accessor value names the
             drifted lifecycle. *)
          List.iter
            (fun word ->
              List.iter
                (fun v ->
                  Alcotest.(check bool)
                    (label ^ ": no " ^ word ^ " leakage") false
                    (Html_assert.contains (String.lowercase_ascii v) word))
                [ Rm.community_name c
                ; vis_str (Rm.community_visibility c)
                ; (match Rm.community_description c with
                  | Some d -> d
                  | None -> "")
                ])
            [ "private"; "draft"; "legacy"; "network" ];
          (* The active relation still suppresses the chooser. *)
          Alcotest.(check int) (label ^ ": eligible list exactly empty") 0
            (List.length (Rm.eligible_communities view));
          Lwt.return_unit)
        [ ( "unlisted target", "phcv-drift-unl", "phcv-drift-unl-home",
            "unlisted" )
        ; ( "private target", "phcv-drift-priv", "phcv-drift-priv-home",
            "currently_unavailable" )
        ; ( "draft target", "phcv-drift-draft", "phcv-drift-draft-home",
            "currently_unavailable" )
        ; ( "legacy target", "phcv-drift-leg", "phcv-drift-leg-home",
            "currently_unavailable" )
        ])

let corrupt_active_case =
  db_case_lifecycle_relaxed "choice: malformed active-target metadata is corrupt, never partial"
    (fun conn ->
      let* a = insert_user conn "phcv_a" in
      let* _, _project =
        make_project conn ~user:a ~ext_id:944400010L ~slug:"phcv-corr"
      in
      let* home =
        Community_fixture.insert_community conn "phcv-corr-home" ~name:"Phcv Corr Home"
      in
      let* _ =
        Community_fixture.request_pending "corrupt fixture" conn ~user:a ~slug:"phcv-corr"
          ~community:home ()
      in
      (* Flags that contradict each other are corruption even on an
         active target — never Currently_unavailable. *)
      let* () = exec conn "mix flags" Home_request_fixture.q_mix_community_flags home in
      let* () =
        load_expect "mixed flags on active target" Rm.Inconsistent_data
          conn ~user:a ~slug:"phcv-corr"
      in
      let* () = exec conn "restore flags" Community_fixture.q_make_listed home in
      (* Identity corruption is now barred by the scoped constraints;
         they are dropped for these probes alone and restored under
         Lwt.finalize once the row is canonical again. *)
      let* () = Network_community_constraints.drop conn in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "corrupt slug" Home_request_fixture.q_corrupt_community_slug
              (home, "phcv-corr bad slug")
          in
          let* () =
            load_expect "non-addressable target slug" Rm.Inconsistent_data
              conn ~user:a ~slug:"phcv-corr"
          in
          let* () =
            exec conn "restore slug" Home_request_fixture.q_corrupt_community_slug
              (home, "phcv-corr-home")
          in
          let* () = exec conn "blank name" Community_fixture.q_set_name (home, "   ") in
          let* () =
            load_expect "blank target name" Rm.Inconsistent_data conn
              ~user:a ~slug:"phcv-corr"
          in
          let* () =
            exec conn "restore name" Community_fixture.q_set_name (home, "Phcv Corr Home")
          in
          let* () =
            exec conn "corrupt description" q_corrupt_description home
          in
          load_expect "control-unsafe description" Rm.Inconsistent_data
            conn ~user:a ~slug:"phcv-corr")
        (fun () ->
          let* () =
            exec conn "recanonicalize" Network_community_constraints.q_recanonicalize
              (home, "phcv-corr-home", "Phcv Corr Home")
          in
          Network_community_constraints.restore conn))

let suite =
  [ pure_inputs_case; storage_case; steward_view_case; collapse_case;
    eligible_case; mixed_flags_case; active_pending_case;
    active_accepted_case; historical_case; drift_case;
    corrupt_active_case ]

let suites =
    (* Home choice read model: owner-authorized view of the active home
       relation or the deterministic eligible target list.
       Database-gated. *)
  [ ("project_home_choice_read_model", suite)
  ]
