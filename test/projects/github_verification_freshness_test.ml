module Phr = Earde.Project_home_relation

(* GitHub verification freshness, access loss and repository-claim release
   (docs/features/github-verification-lifecycle.md). Stored proof grants
   new GitHub-dependent authority only while it is fresh (30 days), is
   renewed only by the steward's own successful re-verification that still
   lists every claimed repository, and a claim with no fresh steward yields
   to a claimant GitHub verified within the day. Ranges: installations
   963200001..963200999, namespaces +100000, repositories +400000; users
   gvf_*, communities gvf-*. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rq = Earde.Project_home_request_store

module Rv = Earde.Project_home_review_store

module Pv = Earde.Project_home_provisioning_store

module Fin = Earde.Project_finalization_store

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let find_opt conn label q v =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find_opt q v in
  or_fail label r

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 963300001 AND 963300999)"
    ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 963300001 AND 963300999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 963200001 AND 963200999)"
    ; "DELETE FROM communities WHERE slug LIKE 'gvf-%'"
    ; "DELETE FROM users WHERE username LIKE 'gvf\\_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 963200001 AND 963200999"
    ]

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
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === queries === *)

let q_fresh_at_age =
  (Caqti_type.string ->! Caqti_type.bool)
  "SELECT github_evidence_is_fresh(NOW() - $1::interval)"

let q_age_steward =
  (Caqti_type.(t3 int64 int string) ->. Caqti_type.unit)
  "UPDATE project_stewards SET github_verified_at = NOW() - $3::interval \
   WHERE project_id = $1 AND user_id = $2"

let q_effective =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT project_github_verification(id, verification_status) \
   FROM open_source_projects WHERE id = $1"

let q_set_stored =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
  "UPDATE open_source_projects SET verification_status = $2 WHERE id = $1"

let q_steward_fresh =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.bool)
  "SELECT github_evidence_is_fresh(github_verified_at) \
   FROM project_stewards WHERE project_id = $1 AND user_id = $2"

let q_steward_evidence =
  (Caqti_type.(t2 int64 int) ->! Caqti_type.string)
  "SELECT github_verified_at::text FROM project_stewards \
   WHERE project_id = $1 AND user_id = $2"

let q_add_steward =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
  "INSERT INTO project_stewards \
     (project_id, user_id, github_installation_record_id, role) \
   VALUES ($1, $2, $3, 'steward')"

let q_claims =
  (Caqti_type.int64 ->* Caqti_type.(t2 int64 bool))
  "SELECT github_repository_id, released_at IS NOT NULL \
   FROM project_repositories WHERE project_id = $1 ORDER BY position"

let q_active_holder =
  (Caqti_type.int64 ->? Caqti_type.int64)
  "SELECT project_id FROM project_repositories \
   WHERE github_repository_id = $1 AND released_at IS NULL"

(* === fixtures === *)

let pending () = Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:None)

(* A permanent project over [repos] (ids), through the real draft,
   selection and finalization stores. Returns the installation record,
   the verified installation (to re-verify later) and the project id. *)
let make_project conn ~user ~ext_id ~slug repo_ids =
  let* inst, draft, v, _account =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        List.mapi
          (fun i id -> Project_fixture.repo ~account_id ~id (Printf.sprintf "r%d" i))
          repo_ids)
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let s1 = List.hd ids in
  let* () =
    Project_fixture.replace_ok "select" conn ~user ~draft ~primary:s1 ids
  in
  let* created =
    Project_fixture.finalize_ok "project" conn ~user ~draft
      (Project_fixture.identity_exn ~slug ~selected:ids ~primary:s1 ())
  in
  Lwt.return (inst, v, Fin.project_id created)

(* The steward's own successful re-verification: the real draft store
   with a fresh listing of [repo_ids] under the same installation. *)
let reverify conn ~user ~v ~ext_id repo_ids =
  let account_id = Int64.add ext_id 100000L in
  let* set =
    Project_fixture.repo_set ~installation:v
      (List.mapi
         (fun i id -> Project_fixture.repo ~account_id ~id (Printf.sprintf "r%d" i))
         repo_ids)
  in
  let* _ = Project_fixture.refresh_ok "re-verification" conn ~user v set in
  Lwt.return_unit

let age conn ~project ~user days =
  exec conn "age evidence" q_age_steward
    (project, user, Printf.sprintf "%d days" days)

let effective conn project = find conn "effective" q_effective project

(* === cases === *)

(* The one window, at its edges. *)
let window_case =
  db_case "freshness: evidence is fresh for exactly 30 days" (fun conn ->
      let at age = find conn age q_fresh_at_age age in
      let* inside = at "29 days 23 hours 59 minutes" in
      let* edge = at "30 days" in
      let* outside = at "30 days 1 minute" in
      Alcotest.(check bool) "a minute inside the window" true inside;
      Alcotest.(check bool) "exactly 30 days old is no longer fresh" false edge;
      Alcotest.(check bool) "a minute past the window" false outside;
      Lwt.return_unit)

(* Stored 'verified' holds only while some steward is fresh; other
   stored states pass through. *)
let effective_status_case =
  db_case "status: verified only while some steward's evidence is fresh"
    (fun conn ->
      let* a = insert_user conn "gvf_eff_a" in
      let* b = insert_user conn "gvf_eff_b" in
      let* inst, _, project =
        make_project conn ~user:a ~ext_id:963200001L ~slug:"gvf-eff"
          [ 963600001L ]
      in
      let* s = effective conn project in
      Alcotest.(check string) "fresh from onboarding" "verified" s;
      let* () = age conn ~project ~user:a 31 in
      let* s = effective conn project in
      Alcotest.(check string) "the only steward went stale" "stale" s;
      let* () = exec conn "second steward" q_add_steward (project, b, inst) in
      let* s = effective conn project in
      Alcotest.(check string) "any fresh steward keeps it verified" "verified" s;
      let* () = exec conn "revoke" q_set_stored (project, "revoked") in
      let* s = effective conn project in
      Alcotest.(check string) "a stored revocation stands" "revoked" s;
      Lwt.return_unit)

(* New GitHub-dependent authority is bound to the acting steward's own
   evidence: a stale steward cannot request a home or provision one,
   even while a co-steward's fresh evidence keeps the project verified. *)
let actor_gates_case =
  db_case "authority: request and provisioning need the actor's own fresh evidence"
    (fun conn ->
      let* a = insert_user conn "gvf_gate_a" in
      let* b = insert_user conn "gvf_gate_b" in
      let* inst, _, project =
        make_project conn ~user:a ~ext_id:963200011L ~slug:"gvf-gate"
          [ 963600011L ]
      in
      let* () = exec conn "co-steward" q_add_steward (project, b, inst) in
      let* cid = Home_request_fixture.insert_community conn "gvf-gate-target" in
      let* () = age conn ~project ~user:a 31 in
      let* () =
        Home_request_fixture.create_expect "stale steward requests" Rq.Project_unavailable
          conn ~user:a ~slug:"gvf-gate" ~community:cid (pending ())
      in
      let* () =
        Home_provisioning_fixture.provision_expect "stale steward provisions" Pv.Project_unavailable
          conn ~actor:a ~slug:"gvf-gate"
          (Home_provisioning_fixture.identity ~slug:"gvf-gate-home" ())
      in
      (* The fresh co-steward still can: nothing was revoked globally. *)
      let* _ =
        Home_request_fixture.create_ok "fresh steward requests" conn ~user:b ~slug:"gvf-gate"
          ~community:cid (pending ())
      in
      Lwt.return_unit)

let provisioning_renewal_case =
  db_case "authority: the steward's own re-verification restores it" (fun conn ->
      let* a = insert_user conn "gvf_renew_a" in
      let* _inst, v, project =
        make_project conn ~user:a ~ext_id:963200021L ~slug:"gvf-renew"
          [ 963600021L; 963600022L ]
      in
      let* () = age conn ~project ~user:a 31 in
      let* () =
        Home_provisioning_fixture.provision_expect "stale" Pv.Project_unavailable conn ~actor:a
          ~slug:"gvf-renew" (Home_provisioning_fixture.identity ~slug:"gvf-renew-home" ())
      in
      (* A listing that lost one claimed repository renews nothing. *)
      let* () = reverify conn ~user:a ~v ~ext_id:963200021L [ 963600021L ] in
      let* fresh = find conn "fresh" q_steward_fresh (project, a) in
      Alcotest.(check bool) "partial access renews nothing" false fresh;
      (* Every claimed repository listed again: renewed. *)
      let* () =
        reverify conn ~user:a ~v ~ext_id:963200021L
          [ 963600021L; 963600022L; 963600023L ]
      in
      let* fresh = find conn "fresh" q_steward_fresh (project, a) in
      Alcotest.(check bool) "full access renews" true fresh;
      let* _ =
        Home_provisioning_fixture.provision_ok "renewed" conn ~actor:a ~slug:"gvf-renew"
          ~expect_slug:"gvf-renew-home" (Home_provisioning_fixture.identity ~slug:"gvf-renew-home" ())
      in
      Lwt.return_unit)

(* Another user's verification of the same installation renews and
   revokes nothing of the steward's. *)
let other_user_case =
  db_case "renewal: another user's verification neither renews nor revokes a steward"
    (fun conn ->
      let* a = insert_user conn "gvf_other_a" in
      let* b = insert_user conn "gvf_other_b" in
      let* _inst, v, project =
        make_project conn ~user:a ~ext_id:963200031L ~slug:"gvf-other"
          [ 963600031L ]
      in
      let* () = age conn ~project ~user:a 10 in
      let* before = find conn "evidence" q_steward_evidence (project, a) in
      let* () = reverify conn ~user:b ~v ~ext_id:963200031L [ 963600031L ] in
      let* after = find conn "evidence" q_steward_evidence (project, a) in
      Alcotest.(check string) "the steward's evidence is untouched" before after;
      let* fresh = find conn "fresh" q_steward_fresh (project, a) in
      Alcotest.(check bool) "and still fresh" true fresh;
      Lwt.return_unit)

(* A moderator can accept only while the project is currently verified;
   rejection stays possible in every state. *)
let review_case =
  db_case "review: accept needs a fresh steward, reject never does" (fun conn ->
      let* a = insert_user conn "gvf_rev_a" in
      let* m = insert_user conn "gvf_rev_m" in
      let* _inst, _, project =
        make_project conn ~user:a ~ext_id:963200041L ~slug:"gvf-rev"
          [ 963600041L ]
      in
      let* cid = Home_request_fixture.insert_community conn "gvf-rev-target" in
      let* () = Home_review_fixture.add_top_mod conn ~user:m ~community:cid in
      let* _ =
        Home_review_fixture.request_ok "request" conn ~user:a ~slug:"gvf-rev" ~community:cid ()
      in
      let* () = age conn ~project ~user:a 31 in
      let* () =
        Home_review_fixture.review_expect "stale accept" Rv.Project_unavailable conn
          ~reviewer:m ~slug:"gvf-rev" ~community:"gvf-rev-target" Rv.Accept
      in
      Home_review_fixture.review_ok "stale reject" Phr.Rejected conn ~reviewer:m
        ~slug:"gvf-rev" ~community:"gvf-rev-target" Rv.Reject)

(* A claim with no fresh steward yields to a claimant GitHub verified
   within the day; the stale project's claims are released together and
   kept as history. A fresh holder is never displaced. *)
let claim_release_case =
  db_case "claims: a stale project's claims yield to a freshly verified claimant"
    (fun conn ->
      let* old_owner = insert_user conn "gvf_claim_old" in
      let* claimant = insert_user conn "gvf_claim_new" in
      let* _, _, old_project =
        make_project conn ~user:old_owner ~ext_id:963200051L ~slug:"gvf-claim-old"
          [ 963600051L; 963600052L ]
      in
      (* Fresh holder: the claimant's finalization is refused. *)
      let attempt label slug =
        let* _, draft, _, _ =
          Project_fixture.make_draft conn ~user:claimant ~ext_id:963200052L (fun account_id ->
              [ Project_fixture.repo ~account_id ~id:963600051L "shared" ])
        in
        let* ids = Project_fixture.snapshot_ids conn draft in
        let s1 = List.hd ids in
        let* () =
          Project_fixture.replace_ok label conn ~user:claimant ~draft ~primary:s1 ids
        in
        Lwt.return (draft, Project_fixture.identity_exn ~slug ~selected:ids ~primary:s1 ())
      in
      let* draft, identity = attempt "fresh holder" "gvf-claim-new" in
      let* () =
        Project_fixture.finalize_expect "fresh holder keeps the claim"
          Fin.Repository_already_connected conn ~user:claimant ~draft identity
      in
      let* claims = collect conn "claims" q_claims old_project in
      Alcotest.(check (list (pair int64 bool))) "nothing released"
        [ (963600051L, false); (963600052L, false) ] claims;
      (* Stale holder: released as a whole, and the claimant holds it. *)
      let* () = age conn ~project:old_project ~user:old_owner 31 in
      let* created = Project_fixture.finalize_ok "stale holder yields" conn ~user:claimant ~draft identity in
      let new_project = Fin.project_id created in
      let* claims = collect conn "claims" q_claims old_project in
      Alcotest.(check (list (pair int64 bool))) "every old claim released, rows kept"
        [ (963600051L, true); (963600052L, true) ] claims;
      let* holder = find_opt conn "holder" q_active_holder 963600051L in
      Alcotest.(check (option int64)) "the claimant now holds it" (Some new_project) holder;
      let* holder = find_opt conn "holder" q_active_holder 963600052L in
      Alcotest.(check (option int64)) "the sibling is simply free" None holder;
      let* s = effective conn old_project in
      Alcotest.(check string) "the old project reads stale" "stale" s;
      (* The old steward cannot renew over released claims. *)
      let* fresh = find conn "old fresh" q_steward_fresh (old_project, old_owner) in
      Alcotest.(check bool) "old steward stays stale" false fresh;
      Lwt.return_unit)

(* The release is part of the finalization transaction: a finalization
   that fails for another reason releases nothing. *)
let claim_release_rollback_case =
  db_case "claims: a failed finalization releases nothing" (fun conn ->
      let* old_owner = insert_user conn "gvf_rb_old" in
      let* claimant = insert_user conn "gvf_rb_new" in
      let* _, _, old_project =
        make_project conn ~user:old_owner ~ext_id:963200061L ~slug:"gvf-rb-old"
          [ 963600061L ]
      in
      let* () = age conn ~project:old_project ~user:old_owner 31 in
      let* _, draft, _, _ =
        Project_fixture.make_draft conn ~user:claimant ~ext_id:963200062L (fun account_id ->
            [ Project_fixture.repo ~account_id ~id:963600061L "shared" ])
      in
      let* ids = Project_fixture.snapshot_ids conn draft in
      let s1 = List.hd ids in
      let* () = Project_fixture.replace_ok "select" conn ~user:claimant ~draft ~primary:s1 ids in
      (* The old project's slug is taken: the finalization fails late. *)
      let* () =
        Project_fixture.finalize_expect "slug taken" Fin.Slug_unavailable conn ~user:claimant
          ~draft (Project_fixture.identity_exn ~slug:"gvf-rb-old" ~selected:ids ~primary:s1 ())
      in
      let* claims = collect conn "claims" q_claims old_project in
      Alcotest.(check (list (pair int64 bool))) "claim untouched" [ (963600061L, false) ] claims;
      Lwt.return_unit)

(* === renewal vs. claim release === *)

(* Renewal decides on project_repositories and writes project_stewards;
   release decides on project_stewards and writes project_repositories.
   These cases force each interleaving with a third connection that holds
   a row lock one side needs, so the first side is suspended after its
   decision and before its commit. The steward renews either through an
   existing active draft (no installation lock is taken) or a new one
   (whose foreign key takes one). Only two outcomes are serializable:
   renewal first (the claims stay active and the claimant is refused) or
   release first (the claims are released and renewal restores no fresh
   authority over the project). *)

let q_steward_installation =
  (Caqti_type.(t3 int64 int int64) ->. Caqti_type.unit)
  "UPDATE project_stewards SET github_installation_record_id = $3 \
   WHERE project_id = $1 AND user_id = $2"

let q_backend_pid = (Caqti_type.unit ->! Caqti_type.int) "SELECT pg_backend_pid()"

let q_waiting =
  (Caqti_type.int ->! Caqti_type.bool)
  "SELECT EXISTS (SELECT 1 FROM pg_locks WHERE pid = $1 AND NOT granted)"

let q_statement_timeout =
  (Caqti_type.unit ->. Caqti_type.unit) "SET statement_timeout = '20s'"

let q_hold_claims =
  (Caqti_type.int64 ->* Caqti_type.int64)
  "SELECT id FROM project_repositories \
   WHERE project_id = $1 AND released_at IS NULL ORDER BY id FOR UPDATE"

let q_hold_steward =
  (Caqti_type.(t2 int64 int) ->* Caqti_type.int)
  "SELECT user_id FROM project_stewards \
   WHERE project_id = $1 AND user_id = $2 FOR UPDATE"

let connect_race () =
  match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
  | None | Some "" -> Alcotest.fail "gated case without a database URL"
  | Some url ->
      let* c = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* c = or_fail "connect" c in
      let* () = exec c "statement timeout" q_statement_timeout () in
      let* pid = find c "pid" q_backend_pid () in
      Lwt.return (c, pid)

let disconnect c =
  let (module C : Caqti_lwt.CONNECTION) = c in
  C.disconnect ()

(* Polls until [pid] waits on a lock or [p] has finished. *)
let settle conn ~pid p =
  let rec go n =
    match Lwt.state p with
    | Lwt.Return _ | Lwt.Fail _ -> Lwt.return `Finished
    | Lwt.Sleep ->
        if n = 0 then Alcotest.fail "race: neither blocked nor finished"
        else
          let* waiting = find conn "waiting" q_waiting pid in
          if waiting then Lwt.return `Blocked
          else
            let* () = Lwt_unix.sleep 0.01 in
            go (n - 1)
  in
  go 1000

let race_case ?(reinstalled = false) ?(claimant_record = false) ~tag ~ext_id ~repo_base ~existing_draft
    ~held () =
  let name =
    Printf.sprintf "race: %s first, renewal through %s draft%s%s"
      (match held with `Release -> "release" | `Renewal -> "renewal")
      (if existing_draft then "an existing" else "a new")
      (if reinstalled then " of a reinstalled app" else "")
      (if claimant_record then ", claimant on another installation record"
       else "")
  in
  db_case name (fun conn ->
      let* steward = insert_user conn ("gvf_race_s_" ^ tag) in
      let* rival = insert_user conn ("gvf_race_r_" ^ tag) in
      let r1 = repo_base and r2 = Int64.add repo_base 1L in
      let* inst, v, project =
        make_project conn ~user:steward ~ext_id ~slug:("gvf-race-" ^ tag)
          [ r1; r2 ]
      in
      let account_id = Int64.add ext_id 100000L in
      (* A reinstalled app: the same account under a new installation
         record, which renewal repoints the steward row at. *)
      let* v =
        if not reinstalled then Lwt.return v
        else
          let ext_id = Int64.succ ext_id in
          let* _ =
            Project_fixture.insert_installation ~login:"pfin-owner" conn ~ext_id
              ~account_id
          in
          Project_fixture.verified ~installation_id:ext_id ~account_id
            ~login:"pfin-owner" ~target:"User" ()
      in
      let listing ids =
        Project_fixture.repo_set ~installation:v
          (List.mapi
             (fun i id -> Project_fixture.repo ~account_id ~id (Printf.sprintf "r%d" i))
             ids)
      in
      (* An existing active draft of the steward's, before the evidence
         goes stale; without one the renewal below inserts a new draft. *)
      let* () =
        if existing_draft then reverify conn ~user:steward ~v ~ext_id [ r1; r2 ]
        else Lwt.return_unit
      in
      let* () = age conn ~project ~user:steward 31 in
      let* () =
        if reinstalled then
          exec conn "old installation" q_steward_installation
            (project, steward, inst)
        else Lwt.return_unit
      in
      (* The claimant's snapshot, fresh today: of the same installation
         record, or of another active record for the same account (a
         stale one nothing has marked revoked), which the installation
         lock alone then does not serialize. *)
      let* rival_v =
        if not claimant_record then Lwt.return v
        else
          let ext_id = Int64.succ ext_id in
          let* _ =
            Project_fixture.insert_installation ~login:"pfin-owner" conn ~ext_id
              ~account_id
          in
          Project_fixture.verified ~installation_id:ext_id ~account_id
            ~login:"pfin-owner" ~target:"User" ()
      in
      let* rival_set =
        Project_fixture.repo_set ~installation:rival_v
          [ Project_fixture.repo ~account_id ~id:r1 "r0" ]
      in
      let* rival_draft =
        Project_fixture.refresh_ok "claimant draft" conn ~user:rival rival_v rival_set
      in
      let rival_draft = Earde.Project_onboarding_draft_store.draft_id rival_draft in
      let* ids = Project_fixture.snapshot_ids conn rival_draft in
      let s1 = List.hd ids in
      let* () =
        Project_fixture.replace_ok "claimant selection" conn ~user:rival
          ~draft:rival_draft ~primary:s1 ids
      in
      let identity =
        Project_fixture.identity_exn ~slug:("gvf-race-new-" ^ tag) ~selected:ids ~primary:s1 ()
      in
      let* renewal_set = listing [ r1; r2 ] in
      let* rconn, rpid = connect_race () in
      let* lconn, lpid = connect_race () in
      let* bconn, _ = connect_race () in
      let (module B : Caqti_lwt.CONNECTION) = bconn in
      Lwt.finalize
        (fun () ->
          let* r = B.start () in
          let* () = or_fail "blocker start" r in
          let* () =
            match held with
            | `Release ->
                let* _ = collect bconn "hold claims" q_hold_claims project in
                Lwt.return_unit
            | `Renewal ->
                let* _ =
                  collect bconn "hold steward" q_hold_steward (project, steward)
                in
                Lwt.return_unit
          in
          let renewal () = Project_fixture.refresh rconn ~user:steward v renewal_set in
          let release () =
            Project_fixture.finalize lconn ~user:rival ~draft:rival_draft identity
          in
          (* The held side must be waiting on the blocker before the
             other side starts. *)
          let held_waits pid p =
            let* s = settle conn ~pid p in
            if s <> `Blocked then Alcotest.fail "race: the held side did not wait";
            Lwt.return_unit
          in
          let* renewal_p, release_p =
            match held with
            | `Release ->
                let l = release () in
                let* () = held_waits lpid l in
                let r = renewal () in
                let* _ = settle conn ~pid:rpid r in
                Lwt.return (r, l)
            | `Renewal ->
                let r = renewal () in
                let* () = held_waits rpid r in
                let l = release () in
                let* _ = settle conn ~pid:lpid l in
                Lwt.return (r, l)
          in
          let* r = B.rollback () in
          let* () = or_fail "blocker release" r in
          let* renewed = renewal_p in
          let* released = release_p in
          (match renewed with
           | Ok _ -> ()
           | Error e -> Alcotest.failf "renewal failed: %s" (Project_fixture.draft_error_str e));
          let* claims = collect conn "claims" q_claims project in
          let* fresh = find conn "fresh" q_steward_fresh (project, steward) in
          let* holder = find_opt conn "holder" q_active_holder r1 in
          (* The held side is suspended after its decision, so the other
             side must serialize behind it. *)
          match held, released with
          | `Renewal, Error Fin.Repository_already_connected ->
              Alcotest.(check (list (pair int64 bool)))
                "renewal first: the claims stay active" [ (r1, false); (r2, false) ] claims;
              Alcotest.(check bool) "renewal first: the steward is fresh" true fresh;
              Alcotest.(check (option int64)) "renewal first: the holder is unchanged"
                (Some project) holder;
              Lwt.return_unit
          | `Release, Ok created ->
              Alcotest.(check (list (pair int64 bool)))
                "release first: every claim released" [ (r1, true); (r2, true) ] claims;
              Alcotest.(check bool)
                "release first: renewal restores no fresh authority" false fresh;
              Alcotest.(check (option int64)) "release first: the claimant holds it"
                (Some (Fin.project_id created)) holder;
              Lwt.return_unit
          | _, Ok _ -> Alcotest.fail "the claimant took claims a renewal kept"
          | _, Error e ->
              Alcotest.failf "claimant: unexpected %s" (Project_fixture.finalize_error_str e))
        (fun () ->
          let* () = disconnect bconn in
          let* () = disconnect lconn in
          disconnect rconn))

let race_cases =
  [ race_case ~tag:"a" ~ext_id:963200071L ~repo_base:963600071L
      ~existing_draft:true ~held:`Release ();
    race_case ~tag:"b" ~ext_id:963200073L ~repo_base:963600073L
      ~existing_draft:true ~held:`Renewal ();
    race_case ~tag:"c" ~ext_id:963200075L ~repo_base:963600075L
      ~existing_draft:false ~held:`Release ();
    race_case ~tag:"d" ~ext_id:963200077L ~repo_base:963600077L
      ~existing_draft:false ~held:`Renewal ();
    race_case ~reinstalled:true ~tag:"e" ~ext_id:963200079L
      ~repo_base:963600079L ~existing_draft:true ~held:`Renewal ();
    race_case ~claimant_record:true ~tag:"f" ~ext_id:963200081L
      ~repo_base:963600081L ~existing_draft:true ~held:`Release ();
    race_case ~claimant_record:true ~tag:"g" ~ext_id:963200083L
      ~repo_base:963600083L ~existing_draft:true ~held:`Renewal () ]

let suite =
  [ window_case; effective_status_case; actor_gates_case;
    provisioning_renewal_case; other_user_case; review_case;
    claim_release_case; claim_release_rollback_case ]
  @ race_cases

let suites =
  [ ("github_verification_freshness", suite)
  ]
