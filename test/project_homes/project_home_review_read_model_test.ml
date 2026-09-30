module Pi = Earde.Project_identity
module Phr = Earde.Project_home_relation

(* === Project home review read model (Project_home_review_read_model) ===
   Moderator-authorized view of the pending home-request queue for one
   community, driven over verified permanent projects built through the real
   draft/selection/finalization chain and pending relations written by the
   real request store. Database-gated (EARDE_TEST_DATABASE_URL, same opt-in
   as Mod_scope) with its own reserved external-installation-id range
   945800001..945800999 (hence account ids 945900001..945900999, which also
   scope the permanent-project cleanup), phrr_% usernames, and phrr-%
   community slugs so no suite shares fixtures. Pure validation is proven
   pre-SQL against a deliberately disconnected connection. Authorization uses
   only the real durable rows: community_moderators role 'top_mod' and
   users.is_admin. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Rv = Earde.Project_home_review_read_model

module Rq = Earde.Project_home_request_store

module Fin = Earde.Project_finalization_store

let error_str : Rv.error -> string = function
  | Rv.Invalid_user_id -> "Invalid_user_id"
  | Rv.Invalid_community_slug -> "Invalid_community_slug"
  | Rv.Inconsistent_data -> "Inconsistent_data"
  | Rv.Storage_error -> "Storage_error"

let verification_str : Rv.project_verification -> string = function
  | Rv.Verified -> "verified"
  | Rv.Stale -> "stale"
  | Rv.Revoked -> "revoked"

let eligibility_str : Rv.host_eligibility -> string = function
  | Rv.Eligible -> "eligible"
  | Rv.Currently_ineligible -> "ineligible"

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let make_project = Home_request_fixture.make_project

let insert_community = Community_fixture.insert_community

(* Same dependency order as the sibling suites; the LIKE pattern also
   catches deliberately corrupted phrr- slugs. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 945900001 AND 945900999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 945900001 AND 945900999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 945800001 AND 945800999)"
    ; "DELETE FROM communities WHERE slug LIKE 'phrr-%'"
    ; "DELETE FROM users WHERE username LIKE 'phrr_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 945800001 AND 945800999"
    ]

(* Durable authorization fixtures — the real rows the read model reads. *)
let q_insert_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  "INSERT INTO community_moderators (user_id, community_id, role) \
   VALUES ($1, $2, $3)"

let q_remove_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "DELETE FROM community_moderators WHERE user_id = $1 AND community_id = $2"

let q_set_moderator_role =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  "UPDATE community_moderators SET role = $3 \
   WHERE user_id = $1 AND community_id = $2"

let q_insert_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_set_admin =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
  "UPDATE users SET is_admin = $2 WHERE id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM users WHERE id = $1"

(* Deterministic queue ordering: creation time is a NOW() default, so it
   is pinned directly (a past value keeps updated_at >= created_at). *)
let q_set_created_at =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
  "UPDATE community_projects \
   SET created_at = $2::timestamptz, updated_at = NOW() WHERE id = $1"

(* Targeted durable corruption. request_note carries only a length CHECK,
   so a padded (non-canonical) note and a forbidden control byte are both
   directly representable. *)
let q_pad_note =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE community_projects SET request_note = '  padded  ' WHERE id = $1"

let q_control_note =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE community_projects \
   SET request_note = 'phrr' || chr(1) || 'bad' WHERE id = $1"

(* forge_namespace_login only CHECKs btrim<>'' and length, so a control
   byte is directly representable and fails the addressable-segment rule. *)
let q_corrupt_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE open_source_projects \
   SET forge_namespace_login = 'phrr' || chr(1) || 'bad' WHERE id = $1"

let q_restore_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE open_source_projects \
   SET forge_namespace_login = 'pfin-owner' WHERE id = $1"

(* html_url only CHECKs btrim<>'', so a non-canonical URL is directly
   representable and fails the structured reconstruction. *)
let q_corrupt_repo_url =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE project_repositories \
   SET html_url = 'https://evil.example/x' WHERE project_id = $1"

let q_restore_repo_url =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE project_repositories \
   SET html_url = 'https://github.com/pfin-owner/alpha' WHERE project_id = $1"

let q_delete_repos =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM project_repositories WHERE project_id = $1"

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

let load conn ~reviewer ~community =
  Rv.load_for_reviewer conn ~reviewer_user_id:reviewer
    ~community_slug:community

let load_view label conn ~reviewer ~community =
  let* r = load conn ~reviewer ~community in
  match r with
  | Ok (Some view) -> Lwt.return view
  | Ok None -> Alcotest.failf "%s: unexpectedly absent" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_none label conn ~reviewer ~community =
  let* r = load conn ~reviewer ~community in
  match r with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Alcotest.failf "%s: unexpectedly present" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn ~reviewer ~community =
  let* r = load conn ~reviewer ~community in
  match r with
  | Ok None -> Alcotest.failf "%s: expected %s, got Ok None" label (error_str expected)
  | Ok (Some _) -> Alcotest.failf "%s: expected %s, got Ok Some" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* Pending relations come only through the real transactional store. *)
let request_ok label conn ~user ~slug ~community ?note () =
  let relation = Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:note) in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok created -> Lwt.return (Rq.relation_id created)
  | Error e -> Alcotest.failf "%s: %s" label (Home_request_fixture.error_str e)

let add_role conn ~user ~community role =
  exec conn "role fixture" q_insert_moderator (user, community, role)

let add_top_mod conn ~user ~community =
  add_role conn ~user ~community "top_mod"

(* A two-repository verified project through the same real chain, for the
   repository ordering assertion. *)
let make_project2 conn ~user ~ext_id ~slug =
  let base = Int64.add ext_id 400000L in
  let* _inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        [ Project_fixture.repo ~account_id ~id:base "alpha"
        ; Project_fixture.repo ~account_id ~id:(Int64.add base 1L) "beta" ])
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let s1 = List.nth ids 0 and s2 = List.nth ids 1 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary:s1
      [ s1; s2 ]
  in
  let identity =
    Project_fixture.identity_exn ~slug ~selected:[ s1; s2 ] ~primary:s1 ()
  in
  let* created = Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity in
  Lwt.return (Fin.project_id created)

let expect_one label conn ~reviewer ~community ~slug =
  let* view = load_view label conn ~reviewer ~community in
  let requests = Rv.pending_requests view in
  Alcotest.(check int) (label ^ ": one request") 1 (List.length requests);
  Alcotest.(check string) (label ^ ": project slug") slug
    (Rv.project_slug (List.hd requests));
  Lwt.return_unit

(* === pure input validation === *)

let pure_inputs_case =
  db_case "review read: invalid inputs rejected before any SQL"
    (fun _conn ->
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some url -> url
        | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
      in
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let expect label e ~reviewer ~community =
        load_expect label e dead ~reviewer ~community
      in
      let* () = expect "user id 0" Rv.Invalid_user_id ~reviewer:0 ~community:"phrr-c" in
      let* () =
        expect "negative user id" Rv.Invalid_user_id ~reviewer:(-7)
          ~community:"phrr-c"
      in
      let* () =
        expect "user checked before community slug" Rv.Invalid_user_id
          ~reviewer:0 ~community:"also bad"
      in
      Lwt_list.iter_s
        (fun bad ->
          expect "invalid community slug" Rv.Invalid_community_slug
            ~reviewer:1 ~community:bad)
        [ ""
        ; "phrr c"
        ; " phrr-c"
        ; "phrr-c "
        ; "phrr/c"
        ; "phrr\tc"
        ; "phrr\nc"
        ; "phrr\x01c"
        ; "phrr\x7fc"
        ])

(* === authorization === *)

let authorization_case =
  db_case "review read: durable top-mod/admin authorize, all else absent"
    (fun conn ->
      let* owner = insert_user conn "phrr_owner" in
      let* mod1 = insert_user conn "phrr_mod1" in
      let* mod2 = insert_user conn "phrr_mod2" in
      let* admin = insert_user conn "phrr_admin" in
      let* member = insert_user conn "phrr_member" in
      let* stranger = insert_user conn "phrr_stranger" in
      let* plain_mod = insert_user conn "phrr_plainmod" in
      let* legacy = insert_user conn "phrr_legacy" in
      let* other = insert_user conn "phrr_other" in
      let* _, _project =
        make_project conn ~user:owner ~ext_id:945800001L ~slug:"phrr-auth"
      in
      let* cid = insert_community conn "phrr-auth-home" in
      let* other_cid = insert_community conn "phrr-auth-other" in
      let* _rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-auth"
          ~community:cid ~note:"please review" ()
      in
      let* () = add_top_mod conn ~user:mod1 ~community:cid in
      let* () = add_top_mod conn ~user:mod2 ~community:cid in
      let* () = exec conn "make admin" q_set_admin (admin, true) in
      let* () = add_role conn ~user:plain_mod ~community:cid "mod" in
      let* () = add_role conn ~user:legacy ~community:cid "legacy_mod" in
      let* () = exec conn "member" q_insert_member (member, cid) in
      let* () = add_top_mod conn ~user:other ~community:other_cid in
      (* Authorized reviewers each see exactly the one pending request. *)
      let* () = expect_one "top_mod" conn ~reviewer:mod1 ~community:"phrr-auth-home" ~slug:"phrr-auth" in
      let* () = expect_one "second top_mod" conn ~reviewer:mod2 ~community:"phrr-auth-home" ~slug:"phrr-auth" in
      let* () = expect_one "durable admin" conn ~reviewer:admin ~community:"phrr-auth-home" ~slug:"phrr-auth" in
      (* Everyone else collapses to Ok None. *)
      let* () = load_none "ordinary member" conn ~reviewer:member ~community:"phrr-auth-home" in
      (* A stranger is also the session-shaped admin case: no durable flag. *)
      let* () = load_none "non-member / session-shaped admin" conn ~reviewer:stranger ~community:"phrr-auth-home" in
      let* () = load_none "plain mod" conn ~reviewer:plain_mod ~community:"phrr-auth-home" in
      let* () = load_none "legacy mod" conn ~reviewer:legacy ~community:"phrr-auth-home" in
      let* () = load_none "other-community top_mod" conn ~reviewer:other ~community:"phrr-auth-home" in
      let* () = load_none "missing community" conn ~reviewer:admin ~community:"phrr-nope" in
      (* Downgrade and removal serialize through the durable role. *)
      let* () = exec conn "downgrade" q_set_moderator_role (mod1, cid, "mod") in
      let* () = load_none "downgraded top_mod" conn ~reviewer:mod1 ~community:"phrr-auth-home" in
      let* () = exec conn "remove" q_remove_moderator (mod2, cid) in
      load_none "removed top_mod" conn ~reviewer:mod2 ~community:"phrr-auth-home")

(* === community eligibility mapping === *)

let eligibility_case =
  db_case_lifecycle_relaxed "review read: community host-eligibility mapping" (fun conn ->
      let* reviewer = insert_user conn "phrr_emod" in
      let check label ~slug ~expect ?visibility ?indexable ?network
          ?onboarding ?discoverable () =
        let* cid =
          insert_community ?visibility ?indexable ?network ?onboarding
            ?discoverable conn slug
        in
        let* () = add_top_mod conn ~user:reviewer ~community:cid in
        let* view = load_view label conn ~reviewer ~community:slug in
        Alcotest.(check string)
          (label ^ ": eligibility") expect
          (eligibility_str (Rv.community_host_eligibility (Rv.community view)));
        Lwt.return_unit
      in
      let* () = check "public listed" ~slug:"phrr-elig-pub" ~expect:"eligible" () in
      let* () =
        check "unlisted" ~slug:"phrr-elig-unl" ~expect:"eligible"
          ~indexable:false ~discoverable:false ()
      in
      let* () =
        check "private" ~slug:"phrr-inelig-priv" ~expect:"ineligible"
          ~visibility:"private" ~indexable:false ~discoverable:false ()
      in
      let* () =
        check "draft" ~slug:"phrr-inelig-draft" ~expect:"ineligible"
          ~onboarding:"draft" ~visibility:"private" ~indexable:false
          ~discoverable:false ()
      in
      check "legacy" ~slug:"phrr-inelig-legacy" ~expect:"ineligible"
        ~network:false ())

let contradictory_flags_case =
  db_case_lifecycle_relaxed "review read: contradictory community flags are corruption"
    (fun conn ->
      let* reviewer = insert_user conn "phrr_cmod" in
      let* stranger = insert_user conn "phrr_cstr" in
      (* Published, public, network, but indexable <> discoverable — a
         leaking published shape. *)
      let* cid =
        insert_community ~indexable:true ~discoverable:false conn
          "phrr-corruptflags"
      in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* () =
        load_expect "mixed flags" Rv.Inconsistent_data conn ~reviewer
          ~community:"phrr-corruptflags"
      in
      (* An unauthorized reviewer still sees only absence: corruption is
         never a probe oracle. *)
      load_none "unauthorized sees no corruption" conn ~reviewer:stranger
        ~community:"phrr-corruptflags")

(* === queue contents and ordering === *)

let empty_queue_case =
  db_case "review read: empty queue with a valid community" (fun conn ->
      let* reviewer = insert_user conn "phrr_qmod" in
      let* cid = insert_community conn "phrr-empty" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* view = load_view "empty" conn ~reviewer ~community:"phrr-empty" in
      Alcotest.(check int) "no requests" 0
        (List.length (Rv.pending_requests view));
      Alcotest.(check string) "community name" "phrr-empty"
        (Rv.community_name (Rv.community view));
      Alcotest.(check string) "community slug" "phrr-empty"
        (Rv.community_slug (Rv.community view));
      Lwt.return_unit)

let ordering_case =
  db_case "review read: queue ordered by created_at then project slug"
    (fun conn ->
      let* owner = insert_user conn "phrr_oowner" in
      let* reviewer = insert_user conn "phrr_omod" in
      let* cid = insert_community conn "phrr-queue" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      (* Built in a scrambled order; created_at (then slug for the tie) is
         what the queue order must reflect. *)
      let build ext slug ts =
        let* _, _ = make_project conn ~user:owner ~ext_id:ext ~slug in
        let* rid = request_ok slug conn ~user:owner ~slug ~community:cid () in
        exec conn ("created_at " ^ slug) q_set_created_at (rid, ts)
      in
      let* () = build 945800010L "phrr-q-alpha" "2020-01-03 00:00:00+00" in
      let* () = build 945800011L "phrr-q-zulu" "2020-01-03 00:00:00+00" in
      let* () = build 945800012L "phrr-q-charlie" "2020-01-01 00:00:00+00" in
      let* () = build 945800013L "phrr-q-bravo" "2020-01-02 00:00:00+00" in
      let* view = load_view "queue" conn ~reviewer ~community:"phrr-queue" in
      let slugs = List.map Rv.project_slug (Rv.pending_requests view) in
      Alcotest.(check (list string)) "created_at then slug order"
        [ "phrr-q-charlie"; "phrr-q-bravo"; "phrr-q-alpha"; "phrr-q-zulu" ]
        slugs;
      Lwt.return_unit)

let excluded_statuses_case =
  db_case "review read: accepted/rejected/removed excluded from the queue"
    (fun conn ->
      let* owner = insert_user conn "phrr_exowner" in
      let* reviewer = insert_user conn "phrr_exmod" in
      let* cid = insert_community conn "phrr-excl" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let build ext slug mark =
        let* _, _ = make_project conn ~user:owner ~ext_id:ext ~slug in
        let* rid = request_ok slug conn ~user:owner ~slug ~community:cid () in
        exec conn ("mark " ^ slug) mark rid
      in
      let* () = build 945800020L "phrr-excl-acc" Home_request_fixture.q_mark_accepted in
      let* () = build 945800021L "phrr-excl-rej" Home_request_fixture.q_mark_rejected in
      let* () = build 945800022L "phrr-excl-rem" Home_request_fixture.q_mark_removed in
      let* view = load_view "excl" conn ~reviewer ~community:"phrr-excl" in
      Alcotest.(check int) "no pending" 0
        (List.length (Rv.pending_requests view));
      Lwt.return_unit)

(* === per-request detail === *)

let verification_mapping_case =
  db_case "review read: verified/stale/revoked project mapping"
    (fun conn ->
      let* owner = insert_user conn "phrr_vowner" in
      let* reviewer = insert_user conn "phrr_vmod" in
      let* cid = insert_community conn "phrr-verif" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      (* A request is created while verified, then verification drifts —
         the request must stay in the queue. *)
      let build ext slug status =
        let* _, project = make_project conn ~user:owner ~ext_id:ext ~slug in
        let* _rid = request_ok slug conn ~user:owner ~slug ~community:cid () in
        if status = "verified" then Lwt.return_unit
        else exec conn ("set " ^ status) Home_request_fixture.q_set_verification (project, status)
      in
      let* () = build 945800030L "phrr-v-verified" "verified" in
      let* () = build 945800031L "phrr-v-stale" "stale" in
      let* () = build 945800032L "phrr-v-revoked" "revoked" in
      let* view = load_view "verif" conn ~reviewer ~community:"phrr-verif" in
      let requests = Rv.pending_requests view in
      Alcotest.(check int) "three requests" 3 (List.length requests);
      let of_slug s = List.find (fun r -> Rv.project_slug r = s) requests in
      Alcotest.(check string) "verified maps" "verified"
        (verification_str (Rv.project_verification (of_slug "phrr-v-verified")));
      Alcotest.(check string) "stale maps" "stale"
        (verification_str (Rv.project_verification (of_slug "phrr-v-stale")));
      Alcotest.(check string) "revoked maps" "revoked"
        (verification_str (Rv.project_verification (of_slug "phrr-v-revoked")));
      Lwt.return_unit)

let detail_case =
  db_case "review read: project, repository, requester, and note detail"
    (fun conn ->
      let* owner = insert_user conn "phrr_downer" in
      let* reviewer = insert_user conn "phrr_dmod" in
      let* _, _ =
        make_project conn ~user:owner ~ext_id:945800040L ~slug:"phrr-detail"
      in
      let* cid = insert_community conn "phrr-detail-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let note = "Prima riga — gi\xc3\xa0 discutiamo qui \xe2\x98\x95" in
      let* _rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-detail"
          ~community:cid ~note ()
      in
      let* view =
        load_view "detail" conn ~reviewer ~community:"phrr-detail-home"
      in
      let req = List.hd (Rv.pending_requests view) in
      Alcotest.(check string) "project name" "Pfin Fixture Project"
        (Rv.project_name req);
      Alcotest.(check string) "project slug" "phrr-detail"
        (Rv.project_slug req);
      Alcotest.(check bool) "kind is project" true
        (Rv.project_kind req = Pi.Project);
      Alcotest.(check string) "namespace login" "pfin-owner"
        (Rv.project_namespace_login req);
      Alcotest.(check string) "verified" "verified"
        (verification_str (Rv.project_verification req));
      let repos = Rv.project_repositories req in
      Alcotest.(check int) "one repository" 1 (List.length repos);
      let r = List.hd repos in
      Alcotest.(check string) "repo full name" "pfin-owner/alpha"
        (Rv.repository_full_name r);
      Alcotest.(check string) "repo url" "https://github.com/pfin-owner/alpha"
        (Rv.repository_html_url r);
      Alcotest.(check bool) "repo primary" true (Rv.repository_is_primary r);
      Alcotest.(check bool) "repo not archived" false
        (Rv.repository_is_archived r);
      Alcotest.(check (option string)) "requester name" (Some "phrr_downer")
        (Rv.requester_name req);
      Alcotest.(check (option string)) "note round-trips" (Some note)
        (Rv.request_note req);
      Lwt.return_unit)

let repositories_order_case =
  db_case "review read: repositories preserved in stored position order"
    (fun conn ->
      let* owner = insert_user conn "phrr_rowner" in
      let* reviewer = insert_user conn "phrr_rmod" in
      let* _project =
        make_project2 conn ~user:owner ~ext_id:945800045L ~slug:"phrr-multi"
      in
      let* cid = insert_community conn "phrr-multi-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* _rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-multi"
          ~community:cid ()
      in
      let* view =
        load_view "multi" conn ~reviewer ~community:"phrr-multi-home"
      in
      let repos = Rv.project_repositories (List.hd (Rv.pending_requests view)) in
      Alcotest.(check (list string)) "repos in position order"
        [ "pfin-owner/alpha"; "pfin-owner/beta" ]
        (List.map Rv.repository_full_name repos);
      Alcotest.(check bool) "first is primary" true
        (Rv.repository_is_primary (List.hd repos));
      Lwt.return_unit)

let deleted_requester_case =
  db_case "review read: deleted requester collapses to None, note kept"
    (fun conn ->
      let* owner = insert_user conn "phrr_delowner" in
      let* reviewer = insert_user conn "phrr_delmod" in
      let* _, _ =
        make_project conn ~user:owner ~ext_id:945800050L ~slug:"phrr-del"
      in
      let* cid = insert_community conn "phrr-del-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* _rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-del" ~community:cid
          ~note:"keep me" ()
      in
      (* Deleting the requester (also the steward) SET NULLs the
         provenance; the project and its request survive. *)
      let* () = exec conn "delete requester" q_delete_user owner in
      let* view = load_view "del" conn ~reviewer ~community:"phrr-del-home" in
      let req = List.hd (Rv.pending_requests view) in
      Alcotest.(check (option string)) "requester None" None
        (Rv.requester_name req);
      Alcotest.(check (option string)) "note preserved" (Some "keep me")
        (Rv.request_note req);
      Lwt.return_unit)

(* === durable corruption === *)

let note_corruption_case =
  db_case "review read: non-canonical durable note is corruption"
    (fun conn ->
      let* owner = insert_user conn "phrr_nowner" in
      let* reviewer = insert_user conn "phrr_nmod" in
      let* _, _ =
        make_project conn ~user:owner ~ext_id:945800060L ~slug:"phrr-note"
      in
      let* cid = insert_community conn "phrr-note-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-note"
          ~community:cid ~note:"fine" ()
      in
      let* () = exec conn "pad note" q_pad_note rid in
      let* () =
        load_expect "padded note" Rv.Inconsistent_data conn ~reviewer
          ~community:"phrr-note-home"
      in
      let* () = exec conn "control note" q_control_note rid in
      load_expect "control note" Rv.Inconsistent_data conn ~reviewer
        ~community:"phrr-note-home")

let project_corruption_case =
  db_case "review read: durable project/repository corruption is Inconsistent"
    (fun conn ->
      let* owner = insert_user conn "phrr_cowner" in
      let* reviewer = insert_user conn "phrr_cmod2" in
      let* _, project =
        make_project conn ~user:owner ~ext_id:945800070L ~slug:"phrr-corr"
      in
      let* cid = insert_community conn "phrr-corr-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* _rid =
        request_ok "pending" conn ~user:owner ~slug:"phrr-corr"
          ~community:cid ()
      in
      let* () = exec conn "corrupt login" q_corrupt_login project in
      let* () =
        load_expect "corrupt login" Rv.Inconsistent_data conn ~reviewer
          ~community:"phrr-corr-home"
      in
      let* () = exec conn "restore login" q_restore_login project in
      let* () = exec conn "corrupt repo url" q_corrupt_repo_url project in
      let* () =
        load_expect "corrupt repo url" Rv.Inconsistent_data conn ~reviewer
          ~community:"phrr-corr-home"
      in
      let* () = exec conn "restore repo url" q_restore_repo_url project in
      let* () = exec conn "delete repos" q_delete_repos project in
      load_expect "missing repositories" Rv.Inconsistent_data conn ~reviewer
        ~community:"phrr-corr-home")

(* Emptying search_path hides the unqualified tables, so the first query
   fails at the SQL layer and Caqti returns an Error the store maps to
   Storage_error — a genuine query failure, not a torn-down connection
   (which the driver signals by raising, not by Error). Reset before the
   case returns so the per-case cleanup still runs. *)
let q_break_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO ''"

let q_reset_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

let storage_error_case =
  db_case "review read: storage failure surfaces as Storage_error"
    (fun conn ->
      let* () = exec conn "break search_path" q_break_search_path () in
      let* () =
        load_expect "broken schema" Rv.Storage_error conn ~reviewer:1
          ~community:"phrr-anything"
      in
      exec conn "reset search_path" q_reset_search_path ())

let suite =
  [ pure_inputs_case; authorization_case; eligibility_case;
    contradictory_flags_case; empty_queue_case; ordering_case;
    excluded_statuses_case; verification_mapping_case; detail_case;
    repositories_order_case; deleted_requester_case; note_corruption_case;
    project_corruption_case; storage_error_case ]

let suites =
    (* Moderator review read model: durable top-mod/admin authorization,
       community eligibility mapping, the pending queue and its ordering,
       and project/repository/requester/note validation. Database-gated. *)
  [ ("project_home_review_read_model", suite)
  ]
