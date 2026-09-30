module Ncpf = Earde.Network_community_publication_form

let q_count_by_slug = Home_provisioning_fixture.q_count_by_slug

(* === Atomic network-community publication store
   (Network_community_publication_store) ===
   The single atomic transaction behind the future POST /c/:slug/publish:
   pure validation before SQL, candidate resolution and the shared
   project-first lock order, the durable top_mod/admin publication
   authority and its collapse, the complete-draft and provisioned-relation
   validation, the pure lifecycle transition, the one guarded
   identity+lifecycle update and its savepoint-fenced slug-conflict
   classification, complete post-update validation, staged rollback
   injection, deterministic concurrency, and the privacy sweep. Reserved
   external-installation-id range 954000001..954000999 (hence account ids
   954100001..954100999, which also scope the permanent-project cleanup),
   ncps_% usernames, and ncps-% community slugs so no suite shares
   fixtures. Verified projects come only through the real draft/selection/
   finalization chain; drafts come only through the real provisioning
   store; publication submissions come only through the real publication
   form. Every per-case wrapper disconnects deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module St = Earde.Network_community_publication_store

module Pv = Earde.Project_home_provisioning_store

module Rms = Earde.Project_home_removal_store

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let collect = Db_fixture.collect

let make_project = Home_provisioning_fixture.make_project

let insert_community = Community_fixture.insert_community

let contains = Html_assert.occurs

let status_of = Http_fixture.status_of

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_home_audit_events \
       WHERE project_id IN \
         (SELECT id FROM open_source_projects \
          WHERE forge_namespace_id BETWEEN 954100001 AND 954100999)"
      ; "DELETE FROM open_source_projects \
       WHERE forge_namespace_id BETWEEN 954100001 AND 954100999"
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 954000001 AND 954000999)"
    ; "DELETE FROM communities WHERE slug LIKE 'ncps-%'"
    ; "DELETE FROM users WHERE username LIKE 'ncps_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 954000001 AND 954000999"
    ]

let q_insert_general_section =
  (Caqti_type.int ->. Caqti_type.unit)
  "INSERT INTO community_sections \
     (community_id, name, slug, description, position, default_sort, \
      is_introduction_section) \
   VALUES ($1, 'General', 'general', 'General discussion', 0, 'new', \
           FALSE)"

let q_archive_channels =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE channels SET is_archived = TRUE WHERE community_id = $1"

let q_unarchive_channels =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE channels SET is_archived = FALSE WHERE community_id = $1"

let q_delete_moderators =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM community_moderators WHERE community_id = $1"

let q_delete_members =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM community_members WHERE community_id = $1"

let q_delete_relations =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM community_projects WHERE community_id = $1"

(* Audit history deliberately RESTRICT-protects relation rows; a probe
   that drops a store-created relation must purge its trail first. *)
let q_purge_audit =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM project_home_audit_events WHERE community_id = $1"

(* Store-shaped accepted provisioned relation, for restoration after the
   absent-relation probe. *)
let q_insert_provisioned_relation =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status, reviewed_at) \
   VALUES ($1, $2, 'home', 'accepted', NOW())"

let q_insert_pending_relation =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "INSERT INTO community_projects \
     (project_id, community_id, relation_type, status) \
   VALUES ($1, $2, 'home', 'pending')"

let q_set_relation_requester =
  (Caqti_type.(t2 int64 (option int)) ->. Caqti_type.unit)
  "UPDATE community_projects SET requested_by_user_id = $2 WHERE id = $1"

let q_delete_relation_by_id =
  (Caqti_type.int64 ->. Caqti_type.unit)
  "DELETE FROM community_projects WHERE id = $1"

(* Lifecycle and identity corruption, only ever run with the matching
   scoped constraint dropped through Nclc_relax / Ncid_relax. *)
let q_set_visibility =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
  "UPDATE communities SET visibility = $2 WHERE id = $1"

let q_set_indexable =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
  "UPDATE communities SET indexable = $2 WHERE id = $1"

let q_set_name =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
  "UPDATE communities SET name = $2 WHERE id = $1"

let q_set_slug =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
  "UPDATE communities SET slug = $2 WHERE id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_set_verification = Home_request_fixture.q_set_verification

let q_mark_removed = Home_request_fixture.q_mark_removed

let q_remove_moderator = Community_fixture.q_remove_moderator

let q_set_moderator_role = Community_fixture.q_set_moderator_role

let q_insert_moderator = Community_fixture.q_insert_moderator

let q_insert_member = Community_fixture.q_insert_member

let q_set_admin = Community_fixture.q_set_admin

(* Test-only failure injection, dropped under Lwt.finalize: one RAISE
   trigger for the update-statement failure, and one side-effect trigger
   (an extra membership row) that survives the update itself but makes
   the complete post-update validation fail. *)
let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_create_fail_fn =
  ddl
    "CREATE FUNCTION ncps_fail_fn() RETURNS trigger \
     LANGUAGE plpgsql \
     AS 'BEGIN RAISE EXCEPTION ''ncps fixture failure''; END'"

let q_drop_fail_fn = ddl "DROP FUNCTION IF EXISTS ncps_fail_fn()"

let q_create_fail_trigger =
  ddl
    "CREATE TRIGGER ncps_fail_update AFTER UPDATE ON communities \
     FOR EACH ROW EXECUTE FUNCTION ncps_fail_fn()"

let q_drop_fail_trigger =
  ddl "DROP TRIGGER IF EXISTS ncps_fail_update ON communities"

let q_create_side_fn ~user_id =
  ddl
    (Printf.sprintf
       "CREATE FUNCTION ncps_side_fn() RETURNS trigger \
        LANGUAGE plpgsql \
        AS 'BEGIN INSERT INTO community_members (user_id, community_id) \
            VALUES (%d, NEW.id); RETURN NEW; END'"
       user_id)

let q_drop_side_fn = ddl "DROP FUNCTION IF EXISTS ncps_side_fn()"

let q_create_side_trigger =
  ddl
    "CREATE TRIGGER ncps_side_update AFTER UPDATE ON communities \
     FOR EACH ROW EXECUTE FUNCTION ncps_side_fn()"

let q_drop_side_trigger =
  ddl "DROP TRIGGER IF EXISTS ncps_side_update ON communities"

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
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let with_second ~url f =
  let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
  let* conn2 = or_fail "second connect" conn2 in
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  Lwt.finalize (fun () -> f conn2) (fun () -> C2.disconnect ())

(* === pure input validation === *)

let pure_inputs_case =
  db_case "publish: invalid inputs rejected before any SQL"
    (fun ~url _conn ->
      (* A deliberately unusable connection: pure validation must return
         without touching it — were any SQL attempted, the driver would
         raise on the finished connection and fail the test. *)
      let* dead = Caqti_lwt_unix.connect (Uri.of_string url) in
      let* dead = or_fail "dead connect" dead in
      let (module Dead : Caqti_lwt.CONNECTION) = dead in
      let* () = Dead.disconnect () in
      let value = Network_community_fixture.publication ~slug:"ncps-final" () in
      let expect label e ~actor ~current =
        Network_community_fixture.publish_expect label e dead ~actor ~current value
      in
      let* () = expect "user id 0" St.Invalid_user_id ~actor:0
                  ~current:"ncps-a" in
      let* () =
        expect "negative user id" St.Invalid_user_id ~actor:(-7)
          ~current:"ncps-a"
      in
      let* () =
        expect "user checked before slug" St.Invalid_user_id ~actor:0
          ~current:"not a slug"
      in
      Lwt_list.iter_s
        (fun bad ->
          expect
            ("invalid current slug " ^ String.escaped bad)
            St.Invalid_community_slug ~actor:1 ~current:bad)
        [ ""
        ; "ncps a"
        ; " ncps-a"
        ; "ncps-a "
        ; "ncps/a"
        ; "ncps\ta"
        ; "ncps\na"
        ; "ncps\x00a"
        ; "ncps\x1fa"
        ; "ncps\x7fa"
        ])

(* === public publication === *)

let public_case =
  db_case
    "publish: a top moderator atomically publishes Public with a new \
     identity" (fun ~url conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* unrelated = insert_user conn "ncps_other" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000001L
          ~project_slug:"ncps-alpha" ~slug:"ncps-alpha-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let name = "Ncps Alpha Hub \xc3\xa8" in
      let description = "Prima riga.\nSeconda con\ttab \xe2\x98\x95" in
      let* _ =
        Network_community_fixture.publish_ok "publish" conn ~actor:owner ~current:"ncps-alpha-home"
          ~expect_slug:"ncps-alpha-hub" ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name ~slug:"ncps-alpha-hub" ~description
             ~visibility:"public" ())
      in
      let* () =
        Network_community_fixture.check_published "published" conn ~cid ~project ~rid ~before ~name
          ~description ~slug:"ncps-alpha-hub" ~old_slug:"ncps-alpha-home"
          ~indexable:true ~discoverable:true
      in
      (* The real /c/:slug route now follows existing public-community
         authorization: the creator, an unrelated user, and the
         anonymous visitor all read the page. *)
      let* response, body =
        Connected_projects_fixture.visit ~session_user_id:owner ~url ~slug:"ncps-alpha-hub" ()
      in
      Alcotest.(check int) "creator reads the community" 200
        (status_of response);
      Alcotest.(check bool) "page carries the published name" true
        (contains ~needle:"Ncps Alpha Hub" body);
      let* response, _ =
        Connected_projects_fixture.visit ~session_user_id:unrelated ~url ~slug:"ncps-alpha-hub"
          ()
      in
      Alcotest.(check int) "unrelated user reads the community" 200
        (status_of response);
      let* response, _ = Connected_projects_fixture.visit ~url ~slug:"ncps-alpha-hub" () in
      Alcotest.(check int) "anonymous visitor reads the community" 200
        (status_of response);
      let* response, _ = Connected_projects_fixture.visit ~url ~slug:"ncps-alpha-home" () in
      Alcotest.(check int) "old slug is gone" 404 (status_of response);
      (* Existing discovery/indexing rules: a public indexable community
         appears in community search. *)
      Network_community_fixture.search_lists "public community is discoverable" conn
        ~name:"Ncps Alpha Hub" ~slug:"ncps-alpha-hub" true)

(* === unlisted publication === *)

let unlisted_case =
  db_case
    "publish: Unlisted lands public but neither indexable nor \
     discoverable" (fun ~url conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000002L
          ~project_slug:"ncps-unl" ~slug:"ncps-unl-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let name = "Ncps Unlisted Hub" in
      let* _ =
        Network_community_fixture.publish_ok "publish unlisted" conn ~actor:owner
          ~current:"ncps-unl-home" ~expect_slug:"ncps-unl-hub"
          ~expect_visibility:Ncpf.Unlisted
          (Network_community_fixture.publication ~name ~slug:"ncps-unl-hub" ~visibility:"unlisted"
             ())
      in
      let* () =
        Network_community_fixture.check_published "unlisted" conn ~cid ~project ~rid ~before ~name
          ~description:"<null>" ~slug:"ncps-unl-hub"
          ~old_slug:"ncps-unl-home" ~indexable:false ~discoverable:false
      in
      (* Reachable directly under existing public authorization... *)
      let* response, _ = Connected_projects_fixture.visit ~url ~slug:"ncps-unl-hub" () in
      Alcotest.(check int) "anonymous direct visit succeeds" 200
        (status_of response);
      (* ...but absent from discovery and indexing surfaces. *)
      Network_community_fixture.search_lists "unlisted community is not discoverable" conn
        ~name:"Ncps Unlisted Hub" ~slug:"ncps-unl-hub" false)

(* === same-slug publication === *)

let same_slug_case =
  db_case
    "publish: keeping the current slug succeeds without conflict \
     handling" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000003L
          ~project_slug:"ncps-same" ~slug:"ncps-same-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let* _ =
        Network_community_fixture.publish_ok "same slug" conn ~actor:owner
          ~current:"ncps-same-home" ~expect_slug:"ncps-same-home"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name:"Ncps Same Hub" ~slug:"ncps-same-home" ())
      in
      let* () =
        Network_community_fixture.check_published "same slug" conn ~cid ~project ~rid ~before
          ~name:"Ncps Same Hub" ~description:"<null>"
          ~slug:"ncps-same-home" ~old_slug:"ncps-same-home"
          ~indexable:true ~discoverable:true
      in
      (* Replay: a published community is no longer a draft. *)
      Network_community_fixture.publish_expect "replay" St.Draft_unavailable conn ~actor:owner
        ~current:"ncps-same-home"
        (Network_community_fixture.publication ~slug:"ncps-same-home" ()))

(* === authorization variants === *)

let authority_case =
  db_case
    "publish: second top moderator, durable admin, and multiply \
     authorized actors all publish" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* second = insert_user conn "ncps_second" in
      let* admin = insert_user conn "ncps_admin" in
      let* () = exec conn "admin flag" q_set_admin (admin, true) in
      (* A second current top moderator. *)
      let* _, cid, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000004L
          ~project_slug:"ncps-au1" ~slug:"ncps-au1-home"
      in
      let* () = exec conn "second member" q_insert_member (second, cid) in
      let* () =
        exec conn "second top mod" q_insert_moderator
          (second, cid, "top_mod")
      in
      let* _ =
        Network_community_fixture.publish_ok "second top mod" conn ~actor:second
          ~current:"ncps-au1-home" ~expect_slug:"ncps-au1-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-au1-hub" ())
      in
      (* A durable global admin who is neither member nor moderator. *)
      let* _, _, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000005L
          ~project_slug:"ncps-au2" ~slug:"ncps-au2-home"
      in
      let* _ =
        Network_community_fixture.publish_ok "durable admin" conn ~actor:admin
          ~current:"ncps-au2-home" ~expect_slug:"ncps-au2-hub"
          ~expect_visibility:Ncpf.Unlisted
          (Network_community_fixture.publication ~slug:"ncps-au2-hub" ~visibility:"unlisted" ())
      in
      (* A multiply authorized actor: creating top moderator and durable
         admin at once. *)
      let* _, _, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000006L
          ~project_slug:"ncps-au3" ~slug:"ncps-au3-home"
      in
      let* () = exec conn "owner admin too" q_set_admin (owner, true) in
      let* _ =
        Network_community_fixture.publish_ok "multiply authorized" conn ~actor:owner
          ~current:"ncps-au3-home" ~expect_slug:"ncps-au3-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-au3-hub" ())
      in
      exec conn "drop owner admin" q_set_admin (owner, false))

let unauthorized_case =
  db_case
    "publish: every unauthorized or unavailable identity collapses into \
     one error with no partial state" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* member = insert_user conn "ncps_member" in
      let* moderator = insert_user conn "ncps_mod" in
      let* legacy_mod = insert_user conn "ncps_legmod" in
      let* stranger = insert_user conn "ncps_stranger" in
      let* foreign_mod = insert_user conn "ncps_foreign" in
      let* downgraded = insert_user conn "ncps_down" in
      let* ghost = insert_user conn "ncps_ghost" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000007L
          ~project_slug:"ncps-un" ~slug:"ncps-un-home"
      in
      let* () = exec conn "member" q_insert_member (member, cid) in
      let* () = exec conn "mod member" q_insert_member (moderator, cid) in
      let* () =
        exec conn "mod role" q_insert_moderator (moderator, cid, "mod")
      in
      let* () = exec conn "legacy member" q_insert_member (legacy_mod, cid)
      in
      let* () =
        exec conn "legacy mod role" q_insert_moderator
          (legacy_mod, cid, "legacy_mod")
      in
      let* other_cid = insert_community conn "ncps-un-other" in
      let* () =
        exec conn "foreign top mod" q_insert_moderator
          (foreign_mod, other_cid, "top_mod")
      in
      let* () = exec conn "downgraded member" q_insert_member
                  (downgraded, cid) in
      let* () =
        exec conn "downgraded once top" q_insert_moderator
          (downgraded, cid, "top_mod")
      in
      let* () =
        exec conn "downgrade" q_set_moderator_role (downgraded, cid, "mod")
      in
      let* () = exec conn "delete ghost" q_delete_user ghost in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let value = Network_community_fixture.publication ~slug:"ncps-un-hub" () in
      let expect label actor =
        let* () =
          Network_community_fixture.publish_expect label St.Draft_unavailable conn ~actor
            ~current:"ncps-un-home" value
        in
        Network_community_fixture.check_unchanged label conn ~cid ~project ~rid before
      in
      (* A session-only admin claim never reaches the store, so a plain
         user with is_admin = FALSE is exactly that case. *)
      let* () = expect "ordinary member" member in
      let* () = expect "'mod' role" moderator in
      let* () = expect "'legacy_mod' role" legacy_mod in
      let* () = expect "unrelated user / session-only admin" stranger in
      let* () = expect "moderator of another community" foreign_mod in
      let* () = expect "downgraded top moderator" downgraded in
      let* () = expect "deleted user" ghost in
      (* Missing and legacy communities are the same collapse. *)
      let* () =
        Network_community_fixture.publish_expect "missing community" St.Draft_unavailable conn
          ~actor:owner ~current:"ncps-nowhere" value
      in
      let* _legacy =
        insert_community ~network:false conn "ncps-un-legacy"
      in
      let* () =
        Network_community_fixture.publish_expect "legacy community" St.Draft_unavailable conn
          ~actor:owner ~current:"ncps-un-legacy" value
      in
      let* n = find conn "no hub" q_count_by_slug "ncps-un-hub" in
      Alcotest.(check int) "no partial publication anywhere" 0 n;
      Lwt.return_unit)

(* === project verification states === *)

let verification_case =
  db_case
    "publish: verified, stale, and revoked projects all publish"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      (* Verified is every other success case; stale and revoked here. *)
      let* project, _, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000008L
          ~project_slug:"ncps-vf1" ~slug:"ncps-vf1-home"
      in
      let* () = exec conn "stale" q_set_verification (project, "stale") in
      let* _ =
        Network_community_fixture.publish_ok "stale project" conn ~actor:owner
          ~current:"ncps-vf1-home" ~expect_slug:"ncps-vf1-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-vf1-hub" ())
      in
      let* project2, _, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000009L
          ~project_slug:"ncps-vf2" ~slug:"ncps-vf2-home"
      in
      let* () = exec conn "revoked" q_set_verification (project2, "revoked")
      in
      let* _ =
        Network_community_fixture.publish_ok "revoked project" conn ~actor:owner
          ~current:"ncps-vf2-home" ~expect_slug:"ncps-vf2-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-vf2-hub" ())
      in
      Lwt.return_unit)

(* === draft corruption === *)

(* Durable corruption defenses that cannot be produced without relaxing
   production constraints stay defensive and undriven here:
   communities.visibility and onboarding_state off-enum values,
   community_projects.status/relation_type off-enum values, a second
   active home for one project (partial unique index), and non-positive
   ids (sequences). The lifecycle and identity shapes below are now
   constraint-protected too, so they run with the matching scoped CHECK
   dropped through Nclc_relax / Ncid_relax and restored under
   Lwt.finalize once the row is normalized again. *)

let shell_corruption_case =
  db_case
    "publish: a missing or malformed shell, membership, moderation, or \
     relation is refused as corruption" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* admin = insert_user conn "ncps_admin" in
      let* () = exec conn "admin flag" q_set_admin (admin, true) in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000010L
          ~project_slug:"ncps-cor" ~slug:"ncps-cor-home"
      in
      let value = Network_community_fixture.publication ~slug:"ncps-cor-hub" () in
      (* The durable-admin actor keeps authorization satisfied, so each
         probe reaches the complete-draft validation it targets. *)
      let expect label =
        let* () =
          Network_community_fixture.publish_expect label St.Inconsistent_data conn ~actor:admin
            ~current:"ncps-cor-home" value
        in
        let* n = find conn (label ^ ": no hub") q_count_by_slug
                   "ncps-cor-hub" in
        Alcotest.(check int) (label ^ ": no partial publication") 0 n;
        Lwt.return_unit
      in
      (* Missing General section. *)
      let* () = exec conn "drop sections" Network_community_fixture.q_delete_sections cid in
      let* () = expect "missing General section" in
      let* () = exec conn "restore section" q_insert_general_section cid in
      (* Missing (archived) general channel. *)
      let* () = exec conn "archive channels" q_archive_channels cid in
      let* () = expect "missing general channel" in
      let* () = exec conn "unarchive channels" q_unarchive_channels cid in
      (* No top moderator at all. *)
      let* () = exec conn "drop moderators" q_delete_moderators cid in
      let* () = expect "no top moderator" in
      let* () =
        exec conn "restore top mod" q_insert_moderator
          (owner, cid, "top_mod")
      in
      (* No member at all. *)
      let* () = exec conn "drop members" q_delete_members cid in
      let* () = expect "no member" in
      let* () = exec conn "restore member" q_insert_member (owner, cid) in
      (* Absent accepted relation. *)
      let* () = exec conn "purge audit" q_purge_audit cid in
      let* () = exec conn "drop relations" q_delete_relations cid in
      let* () = expect "absent accepted relation" in
      let* () =
        exec conn "restore relation" q_insert_provisioned_relation
          (project, cid)
      in
      (* Non-provisioned accepted relation shape: a fabricated
         requester. *)
      let* relation_ids = collect conn "relation ids" Home_provisioning_fixture.q_relation_ids
                            project in
      let* rid2 =
        match relation_ids with
        | [ r ] -> Lwt.return r
        | _ -> Alcotest.fail "expected one restored relation"
      in
      let* () =
        exec conn "fabricate requester" q_set_relation_requester
          (rid2, Some owner)
      in
      let* () = expect "non-provisioned accepted relation" in
      let* () =
        exec conn "clear requester" q_set_relation_requester (rid2, None)
      in
      (* Contradictory active relation state: a pending home relation of
         another project targeting this community. *)
      let* _, project2, _rid3 =
        let* _inst, p2 =
          make_project conn ~user:owner ~ext_id:954000011L
            ~slug:"ncps-cor2"
        in
        Lwt.return (0, p2, 0L)
      in
      let* () =
        exec conn "pending intruder" q_insert_pending_relation
          (project2, cid)
      in
      let* () = expect "pending relation on the community" in
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* pending_ids = collect conn "pending ids" Home_provisioning_fixture.q_relation_ids
                           project2 in
      let* () =
        Lwt_list.iter_s
          (fun id -> exec conn "drop intruder" q_delete_relation_by_id id)
          pending_ids
      in
      (* A second accepted home from another project: the candidate
         resolution itself must refuse the duplicate. *)
      let* () =
        exec conn "second accepted" q_insert_provisioned_relation
          (project2, cid)
      in
      let* () = expect "second accepted home on the community" in
      let* accepted_ids = collect conn "accepted ids" Home_provisioning_fixture.q_relation_ids
                            project2 in
      let* () =
        Lwt_list.iter_s
          (fun id -> exec conn "drop second" q_delete_relation_by_id id)
          accepted_ids
      in
      ignore rid;
      (* With every probe repaired the same inputs succeed — the
         refusals above were the corruption, not the store. *)
      let* _ =
        Network_community_fixture.publish_ok "clean run after probes" conn ~actor:admin
          ~current:"ncps-cor-home" ~expect_slug:"ncps-cor-hub"
          ~expect_visibility:Ncpf.Public value
      in
      Lwt.return_unit)

let lifecycle_corruption_case =
  db_case
    "publish: constraint-protected lifecycle and identity corruption is \
     refused" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* _project, cid, _rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000012L
          ~project_slug:"ncps-lcc" ~slug:"ncps-lcc-home"
      in
      let value = Network_community_fixture.publication ~slug:"ncps-lcc-hub" () in
      let expect label current =
        let* () =
          Network_community_fixture.publish_expect label St.Inconsistent_data conn ~actor:owner
            ~current value
        in
        let* n = find conn (label ^ ": no hub") q_count_by_slug
                   "ncps-lcc-hub" in
        Alcotest.(check int) (label ^ ": no partial publication") 0 n;
        Lwt.return_unit
      in
      (* Lifecycle shapes the new CHECK forbids: producible only with
         the constraint dropped for the length of the probe. *)
      let* () = Network_community_lifecycle_constraint.drop conn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = exec conn "public draft" q_set_visibility
                        (cid, "public") in
            let* () = expect "public draft" "ncps-lcc-home" in
            let* () = exec conn "back private" q_set_visibility
                        (cid, "private") in
            let* () = exec conn "leaking draft" q_set_indexable (cid, true)
            in
            let* () = expect "indexable draft" "ncps-lcc-home" in
            exec conn "back dark" q_set_indexable (cid, false))
          (fun () ->
            (* The row is a valid private draft again before the
               constraint returns, validated. *)
            let* () = exec conn "normalize visibility" q_set_visibility
                        (cid, "private") in
            let* () = exec conn "normalize indexable" q_set_indexable
                        (cid, false) in
            Network_community_lifecycle_constraint.restore conn)
      in
      (* Identity shapes the scoped identity CHECKs forbid. *)
      let* () = Network_community_constraints.drop conn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () =
              exec conn "control name" q_set_name
                (cid, "Ncps\x01Corrupt")
            in
            let* () = expect "control byte in name" "ncps-lcc-home" in
            let* () = exec conn "restore name" q_set_name
                        (cid, "Ncps Community Home") in
            let* () =
              exec conn "noncanonical slug" q_set_slug (cid, "Ncps-Upper")
            in
            let* () = expect "noncanonical stored slug" "Ncps-Upper" in
            exec conn "restore slug" q_set_slug (cid, "ncps-lcc-home"))
          (fun () ->
            let* () =
              exec conn "recanonicalize" Network_community_constraints.q_recanonicalize
                (cid, "ncps-lcc-home", "Ncps Community Home")
            in
            Network_community_constraints.restore conn)
      in
      (* The uncorrupted draft still publishes. *)
      let* _ =
        Network_community_fixture.publish_ok "clean run after corruption" conn ~actor:owner
          ~current:"ncps-lcc-home" ~expect_slug:"ncps-lcc-hub"
          ~expect_visibility:Ncpf.Public value
      in
      Lwt.return_unit)

(* === slug conflicts === *)

let slug_conflict_case =
  db_case
    "publish: legacy, draft, and published holders of the requested slug \
     all win, leaving the loser byte-identical" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000013L
          ~project_slug:"ncps-cf" ~slug:"ncps-cf-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let probe label taken =
        let* holder_id = find conn (label ^ ": holder id") Network_community_fixture.q_community_id
                           taken in
        let* holder_before = find conn (label ^ ": holder sig")
                               Network_community_fixture.q_community_sig holder_id in
        let* () =
          Network_community_fixture.publish_expect label St.Community_slug_unavailable conn
            ~actor:owner ~current:"ncps-cf-home"
            (Network_community_fixture.publication ~slug:taken ())
        in
        let* holder_after = find conn (label ^ ": holder sig after")
                              Network_community_fixture.q_community_sig holder_id in
        Alcotest.(check string) (label ^ ": holder unchanged")
          holder_before holder_after;
        let* n = find conn (label ^ ": one owner") q_count_by_slug taken
        in
        Alcotest.(check int) (label ^ ": exactly one row keeps the slug")
          1 n;
        Network_community_fixture.check_unchanged label conn ~cid ~project ~rid before
      in
      (* A legacy community. *)
      let* _legacy =
        insert_community ~network:false conn "ncps-taken-legacy"
      in
      let* () = probe "legacy holder" "ncps-taken-legacy" in
      (* Another network draft under its own current slug. *)
      let* _, _, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000014L
          ~project_slug:"ncps-cf2" ~slug:"ncps-cf2-home"
      in
      let* () = probe "draft holder" "ncps-cf2-home" in
      (* A published network community. *)
      let* _ =
        Network_community_fixture.publish_ok "publish the second draft" conn ~actor:owner
          ~current:"ncps-cf2-home" ~expect_slug:"ncps-cf2-hub"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-cf2-hub" ())
      in
      let* () = probe "published holder" "ncps-cf2-hub" in
      (* The loser is still a live draft: a fresh slug publishes. *)
      let* _ =
        Network_community_fixture.publish_ok "recovery" conn ~actor:owner ~current:"ncps-cf-home"
          ~expect_slug:"ncps-cf-free" ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-cf-free" ())
      in
      Lwt.return_unit)

(* === rollback injection === *)

let rollback_case =
  db_case
    "publish: a failing update or a failing post-update validation rolls \
     the whole publication back" (fun ~url:_ conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* intruder = insert_user conn "ncps_intruder" in
      let* project, cid, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000016L
          ~project_slug:"ncps-rb" ~slug:"ncps-rb-home"
      in
      let* before = Network_community_fixture.snapshot "before" conn ~cid ~project ~rid in
      let exec_ddl label q =
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* r = C.exec q () in
        let* _ = or_fail label r in
        Lwt.return_unit
      in
      let value = Network_community_fixture.publication ~slug:"ncps-rb-hub" () in
      (* 1: the community update itself fails. *)
      let* () = exec_ddl "create fail fn" q_create_fail_fn in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = exec_ddl "poison update" q_create_fail_trigger in
            Lwt.finalize
              (fun () ->
                let* () =
                  Network_community_fixture.publish_expect "poisoned update" St.Storage_error conn
                    ~actor:owner ~current:"ncps-rb-home" value
                in
                let* () =
                  Network_community_fixture.check_unchanged "poisoned update" conn ~cid ~project
                    ~rid before
                in
                let* n = find conn "no hub" q_count_by_slug "ncps-rb-hub"
                in
                Alcotest.(check int) "poisoned update: no publication" 0 n;
                Lwt.return_unit)
              (fun () -> exec_ddl "unpoison update" q_drop_fail_trigger))
          (fun () -> exec_ddl "drop fail fn" q_drop_fail_fn)
      in
      (* 2: the update succeeds but the complete post-update validation
         finds a moved aggregate (a membership row inserted behind the
         store's back) and refuses to commit. *)
      let* () = exec_ddl "create side fn" (q_create_side_fn
                                             ~user_id:intruder) in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = exec_ddl "install side trigger"
                        q_create_side_trigger in
            Lwt.finalize
              (fun () ->
                let* () =
                  Network_community_fixture.publish_expect "poisoned validation"
                    St.Inconsistent_data conn ~actor:owner
                    ~current:"ncps-rb-home" value
                in
                let* () =
                  Network_community_fixture.check_unchanged "poisoned validation" conn ~cid ~project
                    ~rid before
                in
                let* n = find conn "no hub" q_count_by_slug "ncps-rb-hub"
                in
                Alcotest.(check int) "poisoned validation: no publication"
                  0 n;
                let* members = find conn "members" Home_request_fixture.q_count_members
                                 cid in
                Alcotest.(check int)
                  "the injected membership rolled back too" 1 members;
                Lwt.return_unit)
              (fun () ->
                exec_ddl "drop side trigger" q_drop_side_trigger))
          (fun () -> exec_ddl "drop side fn" q_drop_side_fn)
      in
      (* With every trigger removed the same inputs succeed. *)
      let* _ =
        Network_community_fixture.publish_ok "clean run after poison" conn ~actor:owner
          ~current:"ncps-rb-home" ~expect_slug:"ncps-rb-hub"
          ~expect_visibility:Ncpf.Public value
      in
      Lwt.return_unit)

(* === concurrency === *)

let same_draft_race_case =
  db_case
    "publish: two submissions of one draft leave exactly one published \
     identity" (fun ~url conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* second = insert_user conn "ncps_second" in
      let* _, cid, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000017L
          ~project_slug:"ncps-race1" ~slug:"ncps-race1-home"
      in
      let* () = exec conn "second member" q_insert_member (second, cid) in
      let* () =
        exec conn "second top mod" q_insert_moderator
          (second, cid, "top_mod")
      in
      with_second ~url (fun conn2 ->
          let sub_a =
            Network_community_fixture.publication ~name:"Ncps Race A" ~slug:"ncps-race1-a"
              ~visibility:"public" ()
          in
          let sub_b =
            Network_community_fixture.publication ~name:"Ncps Race B" ~slug:"ncps-race1-b"
              ~visibility:"unlisted" ()
          in
          let* ra, rb =
            Lwt.both
              (Network_community_fixture.publish conn ~actor:owner ~current:"ncps-race1-home" sub_a)
              (Network_community_fixture.publish conn2 ~actor:second ~current:"ncps-race1-home"
                 sub_b)
          in
          let winner_state =
            match (ra, rb) with
            | Ok w, Error St.Draft_unavailable ->
                Alcotest.(check string) "winner slug" "ncps-race1-a"
                  (St.community_slug w);
                "Ncps Race A|<null>|public|published|true|true|true"
            | Error St.Draft_unavailable, Ok w ->
                Alcotest.(check string) "winner slug" "ncps-race1-b"
                  (St.community_slug w);
                "Ncps Race B|<null>|public|published|true|false|false"
            | Ok _, Ok _ -> Alcotest.fail "race: both succeeded"
            | Error a, Error b ->
                Alcotest.failf "race: both failed (%s, %s)" (Network_community_fixture.error_str a)
                  (Network_community_fixture.error_str b)
            | Ok _, Error e | Error e, Ok _ ->
                Alcotest.failf "race: unexpected loser error %s"
                  (Network_community_fixture.error_str e)
          in
          let* state = find conn "state" Network_community_fixture.q_community_state cid in
          Alcotest.(check string) "exactly the winner identity survives"
            winner_state state;
          let loser_slug =
            match ra with
            | Ok _ -> "ncps-race1-b"
            | Error _ -> "ncps-race1-a"
          in
          let* n = find conn "loser slug" q_count_by_slug loser_slug in
          Alcotest.(check int) "no partial second identity" 0 n;
          let* old_n = find conn "old slug" q_count_by_slug
                         "ncps-race1-home" in
          Alcotest.(check int) "the draft slug is gone" 0 old_n;
          Lwt.return_unit))

let same_slug_race_case =
  db_case
    "publish: two drafts racing for one final slug leave one clean \
     winner" (fun ~url conn ->
      let* owner_a = insert_user conn "ncps_owner" in
      let* owner_b = insert_user conn "ncps_second" in
      let* project_a, cid_a, rid_a =
        Network_community_fixture.make_draft conn ~user:owner_a ~ext_id:954000018L
          ~project_slug:"ncps-race2a" ~slug:"ncps-race2a-home"
      in
      let* project_b, cid_b, rid_b =
        Network_community_fixture.make_draft conn ~user:owner_b ~ext_id:954000019L
          ~project_slug:"ncps-race2b" ~slug:"ncps-race2b-home"
      in
      let* before_a = Network_community_fixture.snapshot "a before" conn ~cid:cid_a
                        ~project:project_a ~rid:rid_a in
      let* before_b = Network_community_fixture.snapshot "b before" conn ~cid:cid_b
                        ~project:project_b ~rid:rid_b in
      with_second ~url (fun conn2 ->
          let value = Network_community_fixture.publication ~slug:"ncps-shared-final" () in
          let* ra, rb =
            Lwt.both
              (Network_community_fixture.publish conn ~actor:owner_a ~current:"ncps-race2a-home"
                 value)
              (Network_community_fixture.publish conn2 ~actor:owner_b ~current:"ncps-race2b-home"
                 value)
          in
          let* () =
            match (ra, rb) with
            | Ok w, Error St.Community_slug_unavailable ->
                Alcotest.(check string) "winner slug" "ncps-shared-final"
                  (St.community_slug w);
                (* The loser remains byte-identical under its original
                   slug as a private draft. *)
                Network_community_fixture.check_unchanged "loser b" conn ~cid:cid_b
                  ~project:project_b ~rid:rid_b before_b
            | Error St.Community_slug_unavailable, Ok w ->
                Alcotest.(check string) "winner slug" "ncps-shared-final"
                  (St.community_slug w);
                Network_community_fixture.check_unchanged "loser a" conn ~cid:cid_a
                  ~project:project_a ~rid:rid_a before_a
            | Ok _, Ok _ -> Alcotest.fail "race: both succeeded"
            | Error a, Error b ->
                Alcotest.failf "race: both failed (%s, %s)" (Network_community_fixture.error_str a)
                  (Network_community_fixture.error_str b)
            | Ok _, Error e | Error e, Ok _ ->
                Alcotest.failf "race: unexpected loser error %s"
                  (Network_community_fixture.error_str e)
          in
          let* n = find conn "one owner" q_count_by_slug
                     "ncps-shared-final" in
          Alcotest.(check int) "exactly one community owns the slug" 1 n;
          Lwt.return_unit))

(* A publication blocked on the held relation row shows up as a backend
   some other backend is blocking; polling for it on the idle second
   connection is deterministic coordination, not a sleep. (A single
   row-lock waiter parks on the holder's transactionid, so pg_locks
   shows no not-granted entry on the table itself — pg_blocking_pids is
   the reliable signal.) *)
let q_relation_lock_waiters =
  (Caqti_type.unit ->! Caqti_type.int)
  "SELECT COUNT(*) FROM pg_stat_activity \
   WHERE datname = current_database() \
     AND cardinality(pg_blocking_pids(pid)) > 0"

let q_lock_relation_row =
  (Caqti_type.int64 ->! Caqti_type.int64)
  "SELECT id FROM community_projects WHERE id = $1 FOR UPDATE"

let versus_removal_case =
  db_case
    "publish: removal serializes on the shared project-first order in \
     both directions" (fun ~url conn ->
      let* owner = insert_user conn "ncps_owner" in
      (* Removal commits first, deterministically: the second connection
         locks the accepted relation row before the publication is
         launched, waits until the publication is provably blocked on
         that row (it has already passed candidate resolution and locked
         project, community, and authorization), then completes the
         removal-shaped mutation and commits — so the publication always
         re-reads the relation after the removal committed. *)
      let* _, _, rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000020L
          ~project_slug:"ncps-race3" ~slug:"ncps-race3-home"
      in
      let* () =
        with_second ~url (fun conn2 ->
            let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
            let* r = C2.start () in
            let* () = Home_review_fixture.or_fail "second begin" r in
            let* _ = find conn2 "hold relation" q_lock_relation_row rid in
            let publish_promise =
              Network_community_fixture.publish conn ~actor:owner ~current:"ncps-race3-home"
                (Network_community_fixture.publication ~slug:"ncps-race3-hub" ())
            in
            let rec await_blocked () =
              let* waiters =
                find conn2 "lock waiters" q_relation_lock_waiters ()
              in
              if waiters > 0 then Lwt.return_unit
              else
                let* () = Lwt.pause () in
                await_blocked ()
            in
            let* () = await_blocked () in
            let* () = exec conn2 "removal first" q_mark_removed rid in
            let* r = C2.commit () in
            let* () = Home_review_fixture.or_fail "second commit" r in
            let* r = publish_promise in
            match r with
            | Error St.Draft_unavailable -> Lwt.return_unit
            | Ok _ -> Alcotest.fail "published after removal"
            | Error e ->
                Alcotest.failf "unexpected error %s" (Network_community_fixture.error_str e))
      in
      let* n = find conn "no hub" q_count_by_slug "ncps-race3-hub" in
      Alcotest.(check int) "removal winner leaves no publication" 0 n;
      (* Publication commits first: the real removal store may then
         remove the accepted relation while the published community and
         its content remain. *)
      let* project2, cid2, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000021L
          ~project_slug:"ncps-race4" ~slug:"ncps-race4-home"
      in
      let* _ =
        Network_community_fixture.publish_ok "publication first" conn ~actor:owner
          ~current:"ncps-race4-home" ~expect_slug:"ncps-race4-home"
          ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~slug:"ncps-race4-home" ())
      in
      let* r =
        Rms.remove conn ~actor_user_id:owner ~project_slug:"ncps-race4"
          ~community_slug:"ncps-race4-home"
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error _ -> Alcotest.fail "removal after publication failed"
      in
      let* state = find conn "state" Network_community_fixture.q_community_state cid2 in
      Alcotest.(check string) "published community remains"
        "Ncps Community Home|<null>|public|published|true|true|true"
        state;
      let* active = find conn "active" Home_request_fixture.q_count_active_for_project
                      project2 in
      Alcotest.(check int) "the relation is removed" 0 active;
      Lwt.return_unit)

let authority_revocation_race_case =
  db_case
    "publish: top-mod removal, downgrade, and admin revocation that \
     commit first win the lock race" (fun ~url conn ->
      let* owner = insert_user conn "ncps_owner" in
      let* admin = insert_user conn "ncps_admin" in
      let* () = exec conn "admin flag" q_set_admin (admin, true) in
      let* _, cid, _ =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000022L
          ~project_slug:"ncps-race5" ~slug:"ncps-race5-home"
      in
      let race label ~mutate ~actor =
        with_second ~url (fun conn2 ->
            let* r =
              Db_fixture.serialized_mutation_first conn2
                ~mutate:(fun () -> mutate conn2)
                ~launch:(fun () ->
                  Network_community_fixture.publish conn ~actor ~current:"ncps-race5-home"
                    (Network_community_fixture.publication ~slug:"ncps-race5-hub" ()))
            in
            match r with
            | Error St.Draft_unavailable -> Lwt.return_unit
            | Ok _ -> Alcotest.failf "%s: published anyway" label
            | Error e ->
                Alcotest.failf "%s: unexpected error %s" label
                  (Network_community_fixture.error_str e))
      in
      (* Top-mod removal. *)
      let* () =
        race "top-mod removal"
          ~mutate:(fun conn2 ->
            exec conn2 "revoke role" q_remove_moderator (owner, cid))
          ~actor:owner
      in
      let* () =
        exec conn "restore role" q_insert_moderator (owner, cid, "top_mod")
      in
      (* Top-mod downgrade. *)
      let* () =
        race "top-mod downgrade"
          ~mutate:(fun conn2 ->
            exec conn2 "downgrade" q_set_moderator_role (owner, cid, "mod"))
          ~actor:owner
      in
      let* () =
        exec conn "restore role" q_set_moderator_role
          (owner, cid, "top_mod")
      in
      (* Durable-admin revocation, for an admin-only publisher. *)
      let* () =
        race "admin revocation"
          ~mutate:(fun conn2 ->
            exec conn2 "revoke admin" q_set_admin (admin, false))
          ~actor:admin
      in
      let* n = find conn "no hub" q_count_by_slug "ncps-race5-hub" in
      Alcotest.(check int) "no revoked publication survives" 0 n;
      (* A verification change that commits first does not block: all
         three known states publish. *)
      let* () =
        with_second ~url (fun conn2 ->
            let* project_id =
              find conn "project id"
                ((Caqti_type.string ->! Caqti_type.int64)
                   "SELECT id FROM open_source_projects WHERE slug = $1")
                "ncps-race5"
            in
            let* r =
              Db_fixture.serialized_mutation_first conn2
                ~mutate:(fun () ->
                  exec conn2 "stale first" q_set_verification
                    (project_id, "stale"))
                ~launch:(fun () ->
                  Network_community_fixture.publish conn ~actor:owner ~current:"ncps-race5-home"
                    (Network_community_fixture.publication ~slug:"ncps-race5-hub" ()))
            in
            match r with
            | Ok published ->
                Alcotest.(check string) "published despite staleness"
                  "ncps-race5-hub" (St.community_slug published);
                Lwt.return_unit
            | Error e ->
                Alcotest.failf "verification race: %s" (Network_community_fixture.error_str e))
      in
      Lwt.return_unit)

let versus_provisioning_race_case =
  db_case
    "publish: a provisioning insert racing for the same final slug \
     leaves exactly one owner" (fun ~url conn ->
      let* owner_a = insert_user conn "ncps_owner" in
      let* owner_b = insert_user conn "ncps_second" in
      let* project_a, cid_a, rid_a =
        Network_community_fixture.make_draft conn ~user:owner_a ~ext_id:954000023L
          ~project_slug:"ncps-race6" ~slug:"ncps-race6-home"
      in
      let* _inst, project_b =
        make_project conn ~user:owner_b ~ext_id:954000024L
          ~slug:"ncps-race7"
      in
      let* before_a = Network_community_fixture.snapshot "a before" conn ~cid:cid_a
                        ~project:project_a ~rid:rid_a in
      with_second ~url (fun conn2 ->
          let contested = "ncps-contested" in
          let* pub_r, prov_r =
            Lwt.both
              (Network_community_fixture.publish conn ~actor:owner_a ~current:"ncps-race6-home"
                 (Network_community_fixture.publication ~slug:contested ()))
              (Pv.provision conn2 ~actor_user_id:owner_b
                 ~project_slug:"ncps-race7"
                 ~identity:
                   (Home_provisioning_fixture.phvf_ok "provision identity"
                      (Home_provisioning_fixture.phvf_fields ~name:"Ncps Contested"
                         ~slug:contested ~description:"" ())))
          in
          let* () =
            match (pub_r, prov_r) with
            | Ok w, Error Pv.Community_slug_unavailable ->
                Alcotest.(check string) "publication owns the slug"
                  contested (St.community_slug w);
                (* The provisioning loser left no community and no
                   relation. *)
                let* relations =
                  find conn "loser relations" Home_request_fixture.q_count_for_project
                    project_b
                in
                Alcotest.(check int) "no loser relation" 0 relations;
                Lwt.return_unit
            | Error St.Community_slug_unavailable, Ok home ->
                Alcotest.(check string) "provisioning owns the slug"
                  contested (Pv.community_slug home);
                (* The publication loser remains byte-identical under
                   its original slug as a private draft. *)
                Network_community_fixture.check_unchanged "loser draft" conn ~cid:cid_a
                  ~project:project_a ~rid:rid_a before_a
            | Ok _, Ok _ -> Alcotest.fail "race: both owned the slug"
            | Error a, Error _ ->
                Alcotest.failf "race: both failed (publication %s)"
                  (Network_community_fixture.error_str a)
            | Ok _, Error _ ->
                Alcotest.fail "race: unexpected provisioning error"
            | Error e, _ ->
                Alcotest.failf "race: unexpected publication error %s"
                  (Network_community_fixture.error_str e)
          in
          let* n = find conn "one owner" q_count_by_slug contested in
          Alcotest.(check int) "exactly one community owns the slug" 1 n;
          Lwt.return_unit))

(* === privacy sweep === *)

let privacy_case =
  db_case
    "publish: no credential-shaped or external-identifier fixture \
     reaches the published surface" (fun ~url conn ->
      let* owner = insert_user conn "ncps_priv" in
      let* project, cid, _rid =
        Network_community_fixture.make_draft conn ~user:owner ~ext_id:954000015L
          ~project_slug:"ncps-priv" ~slug:"ncps-priv-home"
      in
      let name = "Ncps Private Hub" in
      let* published =
        Network_community_fixture.publish_ok "publish" conn ~actor:owner ~current:"ncps-priv-home"
          ~expect_slug:"ncps-priv-hub" ~expect_visibility:Ncpf.Public
          (Network_community_fixture.publication ~name ~slug:"ncps-priv-hub" ())
      in
      (* The abstract result exposes exactly the final slug and the
         selected mode — asserted through its only two accessors. *)
      Alcotest.(check string) "result slug only" "ncps-priv-hub"
        (St.community_slug published);
      Alcotest.(check string) "result visibility only" "public"
        (Network_community_fixture.ncpf_vis (St.publication_visibility published));
      let* community = find conn "community sig" Network_community_fixture.q_community_sig cid in
      let* sections = collect conn "sections" Home_provisioning_fixture.q_section_sigs cid in
      let* channels = collect conn "channels" Home_provisioning_fixture.q_channel_sigs cid in
      let* relation_ids = collect conn "relations" Home_provisioning_fixture.q_relation_ids
                            project in
      let* relation_blob =
        match relation_ids with
        | [ rid ] -> find conn "blob" Home_request_fixture.q_relation_text_blob rid
        | _ -> Alcotest.fail "expected one relation"
      in
      let* _, page = Connected_projects_fixture.visit ~url ~slug:"ncps-priv-hub" () in
      let durable_blob =
        String.concat "|"
          ((community :: relation_blob :: sections) @ channels)
      in
      let page_blob = durable_blob ^ "|" ^ page in
      (* The intentionally public identity is present; nothing from the
         owner's GitHub records, account, or the store's internals is.
         The project namespace login is deliberately public identity on
         the published page (the connected-projects section renders it by
         design), so its absence is asserted on the durable community
         surface only. *)
      Alcotest.(check bool) "published name present" true
        (contains ~needle:name page_blob);
      List.iter
        (fun (what, marker) ->
          Alcotest.(check bool)
            ("published surface carries no " ^ what)
            false
            (contains ~needle:marker page_blob))
        [ ("external installation id", "954000015");
          ("external account id", "954100015");
          ("external repository id", "954400015");
          ("account email", "@test.invalid")
        ];
      Alcotest.(check bool)
        "the community rows carry no installation login" false
        (contains ~needle:"pfin-owner" durable_blob);
      Lwt.return_unit)

let suite =
  [ pure_inputs_case; public_case; unlisted_case; same_slug_case;
    authority_case; unauthorized_case; verification_case;
    shell_corruption_case; lifecycle_corruption_case; slug_conflict_case;
    rollback_case; same_draft_race_case; same_slug_race_case;
    versus_removal_case; authority_revocation_race_case;
    versus_provisioning_race_case; privacy_case ]

let suites =
    (* Atomic network-community publication store: pure validation before
       SQL, candidate resolution and the shared project-first lock order,
       durable top_mod/admin authority and its collapse, complete-draft
       and provisioned-relation validation, the guarded identity+lifecycle
       update with savepoint-fenced slug-conflict classification, complete
       post-update validation, rollback injection, deterministic
       concurrency, and the privacy sweep. Database-gated. *)
  [ ("network_community_publication_store", suite)
  ]
