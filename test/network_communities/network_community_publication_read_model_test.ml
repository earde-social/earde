module Pi = Earde.Project_identity

(* === Network-community publication read model
       (Network_community_publication_read_model) ===
   Publisher-authorized load of one provisioned network-community setup
   draft. Database-gated (EARDE_TEST_DATABASE_URL) with its own reserved
   external-installation-id range 952000001..952000999 (hence account and
   forge-namespace ids 952100001..952100999, which also scope the permanent-
   project cleanup), ncpr_% usernames, and ncpr-% community slugs, so no
   suite shares fixtures. Every valid draft is created through the real
   provisioning store, never by fixture INSERTs. Pure validation is proven
   pre-SQL against a deliberately disconnected connection. Credential
   assertions are boolean, so no fixture byte reaches test output on
   failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rm = Earde.Network_community_publication_read_model
module Pv = Earde.Project_home_provisioning_store

let error_str : Rm.error -> string = function
  | Rm.Invalid_user_id -> "Invalid_user_id"
  | Rm.Invalid_community_slug -> "Invalid_community_slug"
  | Rm.Inconsistent_data -> "Inconsistent_data"
  | Rm.Storage_error -> "Storage_error"

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let make_project = Home_provisioning_fixture.make_project
let insert_community = Community_fixture.insert_community

(* Distinctive credential-shaped fixtures. None may appear in any public
   value this read model produces. *)
let credential_markers =
  [
    ("access token", "gho_NCPR_ACCESS_TOKEN_SECRET");
    ("refresh token", "ghr_NCPR_REFRESH_TOKEN");
    ("client secret", "NCPR_CLIENT_SECRET_VALUE");
    ("external installation id", "952000001");
    ("external account id", "952100001");
  ]

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 952100001 \
       AND 952100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       952100001 AND 952100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 952000001 AND 952000999)";
      "DELETE FROM communities WHERE slug LIKE 'ncpr-%'";
      "DELETE FROM users WHERE username LIKE 'ncpr_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       952000001 AND 952000999";
    ]

let q_set_visibility =
  (Caqti_type.(t2 int string) ->. Caqti_type.unit)
    "UPDATE communities SET visibility = $2 WHERE id = $1"

let q_set_indexable =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
    "UPDATE communities SET indexable = $2 WHERE id = $1"

let q_set_discoverable =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
    "UPDATE communities SET discoverable = $2 WHERE id = $1"

let q_set_legacy =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET is_network_community = FALSE WHERE id = $1"

let q_delete_members =
  (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM community_members WHERE community_id = $1"

let q_delete_top_mods =
  (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM community_moderators WHERE community_id = $1 AND role = \
     'top_mod'"

let q_duplicate_section =
  (Caqti_type.int ->. Caqti_type.unit)
    "INSERT INTO community_sections (community_id, name, slug, description, \
     position, default_sort, is_introduction_section) VALUES ($1, 'General', \
     'general', 'Duplicate', 1, 'new', FALSE)"

let q_unarchive_channels =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE channels SET is_archived = FALSE WHERE community_id = $1"

let q_archive_channels =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE channels SET is_archived = TRUE WHERE community_id = $1"

let q_delete_relations =
  (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM community_projects WHERE community_id = $1"

(* Audit history deliberately RESTRICT-protects relation rows; a probe
   that drops a store-created relation must purge its trail first. *)
let q_purge_audit =
  (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM project_home_audit_events WHERE community_id = $1"

let q_set_relation_pending =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE community_projects SET status = 'pending', reviewed_at = NULL, \
     reviewed_by_user_id = NULL WHERE community_id = $1"

(* A second accepted home, from a different project: the partial unique
   index is per project, so the community side is where the contradiction
   shows up. Shape matches what the provisioning store writes. *)
let q_second_accepted =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "INSERT INTO community_projects (project_id, community_id, relation_type, \
     status, reviewed_at) VALUES ($1, $2, 'home', 'accepted', NOW())"

let q_set_verification = Home_request_fixture.q_set_verification
let q_corrupt_login = Home_provisioning_fixture.q_corrupt_login

let q_corrupt_community_name =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET name = 'Ncpr' || chr(1) || 'Name' WHERE id = $1"

let q_corrupt_community_description =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE communities SET description = ' padded ' WHERE id = $1"

(* Off-enum verification requires the permanent-project CHECK to step
   aside for the length of one probe; it is restored under
   Lwt.finalize. *)
let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let q_drop_verification_check =
  ddl
    "ALTER TABLE open_source_projects DROP CONSTRAINT IF EXISTS \
     open_source_projects_verification_status_check"

let q_restore_verification_check =
  ddl
    "ALTER TABLE open_source_projects ADD CONSTRAINT \
     open_source_projects_verification_status_check CHECK ( \
     verification_status IN ('verified', 'stale', 'revoked'))"

(* Emptying search_path hides the unqualified tables, so the query fails
   at the SQL layer and Caqti returns an Error the read model maps to
   Storage_error — a genuine query failure, not a torn-down connection
   (which the driver signals by raising, not by Error). *)
let q_break_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO ''"

let q_reset_search_path =
  (Caqti_type.unit ->. Caqti_type.unit) "SET search_path TO public"

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

(* db_case with the scoped lifecycle CHECK (migration 20260726130000)
   dropped for the whole case: these fixtures deliberately write drift
   shapes the constraint now forbids at the database, and the defensive
   branches they exercise stay covered. The suite cleanup removes every
   fixture row before the constraint returns, validated. *)
let db_case_lifecycle_relaxed name f =
  db_case name (fun conn ->
      Network_community_lifecycle_constraint.around conn
        ~cleanup:(fun () ->
          Network_community_lifecycle_constraint.run_cleanup conn q_cleanup)
        (fun () -> f conn))

(* The one way a valid draft is built: the real atomic provisioning
   transaction, exactly as the POST route runs it. *)
let provision_draft ?name ?description conn ~actor ~project_slug ~slug =
  let* r =
    Pv.provision conn ~actor_user_id:actor ~project_slug
      ~identity:(Network_community_fixture.identity ?name ?description ~slug ())
  in
  match r with
  | Ok home ->
      Alcotest.(check string) "provisioned slug" slug (Pv.community_slug home);
      find conn "community id" Network_community_fixture.q_community_id slug
  | Error _ -> Alcotest.failf "provisioning fixture failed for %s" slug

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

(* === call helpers === *)

let load conn ~user ~slug =
  Rm.load_for_publisher conn ~user_id:user ~community_slug:slug

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
      Alcotest.failf "%s: expected %s, got Ok None" label (error_str expected)
  | Ok (Some _) ->
      Alcotest.failf "%s: expected %s, got Ok Some" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* === pure input validation === *)

let pure_inputs_case =
  db_case "publication read: invalid inputs rejected before any SQL"
    (fun _conn ->
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
      let* () = expect "user 0" Rm.Invalid_user_id ~user:0 ~slug:"ncpr-a" in
      let* () = expect "user -1" Rm.Invalid_user_id ~user:(-1) ~slug:"ncpr-a" in
      (* The user id is checked first: a doubly invalid call never reaches
         the slug rule. *)
      let* () =
        expect "user before slug" Rm.Invalid_user_id ~user:0 ~slug:"bad slug"
      in
      Lwt_list.iter_s
        (fun slug ->
          expect
            ("slug " ^ String.escaped slug)
            Rm.Invalid_community_slug ~user:1 ~slug)
        [
          "";
          " ncpr-a";
          "ncpr-a ";
          "ncpr a";
          "ncpr/a";
          "ncpr\ta";
          "ncpr\na";
          "ncpr\x00";
          "ncpr\x7f";
        ])

(* === authorization === *)

let authorization_case =
  db_case
    "publication read: only a current top moderator or a durable global admin \
     sees the draft; every other identity is the same absence" (fun conn ->
      let* owner = insert_user conn "ncpr_owner" in
      let* second = insert_user conn "ncpr_second" in
      let* member = insert_user conn "ncpr_member" in
      let* moddy = insert_user conn "ncpr_mod" in
      let* legacy = insert_user conn "ncpr_legacymod" in
      let* elsewhere = insert_user conn "ncpr_elsewhere" in
      let* stranger = insert_user conn "ncpr_stranger" in
      let* admin = insert_user conn "ncpr_admin" in
      let* () = set_admin conn ~user:admin true in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000001L ~slug:"ncpr-auth"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-auth"
          ~slug:"ncpr-auth-home"
      in
      (* The creating steward is the initial top moderator. *)
      let* view = load_view "creator" conn ~user:owner ~slug:"ncpr-auth-home" in
      Alcotest.(check bool)
        "creator is top mod" true
        (Rm.publisher_is_top_moderator view);
      Alcotest.(check bool)
        "creator is not a durable admin" false
        (Rm.publisher_is_durable_admin view);
      (* A second current top moderator is equally authorized. *)
      let* () = add_role conn ~user:second ~community "top_mod" in
      let* second_view =
        load_view "second top mod" conn ~user:second ~slug:"ncpr-auth-home"
      in
      Alcotest.(check bool)
        "second is top mod" true
        (Rm.publisher_is_top_moderator second_view);
      (* A durable global administrator is authorized without any local
         role, and says so. *)
      let* admin_view =
        load_view "durable admin" conn ~user:admin ~slug:"ncpr-auth-home"
      in
      Alcotest.(check bool)
        "admin is durable admin" true
        (Rm.publisher_is_durable_admin admin_view);
      Alcotest.(check bool)
        "admin holds no local role" false
        (Rm.publisher_is_top_moderator admin_view);
      (* Everyone else collapses to the same absence. A session-shaped
         admin claim is invisible here: only users.is_admin counts, and
         this user has no durable row. *)
      let* () = add_member conn ~user:member ~community in
      let* () = add_role conn ~user:moddy ~community "mod" in
      let* () = add_role conn ~user:legacy ~community "legacy_mod" in
      let* other = insert_community conn "ncpr-other" in
      let* () = add_role conn ~user:elsewhere ~community:other "top_mod" in
      let* () =
        Lwt_list.iter_s
          (fun (label, user) ->
            load_none label conn ~user ~slug:"ncpr-auth-home")
          [
            ("ordinary member", member);
            ("mod", moddy);
            ("legacy_mod", legacy);
            ("moderator of another community", elsewhere);
            ("stranger", stranger);
            ("session-shaped admin", stranger);
          ]
      in
      (* A removed top moderator loses access with no distinguishable
         answer, and a downgraded one likewise. *)
      let* () =
        exec conn "downgrade" Community_fixture.q_set_moderator_role
          (second, community, "mod")
      in
      let* () =
        load_none "downgraded top mod" conn ~user:second ~slug:"ncpr-auth-home"
      in
      let* () =
        exec conn "remove" Community_fixture.q_remove_moderator
          (second, community)
      in
      let* () =
        load_none "removed top mod" conn ~user:second ~slug:"ncpr-auth-home"
      in
      (* A revoked durable admin flag revokes access too. *)
      let* () = set_admin conn ~user:admin false in
      load_none "revoked durable admin" conn ~user:admin ~slug:"ncpr-auth-home")

(* === availability and lifecycle === *)

let lifecycle_case =
  db_case_lifecycle_relaxed
    "publication read: only an exact private network setup draft is loadable; \
     every other lifecycle is the same absence" (fun conn ->
      let* owner = insert_user conn "ncpr_lifeowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000002L ~slug:"ncpr-life"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-life"
          ~slug:"ncpr-life-home"
      in
      let* _ = load_view "draft" conn ~user:owner ~slug:"ncpr-life-home" in
      (* A nonexistent slug is the same absence. *)
      let* () = load_none "missing" conn ~user:owner ~slug:"ncpr-nothing" in
      (* A legacy community is never loadable, even for its own top
         moderator. *)
      let* legacy_community =
        insert_community ~network:false conn "ncpr-legacy"
      in
      let* () =
        add_role conn ~user:owner ~community:legacy_community "top_mod"
      in
      let* () =
        load_none "legacy community" conn ~user:owner ~slug:"ncpr-legacy"
      in
      (* Each individual lifecycle drift removes the draft from view; the
         row is restored between probes so the drifts are independent. *)
      let restore () =
        let* () =
          exec conn "restore visibility" q_set_visibility (community, "private")
        in
        let* () =
          exec conn "restore indexable" q_set_indexable (community, false)
        in
        let* () =
          exec conn "restore discoverable" q_set_discoverable (community, false)
        in
        Lwt.return_unit
      in
      let* () = exec conn "public" q_set_visibility (community, "public") in
      let* () =
        load_none "public draft" conn ~user:owner ~slug:"ncpr-life-home"
      in
      let* () = restore () in
      let* () = exec conn "indexable" q_set_indexable (community, true) in
      let* () =
        load_none "indexable draft" conn ~user:owner ~slug:"ncpr-life-home"
      in
      let* () = restore () in
      let* () = exec conn "discoverable" q_set_discoverable (community, true) in
      let* () =
        load_none "discoverable draft" conn ~user:owner ~slug:"ncpr-life-home"
      in
      let* () = restore () in
      let* _ =
        load_view "restored draft" conn ~user:owner ~slug:"ncpr-life-home"
      in
      (* Publication ends the surface for good. *)
      let* () =
        exec conn "publish" Network_community_fixture.q_publish community
      in
      let* () =
        load_none "published network community" conn ~user:owner
          ~slug:"ncpr-life-home"
      in
      (* Dropping the network marker is equally final. *)
      let* () =
        exec conn "publish undo" q_set_visibility (community, "private")
      in
      let* () = exec conn "unindex" q_set_indexable (community, false) in
      let* () = exec conn "undiscover" q_set_discoverable (community, false) in
      let* () = exec conn "legacy" q_set_legacy community in
      load_none "network marker dropped" conn ~user:owner ~slug:"ncpr-life-home")

(* === the connected project === *)

let project_case =
  db_case
    "publication read: exactly one accepted home project is exposed by \
     identity, and verification drift does not gate the view" (fun conn ->
      let* owner = insert_user conn "ncpr_projowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:952000003L ~slug:"ncpr-proj"
          ~name:"Ncpr Proj" ~description:"Project body."
      in
      let* _community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-proj"
          ~slug:"ncpr-proj-home"
      in
      let* view = load_view "project" conn ~user:owner ~slug:"ncpr-proj-home" in
      let loaded = Rm.project view in
      Alcotest.(check string)
        "project name" "Ncpr Proj" (Rm.project_name loaded);
      Alcotest.(check string)
        "project slug" "ncpr-proj" (Rm.project_slug loaded);
      Alcotest.(check string)
        "namespace login" "pfin-owner"
        (Rm.project_namespace_login loaded);
      Alcotest.(check string)
        "kind" "project"
        (Pi.string_of_kind (Rm.project_kind loaded));
      (* The community identity is the draft's own, byte-exact. *)
      let community = Rm.community view in
      Alcotest.(check string)
        "community name" "Ncpr Home"
        (Rm.community_name community);
      Alcotest.(check string)
        "community slug" "ncpr-proj-home"
        (Rm.community_slug community);
      Alcotest.(check (option string))
        "community description" None
        (Rm.community_description community);
      (* Stale and revoked verification are validated as known values and
         deliberately do not withdraw the view: the project was verified
         when the home was established, and a private draft must not be
         stranded unpublishable by later token drift. *)
      let* () = exec conn "stale" q_set_verification (project, "stale") in
      let* _ =
        load_view "stale project" conn ~user:owner ~slug:"ncpr-proj-home"
      in
      let* () = exec conn "revoked" q_set_verification (project, "revoked") in
      let* _ =
        load_view "revoked project" conn ~user:owner ~slug:"ncpr-proj-home"
      in
      let* () = exec conn "verified" q_set_verification (project, "verified") in
      let* _ =
        load_view "verified again" conn ~user:owner ~slug:"ncpr-proj-home"
      in
      Lwt.return_unit)

let relation_corruption_case =
  db_case
    "publication read: a missing, pending, duplicated, or malformed accepted \
     home relation is inconsistent durable state" (fun conn ->
      let* owner = insert_user conn "ncpr_relowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:952000004L ~slug:"ncpr-rel"
      in
      let* _inst2, second_project =
        make_project conn ~user:owner ~ext_id:952000005L ~slug:"ncpr-rel-b"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-rel"
          ~slug:"ncpr-rel-home"
      in
      let expect label e =
        load_expect label e conn ~user:owner ~slug:"ncpr-rel-home"
      in
      let* _ = load_view "one home" conn ~user:owner ~slug:"ncpr-rel-home" in
      (* A second accepted home from a different project: the partial
         unique index is per project, so the contradiction shows up on the
         community side, and the view must refuse rather than pick one. *)
      let* () =
        exec conn "second accepted" q_second_accepted (second_project, community)
      in
      let* () = expect "two accepted homes" Rm.Inconsistent_data in
      (* No accepted home at all is equally inconsistent — a provisioned
         draft always has exactly one. *)
      let* () = exec conn "purge audit" q_purge_audit community in
      let* () = exec conn "clear relations" q_delete_relations community in
      let* () = expect "no accepted home" Rm.Inconsistent_data in
      (* Reinstating exactly one restores the view. *)
      let* () = exec conn "reinstate" q_second_accepted (project, community) in
      let* _ =
        load_view "one home again" conn ~user:owner ~slug:"ncpr-rel-home"
      in
      Lwt.return_unit)

let pending_relation_case =
  db_case
    "publication read: a home relation demoted to pending leaves the draft \
     inconsistent, never silently homeless" (fun conn ->
      let* owner = insert_user conn "ncpr_pendowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000006L ~slug:"ncpr-pend"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-pend"
          ~slug:"ncpr-pend-home"
      in
      let* _ = load_view "accepted" conn ~user:owner ~slug:"ncpr-pend-home" in
      let* () = exec conn "demote" q_set_relation_pending community in
      let* () =
        load_expect "pending only" Rm.Inconsistent_data conn ~user:owner
          ~slug:"ncpr-pend-home"
      in
      (* Removing the relation entirely is the same inconsistency, never a
         partial view of a homeless draft. *)
      let* () = exec conn "purge audit" q_purge_audit community in
      let* () = exec conn "clear" q_delete_relations community in
      load_expect "relation gone" Rm.Inconsistent_data conn ~user:owner
        ~slug:"ncpr-pend-home")

(* === complete-draft and identity corruption === *)

let shell_corruption_case =
  db_case
    "publication read: an incomplete community shell is inconsistent durable \
     state" (fun conn ->
      let* owner = insert_user conn "ncpr_shellowner" in
      let* admin = insert_user conn "ncpr_shelladmin" in
      let* () = set_admin conn ~user:admin true in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000007L ~slug:"ncpr-shell"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-shell"
          ~slug:"ncpr-shell-home"
      in
      let expect label e =
        load_expect label e conn ~user:admin ~slug:"ncpr-shell-home"
      in
      (* Membership is required in its own right. *)
      let* () = exec conn "drop members" q_delete_members community in
      let* () = expect "no member" Rm.Inconsistent_data in
      let* () = add_member conn ~user:owner ~community in
      let* _ =
        load_view "member restored" conn ~user:admin ~slug:"ncpr-shell-home"
      in
      (* A draft with no top moderator is corrupt even for a durable
         admin, who can still reach it. *)
      let* () = exec conn "drop top mods" q_delete_top_mods community in
      let* () = expect "no top mod" Rm.Inconsistent_data in
      let* () = add_role conn ~user:owner ~community "top_mod" in
      let* _ =
        load_view "top mod restored" conn ~user:admin ~slug:"ncpr-shell-home"
      in
      (* The required general channel must exist and be active. *)
      let* () = exec conn "archive channels" q_archive_channels community in
      let* () = expect "archived general channel" Rm.Inconsistent_data in
      let* () = exec conn "unarchive channels" q_unarchive_channels community in
      let* _ =
        load_view "channel restored" conn ~user:admin ~slug:"ncpr-shell-home"
      in
      (* The required General section. Duplication is not reachable through
         the durable model — community_sections and channels each carry a
         unique (community_id, slug) key — so the "exactly one" rule can
         only be violated downward. The attempt is asserted to be rejected
         rather than quietly skipped, and absence is then exercised. *)
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* duplicate = C.exec q_duplicate_section community in
      (match duplicate with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "a duplicate General section was accepted");
      let* () =
        exec conn "drop sections" Network_community_fixture.q_delete_sections
          community
      in
      expect "no General section" Rm.Inconsistent_data)

let identity_corruption_case =
  db_case
    "publication read: a community identity outside the scoped network policy \
     is inconsistent durable state" (fun conn ->
      let* owner = insert_user conn "ncpr_idowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000008L ~slug:"ncpr-id"
      in
      let* community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-id"
          ~slug:"ncpr-id-home"
      in
      let* _ = load_view "canonical" conn ~user:owner ~slug:"ncpr-id-home" in
      (* The scoped CHECKs make these states unreachable through normal
         writes, so they are produced with the constraints briefly stood
         down and canonicalized again before the constraints return. *)
      let* () = Network_community_constraints.drop conn in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "control byte in name" q_corrupt_community_name community
          in
          let* () =
            load_expect "control-bearing name" Rm.Inconsistent_data conn
              ~user:owner ~slug:"ncpr-id-home"
          in
          let* () =
            exec conn "recanonicalize"
              Network_community_constraints.q_recanonicalize
              (community, "ncpr-id-home", "Ncpr Home")
          in
          let* _ =
            load_view "name restored" conn ~user:owner ~slug:"ncpr-id-home"
          in
          let* () =
            exec conn "padded description" q_corrupt_community_description
              community
          in
          let* () =
            load_expect "untrimmed description" Rm.Inconsistent_data conn
              ~user:owner ~slug:"ncpr-id-home"
          in
          Lwt.return_unit)
        (fun () ->
          let* () =
            exec conn "recanonicalize"
              Network_community_constraints.q_recanonicalize
              (community, "ncpr-id-home", "Ncpr Home")
          in
          Network_community_constraints.restore conn))

let project_corruption_case =
  db_case
    "publication read: a malformed or off-enum project identity is \
     inconsistent durable state" (fun conn ->
      let* owner = insert_user conn "ncpr_pcowner" in
      let* _inst, project =
        make_project conn ~user:owner ~ext_id:952000009L ~slug:"ncpr-pc"
      in
      let* _community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-pc"
          ~slug:"ncpr-pc-home"
      in
      let* _ = load_view "sound" conn ~user:owner ~slug:"ncpr-pc-home" in
      (* A namespace login with an embedded space still satisfies the
         permanent CHECK (btrim-equal, non-empty) but is not addressable. *)
      let* () = exec conn "corrupt login" q_corrupt_login project in
      let* () =
        load_expect "non-addressable login" Rm.Inconsistent_data conn
          ~user:owner ~slug:"ncpr-pc-home"
      in
      let* () =
        exec conn "restore login" Home_provisioning_fixture.q_restore_project
          project
      in
      let* _ =
        load_view "login restored" conn ~user:owner ~slug:"ncpr-pc-home"
      in
      (* An off-enum verification status needs the permanent CHECK to step
         aside for the length of one probe. *)
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* r = C.exec q_drop_verification_check () in
      let* () = or_fail "drop verification check" r in
      Lwt.finalize
        (fun () ->
          let* () =
            exec conn "off-enum verification" q_set_verification
              (project, "pending_reverification")
          in
          load_expect "unknown verification" Rm.Inconsistent_data conn
            ~user:owner ~slug:"ncpr-pc-home")
        (fun () ->
          let* () =
            exec conn "restore verification" q_set_verification
              (project, "verified")
          in
          let* r = C.exec q_restore_verification_check () in
          let* () = or_fail "restore verification check" r in
          Lwt.return_unit))

(* === storage failure === *)

let storage_case =
  db_case "publication read: a query failure is the payload-free storage error"
    (fun conn ->
      let* owner = insert_user conn "ncpr_stowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000010L ~slug:"ncpr-st"
      in
      let* _community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-st"
          ~slug:"ncpr-st-home"
      in
      let* _ = load_view "before" conn ~user:owner ~slug:"ncpr-st-home" in
      let* () = exec conn "break search_path" q_break_search_path () in
      Lwt.finalize
        (fun () ->
          load_expect "hidden tables" Rm.Storage_error conn ~user:owner
            ~slug:"ncpr-st-home")
        (fun () -> exec conn "reset search_path" q_reset_search_path ()))

(* === privacy sweep === *)

let privacy_case =
  db_case
    "publication read: no identifier, credential, or private workflow value \
     crosses the public surface" (fun conn ->
      let* owner = insert_user conn "ncpr_privowner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:952000001L ~slug:"ncpr-priv"
          ~name:"Ncpr Priv" ~description:"Priv body."
      in
      let* _community =
        provision_draft conn ~actor:owner ~project_slug:"ncpr-priv"
          ~slug:"ncpr-priv-home" ~name:"Ncpr Priv Home"
          ~description:"Home body."
      in
      let* view = load_view "priv" conn ~user:owner ~slug:"ncpr-priv-home" in
      let community = Rm.community view in
      let project = Rm.project view in
      (* Every public value of the view, concatenated: nothing else is
         reachable through the abstract types. *)
      let surface =
        String.concat "|"
          [
            Rm.community_name community;
            Rm.community_slug community;
            Option.value ~default:"" (Rm.community_description community);
            Rm.project_name project;
            Rm.project_slug project;
            Rm.project_namespace_login project;
            Pi.string_of_kind (Rm.project_kind project);
            string_of_bool (Rm.publisher_is_top_moderator view);
            string_of_bool (Rm.publisher_is_durable_admin view);
          ]
      in
      List.iter
        (fun (what, needle) ->
          Alcotest.(check bool)
            ("view free of " ^ what) false
            (Html_assert.contains surface needle))
        credential_markers;
      (* No internal identifier can appear either: the actor's own id, the
         community id, and the project id are all absent by construction. *)
      Alcotest.(check bool)
        "no actor id" false
        (Html_assert.contains surface (string_of_int owner));
      Lwt.return_unit)

let suite =
  [
    pure_inputs_case;
    authorization_case;
    lifecycle_case;
    project_case;
    relation_corruption_case;
    pending_relation_case;
    shell_corruption_case;
    identity_corruption_case;
    project_corruption_case;
    storage_case;
    privacy_case;
  ]

let suites =
  (* Publisher-authorized setup read model: pure input validation before
       SQL, top-mod-or-durable-admin authorization with no session bypass,
       the exact private-draft lifecycle, the single accepted home project
       and its verification-drift tolerance, complete-draft and identity
       revalidation, storage failure, and the privacy sweep.
       Database-gated. *)
  [ ("network_community_publication_read_model", suite) ]
