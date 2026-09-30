(* Dedicated community homes: provisioning identities, projects ready to
   provision, and the queries that pin provisioned state. *)

module Phr = Earde.Project_home_relation
module Phvf = Earde.Project_home_provisioning_form

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store
module Fin = Earde.Project_finalization_store

let exec = Db_fixture.exec
let q_insert_moderator = Community_fixture.q_insert_moderator

module Pv = Earde.Project_home_provisioning_store

let or_fail = Db_fixture.or_fail
let find = Db_fixture.find
let collect = Db_fixture.collect

let phvf_err : Phvf.error -> string = function
  | Phvf.Invalid_form -> "Invalid_form"
  | Phvf.Invalid_community_name -> "Invalid_community_name"
  | Phvf.Invalid_community_slug -> "Invalid_community_slug"
  | Phvf.Invalid_community_description -> "Invalid_community_description"

let phvf_fields ?(name = "Phvf Community") ?(slug = "phvf-community")
    ?(description = "") () =
  [
    ("community_name", name);
    ("community_slug", slug);
    ("community_description", description);
  ]

let phvf_ok label fields =
  match Phvf.of_fields fields with
  | Ok parsed -> parsed
  | Error e -> Alcotest.failf "%s: rejected with %s" label (phvf_err e)

(* A two-byte scalar, so length rules are proven to count scalars (as
   PostgreSQL char_length does) rather than bytes. *)
let phvf_scalar = "\xc3\xa8"
let phvf_repeat s n = String.concat "" (List.init n (fun _ -> s))

(* Durable corruption the read model must refuse to build a suggestion
   from. Each value still satisfies every production CHECK — btrim() only
   trims spaces, and none exceeds a length limit — so nothing here weakens
   a constraint to manufacture the failure. *)
let q_corrupt_name_control =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = 'Phvr' || chr(1) || 'Name' WHERE \
     id = $1"

let q_corrupt_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET forge_namespace_login = 'phvr owner' \
     WHERE id = $1"

let q_restore_project =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET name = 'Phvr Project', description = \
     NULL, forge_namespace_login = 'pfin-owner' WHERE id = $1"

let q_insert_steward = Home_request_fixture.q_insert_steward

(* Verified permanent projects come only through the real chain — draft
   store, selection store, finalization store — never fixture INSERTs.
   Returns the installation record id (for extra-steward fixtures) and the
   permanent project id. *)
let make_project ?(name = "Phvr Project") ?description conn ~user ~ext_id ~slug
    =
  let repo_id = Int64.add ext_id 400000L in
  let* inst, draft, _, _ =
    Project_fixture.make_draft conn ~user ~ext_id (fun account_id ->
        [ Project_fixture.repo ~account_id ~id:repo_id "alpha" ])
  in
  let* ids = Project_fixture.snapshot_ids conn draft in
  let s1 = List.nth ids 0 in
  let* () =
    Project_fixture.replace_ok "seed selection" conn ~user ~draft ~primary:s1
      [ s1 ]
  in
  let identity =
    Project_fixture.identity_exn ~name ~slug ?description ~selected:[ s1 ]
      ~primary:s1 ()
  in
  let* created =
    Project_fixture.finalize_ok "fixture project" conn ~user ~draft identity
  in
  Lwt.return (inst, Fin.project_id created)

let add_steward conn ~project ~user ~installation =
  exec conn "extra steward" q_insert_steward (project, user, installation)

let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" q_insert_moderator (user, community, "top_mod")

let request_pending label conn ~user ~slug ~community =
  let relation =
    Home_request_fixture.phr_expect_ok (Phr.create_pending ~request_note:None)
  in
  let* r =
    Rq.create conn ~user_id:user ~project_slug:slug
      ~target_community_id:community ~relation
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: request fixture failed" label

let review label conn ~reviewer ~slug ~community_slug decision =
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community_slug ~decision
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: review fixture failed" label

let error_str : Pv.error -> string = function
  | Pv.Invalid_user_id -> "Invalid_user_id"
  | Pv.Invalid_project_slug -> "Invalid_project_slug"
  | Pv.Project_unavailable -> "Project_unavailable"
  | Pv.Community_slug_unavailable -> "Community_slug_unavailable"
  | Pv.Active_home_exists -> "Active_home_exists"
  | Pv.Inconsistent_data -> "Inconsistent_data"
  | Pv.Storage_error -> "Storage_error"

let q_count_by_slug =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*) FROM communities WHERE slug = $1"

(* Everything durable on the community row except the slug key, as one
   signature: identity bytes, the private-draft lifecycle, the network
   marker, both publication flags, and the structured-shell flag. *)
let q_community_state =
  (Caqti_type.string ->? Caqti_type.(t2 int string))
    "SELECT id, name || '|' || COALESCE(description, '<null>') || '|' || \
     visibility || '|' || onboarding_state || '|' || \
     is_network_community::text || '|' || indexable::text || '|' || \
     discoverable::text || '|' || sections_enabled::text FROM communities \
     WHERE slug = $1"

let q_member_present =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "SELECT COUNT(*) FROM community_members WHERE community_id = $1 AND \
     user_id = $2"

let q_moderator_role =
  (Caqti_type.(t2 int int) ->? Caqti_type.string)
    "SELECT role FROM community_moderators WHERE community_id = $1 AND user_id \
     = $2"

let q_section_sigs =
  (Caqti_type.int ->* Caqti_type.string)
    "SELECT slug || '|' || name || '|' || COALESCE(description, '<null>') || \
     '|' || position::text || '|' || default_sort || '|' || \
     is_introduction_section::text || '|' || indexable::text FROM \
     community_sections WHERE community_id = $1 ORDER BY position, slug"

let q_channel_sigs =
  (Caqti_type.int ->* Caqti_type.string)
    "SELECT slug || '|' || name || '|' || COALESCE(topic, '<null>') || '|' || \
     position::text || '|' || is_archived::text || '|' || indexable::text FROM \
     channels WHERE community_id = $1 ORDER BY position, slug"

let q_relation_ids =
  (Caqti_type.int64 ->* Caqti_type.int64)
    "SELECT id FROM community_projects WHERE project_id = $1 ORDER BY id"

let q_relation_times =
  (Caqti_type.int64 ->! Caqti_type.(t2 bool bool))
    "SELECT reviewed_at >= created_at, updated_at >= created_at FROM \
     community_projects WHERE id = $1"

(* Identities come only through the real form parser, exactly as the
   future POST handler will hand them to the store. *)
let identity ?(name = "Phvs Community Home") ?(slug = "phvs-home")
    ?(description = "") () =
  phvf_ok "identity fixture" (phvf_fields ~name ~slug ~description ())

let provision conn ~actor ~slug identity =
  Pv.provision conn ~actor_user_id:actor ~project_slug:slug ~identity

(* Every success asserts both public accessors — the only observable
   surface of the abstract result. *)
let provision_ok label conn ~actor ~slug ~expect_slug identity =
  let* r = provision conn ~actor ~slug identity in
  match r with
  | Ok home ->
      Alcotest.(check string)
        (label ^ ": community slug")
        expect_slug (Pv.community_slug home);
      Alcotest.(check string)
        (label ^ ": resulting status")
        (Phr.string_of_status Phr.Accepted)
        (Phr.string_of_status (Pv.resulting_status home));
      Lwt.return home
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let provision_expect label expected conn ~actor ~slug identity =
  let* r = provision conn ~actor ~slug identity in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let community_state label conn slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find_opt q_community_state slug in
  let* row = or_fail label r in
  match row with
  | Some state -> Lwt.return state
  | None -> Alcotest.failf "%s: community %s missing" label slug

let check_no_state label conn ~project ~slug =
  let* n = find conn "loser community" q_count_by_slug slug in
  Alcotest.(check int) (label ^ ": no community") 0 n;
  let* relations =
    find conn "relations" Home_request_fixture.q_count_for_project project
  in
  Alcotest.(check int) (label ^ ": no relation") 0 relations;
  Lwt.return_unit

(* The complete durable draft one successful provision must leave. *)
let check_provisioned_draft label conn ~actor ~project ~slug ~name ~description
    =
  let* cid, state = community_state (label ^ ": community") conn slug in
  Alcotest.(check string)
    (label ^ ": community identity and lifecycle")
    (name ^ "|" ^ description ^ "|private|draft|true|false|false|true")
    state;
  let* members = find conn "members" Home_request_fixture.q_count_members cid in
  Alcotest.(check int) (label ^ ": exactly one member") 1 members;
  let* mine = find conn "actor member" q_member_present (cid, actor) in
  Alcotest.(check int) (label ^ ": the member is the actor") 1 mine;
  let* mods =
    find conn "moderators" Home_request_fixture.q_count_moderators cid
  in
  Alcotest.(check int) (label ^ ": exactly one moderator") 1 mods;
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* role = C.find_opt q_moderator_role (cid, actor) in
  let* role = or_fail (label ^ ": role") role in
  Alcotest.(check (option string))
    (label ^ ": the actor is top_mod")
    (Some "top_mod") role;
  let* sections = collect conn "sections" q_section_sigs cid in
  Alcotest.(check (list string))
    (label ^ ": exactly the default General section")
    [ "general|General|General discussion|0|new|false|true" ]
    sections;
  let* channels = collect conn "channels" q_channel_sigs cid in
  Alcotest.(check (list string))
    (label ^ ": exactly the default general channel")
    [ "general|general|General chat|0|false|true" ]
    channels;
  let* relation_ids = collect conn "relation ids" q_relation_ids project in
  match relation_ids with
  | [ rid ] ->
      let* ((rp, rc), (rtype, rstatus)), ((req, rev), (note, times)) =
        Home_request_fixture.relation_row conn rid
      in
      Alcotest.(check int64) (label ^ ": relation project") project rp;
      Alcotest.(check int) (label ^ ": relation community") cid rc;
      Alcotest.(check string) (label ^ ": relation type") "home" rtype;
      Alcotest.(check string) (label ^ ": accepted") "accepted" rstatus;
      Alcotest.(check (option int))
        (label ^ ": no fabricated requester")
        None req;
      Alcotest.(check (option int))
        (label ^ ": no fabricated reviewer")
        None rev;
      Alcotest.(check (option string)) (label ^ ": no note") None note;
      let reviewed_present, removed_present, updated_ok = times in
      Alcotest.(check bool)
        (label ^ ": reviewed_at present")
        true reviewed_present;
      Alcotest.(check bool)
        (label ^ ": removed_at absent")
        false removed_present;
      Alcotest.(check bool) (label ^ ": updated coherent") true updated_ok;
      let* reviewed_ok, updated_ok = find conn "times" q_relation_times rid in
      Alcotest.(check bool)
        (label ^ ": reviewed_at >= created_at")
        true reviewed_ok;
      Alcotest.(check bool)
        (label ^ ": updated_at >= created_at")
        true updated_ok;
      Lwt.return (cid, rid)
  | ids ->
      Alcotest.failf "%s: expected one relation, found %d" label
        (List.length ids)
