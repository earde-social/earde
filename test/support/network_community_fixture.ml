(* Network-community drafts and publication, through the real stores. *)

module Ncpf = Earde.Network_community_publication_form
let ( let* ) = Lwt.bind
open Caqti_request.Infix

module St = Earde.Network_community_publication_store
module Pv = Earde.Project_home_provisioning_store
let find = Db_fixture.find
let collect = Db_fixture.collect
let make_project = Home_provisioning_fixture.make_project

let ncpf_err : Ncpf.error -> string = function
  | Ncpf.Invalid_form -> "Invalid_form"
  | Ncpf.Invalid_community_name -> "Invalid_community_name"
  | Ncpf.Invalid_community_slug -> "Invalid_community_slug"
  | Ncpf.Invalid_community_description -> "Invalid_community_description"
  | Ncpf.Invalid_publication_visibility -> "Invalid_publication_visibility"

let ncpf_vis : Ncpf.publication_visibility -> string = function
  | Ncpf.Public -> "public"
  | Ncpf.Unlisted -> "unlisted"

let ncpf_fields ?(name = "Ncpf Community") ?(slug = "ncpf-community")
    ?(description = "") ?(visibility = "public") () =
  [ ("community_name", name);
    ("community_slug", slug);
    ("community_description", description);
    ("publication_visibility", visibility)
  ]

let ncpf_ok label fields =
  match Ncpf.of_fields fields with
  | Ok parsed -> parsed
  | Error e -> Alcotest.failf "%s: rejected with %s" label (ncpf_err e)

let q_publish =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET onboarding_state = 'published', \
   visibility = 'public', indexable = TRUE, discoverable = TRUE \
   WHERE id = $1"

let identity ?(name = "Ncpr Home") ?(description = "") ~slug () =
  Home_provisioning_fixture.phvf_ok "identity fixture" (Home_provisioning_fixture.phvf_fields ~name ~slug ~description ())

let error_str : St.error -> string = function
  | St.Invalid_user_id -> "Invalid_user_id"
  | St.Invalid_community_slug -> "Invalid_community_slug"
  | St.Draft_unavailable -> "Draft_unavailable"
  | St.Community_slug_unavailable -> "Community_slug_unavailable"
  | St.Inconsistent_data -> "Inconsistent_data"
  | St.Storage_error -> "Storage_error"

let q_community_id =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT id FROM communities WHERE slug = $1"

let q_count_by_slug = Home_provisioning_fixture.q_count_by_slug

(* Everything durable on the community row, keyed by id so it survives a
   slug change, as one byte-comparable signature. *)
let q_community_sig =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT slug || '|' || name || '|' || \
          COALESCE(description, '<null>') || '|' || \
          COALESCE(rules, '<null>') || '|' || \
          COALESCE(avatar_url, '<null>') || '|' || \
          COALESCE(banner_url, '<null>') || '|' || \
          allow_downvotes::text || '|' || sections_enabled::text || '|' || \
          visibility || '|' || onboarding_state || '|' || \
          is_network_community::text || '|' || indexable::text || '|' || \
          discoverable::text \
   FROM communities WHERE id = $1"

(* The lifecycle-and-identity half only, for exact published-state
   assertions. *)
let q_community_state =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT name || '|' || COALESCE(description, '<null>') || '|' || \
          visibility || '|' || onboarding_state || '|' || \
          is_network_community::text || '|' || indexable::text || '|' || \
          discoverable::text \
   FROM communities WHERE id = $1"

(* Every durable column of one relation row, exact timestamps included,
   for byte-unchanged assertions. *)
let q_relation_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT id::text || '|' || project_id::text || '|' || \
          community_id::text || '|' || relation_type || '|' || status \
          || '|' || COALESCE(requested_by_user_id::text, '<null>') \
          || '|' || COALESCE(reviewed_by_user_id::text, '<null>') \
          || '|' || COALESCE(request_note, '<null>') \
          || '|' || created_at::text || '|' || updated_at::text \
          || '|' || COALESCE(reviewed_at::text, '<null>') \
          || '|' || COALESCE(removed_at::text, '<null>') \
   FROM community_projects WHERE id = $1"

(* Every durable column of the permanent project. *)
let q_project_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT id::text || '|' || \
          COALESCE(source_onboarding_draft_id::text, '<null>') || '|' || \
          name || '|' || slug || '|' || \
          COALESCE(description, '<null>') || '|' || \
          COALESCE(website_url, '<null>') || '|' || \
          kind || '|' || forge || '|' || forge_namespace_id::text || '|' || \
          forge_namespace_login || '|' || forge_namespace_type || '|' || \
          verification_status || '|' || \
          COALESCE(created_by_user_id::text, '<null>') || '|' || \
          created_at::text || '|' || updated_at::text \
   FROM open_source_projects WHERE id = $1"

(* Community-side corruption and restoration fixtures. *)
let q_delete_sections =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM community_sections WHERE community_id = $1"

(* One complete private draft, only through the real chain: permanent
   project via draft/selection/finalization, then the real atomic
   provisioning transaction. Returns the project, community, and
   relation ids. *)
let make_draft ?(name = "Ncps Community Home") ?(description = "") conn
    ~user ~ext_id ~project_slug ~slug =
  let* _inst, project =
    make_project conn ~user ~ext_id ~slug:project_slug
  in
  let identity =
    Home_provisioning_fixture.phvf_ok "identity fixture" (Home_provisioning_fixture.phvf_fields ~name ~slug ~description ())
  in
  let* r = Pv.provision conn ~actor_user_id:user ~project_slug ~identity in
  let* () =
    match r with
    | Ok home ->
        Alcotest.(check string) "draft fixture slug" slug
          (Pv.community_slug home);
        Lwt.return_unit
    | Error _ -> Alcotest.failf "draft fixture failed for %s" slug
  in
  let* cid = find conn "draft community id" q_community_id slug in
  let* relation_ids = collect conn "relation ids" Home_provisioning_fixture.q_relation_ids
                        project in
  match relation_ids with
  | [ rid ] -> Lwt.return (project, cid, rid)
  | ids ->
      Alcotest.failf "draft fixture: expected one relation, found %d"
        (List.length ids)

(* Publication submissions come only through the real form parser,
   exactly as the future POST handler will hand them to the store. *)
let publication ?(name = "Ncps Community Home") ~slug
    ?(description = "") ?(visibility = "public") () =
  ncpf_ok "publication fixture"
    (ncpf_fields ~name ~slug ~description ~visibility ())

let publish conn ~actor ~current value =
  St.publish conn ~actor_user_id:actor ~current_community_slug:current
    ~publication:value

(* Every success asserts both public accessors — the only observable
   surface of the abstract result. *)
let publish_ok label conn ~actor ~current ~expect_slug ~expect_visibility
    value =
  let* r = publish conn ~actor ~current value in
  match r with
  | Ok published ->
      Alcotest.(check string)
        (label ^ ": final slug")
        expect_slug
        (St.community_slug published);
      Alcotest.(check string)
        (label ^ ": publication visibility")
        (ncpf_vis expect_visibility)
        (ncpf_vis (St.publication_visibility published));
      Lwt.return published
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let publish_expect label expected conn ~actor ~current value =
  let* r = publish conn ~actor ~current value in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* The complete durable state around one draft, byte-comparable, for
   loser/rollback assertions: community row, member/moderator counts,
   shell signatures, the exact relation row, the exact project row. *)
let snapshot label conn ~cid ~project ~rid =
  let* community = find conn (label ^ ": community sig") q_community_sig
                     cid in
  let* members = find conn (label ^ ": members") Home_request_fixture.q_count_members cid
  in
  let* moderators =
    find conn (label ^ ": moderators") Home_request_fixture.q_count_moderators cid
  in
  let* sections = collect conn (label ^ ": sections") Home_provisioning_fixture.q_section_sigs
                    cid in
  let* channels = collect conn (label ^ ": channels") Home_provisioning_fixture.q_channel_sigs
                    cid in
  let* relation = find conn (label ^ ": relation sig") q_relation_sig rid
  in
  let* project_sig = find conn (label ^ ": project sig") q_project_sig
                       project in
  Lwt.return
    ((community, (members, moderators)), (sections, channels),
     (relation, project_sig))

let check_unchanged label conn ~cid ~project ~rid before =
  let* after = snapshot label conn ~cid ~project ~rid in
  let (community_b, counts_b), (sections_b, channels_b),
      (relation_b, project_b) =
    before
  in
  let (community_a, counts_a), (sections_a, channels_a),
      (relation_a, project_a) =
    after
  in
  Alcotest.(check string) (label ^ ": community unchanged") community_b
    community_a;
  Alcotest.(check (pair int int)) (label ^ ": counts unchanged") counts_b
    counts_a;
  Alcotest.(check (list string)) (label ^ ": sections unchanged")
    sections_b sections_a;
  Alcotest.(check (list string)) (label ^ ": channels unchanged")
    channels_b channels_a;
  Alcotest.(check string) (label ^ ": relation unchanged") relation_b
    relation_a;
  Alcotest.(check string) (label ^ ": project unchanged") project_b
    project_a;
  Lwt.return_unit

(* Everything a successful publication must leave: the exact selected
   identity and lifecycle on the same row, no row under the old slug
   (when it changed), and the relation, project, shell, and roles
   untouched relative to the pre-publication snapshot. *)
let check_published label conn ~cid ~project ~rid ~before ~name
    ~description ~slug ~old_slug ~indexable ~discoverable =
  let* state = find conn (label ^ ": state") q_community_state cid in
  Alcotest.(check string)
    (label ^ ": identity and lifecycle")
    (name ^ "|" ^ description ^ "|public|published|true|"
    ^ string_of_bool indexable ^ "|" ^ string_of_bool discoverable)
    state;
  let* n = find conn (label ^ ": final slug count") q_count_by_slug slug in
  Alcotest.(check int) (label ^ ": exactly one community owns the slug") 1
    n;
  let* () =
    if String.equal old_slug slug then Lwt.return_unit
    else
      let* old_n = find conn (label ^ ": old slug") q_count_by_slug
                     old_slug in
      Alcotest.(check int) (label ^ ": old slug released") 0 old_n;
      Lwt.return_unit
  in
  let (_, counts_b), (sections_b, channels_b), (relation_b, project_b) =
    before
  in
  let* after = snapshot label conn ~cid ~project ~rid in
  let (_, counts_a), (sections_a, channels_a), (relation_a, project_a) =
    after
  in
  Alcotest.(check (pair int int))
    (label ^ ": membership and moderation unchanged")
    counts_b counts_a;
  Alcotest.(check (list string)) (label ^ ": sections unchanged")
    sections_b sections_a;
  Alcotest.(check (list string)) (label ^ ": channels unchanged")
    channels_b channels_a;
  Alcotest.(check string) (label ^ ": relation unchanged byte-for-byte")
    relation_b relation_a;
  Alcotest.(check string) (label ^ ": project unchanged") project_b
    project_a;
  Lwt.return_unit

let search_lists label conn ~name ~slug expected =
  let* r = Earde.Community_store.search_communities conn name 50 0 in
  match r with
  | Error e -> Alcotest.failf "%s: search failed: %s" label e
  | Ok rows ->
      Alcotest.(check bool) label expected
        (List.exists
           (fun (c : Earde.Community_types.community) -> String.equal c.slug slug)
           rows);
      Lwt.return_unit
