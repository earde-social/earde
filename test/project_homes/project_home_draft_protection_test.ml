module Ob = Earde.Project_onboarding
module Phr = Earde.Project_home_relation

(* === Unpublished network-draft home protection (classification B) ===
   The rule that keeps a dedicated-community setup draft publishable: while
   a community is exactly the unpublished network draft (network, draft,
   private, neither indexable nor discoverable) and its accepted home
   carries exactly the provisioned shape the provisioning store wrote (NULL
   requester, NULL reviewer, NULL note, reviewed_at present, removed_at
   NULL), no authority may detach that relation — not the project steward,
   not the target top moderator, not a durable global administrator, and not
   an actor holding several of those at once. Without it, either removal
   surface could strand the draft: the publication store requires the
   relation, so a removed home leaves an orphan community carrying
   membership, moderation and shell rows that can never be published.

   The store is the authority; the two surfaces only stop advertising a
   control the store would refuse. This suite therefore covers the durable
   rule, both renderings, and forged same-origin POSTs against both route
   shapes, plus the resumption of ordinary removal after publication and the
   untouched legacy path.

   Database-gated (EARDE_TEST_DATABASE_URL, the same opt-in as Mod_scope)
   with its own reserved external-installation-id range
   955000001..955000999 (hence account ids 955100001..955100999, which also
   scope the permanent-project cleanup), phdp_% usernames, and phdp-%
   community slugs so no suite shares fixtures. Verified projects come only
   through the real draft/selection/finalization chain, drafts only through
   the real provisioning store, publications only through the real
   publication store and form, reviewed homes only through the real
   request/review stores. Every per-case wrapper disconnects
   deterministically. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rms = Earde.Project_home_removal_store
module Pv = Earde.Project_home_provisioning_store
module Pub = Earde.Network_community_publication_store
module Rq = Earde.Project_home_request_store
module Rvs = Earde.Project_home_review_store

let error_str = Connected_projects_fixture.error_str
let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect
let make_project = Home_provisioning_fixture.make_project
let insert_community = Community_fixture.insert_community
let status_of = Http_fixture.status_of
let ok_loader = Http_fixture.ok_loader

(* Positional substring test, matching the sibling handler suites' shape. *)
let contains haystack needle = Html_assert.occurs haystack ~needle

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_home_audit_events WHERE project_id IN (SELECT id \
       FROM open_source_projects WHERE forge_namespace_id BETWEEN 955100001 \
       AND 955100999)";
      "DELETE FROM open_source_projects WHERE forge_namespace_id BETWEEN \
       955100001 AND 955100999";
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 955000001 AND 955000999)";
      "DELETE FROM communities WHERE slug LIKE 'phdp-%'";
      "DELETE FROM users WHERE username LIKE 'phdp_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       955000001 AND 955000999";
    ]

(* === queries === *)

let q_community_id =
  (Caqti_type.string ->! Caqti_type.int)
    "SELECT id FROM communities WHERE slug = $1"

(* The exact lifecycle tuple the protection rule keys on. *)
let q_lifecycle =
  (Caqti_type.int ->! Caqti_type.string)
    "SELECT visibility || '|' || onboarding_state || '|' || \
     is_network_community::text || '|' || indexable::text || '|' || \
     discoverable::text FROM communities WHERE id = $1"

let q_community_sig = Network_community_fixture.q_community_sig
let q_relation_sig = Network_community_fixture.q_relation_sig
let q_project_sig = Network_community_fixture.q_project_sig
let q_relation_ids = Home_provisioning_fixture.q_relation_ids
let q_section_sigs = Home_provisioning_fixture.q_section_sigs
let q_channel_sigs = Home_provisioning_fixture.q_channel_sigs

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM community_projects WHERE id = $1"

let q_member_sig =
  (Caqti_type.int ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(user_id::text, ',' ORDER BY user_id), \
     '<none>') FROM community_members WHERE community_id = $1"

let q_moderator_sig =
  (Caqti_type.int ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(user_id::text || ':' || role, ',' ORDER BY \
     user_id), '<none>') FROM community_moderators WHERE community_id = $1"

let q_steward_sig =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT COALESCE(string_agg(user_id::text || ':' || role, ',' ORDER BY \
     user_id), '<none>') FROM project_stewards WHERE project_id = $1"

(* The installation record the project's existing stewardship already
   references, so an extra steward row keeps a coherent provenance FK. *)
let q_project_installation =
  (Caqti_type.int64 ->! Caqti_type.int64)
    "SELECT github_installation_record_id FROM project_stewards WHERE \
     project_id = $1 ORDER BY user_id LIMIT 1"

let q_insert_post =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id) VALUES ('Phdp \
     Post', 'Phdp body', $1, $2) RETURNING id"

let q_count_posts =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM posts WHERE community_id = $1"

(* Malformed-draft fixtures. Every value below still satisfies the
   production community_projects_status_shape_check: an 'accepted' row may
   legitimately carry a requester, a reviewer, and a note — that is exactly
   the reviewed accepted home the review store writes. So no constraint is
   relaxed to reach these branches; only the *combination* of that shape
   with an unpublished network draft is contradictory, and that is what the
   store must reject.

   Deliberately not reachable, and therefore not attempted: an accepted row
   with reviewed_at NULL. The shape CHECK forbids it at the database, so
   that branch of the store is schema-protected rather than test-driven. *)
let q_set_requester =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "UPDATE community_projects SET requested_by_user_id = $2 WHERE id = $1"

let q_set_reviewer =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
    "UPDATE community_projects SET reviewed_by_user_id = $2 WHERE id = $1"

let q_set_note =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE community_projects SET request_note = $2 WHERE id = $1"

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

let with_second ~url f =
  let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
  let* conn2 = or_fail "second connect" conn2 in
  let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
  Lwt.finalize (fun () -> f conn2) (fun () -> C2.disconnect ())

(* === call helpers === *)

let remove conn ~actor ~slug ~community =
  Rms.remove conn ~actor_user_id:actor ~project_slug:slug
    ~community_slug:community

let remove_ok label conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok removed ->
      Alcotest.(check string)
        (label ^ ": resulting status")
        (Phr.string_of_status Phr.Removed)
        (Phr.string_of_status (Rms.resulting_status removed));
      Lwt.return_unit
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let remove_expect label expected conn ~actor ~slug ~community =
  let* r = remove conn ~actor ~slug ~community in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* === fixtures === *)

(* One complete unpublished dedicated-community draft, only through the
   real chain: permanent project via draft/selection/finalization, then the
   real atomic provisioning transaction. *)
let make_draft ?(name = "Phdp Community Home") conn ~user ~ext_id ~project_slug
    ~slug =
  let* _inst, project = make_project conn ~user ~ext_id ~slug:project_slug in
  let identity =
    Home_provisioning_fixture.phvf_ok "identity fixture"
      (Home_provisioning_fixture.phvf_fields ~name ~slug ~description:"" ())
  in
  let* r = Pv.provision conn ~actor_user_id:user ~project_slug ~identity in
  let* () =
    match r with
    | Ok home ->
        Alcotest.(check string)
          "draft fixture slug" slug (Pv.community_slug home);
        Lwt.return_unit
    | Error _ -> Alcotest.failf "draft fixture failed for %s" slug
  in
  let* cid = find conn "draft community id" q_community_id slug in
  let* ids = collect conn "relation ids" q_relation_ids project in
  match ids with
  | [ rid ] -> Lwt.return (project, cid, rid)
  | ids ->
      Alcotest.failf "draft fixture: expected one relation, found %d"
        (List.length ids)

(* Publication only through the real store and the real form, keeping the
   identity byte-identical so the community stays addressable at the same
   slug and only the lifecycle changes. *)
let publish_draft label conn ~actor ~slug ~visibility
    ?(name = "Phdp Community Home") () =
  let value =
    Network_community_fixture.ncpf_ok "publication fixture"
      (Network_community_fixture.ncpf_fields ~name ~slug ~description:""
         ~visibility ())
  in
  let* r =
    Pub.publish conn ~actor_user_id:actor ~current_community_slug:slug
      ~publication:value
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.failf "%s: publication fixture failed" label

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let add_top_mod conn ~user ~community = add_role conn ~user ~community "top_mod"

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

let add_steward conn ~project ~user ~installation =
  exec conn "steward fixture" Home_provisioning_fixture.q_insert_steward
    (project, user, installation)

(* A reviewed accepted home on an ordinary legacy community, produced
   exactly as production produces one. *)
let reviewed_home label conn ~owner ~reviewer ~slug ~cid ~community_slug =
  let relation =
    Home_request_fixture.phr_expect_ok
      (Phr.create_pending ~request_note:(Some "phdp note"))
  in
  let* r =
    Rq.create conn ~user_id:owner ~project_slug:slug ~target_community_id:cid
      ~relation
  in
  let* rid =
    match r with
    | Ok created -> Lwt.return (Rq.relation_id created)
    | Error _ -> Alcotest.failf "%s: request fixture failed" label
  in
  let* r =
    Rvs.review conn ~reviewer_user_id:reviewer ~project_slug:slug
      ~target_community_slug:community_slug ~decision:Rvs.Accept
  in
  match r with
  | Ok _ -> Lwt.return rid
  | Error _ -> Alcotest.failf "%s: review fixture failed" label

(* === durable snapshots ===

   Everything a protected removal attempt must leave byte-identical: the
   complete community row, its exact lifecycle tuple, the whole relation
   row (timestamps included), the project row, membership, moderation,
   stewardship, the shell, and the community's content. *)
let snapshot label conn ~cid ~project ~rid =
  let* community = find conn (label ^ ": community") q_community_sig cid in
  let* lifecycle = find conn (label ^ ": lifecycle") q_lifecycle cid in
  let* relation = find conn (label ^ ": relation") q_relation_sig rid in
  let* project_sig = find conn (label ^ ": project") q_project_sig project in
  let* members = find conn (label ^ ": members") q_member_sig cid in
  let* moderators = find conn (label ^ ": mods") q_moderator_sig cid in
  let* stewards = find conn (label ^ ": stewards") q_steward_sig project in
  let* sections = collect conn (label ^ ": sections") q_section_sigs cid in
  let* channels = collect conn (label ^ ": channels") q_channel_sigs cid in
  let* posts = find conn (label ^ ": posts") q_count_posts cid in
  Lwt.return
    (String.concat "||"
       ([
          community;
          lifecycle;
          relation;
          project_sig;
          members;
          moderators;
          stewards;
          string_of_int posts;
        ]
       @ sections @ channels))

let check_unchanged label conn ~cid ~project ~rid before =
  let* after = snapshot label conn ~cid ~project ~rid in
  Alcotest.(check string)
    (label
   ^ ": community, lifecycle, relation, project, membership, moderation, \
      stewardship, shell and content all unchanged")
    before after;
  Lwt.return_unit

(* The exact protected draft tuple, asserted directly rather than only
   through the snapshot, so a fixture that silently stopped being a draft
   could never make the protection assertions vacuous. *)
let check_is_draft label conn cid =
  let* lifecycle = find conn (label ^ ": lifecycle") q_lifecycle cid in
  Alcotest.(check string)
    (label ^ ": private non-indexable non-discoverable network draft")
    "private|draft|true|false|false" lifecycle;
  Lwt.return_unit

let check_status label conn rid expected =
  let* status = find conn (label ^ ": status") q_status rid in
  Alcotest.(check string) (label ^ ": relation status") expected status;
  Lwt.return_unit

(* ================= Store: the draft is protected ================= *)

(* Every durable authority, separately and then combined, against the same
   freshly provisioned draft. Each attempt must answer with the ordinary
   collapsed Removal_unavailable and leave the whole draft byte-identical.
   Both route-independent call shapes are the same store entry point, so
   the loop below is exactly what either HTTP route reaches. *)
let protected_authorities_case =
  db_case
    "draft protection: no steward, top moderator, durable admin, or multiply \
     authorized actor can detach a provisioned home while the community is an \
     unpublished draft" (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* moderator = insert_user conn "phdp_mod" in
      let* admin = insert_user conn "phdp_admin" in
      let* both = insert_user conn "phdp_both" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000001L
          ~project_slug:"phdp-alpha" ~slug:"phdp-alpha-home"
      in
      (* The provisioning actor is already the draft's top_mod; the other
         identities gain exactly one authority each, and the last gains
         all three at once. *)
      let* () = add_member conn ~user:moderator ~community:cid in
      let* () = add_top_mod conn ~user:moderator ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* installation =
        find conn "installation" q_project_installation project
      in
      let* () = add_steward conn ~project ~user:both ~installation in
      let* () = add_member conn ~user:both ~community:cid in
      let* () = add_top_mod conn ~user:both ~community:cid in
      let* () = set_admin conn ~user:both true in
      let* () = check_is_draft "fixture" conn cid in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* () =
        Lwt_list.iter_s
          (fun (who, actor) ->
            let* () =
              remove_expect
                (who ^ " cannot detach a protected draft home")
                Rms.Removal_unavailable conn ~actor ~slug:"phdp-alpha"
                ~community:"phdp-alpha-home"
            in
            (* Nothing accumulates across attempts either. *)
            check_unchanged (who ^ ": after") conn ~cid ~project ~rid before)
          [
            ("project steward", owner);
            ("target top moderator", moderator);
            ("durable global administrator", admin);
            ("multiply authorized actor", both);
          ]
      in
      let* () = check_status "after every attempt" conn rid "accepted" in
      let* () = check_is_draft "after every attempt" conn cid in
      (* The draft stays complete and therefore still publishable: the
         relation the publication store requires is intact, and so is the
         shell around it. *)
      let* sections = collect conn "sections" q_section_sigs cid in
      let* channels = collect conn "channels" q_channel_sigs cid in
      Alcotest.(check int) "General section survives" 1 (List.length sections);
      Alcotest.(check int) "general channel survives" 1 (List.length channels);
      (* Proof the draft really is still publishable — the whole point of
         the rule — through the real publication store. *)
      publish_draft "still publishable" conn ~actor:owner
        ~slug:"phdp-alpha-home" ~visibility:"public" ())

(* An unauthorized actor must still learn nothing: protection must not
   become a second oracle that distinguishes drafts from anything else. *)
let protected_unauthorized_case =
  db_case
    "draft protection: an unauthorized actor still receives the authorization \
     answer, not the protection answer" (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* stranger = insert_user conn "phdp_stranger" in
      let* member = insert_user conn "phdp_member" in
      let* plain_mod = insert_user conn "phdp_plainmod" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000002L ~project_slug:"phdp-beta"
          ~slug:"phdp-beta-home"
      in
      let* () = add_member conn ~user:member ~community:cid in
      let* () = add_member conn ~user:plain_mod ~community:cid in
      let* () = add_role conn ~user:plain_mod ~community:cid "mod" in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      let* () =
        Lwt_list.iter_s
          (fun (who, actor) ->
            remove_expect who Rms.Actor_unauthorized conn ~actor
              ~slug:"phdp-beta" ~community:"phdp-beta-home")
          [
            ("stranger", stranger);
            ("plain member", member);
            ("community mod", plain_mod);
          ]
      in
      check_unchanged "after" conn ~cid ~project ~rid before)

(* ================= Store: publication lifts the protection ============ *)

(* Public and Unlisted separately, and for each the three authorities
   separately — each on its own draft, because a successful removal
   consumes the relation. *)
let published_removable_case =
  db_case
    "published network community: the provisioned home becomes removable again \
     by every durable authority, and the community and its content remain"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* moderator = insert_user conn "phdp_mod" in
      let* admin = insert_user conn "phdp_admin" in
      let* () = set_admin conn ~user:admin true in
      let next_ext = ref 955000010L in
      let scenario (visibility, who, actor_of) index =
        let ext = !next_ext in
        next_ext := Int64.add ext 1L;
        let tag = Printf.sprintf "%s-%d" visibility index in
        let project_slug = "phdp-pub-" ^ tag in
        let slug = "phdp-pub-" ^ tag ^ "-home" in
        let label = visibility ^ " / " ^ who in
        let* project, cid, rid =
          make_draft conn ~user:owner ~ext_id:ext ~project_slug ~slug
        in
        let* () = add_member conn ~user:moderator ~community:cid in
        let* () = add_top_mod conn ~user:moderator ~community:cid in
        (* One post, so "the community and its content remain" is a real
           assertion rather than a vacuous one. *)
        let* _ = find conn "post fixture" q_insert_post (cid, owner) in
        let* () = publish_draft label conn ~actor:owner ~slug ~visibility () in
        let* lifecycle = find conn (label ^ ": lifecycle") q_lifecycle cid in
        Alcotest.(check string)
          (label ^ ": published lifecycle")
          (if visibility = "public" then "public|published|true|true|true"
           else "public|published|true|false|false")
          lifecycle;
        let* community_before =
          find conn (label ^ ": community") q_community_sig cid
        in
        let* () =
          remove_ok label conn
            ~actor:(actor_of ~owner ~moderator ~admin)
            ~slug:project_slug ~community:slug
        in
        let* () = check_status label conn rid "removed" in
        (* The community, its lifecycle, its shell and its content all
           survive detaching the relation. *)
        let* community_after =
          find conn (label ^ ": community after") q_community_sig cid
        in
        Alcotest.(check string)
          (label ^ ": community row untouched by removal")
          community_before community_after;
        let* posts = find conn (label ^ ": posts") q_count_posts cid in
        Alcotest.(check int) (label ^ ": content remains") 1 posts;
        let* sections =
          collect conn (label ^ ": sections") q_section_sigs cid
        in
        let* channels =
          collect conn (label ^ ": channels") q_channel_sigs cid
        in
        Alcotest.(check int)
          (label ^ ": section remains")
          1 (List.length sections);
        Alcotest.(check int)
          (label ^ ": channel remains")
          1 (List.length channels);
        let* project_still =
          find conn (label ^ ": project") q_project_sig project
        in
        Alcotest.(check bool)
          (label ^ ": project remains")
          true
          (String.length project_still > 0);
        Lwt.return_unit
      in
      let authorities =
        [
          ("steward", fun ~owner ~moderator:_ ~admin:_ -> owner);
          ("top moderator", fun ~owner:_ ~moderator ~admin:_ -> moderator);
          ("durable admin", fun ~owner:_ ~moderator:_ ~admin -> admin);
        ]
      in
      Lwt_list.iteri_s
        (fun i (visibility, (who, actor_of)) ->
          scenario (visibility, who, actor_of) i)
        (List.concat_map
           (fun v -> List.map (fun a -> (v, a)) authorities)
           [ "public"; "unlisted" ]))

(* ================= Store: legacy behaviour is untouched ============== *)

let legacy_reviewed_case =
  db_case
    "legacy community: an ordinary reviewed accepted home stays removable \
     exactly as before" (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* reviewer = insert_user conn "phdp_mod" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:955000030L ~slug:"phdp-legacy"
      in
      (* A plain legacy community: not a network community at all, so no
         lifecycle protection can apply to it. *)
      let* cid = insert_community conn "phdp-legacy-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* rid =
        reviewed_home "legacy" conn ~owner ~reviewer ~slug:"phdp-legacy" ~cid
          ~community_slug:"phdp-legacy-home"
      in
      let* community_before = find conn "community" q_community_sig cid in
      let* () =
        remove_ok "legacy steward removal" conn ~actor:owner ~slug:"phdp-legacy"
          ~community:"phdp-legacy-home"
      in
      let* () = check_status "legacy" conn rid "removed" in
      let* community_after = find conn "community after" q_community_sig cid in
      Alcotest.(check string)
        "legacy community untouched" community_before community_after;
      Lwt.return_unit)

(* ================= Store: malformed draft relations ================== *)

let malformed_draft_case =
  db_case
    "malformed draft: an accepted home carrying review provenance under an \
     unpublished draft is corruption, not a removable relation"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* other = insert_user conn "phdp_other" in
      let next_ext = ref 955000040L in
      let scenario (what, corrupt) index =
        let ext = !next_ext in
        next_ext := Int64.add ext 1L;
        let project_slug = Printf.sprintf "phdp-mal-%d" index in
        let slug = Printf.sprintf "phdp-mal-%d-home" index in
        let* project, cid, rid =
          make_draft conn ~user:owner ~ext_id:ext ~project_slug ~slug
        in
        let* () = corrupt conn rid in
        let* before = snapshot what conn ~cid ~project ~rid in
        (* Every authority answers identically: corruption is never
           silently treated as an ordinary removable relation. *)
        let* () =
          remove_expect (what ^ ": steward") Rms.Inconsistent_data conn
            ~actor:owner ~slug:project_slug ~community:slug
        in
        let* () =
          check_unchanged (what ^ ": after") conn ~cid ~project ~rid before
        in
        check_status (what ^ ": still accepted") conn rid "accepted"
      in
      Lwt_list.iteri_s
        (fun i (what, corrupt) -> scenario (what, corrupt) i)
        [
          ( "non-NULL requester",
            fun conn rid -> exec conn "requester" q_set_requester (rid, other)
          );
          ( "non-NULL reviewer",
            fun conn rid -> exec conn "reviewer" q_set_reviewer (rid, other) );
          ( "non-NULL request note",
            fun conn rid -> exec conn "note" q_set_note (rid, "phdp forged note")
          );
        ])

(* ================= Store: publication / removal concurrency ========== *)

(* Both serializations, with two real connections and deterministic
   coordination — the losing side is made to observe committed state, never
   a sleep. The two stores share the project-first lock order, so neither
   ordering can deadlock. *)
let removal_first_race_case =
  db_case
    "race: a removal that reaches the unpublished draft first is refused, and \
     publication then still succeeds" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000050L ~project_slug:"phdp-race"
          ~slug:"phdp-race-home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      with_second ~url (fun conn2 ->
          (* The removal runs first and completes on its own connection,
             against the committed draft state. *)
          let* () =
            remove_expect "removal reaches the draft first"
              Rms.Removal_unavailable conn2 ~actor:owner ~slug:"phdp-race"
              ~community:"phdp-race-home"
          in
          let* () =
            check_unchanged "after refusal" conn ~cid ~project ~rid before
          in
          let* () = check_status "still accepted" conn rid "accepted" in
          (* Publication then proceeds on the other connection. *)
          let* () =
            publish_draft "publication after refused removal" conn ~actor:owner
              ~slug:"phdp-race-home" ~visibility:"public" ()
          in
          let* lifecycle = find conn "lifecycle" q_lifecycle cid in
          Alcotest.(check string)
            "published after the refused removal"
            "public|published|true|true|true" lifecycle;
          Lwt.return_unit))

let publication_first_race_case =
  db_case
    "race: when publication commits first, the subsequent removal succeeds \
     normally and the published community remains" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000051L
          ~project_slug:"phdp-race2" ~slug:"phdp-race2-home"
      in
      let* _ = find conn "post fixture" q_insert_post (cid, owner) in
      with_second ~url (fun conn2 ->
          (* Publication commits on the second connection while the first
             holds nothing; the removal that follows observes only the
             published state. *)
          let* () =
            publish_draft "publication first" conn2 ~actor:owner
              ~slug:"phdp-race2-home" ~visibility:"unlisted" ()
          in
          let* lifecycle = find conn "lifecycle" q_lifecycle cid in
          Alcotest.(check string)
            "published first" "public|published|true|false|false" lifecycle;
          let* community_before = find conn "community" q_community_sig cid in
          let* () =
            remove_ok "removal after publication" conn ~actor:owner
              ~slug:"phdp-race2" ~community:"phdp-race2-home"
          in
          let* () = check_status "removed" conn rid "removed" in
          let* community_after =
            find conn "community after" q_community_sig cid
          in
          Alcotest.(check string)
            "published community remains" community_before community_after;
          let* posts = find conn "posts" q_count_posts cid in
          Alcotest.(check int) "content remains" 1 posts;
          let* project_still = find conn "project" q_project_sig project in
          Alcotest.(check bool)
            "project remains" true
            (String.length project_still > 0);
          Lwt.return_unit))

(* A publication that loses its authority mid-flight leaves the draft
   unpublished — and therefore still protected. *)
let publication_fails_case =
  db_case "race: when publication fails, the draft home stays protected"
    (fun ~url:_ conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* outsider = insert_user conn "phdp_outsider" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000052L ~project_slug:"phdp-fail"
          ~slug:"phdp-fail-home"
      in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      (* An actor with no publication authority: the draft stays a draft. *)
      let value =
        Network_community_fixture.ncpf_ok "publication"
          (Network_community_fixture.ncpf_fields ~name:"Phdp Community Home"
             ~slug:"phdp-fail-home" ~description:"" ~visibility:"public" ())
      in
      let* r =
        Pub.publish conn ~actor_user_id:outsider
          ~current_community_slug:"phdp-fail-home" ~publication:value
      in
      (match r with
      | Ok _ -> Alcotest.fail "unauthorized publication unexpectedly succeeded"
      | Error _ -> ());
      let* () = check_is_draft "after failed publication" conn cid in
      let* () =
        remove_expect "still protected" Rms.Removal_unavailable conn
          ~actor:owner ~slug:"phdp-fail" ~community:"phdp-fail-home"
      in
      check_unchanged "after" conn ~cid ~project ~rid before)

let store_suite =
  [
    protected_authorities_case;
    protected_unauthorized_case;
    published_removable_case;
    legacy_reviewed_case;
    malformed_draft_case;
    removal_first_race_case;
    publication_first_race_case;
    publication_fails_case;
  ]

(* ================= HTTP: the real production pipeline ================ *)

(* One shared single-connection sql_pool for the whole suite, exactly as
   the sibling removal-handler suite does: nothing closes a
   Dream.sql_pool, and a fresh pool per request would exhaust
   max_connections. The session identity is swapped through a ref; cases
   run sequentially. Everything else is the real production shape — the
   real router paths bound to the real handlers. *)
let shared_identity : (int * bool) option ref = ref None
let shared_pipeline = ref None

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline =
        Dream.sql_pool ~size:1 url
        @@ Dream.set_secret Github_fixture.cookie_secret
        @@ Dream.memory_sessions
        @@ (fun handler request ->
          match !shared_identity with
          | None -> handler request
          | Some (uid, is_admin) ->
              let* () =
                Dream.set_session_field request "user_id" (string_of_int uid)
              in
              let* () =
                if is_admin then
                  Dream.set_session_field request "is_admin" "true"
                else Lwt.return_unit
              in
              handler request)
        @@ Dream.router
             [
               (* A protected surface renders no form, so there is no
                  page-embedded CSRF field to lift: a forged submission
                  must mint its own token, which is exactly what an
                  attacker with a live session would do. *)
               Dream.get "/mint" (fun req ->
                   Dream.respond (Dream.csrf_token req));
               Dream.get "/c/:slug/settings"
                 Earde.Community_settings_handlers.community_settings_handler;
               Dream.get "/projects/:slug/request-home" (fun req ->
                   Earde.Project_home_request_handlers
                   .make_project_home_choice_handler ~mode:Ob.Public req);
               Dream.post
                 "/projects/:project_slug/community-home/:community_slug/remove"
                 (fun req ->
                   Earde.Project_home_removal_handlers
                   .make_project_side_home_removal_handler ~mode:Ob.Public
                     ~load_config:(fun () -> ok_loader ())
                     req);
               Dream.post
                 "/c/:community_slug/projects/:project_slug/remove-home"
                 (fun req ->
                   Earde.Project_home_removal_handlers
                   .make_community_side_home_removal_handler ~mode:Ob.Public
                     ~load_config:(fun () -> ok_loader ())
                     req);
             ]
      in
      shared_pipeline := Some pipeline;
      pipeline

let as_user ?(admin_session = false) uid =
  shared_identity := Some (uid, admin_session)

let do_get ~url ~target () =
  let pipeline = pipeline_for ~url in
  let* response = pipeline (Dream.request ~method_:`GET ~target "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* A live session cookie plus a valid framework CSRF token, minted
   independently of any rendered page. *)
let mint ~url =
  let pipeline = pipeline_for ~url in
  let* response = pipeline (Dream.request ~method_:`GET ~target:"/mint" "") in
  let cookie = Http_fixture.session_cookie "mint" response in
  let* token = Dream.body response in
  Lwt.return (cookie, token)

let do_post ~url ~cookie ~target ~token () =
  let pipeline = pipeline_for ~url in
  let headers =
    [
      ("Origin", "https://earde.com");
      ("Content-Type", "application/x-www-form-urlencoded");
      ("Cookie", cookie);
    ]
  in
  pipeline
    (Dream.request ~method_:`POST ~target ~headers
       (Http_fixture.form_body [ ("dream.csrf", token) ]))

let check_clean_redirect label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": exact Location, no query or fragment")
    (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": no-referrer") (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

(* Nothing anywhere in a response may name the protection reason, the
   lifecycle, the authority, or the relation's provenance. *)
let check_no_reason label response body =
  let headers =
    String.concat "\n"
      (List.map (fun (k, v) -> k ^ ": " ^ v) (Dream.all_headers response))
  in
  let blob = body ^ "\n" ^ headers in
  List.iter
    (fun (what, needle) ->
      Alcotest.(check bool)
        (label ^ ": free of " ^ what)
        false (contains blob needle))
    [
      ("onboarding state", "onboarding_state");
      ("draft marker", "draft");
      ("network marker", "is_network_community");
      ("relation table", "community_projects");
      ("index name", "one_active_home");
      ("store variant", "Removal_unavailable");
      ("authority source", "top_mod");
      ("steward source", "project_stewards");
    ]

(* ================= HTTP: project-side rendering ====================== *)

let project_side_render_case =
  db_case
    "project side: the accepted-home page of an unpublished draft carries no \
     removal form and no removal action, and explains why" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* _project, cid, _rid =
        make_draft conn ~user:owner ~ext_id:955000060L
          ~project_slug:"phdp-render" ~slug:"phdp-render-home"
      in
      let* () = check_is_draft "fixture" conn cid in
      as_user owner;
      let* response, body =
        do_get ~url ~target:"/projects/phdp-render/request-home" ()
      in
      Alcotest.(check int) "200" 200 (status_of response);
      (* The accepted state itself still renders. *)
      Alcotest.(check bool)
        "accepted copy" true
        (contains body "Community home connected");
      (* The load-bearing assertion: no action path a forged submission
         could be lifted from. Asserted on the removal route and controls
         specifically — the surrounding site chrome legitimately carries
         its own unrelated forms, so a blanket "<form" count on the whole
         page would prove nothing about this fragment. *)
      Alcotest.(check bool)
        "no removal action emitted" false
        (contains body "community-home");
      Alcotest.(check bool)
        "no removal form element" false
        (contains body "phrm-removal-form");
      Alcotest.(check bool)
        "no remove control" false
        (contains body "Remove home");
      Alcotest.(check bool)
        "no removal heading" false
        (contains body "Remove community home");
      (* Nothing stands in for the missing form: no hidden field carries
         the identities the action path would have. *)
      Alcotest.(check bool)
        "no hidden project slug" false
        (contains body "name='project_slug'");
      Alcotest.(check bool)
        "no hidden community slug" false
        (contains body "name='community_slug'");
      (* The restrained explanation is present and promises nothing. *)
      Alcotest.(check bool)
        "draft-integrity copy" true
        (contains body
           "This project home is part of an unpublished community setup draft.");
      Alcotest.(check bool)
        "next step named" true
        (contains body
           "Complete setup and publish the community before detaching it.");
      Alcotest.(check bool)
        "no destructive alternative" false (contains body "Delete");
      (* No setup link: project stewardship never establishes that the
         viewer may reach the community's setup surface. *)
      Alcotest.(check bool)
        "no setup link inferred from stewardship" false
        (contains body "/c/phdp-render-home/setup");
      Lwt.return_unit)

let project_side_published_render_case =
  db_case
    "project side: Public, Unlisted and legacy reviewed accepted homes all \
     keep the existing removal form" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* reviewer = insert_user conn "phdp_mod" in
      let check label ~project_slug ~community_slug =
        as_user owner;
        let* response, body =
          do_get ~url ~target:("/projects/" ^ project_slug ^ "/request-home") ()
        in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        Alcotest.(check bool)
          (label ^ ": removal form present")
          true
          (contains body
             (Printf.sprintf "action='/projects/%s/community-home/%s/remove'"
                project_slug community_slug));
        Alcotest.(check bool)
          (label ^ ": remove control")
          true
          (contains body "Remove home");
        Alcotest.(check bool)
          (label ^ ": association-only warning")
          true
          (contains body "This removes the association only.");
        Alcotest.(check bool)
          (label ^ ": no draft copy")
          false
          (contains body "unpublished community setup draft");
        Lwt.return_unit
      in
      (* Published Public. *)
      let* _p, _c, _r =
        make_draft conn ~user:owner ~ext_id:955000061L ~project_slug:"phdp-rpub"
          ~slug:"phdp-rpub-home"
      in
      let* () =
        publish_draft "public" conn ~actor:owner ~slug:"phdp-rpub-home"
          ~visibility:"public" ()
      in
      let* () =
        check "public" ~project_slug:"phdp-rpub"
          ~community_slug:"phdp-rpub-home"
      in
      (* Published Unlisted. *)
      let* _p, _c, _r =
        make_draft conn ~user:owner ~ext_id:955000062L ~project_slug:"phdp-runl"
          ~slug:"phdp-runl-home"
      in
      let* () =
        publish_draft "unlisted" conn ~actor:owner ~slug:"phdp-runl-home"
          ~visibility:"unlisted" ()
      in
      let* () =
        check "unlisted" ~project_slug:"phdp-runl"
          ~community_slug:"phdp-runl-home"
      in
      (* Legacy reviewed accepted home. *)
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:955000063L ~slug:"phdp-rleg"
      in
      let* cid = insert_community conn "phdp-rleg-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* _rid =
        reviewed_home "legacy render" conn ~owner ~reviewer ~slug:"phdp-rleg"
          ~cid ~community_slug:"phdp-rleg-home"
      in
      check "legacy reviewed" ~project_slug:"phdp-rleg"
        ~community_slug:"phdp-rleg-home")

let project_side_pending_case =
  db_case
    "project side: the pending and chooser states are unchanged by the \
     protection rule" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:955000064L ~slug:"phdp-pend"
      in
      let* cid = insert_community conn "phdp-pend-home" in
      let relation =
        Home_request_fixture.phr_expect_ok
          (Phr.create_pending ~request_note:(Some "phdp note"))
      in
      let* r =
        Rq.create conn ~user_id:owner ~project_slug:"phdp-pend"
          ~target_community_id:cid ~relation
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error _ -> Alcotest.fail "pending fixture failed"
      in
      as_user owner;
      let* response, body =
        do_get ~url ~target:"/projects/phdp-pend/request-home" ()
      in
      Alcotest.(check int) "200" 200 (status_of response);
      Alcotest.(check bool)
        "pending copy" true
        (contains body "Home request pending");
      Alcotest.(check bool)
        "no removal form" false
        (contains body "community-home");
      Alcotest.(check bool)
        "no draft copy" false
        (contains body "unpublished community setup draft");
      Lwt.return_unit)

(* ================= HTTP: community-settings rendering =============== *)

let settings_render_case =
  db_case
    "community settings: a draft's top moderator and a durable admin both see \
     the connected project without any removal form, alongside the existing \
     setup link" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* admin = insert_user conn "phdp_admin" in
      let* _project, cid, _rid =
        make_draft conn ~user:owner ~ext_id:955000070L ~project_slug:"phdp-set"
          ~slug:"phdp-set-home"
      in
      let* () = set_admin conn ~user:admin true in
      let* () = check_is_draft "fixture" conn cid in
      let check label ~actor ~admin_session =
        as_user ~admin_session actor;
        let* response, body =
          do_get ~url ~target:"/c/phdp-set-home/settings?panel=projects" ()
        in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        (* The connected project's identity is still shown. *)
        Alcotest.(check bool)
          (label ^ ": connected project identity")
          true (contains body "phdp-set");
        Alcotest.(check bool)
          (label ^ ": panel present")
          true
          (contains body "Connected projects");
        (* But no removal control anywhere. *)
        Alcotest.(check bool)
          (label ^ ": no removal action")
          false
          (contains body "remove-home");
        Alcotest.(check bool)
          (label ^ ": no remove control")
          false
          (contains body "Remove home");
        Alcotest.(check bool)
          (label ^ ": draft-integrity copy")
          true
          (contains body "unpublished community setup draft");
        Alcotest.(check bool)
          (label ^ ": association-only warning replaced")
          false
          (contains body "This removes the association only.");
        (* The existing publication navigation is preserved. *)
        Alcotest.(check bool)
          (label ^ ": setup and publish link")
          true
          (contains body "/c/phdp-set-home/setup");
        Alcotest.(check bool)
          (label ^ ": setup link copy")
          true
          (contains body "Complete setup and publish");
        Lwt.return_unit
      in
      let* () = check "top moderator" ~actor:owner ~admin_session:false in
      check "durable admin" ~actor:admin ~admin_session:true)

let settings_published_render_case =
  db_case
    "community settings: Public, Unlisted and legacy communities keep the \
     existing removal form" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* reviewer = insert_user conn "phdp_mod" in
      let check label ~actor ~community_slug ~project_slug =
        as_user actor;
        let* response, body =
          do_get ~url
            ~target:("/c/" ^ community_slug ^ "/settings?panel=projects")
            ()
        in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        Alcotest.(check bool)
          (label ^ ": removal form present")
          true
          (contains body
             (Printf.sprintf "action='/c/%s/projects/%s/remove-home'"
                community_slug project_slug));
        Alcotest.(check bool)
          (label ^ ": remove control")
          true
          (contains body "Remove home");
        Alcotest.(check bool)
          (label ^ ": no draft copy")
          false
          (contains body "unpublished community setup draft");
        Lwt.return_unit
      in
      let* _p, _c, _r =
        make_draft conn ~user:owner ~ext_id:955000071L ~project_slug:"phdp-spub"
          ~slug:"phdp-spub-home"
      in
      let* () =
        publish_draft "public" conn ~actor:owner ~slug:"phdp-spub-home"
          ~visibility:"public" ()
      in
      let* () =
        check "public" ~actor:owner ~community_slug:"phdp-spub-home"
          ~project_slug:"phdp-spub"
      in
      let* _p, _c, _r =
        make_draft conn ~user:owner ~ext_id:955000072L ~project_slug:"phdp-sunl"
          ~slug:"phdp-sunl-home"
      in
      let* () =
        publish_draft "unlisted" conn ~actor:owner ~slug:"phdp-sunl-home"
          ~visibility:"unlisted" ()
      in
      let* () =
        check "unlisted" ~actor:owner ~community_slug:"phdp-sunl-home"
          ~project_slug:"phdp-sunl"
      in
      let* _inst, _project =
        make_project conn ~user:owner ~ext_id:955000073L ~slug:"phdp-sleg"
      in
      let* cid = insert_community conn "phdp-sleg-home" in
      let* () = add_top_mod conn ~user:reviewer ~community:cid in
      let* _rid =
        reviewed_home "legacy settings" conn ~owner ~reviewer ~slug:"phdp-sleg"
          ~cid ~community_slug:"phdp-sleg-home"
      in
      check "legacy" ~actor:reviewer ~community_slug:"phdp-sleg-home"
        ~project_slug:"phdp-sleg")

let settings_unauthorized_case =
  db_case
    "community settings: an ordinary moderator gains no management panel on a \
     protected draft, and the public page gains no controls" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* plain_mod = insert_user conn "phdp_plainmod" in
      let* _project, cid, _rid =
        make_draft conn ~user:owner ~ext_id:955000074L ~project_slug:"phdp-un"
          ~slug:"phdp-un-home"
      in
      let* () = add_member conn ~user:plain_mod ~community:cid in
      let* () = add_role conn ~user:plain_mod ~community:cid "mod" in
      as_user plain_mod;
      let* response, body =
        do_get ~url ~target:"/c/phdp-un-home/settings?panel=projects" ()
      in
      Alcotest.(check int) "200" 200 (status_of response);
      (* No panel at all: the route never loads the read model for a
         non-top-mod, so neither identities nor copy nor controls appear. *)
      Alcotest.(check bool)
        "no management panel" false
        (contains body "Connected projects");
      Alcotest.(check bool)
        "no removal action" false
        (contains body "remove-home");
      Alcotest.(check bool)
        "no draft-integrity copy either" false
        (contains body "unpublished community setup draft");
      Lwt.return_unit)

(* ================= HTTP: forged POSTs against both routes =========== *)

(* A valid same-origin, valid-CSRF submission that no rendered page
   offered. Both route shapes must reach the store, receive the ordinary
   Removal_unavailable, and follow the handlers' existing safe
   current-state redirect — the same destination a successful removal
   uses, so the response is not an oracle either. *)
let forged_post_case =
  db_case
    "forged POST: both route shapes reach the store, are refused, and redirect \
     exactly as an ordinary unavailable removal" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let* project, cid, rid =
        make_draft conn ~user:owner ~ext_id:955000080L
          ~project_slug:"phdp-forge" ~slug:"phdp-forge-home"
      in
      let* () = check_is_draft "fixture" conn cid in
      let* before = snapshot "before" conn ~cid ~project ~rid in
      as_user owner;
      let* cookie, token = mint ~url in
      let attempt label ~target ~expected =
        let* response = do_post ~url ~cookie ~target ~token () in
        let* () = check_clean_redirect label expected response in
        let* body = Dream.body response in
        check_no_reason label response body;
        (* No mutation of any kind. *)
        let* () =
          check_unchanged
            (label ^ ": durable state")
            conn ~cid ~project ~rid before
        in
        check_status (label ^ ": still accepted") conn rid "accepted"
      in
      let* () =
        attempt "project-side route"
          ~target:"/projects/phdp-forge/community-home/phdp-forge-home/remove"
          ~expected:"/projects/phdp-forge/request-home"
      in
      let* () =
        attempt "community-side route"
          ~target:"/c/phdp-forge-home/projects/phdp-forge/remove-home"
          ~expected:"/c/phdp-forge-home/settings?panel=projects"
      in
      (* And the draft is still publishable afterwards. *)
      publish_draft "publishable after forged POSTs" conn ~actor:owner
        ~slug:"phdp-forge-home" ~visibility:"public" ())

(* The same two routes, after publication: the identical submission now
   commits, proving the redirect above really was the shared destination
   and not a protection-specific response. *)
let forged_post_after_publication_case =
  db_case
    "after publication: the same submission against each route removes the \
     home and lands on the same destination" (fun ~url conn ->
      let* owner = insert_user conn "phdp_owner" in
      let scenario label ~ext ~project_slug ~slug ~target ~expected =
        let* _project, cid, rid =
          make_draft conn ~user:owner ~ext_id:ext ~project_slug ~slug
        in
        let* () =
          publish_draft label conn ~actor:owner ~slug ~visibility:"public" ()
        in
        let* community_before =
          find conn (label ^ ": community") q_community_sig cid
        in
        as_user owner;
        let* cookie, token = mint ~url in
        let* response = do_post ~url ~cookie ~target ~token () in
        let* () = check_clean_redirect label expected response in
        let* () = check_status (label ^ ": removed") conn rid "removed" in
        let* community_after =
          find conn (label ^ ": community after") q_community_sig cid
        in
        Alcotest.(check string)
          (label ^ ": published community remains")
          community_before community_after;
        Lwt.return_unit
      in
      let* () =
        scenario "project-side route" ~ext:955000081L ~project_slug:"phdp-fpub"
          ~slug:"phdp-fpub-home"
          ~target:"/projects/phdp-fpub/community-home/phdp-fpub-home/remove"
          ~expected:"/projects/phdp-fpub/request-home"
      in
      scenario "community-side route" ~ext:955000082L ~project_slug:"phdp-fpub2"
        ~slug:"phdp-fpub2-home"
        ~target:"/c/phdp-fpub2-home/projects/phdp-fpub2/remove-home"
        ~expected:"/c/phdp-fpub2-home/settings?panel=projects")

let http_suite =
  [
    project_side_render_case;
    project_side_published_render_case;
    project_side_pending_case;
    settings_render_case;
    settings_published_render_case;
    settings_unauthorized_case;
    forged_post_case;
    forged_post_after_publication_case;
  ]

let suites =
  (* Unpublished network-draft home protection: the durable rule that a
       provisioned dedicated-community home cannot be detached by any
       authority while its community is still an unpublished setup draft
       (and that a draft carrying review provenance is corruption), the
       resumption of ordinary removal after Public/Unlisted publication,
       the untouched legacy path, publication/removal concurrency in both
       serializations, both suppressed removal surfaces, and forged
       same-origin POSTs against both route shapes. Database-gated. *)
  [
    ("project_home_draft_protection_store", store_suite);
    ("project_home_draft_protection_http", http_suite);
  ]
