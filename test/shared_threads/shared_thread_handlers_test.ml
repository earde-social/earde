(* End-to-end coverage of the shared-threads HTTP slice over the real
   routed application: the per-thread Share page, the community management
   page, and the five mutations, each against real durable rows, the real
   Slice 1 store, and the real notification renderer. The harness is the
   Ccon_http one: a full middleware pipeline (one shared sql_pool, secret,
   memory sessions, a login-injecting middleware, the production route
   shapes) invoked directly on synthesized requests — no socket, no
   Dream.run. /mint hands each acting session a fresh and a deliberately
   expired CSRF token. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module H = Earde.Shared_thread_placement_handlers
module Store = Earde.Shared_thread_placement_store

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let status_of = Http_fixture.status_of
let must = Html_assert.must
let must_not = Html_assert.must_not

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator
    (user, community, role)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

let q_remove_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_members WHERE user_id = $1 AND community_id = $2"

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
    "SELECT status FROM shared_thread_placements WHERE id = $1"

let q_section_of =
  (Caqti_type.int64 ->! Caqti_type.(option int))
    "SELECT destination_section_id FROM shared_thread_placements WHERE id = $1"

let action_path ~slug ~id verb =
  Printf.sprintf "%s/%Ld/%s" (Shared_thread_http_fixture.mgmt_path slug) id verb

let check_status label conn id expected =
  let* status = find conn (label ^ ": status") q_status id in
  Alcotest.(check string) (label ^ ": status") expected status;
  Lwt.return_unit

(* === GET /c/:slug/t/:thread/share === *)

let get_share_authz_case =
  Shared_thread_http_fixture.db_case
    "GET share: exactly the author-while-member, origin top_mod, and durable \
     admins; everyone else and every broken subject is one generic 404"
    (fun ~url conn ->
      let* author, otop, _dtop, _o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "ga"
      in
      let* admin = insert_user conn "sth_ga_admin" in
      let* () = set_admin conn ~user:admin true in
      let* modu = insert_user conn "sth_ga_mod" in
      let* legacy = insert_user conn "sth_ga_legacy" in
      let* member = insert_user conn "sth_ga_member" in
      let* stranger = insert_user conn "sth_ga_stranger" in
      let* sonly = insert_user conn "sth_ga_sonly" in
      let* o_id =
        find conn "origin id"
          ((Caqti_type.string ->! Caqti_type.int)
             "SELECT id FROM communities WHERE slug = $1")
          "sth-ga-o"
      in
      let* () = add_role conn ~user:modu ~community:o_id "mod" in
      let* () = add_role conn ~user:legacy ~community:o_id "legacy_mod" in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:member ~community:o_id
      in
      let target =
        Shared_thread_http_fixture.share_path ~slug:"sth-ga-o" ~post
      in
      let sees label uid ~admin =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* response, body = Shared_thread_http_fixture.get ~target pipeline in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        must body ">Share thread</h1>";
        Lwt.return_unit
      in
      let denied label uid ~admin ~target =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* response, body = Shared_thread_http_fixture.get ~target pipeline in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        must_not body "Share thread</h1>";
        Lwt.return_unit
      in
      let* () = sees "author while member" author ~admin:false in
      let* () = sees "origin top mod" otop ~admin:false in
      let* () = sees "durable admin" admin ~admin:true in
      let* () =
        denied "durable admin without claim" admin ~admin:false ~target
      in
      let* () = denied "session-only admin claim" sonly ~admin:true ~target in
      let* () = denied "ordinary mod" modu ~admin:false ~target in
      let* () = denied "legacy mod" legacy ~admin:false ~target in
      let* () = denied "plain member" member ~admin:false ~target in
      let* () = denied "stranger" stranger ~admin:false ~target in
      (* Author paths that must have lapsed. *)
      let* () = exec conn "leave" q_remove_member (author, o_id) in
      let* () = denied "author after leaving" author ~admin:false ~target in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:author ~community:o_id
      in
      let* () =
        exec conn "cban" Shared_thread_http_fixture.q_ban_community
          (author, o_id)
      in
      let* () = denied "community-banned author" author ~admin:false ~target in
      let* () =
        exec conn "unban"
          ((Caqti_type.(t2 int int) ->. Caqti_type.unit)
             "DELETE FROM community_bans WHERE user_id = $1 AND community_id = \
              $2")
          (author, o_id)
      in
      let* () =
        exec conn "gban" Shared_thread_http_fixture.q_ban_global author
      in
      let* () = denied "globally banned author" author ~admin:false ~target in
      let* () =
        exec conn "gunban"
          ((Caqti_type.int ->. Caqti_type.unit)
             "UPDATE users SET is_banned = FALSE WHERE id = $1")
          author
      in
      (* Subject binding and collapse. *)
      let* () =
        denied "wrong community slug" author ~admin:false
          ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-ga-d" ~post)
      in
      ignore d;
      let* () =
        denied "missing thread" author ~admin:false
          ~target:
            (Shared_thread_http_fixture.share_path ~slug:"sth-ga-o"
               ~post:(post + 999999))
      in
      let* () =
        denied "malformed thread segment" author ~admin:false
          ~target:"/c/sth-ga-o/t/not-a-thread/share"
      in
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[deleted]")
      in
      let* () = denied "tombstoned thread" author ~admin:false ~target in
      (* Anonymous callers never reach the read model at all. *)
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response, _ = Shared_thread_http_fixture.get ~target anon in
      Alcotest.(check int) "anonymous: 303" 303 (status_of response);
      Alcotest.(check (option string))
        "to /login" (Some "/login")
        (Dream.header response "Location");
      Lwt.return_unit)

let get_share_candidates_case =
  Shared_thread_http_fixture.db_case
    "GET share: the destination picker offers exactly the eligible, connected, \
     placement-free communities, in normalized order" (fun ~url conn ->
      let* author, otop, _dtop, o, _d, post, _ =
        Shared_thread_http_fixture.fixture conn "gc"
      in
      (* d is connected and eligible: expected. A second connected,
         eligible community with a lowercase name checks normalized
         ordering against d's capital name. *)
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"a lowercase dest"
          conn "sth-gc-d2"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      (* Connected but currently ineligible, in each of the three ways. *)
      let* priv =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Priv" conn
          "sth-gc-priv"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o priv in
      let* () = exec conn "privatize" Community_fixture.q_make_private priv in
      let* draft =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Draft" conn
          "sth-gc-draft"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o draft in
      let* () = exec conn "draft" Community_fixture.q_make_draft_state draft in
      let* dark =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Dark" conn
          "sth-gc-dark"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o dark in
      let* () =
        exec conn "undiscoverable" Community_fixture.q_make_unlisted dark
      in
      (* Eligible but not connected. *)
      let* loose =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Loose" conn
          "sth-gc-loose"
      in
      ignore loose;
      (* Connected with an active placement: excluded from the picker,
         present in the placements list. A terminal history frees the
         slot again. *)
      let* d3 =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Held" conn
          "sth-gc-d3"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d3 in
      let* held =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d3 ()
      in
      ignore held;
      let* d4 =
        Shared_thread_http_fixture.insert_community ~name:"Sth gc Freed" conn
          "sth-gc-d4"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d4 in
      let* freed =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d4 ()
      in
      let* r =
        Store.withdraw conn ~actor_user_id:author ~placement_id:freed
          ~origin_community_id:o
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "fixture withdraw: %s"
              (Shared_thread_fixture.error_str e)
      in
      let pipeline = Shared_thread_http_fixture.session ~url ~uid:author () in
      let* response, body =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-gc-o" ~post)
          pipeline
      in
      Alcotest.(check int) "page 200" 200 (status_of response);
      must body "<option value='sth-gc-d'>";
      must body "<option value='sth-gc-d2'>";
      must body "<option value='sth-gc-d4'>";
      must_not body "<option value='sth-gc-priv'>";
      must_not body "<option value='sth-gc-draft'>";
      must_not body "<option value='sth-gc-dark'>";
      must_not body "<option value='sth-gc-loose'>";
      must_not body "<option value='sth-gc-d3'>";
      (* Ineligible names never leak into the panel at all. *)
      must_not body "Sth gc Priv";
      must_not body "Sth gc Draft";
      (* LOWER(name) ordering: "a lowercase dest" precedes "Sth gc Dest". *)
      (match
         ( Html_assert.index_from body "<option value='sth-gc-d2'>" 0,
           Html_assert.index_from body "<option value='sth-gc-d'>" 0 )
       with
      | Some a, Some b -> Alcotest.(check bool) "normalized order" true (a < b)
      | _ -> Alcotest.fail "options missing for order check");
      (* The held destination shows as a current placement instead. *)
      must body "Sth gc Held";
      must body "Awaiting approval";
      Lwt.return_unit)

let get_share_controls_case =
  Shared_thread_http_fixture.db_case
    "GET share: withdraw/remove controls and the private note follow the \
     viewer, not the login" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "gs"
      in
      (* The author's own pending request, with a distinctive note. *)
      let* mine =
        Shared_thread_http_fixture.seed_request conn ~actor:author
          ~note:"STH_NOTE_GS_AUTHOR" ~post ~destination:d ()
      in
      (* A moderator-sent accepted placement of a second thread. *)
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth gs second", (o, author))
      in
      let* theirs =
        Shared_thread_http_fixture.seed_request conn ~actor:otop
          ~note:"STH_NOTE_GS_TOP" ~post:post2 ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop
          ~placement:theirs ~destination:d ()
      in
      let author_pipe =
        Shared_thread_http_fixture.session ~url ~uid:author ()
      in
      let* _, body =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-gs-o" ~post)
          author_pipe
      in
      (* The author sees their own note and their own withdraw control. *)
      must body "STH_NOTE_GS_AUTHOR";
      must body
        (Printf.sprintf "/c/sth-gs-o/settings/shared-threads/%Ld/withdraw" mine);
      (* But no remove control anywhere: the author is not an origin
         manager. *)
      must_not body "Stop sharing";
      let* _, body2 =
        Shared_thread_http_fixture.get
          ~target:
            (Shared_thread_http_fixture.share_path ~slug:"sth-gs-o" ~post:post2)
          author_pipe
      in
      (* On the moderator's request the author has no withdraw control
         and cannot read the moderator's note. *)
      must_not body2 "STH_NOTE_GS_TOP";
      must_not body2 "/withdraw";
      must body2 ">Shared<";
      let top_pipe = Shared_thread_http_fixture.session ~url ~uid:otop () in
      let* _, body3 =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-gs-o" ~post)
          top_pipe
      in
      (* The origin manager reads the pending note and manages both. *)
      must body3 "STH_NOTE_GS_AUTHOR";
      must body3
        (Printf.sprintf "/c/sth-gs-o/settings/shared-threads/%Ld/withdraw" mine);
      let* _, body4 =
        Shared_thread_http_fixture.get
          ~target:
            (Shared_thread_http_fixture.share_path ~slug:"sth-gs-o" ~post:post2)
          top_pipe
      in
      must body4 "Stop sharing";
      Lwt.return_unit)

let thread_entry_case =
  Shared_thread_http_fixture.db_case
    "thread page: the compact Share action renders exactly for viewers the \
     share read model admits" (fun ~url conn ->
      let* author, otop, _dtop, o, _d, post, _ =
        Shared_thread_http_fixture.fixture conn "te"
      in
      let* member = insert_user conn "sth_te_member" in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:member ~community:o
      in
      let thread_target =
        Earde.Post_cards.canonical_thread_path "sth-te-o" post "Sth te thread"
      in
      let marker = "Share with a community" in
      let body_for uid =
        let pipeline =
          match uid with
          | None -> Shared_thread_http_fixture.app_pipeline ~url ()
          | Some uid -> Shared_thread_http_fixture.session ~url ~uid ()
        in
        let* response, body =
          Shared_thread_http_fixture.get ~target:thread_target pipeline
        in
        Alcotest.(check int) "thread 200" 200 (status_of response);
        Lwt.return body
      in
      let* body = body_for (Some author) in
      must body marker;
      must body (thread_target ^ "/share");
      let* body = body_for (Some otop) in
      must body marker;
      (* A mere login — even a member — never shows the action. *)
      let* body = body_for (Some member) in
      must_not body marker;
      let* body = body_for None in
      must_not body marker;
      (* A tombstoned thread hides it from everyone. *)
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[deleted]")
      in
      let* body = body_for (Some author) in
      must_not body marker;
      Lwt.return_unit)

let settings_nav_case =
  Shared_thread_http_fixture.db_case
    "settings index: Shared threads appears exactly for top mods and admins"
    (fun ~url conn ->
      let* _author, otop, _dtop, o, _d, _post, _ =
        Shared_thread_http_fixture.fixture conn "nv"
      in
      let* modu = insert_user conn "sth_nv_mod" in
      let* () = add_role conn ~user:modu ~community:o "mod" in
      let link = "/c/sth-nv-o/settings/shared-threads" in
      let top_pipe = Shared_thread_http_fixture.session ~url ~uid:otop () in
      let* response, body =
        Shared_thread_http_fixture.get ~target:"/c/sth-nv-o/settings" top_pipe
      in
      Alcotest.(check int) "settings 200" 200 (status_of response);
      must body link;
      must body ">Shared threads</a>";
      let mod_pipe = Shared_thread_http_fixture.session ~url ~uid:modu () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/c/sth-nv-o/settings" mod_pipe
      in
      must_not body link;
      Lwt.return_unit)

(* === POST /c/:slug/t/:thread/share === *)

let post_share_flow_case =
  Shared_thread_http_fixture.db_case
    "POST share: each authorized identity creates one pending placement, with \
     the PRG notice, the audit event, and the destination notification"
    (fun ~url conn ->
      let* author, otop, dtop, o, _d, post, _ =
        Shared_thread_http_fixture.fixture conn "pf"
      in
      let* admin = insert_user conn "sth_pf_admin" in
      let* () = set_admin conn ~user:admin true in
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth pf D2" conn
          "sth-pf-d2"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      let* d3 =
        Shared_thread_http_fixture.insert_community ~name:"Sth pf D3" conn
          "sth-pf-d3"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d3 in
      let target =
        Shared_thread_http_fixture.share_path ~slug:"sth-pf-o" ~post
      in
      let expected_redirect =
        Earde.Post_cards.canonical_thread_path "sth-pf-o" post "Sth pf thread"
        ^ "/share?done=requested"
      in
      let send label uid ~admin ~destination ~note =
        let* pipeline, cookie, token, _ =
          Shared_thread_http_fixture.acting label ~url ~uid ~admin ()
        in
        let* response, body =
          Shared_thread_http_fixture.send_post ~cookie ~target
            ~fields:
              [
                ("dream.csrf", token);
                ("destination", destination);
                ("note", note);
              ]
            pipeline
        in
        Lwt.return (pipeline, cookie, response, body)
      in
      let* pipeline, cookie, response, _ =
        send "author" author ~admin:false ~destination:"sth-pf-d"
          ~note:"STH_NOTE_PF_ONE"
      in
      Shared_thread_http_fixture.check_location "author request"
        expected_redirect response;
      (* Following the redirect renders the restrained notice and the new
         pending row. *)
      let* response, body =
        Shared_thread_http_fixture.get ~cookie ~target:expected_redirect
          pipeline
      in
      Alcotest.(check int) "share after PRG: 200" 200 (status_of response);
      must body "Sharing request sent.";
      must body "Awaiting approval";
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "one placement" 1 n;
      let* events =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      Alcotest.(check int) "one audit event" 1 events;
      let* unread =
        find conn "dtop unread" Shared_thread_http_fixture.q_unread_kinds dtop
      in
      Alcotest.(check int) "destination top mod notified" 1 unread;
      (* The other two authorized identities, to fresh destinations. *)
      let* _, _, response, _ =
        send "origin top mod" otop ~admin:false ~destination:"sth-pf-d2"
          ~note:""
      in
      Shared_thread_http_fixture.check_location "top mod request"
        expected_redirect response;
      let* _, _, response, _ =
        send "durable admin" admin ~admin:true ~destination:"sth-pf-d3" ~note:""
      in
      Shared_thread_http_fixture.check_location "admin request"
        expected_redirect response;
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "three placements" 3 n;
      Lwt.return_unit)

let post_share_denied_case =
  Shared_thread_http_fixture.db_case
    "POST share: every unauthorized identity is a 404 and writes nothing; \
     anonymous goes to login" (fun ~url conn ->
      let* _author, _otop, _dtop, o, _d, post, _ =
        Shared_thread_http_fixture.fixture conn "pd"
      in
      let* modu = insert_user conn "sth_pd_mod" in
      let* () = add_role conn ~user:modu ~community:o "mod" in
      let* legacy = insert_user conn "sth_pd_legacy" in
      let* () = add_role conn ~user:legacy ~community:o "legacy_mod" in
      let* member = insert_user conn "sth_pd_member" in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:member ~community:o
      in
      let* stranger = insert_user conn "sth_pd_stranger" in
      let target =
        Shared_thread_http_fixture.share_path ~slug:"sth-pd-o" ~post
      in
      let refused label uid ~admin =
        let* pipeline, cookie, token, _ =
          Shared_thread_http_fixture.acting label ~url ~uid ~admin ()
        in
        let* response, body =
          Shared_thread_http_fixture.send_post ~cookie ~target
            ~fields:
              [
                ("dream.csrf", token); ("destination", "sth-pd-d"); ("note", "");
              ]
            pipeline
        in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        Lwt.return_unit
      in
      let* () = refused "ordinary mod" modu ~admin:false in
      let* () = refused "legacy mod" legacy ~admin:false in
      let* () = refused "plain member" member ~admin:false in
      let* () = refused "stranger" stranger ~admin:false in
      let* () = refused "session-only admin" stranger ~admin:true in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response =
        anon
          (Dream.request ~method_:`POST ~target
             ~headers:[ ("Content-Type", "application/x-www-form-urlencoded") ]
             (Http_fixture.form_body [ ("destination", "sth-pd-d") ]))
      in
      Alcotest.(check int) "anonymous: 303" 303 (status_of response);
      Alcotest.(check (option string))
        "to /login" (Some "/login")
        (Dream.header response "Location");
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "nothing written" 0 n;
      let* events =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      Alcotest.(check int) "no audit" 0 events;
      Lwt.return_unit)

let post_share_validation_case =
  Shared_thread_http_fixture.db_case
    "POST share: field shape, the empty choice, the oversized note, and \
     destination tampering are each refused without a write" (fun ~url conn ->
      let* author, _otop, _dtop, _o, _d, post, _ =
        Shared_thread_http_fixture.fixture conn "pv"
      in
      let* outsider =
        Shared_thread_http_fixture.insert_community ~name:"Sth pv Outside" conn
          "sth-pv-out"
      in
      ignore outsider;
      let target =
        Shared_thread_http_fixture.share_path ~slug:"sth-pv-o" ~post
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "author" ~url ~uid:author ()
      in
      let send fields =
        Shared_thread_http_fixture.send_post ~cookie ~target ~fields pipeline
      in
      (* No destination field at all. *)
      let* response, _ = send [ ("dream.csrf", token) ] in
      Alcotest.(check int) "missing field: 400" 400 (status_of response);
      (* A blank choice is the required-feedback re-render. *)
      let* response, body =
        send [ ("dream.csrf", token); ("destination", ""); ("note", "") ]
      in
      Alcotest.(check int) "blank choice: 400" 400 (status_of response);
      must body "Choose a community to share this thread with.";
      (* An unknown extra field never reaches the store. *)
      let* response, _ =
        send
          [
            ("dream.csrf", token);
            ("destination", "sth-pv-d");
            ("note", "");
            ("extra", "x");
          ]
      in
      Alcotest.(check int) "extra field: 400" 400 (status_of response);
      (* An oversized note. *)
      let* response, body =
        send
          [
            ("dream.csrf", token);
            ("destination", "sth-pv-d");
            ("note", String.make 2001 'a');
          ]
      in
      Alcotest.(check int) "oversized note: 400" 400 (status_of response);
      must body "Notes are limited to 2,000 characters";
      (* Tampered destinations: an unconnected community and a missing
         one answer identically. *)
      let tampered label slug =
        let* response, body =
          send [ ("dream.csrf", token); ("destination", slug); ("note", "") ]
        in
        Alcotest.(check int) (label ^ ": 409") 409 (status_of response);
        must body "That community is not available to share";
        Lwt.return_unit
      in
      let* () = tampered "unconnected" "sth-pv-out" in
      let* () = tampered "missing" "sth-pv-none" in
      let* () = tampered "the origin itself" "sth-pv-o" in
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "nothing written" 0 n;
      Lwt.return_unit)

let post_share_stale_case =
  Shared_thread_http_fixture.db_case
    "POST share: a connection, eligibility, content, or uniqueness change \
     between render and submit is a safe refusal" (fun ~url conn ->
      let* author, otop, _dtop, o, d, post, connection =
        Shared_thread_http_fixture.fixture conn "pt"
      in
      let target =
        Shared_thread_http_fixture.share_path ~slug:"sth-pt-o" ~post
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "author" ~url ~uid:author ()
      in
      let send () =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:
            [ ("dream.csrf", token); ("destination", "sth-pt-d"); ("note", "") ]
          pipeline
      in
      (* Disconnected after the page rendered. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:otop ~connection ~acting:o
      in
      let* response, body = send () in
      Alcotest.(check int) "disconnected: 409" 409 (status_of response);
      must body "That community is not available to share";
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d in
      (* The destination went private. *)
      let* () = exec conn "privatize" Community_fixture.q_make_private d in
      let* response, body = send () in
      Alcotest.(check int) "ineligible: 409" 409 (status_of response);
      must body "That community is not available to share";
      let* () = exec conn "restore" Shared_thread_fixture.q_make_eligible d in
      (* A concurrent duplicate already holds the active slot. *)
      let* first =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      ignore first;
      let* response, body = send () in
      Alcotest.(check int) "duplicate: 409" 409 (status_of response);
      must body "already shared with that community, or a request is";
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "exactly the seeded row" 1 n;
      let* events =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      Alcotest.(check int) "exactly the seeded event" 1 events;
      (* The thread was tombstoned: the share surface itself collapses. *)
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[deleted]")
      in
      let* response, body = send () in
      Alcotest.(check int) "tombstoned: 404" 404 (status_of response);
      must body "This page does not exist.";
      Lwt.return_unit)

(* === GET /c/:slug/settings/shared-threads === *)

let mgmt_authz_case =
  Shared_thread_http_fixture.db_case
    "GET management: exactly top_mod and durable admins; everyone else is one \
     generic 404" (fun ~url conn ->
      let* _author, _otop, dtop, o, _d, _post, _ =
        Shared_thread_http_fixture.fixture conn "ma"
      in
      let* admin = insert_user conn "sth_ma_admin" in
      let* () = set_admin conn ~user:admin true in
      let* modu = insert_user conn "sth_ma_mod" in
      let* legacy = insert_user conn "sth_ma_legacy" in
      let* member = insert_user conn "sth_ma_member" in
      let* stranger = insert_user conn "sth_ma_stranger" in
      let* sonly = insert_user conn "sth_ma_sonly" in
      let* d_id =
        find conn "dest id"
          ((Caqti_type.string ->! Caqti_type.int)
             "SELECT id FROM communities WHERE slug = $1")
          "sth-ma-d"
      in
      let* () = add_role conn ~user:modu ~community:d_id "mod" in
      let* () = add_role conn ~user:legacy ~community:d_id "legacy_mod" in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:member ~community:d_id
      in
      let sees label uid ~admin =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* response, body =
          Shared_thread_http_fixture.get
            ~target:(Shared_thread_http_fixture.mgmt_path "sth-ma-d")
            pipeline
        in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        must body ">Shared threads</h1>";
        Lwt.return_unit
      in
      let denied label uid ~admin ~slug =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* response, body =
          Shared_thread_http_fixture.get
            ~target:(Shared_thread_http_fixture.mgmt_path slug)
            pipeline
        in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        must_not body "Shared threads</h1>";
        Lwt.return_unit
      in
      let* () = sees "destination top mod" dtop ~admin:false in
      let* () = sees "durable admin" admin ~admin:true in
      let* () =
        denied "durable admin without claim" admin ~admin:false ~slug:"sth-ma-d"
      in
      let* () =
        denied "session-only admin" sonly ~admin:true ~slug:"sth-ma-d"
      in
      let* () = denied "ordinary mod" modu ~admin:false ~slug:"sth-ma-d" in
      let* () = denied "legacy mod" legacy ~admin:false ~slug:"sth-ma-d" in
      let* () = denied "member" member ~admin:false ~slug:"sth-ma-d" in
      let* () = denied "stranger" stranger ~admin:false ~slug:"sth-ma-d" in
      let* () =
        denied "top mod of another community" dtop ~admin:false ~slug:"sth-ma-o"
      in
      ignore o;
      let* () =
        denied "missing community" dtop ~admin:false ~slug:"sth-ma-absent"
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response, _ =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.mgmt_path "sth-ma-d")
          anon
      in
      Alcotest.(check int) "anonymous: 303" 303 (status_of response);
      Alcotest.(check (option string))
        "to /login" (Some "/login")
        (Dream.header response "Location");
      Lwt.return_unit)

let mgmt_content_case =
  Shared_thread_http_fixture.db_case
    "GET management: the four bounded sections classify and order every live \
     placement, with notes, sections, and escapes intact" (fun ~url conn ->
      (* X is the community under management: sectioned, with traffic in
         all four directions. *)
      let* xtop = insert_user conn "sth_mc_xtop" in
      let* requester = insert_user conn "sth_mc_req" in
      let* x =
        Shared_thread_http_fixture.insert_community ~name:"Sth mc Home" conn
          "sth-mc-x"
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:xtop ~community:x
      in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:requester ~community:x
      in
      let* section =
        find conn "section" Shared_thread_fixture.q_insert_section
          (x, "sth-mc-general")
      in
      let* o1 =
        Shared_thread_http_fixture.insert_community
          ~name:"Sth mc O1 & <b>co</b>" conn "sth-mc-o1"
      in
      let* o1top = insert_user conn "sth_mc_o1top" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:o1top ~community:o1
      in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:o1top ~community:o1
      in
      let* d1 =
        Shared_thread_http_fixture.insert_community ~name:"Sth mc D1" conn
          "sth-mc-d1"
      in
      let* () =
        exec conn "flat d1" Shared_thread_fixture.q_set_sections (d1, false)
      in
      let* d1top = insert_user conn "sth_mc_d1top" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:d1top ~community:d1
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:xtop x o1 in
      let* _ = Shared_thread_fixture.connect conn ~actor:xtop x d1 in
      (* Two incoming pending rows from o1, in a fixed order, the first
         with a hostile title and a private note. *)
      let* in_post1 =
        find conn "in post 1" Shared_thread_http_fixture.q_insert_post
          ("Sth mc <script>alert(1)</script> incoming", (o1, o1top))
      in
      let* in_post2 =
        find conn "in post 2" Shared_thread_http_fixture.q_insert_post
          ("Sth mc second incoming", (o1, o1top))
      in
      let* _in1 =
        Shared_thread_http_fixture.seed_request conn ~actor:o1top
          ~note:"STH_NOTE_MC_IN" ~post:in_post1 ~destination:x ()
      in
      let* _in2 =
        Shared_thread_http_fixture.seed_request conn ~actor:o1top ~post:in_post2
          ~destination:x ()
      in
      (* One outgoing pending row: X's own thread requested into d1. *)
      let* out_post =
        find conn "out post" Shared_thread_http_fixture.q_insert_post
          ("Sth mc outgoing", (x, requester))
      in
      let* _out =
        Shared_thread_http_fixture.seed_request conn ~actor:requester
          ~note:"STH_NOTE_MC_OUT" ~post:out_post ~destination:d1 ()
      in
      (* One accepted incoming (into the section) and one accepted
         outgoing (flat). *)
      let* acc_in_post =
        find conn "acc in post" Shared_thread_http_fixture.q_insert_post
          ("Sth mc accepted in", (o1, o1top))
      in
      let* acc_in =
        Shared_thread_http_fixture.seed_request conn ~actor:o1top
          ~post:acc_in_post ~destination:x ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:xtop ~section
          ~placement:acc_in ~destination:x ()
      in
      let* acc_out_post =
        find conn "acc out post" Shared_thread_http_fixture.q_insert_post
          ("Sth mc accepted out", (x, requester))
      in
      let* acc_out =
        Shared_thread_http_fixture.seed_request conn ~actor:requester
          ~post:acc_out_post ~destination:d1 ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:d1top
          ~placement:acc_out ~destination:d1 ()
      in
      let pipeline = Shared_thread_http_fixture.session ~url ~uid:xtop () in
      let* response, body =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.mgmt_path "sth-mc-x")
          pipeline
      in
      Alcotest.(check int) "page 200" 200 (status_of response);
      (* Section order. *)
      let idx label =
        match Html_assert.index_from body label 0 with
        | Some i -> i
        | None -> Alcotest.failf "section %S missing" label
      in
      let incoming = idx ">Incoming requests</h2>" in
      let outgoing = idx ">Outgoing requests</h2>" in
      let into = idx ">Shared into this community</h2>" in
      let from = idx ">Shared from this community</h2>" in
      Alcotest.(check bool)
        "order" true
        (incoming < outgoing && outgoing < into && into < from);
      (* Classification: each row in its own section's span. *)
      let within lo hi needle =
        match Html_assert.index_from body needle 0 with
        | Some i -> i > lo && i < hi
        | None -> false
      in
      Alcotest.(check bool)
        "first incoming row" true
        (within incoming outgoing
           "Sth mc &lt;script&gt;alert(1)&lt;/script&gt; incoming");
      Alcotest.(check bool)
        "second incoming row" true
        (within incoming outgoing "Sth mc second incoming");
      Alcotest.(check bool)
        "incoming order is arrival order" true
        (match
           ( Html_assert.index_from body
               "Sth mc &lt;script&gt;alert(1)&lt;/script&gt; incoming" 0,
             Html_assert.index_from body "Sth mc second incoming" 0 )
         with
        | Some a, Some b -> a < b
        | _ -> false);
      Alcotest.(check bool)
        "outgoing row" true
        (within outgoing into "Sth mc outgoing");
      Alcotest.(check bool)
        "accepted incoming row" true
        (within into from "Sth mc accepted in");
      Alcotest.(check bool)
        "accepted outgoing row" true
        (within from (String.length body) "Sth mc accepted out");
      (* The hostile title stayed inert; the counterpart name escaped. *)
      must_not body "<script>alert(1)</script>";
      must body "Sth mc O1 &amp; &lt;b&gt;co&lt;/b&gt;";
      (* Notes on both pending queues; the accepted rows carry none. *)
      must body "STH_NOTE_MC_IN";
      must body "STH_NOTE_MC_OUT";
      (* Section labels: named for the sectioned acceptance, the flat
         label for the flat one. *)
      must body "Section: sth-mc-general";
      must body "Uncategorized";
      (* One selector per incoming accept form, only X's own sections. *)
      must body "name='section'";
      must body (Printf.sprintf "<option value='%d'>" section);
      (* Timestamps render as relative text. *)
      must body "just now";
      Lwt.return_unit)

let mgmt_flat_and_ineligible_case =
  Shared_thread_http_fixture.db_case
    "GET management: a flat community renders no selector; an ineligible one \
     keeps reject and remove but not accept" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "mf"
      in
      ignore author;
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth mf two", (o, otop))
      in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:otop ~community:o
      in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~post
          ~destination:d ()
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~post:post2
          ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:p2
          ~destination:d ()
      in
      ignore p1;
      let pipeline = Shared_thread_http_fixture.session ~url ~uid:dtop () in
      let* _, body =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.mgmt_path "sth-mf-d")
          pipeline
      in
      (* Flat destination: accept form, no selector. *)
      must body ">Accept</button>";
      must_not body "name='section'";
      (* Now ineligible: accept withdraws, reject and remove stay. *)
      let* () = exec conn "privatize" Community_fixture.q_make_private d in
      let* response, body =
        Shared_thread_http_fixture.get
          ~target:(Shared_thread_http_fixture.mgmt_path "sth-mf-d")
          pipeline
      in
      Alcotest.(check int) "still 200" 200 (status_of response);
      must body "cannot accept newly shared threads right now";
      must_not body ">Accept</button>";
      must body ">Reject</button>";
      must body ">Remove</button>";
      Lwt.return_unit)

(* === POST .../:placement_id/{accept,reject} === *)

let review_flow_case =
  Shared_thread_http_fixture.db_case
    "POST review: flat accept, sectioned accept, and reject each commit once \
     with the PRG notice" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "rf"
      in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "dtop" ~url ~uid:dtop ()
      in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug:"sth-rf-d" ~id:p1 "accept")
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "flat accept"
        (Shared_thread_http_fixture.mgmt_path "sth-rf-d" ^ "?done=accepted")
        response;
      let* () = check_status "flat accept" conn p1 "accepted" in
      let* section_id = find conn "flat section" q_section_of p1 in
      Alcotest.(check (option int)) "flat: no section" None section_id;
      (* The notice renders on the reloaded management page. *)
      let* _, body =
        Shared_thread_http_fixture.get ~cookie
          ~target:
            (Shared_thread_http_fixture.mgmt_path "sth-rf-d" ^ "?done=accepted")
          pipeline
      in
      must body "The thread is now shared into this community.";
      (* A sectioned destination requires — and stores — the choice. *)
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth rf D2" conn
          "sth-rf-d2"
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dtop ~community:d2
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      let* section =
        find conn "section" Shared_thread_fixture.q_insert_section
          (d2, "sth-rf-sec")
      in
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth rf two", (o, author))
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post2
          ~destination:d2 ()
      in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug:"sth-rf-d2" ~id:p2 "accept")
          ~fields:[ ("dream.csrf", token); ("section", string_of_int section) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "sectioned accept"
        (Shared_thread_http_fixture.mgmt_path "sth-rf-d2" ^ "?done=accepted")
        response;
      let* stored = find conn "stored section" q_section_of p2 in
      Alcotest.(check (option int)) "sectioned: stored" (Some section) stored;
      (* Reject. *)
      let* post3 =
        find conn "post3" Shared_thread_http_fixture.q_insert_post
          ("Sth rf three", (o, author))
      in
      let* p3 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post3
          ~destination:d ()
      in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug:"sth-rf-d" ~id:p3 "reject")
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "reject"
        (Shared_thread_http_fixture.mgmt_path "sth-rf-d" ^ "?done=rejected")
        response;
      let* () = check_status "reject" conn p3 "rejected" in
      let* _, body =
        Shared_thread_http_fixture.get ~cookie
          ~target:
            (Shared_thread_http_fixture.mgmt_path "sth-rf-d" ^ "?done=rejected")
          pipeline
      in
      must body "The request was declined. Nothing else changed.";
      Lwt.return_unit)

let review_section_case =
  Shared_thread_http_fixture.db_case
    "POST accept: a foreign, deleted, missing, or misplaced section choice is \
     one generic invalid selection, without a write" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "rs"
      in
      (* A sectioned second destination managed by the same reviewer. *)
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth rs D2" conn
          "sth-rs-d2"
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dtop ~community:d2
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      let* own_section =
        find conn "own section" Shared_thread_fixture.q_insert_section
          (d2, "sth-rs-own")
      in
      (* A section belonging to the origin: valid id, wrong community. *)
      let* foreign_section =
        find conn "foreign section" Shared_thread_fixture.q_insert_section
          (o, "sth-rs-for")
      in
      let* doomed_section =
        find conn "doomed section" Shared_thread_fixture.q_insert_section
          (d2, "sth-rs-del")
      in
      let* () =
        exec conn "delete" Shared_thread_fixture.q_delete_section doomed_section
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d2 ()
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "dtop" ~url ~uid:dtop ()
      in
      let target = action_path ~slug:"sth-rs-d2" ~id:p "accept" in
      let refused label ~status fields =
        let* response, body =
          Shared_thread_http_fixture.send_post ~cookie ~target ~fields pipeline
        in
        Alcotest.(check int) (label ^ ": status") status (status_of response);
        must body "Choose one of this community's own forum sections";
        let* () = check_status label conn p "pending" in
        Lwt.return_unit
      in
      let* () =
        refused "foreign section" ~status:409
          [ ("dream.csrf", token); ("section", string_of_int foreign_section) ]
      in
      let* () =
        refused "deleted section" ~status:409
          [ ("dream.csrf", token); ("section", string_of_int doomed_section) ]
      in
      let* () =
        refused "no choice on a sectioned destination" ~status:400
          [ ("dream.csrf", token) ]
      in
      let* () =
        refused "blank choice" ~status:400
          [ ("dream.csrf", token); ("section", "") ]
      in
      (* On the flat destination a section field is refused too. *)
      let* p_flat =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug:"sth-rs-d" ~id:p_flat "accept")
          ~fields:
            [ ("dream.csrf", token); ("section", string_of_int own_section) ]
          pipeline
      in
      Alcotest.(check int) "section on flat: 400" 400 (status_of response);
      let* () = check_status "flat untouched" conn p_flat "pending" in
      (* The valid choice still works afterwards. *)
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:
            [ ("dream.csrf", token); ("section", string_of_int own_section) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "valid accept"
        (Shared_thread_http_fixture.mgmt_path "sth-rs-d2" ^ "?done=accepted")
        response;
      Lwt.return_unit)

let review_stale_case =
  Shared_thread_http_fixture.db_case
    "POST review: accept refuses on a stale world, reject always closes, and a \
     decided request answers one stable conflict" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, connection =
        Shared_thread_http_fixture.fixture conn "rw"
      in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "dtop" ~url ~uid:dtop ()
      in
      let accept_target id = action_path ~slug:"sth-rw-d" ~id "accept" in
      let reject_target id = action_path ~slug:"sth-rw-d" ~id "reject" in
      let accept id =
        Shared_thread_http_fixture.send_post ~cookie ~target:(accept_target id)
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      let reject id =
        Shared_thread_http_fixture.send_post ~cookie ~target:(reject_target id)
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      (* Disconnected. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:otop ~connection ~acting:o
      in
      let* response, body = accept p1 in
      Alcotest.(check int) "disconnected accept: 409" 409 (status_of response);
      must body "no longer available for sharing";
      let* () = check_status "still pending" conn p1 "pending" in
      let* connection = Shared_thread_fixture.connect conn ~actor:otop o d in
      ignore connection;
      (* The origin went private. *)
      let* () =
        exec conn "privatize origin" Community_fixture.q_make_private o
      in
      let* response, body = accept p1 in
      Alcotest.(check int)
        "origin ineligible accept: 409" 409 (status_of response);
      must body "no longer available for sharing";
      let* () =
        exec conn "restore origin" Shared_thread_fixture.q_make_eligible o
      in
      (* The thread was tombstoned. *)
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[removed by moderator]")
      in
      let* response, body = accept p1 in
      Alcotest.(check int) "tombstoned accept: 409" 409 (status_of response);
      must body "This thread can no longer be shared.";
      let* () = check_status "still pending" conn p1 "pending" in
      (* Reject stays available under exactly those conditions. *)
      let* () =
        exec conn "privatize origin" Community_fixture.q_make_private o
      in
      let* response, _ = reject p1 in
      Shared_thread_http_fixture.check_location "stale-world reject"
        (Shared_thread_http_fixture.mgmt_path "sth-rw-d" ^ "?done=rejected")
        response;
      let* () = check_status "rejected" conn p1 "rejected" in
      (* A decided request: both verbs answer the one stable conflict,
         and neither writes a second decision, audit event, or
         notification. *)
      let* events_before =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      let* notifs_before =
        find conn "notifs" Shared_thread_fixture.q_notifs_for_post post
      in
      let* response, body = accept p1 in
      Alcotest.(check int) "decided accept: 409" 409 (status_of response);
      must body "That sharing request is no longer pending.";
      let* response, body = reject p1 in
      Alcotest.(check int) "decided reject: 409" 409 (status_of response);
      must body "That sharing request is no longer pending.";
      let* events_after =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      let* notifs_after =
        find conn "notifs" Shared_thread_fixture.q_notifs_for_post post
      in
      Alcotest.(check int) "no duplicate audit" events_before events_after;
      Alcotest.(check int)
        "no duplicate notifications" notifs_before notifs_after;
      Lwt.return_unit)

let review_authz_case =
  Shared_thread_http_fixture.db_case
    "POST review: only the destination's top mods and durable admins, on the \
     destination's own route, over its own placement" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "ra"
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      (* An unrelated pair with its own pending placement. *)
      let* o2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth ra O2" conn
          "sth-ra-o2"
      in
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth ra D2" conn
          "sth-ra-d2"
      in
      let* () =
        exec conn "flat d2" Shared_thread_fixture.q_set_sections (d2, false)
      in
      let* other = insert_user conn "sth_ra_other" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:other ~community:o2
      in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:other ~community:o2
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:other o2 d2 in
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth ra other", (o2, other))
      in
      let* foreign =
        Shared_thread_http_fixture.seed_request conn ~actor:other ~post:post2
          ~destination:d2 ()
      in
      let* modu = insert_user conn "sth_ra_mod" in
      let* () = add_role conn ~user:modu ~community:d "mod" in
      let* legacy = insert_user conn "sth_ra_legacy" in
      let* () = add_role conn ~user:legacy ~community:d "legacy_mod" in
      let refused label uid ~admin ~target =
        let* pipeline, cookie, token, _ =
          Shared_thread_http_fixture.acting label ~url ~uid ~admin ()
        in
        let* response, body =
          Shared_thread_http_fixture.send_post ~cookie ~target
            ~fields:[ ("dream.csrf", token) ]
            pipeline
        in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        Lwt.return_unit
      in
      let accept_d = action_path ~slug:"sth-ra-d" ~id:p "accept" in
      let* () = refused "origin top mod" otop ~admin:false ~target:accept_d in
      let* () = refused "ordinary mod" modu ~admin:false ~target:accept_d in
      let* () = refused "legacy mod" legacy ~admin:false ~target:accept_d in
      let* () = refused "author" author ~admin:false ~target:accept_d in
      let* () =
        refused "session-only admin" modu ~admin:true ~target:accept_d
      in
      (* The origin's route never reviews: subject mismatch. *)
      let* () =
        refused "origin-route accept" otop ~admin:false
          ~target:(action_path ~slug:"sth-ra-o" ~id:p "accept")
      in
      (* Someone else's placement through this community's route. *)
      let* () =
        refused "foreign placement" dtop ~admin:false
          ~target:(action_path ~slug:"sth-ra-d" ~id:foreign "accept")
      in
      (* Broken ids. *)
      let* () =
        refused "absent placement" dtop ~admin:false
          ~target:(action_path ~slug:"sth-ra-d" ~id:99999999L "accept")
      in
      let* () =
        refused "malformed placement id" dtop ~admin:false
          ~target:
            (Shared_thread_http_fixture.mgmt_path "sth-ra-d"
            ^ "/not-an-id/accept")
      in
      let* () = check_status "untouched" conn p "pending" in
      let* () = check_status "foreign untouched" conn foreign "pending" in
      ignore o;
      Lwt.return_unit)

(* === POST .../:placement_id/withdraw === *)

let withdraw_authz_case =
  Shared_thread_http_fixture.db_case
    "POST withdraw: the requester, origin top mods, and durable admins; \
     everyone else is one generic 404" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "wa"
      in
      let* admin = insert_user conn "sth_wa_admin" in
      let* () = set_admin conn ~user:admin true in
      let* stranger = insert_user conn "sth_wa_stranger" in
      (* Three parallel pending requests to distinct destinations, all
         sent by the author. *)
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth wa D2" conn
          "sth-wa-d2"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      let* d3 =
        Shared_thread_http_fixture.insert_community ~name:"Sth wa D3" conn
          "sth-wa-d3"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d3 in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d2 ()
      in
      let* p3 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d3 ()
      in
      let withdraw label uid ~admin ~slug ~id =
        let* pipeline, cookie, token, _ =
          Shared_thread_http_fixture.acting label ~url ~uid ~admin ()
        in
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug ~id "withdraw")
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      (* Every denied identity first, against the same live request. *)
      let refused label uid ~admin ~slug ~id =
        let* response, body = withdraw label uid ~admin ~slug ~id in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        let* () = check_status label conn id "pending" in
        Lwt.return_unit
      in
      let* () =
        refused "author as non-requester bystander" stranger ~admin:false
          ~slug:"sth-wa-o" ~id:p1
      in
      let* () =
        refused "destination top mod" dtop ~admin:false ~slug:"sth-wa-o" ~id:p1
      in
      let* () =
        refused "destination route" author ~admin:false ~slug:"sth-wa-d" ~id:p1
      in
      let* () =
        refused "session-only admin" stranger ~admin:true ~slug:"sth-wa-o"
          ~id:p1
      in
      (* A thread author who did not send this request and holds no role:
         a second author's post requested by the top mod. *)
      let* author2 = insert_user conn "sth_wa_author2" in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:author2 ~community:o
      in
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth wa two", (o, author2))
      in
      let* d4 =
        Shared_thread_http_fixture.insert_community ~name:"Sth wa D4" conn
          "sth-wa-d4"
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d4 in
      let* p4 =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~post:post2
          ~destination:d4 ()
      in
      let* () =
        refused "author without requester or role" author2 ~admin:false
          ~slug:"sth-wa-o" ~id:p4
      in
      (* The three authorized identities. *)
      let* response, _ =
        withdraw "requester" author ~admin:false ~slug:"sth-wa-o" ~id:p1
      in
      Shared_thread_http_fixture.check_location "requester"
        (Shared_thread_http_fixture.mgmt_path "sth-wa-o" ^ "?done=withdrawn")
        response;
      let* () = check_status "requester withdrew" conn p1 "withdrawn" in
      let* response, _ =
        withdraw "origin top mod" otop ~admin:false ~slug:"sth-wa-o" ~id:p2
      in
      Shared_thread_http_fixture.check_location "origin top mod"
        (Shared_thread_http_fixture.mgmt_path "sth-wa-o" ^ "?done=withdrawn")
        response;
      let* response, _ =
        withdraw "durable admin" admin ~admin:true ~slug:"sth-wa-o" ~id:p3
      in
      Shared_thread_http_fixture.check_location "durable admin"
        (Shared_thread_http_fixture.mgmt_path "sth-wa-o" ^ "?done=withdrawn")
        response;
      Lwt.return_unit)

let withdraw_resilience_case =
  Shared_thread_http_fixture.db_case
    "POST withdraw: survives disconnection, ineligibility, tombstoning, and \
     membership loss; a stale repeat answers each surface truthfully"
    (fun ~url conn ->
      let* author, otop, _dtop, o, d, post, connection =
        Shared_thread_http_fixture.fixture conn "wr"
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      (* The world rots completely. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:otop ~connection ~acting:o
      in
      let* () = exec conn "privatize" Community_fixture.q_make_private o in
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[deleted]")
      in
      let* () = exec conn "leave" q_remove_member (author, o) in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "author" ~url ~uid:author ()
      in
      let target = action_path ~slug:"sth-wr-o" ~id:p "withdraw" in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "withdrawal after everything"
        (Shared_thread_http_fixture.mgmt_path "sth-wr-o" ^ "?done=withdrawn")
        response;
      let* () = check_status "withdrawn" conn p "withdrawn" in
      (* The stale repeat: the requester lost every surface, so the
         truthful generic conflict page answers. *)
      let* response, body =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Alcotest.(check int) "surface-less repeat: 409" 409 (status_of response);
      must body "That sharing request is no longer pending. Nothing was";
      (* A manager's stale repeat re-renders their management page. *)
      let* mpipe, mcookie, mtoken, _ =
        Shared_thread_http_fixture.acting "otop" ~url ~uid:otop ()
      in
      let* response, body =
        Shared_thread_http_fixture.send_post ~cookie:mcookie ~target
          ~fields:[ ("dream.csrf", mtoken) ]
          mpipe
      in
      Alcotest.(check int) "manager repeat: 409" 409 (status_of response);
      must body ">Shared threads</h1>";
      must body "That sharing request is no longer pending.";
      Lwt.return_unit)

let withdraw_context_case =
  Shared_thread_http_fixture.db_case
    "POST withdraw: the share-page marker returns the browser to the Share \
     page; anything else in the fields is refused" (fun ~url conn ->
      let* author, _otop, _dtop, _o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "wc"
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "author" ~url ~uid:author ()
      in
      let target = action_path ~slug:"sth-wc-o" ~id:p "withdraw" in
      (* An unknown field shape never reaches the store. *)
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token); ("context", "elsewhere") ]
          pipeline
      in
      Alcotest.(check int) "unknown context: 400" 400 (status_of response);
      let* () = check_status "untouched" conn p "pending" in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token); ("context", "share") ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "share-page withdrawal"
        (Printf.sprintf "/c/sth-wc-o/t/%d/share?done=withdrawn" post)
        response;
      let* () = check_status "withdrawn" conn p "withdrawn" in
      (* Following the redirect renders the notice on the Share page. *)
      let* response, body =
        Shared_thread_http_fixture.get ~cookie
          ~target:(Printf.sprintf "/c/sth-wc-o/t/%d/share?done=withdrawn" post)
          pipeline
      in
      Alcotest.(check int) "share page 200" 200 (status_of response);
      must body "The sharing request was withdrawn.";
      Lwt.return_unit)

(* === POST .../:placement_id/remove === *)

let remove_authz_case =
  Shared_thread_http_fixture.db_case
    "POST remove: either side's top mods and durable admins; the author, \
     ordinary moderators, and outside routes are 404" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "va"
      in
      let* admin = insert_user conn "sth_va_admin" in
      let* () = set_admin conn ~user:admin true in
      let* modu = insert_user conn "sth_va_mod" in
      let* () = add_role conn ~user:modu ~community:o "mod" in
      let* legacy = insert_user conn "sth_va_legacy" in
      let* () = add_role conn ~user:legacy ~community:d "legacy_mod" in
      (* An uninvolved community with its own top mod. *)
      let* x =
        Shared_thread_http_fixture.insert_community ~name:"Sth va X" conn
          "sth-va-x"
      in
      let* xtop = insert_user conn "sth_va_xtop" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:xtop ~community:x
      in
      let accepted () =
        let* p =
          Shared_thread_http_fixture.seed_request conn ~actor:author ~post
            ~destination:d ()
        in
        let* () =
          Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop
            ~placement:p ~destination:d ()
        in
        Lwt.return p
      in
      let remove label uid ~admin ~slug ~id =
        let* pipeline, cookie, token, _ =
          Shared_thread_http_fixture.acting label ~url ~uid ~admin ()
        in
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug ~id "remove")
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      let* p = accepted () in
      let refused label uid ~admin ~slug =
        let* response, body = remove label uid ~admin ~slug ~id:p in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        let* () = check_status label conn p "accepted" in
        Lwt.return_unit
      in
      let* () =
        refused "author without a role" author ~admin:false ~slug:"sth-va-o"
      in
      let* () = refused "ordinary mod" modu ~admin:false ~slug:"sth-va-o" in
      let* () = refused "legacy mod" legacy ~admin:false ~slug:"sth-va-d" in
      let* () =
        refused "uninvolved community's top mod" xtop ~admin:false
          ~slug:"sth-va-x"
      in
      let* () =
        refused "session-only admin" modu ~admin:true ~slug:"sth-va-o"
      in
      (* The three authorized identities, one placement each. *)
      let* response, _ =
        remove "origin top mod" otop ~admin:false ~slug:"sth-va-o" ~id:p
      in
      Shared_thread_http_fixture.check_location "origin side"
        (Shared_thread_http_fixture.mgmt_path "sth-va-o" ^ "?done=removed")
        response;
      let* () = check_status "removed" conn p "removed" in
      let* p2 = accepted () in
      let* response, _ =
        remove "destination top mod" dtop ~admin:false ~slug:"sth-va-d" ~id:p2
      in
      Shared_thread_http_fixture.check_location "destination side"
        (Shared_thread_http_fixture.mgmt_path "sth-va-d" ^ "?done=removed")
        response;
      let* p3 = accepted () in
      let* response, _ =
        remove "durable admin" admin ~admin:true ~slug:"sth-va-d" ~id:p3
      in
      Shared_thread_http_fixture.check_location "admin"
        (Shared_thread_http_fixture.mgmt_path "sth-va-d" ^ "?done=removed")
        response;
      Lwt.return_unit)

let remove_resilience_case =
  Shared_thread_http_fixture.db_case
    "POST remove: survives every stale condition, touches only its own \
     placement, and a repeat answers one stable conflict" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, connection =
        Shared_thread_http_fixture.fixture conn "vr"
      in
      (* The destination becomes sectioned; the acceptance names the
         section; a sibling placement of the same thread stays accepted
         elsewhere; the canonical thread has a comment to preserve. *)
      let* () =
        exec conn "sectioned" Shared_thread_fixture.q_set_sections (d, true)
      in
      let* section =
        find conn "section" Shared_thread_fixture.q_insert_section
          (d, "sth-vr-sec")
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section
          ~placement:p ~destination:d ()
      in
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth vr D2" conn
          "sth-vr-d2"
      in
      let* () =
        exec conn "flat d2" Shared_thread_fixture.q_set_sections (d2, false)
      in
      let* d2top = insert_user conn "sth_vr_d2top" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:d2top ~community:d2
      in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      let* sibling =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d2 ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:d2top
          ~placement:sibling ~destination:d2 ()
      in
      let* _comment =
        find conn "comment" Shared_thread_fixture.q_insert_comment (post, author)
      in
      (* The world rots: disconnect, privatize, tombstone, delete the
         accepted section. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:otop ~connection ~acting:o
      in
      let* () =
        exec conn "privatize origin" Community_fixture.q_make_private o
      in
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[removed by admin]")
      in
      let* () =
        exec conn "delete section" Shared_thread_fixture.q_delete_section
          section
      in
      let* pipeline, cookie, token, _ =
        Shared_thread_http_fixture.acting "otop" ~url ~uid:otop ()
      in
      let target = action_path ~slug:"sth-vr-o" ~id:p "remove" in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "removal after everything"
        (Shared_thread_http_fixture.mgmt_path "sth-vr-o" ^ "?done=removed")
        response;
      let* () = check_status "removed" conn p "removed" in
      (* Only the selected placement changed; the canonical thread and
         its comments survive untouched. *)
      let* () = check_status "sibling untouched" conn sibling "accepted" in
      let* title, content =
        find conn "post row" Shared_thread_fixture.q_post_row post
      in
      Alcotest.(check string) "title kept" "Sth vr thread" title;
      Alcotest.(check (option string))
        "content untouched by removal" (Some "[removed by admin]") content;
      let* comments =
        find conn "comments" Shared_thread_fixture.q_comment_count post
      in
      Alcotest.(check int) "comment kept" 1 comments;
      (* A repeat answers the stable conflict on the management page. *)
      let* response, body =
        Shared_thread_http_fixture.send_post ~cookie ~target
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      Alcotest.(check int) "repeat: 409" 409 (status_of response);
      must body "That thread is no longer shared here.";
      (* The share-page marker returns an origin-side manager to the
         Share page — over a healthy world again, since a fresh request
         needs the origin eligible and the content live. *)
      let* () =
        exec conn "restore origin" Shared_thread_fixture.q_make_eligible o
      in
      let* () =
        exec conn "restore content" Shared_thread_fixture.q_tombstone
          (post, "sth body")
      in
      let* r =
        Store.remove conn ~actor_user_id:otop ~placement_id:sibling
          ~acting_community_id:o
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "clear sibling: %s"
              (Shared_thread_fixture.error_str e)
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~post
          ~destination:d2 ()
      in
      let* r =
        Store.review conn ~reviewer_user_id:d2top ~placement_id:p2
          ~destination_community_id:d2 ~decision:(Store.Accept None)
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "sibling accept: %s"
              (Shared_thread_fixture.error_str e)
      in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(action_path ~slug:"sth-vr-o" ~id:p2 "remove")
          ~fields:[ ("dream.csrf", token); ("context", "share") ]
          pipeline
      in
      Shared_thread_http_fixture.check_location "share-page removal"
        (Printf.sprintf "/c/sth-vr-o/t/%d/share?done=removed" post)
        response;
      Lwt.return_unit)

(* === CSRF === *)

let csrf_case =
  Shared_thread_http_fixture.db_case
    "every mutation requires the framework CSRF field and answers a stale \
     token with an authorized re-render" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "cs"
      in
      let* p_pending =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth cs two", (o, author))
      in
      let* p_accepted =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post2
          ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop
          ~placement:p_accepted ~destination:d ()
      in
      let* apipe, acookie, _atoken, aexpired =
        Shared_thread_http_fixture.acting "author" ~url ~uid:author ()
      in
      let* dpipe, dcookie, _dtoken, dexpired =
        Shared_thread_http_fixture.acting "dtop" ~url ~uid:dtop ()
      in
      let* opipe, ocookie, _otoken, oexpired =
        Shared_thread_http_fixture.acting "otop" ~url ~uid:otop ()
      in
      let refused label ~pipeline ~cookie ~target ~fields =
        let* response, body =
          Shared_thread_http_fixture.send_post ~cookie ~target ~fields pipeline
        in
        Alcotest.(check int) (label ^ ": 403") 403 (status_of response);
        must body "open too long";
        Lwt.return_unit
      in
      let share_target =
        Shared_thread_http_fixture.share_path ~slug:"sth-cs-o" ~post
      in
      let* () =
        refused "share, no token" ~pipeline:apipe ~cookie:acookie
          ~target:share_target
          ~fields:[ ("destination", "sth-cs-d") ]
      in
      let* () =
        refused "share, junk token" ~pipeline:apipe ~cookie:acookie
          ~target:share_target
          ~fields:[ ("dream.csrf", "junk"); ("destination", "sth-cs-d") ]
      in
      let* () =
        refused "share, expired token" ~pipeline:apipe ~cookie:acookie
          ~target:share_target
          ~fields:[ ("dream.csrf", aexpired); ("destination", "sth-cs-d") ]
      in
      let* () =
        refused "accept, no token" ~pipeline:dpipe ~cookie:dcookie
          ~target:(action_path ~slug:"sth-cs-d" ~id:p_pending "accept")
          ~fields:[]
      in
      let* () =
        refused "reject, expired token" ~pipeline:dpipe ~cookie:dcookie
          ~target:(action_path ~slug:"sth-cs-d" ~id:p_pending "reject")
          ~fields:[ ("dream.csrf", dexpired) ]
      in
      let* () =
        refused "withdraw, no token" ~pipeline:apipe ~cookie:acookie
          ~target:(action_path ~slug:"sth-cs-o" ~id:p_pending "withdraw")
          ~fields:[]
      in
      let* () =
        refused "remove, expired token" ~pipeline:opipe ~cookie:ocookie
          ~target:(action_path ~slug:"sth-cs-o" ~id:p_accepted "remove")
          ~fields:[ ("dream.csrf", oexpired) ]
      in
      (* Nothing moved, and no event or notification was appended. *)
      let* () = check_status "still pending" conn p_pending "pending" in
      let* () = check_status "still accepted" conn p_accepted "accepted" in
      let* events =
        find conn "events" Shared_thread_fixture.q_events_for_post post
      in
      Alcotest.(check int) "one request event only" 1 events;
      let* n =
        find conn "rows" Shared_thread_http_fixture.q_count_for_post post
      in
      Alcotest.(check int) "one placement only" 1 n;
      Lwt.return_unit)

(* === Notification rendering === *)

let notif_render_case =
  Shared_thread_http_fixture.db_case
    "the five kinds render actor-neutral copy with per-side links, count in \
     the badge, and are read by visiting the mailbox" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "nr"
      in
      let share_link =
        Earde.Post_cards.canonical_thread_path "sth-nr-o" post "Sth nr thread"
        ^ "/share"
      in
      (* 1: requested — the destination's exact top mods. *)
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author
          ~note:"STH_NOTE_NR_SECRET" ~post ~destination:d ()
      in
      let unread_of label user =
        let* counted = Earde.Notification_store.count_unread_notifs conn user in
        match counted with
        | Ok n -> Lwt.return n
        | Error e -> Alcotest.failf "%s: %s" label e
      in
      (* The badge's own durable count includes the new kind with no
         middleware change; the kind-filtered count isolates it from the
         connection handshake the fixture already notified about. *)
      let* unread_shared =
        find conn "kind unread" Shared_thread_http_fixture.q_unread_kinds dtop
      in
      Alcotest.(check int) "badge counts the new row" 1 unread_shared;
      let* unread = unread_of "badge before" dtop in
      Alcotest.(check bool) "durable count includes it" true (unread >= 1);
      let dpipe = Shared_thread_http_fixture.session ~url ~uid:dtop () in
      let* response, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      Alcotest.(check int) "mailbox 200" 200 (status_of response);
      must body
        "Sth nr Origin requested to share &#8220;Sth nr thread&#8221; with Sth \
         nr Dest";
      must body "href='/c/sth-nr-d/settings/shared-threads#incoming'";
      must body "&#128279;";
      must_not body "sth_nr_author";
      must_not body "STH_NOTE_NR_SECRET";
      (* Visiting the mailbox — and only that — marked it read. *)
      let* unread = unread_of "badge after" dtop in
      Alcotest.(check int) "mailbox visit marked read" 0 unread;
      (* 2: accepted — the requester/author, in origin context. *)
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:p1
          ~destination:d ()
      in
      let apipe = Shared_thread_http_fixture.session ~url ~uid:author () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" apipe
      in
      must body "Sth nr Dest accepted &#8220;Sth nr thread&#8221;";
      must body (Printf.sprintf "href='%s'" share_link);
      must_not body "sth_nr_dtop";
      (* 3: rejected. *)
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth nr two", (o, author))
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post2
          ~destination:d ()
      in
      let* r =
        Store.review conn ~reviewer_user_id:dtop ~placement_id:p2
          ~destination_community_id:d ~decision:Store.Reject
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "reject: %s" (Shared_thread_fixture.error_str e)
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" apipe
      in
      must body "Sth nr Dest declined &#8220;Sth nr two&#8221;";
      (* 4: withdrawn — the destination's top mods, actor excluded. *)
      let* post3 =
        find conn "post3" Shared_thread_http_fixture.q_insert_post
          ("Sth nr three", (o, author))
      in
      let* p3 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post3
          ~destination:d ()
      in
      let* r =
        Store.withdraw conn ~actor_user_id:author ~placement_id:p3
          ~origin_community_id:o
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "withdraw: %s" (Shared_thread_fixture.error_str e)
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must body
        "A request to share &#8220;Sth nr three&#8221; with Sth nr Dest was \
         withdrawn";
      must body "href='/c/sth-nr-d/settings/shared-threads'";
      (* 5: removed — origin-context recipients link to the Share page;
         the destination actor is excluded. *)
      let* r =
        Store.remove conn ~actor_user_id:dtop ~placement_id:p1
          ~acting_community_id:d
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "remove: %s" (Shared_thread_fixture.error_str e)
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" apipe
      in
      must body
        "&#8220;Sth nr thread&#8221; is no longer shared with Sth nr Dest";
      let opipe = Shared_thread_http_fixture.session ~url ~uid:otop () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" opipe
      in
      must body
        "&#8220;Sth nr thread&#8221; is no longer shared with Sth nr Dest";
      must body (Printf.sprintf "href='%s'" share_link);
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must_not body "is no longer shared with";
      Lwt.return_unit)

let notif_access_loss_case =
  Shared_thread_http_fixture.db_case
    "a recipient who can no longer read a side gets the generic line, and the \
     row survives access loss" (fun ~url conn ->
      let* author, _otop, dtop, _o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "nl"
      in
      let* _p =
        Shared_thread_http_fixture.seed_request conn ~actor:author
          ~note:"STH_NOTE_NL" ~post ~destination:d ()
      in
      let dpipe = Shared_thread_http_fixture.session ~url ~uid:dtop () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must body "requested to share &#8220;Sth nl thread&#8221;";
      (* The origin goes private: its title and identity leave the
         recipient's copy, but the row stays. *)
      let* o_id =
        find conn "origin id"
          ((Caqti_type.string ->! Caqti_type.int)
             "SELECT id FROM communities WHERE slug = $1")
          "sth-nl-o"
      in
      let* () =
        exec conn "privatize origin" Community_fixture.q_make_private o_id
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must body "A shared thread update";
      must_not body "Sth nl thread";
      must_not body "requested to share";
      must_not body "sth-nl-o";
      (* Restored access restores the full line. *)
      let* () =
        exec conn "restore" Shared_thread_fixture.q_make_eligible o_id
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must body "requested to share &#8220;Sth nl thread&#8221;";
      (* The destination goes private and the recipient loses the role:
         now their own side is unreadable too. *)
      let* () = exec conn "privatize dest" Community_fixture.q_make_private d in
      let* () =
        exec conn "demote" Community_fixture.q_remove_moderator (dtop, d)
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" dpipe
      in
      must body "A shared thread update";
      must_not body "Sth nl thread";
      must_not body "/settings/shared-threads";
      Lwt.return_unit)

(* === Link capability ===

   Read access decides what a notification may NAME; the link targets gate
   harder. A rendered row must never link a page its recipient is
   guaranteed to 404 on: the Share-page link follows that page's own gate
   and degrades to the canonical thread, the management link follows the
   settings gate and degrades to plain text. *)

let q_unban_community =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "DELETE FROM community_bans WHERE user_id = $1 AND community_id = $2"

let q_unban_global =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = FALSE WHERE id = $1"

let capability_share_case =
  Shared_thread_http_fixture.db_case
    "accepted links follow the Share page's own gate: the author arm needs \
     current unbanned membership, the admin arm the durable pair, and the \
     fallback is the canonical thread" (fun ~url conn ->
      let* author, _otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "cs"
      in
      let thread_link =
        Earde.Post_cards.canonical_thread_path "sth-cs-o" post "Sth cs thread"
      in
      let share_href = Printf.sprintf "href='%s/share'" thread_link in
      let thread_href = Printf.sprintf "href='%s'" thread_link in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author
          ~note:"STH_NOTE_CS" ~post ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:p1
          ~destination:d ()
      in
      let mailbox ?(admin = false) uid =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* _, body =
          Shared_thread_http_fixture.get ~target:"/notifications" pipeline
        in
        Lwt.return body
      in
      (* The author while a current member may open the Share page, so
         the acceptance links there. *)
      let* body = mailbox author in
      must body "Sth cs Dest accepted &#8220;Sth cs thread&#8221;";
      must body share_href;
      must_not body "STH_NOTE_CS";
      must_not body "sth_cs_dtop";
      (* Leaving a public origin keeps the copy readable but closes the
         Share page: the link falls back to the canonical thread. *)
      let* () = exec conn "leave" q_remove_member (author, o) in
      let* body = mailbox author in
      must body "Sth cs Dest accepted &#8220;Sth cs thread&#8221;";
      must body thread_href;
      must_not body share_href;
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:author ~community:o
      in
      (* A community ban defeats the author arm the same way... *)
      let* () =
        exec conn "cban" Shared_thread_http_fixture.q_ban_community (author, o)
      in
      let* body = mailbox author in
      must body thread_href;
      must_not body share_href;
      let* () = exec conn "cunban" q_unban_community (author, o) in
      (* ...as does a global ban. *)
      let* () =
        exec conn "gban" Shared_thread_http_fixture.q_ban_global author
      in
      let* body = mailbox author in
      must body thread_href;
      must_not body share_href;
      let* () = exec conn "gunban" q_unban_global author in
      (* A session admin claim without durable users.is_admin grants
         nothing; the durable pair restores the Share link even for a
         departed author — and only behind the claim. *)
      let* () = exec conn "leave again" q_remove_member (author, o) in
      let* body = mailbox ~admin:true author in
      must body thread_href;
      must_not body share_href;
      let* () = set_admin conn ~user:author true in
      let* body = mailbox ~admin:true author in
      must body share_href;
      let* body = mailbox author in
      must body thread_href;
      must_not body share_href;
      let* () = set_admin conn ~user:author false in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:author ~community:o
      in
      (* A tombstoned thread closes the Share page for every viewer. *)
      let* () =
        exec conn "tombstone" Shared_thread_fixture.q_tombstone
          (post, "[deleted]")
      in
      let* body = mailbox author in
      must body thread_href;
      must_not body share_href;
      let* () =
        exec conn "restore" Shared_thread_http_fixture.q_restore_content post
      in
      let* body = mailbox author in
      must body share_href;
      (* Rejected rows choose their link the same way. *)
      let* post2 =
        find conn "post2" Shared_thread_http_fixture.q_insert_post
          ("Sth cs two", (o, author))
      in
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post2
          ~destination:d ()
      in
      let* r =
        Store.review conn ~reviewer_user_id:dtop ~placement_id:p2
          ~destination_community_id:d ~decision:Store.Reject
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "reject: %s" (Shared_thread_fixture.error_str e)
      in
      let thread2 =
        Earde.Post_cards.canonical_thread_path "sth-cs-o" post2 "Sth cs two"
      in
      let* body = mailbox author in
      must body "Sth cs Dest declined &#8220;Sth cs two&#8221;";
      must body (Printf.sprintf "href='%s/share'" thread2);
      let* () = exec conn "leave third" q_remove_member (author, o) in
      let* body = mailbox author in
      must body (Printf.sprintf "href='%s'" thread2);
      must_not body (Printf.sprintf "href='%s/share'" thread2);
      Lwt.return_unit)

let capability_requester_role_case =
  Shared_thread_http_fixture.db_case
    "having been the requester grants no Share-page link once the role that \
     permitted sharing has lapsed" (fun ~url conn ->
      let* _author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "cq"
      in
      let* p =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~post
          ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:p
          ~destination:d ()
      in
      let thread_link =
        Earde.Post_cards.canonical_thread_path "sth-cq-o" post "Sth cq thread"
      in
      let share_href = Printf.sprintf "href='%s/share'" thread_link in
      let thread_href = Printf.sprintf "href='%s'" thread_link in
      let opipe = Shared_thread_http_fixture.session ~url ~uid:otop () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" opipe
      in
      must body "Sth cq Dest accepted &#8220;Sth cq thread&#8221;";
      must body share_href;
      (* The role lapses: the still-public origin keeps the copy, and
         the link degrades to the canonical thread. *)
      let* () =
        exec conn "demote" Community_fixture.q_remove_moderator (otop, o)
      in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/notifications" opipe
      in
      must body "Sth cq Dest accepted &#8220;Sth cq thread&#8221;";
      must body thread_href;
      must_not body share_href;
      Lwt.return_unit)

let capability_manage_case =
  Shared_thread_http_fixture.db_case
    "management links render only for the destination's exact current top_mods \
     and durable admins; every lapsed or lesser role keeps the copy without \
     the link" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ =
        Shared_thread_http_fixture.fixture conn "cn"
      in
      let* dtop2 = insert_user conn "sth_cn_dtop2" in
      let* dmodu = insert_user conn "sth_cn_dmodu" in
      let* dlegacyu = insert_user conn "sth_cn_dlegacyu" in
      let* dadmin = insert_user conn "sth_cn_dadmin" in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dtop2 ~community:d
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dmodu ~community:d
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dlegacyu ~community:d
      in
      let* () =
        Shared_thread_http_fixture.add_top_mod conn ~user:dadmin ~community:d
      in
      let* p1 =
        Shared_thread_http_fixture.seed_request conn ~actor:author
          ~note:"STH_NOTE_CN" ~post ~destination:d ()
      in
      let mgmt = "/c/sth-cn-d/settings/shared-threads" in
      let incoming_href = Printf.sprintf "href='%s#incoming'" mgmt in
      let mgmt_href = Printf.sprintf "href='%s'" mgmt in
      let mailbox ?(admin = false) uid =
        let pipeline = Shared_thread_http_fixture.session ~url ~uid ~admin () in
        let* _, body =
          Shared_thread_http_fixture.get ~target:"/notifications" pipeline
        in
        Lwt.return body
      in
      (* The destination top_mod who keeps the role links to the queue. *)
      let* body = mailbox dtop in
      must body "requested to share &#8220;Sth cn thread&#8221;";
      must body incoming_href;
      must_not body "STH_NOTE_CN";
      must_not body "sth_cn_author";
      (* Lapsing to plain membership keeps the copy — the community is
         still readable — but never the link. *)
      let* () =
        exec conn "demote dtop2" Community_fixture.q_remove_moderator (dtop2, d)
      in
      let* () =
        Shared_thread_http_fixture.add_member conn ~user:dtop2 ~community:d
      in
      let* body = mailbox dtop2 in
      must body "requested to share &#8220;Sth cn thread&#8221;";
      must_not body "/settings/shared-threads";
      (* mod and legacy_mod are not top_mod. *)
      let* () =
        exec conn "demote dmodu" Community_fixture.q_remove_moderator (dmodu, d)
      in
      let* () = add_role conn ~user:dmodu ~community:d "mod" in
      let* body = mailbox dmodu in
      must body "requested to share &#8220;Sth cn thread&#8221;";
      must_not body "/settings/shared-threads";
      let* () =
        exec conn "demote dlegacyu" Community_fixture.q_remove_moderator
          (dlegacyu, d)
      in
      let* () = add_role conn ~user:dlegacyu ~community:d "legacy_mod" in
      let* body = mailbox dlegacyu in
      must body "requested to share &#8220;Sth cn thread&#8221;";
      must_not body "/settings/shared-threads";
      (* A stale session claim with durable is_admin FALSE grants
         nothing. *)
      let* body = mailbox ~admin:true dtop2 in
      must_not body "/settings/shared-threads";
      (* The durable pair alone suffices, with no moderator row at all —
         and only behind the claim. *)
      let* () =
        exec conn "unmod dadmin" Community_fixture.q_remove_moderator (dadmin, d)
      in
      let* () = set_admin conn ~user:dadmin true in
      let* body = mailbox ~admin:true dadmin in
      must body incoming_href;
      let* body = mailbox dadmin in
      must_not body "/settings/shared-threads";
      (* Withdrawn rows obey the same gate. *)
      let* r =
        Store.withdraw conn ~actor_user_id:author ~placement_id:p1
          ~origin_community_id:o
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "withdraw: %s" (Shared_thread_fixture.error_str e)
      in
      let* body = mailbox dtop in
      must body
        "A request to share &#8220;Sth cn thread&#8221; with Sth cn Dest was \
         withdrawn";
      must body mgmt_href;
      (* Destination-context removal rows too: linked while the role
         holds, plain text once it lapses — never a guaranteed 404. *)
      let* p2 =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post
          ~destination:d ()
      in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:p2
          ~destination:d ()
      in
      let* r =
        Store.remove conn ~actor_user_id:otop ~placement_id:p2
          ~acting_community_id:o
      in
      let* () =
        match r with
        | Ok _ -> Lwt.return_unit
        | Error e ->
            Alcotest.failf "remove: %s" (Shared_thread_fixture.error_str e)
      in
      let* body = mailbox dtop in
      must body
        "&#8220;Sth cn thread&#8221; is no longer shared with Sth cn Dest";
      must body mgmt_href;
      let* () =
        exec conn "demote dtop" Community_fixture.q_remove_moderator (dtop, d)
      in
      let* body = mailbox dtop in
      must body
        "&#8220;Sth cn thread&#8221; is no longer shared with Sth cn Dest";
      must_not body "/settings/shared-threads";
      (* Total access loss still degrades to the generic unlinked line:
         capability gating changed nothing about the read gate. *)
      let* () =
        exec conn "privatize origin" Community_fixture.q_make_private o
      in
      let* body = mailbox dtop2 in
      must body "A shared thread update";
      must_not body "Sth cn thread";
      must_not body "/settings/shared-threads";
      Lwt.return_unit)

let share_get_suite =
  [
    get_share_authz_case;
    get_share_candidates_case;
    get_share_controls_case;
    thread_entry_case;
    settings_nav_case;
  ]

let share_post_suite =
  [
    post_share_flow_case;
    post_share_denied_case;
    post_share_validation_case;
    post_share_stale_case;
  ]

let mgmt_suite =
  [ mgmt_authz_case; mgmt_content_case; mgmt_flat_and_ineligible_case ]

let review_suite =
  [
    review_flow_case; review_section_case; review_stale_case; review_authz_case;
  ]

let withdraw_suite =
  [ withdraw_authz_case; withdraw_resilience_case; withdraw_context_case ]

let remove_suite = [ remove_authz_case; remove_resilience_case ]
let csrf_suite = [ csrf_case ]
let notif_suite = [ notif_render_case; notif_access_loss_case ]

let notif_capability_suite =
  [
    capability_share_case;
    capability_requester_role_case;
    capability_manage_case;
  ]

let suites =
  [
    ("shared_thread_share_page_http", share_get_suite);
    ("shared_thread_share_request_http", share_post_suite);
    ("shared_thread_management_http", mgmt_suite);
    ("shared_thread_review_http", review_suite);
    ("shared_thread_withdraw_http", withdraw_suite);
    ("shared_thread_remove_http", remove_suite);
    ("shared_thread_mutation_csrf", csrf_suite);
    ("shared_thread_notification_rendering", notif_suite);
    ("shared_thread_notification_link_capability", notif_capability_suite);
  ]
