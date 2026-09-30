(* Community connections, slice 2 — the real handlers over a real database:
   the authorization matrix, subject binding, CSRF, eligibility enforcement,
   target-search exclusions, and the rendered sections. Database-gated on
   EARDE_TEST_DATABASE_URL. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module H = Earde.Community_connections_handlers

module Store = Earde.Community_connections_store

module Cc = Earde.Community_connections

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let contains haystack needle = Html_assert.occurs haystack ~needle

let status_of = Http_fixture.status_of

let must = Html_assert.must

let must_not = Html_assert.must_not

let base slug = Printf.sprintf "/c/%s/settings/connections" slug

let action ~slug ~id verb =
  Printf.sprintf "%s/%Ld/%s" (base slug) id verb

(* --- Fixtures --- *)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM community_connection_audit_events \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'ccnh-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'ccnh-%')"
    ; "DELETE FROM community_connections \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'ccnh-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'ccnh-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'ccnh-%'"
    ; "DELETE FROM users WHERE username LIKE 'ccnh_%'"
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
               (fun () -> f ~url conn)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let add_role conn ~user ~community role =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, role)

let add_top_mod conn ~user ~community = add_role conn ~user ~community "top_mod"

let add_member conn ~user ~community =
  exec conn "member fixture" Community_fixture.q_insert_member (user, community)

let set_admin conn ~user flag =
  exec conn "admin fixture" Community_fixture.q_set_admin (user, flag)

(* Lifecycle drift written directly, exactly as the durable columns
   represent it — the same fixtures the choice-read-model suite uses. *)
let make_private conn cid = exec conn "private" Community_fixture.q_make_private cid

let make_unlisted conn cid = exec conn "unlisted" Community_fixture.q_make_unlisted cid

let make_draft conn cid = exec conn "draft" Community_fixture.q_make_draft_state cid

let q_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM community_connections WHERE id = $1"

let q_count_pair =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_connections \
   WHERE LEAST(requester_community_id, recipient_community_id) \
         = LEAST($1, $2) \
     AND GREATEST(requester_community_id, recipient_community_id) \
         = GREATEST($1, $2)"

let q_event_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM community_connection_audit_events \
   WHERE connection_id = $1"

let check_status label conn id expected =
  let* status = find conn (label ^ ": status") q_status id in
  Alcotest.(check string) (label ^ ": status") expected status;
  Lwt.return_unit

let check_events label conn id expected =
  let* n = find conn (label ^ ": events") q_event_count id in
  Alcotest.(check int) (label ^ ": audit events") expected n;
  Lwt.return_unit

let accept_direct conn ~actor ~id ~recipient =
  let* r =
    Store.review conn ~reviewer_user_id:actor ~connection_id:id
      ~recipient_community_id:recipient ~decision:Store.Accept
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.fail "fixture: accept refused"

(* --- The real routed application, over the real pool --- *)

(* One sql_pool for the whole suite. Every case below builds one pipeline
   per acting session, and nothing ever closes a Dream.sql_pool; since its
   pool lives inside the middleware closure, reusing that one value is what
   keeps those pipelines on a single connection instead of leaking one per
   session until Postgres refuses. Sessions stay separate — each pipeline
   still gets its own memory session store. *)
let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

let app_pipeline ?session_user_id ?(session_admin = false) ~url () =
  sql_pool url @@ Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
  @@ (fun handler request ->
       match session_user_id with
       | None -> handler request
       | Some uid ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           let* () =
             if session_admin then
               Dream.set_session_field request "is_admin" "true"
             else Lwt.return_unit
           in
           handler request)
  @@ Dream.router
       [ Dream.get "/mint" (fun req ->
             Dream.respond
               (Dream.csrf_token req ^ "\n"
              ^ Dream.csrf_token ~valid_for:(-60.) req));
         Dream.get "/c/:slug/settings/connections"
           H.make_connections_page_handler;
         Dream.get "/c/:slug/settings/connections/new"
           H.make_connections_search_handler;
         Dream.post "/c/:slug/settings/connections/request"
           H.make_connection_request_handler;
         Dream.post "/c/:slug/settings/connections/:id/accept"
           H.make_connection_accept_handler;
         Dream.post "/c/:slug/settings/connections/:id/reject"
           H.make_connection_reject_handler;
         Dream.post "/c/:slug/settings/connections/:id/remove"
           H.make_connection_removal_handler;
       ]

let mint label pipeline =
  let* response =
    pipeline (Dream.request ~method_:`GET ~target:"/mint" "")
  in
  let cookie = Http_fixture.session_cookie label response in
  let* body = Dream.body response in
  match String.split_on_char '\n' body with
  | [ fresh; expired ] -> Lwt.return (cookie, fresh, expired)
  | _ -> Alcotest.failf "%s: unexpected mint body" label

let get ?cookie ~target pipeline = Http_fixture.do_get ?cookie ~target pipeline

let post ?(content_type = true) ~cookie ~target ~fields pipeline =
  let headers =
    (if content_type then
       [ ("Content-Type", "application/x-www-form-urlencoded") ]
     else [])
    @ [ ("Cookie", cookie) ]
  in
  let* response =
    pipeline
      (Dream.request ~method_:`POST ~target ~headers (Http_fixture.form_body fields))
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* One authorized session that has already opened the page, so both a
   cookie and a live token exist for the follow-up POST. *)
let session ~url ~uid ?(admin = false) () =
  app_pipeline ~session_user_id:uid ~session_admin:admin ~url ()

let acting label ~url ~uid ?(admin = false) () =
  let pipeline = session ~url ~uid ~admin () in
  let* cookie, token, expired = mint label pipeline in
  Lwt.return (pipeline, cookie, token, expired)

(* === GET: authorization matrix === *)

let get_authz_case =
  db_case "GET connections: exactly top_mod and durable admins; everyone \
           else is one generic 404" (fun ~url conn ->
      let* top = insert_user conn "ccnh_top" in
      let* admin = insert_user conn "ccnh_admin" in
      let* modu = insert_user conn "ccnh_mod" in
      let* legacy = insert_user conn "ccnh_legacy" in
      let* member = insert_user conn "ccnh_member" in
      let* stranger = insert_user conn "ccnh_stranger" in
      let* othertop = insert_user conn "ccnh_othertop" in
      let* sonly = insert_user conn "ccnh_sonly" in
      let* cid = insert_community ~name:"Ccnh Home" conn "ccnh-home" in
      let* other = insert_community conn "ccnh-other" in
      let* () = add_top_mod conn ~user:top ~community:cid in
      let* () = set_admin conn ~user:admin true in
      let* () = add_role conn ~user:modu ~community:cid "mod" in
      let* () = add_role conn ~user:legacy ~community:cid "legacy_mod" in
      let* () = add_member conn ~user:member ~community:cid in
      let* () = add_top_mod conn ~user:othertop ~community:other in
      let sees label uid ~admin =
        let pipeline = session ~url ~uid ~admin () in
        let* response, body = get ~target:(base "ccnh-home") pipeline in
        Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
        must body ">Connected communities</h2>";
        Lwt.return_unit
      in
      let denied label uid ~admin ~slug =
        let pipeline = session ~url ~uid ~admin () in
        let* response, body = get ~target:(base slug) pipeline in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        must_not body "Connected communities";
        Lwt.return_unit
      in
      let* () = sees "top mod" top ~admin:false in
      (* The durable flag is what counts; the session flag only enables the
         check. *)
      let* () = sees "durable admin" admin ~admin:true in
      (* Both halves are required: the session claim only enables the
         durable check, and the durable flag only answers a claim. *)
      let* () =
        denied "durable admin without the session claim" admin ~admin:false
          ~slug:"ccnh-home"
      in
      let* () = denied "ordinary mod" modu ~admin:false ~slug:"ccnh-home" in
      let* () = denied "legacy mod" legacy ~admin:false ~slug:"ccnh-home" in
      let* () = denied "member" member ~admin:false ~slug:"ccnh-home" in
      let* () = denied "non-member" stranger ~admin:false ~slug:"ccnh-home" in
      let* () =
        denied "top mod of another community" othertop ~admin:false
          ~slug:"ccnh-home"
      in
      let* () =
        denied "session-only admin claim" sonly ~admin:true ~slug:"ccnh-home"
      in
      let* () = denied "missing community" top ~admin:false ~slug:"ccnh-absent" in
      (* Anonymous callers never reach the read model at all. *)
      let anon = app_pipeline ~url () in
      let* response, _ = get ~target:(base "ccnh-home") anon in
      Alcotest.(check int) "anonymous: 303" 303 (status_of response);
      Alcotest.(check (option string)) "to /login" (Some "/login")
        (Dream.header response "Location");
      Lwt.return_unit)

(* === GET: the three sections over real rows === *)

let get_sections_case =
  db_case "GET connections: accepted reads symmetrically, pending rows carry \
           their private note, nothing names a person" (fun ~url conn ->
      let* top = insert_user conn "ccnh_s_top" in
      let* peer = insert_user conn "ccnh_s_peer" in
      let* home = insert_community ~name:"Ccnh Sections" conn "ccnh-sec" in
      let* friend = insert_community ~name:"Friend" conn "ccnh-sec-friend" in
      let* asker = insert_community ~name:"Asker" conn "ccnh-sec-asker" in
      let* wanted = insert_community ~name:"Wanted" conn "ccnh-sec-wanted" in
      let* () = add_top_mod conn ~user:top ~community:home in
      (* Accepted, requested by the OTHER side — it must still appear. *)
      let* accepted_id =
        Http_fixture.request_direct conn ~actor:peer ~requester:friend ~recipient:home ()
      in
      let* () = accept_direct conn ~actor:top ~id:accepted_id ~recipient:home in
      (* Incoming, with a hostile note. *)
      let* incoming_id =
        Http_fixture.request_direct conn ~actor:peer ~requester:asker ~recipient:home
          ~note:"<script>x</script> joint reading group" ()
      in
      (* Outgoing. *)
      let* outgoing_id =
        Http_fixture.request_direct conn ~actor:top ~requester:home ~recipient:wanted
          ~note:"we admire your work" ()
      in
      let pipeline = session ~url ~uid:top () in
      let* response, body = get ~target:(base "ccnh-sec") pipeline in
      Alcotest.(check int) "200" 200 (status_of response);
      must body ">Friend</a>";
      must body ">Asker</a>";
      must body ">Wanted</a>";
      must body (Printf.sprintf "connections/%Ld/remove" accepted_id);
      must body (Printf.sprintf "connections/%Ld/accept" incoming_id);
      must body (Printf.sprintf "connections/%Ld/reject" incoming_id);
      (* An outgoing request carries no control. *)
      must_not body (Printf.sprintf "connections/%Ld/" outgoing_id);
      must body "Waiting for a reply";
      (* The note is visible on this authorized page, escaped. *)
      must body "joint reading group";
      must_not body "<script>x</script>";
      must body "&lt;script&gt;";
      must body "we admire your work";
      (* No person is named anywhere in the connections fragment. *)
      let frag = Html_assert.panel_fragment body in
      List.iter (fun n -> must_not frag n)
        [ "ccnh_s_top"; "ccnh_s_peer"; "Accepted by"; "Requested by"
        ; "Reviewed by"; "Removed by" ];
      Lwt.return_unit)

(* === GET: target search exclusions === *)

let search_case =
  db_case "GET search: only eligible, unconnected, other communities are \
           offered" (fun ~url conn ->
      let* top = insert_user conn "ccnh_q_top" in
      let* peer = insert_user conn "ccnh_q_peer" in
      let* home = insert_community ~name:"Ccnh Query" conn "ccnh-q-home" in
      let* _good = insert_community ~name:"Ccnh Query Good" conn "ccnh-q-good" in
      let* privc =
        insert_community ~network:false ~name:"Ccnh Query Priv" conn
          "ccnh-q-priv"
      in
      let* draft =
        insert_community ~network:false ~name:"Ccnh Query Draft" conn
          "ccnh-q-draft"
      in
      let* unlisted =
        insert_community ~network:false ~name:"Ccnh Query Unlisted" conn
          "ccnh-q-unlisted"
      in
      let* pending_target =
        insert_community ~name:"Ccnh Query Pending" conn "ccnh-q-pending"
      in
      let* accepted_target =
        insert_community ~name:"Ccnh Query Accepted" conn "ccnh-q-accepted"
      in
      let* rejected_target =
        insert_community ~name:"Ccnh Query Rejected" conn "ccnh-q-rejected"
      in
      let* () = add_top_mod conn ~user:top ~community:home in
      let* () = make_private conn privc in
      let* () = make_draft conn draft in
      let* () = make_unlisted conn unlisted in
      let* _ =
        Http_fixture.request_direct conn ~actor:top ~requester:home
          ~recipient:pending_target ()
      in
      let* acc =
        Http_fixture.request_direct conn ~actor:peer ~requester:accepted_target
          ~recipient:home ()
      in
      let* () = accept_direct conn ~actor:top ~id:acc ~recipient:home in
      let* rej =
        Http_fixture.request_direct conn ~actor:top ~requester:home
          ~recipient:rejected_target ()
      in
      let* r =
        Store.review conn ~reviewer_user_id:top ~connection_id:rej
          ~recipient_community_id:rejected_target ~decision:Store.Reject
      in
      (match r with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "fixture: reject refused");
      let pipeline = session ~url ~uid:top () in
      let* response, body =
        get ~target:(base "ccnh-q-home" ^ "/new?q=Ccnh+Query") pipeline
      in
      Alcotest.(check int) "200" 200 (status_of response);
      (* Offered: eligible, unconnected, and the rejected-history one. *)
      must body "target=ccnh-q-good";
      must body "target=ccnh-q-rejected";
      (* Withheld, and never even named. *)
      (* Withheld communities are not named at all; the searching
         community's own slug legitimately appears in its own form action
         and back link, so only its offer is checked. *)
      must_not body "target=ccnh-q-home";
      List.iter
        (fun slug ->
          must_not body ("target=" ^ slug);
          must_not body ("/c/" ^ slug))
        [ "ccnh-q-priv"; "ccnh-q-draft"; "ccnh-q-unlisted"; "ccnh-q-pending"
        ; "ccnh-q-accepted" ];
      List.iter (fun n -> must_not body n)
        [ "Ccnh Query Priv"; "Ccnh Query Draft"; "Ccnh Query Unlisted" ];
      (* A blank query is the unsearched page; a matching-nothing query is
         the empty one. Neither states a count. *)
      let* _, blank = get ~target:(base "ccnh-q-home" ^ "/new") pipeline in
      must blank "Enter a name or address to search.";
      let* _, none =
        get ~target:(base "ccnh-q-home" ^ "/new?q=zzzznothing") pipeline
      in
      must none "No communities matched.";
      (* A wildcard cannot widen the pattern. *)
      let* _, wild = get ~target:(base "ccnh-q-home" ^ "/new?q=%25") pipeline in
      must wild "No communities matched.";
      (* Step two only accepts a target the search would still offer. *)
      let* _, confirm =
        get ~target:(base "ccnh-q-home" ^ "/new?target=ccnh-q-good") pipeline
      in
      must confirm "name='target'";
      must confirm "value='ccnh-q-good'";
      must confirm "<textarea";
      let* _, refused =
        get ~target:(base "ccnh-q-home" ^ "/new?target=ccnh-q-priv") pipeline
      in
      must refused "not available to connect";
      must_not refused "<textarea";
      must_not refused "Ccnh Query Priv";
      (* A pair that already has a live request is named as such — that is
         this community's own state, visible on its own management page —
         while a withheld community stays collapsed above. *)
      let* _, active =
        get ~target:(base "ccnh-q-home" ^ "/new?target=ccnh-q-pending")
          pipeline
      in
      must active "already connected";
      must_not active "<textarea";
      (* And an unaddressable slug is refused without any oracle. *)
      let* _, weird =
        get ~target:(base "ccnh-q-home" ^ "/new?target=a%2Fb") pipeline
      in
      must weird "not available to connect";
      Lwt.return_unit)

(* === POST request: authorization and eligibility === *)

let request_authz_case =
  db_case "POST request: only the source community's top_mod or a durable \
           admin may send one" (fun ~url conn ->
      let* top = insert_user conn "ccnh_r_top" in
      let* admin = insert_user conn "ccnh_r_admin" in
      let* modu = insert_user conn "ccnh_r_mod" in
      let* member = insert_user conn "ccnh_r_member" in
      let* stranger = insert_user conn "ccnh_r_stranger" in
      let* home = insert_community ~name:"Ccnh Req" conn "ccnh-r-home" in
      let* target = insert_community ~name:"Ccnh Req Target" conn "ccnh-r-target" in
      let* target2 =
        insert_community ~name:"Ccnh Req Target Two" conn "ccnh-r-target2"
      in
      let* () = add_top_mod conn ~user:top ~community:home in
      let* () = set_admin conn ~user:admin true in
      let* () = add_role conn ~user:modu ~community:home "mod" in
      let* () = add_member conn ~user:member ~community:home in
      let send label uid ~admin ~target_slug =
        let* pipeline, cookie, token, _ =
          acting label ~url ~uid ~admin ()
        in
        post ~cookie
          ~target:(base "ccnh-r-home" ^ "/request")
          ~fields:
            [ ("dream.csrf", token); ("target", target_slug);
              ("note", "please connect") ]
          pipeline
      in
      (* The two authorized identities each succeed once. *)
      let* response, _ =
        send "top mod" top ~admin:false ~target_slug:"ccnh-r-target"
      in
      Alcotest.(check int) "top mod: 303" 303 (status_of response);
      Alcotest.(check (option string)) "back to the management page"
        (Some (base "ccnh-r-home"))
        (Dream.header response "Location");
      let* n = find conn "pair" q_count_pair (home, target) in
      Alcotest.(check int) "one pending row" 1 n;
      let* response, _ =
        send "durable admin" admin ~admin:true ~target_slug:"ccnh-r-target2"
      in
      Alcotest.(check int) "admin: 303" 303 (status_of response);
      let* n = find conn "pair2" q_count_pair (home, target2) in
      Alcotest.(check int) "admin's row" 1 n;
      (* Everyone else is refused before any store call. *)
      let refused label uid ~admin =
        let* response, body = send label uid ~admin ~target_slug:"ccnh-r-target" in
        Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
        must body "This page does not exist.";
        Lwt.return_unit
      in
      let* () = refused "ordinary mod" modu ~admin:false in
      let* () = refused "member" member ~admin:false in
      let* () = refused "non-member" stranger ~admin:false in
      let* () = refused "session-only admin" stranger ~admin:true in
      (* And nothing new was written by any of them. *)
      let* n = find conn "still one" q_count_pair (home, target) in
      Alcotest.(check int) "no extra rows" 1 n;
      (* A duplicate is a stable conflict, not a second row. *)
      let* response, body =
        send "duplicate" top ~admin:false ~target_slug:"ccnh-r-target"
      in
      Alcotest.(check int) "duplicate: 409" 409 (status_of response);
      must body "already connected";
      let* n = find conn "still one after duplicate" q_count_pair (home, target) in
      Alcotest.(check int) "no extra rows" 1 n;
      Lwt.return_unit)

let request_eligibility_case =
  db_case "POST request: an ineligible source or target is refused at the \
           transaction boundary" (fun ~url conn ->
      let* top = insert_user conn "ccnh_e_top" in
      let* home =
        insert_community ~network:false ~name:"Ccnh Elig" conn "ccnh-e-home"
      in
      let* target =
        insert_community ~network:false ~name:"Ccnh Elig Target" conn
          "ccnh-e-target"
      in
      let* () = add_top_mod conn ~user:top ~community:home in
      let send label ~target_slug =
        let* pipeline, cookie, token, _ = acting label ~url ~uid:top () in
        post ~cookie
          ~target:(base "ccnh-e-home" ^ "/request")
          ~fields:[ ("dream.csrf", token); ("target", target_slug) ]
          pipeline
      in
      (* The target goes private after the page was rendered. *)
      let* () = make_private conn target in
      let* response, body = send "private target" ~target_slug:"ccnh-e-target" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "not available to connect";
      (* The refusal never says why, and never that the community exists. *)
      must_not body "private";
      let* n = find conn "nothing written" q_count_pair (home, target) in
      Alcotest.(check int) "no row" 0 n;
      (* Now the source itself becomes ineligible. *)
      let* () = exec conn "restore" Community_fixture.q_make_listed target in
      let* () = make_unlisted conn home in
      let* response, body = send "unlisted source" ~target_slug:"ccnh-e-target" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "cannot create or accept new connections";
      let* n = find conn "still nothing" q_count_pair (home, target) in
      Alcotest.(check int) "no row" 0 n;
      (* Its management page still loads, and the search surface says no. *)
      let pipeline = session ~url ~uid:top () in
      let* response, page = get ~target:(base "ccnh-e-home") pipeline in
      Alcotest.(check int) "management still 200" 200 (status_of response);
      must page "cannot create or accept new connections";
      let* response, _ = get ~target:(base "ccnh-e-home" ^ "/new") pipeline in
      Alcotest.(check int) "search closed" 403 (status_of response);
      Lwt.return_unit)

(* === POST accept/reject: subject binding === *)

let review_authz_case =
  db_case "POST accept/reject: only the recipient community's authority, \
           and only over its own connection" (fun ~url conn ->
      let* rtop = insert_user conn "ccnh_v_rtop" in
      let* stop = insert_user conn "ccnh_v_stop" in
      let* xtop = insert_user conn "ccnh_v_xtop" in
      let* admin = insert_user conn "ccnh_v_admin" in
      let* source = insert_community ~name:"Ccnh Src" conn "ccnh-v-src" in
      let* recipient = insert_community ~name:"Ccnh Rec" conn "ccnh-v-rec" in
      let* outsider = insert_community ~name:"Ccnh Out" conn "ccnh-v-out" in
      let* () = add_top_mod conn ~user:stop ~community:source in
      let* () = add_top_mod conn ~user:rtop ~community:recipient in
      let* () = add_top_mod conn ~user:xtop ~community:outsider in
      let* () = set_admin conn ~user:admin true in
      let* () = add_top_mod conn ~user:admin ~community:outsider in
      let* id =
        Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient ()
      in
      let review label uid ~admin ~slug verb =
        let* pipeline, cookie, token, _ = acting label ~url ~uid ~admin () in
        post ~cookie
          ~target:(Printf.sprintf "%s/%Ld/%s" (base slug) id verb)
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      (* The requesting side cannot review its own request, even though it
         is a top_mod of a community in the pair. *)
      let* response, body = review "requester side" stop ~admin:false ~slug:"ccnh-v-src" "accept" in
      Alcotest.(check int) "404" 404 (status_of response);
      must body "This page does not exist.";
      let* () = check_status "untouched" conn id "pending" in
      (* An unrelated community's top mod, addressing it through their own
         route, cannot reach it either. *)
      let* response, _ = review "outsider" xtop ~admin:false ~slug:"ccnh-v-out" "accept" in
      Alcotest.(check int) "404" 404 (status_of response);
      let* () = check_status "untouched" conn id "pending" in
      (* A global admin operating from an unrelated community context is
         refused too: authority does not remove the subject binding. *)
      let* response, _ = review "admin, wrong context" admin ~admin:true ~slug:"ccnh-v-out" "accept" in
      Alcotest.(check int) "404" 404 (status_of response);
      let* () = check_status "untouched" conn id "pending" in
      (* Only one audit event so far: the request. *)
      let* () = check_events "no review yet" conn id 1 in
      (* The recipient's top mod succeeds. *)
      let* response, _ = review "recipient top mod" rtop ~admin:false ~slug:"ccnh-v-rec" "accept" in
      Alcotest.(check int) "303" 303 (status_of response);
      let* () = check_status "accepted" conn id "accepted" in
      let* () = check_events "request + accept" conn id 2 in
      (* A stale duplicate submission is a stable conflict, never a second
         write. *)
      let* response, body = review "duplicate" rtop ~admin:false ~slug:"ccnh-v-rec" "accept" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "no longer pending";
      let* () = check_events "still two" conn id 2 in
      let* response, body = review "late reject" rtop ~admin:false ~slug:"ccnh-v-rec" "reject" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "no longer pending";
      let* () = check_events "still two" conn id 2 in
      Lwt.return_unit)

let review_eligibility_case =
  db_case "POST accept: blocked once either side loses eligibility; reject \
           stays available" (fun ~url conn ->
      let* stop = insert_user conn "ccnh_w_stop" in
      let* rtop = insert_user conn "ccnh_w_rtop" in
      let* source =
        insert_community ~network:false ~name:"Ccnh WSrc" conn "ccnh-w-src"
      in
      let* recipient =
        insert_community ~network:false ~name:"Ccnh WRec" conn "ccnh-w-rec"
      in
      let* () = add_top_mod conn ~user:stop ~community:source in
      let* () = add_top_mod conn ~user:rtop ~community:recipient in
      let* id = Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient () in
      let review label verb =
        let* pipeline, cookie, token, _ = acting label ~url ~uid:rtop () in
        post ~cookie ~target:(action ~slug:"ccnh-w-rec" ~id verb)
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      (* The requesting community goes private after the request. *)
      let* () = make_private conn source in
      let* response, body = review "accept, source private" "accept" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "not available to connect";
      let* () = check_status "still pending" conn id "pending" in
      (* Now the reviewing community loses eligibility too. *)
      let* () = exec conn "restore source" Community_fixture.q_make_listed source in
      let* () = make_unlisted conn recipient in
      let* response, body = review "accept, recipient unlisted" "accept" in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "cannot create or accept new connections";
      let* () = check_status "still pending" conn id "pending" in
      (* The page renders without an accept control but with reject. *)
      let pipeline = session ~url ~uid:rtop () in
      let* _, page = get ~target:(base "ccnh-w-rec") pipeline in
      must page (Printf.sprintf "connections/%Ld/reject" id);
      must_not page (Printf.sprintf "connections/%Ld/accept" id);
      (* And rejection still closes it. *)
      let* response, _ = review "reject while ineligible" "reject" in
      Alcotest.(check int) "303" 303 (status_of response);
      let* () = check_status "rejected" conn id "rejected" in
      let* () = check_events "request + reject" conn id 2 in
      Lwt.return_unit)

(* === POST remove === *)

let removal_case =
  db_case "POST remove: either side may detach, an outsider may not, and \
           eligibility is irrelevant" (fun ~url conn ->
      let* stop = insert_user conn "ccnh_x_stop" in
      let* rtop = insert_user conn "ccnh_x_rtop" in
      let* xtop = insert_user conn "ccnh_x_xtop" in
      let* source =
        insert_community ~network:false ~name:"Ccnh XSrc" conn "ccnh-x-src"
      in
      let* recipient = insert_community ~name:"Ccnh XRec" conn "ccnh-x-rec" in
      let* outsider = insert_community ~name:"Ccnh XOut" conn "ccnh-x-out" in
      let* () = add_top_mod conn ~user:stop ~community:source in
      let* () = add_top_mod conn ~user:rtop ~community:recipient in
      let* () = add_top_mod conn ~user:xtop ~community:outsider in
      let* first = Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient () in
      let* () = accept_direct conn ~actor:rtop ~id:first ~recipient in
      let remove label uid ~slug ~id =
        let* pipeline, cookie, token, _ = acting label ~url ~uid () in
        post ~cookie ~target:(action ~slug ~id "remove")
          ~fields:[ ("dream.csrf", token) ]
          pipeline
      in
      (* An outsider addressing it through their own community is refused
         before the store. *)
      let* response, _ = remove "outsider" xtop ~slug:"ccnh-x-out" ~id:first in
      Alcotest.(check int) "404" 404 (status_of response);
      let* () = check_status "still accepted" conn first "accepted" in
      (* The recipient side removes it. *)
      let* response, _ = remove "recipient side" rtop ~slug:"ccnh-x-rec" ~id:first in
      Alcotest.(check int) "303" 303 (status_of response);
      let* () = check_status "removed" conn first "removed" in
      let* () = check_events "three events" conn first 3 in
      (* A stale duplicate removal is a stable conflict. *)
      let* response, body = remove "duplicate" rtop ~slug:"ccnh-x-rec" ~id:first in
      Alcotest.(check int) "409" 409 (status_of response);
      must body "no longer active";
      let* () = check_events "still three" conn first 3 in
      (* A fresh connection, removed from the requesting side, while that
         side is no longer eligible. *)
      let* second = Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient () in
      let* () = accept_direct conn ~actor:rtop ~id:second ~recipient in
      let* () = make_private conn source in
      let* response, _ = remove "requester side, ineligible" stop ~slug:"ccnh-x-src" ~id:second in
      Alcotest.(check int) "303" 303 (status_of response);
      let* () = check_status "removed" conn second "removed" in
      Lwt.return_unit)

(* === CSRF on every mutation === *)

let csrf_case =
  db_case "every mutation requires the framework CSRF field" (fun ~url conn ->
      let* stop = insert_user conn "ccnh_c_stop" in
      let* rtop = insert_user conn "ccnh_c_rtop" in
      let* source = insert_community ~name:"Ccnh CSrc" conn "ccnh-c-src" in
      let* recipient = insert_community ~name:"Ccnh CRec" conn "ccnh-c-rec" in
      let* free = insert_community ~name:"Ccnh CFree" conn "ccnh-c-free" in
      let* () = add_top_mod conn ~user:stop ~community:source in
      let* () = add_top_mod conn ~user:rtop ~community:recipient in
      let* pending_id =
        Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient ()
      in
      let* accepted_id =
        Http_fixture.request_direct conn ~actor:stop ~requester:source ~recipient:free ()
      in
      let* () = accept_direct conn ~actor:stop ~id:accepted_id ~recipient:free in
      let* pipeline, cookie, _token, expired = acting "mint" ~url ~uid:rtop () in
      let* spipe, scookie, _stoken, sexpired = acting "mint2" ~url ~uid:stop () in
      let refused label ~pipeline ~cookie ~target ~fields =
        let* response, body = post ~cookie ~target ~fields pipeline in
        Alcotest.(check int) (label ^ ": 403") 403 (status_of response);
        (* The refusal re-renders the authorized page with a fresh token
           rather than dead-ending. *)
        must body "open too long";
        Lwt.return_unit
      in
      let accept_target = action ~slug:"ccnh-c-rec" ~id:pending_id "accept" in
      let reject_target = action ~slug:"ccnh-c-rec" ~id:pending_id "reject" in
      let remove_target = action ~slug:"ccnh-c-src" ~id:accepted_id "remove" in
      let request_target = base "ccnh-c-src" ^ "/request" in
      let* () = refused "accept, no token" ~pipeline ~cookie ~target:accept_target ~fields:[] in
      let* () =
        refused "accept, junk token" ~pipeline ~cookie ~target:accept_target
          ~fields:[ ("dream.csrf", "not-a-token") ]
      in
      let* () =
        refused "accept, expired token" ~pipeline ~cookie ~target:accept_target
          ~fields:[ ("dream.csrf", expired) ]
      in
      let* () = refused "reject, no token" ~pipeline ~cookie ~target:reject_target ~fields:[] in
      let* () =
        refused "remove, no token" ~pipeline:spipe ~cookie:scookie
          ~target:remove_target ~fields:[]
      in
      let* () =
        refused "request, expired token" ~pipeline:spipe ~cookie:scookie
          ~target:request_target
          ~fields:[ ("dream.csrf", sexpired); ("target", "ccnh-c-free") ]
      in
      (* Nothing moved. *)
      let* () = check_status "still pending" conn pending_id "pending" in
      let* () = check_status "still accepted" conn accepted_id "accepted" in
      let* () = check_events "request only" conn pending_id 1 in
      Lwt.return_unit)

(* === the settings entry point, and the surfaces it must not disturb === *)

let settings_community ~is_network : Earde.Community_types.community =
  { id = 5150; slug = "ccnh-nav"; name = "Ccnh Nav"; description = None;
    rules = None; avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = false; visibility = Earde.Community_types.Community_public;
    indexable = true; is_network_community = is_network;
    onboarding_state = Earde.Community_types.Community_published; discoverable = true }

let render_settings ~is_admin ~is_top_mod =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.community_settings_page ~is_admin ~is_top_mod
           ~open_reports_count:0
           ~community:(settings_community ~is_network:false)
           ~mods:[] ~banned_users:[] ~members:[] ~sections:[] ~channels:[] req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/c/ccnh-nav/settings" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "settings renderer did not run"

let settings_nav_case =
  Alcotest.test_case
    "settings: the connections entry appears for top mods and admins only, \
     beside the untouched project-home entry" `Quick (fun () ->
      let link = "href='/c/ccnh-nav/settings/connections'" in
      let top = render_settings ~is_admin:false ~is_top_mod:true in
      Alcotest.(check bool) "top_mod sees it" true (contains top link);
      Alcotest.(check bool) "exact label" true (contains top ">Connections</a>");
      let admin = render_settings ~is_admin:true ~is_top_mod:false in
      Alcotest.(check bool) "admin sees it" true (contains admin link);
      let plain = render_settings ~is_admin:false ~is_top_mod:false in
      Alcotest.(check bool) "ordinary mod does not" false (contains plain link);
      (* The pre-existing entry is untouched on every one of them. *)
      List.iter
        (fun (label, html, expected) ->
          Alcotest.(check bool)
            (label ^ ": project-home entry unchanged")
            expected
            (contains html "href='/c/ccnh-nav/project-home-requests'"))
        [ ("top mod", top, true); ("admin", admin, true)
        ; ("ordinary mod", plain, false) ])

let suite =
  [ get_authz_case; get_sections_case; search_case; request_authz_case
  ; request_eligibility_case; review_authz_case; review_eligibility_case
  ; removal_case; csrf_case; settings_nav_case ]

let suites =
  [ ("community_connections_http", suite)
  ]
